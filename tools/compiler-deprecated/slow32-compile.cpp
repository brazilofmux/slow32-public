// SLOW-32 compiler v3 - Unified version with correct PHI handling and code generation
// Combines:
// - Correct PHI linearization from slow32-compile-phi.cpp
// - Code generation from slow32-compile.cpp
//
// This is the production version that should replace the broken implementations

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/CFG.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"

#include <iostream>
#include <memory>
#include <vector>
#include <map>
#include <set>
#include <queue>
#include <algorithm>
#include <sstream>
#include <cassert>

using namespace llvm;

class SLOW32Compiler {
private:
    // ========== Constants and Helpers ==========
    static constexpr int UNALLOC = -999;
    static constexpr int SPILL_BASE = -1000;
    
    static inline bool isUnalloc(int r) { return r == UNALLOC; }
    static inline bool isSpill(int r) { return r <= SPILL_BASE; }  // NOTE: <= not <
    static inline int spillSlot(int r) { return -r - 1000; }       // r <= -1000
    
    inline int spillByteOffset(int slot) const {
        return SpillBaseOffset + 4 * slot;  // byte address within frame
    }
    
    // ========== Data Structures for PHI Handling ==========
    
    struct CFGEdge {
        BasicBlock* from;
        BasicBlock* to;
        int edge_index;
        
        bool operator<(const CFGEdge& other) const {
            if (from != other.from) return from < other.from;
            if (to != other.to) return to < other.to;
            return edge_index < other.edge_index;
        }
    };
    
    struct ParallelCopy {
        Value* src_value;
        Value* dest_value;
        int src_reg;
        int dest_reg;
    };
    
    // Note: Removed dead Move struct - PHI resolution is handled by resolveParallelCopies()
    
    // ========== Compiler State ==========
    
    std::unique_ptr<Module> TheModule;
    std::map<Value*, int> ValueRegisterMap;
    // No longer track global addresses - linker assigns them
    
    // Enhanced to handle both strings and numeric arrays
    struct GlobalDataEntry {
        GlobalVariable* gv;
        std::string uniqueName;  // Unique name to avoid conflicts
        enum { STRING, NUMERIC_ARRAY, OTHER } type;
        std::string stringData;  // For STRING type
        std::vector<uint32_t> numericData;  // For NUMERIC_ARRAY type
    };
    std::vector<GlobalDataEntry> GlobalData;
    int GlobalStringCounter = 0;  // Counter for unique string labels
    std::map<GlobalVariable*, std::string> GlobalNameMap;  // Map GV to unique name
    std::set<GlobalVariable*> GlobalsEmitted;  // Track which globals have been loaded
    
    std::map<BasicBlock*, std::string> BlockLabels;
    std::map<CFGEdge, std::vector<ParallelCopy>> EdgeCopies;
    
    struct PendingSplitBlock {
        std::string label;
        BasicBlock* from;
        BasicBlock* to;
    };
    std::vector<PendingSplitBlock> pendingSplits;
    std::set<CFGEdge> EmittedPC;  // Track already emitted PHI copies
    
    // Register allocation: r12-r27 for general use
    // r11 = reserved scratch register for codegen
    // r28 = reserved for PHI/cycle breaking temp
    // r29 = sp, r30 = fp, r31 = lr
    int NextReg = 12;
    int NextSpillSlot = 0;
    int StackAllocated = 0;
    int CurrentFrameSize = 0;  // Track actual frame size for epilogue
    // No longer need to track data addresses - linker will assign them
    // uint32_t NextDataAddr = 0x200000;  // REMOVED - linker handles this
    
    // Alloca and spill management
    std::map<AllocaInst*, int> AllocaOffset;  // byte offset from fp
    int AllocaBytes = 0;
    int SpillBaseOffset = 0;  // byte offset where spill slots start (from fp)
    
    // ======== Small helpers ========
    // Emit a full 32-bit immediate into a register using the same
    // HI20/LO12 signed split as the linker (LO12 sign-extended).
    void emitLoadImm32(int rd, int64_t val) {
        if (val >= -2048 && val <= 2047) {
            outs() << "    addi r" << rd << ", r0, " << (int32_t)val << "\n";
            return;
        }
        int32_t lo = (int32_t)((val << 20) >> 20);            // signed low 12
        int32_t hi = (int32_t)((val - lo) >> 12);             // carry-aware hi
        outs() << "    lui r" << rd << ", " << hi << "\n";
        outs() << "    addi r" << rd << ", r" << rd << ", " << lo << "\n";
    }
    
    // ========== Register Allocation ==========
    
    int allocateRegister(Value* V) {
        if (ValueRegisterMap.find(V) != ValueRegisterMap.end()) {
            return ValueRegisterMap[V];
        }
        
        if (NextReg <= 27) {  // Reserve r11 for scratch, r28 for PHI temp, r29=sp, r30=fp, r31=lr
            ValueRegisterMap[V] = NextReg++;
            return ValueRegisterMap[V];
        } else {
            // Spill to stack
            int slot = NextSpillSlot++;
            ValueRegisterMap[V] = -1000 - slot;
            return ValueRegisterMap[V];
        }
    }
    
    int getReg(Value* V) {
        if (auto* CI = dyn_cast<ConstantInt>(V)) {
            if (CI->getSExtValue() == 0) return 0;
            int reg = 2;  // Use r2 as temp for constants
            int64_t val = CI->getSExtValue();
            emitLoadImm32(reg, val);
            return reg;
        }
        
        // Handle global variables - load their address symbolically
        if (auto* GV = dyn_cast<GlobalVariable>(V)) {
            // Check if already allocated
            auto it = ValueRegisterMap.find(V);
            if (it != ValueRegisterMap.end()) {
                // Already allocated - check if we've emitted load code
                if (GlobalsEmitted.find(GV) == GlobalsEmitted.end()) {
                    // First time using this global - emit load code
                    GlobalsEmitted.insert(GV);
                    std::string name = GlobalNameMap.count(GV) ? GlobalNameMap[GV] : GV->getName().str();
                    int reg = it->second;
                    int tmp = isSpill(reg) ? 2 : reg;
                    
                    outs() << "    lui r" << tmp << ", %hi(" << name << ")\n";
                    outs() << "    addi r" << tmp << ", r" << tmp << ", %lo(" << name << ")\n";
                    
                    if (isSpill(reg)) {
                        storeIfSpilled(V, tmp);
                        return 2;
                    }
                    return tmp;
                } else {
                    // Already loaded - just return register
                    int reg = it->second;
                    if (isSpill(reg)) {
                        outs() << "    ldw r2, fp+" << spillByteOffset(spillSlot(reg)) << "  # reload\n";
                        return 2;
                    }
                    return reg;
                }
            } else {
                // First allocation - emit load code
                std::string name = GlobalNameMap.count(GV) ? GlobalNameMap[GV] : GV->getName().str();
                int reg = allocateRegister(V);
                int tmp = (reg >= 0) ? reg : 2;
                
                GlobalsEmitted.insert(GV);
                
                // Emit symbolic address loading - let the linker resolve it
                outs() << "    lui r" << tmp << ", %hi(" << name << ")\n";
                outs() << "    addi r" << tmp << ", r" << tmp << ", %lo(" << name << ")\n";
                
                if (isSpill(reg)) {
                    storeIfSpilled(V, tmp);
                    return 2;
                }
                return tmp;
            }
        }
        
        auto it = ValueRegisterMap.find(V);
        if (it == ValueRegisterMap.end()) {
            int r = allocateRegister(V);
            if (isSpill(r)) {
                outs() << "    ldw r2, fp+" << spillByteOffset(spillSlot(r)) << "  # reload\n";
                return 2;
            }
            return r;
        }
        
        int r = it->second;
        // Check for stack arg sentinel: -2000 - argIndex
        if (r <= -2000) {
            int argIndex = -2000 - r;
            if (argIndex >= 8) {
                // Incoming stack args sit above our frame: fp + CurrentFrameSize + 4*(argIndex-8)
                int off = CurrentFrameSize + 4 * (argIndex - 8);
                outs() << "    ldw r2, fp+" << off << "  # incoming stack arg\n";
                return 2;
            }
        }
        if (isSpill(r)) {
            outs() << "    ldw r2, fp+" << spillByteOffset(spillSlot(r)) << "  # reload\n";
            return 2;
        }
        return r;
    }
    
    // Load value into specific scratch register (for avoiding collisions)
    int getInto(Value* V, int scratch) {
        if (auto* CI = dyn_cast<ConstantInt>(V)) {
            int64_t val = CI->getSExtValue();
            if (val == 0) return 0;
            emitLoadImm32(scratch, val);
            return scratch;
        }
        
        // Handle globals specially - load address into scratch register
        if (auto* GV = dyn_cast<GlobalVariable>(V)) {
            std::string name = GlobalNameMap.count(GV) ? GlobalNameMap[GV] : GV->getName().str();
            outs() << "    lui r" << scratch << ", %hi(" << name << ")\n";
            outs() << "    addi r" << scratch << ", r" << scratch << ", %lo(" << name << ")\n";
            return scratch;
        }
        
        auto it = ValueRegisterMap.find(V);
        int r = (it == ValueRegisterMap.end()) ? allocateRegister(V) : it->second;
        if (isSpill(r)) {
            outs() << "    ldw r" << scratch << ", fp+" << spillByteOffset(spillSlot(r)) << "  # reload\n";
            return scratch;
        }
        return r;
    }
    
    void storeIfSpilled(Value* V, int tempReg) {
        if (isSpill(ValueRegisterMap[V])) {
            int slot = spillSlot(ValueRegisterMap[V]);
            outs() << "    stw fp+" << spillByteOffset(slot) << ", r" << tempReg << "  # spill\n";
        }
    }
    
    // ========== PHI Linearization ==========
    
    // Old PHI mover removed - now using lowerParallelCopy exclusively
    // which correctly uses r28 as TMP for cycle breaking
    
    // ========== Code Generation ==========
    
    std::string getBlockLabel(BasicBlock* BB) {
        if (BlockLabels.find(BB) == BlockLabels.end()) {
            std::stringstream ss;
            ss << ".L_" << BB;
            BlockLabels[BB] = ss.str();
        }
        return BlockLabels[BB];
    }
    
    void collectPHICopies(Function& F) {
        std::vector<CFGEdge> edges = enumerateEdges(F);
        
        for (const CFGEdge& edge : edges) {
            std::vector<ParallelCopy> copies;
            
            for (Instruction& I : *edge.to) {
                auto* phi = dyn_cast<PHINode>(&I);
                if (!phi) break;
                
                Value* incoming = phi->getIncomingValueForBlock(edge.from);
                if (incoming) {
                    ParallelCopy copy;
                    copy.src_value = incoming;
                    copy.dest_value = phi;
                    
                    // Get registers (don't allocate yet)
                    if (isa<ConstantInt>(incoming)) {
                        copy.src_reg = -1;
                    } else {
                        // Check if already allocated, otherwise mark for later
                        auto it = ValueRegisterMap.find(incoming);
                        if (it != ValueRegisterMap.end()) {
                            copy.src_reg = it->second;
                        } else {
                            copy.src_reg = UNALLOC;  // Mark as unallocated
                        }
                    }
                    
                    // PHI dest always gets allocated
                    auto it = ValueRegisterMap.find(phi);
                    if (it != ValueRegisterMap.end()) {
                        copy.dest_reg = it->second;
                    } else {
                        copy.dest_reg = allocateRegister(phi);
                    }
                    
                    copies.push_back(copy);
                }
            }
            
            if (!copies.empty()) {
                EdgeCopies[edge] = copies;
            }
        }
    }
    
    std::vector<CFGEdge> enumerateEdges(Function& F) {
        std::vector<CFGEdge> edges;
        
        for (BasicBlock& BB : F) {
            auto* term = BB.getTerminator();
            if (!term) continue;
            
            for (unsigned i = 0; i < term->getNumSuccessors(); i++) {
                BasicBlock* succ = term->getSuccessor(i);
                CFGEdge edge;
                edge.from = &BB;
                edge.to = succ;
                edge.edge_index = i;
                edges.push_back(edge);
            }
        }
        
        return edges;
    }
    
    static int getEdgeIndex(BasicBlock* from, BasicBlock* to) {
        auto* term = from->getTerminator();
        for (unsigned i = 0; i < term->getNumSuccessors(); ++i) {
            if (term->getSuccessor(i) == to) return (int)i;
        }
        return 0; // fallback
    }
    
    bool alreadyEmitted(const CFGEdge& e) const { return EmittedPC.count(e); }
    void markEmitted(const CFGEdge& e) { EmittedPC.insert(e); }
    
    void emitPHIMovesOnce(BasicBlock* from, BasicBlock* to, int edge_index) {
        CFGEdge e{from, to, edge_index};
        if (alreadyEmitted(e)) return;
        emitPHIMoves(from, to, edge_index);
        markEmitted(e);
    }
    
    void lowerParallelCopy(std::vector<ParallelCopy> copies) {
        auto destIsSource = [&](int dest) -> bool {
            for (auto& c : copies) if (c.src_reg == dest) return true;
            return false;
        };
        
        const int TMP = 28; // cycle-break tmp (r28 reserved, r29=sp, r30=fp, r31=lr)
        
        // 1) Remove trivial self-copies (reg->same reg)
        copies.erase(
            std::remove_if(copies.begin(), copies.end(),
                [](const ParallelCopy& c){
                    return c.src_reg >= 0 && c.dest_reg >= 0 && c.src_reg == c.dest_reg;
                }),
            copies.end());
        
        while (!copies.empty()) {
            bool progress = false;
            
            // 2) Emit any copy whose DEST is not used as a SOURCE
            for (size_t i = 0; i < copies.size(); ++i) {
                auto c = copies[i];
                if (destIsSource(c.dest_reg)) continue;
                
                if (isa<ConstantInt>(c.src_value)) {
                    // const -> reg or const -> spill
                    int64_t val = cast<ConstantInt>(c.src_value)->getSExtValue();
                    if (!isSpill(c.dest_reg)) {
                        // const -> reg
                        emitLoadImm32(c.dest_reg, val);
                        outs() << "    # PHI const\n";
                    } else {
                        // const -> spill (materialize in TMP then store)
                        emitLoadImm32(TMP, val);
                        outs() << "    # PHI const\n";
                        outs() << "    stw fp+" << spillByteOffset(spillSlot(c.dest_reg)) << ", r" << TMP << "  # PHI spill\n";
                    }
                } else if (isSpill(c.src_reg)) {
                    // spill -> reg or spill -> spill
                    int s = spillSlot(c.src_reg);
                    if (!isSpill(c.dest_reg)) {
                        // spill -> reg: load straight into dest
                        outs() << "    ldw r" << c.dest_reg << ", fp+" << spillByteOffset(s) << "  # PHI reload\n";
                    } else {
                        // spill -> spill: TMP bridge
                        outs() << "    ldw r" << TMP << ", fp+" << spillByteOffset(s) << "  # PHI reload\n";
                        outs() << "    stw fp+" << spillByteOffset(spillSlot(c.dest_reg)) << ", r" << TMP << "  # PHI spill\n";
                    }
                } else {
                    // reg -> reg or reg -> spill
                    if (!isSpill(c.dest_reg)) {
                        outs() << "    add r" << c.dest_reg << ", r" << c.src_reg << ", r0  # PHI move\n";
                    } else {
                        outs() << "    stw fp+" << spillByteOffset(spillSlot(c.dest_reg)) << ", r" << c.src_reg << "  # PHI spill\n";
                    }
                }
                
                copies.erase(copies.begin() + i);
                progress = true;
                break;
            }
            
            if (progress) continue;
            
            // 3) Cycle remains. Must be all REG->REG. Break with TMP.
            // pick one edge s->d
            auto &first = copies[0];
            assert(first.src_reg >= 0 && first.dest_reg >= 0 && "only reg cycles should remain");
            outs() << "    add r" << TMP << ", r" << first.src_reg << ", r0  # PHI save\n";
            int saved = first.src_reg;
            // replace all occurrences of saved as a src
            for (auto& c : copies) if (c.src_reg == saved) c.src_reg = TMP;
        }
    }
    
    void emitPHIMoves(BasicBlock* from, BasicBlock* to, int edge_index) {
        CFGEdge edge{from, to, edge_index};
        auto it = EdgeCopies.find(edge);
        if (it == EdgeCopies.end()) return;
        
        auto copies = it->second;
        
        // Allocate any late-bound regs
        for (auto& c : copies) {
            if (isUnalloc(c.src_reg)) c.src_reg = allocateRegister(c.src_value);
            if (c.dest_reg == 0) {} // ok
        }
        
        // Use the new unified lowering
        lowerParallelCopy(copies);
    }
    
    void compileInstruction(Instruction* I);
    void compileBinaryOp(BinaryOperator* BI);
    void compileComparison(ICmpInst* CI);
    void compileBranch(BranchInst* BI);
    void compileReturn(ReturnInst* RI);
    void compileCall(CallInst* CI);
    void compileLoad(LoadInst* LI);
    void compileStore(StoreInst* SI);
    void compileAlloca(AllocaInst* AI);
    void compileGEP(GetElementPtrInst* GEP);
    bool isCriticalEdge(BasicBlock* from, BasicBlock* to);
    
    void preallocateAll(Function& F) {
        // PHI dests were allocated in collectPHICopies(); leave them.
        for (BasicBlock& BB : F) {
            for (Instruction& I : BB) {
                if (isa<PHINode>(&I)) continue;            // already handled
                if (I.getType()->isVoidTy()) continue;     // no result
                // Ensure this SSA value has a home now (reg or spill)
                (void)allocateRegister(&I);
            }
        }
    }
    
    void compileFunction(Function& F) {
        outs() << "# Function: " << F.getName() << "\n";
        outs() << ".global " << F.getName() << "\n";
        outs() << F.getName() << ":\n";
        
        // Reset state
        ValueRegisterMap.clear();
        BlockLabels.clear();
        EdgeCopies.clear();
        GlobalsEmitted.clear();
        pendingSplits.clear();
        EmittedPC.clear();  // Clear emitted PHI copies tracking
        NextReg = 12;
        NextSpillSlot = 0;
        StackAllocated = 0;
        
        // Map arguments to registers
        int argReg = 3;
        for (auto& Arg : F.args()) {
            if (argReg <= 10) {
                ValueRegisterMap[&Arg] = argReg++;
            } else {
                // Arguments beyond r10 are on stack
                ValueRegisterMap[&Arg] = -2000 - (&Arg - F.arg_begin());
            }
        }
        
        // Collect PHI copies
        collectPHICopies(F);
        
        // Preallocate all registers/spills to freeze NextSpillSlot
        preallocateAll(F);
        
        // Calculate alloca offsets and stack frame size
        AllocaBytes = 0;
        AllocaOffset.clear();
        for (BasicBlock& BB : F) {
            for (Instruction& I : BB) {
                if (auto* AI = dyn_cast<AllocaInst>(&I)) {
                    // Use DataLayout to get correct size for the allocated type
                    auto bytes = (int)TheModule->getDataLayout().getTypeAllocSize(AI->getAllocatedType());
                    bytes = (bytes + 3) & ~3;  // Keep 4-byte stack alignment
                    // Assign fixed offset right after lr/fp area (0..7 reserved)
                    AllocaOffset[AI] = 8 + AllocaBytes;
                    AllocaBytes += bytes;
                }
            }
        }
        
        SpillBaseOffset = 8 + AllocaBytes;  // spills live after allocas (no extra hole)
        int frameSize = SpillBaseOffset + NextSpillSlot * 4;
        frameSize = (frameSize + 15) & ~15;  // Align to 16
        CurrentFrameSize = frameSize;  // Store for epilogue
        
        // Prologue
        outs() << "    addi sp, sp, -" << frameSize << "\n";
        outs() << "    stw sp+0, lr\n";
        outs() << "    stw sp+4, fp\n";
        outs() << "    add fp, sp, r0\n";
        
        // Compile basic blocks
        for (BasicBlock& BB : F) {
            outs() << getBlockLabel(&BB) << ":\n";
            
            // At block prologue: if this block has exactly one predecessor, 
            // emit its edge copies here (they're safe to execute unconditionally)
            int num_preds = std::distance(pred_begin(&BB), pred_end(&BB));
            if (num_preds == 1) {
                BasicBlock* pred = *pred_begin(&BB);
                int edge_idx = getEdgeIndex(pred, &BB);
                emitPHIMovesOnce(pred, &BB, edge_idx);
            }
            
            for (Instruction& I : BB) {
                compileInstruction(&I);
            }
            
            // Emit split blocks for this block's critical edges immediately
            for (auto it = pendingSplits.begin(); it != pendingSplits.end(); ) {
                if (it->from == &BB) {
                    outs() << it->label << ":  # Critical edge split\n";
                    int edge_idx = getEdgeIndex(it->from, it->to);
                    emitPHIMovesOnce(it->from, it->to, edge_idx);
                    outs() << "    beq r0, r0, " << getBlockLabel(it->to) << "\n";
                    it = pendingSplits.erase(it);
                } else {
                    ++it;
                }
            }
        }
    }
    
    void processGlobals() {
        // Get module name for unique prefixing
        std::string modName = TheModule->getModuleIdentifier();
        // Remove .c or .ll extension and sanitize
        size_t dot = modName.rfind('.');
        if (dot != std::string::npos) {
            modName = modName.substr(0, dot);
        }
        // Replace any non-alphanumeric with underscore
        for (char& c : modName) {
            if (!isalnum(c)) c = '_';
        }
        
        for (auto& GV : TheModule->globals()) {
            if (GV.hasInitializer()) {
                // No longer track addresses - just collect data
                
                if (auto* CA = dyn_cast<ConstantDataArray>(GV.getInitializer())) {
                    // Check if this is a string (array of i8)
                    if (CA->isString()) {
                        GlobalDataEntry entry;
                        entry.gv = &GV;
                        // Create unique name for string constants with module prefix
                        entry.uniqueName = ".L_" + modName + "_str_" + std::to_string(GlobalStringCounter++);
                        entry.type = GlobalDataEntry::STRING;
                        entry.stringData = CA->getAsString().str();
                        GlobalNameMap[&GV] = entry.uniqueName;  // Map for later reference
                        GlobalData.push_back(entry);
                        // Linker will handle size and alignment
                    } else {
                        // Handle numeric arrays (like our CRC table)
                        GlobalDataEntry entry;
                        entry.gv = &GV;
                        entry.uniqueName = GV.getName().str();  // Keep original name for non-strings
                        GlobalNameMap[&GV] = entry.uniqueName;  // Map for later reference
                        entry.type = GlobalDataEntry::NUMERIC_ARRAY;
                        
                        Type* elemType = CA->getElementType();
                        unsigned numElems = CA->getNumElements();
                        
                        // Extract numeric data based on element type
                        if (elemType->isIntegerTy(32)) {
                            for (unsigned i = 0; i < numElems; i++) {
                                uint64_t val = CA->getElementAsInteger(i);
                                entry.numericData.push_back(static_cast<uint32_t>(val));
                            }
                            // Size tracked by linker
                        } else if (elemType->isIntegerTy(8)) {
                            // Handle byte arrays
                            for (unsigned i = 0; i < numElems; i++) {
                                uint64_t val = CA->getElementAsInteger(i);
                                entry.numericData.push_back(static_cast<uint32_t>(val));
                            }
                            // Size tracked by linker
                        } else {
                            // Other integer types - just reserve space for now
                            unsigned elemSize = elemType->getPrimitiveSizeInBits() / 8;
                            // Size tracked by linker
                        }
                        
                        GlobalData.push_back(entry);
                        // Linker handles alignment
                    }
                } else if (auto* C = dyn_cast<Constant>(GV.getInitializer())) {
                    // Handle other constant types (structs, etc.)
                    // For now, just reserve space based on type size
                    Type* type = C->getType();
                    unsigned size = TheModule->getDataLayout().getTypeAllocSize(type);
                    // Linker handles size and alignment
                }
            }
        }
    }
    
    void emitDataSection() {
        if (GlobalData.empty()) return;
        
        // Separate strings (go to .rodata) from data (go to .data)
        bool hasStrings = false;
        bool hasData = false;
        
        for (const auto& entry : GlobalData) {
            if (entry.type == GlobalDataEntry::STRING) {
                hasStrings = true;
            } else {
                hasData = true;
            }
        }
        
        // Emit .rodata section for string constants
        if (hasStrings) {
            outs() << "\n.rodata\n";
            for (const auto& entry : GlobalData) {
                if (entry.type != GlobalDataEntry::STRING) continue;
                
                // Only emit .global for externally visible symbols
                // Local symbols will be added to symbol table by assembler if referenced
                if (entry.gv->hasExternalLinkage()) {
                    outs() << ".global " << entry.uniqueName << "\n";
                }
                outs() << entry.uniqueName << ":\n";
                
                outs() << ".string \"";
                // Escape special characters in string
                // .string directive adds null terminator, so stop at first \0
                for (char c : entry.stringData) {
                    if (c == '\0') break;  // .string adds null terminator
                    switch (c) {
                        case '\n': outs() << "\\n"; break;
                        case '\r': outs() << "\\r"; break;
                        case '\t': outs() << "\\t"; break;
                        case '\\': outs() << "\\\\"; break;
                        case '"':  outs() << "\\\""; break;
                        default:
                            if (c >= 32 && c < 127) {
                                outs() << c;
                            } else {
                                outs() << format("\\x%02x", (unsigned char)c);
                            }
                            break;
                    }
                }
                outs() << "\"\n";
            }
        }
        
        // Emit .data section for numeric data
        if (hasData) {
            outs() << "\n.data\n";
            for (const auto& entry : GlobalData) {
                if (entry.type == GlobalDataEntry::STRING) continue;
                
                // Only emit .global for externally visible symbols
                // Local symbols will be added to symbol table by assembler if referenced
                if (entry.gv->hasExternalLinkage()) {
                    outs() << ".global " << entry.uniqueName << "\n";
                }
                outs() << entry.uniqueName << ":\n";
                
                switch (entry.type) {
                    case GlobalDataEntry::NUMERIC_ARRAY:
                        // Check element type to decide directive
                        if (entry.gv && entry.gv->getInitializer()) {
                            if (auto* CA = dyn_cast<ConstantDataArray>(entry.gv->getInitializer())) {
                                Type* elemType = CA->getElementType();
                                if (elemType->isIntegerTy(8)) {
                                    // Emit i8 arrays as .byte directives
                                    for (size_t i = 0; i < entry.numericData.size(); i++) {
                                        if (i % 16 == 0) {
                                            if (i > 0) outs() << "\n";
                                            outs() << "    .byte ";
                                        } else {
                                            outs() << ", ";
                                        }
                                        outs() << format("0x%02x", entry.numericData[i] & 0xFF);
                                    }
                                    outs() << "\n";
                                    // Add alignment for subsequent data
                                    outs() << "    .align 4\n";
                                } else {
                                    // Emit other numeric types as .word directives
                                    outs() << "    .align 4\n";  // Ensure alignment before words
                                    for (size_t i = 0; i < entry.numericData.size(); i++) {
                                        if (i % 4 == 0) {
                                            if (i > 0) outs() << "\n";
                                            outs() << "    .word ";
                                        } else {
                                            outs() << ", ";
                                        }
                                        outs() << format("0x%08x", entry.numericData[i]);
                                    }
                                    outs() << "\n";
                                }
                            } else {
                                // Fallback for non-ConstantDataArray
                                for (size_t i = 0; i < entry.numericData.size(); i++) {
                                    if (i % 4 == 0) {
                                        if (i > 0) outs() << "\n";
                                        outs() << "    .word ";
                                    } else {
                                        outs() << ", ";
                                    }
                                    outs() << format("0x%08x", entry.numericData[i]);
                                }
                                outs() << "\n";
                            }
                        }
                        break;
                        
                    case GlobalDataEntry::OTHER:
                        outs() << "    # TODO: Emit other constant type\n";
                        break;
                        
                    case GlobalDataEntry::STRING:
                        // Already handled above
                        break;
                }
            }
        }
    }
    
public:
    SLOW32Compiler(std::unique_ptr<Module> M) : TheModule(std::move(M)) {}
    
    void compile() {
        outs() << "# SLOW-32 Assembly (v3 with correct PHI handling)\n\n";
        
        processGlobals();
        
        // Never emit _start - that comes from crt0.s
        
        for (Function& F : *TheModule) {
            if (!F.isDeclaration()) {
                compileFunction(F);
                outs() << "\n";
            }
        }
        
        emitDataSection();
    }
};

// ========== Instruction Compilation ==========

void SLOW32Compiler::compileInstruction(Instruction* I) {
    // ---- Lower llvm.va_start / llvm.va_end as inline sequences ----
    if (auto *CI = dyn_cast<CallInst>(I)) {
        if (Function *Callee = CI->getCalledFunction()) {
            auto Name = Callee->getName();
            // llvm.va_start.*(i8* ap)
            if (Name.starts_with("llvm.va_start")) {
                // ap in arg0: store (fp + CurrentFrameSize) into *ap
                int ap = getReg(CI->getArgOperand(0));
                int tmp = 11; // scratch
                outs() << "    addi r" << tmp << ", fp, " << CurrentFrameSize << "  # ap = entry SP\n";
                outs() << "    stw r" << ap << "+0, r" << tmp << "\n";
                return; // no real call
            }
            // llvm.va_end.*(i8* ap) — nothing to do
            if (Name.starts_with("llvm.va_end")) {
                return;
            }
        }
    }

    // ---- Lower 'va_arg' for 32-bit types ----
    if (auto *VAI = dyn_cast<VAArgInst>(I)) {
        // Operand is 'ap' (pointer to va_list storage). Our va_list is a single pointer.
        //   cur = *(ap)
        //   result = *(cur)
        //   *(ap) = cur + 4
        int ap = getReg(VAI->getPointerOperand());
        int cur = 11; // scratch
        outs() << "    ldw r" << cur << ", r" << ap << "+0\n";

        int dst = allocateRegister(I);
        int dd  = isSpill(dst) ? 2 : dst;
        outs() << "    ldw r" << dd << ", r" << cur << "+0\n";
        outs() << "    addi r" << cur << ", r" << cur << ", 4\n";
        outs() << "    stw r" << ap << "+0, r" << cur << "\n";
        if (isSpill(dst)) storeIfSpilled(I, 2);
        return;
    }
    
    if (isa<PHINode>(I)) {
        return;  // PHI nodes handled via edge copies
    }
    
    if (auto* AI = dyn_cast<AllocaInst>(I)) {
        compileAlloca(AI);
    } else if (auto* SI = dyn_cast<StoreInst>(I)) {
        compileStore(SI);
    } else if (auto* LI = dyn_cast<LoadInst>(I)) {
        compileLoad(LI);
    } else if (auto* BI = dyn_cast<BinaryOperator>(I)) {
        compileBinaryOp(BI);
    } else if (auto* CI = dyn_cast<ICmpInst>(I)) {
        compileComparison(CI);
    } else if (auto* BI = dyn_cast<BranchInst>(I)) {
        compileBranch(BI);
    } else if (auto* RI = dyn_cast<ReturnInst>(I)) {
        compileReturn(RI);
    } else if (auto* CI = dyn_cast<CallInst>(I)) {
        compileCall(CI);
    } else if (auto* GEP = dyn_cast<GetElementPtrInst>(I)) {
        compileGEP(GEP);
    } else if (auto* ZI = dyn_cast<ZExtInst>(I)) {
        // Zero extend with proper masking
        Value* Op = ZI->getOperand(0);
        int srcReg = getReg(Op);
        int destReg = allocateRegister(ZI);
        int w = ZI->getSrcTy()->getIntegerBitWidth();
        int tmpReg = isSpill(destReg) ? 2 : destReg;
        
        if (w == 8) {
            outs() << "    andi r" << tmpReg << ", r" << srcReg << ", 255\n";
        } else if (w == 16) {
            // Use shift pair for 16-bit mask since andi can't encode 65535
            outs() << "    slli r" << tmpReg << ", r" << srcReg << ", 16\n";
            outs() << "    srli r" << tmpReg << ", r" << tmpReg << ", 16\n";
        } else {
            // 32-bit, just copy
            outs() << "    add r" << tmpReg << ", r" << srcReg << ", r0\n";
        }
        if (isSpill(destReg)) storeIfSpilled(ZI, tmpReg);
    } else if (auto* SI = dyn_cast<SExtInst>(I)) {
        // Sign extend with shift-left/right pair
        Value* Op = SI->getOperand(0);
        int srcReg = getReg(Op);
        int destReg = allocateRegister(SI);
        int w = SI->getSrcTy()->getIntegerBitWidth();
        int tmpReg = isSpill(destReg) ? 2 : destReg;
        
        if (w < 32) {
            int sh = 32 - w;
            outs() << "    slli r" << tmpReg << ", r" << srcReg << ", " << sh << "\n";
            outs() << "    srai r" << tmpReg << ", r" << tmpReg << ", " << sh << "\n";
        } else {
            // 32-bit, just copy
            outs() << "    add r" << tmpReg << ", r" << srcReg << ", r0\n";
        }
        if (isSpill(destReg)) storeIfSpilled(SI, tmpReg);
    } else if (auto* TI = dyn_cast<TruncInst>(I)) {
        // Truncate with proper masking to keep only low bits
        Value* Op = TI->getOperand(0);
        int srcReg = getReg(Op);
        int destReg = allocateRegister(TI);
        int w = TI->getType()->getIntegerBitWidth();
        int tmpReg = isSpill(destReg) ? 2 : destReg;
        
        if (w == 8) {
            outs() << "    andi r" << tmpReg << ", r" << srcReg << ", 255\n";
        } else if (w == 16) {
            // Use shift pair for 16-bit mask since andi can't encode 65535
            outs() << "    slli r" << tmpReg << ", r" << srcReg << ", 16\n";
            outs() << "    srli r" << tmpReg << ", r" << tmpReg << ", 16\n";
        } else {
            // 32-bit, just copy
            outs() << "    add r" << tmpReg << ", r" << srcReg << ", r0\n";
        }
        if (isSpill(destReg)) storeIfSpilled(TI, tmpReg);
    }
}

void SLOW32Compiler::compileAlloca(AllocaInst* AI) {
    int dest = allocateRegister(AI);
    int off = AllocaOffset[AI];  // byte offset from fp
    int tmp = (dest >= 0) ? dest : 2;  // use r2 if spilled
    outs() << "    addi r" << tmp << ", fp, " << off << "\n";
    if (isSpill(dest)) storeIfSpilled(AI, tmp);
}

void SLOW32Compiler::compileStore(StoreInst* SI) {
    Value* Val = SI->getValueOperand();
    Value* Ptr = SI->getPointerOperand();
    int valReg = getReg(Val);
    int ptrReg = getReg(Ptr);
    
    auto *VTy = Val->getType();
    const char* storeOp = "stw";
    if (VTy->isIntegerTy(8))  storeOp = "stb";
    else if (VTy->isIntegerTy(16)) storeOp = "sth";
    
    outs() << "    " << storeOp << " r" << ptrReg << "+0, r" << valReg << "\n";
}

void SLOW32Compiler::compileLoad(LoadInst* LI) {
    Value* Ptr = LI->getPointerOperand();
    int ptrReg = getReg(Ptr);
    int destReg = allocateRegister(LI);
    
    const char* loadOp = "ldw";
    Type* LoadType = LI->getType();
    if (LoadType->isIntegerTy(8)) {
        loadOp = "ldbu";
    } else if (LoadType->isIntegerTy(16)) {
        loadOp = "ldhu";
    }
    
    if (isSpill(destReg)) {
        outs() << "    " << loadOp << " r2, r" << ptrReg << "+0\n";
        storeIfSpilled(LI, 2);
    } else {
        outs() << "    " << loadOp << " r" << destReg << ", r" << ptrReg << "+0\n";
    }
}

void SLOW32Compiler::compileBinaryOp(BinaryOperator* BI) {
    Value* Op0 = BI->getOperand(0);
    Value* Op1 = BI->getOperand(1);
    
    // Use getInto to avoid collision when both operands are spilled
    // IMPORTANT: Don't use r3-r10 as scratch - they hold parameters!
    int reg0 = getInto(Op0, 2);
    int reg1 = getInto(Op1, 11);  // Use r11, not r3!
    int destReg = allocateRegister(BI);
    
    const char* op = nullptr;
    switch (BI->getOpcode()) {
        case Instruction::Add: op = "add"; break;
        case Instruction::Sub: op = "sub"; break;
        case Instruction::Mul: op = "mul"; break;
        case Instruction::SDiv: op = "div"; break;
        case Instruction::SRem: op = "rem"; break;
        case Instruction::Shl: op = "sll"; break;
        case Instruction::LShr: op = "srl"; break;
        case Instruction::AShr: op = "sra"; break;
        case Instruction::And: op = "and"; break;
        case Instruction::Or: op = "or"; break;
        case Instruction::Xor: op = "xor"; break;
        default: return;
    }
    
    if (isSpill(destReg)) {
        outs() << "    " << op << " r2, r" << reg0 << ", r" << reg1 << "\n";
        storeIfSpilled(BI, 2);
    } else {
        outs() << "    " << op << " r" << destReg << ", r" << reg0 << ", r" << reg1 << "\n";
    }
}

void SLOW32Compiler::compileComparison(ICmpInst* CI) {
    Value* Op0 = CI->getOperand(0);
    Value* Op1 = CI->getOperand(1);
    
    // Use getInto to avoid collision when both operands are spilled
    // IMPORTANT: Don't use r3-r10 as scratch - they hold parameters!
    int reg0 = getInto(Op0, 2);
    int reg1 = getInto(Op1, 11);  // Use r11, not r3!
    int destReg = allocateRegister(CI);
    
    const char* op = nullptr;
    switch (CI->getPredicate()) {
        case ICmpInst::ICMP_EQ: op = "seq"; break;
        case ICmpInst::ICMP_NE: op = "sne"; break;
        case ICmpInst::ICMP_SLT: op = "slt"; break;
        case ICmpInst::ICMP_SLE: op = "sle"; break;
        case ICmpInst::ICMP_SGT: op = "sgt"; break;
        case ICmpInst::ICMP_SGE: op = "sge"; break;
        case ICmpInst::ICMP_ULT: op = "sltu"; break;
        case ICmpInst::ICMP_ULE: op = "sleu"; break;
        case ICmpInst::ICMP_UGT: op = "sgtu"; break;
        case ICmpInst::ICMP_UGE: op = "sgeu"; break;
        default: return;
    }
    
    if (isSpill(destReg)) {
        outs() << "    " << op << " r2, r" << reg0 << ", r" << reg1 << "\n";
        storeIfSpilled(CI, 2);
    } else {
        outs() << "    " << op << " r" << destReg << ", r" << reg0 << ", r" << reg1 << "\n";
    }
}

void SLOW32Compiler::compileBranch(BranchInst* BI) {
    if (BI->isUnconditional()) {
        BasicBlock* Target = BI->getSuccessor(0);
        int edge_idx = getEdgeIndex(BI->getParent(), Target);
        
        // Check if this edge is critical and has PHI copies
        bool isCritical = isCriticalEdge(BI->getParent(), Target);
        CFGEdge edge;
        edge.from = BI->getParent();
        edge.to = Target;
        edge.edge_index = edge_idx;
        
        auto it = EdgeCopies.find(edge);
        bool hasPHICopies = (it != EdgeCopies.end() && !it->second.empty());
        
        if (isCritical && hasPHICopies) {
            // For critical edges, we need to defer PHI move emission
            // Store the split block info for later
            std::string splitLabel = getBlockLabel(BI->getParent()) + "_to_" + getBlockLabel(Target).substr(3);
            outs() << "    beq r0, r0, " << splitLabel << "\n";
            
            // Add to list of pending split blocks
            PendingSplitBlock split;
            split.label = splitLabel;
            split.from = BI->getParent();
            split.to = Target;
            pendingSplits.push_back(split);
        } else if (!isCritical && hasPHICopies) {
            // Unconditional edge - if predecessor has only one successor (true for unconditional),
            // it is always safe (and preferable) to emit here.
            emitPHIMovesOnce(BI->getParent(), Target, edge_idx);
            outs() << "    beq r0, r0, " << getBlockLabel(Target) << "\n";
        } else {
            // No PHI copies
            outs() << "    beq r0, r0, " << getBlockLabel(Target) << "\n";
        }
    } else {
        // Conditional branch
        Value* Cond = BI->getCondition();
        int condReg = getReg(Cond);
        BasicBlock* TrueBB = BI->getSuccessor(0);
        BasicBlock* FalseBB = BI->getSuccessor(1);
        
        // Helper lambda to compute final label (with split if needed)
        auto edgeLabel = [&](BasicBlock* from, BasicBlock* to) -> std::string {
            int idx = getEdgeIndex(from, to);
            CFGEdge e{from, to, idx};
            auto it = EdgeCopies.find(e);
            bool hasPHI = (it != EdgeCopies.end() && !it->second.empty());
            
            if (!hasPHI) return getBlockLabel(to);
            
            // Create split label and enqueue it
            std::string split = getBlockLabel(from) + "_to_" + getBlockLabel(to).substr(3);
            PendingSplitBlock sb;
            sb.label = split;
            sb.from = from;
            sb.to = to;
            pendingSplits.push_back(sb);
            return split;
        };
        
        std::string trueTarget = edgeLabel(BI->getParent(), TrueBB);
        std::string falseTarget = edgeLabel(BI->getParent(), FalseBB);
        
        // **Canonical beq pattern** - NEVER use bne!
        // If condition is 0 (false), branch to false target
        // Otherwise fall through to unconditional jump to true target
        outs() << "    beq r" << condReg << ", r0, " << falseTarget << "\n";
        outs() << "    beq r0, r0, " << trueTarget << "\n";
    }
}

bool SLOW32Compiler::isCriticalEdge(BasicBlock* from, BasicBlock* to) {
    if (!from || !to) return false;
    
    auto* term = from->getTerminator();
    if (!term) return false;
    
    int num_successors = term->getNumSuccessors();
    int num_predecessors = std::distance(pred_begin(to), pred_end(to));
    
    // Critical edge: predecessor has >1 successors AND successor has >1 predecessors
    return (num_successors > 1) && (num_predecessors > 1);
}

void SLOW32Compiler::compileReturn(ReturnInst* RI) {
    if (Value* RetVal = RI->getReturnValue()) {
        int retReg = getReg(RetVal);
        if (retReg != 1) {
            outs() << "    add r1, r" << retReg << ", r0\n";
        }
    }
    
    outs() << "    add sp, fp, r0\n";
    outs() << "    ldw lr, sp+0\n";
    outs() << "    ldw fp, sp+4\n";
    outs() << "    addi sp, sp, " << CurrentFrameSize << "\n";  // Use actual frame size
    outs() << "    jalr r0, lr, 0\n";  // Return instruction
}

void SLOW32Compiler::compileCall(CallInst* CI) {
    Function* Callee = CI->getCalledFunction();
    if (!Callee) return;
    
    // Check if this is a varargs call
    bool isVarArg = Callee->isVarArg();
    
    if (isVarArg) {
        // ===== Variadic call ABI =====
        //   - Pass FIXED params in r3..r10 (as usual).
        //   - Spill any FIXED params beyond 8 to the stack.
        //   - THEN push ONLY the unnamed varargs as one contiguous block.
        // Callee lowers va_start to: ap = (fp + CurrentFrameSize) == SP at entry,
        // which must point to the first unnamed vararg. Therefore the varargs
        // block must be the LAST thing we push before JAL.
        auto *FTy = Callee->getFunctionType();
        int fixed = (int)FTy->getNumParams();          // e.g., printf has 1 fixed param (fmt)
        int total = (int)CI->arg_size();
        int varcount = std::max(0, total - fixed);     // unnamed args count
        int stackExtra = std::max(0, fixed - 8);        // fixed args spilled

        // 1) Marshal FIXED params to r3..r10
        int argReg = 3;
        for (int i = 0; i < std::min(fixed, 8); ++i) {
            int src = getReg(CI->getArgOperand(i));
            if (src != argReg) outs() << "    add r" << argReg << ", r" << src << ", r0\n";
            argReg++;
        }

        // 2) Spill FIXED overflow (>=9th fixed arg) FIRST
        if (stackExtra > 0) {
            int bytes = stackExtra * 4;
            outs() << "    addi sp, sp, -" << bytes << "  # spill fixed args beyond 8\n";
            int off = 0;
            for (int i = 8; i < fixed; ++i) {
                int src = getReg(CI->getArgOperand(i));
                outs() << "    stw sp+" << off << ", r" << src << "\n";
                off += 4;
            }
        }

        // 3) Push the UNNAMED VARARGS block LAST so SP(entry) == first vararg
        if (varcount > 0) {
            int bytes = varcount * 4;
            outs() << "    addi sp, sp, -" << bytes << "  # varargs block (unnamed args)\n";
            int off = 0;
            for (int i = fixed; i < total; ++i) {
                int src = getReg(CI->getArgOperand(i));
                outs() << "    stw sp+" << off << ", r" << src << "\n";
                off += 4;
            }
        }

        // 4) Call
        outs() << "    jal " << Callee->getName() << "\n";

        // 5) Pop in reverse order: varargs block, then spilled fixed args
        if (varcount > 0)   outs() << "    addi sp, sp, " << (varcount*4) << "\n";
        if (stackExtra > 0) outs() << "    addi sp, sp, " << (stackExtra*4) << "\n";
    } else {
        // Regular calling convention: r3-r10 for first 8 args, then stack
        int totalArgs = CI->arg_size();
        int stackArgs = (totalArgs > 8) ? (totalArgs - 8) : 0;
        
        // Adjust stack for extra arguments if needed
        if (stackArgs > 0) {
            outs() << "    addi sp, sp, -" << (stackArgs * 4) << "  # space for extra args\n";
        }
        
        // Set up arguments using LLVM's args() API
        int argReg = 3;
        int stackOffset = 0;
        for (auto &Arg : CI->args()) {
            if (argReg <= 10) {
                // Pass in register
                int src = getInto(Arg.get(), argReg);  // Use getInto to avoid r2 reuse
                if (src != argReg) {
                    outs() << "    add r" << argReg << ", r" << src << ", r0\n";
                }
                argReg++;
            } else {
                // Pass on stack
                int src = getReg(Arg.get());
                outs() << "    stw sp+" << stackOffset << ", r" << src << "  # arg" << (argReg - 3) << "\n";
                stackOffset += 4;
                argReg++;
            }
        }
        
        outs() << "    jal " << Callee->getName() << "\n";
        
        // Restore stack if we pushed args
        if (stackArgs > 0) {
            outs() << "    addi sp, sp, " << (stackArgs * 4) << "  # restore stack\n";
        }
    }
    
    // Get return value
    if (!CI->getType()->isVoidTy()) {
        int destReg = allocateRegister(CI);
        if (isSpill(destReg)) {
            storeIfSpilled(CI, 1);
        } else if (destReg != 1) {
            outs() << "    add r" << destReg << ", r1, r0\n";
        }
    }
}

void SLOW32Compiler::compileGEP(GetElementPtrInst* GEP) {
    Value* Ptr = GEP->getPointerOperand();
    int ptrReg = getReg(Ptr);
    
    // If ptrReg is r2 (loaded from spill), save it to r12 before loading index
    // which might also need r2
    if (ptrReg == 2 && isSpill(allocateRegister(Ptr))) {
        outs() << "    add r12, r2, r0  # save ptr\n";
        ptrReg = 12;
    }
    
    int destReg = allocateRegister(GEP);
    auto *pointeeTy = GEP->getSourceElementType();
    
    // Handle [0, idx] pattern with proper element size scaling
    if (GEP->getNumIndices() == 2) {
        auto* idx1 = GEP->getOperand(1);
        auto* idx2 = GEP->getOperand(2);
        
        if (auto* CI = dyn_cast<ConstantInt>(idx1)) {
            if (CI->getSExtValue() == 0) {
                int idxReg = getReg(idx2);
                // For array types, we need the element type size, not the array size
                Type* elemTy = pointeeTy;
                if (auto* ArrayTy = dyn_cast<ArrayType>(pointeeTy)) {
                    elemTy = ArrayTy->getElementType();
                }
                uint64_t scale = TheModule->getDataLayout().getTypeAllocSize(elemTy);
                int tempReg = (destReg >= 0) ? destReg : 2;
                
                if (scale == 1) {
                    // No scaling needed
                    // Check if both ptrReg and idxReg are the same (both loaded from spills into r2)
                    if (ptrReg == idxReg && ptrReg == 2) {
                        // Save index to r12 to avoid clobbering
                        outs() << "    add r12, r" << idxReg << ", r0\n";
                        outs() << "    add r" << tempReg << ", r" << ptrReg << ", r12\n";
                    } else {
                        outs() << "    add r" << tempReg << ", r" << ptrReg << ", r" << idxReg << "\n";
                    }
                } else if ((scale & (scale-1)) == 0) {
                    // Power of 2, use shift
                    int sh = __builtin_ctzll(scale);
                    // Use r12 as scratch to avoid clobbering ptrReg
                    int shiftReg = (ptrReg == 2 && tempReg == 2) ? 12 : tempReg;
                    outs() << "    slli r" << shiftReg << ", r" << idxReg << ", " << sh << "\n";
                    outs() << "    add r" << tempReg << ", r" << ptrReg << ", r" << shiftReg << "\n";
                } else {
                    // General case: load scale into r11 then mul
                    int k = 11; // scratch register (reserved)
                    emitLoadImm32(k, (int64_t)scale);
                    // Use r12 as scratch for multiplication to avoid clobbering ptrReg
                    // (ptrReg might be r2 if it's spilled, and tempReg might also be r2)
                    int mulReg = 12;
                    outs() << "    mul r" << mulReg << ", r" << idxReg << ", r" << k << "\n";
                    outs() << "    add r" << tempReg << ", r" << ptrReg << ", r" << mulReg << "\n";
                }
                
                if (destReg < 0) {
                    storeIfSpilled(GEP, tempReg);
                }
                return;
            }
        }
    }
    
    // Fallback - just copy pointer
    if (destReg < 0) {
        outs() << "    add r2, r" << ptrReg << ", r0\n";
        storeIfSpilled(GEP, 2);
    } else {
        outs() << "    add r" << destReg << ", r" << ptrReg << ", r0\n";
    }
}

int main(int argc, char** argv) {
    if (argc != 2) {
        errs() << "Usage: " << argv[0] << " <input.ll>\n";
        return 1;
    }
    
    LLVMContext Context;
    SMDiagnostic Err;
    auto TheModule = parseIRFile(argv[1], Err, Context);
    
    if (!TheModule) {
        Err.print(argv[0], errs());
        return 1;
    }
    
    SLOW32Compiler compiler(std::move(TheModule));
    compiler.compile();
    
    return 0;
}