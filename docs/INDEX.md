# SLOW-32 Documentation Index

**Last Updated:** September 5, 2025  
**Version:** 1.0.0

## Quick Navigation

### 🚀 Getting Started
- [Installation](02-installation.md) - Download and install binaries
- [Quickstart](10-quickstart.md) - Hello World in 2 minutes
- [Getting Started](01-getting-started.md) - Complete setup guide
- [Examples](../examples/) - Sample programs

### 📖 Core Documentation
- [Architecture Rationale](05-architecture-rationale.md) - Why SLOW-32 exists
- [Overview](00-overview.md) - Philosophy and goals
- [Programmer's Guide](20-programmers-guide.md) - How to write SLOW-32 code
- [Instruction Set](25-instruction-set.md) - Complete ISA reference

### 🔧 Toolchain
- [Toolchain Overview](15-toolchain-overview.md) - All tools explained
- [LLVM Backend](35-llvm-backend.md) - C/C++ compilation

### 📊 Technical Details
- [Known Limitations](91-known-limitations.md) - Current constraints

### 📅 Planning
- [Roadmap](90-roadmap.md) - Future development
- [Articles](../articles/) - Substack and blog posts


## Learning Path

### For Beginners
1. Read [Architecture Rationale](05-architecture-rationale.md) - Understand the "why"
2. Follow [Installation](02-installation.md) - Get the tools
3. Try [Quickstart](10-quickstart.md) - Run your first program
4. Study [Examples](../examples/) - Learn by doing

### For Compiler Writers
1. Review [Instruction Set](25-instruction-set.md) - Target ISA
2. Study [Programmer's Guide](20-programmers-guide.md) - ABI details
3. Read [LLVM Backend](35-llvm-backend.md) - Implementation reference
4. Check [Known Limitations](91-known-limitations.md) - Current constraints

### For System Programmers
1. Understand [Object Format](30-object-format.md) - Binary layout
2. Master [Toolchain Overview](15-toolchain-overview.md) - Tool capabilities
3. Explore [Emulators](40-emulators.md) - Execution model
4. Review [Performance](60-performance.md) - Optimization strategies

## Document Conventions

### Version Numbers
- **Major.Minor.Patch** format
- Revision history at document top
- Future features marked "TBD" or "Planned"

### Code Examples
```asm
# Assembly examples use SLOW-32 syntax
        li r3, 42
        halt
```

```c
// C examples assume --target slow32-unknown-none
int main() { return 42; }
```

### Status Indicators
- ✅ Fully implemented
- ⚠️ Partially implemented  
- ❌ Not implemented
- 🚧 Under development

## Getting Help

### Common Issues
- See [Known Limitations](91-known-limitations.md)
- Check example [Makefile](../examples/Makefile)
- Review [Installation](02-installation.md) troubleshooting

### Reporting Problems
- GitHub Issues (when repository is public)
- Include: tool version, OS, error messages
- Minimal reproduction case preferred

## Contributing

### Documentation Improvements
- Fix typos and clarity issues
- Add examples and use cases
- Update revision history tables
- Maintain formal tone with revision tracking

### Technical Contributions
- Test on different platforms
- Report bugs with reproductions
- Suggest optimizations
- Implement planned features

## Philosophy Reminder

> "If it needs more than a sentence to explain, it's too complex"

SLOW-32 prioritizes:
1. **Clarity** over performance
2. **Simplicity** over features  
3. **Education** over production
4. **Understanding** over optimization

Every document, every tool, every instruction should embody these principles.

---

*Welcome to SLOW-32. Let's build something we can understand.*