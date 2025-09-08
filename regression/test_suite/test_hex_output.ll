; ModuleID = 'test_hex_output.c'
source_filename = "test_hex_output.c"
target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i32:32:32-i64:32:32-f32:32:32-f64:32:32-n8:16:32-S32"
target triple = "slow32-unknown-none"

@__const.main.test_string = private unnamed_addr constant [15 x i8] c"Hello, SLOW32!\00", align 1

; Function Attrs: noinline nounwind optnone
define dso_local i32 @calculate_checksum(ptr noundef %data, i32 noundef %len) #0 {
entry:
  %data.addr = alloca ptr, align 4
  %len.addr = alloca i32, align 4
  %sum = alloca i32, align 4
  %i = alloca i32, align 4
  store ptr %data, ptr %data.addr, align 4
  store i32 %len, ptr %len.addr, align 4
  store i32 305419896, ptr %sum, align 4
  store i32 0, ptr %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32, ptr %i, align 4
  %1 = load i32, ptr %len.addr, align 4
  %cmp = icmp slt i32 %0, %1
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %2 = load i32, ptr %sum, align 4
  %3 = load ptr, ptr %data.addr, align 4
  %4 = load i32, ptr %i, align 4
  %arrayidx = getelementptr inbounds i8, ptr %3, i32 %4
  %5 = load i8, ptr %arrayidx, align 1
  %conv = sext i8 %5 to i32
  %6 = load i32, ptr %i, align 4
  %and = and i32 %6, 3
  %mul = mul nsw i32 %and, 8
  %shl = shl i32 %conv, %mul
  %xor = xor i32 %2, %shl
  store i32 %xor, ptr %sum, align 4
  %7 = load i32, ptr %sum, align 4
  %shl1 = shl i32 %7, 1
  %8 = load i32, ptr %sum, align 4
  %shr = lshr i32 %8, 31
  %or = or i32 %shl1, %shr
  store i32 %or, ptr %sum, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %9 = load i32, ptr %i, align 4
  %inc = add nsw i32 %9, 1
  store i32 %inc, ptr %i, align 4
  br label %for.cond, !llvm.loop !3

for.end:                                          ; preds = %for.cond
  %10 = load i32, ptr %sum, align 4
  ret i32 %10
}

; Function Attrs: noinline nounwind optnone
define dso_local signext i8 @nibble_to_hex(i32 noundef %n) #0 {
entry:
  %retval = alloca i8, align 1
  %n.addr = alloca i32, align 4
  store i32 %n, ptr %n.addr, align 4
  %0 = load i32, ptr %n.addr, align 4
  %and = and i32 %0, 15
  store i32 %and, ptr %n.addr, align 4
  %1 = load i32, ptr %n.addr, align 4
  %cmp = icmp slt i32 %1, 10
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %2 = load i32, ptr %n.addr, align 4
  %add = add nsw i32 48, %2
  %conv = trunc i32 %add to i8
  store i8 %conv, ptr %retval, align 1
  br label %return

if.else:                                          ; preds = %entry
  %3 = load i32, ptr %n.addr, align 4
  %sub = sub nsw i32 %3, 10
  %add1 = add nsw i32 65, %sub
  %conv2 = trunc i32 %add1 to i8
  store i8 %conv2, ptr %retval, align 1
  br label %return

return:                                           ; preds = %if.else, %if.then
  %4 = load i8, ptr %retval, align 1
  ret i8 %4
}

; Function Attrs: noinline nounwind optnone
define dso_local void @print_hex(i32 noundef %value) #0 {
entry:
  %value.addr = alloca i32, align 4
  %i = alloca i32, align 4
  %nibble = alloca i32, align 4
  store i32 %value, ptr %value.addr, align 4
  call void @debug_char(i8 noundef signext 48)
  call void @debug_char(i8 noundef signext 120)
  store i32 7, ptr %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32, ptr %i, align 4
  %cmp = icmp sge i32 %0, 0
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %1 = load i32, ptr %value.addr, align 4
  %2 = load i32, ptr %i, align 4
  %mul = mul nsw i32 %2, 4
  %shr = lshr i32 %1, %mul
  %and = and i32 %shr, 15
  store i32 %and, ptr %nibble, align 4
  %3 = load i32, ptr %nibble, align 4
  %call = call signext i8 @nibble_to_hex(i32 noundef %3)
  call void @debug_char(i8 noundef signext %call)
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %4 = load i32, ptr %i, align 4
  %dec = add nsw i32 %4, -1
  store i32 %dec, ptr %i, align 4
  br label %for.cond, !llvm.loop !5

for.end:                                          ; preds = %for.cond
  call void @debug_char(i8 noundef signext 10)
  ret void
}

declare dso_local void @debug_char(i8 noundef signext) #1

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %test_string = alloca [15 x i8], align 1
  %checksum = alloca i32, align 4
  %i = alloca i32, align 4
  store i32 0, ptr %retval, align 4
  call void @llvm.memcpy.p0.p0.i32(ptr align 1 %test_string, ptr align 1 @__const.main.test_string, i32 15, i1 false)
  %arraydecay = getelementptr inbounds [15 x i8], ptr %test_string, i32 0, i32 0
  %call = call i32 @calculate_checksum(ptr noundef %arraydecay, i32 noundef 14)
  store i32 %call, ptr %checksum, align 4
  %0 = load i32, ptr %checksum, align 4
  call void @print_hex(i32 noundef %0)
  call void @print_hex(i32 noundef -559038737)
  store i32 0, ptr %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %1 = load i32, ptr %i, align 4
  %cmp = icmp slt i32 %1, 16
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %2 = load i32, ptr %i, align 4
  %call1 = call signext i8 @nibble_to_hex(i32 noundef %2)
  call void @debug_char(i8 noundef signext %call1)
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %3 = load i32, ptr %i, align 4
  %inc = add nsw i32 %3, 1
  store i32 %inc, ptr %i, align 4
  br label %for.cond, !llvm.loop !6

for.end:                                          ; preds = %for.cond
  call void @debug_char(i8 noundef signext 10)
  ret i32 0
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i32(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i32, i1 immarg) #2

attributes #0 = { noinline nounwind optnone "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"frame-pointer", i32 2}
!2 = !{!"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 2a3044900e5648d483258e4403a2a2f47e27dcfe)"}
!3 = distinct !{!3, !4}
!4 = !{!"llvm.loop.mustprogress"}
!5 = distinct !{!5, !4}
!6 = distinct !{!6, !4}
