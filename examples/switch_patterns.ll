; ModuleID = 'examples/switch_patterns.c'
source_filename = "examples/switch_patterns.c"
target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i32:32:32-i64:32:32-f32:32:32-f64:32:32-n8:16:32-S32"
target triple = "slow32-unknown-none"

@switch.table.gap_switch = private unnamed_addr constant [6 x i32] [i32 10, i32 20, i32 30, i32 -1, i32 50, i32 60], align 4

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local range(i32 -1, 106) i32 @dense_switch(i32 noundef %x) local_unnamed_addr #0 {
entry:
  %0 = icmp ult i32 %x, 6
  %switch.offset = add nsw i32 %x, 100
  %retval.0 = select i1 %0, i32 %switch.offset, i32 -1
  ret i32 %retval.0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local range(i32 -1, 41) i32 @sparse_switch(i32 noundef %x) local_unnamed_addr #0 {
entry:
  switch i32 %x, label %sw.default [
    i32 1, label %return
    i32 10, label %sw.bb1
    i32 100, label %sw.bb2
    i32 1000, label %sw.bb3
  ]

sw.bb1:                                           ; preds = %entry
  br label %return

sw.bb2:                                           ; preds = %entry
  br label %return

sw.bb3:                                           ; preds = %entry
  br label %return

sw.default:                                       ; preds = %entry
  br label %return

return:                                           ; preds = %entry, %sw.default, %sw.bb3, %sw.bb2, %sw.bb1
  %retval.0 = phi i32 [ -1, %sw.default ], [ 20, %sw.bb1 ], [ 30, %sw.bb2 ], [ 40, %sw.bb3 ], [ 10, %entry ]
  ret i32 %retval.0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local range(i32 -1, 61) i32 @gap_switch(i32 noundef %x) local_unnamed_addr #0 {
entry:
  %switch.tableidx = add i32 %x, -1
  %0 = icmp ult i32 %switch.tableidx, 6
  br i1 %0, label %switch.lookup, label %return

switch.lookup:                                    ; preds = %entry
  %switch.gep = getelementptr inbounds nuw i32, ptr @switch.table.gap_switch, i32 %switch.tableidx
  %switch.load = load i32, ptr %switch.gep, align 4
  br label %return

return:                                           ; preds = %entry, %switch.lookup
  %retval.0 = phi i32 [ %switch.load, %switch.lookup ], [ -1, %entry ]
  ret i32 %retval.0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local range(i32 0, 6) i32 @char_switch(i32 noundef %c) local_unnamed_addr #0 {
entry:
  %switch.tableidx = add i32 %c, -97
  %0 = icmp ult i32 %switch.tableidx, 5
  %switch.offset = add i32 %c, -96
  %retval.0 = select i1 %0, i32 %switch.offset, i32 0
  ret i32 %retval.0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local range(i32 -1, 1016) i32 @large_switch(i32 noundef %x) local_unnamed_addr #0 {
entry:
  %0 = icmp ult i32 %x, 16
  %switch.offset = add nsw i32 %x, 1000
  %retval.0 = select i1 %0, i32 %switch.offset, i32 -1
  ret i32 %retval.0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local range(i32 0, 1281) i32 @binary_switch(i32 noundef %x) local_unnamed_addr #0 {
entry:
  switch i32 %x, label %sw.default [
    i32 1, label %return
    i32 2, label %sw.bb1
    i32 4, label %sw.bb2
    i32 8, label %sw.bb3
    i32 16, label %sw.bb4
    i32 32, label %sw.bb5
    i32 64, label %sw.bb6
    i32 128, label %sw.bb7
  ]

sw.bb1:                                           ; preds = %entry
  br label %return

sw.bb2:                                           ; preds = %entry
  br label %return

sw.bb3:                                           ; preds = %entry
  br label %return

sw.bb4:                                           ; preds = %entry
  br label %return

sw.bb5:                                           ; preds = %entry
  br label %return

sw.bb6:                                           ; preds = %entry
  br label %return

sw.bb7:                                           ; preds = %entry
  br label %return

sw.default:                                       ; preds = %entry
  br label %return

return:                                           ; preds = %entry, %sw.default, %sw.bb7, %sw.bb6, %sw.bb5, %sw.bb4, %sw.bb3, %sw.bb2, %sw.bb1
  %retval.0 = phi i32 [ 0, %sw.default ], [ 20, %sw.bb1 ], [ 40, %sw.bb2 ], [ 80, %sw.bb3 ], [ 160, %sw.bb4 ], [ 320, %sw.bb5 ], [ 640, %sw.bb6 ], [ 1280, %sw.bb7 ], [ 10, %entry ]
  ret i32 %retval.0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local range(i32 0, 4) i32 @state_machine(i32 noundef %state, i32 noundef %input) local_unnamed_addr #0 {
entry:
  switch i32 %state, label %sw.default [
    i32 0, label %sw.bb
    i32 1, label %return
    i32 2, label %sw.bb2
  ]

sw.bb:                                            ; preds = %entry
  %cmp = icmp sgt i32 %input, 0
  %cond = zext i1 %cmp to i32
  br label %return

sw.bb2:                                           ; preds = %entry
  %cmp3 = icmp eq i32 %input, 0
  %cond4 = select i1 %cmp3, i32 3, i32 2
  br label %return

sw.default:                                       ; preds = %entry
  br label %return

return:                                           ; preds = %entry, %sw.default, %sw.bb2, %sw.bb
  %retval.0 = phi i32 [ 0, %sw.default ], [ %cond, %sw.bb ], [ %cond4, %sw.bb2 ], [ 2, %entry ]
  ret i32 %retval.0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local noundef range(i32 -4, 2509) i32 @main() local_unnamed_addr #0 {
entry:
  ret i32 1359
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"frame-pointer", i32 2}
!2 = !{!"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"}
