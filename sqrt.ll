declare double @llvm.sqrt.f64(double %v);

define i32 @main(i32, i8**) {
    %k = call double @llvm.sqrt.f64(double 32.0);
    %rv = fptoui double %k to i32
    ret i32 %rv;
}
