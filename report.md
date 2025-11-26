# Codes compilation report

Generated: 2025-11-26T12:06:57-05:00

## closures_test1.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/closures_test1.rs:5:19-5:33: closures are not supported in HIR lowering
    let add_one = |n: i32| n + 1;
                  ^^^^^^^^^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/closures_test1.rs:6:27-6:72: closures are not supported in HIR lowering
    let double_then_add = |n: i32| { let doubled = n * 2; doubled + 3 };
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


#### Run output

Skipped because compilation failed.

## closures_test2.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/closures_test2.rs:7:15-7:37: closures are not supported in HIR lowering
    let sum = |a: i32, b: i32| a + b;
              ^^^^^^^^^^^^^^^^^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/closures_test2.rs:8:19-8:45: closures are not supported in HIR lowering
    let product = |a: i32, b: i32| { a * b };
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^


#### Run output

Skipped because compilation failed.

## conditionals_and_ranges_test1.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/conditionals_and_ranges_test1.rs:6:5-8:6: for loops are not supported in HIR lowering yet
    for i in 1..=n {
    ^^^^^^^^^^^^^^^^
        if i % 2 == 0 { total += i; }
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    }
^^^^^


#### Run output

Skipped because compilation failed.

## conditionals_and_ranges_test2.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/conditionals_and_ranges_test2.rs:3:24-3:27: expected ';' after let statement
    let mut product = 1i32;
                       ^^^
error: /home/hybridz/Projects/Rust-compiler/codes/conditionals_and_ranges_test2.rs:4:5-7:6: for loops are not supported in HIR lowering yet
    for i in 2..5 { // 2,3,4
    ^^^^^^^^^^^^^^^^^^^^^^^^
        let factor = if i > 3 { i - 1 } else { i + 1 };
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        product *= factor;
^^^^^^^^^^^^^^^^^^^^^^^^^^
    }
^^^^^


#### Run output

Skipped because compilation failed.

## control_flow_test1.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## control_flow_test2.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/control_flow_test2.rs:4:5-6:6: for loops are not supported in HIR lowering yet
    for v in 0..limit {
    ^^^^^^^^^^^^^^^^^^^
        acc += v;
^^^^^^^^^^^^^^^^^
    }
^^^^^


#### Run output

Skipped because compilation failed.

## conversions_test1.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/conversions_test1.rs:7:21-7:24: expected ';' after let statement
    let ptr: *const i32 = &small as *const i32;
                    ^^^


#### Run output

Skipped because compilation failed.

## conversions_test2.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## core_language_test1.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## core_language_test2.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/core_language_test2.rs:3:34-3:35: unresolved identifier `n`
    let result = { let interim = n + 1; interim * 2 };
                                 ^
error: /home/hybridz/Projects/Rust-compiler/codes/core_language_test2.rs:4:5-4:11: unresolved identifier `result`
    result
    ^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/core_language_test2.rs:10:38-10:47: unresolved identifier `inner_sum`
    println!("{} {}", double(outer), inner_sum);
                                     ^^^^^^^^^


#### Run output

Skipped because compilation failed.

## dynamic_data_test1.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## dynamic_data_test2.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## expressions_test1.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/expressions_test1.rs:3:14-3:17: expected ';' after let statement
    let a = 8i32;
             ^^^
error: /home/hybridz/Projects/Rust-compiler/codes/expressions_test1.rs:4:14-4:17: expected ';' after let statement
    let b = 3i32;
             ^^^


#### Run output

Skipped because compilation failed.

## expressions_test2.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## functions_and_methods_test1.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:3:46-3:52: addressing operators not supported in HIR lowering yet
fn scale(value: &i32, factor: &i32) -> i32 { *value * *factor }
                                             ^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:3:55-3:62: addressing operators not supported in HIR lowering yet
fn scale(value: &i32, factor: &i32) -> i32 { *value * *factor }
                                                      ^^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:8:33-8:38: addressing operators not supported in HIR lowering yet
    let total = add(base, scale(&base, &factor));
                                ^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:8:40-8:47: addressing operators not supported in HIR lowering yet
    let total = add(base, scale(&base, &factor));
                                       ^^^^^^^


#### Run output

Skipped because compilation failed.

## functions_and_methods_test2.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## generics_test1.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## generics_test2.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## hello_println.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


clang: warning: argument unused during compilation: '-fstack-clash-protection' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fzero-call-used-regs=used-gpr' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _LIBCPP_HARDENING_MODE=_LIBCPP_HARDENING_MODE_EXTENSIVE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fstack-protector-strong' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '--param ssp-buffer-size=4' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fwrapv' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fno-omit-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-mno-omit-leaf-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _FORTIFY_SOURCE=2' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-idirafter /nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-nostdlibinc' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-frandom-seed=a8h0xwdrdh' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
hello


## input1.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


clang: warning: argument unused during compilation: '-fstack-clash-protection' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fzero-call-used-regs=used-gpr' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _LIBCPP_HARDENING_MODE=_LIBCPP_HARDENING_MODE_EXTENSIVE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fstack-protector-strong' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '--param ssp-buffer-size=4' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fwrapv' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fno-omit-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-mno-omit-leaf-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _FORTIFY_SOURCE=2' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-idirafter /nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-nostdlibinc' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-frandom-seed=a8h0xwdrdh' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
20
10
1000000


## input2.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


clang: warning: argument unused during compilation: '-fstack-clash-protection' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fzero-call-used-regs=used-gpr' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _LIBCPP_HARDENING_MODE=_LIBCPP_HARDENING_MODE_EXTENSIVE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fstack-protector-strong' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '--param ssp-buffer-size=4' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fwrapv' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fno-omit-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-mno-omit-leaf-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _FORTIFY_SOURCE=2' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-idirafter /nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-nostdlibinc' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-frandom-seed=a8h0xwdrdh' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
5


## input3.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/input3.rs:6:5-8:6: for loops are not supported in HIR lowering yet
    for i in 0..10 { // i is a i32
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        x += i;
^^^^^^^^^^^^^^^
    }
^^^^^


#### Run output

Skipped because compilation failed.

## input4.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/input4.rs:2:5-2:6: unresolved identifier `a`
    a + b
    ^
error: /home/hybridz/Projects/Rust-compiler/codes/input4.rs:2:9-2:10: unresolved identifier `b`
    a + b
        ^


#### Run output

Skipped because compilation failed.

## macros_test1.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


clang: warning: argument unused during compilation: '-fstack-clash-protection' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fzero-call-used-regs=used-gpr' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _LIBCPP_HARDENING_MODE=_LIBCPP_HARDENING_MODE_EXTENSIVE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fstack-protector-strong' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '--param ssp-buffer-size=4' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fwrapv' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fno-omit-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-mno-omit-leaf-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _FORTIFY_SOURCE=2' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-idirafter /nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-nostdlibinc' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-frandom-seed=a8h0xwdrdh' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
hello from println!


## macros_test2.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/macros_test2.rs:3:32-3:33: unresolved identifier `a`
    println!("a = {}, b = {}", a, b);
                               ^
error: /home/hybridz/Projects/Rust-compiler/codes/macros_test2.rs:3:35-3:36: unresolved identifier `b`
    println!("a = {}, b = {}", a, b);
                                  ^


#### Run output

Skipped because compilation failed.

## statements_and_patterns_test1.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


clang: warning: argument unused during compilation: '-fstack-clash-protection' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fzero-call-used-regs=used-gpr' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _LIBCPP_HARDENING_MODE=_LIBCPP_HARDENING_MODE_EXTENSIVE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fstack-protector-strong' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '--param ssp-buffer-size=4' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fwrapv' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fno-omit-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-mno-omit-leaf-frame-pointer' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-U _FORTIFY_SOURCE' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-D _FORTIFY_SOURCE=2' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-idirafter /nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-glibc-2.40-66-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-nostdlibinc' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-frandom-seed=a8h0xwdrdh' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/ah42ik7ig7yk1xday8rvi66ggbwr2fk2-lldb-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-lldb-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-isystem /nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/gj06hl0vb46kf66zpsss4gk5wphfh5lk-compiler-rt-libc-21.1.2-dev=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-compiler-rt-libc-21.1.2-dev' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-cxx-isystem /nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-fmacro-prefix-map=/nix/store/kzq78n13l8w24jn8bx4djj79k5j717f1-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu=/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-gcc-14.3.0/include/c++/14.3.0/x86_64-unknown-linux-gnu' [-Wunused-command-line-argument]
2 5


## statements_and_patterns_test2.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## types_test1.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/types_test1.rs:6:33-6:34: expected type
    let truth: bool = int_val < 0;
                                ^
error: /home/hybridz/Projects/Rust-compiler/codes/types_test1.rs:6:33-6:34: expected '>' after generic arguments
    let truth: bool = int_val < 0;
                                ^
error: /home/hybridz/Projects/Rust-compiler/codes/types_test1.rs:6:33-6:34: expected ';' after let statement
    let truth: bool = int_val < 0;
                                ^
error: /home/hybridz/Projects/Rust-compiler/codes/types_test1.rs:9:25-9:37: complex paths not yet supported in HIR lowering
    let owned: String = String::from("world");
                        ^^^^^^^^^^^^


#### Run output

Skipped because compilation failed.

## types_test2.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

