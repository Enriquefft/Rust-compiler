# Codes compilation report

Generated: 2025-11-26T12:49:11-05:00

## closures_test1.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/closures_test1.rs:6:28-6:29: duplicate local binding `n` previously defined as local 0
    let double_then_add = |n: i32| { let doubled = n * 2; doubled + 3 };
                           ^


#### Run output

Skipped because compilation failed.

## closures_test2.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/closures_test2.rs:8:20-8:21: duplicate local binding `a` previously defined as local 0
    let product = |a: i32, b: i32| { a * b };
                   ^
error: /home/hybridz/Projects/Rust-compiler/codes/closures_test2.rs:8:28-8:29: duplicate local binding `b` previously defined as local 1
    let product = |a: i32, b: i32| { a * b };
                           ^


#### Run output

Skipped because compilation failed.

## conditionals_and_ranges_test1.rs

- Compile: success
#### Compile output





- Run: failed
#### Run output


/tmp/tmp.Iqz0UOmfkH/conditionals_and_ranges_test1.s:22:5: error: invalid operand for instruction
    idiv rax, 2
    ^
/tmp/tmp.Iqz0UOmfkH/conditionals_and_ranges_test1.s:50:5: error: invalid operand for instruction
    idiv rdi, 2
    ^
/tmp/tmp.Iqz0UOmfkH/conditionals_and_ranges_test1.s:75:9: error: ambiguous operand size for instruction 'add'
    add [rbp-40], 1
        ^~~~~~~


## conditionals_and_ranges_test2.rs

- Compile: failed
#### Compile output


error: /home/hybridz/Projects/Rust-compiler/codes/conditionals_and_ranges_test2.rs:3:24-3:27: expected ';' after let statement
    let mut product = 1i32;
                       ^^^


#### Run output

Skipped because compilation failed.

## control_flow_test1.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

## control_flow_test2.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


10


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

- Compile: success
#### Compile output





- Run: failed
#### Run output


/tmp/tmp.Iqz0UOmfkH/core_language_test2.s:38:16: error: Expected 'PTR' or 'ptr' token!
    call double
               ^
/tmp/tmp.Iqz0UOmfkH/core_language_test2.s:42:16: error: Expected 'PTR' or 'ptr' token!
    call double
               ^


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


error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:3:46-3:52: cannot dereference non-pointer type
fn scale(value: &i32, factor: &i32) -> i32 { *value * *factor }
                                             ^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:3:55-3:62: cannot dereference non-pointer type
fn scale(value: &i32, factor: &i32) -> i32 { *value * *factor }
                                                      ^^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:3:46-3:62: arithmetic operands must be numeric
fn scale(value: &i32, factor: &i32) -> i32 { *value * *factor }
                                             ^^^^^^^^^^^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:3:1-3:64: function return type does not match body
fn scale(value: &i32, factor: &i32) -> i32 { *value * *factor }
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:8:27-8:48: argument type does not match parameter
    let total = add(base, scale(&base, &factor));
                          ^^^^^^^^^^^^^^^^^^^^^
error: /home/hybridz/Projects/Rust-compiler/codes/functions_and_methods_test1.rs:8:27-8:48: argument type does not match parameter
    let total = add(base, scale(&base, &factor));
                          ^^^^^^^^^^^^^^^^^^^^^


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


hello


## input1.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


20
10
1000000


## input2.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


10


## input3.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


46


## input4.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


21


## macros_test1.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


hello from println!


## macros_test2.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


a = 3, b = 4


## statements_and_patterns_test1.rs

- Compile: success
#### Compile output





- Run: success
#### Run output


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


#### Run output

Skipped because compilation failed.

## types_test2.rs

- Compile: failed
Compilation terminated after timeout (30s).

#### Compile output





#### Run output

Skipped because compilation failed.

