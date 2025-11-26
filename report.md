# Codes compilation report

Generated: 2025-11-26T17:29:58-05:00

## closures_test1.rs

- Compile: success
#### Compile output


Function call detected with callee type .{ .params = { 1 }, .ret = 2 }
Function call detected with callee type .{ .params = { 0, 3 }, .ret = 4 }
Function call detected with callee type .{ .params = { 14 }, .ret = 14 }


- Run: success
#### Run output


6 11


## closures_test2.rs

- Compile: success
#### Compile output


Function call detected with callee type .{ .params = { 1, 2 }, .ret = 3 }
Function call detected with callee type .{ .params = { 0, 4, 5 }, .ret = 6 }
Function call detected with callee type .{ .params = { 0, 4, 5 }, .ret = 6 }


- Run: success
#### Run output


6 15


## conditionals_and_ranges_test1.rs

- Compile: success
#### Compile output





- Run: failed
#### Run output


/tmp/tmp.zAGhZzhAGo/conditionals_and_ranges_test1.s: Assembler messages:
/tmp/tmp.zAGhZzhAGo/conditionals_and_ranges_test1.s:22: Error: operand type mismatch for `idiv'
/tmp/tmp.zAGhZzhAGo/conditionals_and_ranges_test1.s:50: Error: operand type mismatch for `idiv'
/tmp/tmp.zAGhZzhAGo/conditionals_and_ranges_test1.s:82: Error: ambiguous operand size for `add'


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


Function call detected with callee type .{ .params = { 0 }, .ret = 1 }


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


Function call detected with callee type .{ .params = { 0 }, .ret = 1 }
Function call detected with callee type .{ .params = { 0 }, .ret = 1 }


- Run: success
#### Run output


8 14


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


Function call detected with callee type .{ .params = { 0, 1 }, .ret = 2 }
Function call detected with callee type .{ .params = { 7, 9 }, .ret = 11 }
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


Function call detected with callee type .{ .params = { 0, 1 }, .ret = 2 }


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


Function call detected with callee type .{ .params = { 0, 1 }, .ret = 2 }


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

