# x86_64 assembly
.intel_syntax noprefix
.extern printf

.section .rodata
.Lstr0:
    .byte 37, 100, 32, 37, 100, 32, 37, 100, 10, 0

.text
.globl add
add:
    push rbp
    mov rbp, rsp
    sub rsp, 80
.Ladd_0:
    mov [rbp-32], rdi
    mov [rbp-64], rsi
    mov rax, [rbp-32]
    add rax, [rbp-64]
    mov [rbp-72], rax
    mov rax, [rbp-72]
    leave
    ret
    leave
    ret

.globl Point_offset
Point_offset:
    push rbp
    mov rbp, rsp
    sub rsp, 144
.LPoint_offset_0:
    mov [rbp-32], rdi
    mov [rbp-64], rsi
    mov [rbp-96], rdx
    mov [rbp-128], rcx
    mov rax, [rbp-32]
    mov rbx, rax
    add rbx, [rbp-96]
    mov rcx, [rbp-40]
    mov rdx, rcx
    add rdx, [rbp-128]
    mov rsi, rbx
    mov [rbp-136], rdx
    mov rdx, [rbp-136]
    mov [rbp-144], rax
    mov rax, rsi
    leave
    ret
    leave
    ret

.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 208
.Lmain_0:
    mov qword ptr [rbp-32], 1
    mov qword ptr [rbp-64], 2
    mov rdi, [rbp-32]
    mov rsi, [rbp-64]
    mov rdx, 3
    mov rcx, 4
    call Point_offset
    mov rax, rax
    mov [rbp-96], rax
    mov [rbp-128], rdx
    mov rbx, [rbp-96]
    mov rcx, [rbp-104]
    mov rdi, rbx
    mov rsi, rcx
    mov [rbp-168], rbx
    mov [rbp-176], rax
    mov [rbp-184], rcx
    call add
    mov rdx, rax
    mov [rbp-160], rdx
    mov rsi, [rbp-32]
    mov rdi, [rbp-104]
    mov [rbp-192], rdi
    lea rdi, [rip + .Lstr0]
    mov [rbp-200], rsi
    mov rsi, [rbp-200]
    mov [rbp-208], rdx
    mov rdx, [rbp-192]
    mov rcx, [rbp-160]
    mov rax, 0
    call printf
    mov rax, 0
    leave
    ret
    leave
    ret

.section .note.GNU-stack,"",@progbits
