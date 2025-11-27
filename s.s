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
    sub rsp, 32
.Ladd_0:
    mov [rbp-8], rdi
    mov [rbp-16], rsi
    mov rax, [rbp-8]
    add rax, [rbp-16]
    mov [rbp-24], rax
    mov rax, [rbp-24]
    leave
    ret
    leave
    ret

.globl offset
offset:
    push rbp
    mov rbp, rsp
    sub rsp, 32
.Loffset_0:
    mov [rbp-8], rdi
    mov [rbp-16], rsi
    mov [rbp-24], rdx
    mov rax, [rbp-8]
    mov rbx, rax
    add rbx, [rbp-16]
    mov rcx, [rbp+0]
    mov rdx, rcx
    add rdx, [rbp-24]
    mov rsi, rbx
    mov [rbp-32], rax
    mov rax, rsi
    leave
    ret
    leave
    ret

.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 80
.Lmain_0:
    mov rax, 1
    mov [rbp-8], rax
    mov rdi, [rbp-8]
    mov rsi, 3
    mov rdx, 4
    mov [rbp-32], rax
    call offset
    mov rbx, rax
    mov [rbp-16], rbx
    mov rcx, [rbp-16]
    mov rdx, [rbp-8]
    mov rdi, rcx
    mov rsi, rdx
    mov [rbp-40], rbx
    mov [rbp-48], rcx
    mov [rbp-56], rdx
    call main
    mov rsi, rax
    mov [rbp-24], rsi
    mov rdi, [rbp-8]
    mov r8, [rbp-8]
    mov [rbp-64], rdi
    lea rdi, [rip + .Lstr0]
    mov [rbp-72], rsi
    mov rsi, [rbp-64]
    mov rdx, r8
    mov rcx, [rbp-24]
    mov rax, 0
    mov [rbp-80], r8
    call printf
    mov rax, 0
    leave
    ret
    leave
    ret

.section .note.GNU-stack,"",@progbits
