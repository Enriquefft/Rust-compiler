# x86_64 assembly
.intel_syntax noprefix
.extern printf

.section .rodata
.Lstr0:
    .byte 37, 100, 32, 37, 100, 10, 0

.text
.globl apply
apply:
    push rbp
    mov rbp, rsp
    sub rsp, 16
.Lapply_0:
    mov [rbp-8], rdi
    mov [rbp-16], rsi
    mov rdi, [rbp-16]
    call [rbp-8]
    mov rax, rax
    mov rax, rax
    leave
    ret

.globl lambda$1
lambda$1:
    push rbp
    mov rbp, rsp
    sub rsp, 16
.Llambda$1_0:
    mov [rbp-8], rdi
    mov rax, [rbp-8]
    add rax, 1
    mov rax, rax
    leave
    ret

.globl lambda$2
lambda$2:
    push rbp
    mov rbp, rsp
    sub rsp, 32
.Llambda$2_0:
    mov [rbp-24], rdi
    mov rax, [rbp-24]
    imul rax, 2
    mov [rbp-32], rax
    mov rbx, [rbp-32]
    add rbx, 3
    mov rax, rbx
    leave
    ret

.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 64
.Lmain_0:
    mov qword ptr [rbp-16], OFFSET FLAT:lambda$1
    mov qword ptr [rbp-40], OFFSET FLAT:lambda$2
    mov rdi, [rbp-16]
    mov rsi, 5
    call apply
    mov rax, rax
    mov rdi, 4
    mov [rbp-48], rax
    call [rbp-40]
    mov rbx, rax
    lea rdi, [rip + .Lstr0]
    mov rsi, [rbp-48]
    mov rdx, rbx
    mov rax, 0
    mov [rbp-56], rbx
    call printf
    mov rax, 0
    leave
    ret

.section .note.GNU-stack,"",@progbits
