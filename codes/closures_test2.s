# x86_64 assembly
.intel_syntax noprefix
.extern printf

.section .rodata
.Lstr0:
    .byte 37, 100, 32, 37, 100, 10, 0

.text
.globl operate
operate:
    push rbp
    mov rbp, rsp
    sub rsp, 32
.Loperate_0:
    mov [rbp-8], rdi
    mov [rbp-16], rsi
    mov [rbp-24], rdx
    mov rdi, [rbp-16]
    mov rsi, [rbp-24]
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
    mov [rbp-16], rsi
    mov rax, [rbp-8]
    add rax, [rbp-16]
    mov rax, rax
    leave
    ret

.globl lambda$2
lambda$2:
    push rbp
    mov rbp, rsp
    sub rsp, 48
.Llambda$2_0:
    mov [rbp-32], rdi
    mov [rbp-40], rsi
    mov rax, [rbp-32]
    imul rax, [rbp-40]
    mov rax, rax
    leave
    ret

.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 64
.Lmain_0:
    mov qword ptr [rbp-24], OFFSET FLAT:lambda$1
    mov qword ptr [rbp-48], OFFSET FLAT:lambda$2
    mov rdi, [rbp-24]
    mov rsi, 2
    mov rdx, 4
    call operate
    mov rax, rax
    mov rdi, [rbp-48]
    mov rsi, 3
    mov rdx, 5
    mov [rbp-56], rax
    call operate
    mov rbx, rax
    lea rdi, [rip + .Lstr0]
    mov rsi, [rbp-56]
    mov rdx, rbx
    mov rax, 0
    mov [rbp-64], rbx
    call printf
    mov rax, 0
    leave
    ret

.section .note.GNU-stack,"",@progbits
