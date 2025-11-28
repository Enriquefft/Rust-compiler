# x86_64 assembly
.intel_syntax noprefix
.extern printf

.section .rodata
.Lstr0:
    .byte 115, 117, 109, 32, 61, 32, 37, 108, 100, 10, 0

.text
.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 208
.Lmain_0:
    mov rax, 3
    mov rbx, 6
    mov rcx, 1
    mov rdx, 8
    mov [rbp-136], rdx
    mov rdx, rbx
    mov [rbp-144], rcx
    mov rcx, [rbp-144]
    mov r8, [rbp-136]
    mov [rbp-32], rax
    mov [rbp-40], rdx
    mov [rbp-48], rcx
    mov [rbp-56], r8
    mov qword ptr [rbp-64], 0
    mov qword ptr [rbp-128], 0
    jmp .Lmain_1
.Lmain_1:
    mov rbx, [rbp-128]
    mov qword ptr [rbp-144], 4
    cmp rbx, [rbp-144]
    setl byte ptr [rbp-136]
    and qword ptr [rbp-136], 1
    mov r11, [rbp-136]
    test r11, r11
    jne .Lmain_2
    jmp .Lmain_3
.Lmain_2:
    mov rsi, [rbp-128]
    mov rdi, rsi
    imul rdi, 8
    lea r8, [rbp-32]
    sub r8, rdi
    mov r9, [r8]
    mov [rbp-96], r9
    mov r10, [rbp-64]
    mov rdi, r10
    add rdi, [rbp-96]
    mov [rbp-64], rdi
    mov r8, [rbp-128]
    mov [rbp-152], r8
    add qword ptr [rbp-152], 1
    mov r11, [rbp-152]
    mov [rbp-128], r11
    jmp .Lmain_1
.Lmain_3:
    mov [rbp-160], rdi
    lea rdi, [rip + .Lstr0]
    mov [rbp-168], rsi
    mov rsi, [rbp-64]
    mov [rbp-176], rax
    mov rax, 0
    mov [rbp-184], r9
    mov [rbp-192], rbx
    mov [rbp-200], r8
    mov [rbp-208], r10
    call printf
    mov rax, 0
    leave
    ret
    leave
    ret

.section .note.GNU-stack,"",@progbits
