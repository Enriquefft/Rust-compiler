# x86_64 assembly
.intel_syntax noprefix
.extern printf

.section .rodata
.Lstr0:
    .byte 34, 101, 118, 101, 110, 34, 0
.Lstr1:
    .byte 34, 111, 100, 100, 34, 0
.Lstr2:
    .byte 37, 115, 32, 37, 108, 100, 10, 0

.text
.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 96
.Lmain_0:
    mov qword ptr [rbp-8], 6
    mov rax, [rbp-8]
    cqo
    mov r11, 2
    idiv r11
    mov rax, rdx
    cmp rax, 0
    sete bl
    and rbx, 1
    test rbx, rbx
    jne .Lmain_1
    jmp .Lmain_2
.Lmain_1:
    lea rcx, [rip + .Lstr0]
    jmp .Lmain_3
.Lmain_2:
    lea rcx, [rip + .Lstr1]
    jmp .Lmain_3
.Lmain_3:
    mov [rbp-16], rcx
    mov qword ptr [rbp-24], 0
    mov qword ptr [rbp-32], 1
    jmp .Lmain_4
.Lmain_4:
    mov rdx, [rbp-32]
    cmp rdx, [rbp-8]
    setle sil
    and rsi, 1
    test rsi, rsi
    jne .Lmain_5
    jmp .Lmain_6
.Lmain_5:
    mov rdi, [rbp-32]
    mov rax, rdi
    cqo
    mov r11, 2
    idiv r11
    mov rdi, rdx
    cmp rdi, 0
    sete r8b
    and r8, 1
    test r8, r8
    jne .Lmain_7
    jmp .Lmain_8
.Lmain_6:
    lea rdi, [rip + .Lstr2]
    mov rsi, [rbp-16]
    mov rdx, [rbp-24]
    mov rax, 0
    mov [rbp-40], rdi
    mov [rbp-48], rbx
    mov [rbp-56], rax
    mov [rbp-64], rcx
    mov [rbp-72], rdx
    mov [rbp-80], rsi
    mov [rbp-88], r8
    call printf
    mov rax, 0
.Lmain_7:
    mov r9, [rbp-24]
    mov r10, r9
    add r10, [rbp-32]
    mov [rbp-24], r10
    jmp .Lmain_9
.Lmain_8:
    jmp .Lmain_9
.Lmain_9:
    mov r11, [rbp-32]
    mov [rbp-96], r11
    add qword ptr [rbp-96], 1
    mov r11, [rbp-96]
    mov [rbp-32], r11
    jmp .Lmain_4
    leave
    ret

.section .note.GNU-stack,"",@progbits
