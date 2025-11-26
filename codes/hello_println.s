# x86_64 assembly
.intel_syntax noprefix
.extern printf

.section .rodata
.Lstr0:
    .byte 104, 101, 108, 108, 111, 10, 0

.text
.globl main
main:
    push rbp
    mov rbp, rsp
.Lmain_0:
    lea rdi, [rip + .Lstr0]
    mov rax, 0
    call printf
    mov rax, 0
    leave
    ret

.section .note.GNU-stack,"",@progbits
