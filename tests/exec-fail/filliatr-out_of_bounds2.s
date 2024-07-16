	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $8, %rax
	call malloc
	pushq $1
	pushq $42
	movq %rdi, %rcx
	addq $0, %rcx
	movq 0(%rsp), %r11
	movq %r11, 0(%rcx)
	addq $8, %rsp
	pushq %rdi
	movq $0, %rcx
	addq %rbp, %rcx
	movq 8(%rsp), %r11
	movq %r11, 0(%rcx)
	movq 0(%rsp), %r11
	movq %r11, 8(%rcx)
	addq $16, %rsp
	pushq 0(%rbp)
	popq %rcx
	popq %rbx
	pushq $1
	popq %rax
	addq %rax, %rcx
	movq 0(%rcx), %r11
	pushq %r11
	popq %rax
	pushq 0(%rbp)
	popq %rax
	pushq $1
	popq %rbx
	popq %rax
	imulq $1, %rbx
	addq %rbx, %rax
	pushq %rax
	popq %rcx
	pushq $0
	popq %rdx
	movq %rdx, 8(%rcx)
	movq %rsp, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
	xorq %rax, %rax
	ret
print_string:
	xorq %rax, %rax
	call printf
	ret
	.data
