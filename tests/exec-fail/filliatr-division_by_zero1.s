	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	pushq $1
	popq %rbx
	cqto
	idivq %rbx
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
