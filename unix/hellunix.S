.text					# section declaration

			# we must export the entry point to the ELF linker or
    .global _start	# loader. They conventionally recognize _start as their
			# entry point. Use ld -e foo to override the default.

_start:

	nop
	movb	$0,mydat

	movl	$0,%ebx		# first argument: exit code
	movl	$1,%eax		# system call number (sys_exit)
	int	$0x80		# call kernel

	.lcomm	mydat,4
