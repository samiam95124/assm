	nop
	movb	[mydat],0

	movd	ebx,0		! first argument: exit code
	movd	eax,1		! system call number (sys_exit)
	int	$80		! call kernel

mydat:  defvs   4
