	global	main
	section  .text
main:
	Sub		rsp, 24
	Mov		qword [rsp + 8], 20
	Mov		qword [rsp + 16], 20
	Mov		r10, qword [rsp + 16]
	Cmp		qword [rsp + 8], r10
	Sete		byte [rsp]
	Mov		rax, qword [rsp]
	Add		rsp, 24
	ret