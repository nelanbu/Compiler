	global	main
	section  .text
main:
	Sub		rsp, 144
	Mov		qword [rsp + 8], 5
	Mov		qword [rsp + 16], 4
	Mov		r10, qword [rsp + 16]
	Cmp		qword [rsp + 8], r10
	Setg		byte [rsp]
	Mov		qword [rsp + 32], 5
	Mov		qword [rsp + 40], 5
	Mov		r10, qword [rsp + 40]
	Cmp		qword [rsp + 32], r10
	Setg		byte [rsp + 24]
	Mov		qword [rsp + 56], 5
	Mov		qword [rsp + 64], 6
	Mov		r10, qword [rsp + 64]
	Cmp		qword [rsp + 56], r10
	Setg		byte [rsp + 48]
	Mov		r10, qword [rsp]
	Mov		qword [rsp + 128], r10
	Mov		r10, qword [rsp]
	Mov		qword [rsp + 136], r10
	Mov		r10, qword [rsp + 128]
	Mov		qword [rsp + 112], r10
	Mov		r10, qword [rsp + 136]
	Add		qword [rsp + 112], r10
	Mov		r10, qword [rsp]
	Mov		qword [rsp + 120], r10
	Mov		r10, qword [rsp + 112]
	Mov		qword [rsp + 96], r10
	Mov		r10, qword [rsp + 120]
	Add		qword [rsp + 96], r10
	Mov		r10, qword [rsp + 24]
	Mov		qword [rsp + 104], r10
	Mov		r10, qword [rsp + 96]
	Mov		qword [rsp + 80], r10
	Mov		r10, qword [rsp + 104]
	Add		qword [rsp + 80], r10
	Mov		r10, qword [rsp + 48]
	Mov		qword [rsp + 88], r10
	Mov		r10, qword [rsp + 80]
	Mov		qword [rsp + 72], r10
	Mov		r10, qword [rsp + 88]
	Add		qword [rsp + 72], r10
	Mov		rax, qword [rsp + 72]
	Add		rsp, 144
	ret