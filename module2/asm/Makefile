

.PHONY: all clean

all:
	gcc factorial.c main-c.c -o main-c

	nasm -felf64 main-asm.asm -o main-asm.o
	nasm -felf64 factorial.asm -o factorial.o

	gcc -no-pie main-asm.o factorial.o -o main-asm

clean:
	rm -f *.o main-c main-asm *~
