;yagmur eren
%define new_line 10

        global  main
        extern  puts
        extern  fflush
        extern  putchar
        extern  atoi        
        extern  factorial_message
        extern  printf

        section .text

menu_str       db      "usage: <command> <arg1> <arg2>", new_line, new_line, \
                        "commands:", new_line, \
                        "  e   Echo arguments arg1 and arg2", new_line, \
                        "  m   Prints out a magic number", new_line, \
                        "  +   Adds together arg1 and arg2", new_line, \
                        "  !   Prints out the factorial value of", new_line, \
                        "      arg1, as well as the message in arg2", new_line, 0
        

main:           mov     rbp, rdi        ; argc in rdi. Save in rbp, caller-saved
                mov     rbx, rsi        ; argv in rsi. Save in rbx, caller-saved
                
                cmp     rbp, 4
                jl      menu
                
                mov     rdi, [rbx+8] ; command - argv[1] 
                mov     dil, byte [rdi] ;--since argv is pointer to a pointer/ and read one byte since each char is one byte
                cmp     dil, 'e'
                je      echo
                cmp     dil, 'm'
                je      magic
                cmp     dil, '+'
                je      add_
                cmp     dil, '!'
                je      factorial


        ;mov     rax, 0
        ;ret


menu:           mov     rdi,  menu_str
                call    puts
                mov     rax, 1
                ret

echo:           mov     rdi, [rbx + 16] ;argv[2]
                call    puts
                mov     rdi, [rbx + 24] ;argv[3]
                call    puts
                mov     rax, 0
                ret

print_int:      mov  r15, 0 ;counter
                mov  r14, 10 ;divider
                mov  rax, rdi
                cmp  rax, 0
                jge  loop1
                mov  rdi, '-'
                push rax
                call putchar       
                pop rax 
                neg rax

loop1:          mov  rdx, 0   
                div  r14      ; rax has the result, rdx is remainder
                inc  r15
                push rdx
                cmp  rax, 0
                jne  loop1
           
loop2:          pop rdi     
                add rdi, '0'
                call putchar
                dec r15
                cmp r15, 0
                jne loop2

                mov rdi, new_line
                call putchar
                ret


magic:          mov     rdi, -126
                call    print_int
                mov     rax, 0
                ret


add_:           mov     r15, 0
                mov     rdi, [rbx + 16] ;argv[2]
                call    atoi
                add     r15, rax
                mov     rdi, [rbx + 24] ;argv[2]
                call    atoi
                add     r15, rax
                mov     rdi, r15
                call    print_int
                mov     rax, 0
                ret


factorial:      call    factorial_message
                call    print_int               ;takes arg from rdi
                mov     rax, 0
                ret

