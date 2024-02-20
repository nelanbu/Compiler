;yagmur eren 
                global  factorial_message
                extern  printf
                extern  atoi
                extern  puts

format:         db  "My message is '%s'.", 10, "%d! = ", 0

factorial_message:
                mov     rdi, [rbx + 16] ;argv[2]
                call    atoi    ;int is in rax
                mov     r15, rax
                
                mov     rdi, format
                mov     rsi, [rbx + 24] ;argv[3]
                mov     rdx, r15
                xor     rax, rax
                call    printf
                mov     rax, 1

factorial:      cmp     r15, 1          
                jle     small      
                mul     r15
                dec     r15
                jmp     factorial

small:          mov     rdi, rax    ;answer is in rdi
                mov     rax, 0
                ret