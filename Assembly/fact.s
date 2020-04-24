global start 

section .text

start: 
  mov r8, 5

fact: 
  mov r9, 1

fact_acc:
  cmp r8, 0
  je fact_done
  imul r9, r8
  dec r8  
  jmp fact_acc

fact_done:
exit:
  mov rax, 0x2000001

  mov rdi, r9  ;; it has answer
  syscall

section .data

dummy: db 0
