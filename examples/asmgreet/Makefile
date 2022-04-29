NASM = nasm
NASMFLAGS = -f elf -dOS_LINUX -g

SOURCES = greet.asm strlen.asm getstr.asm putstr.asm quit.asm
OBJECTS = $(SOURCES:.asm=.o)


%.o:	%.asm
	$(NASM) $(NASMFLAGS) $< -o $@

prog:	$(OBJECTS)
	$(LD) -m elf_i386 $^ -o $@


clean:
	rm -f $(OBJECTS) prog a.out core
