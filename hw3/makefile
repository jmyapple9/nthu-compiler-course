all: compile 
compile:
	flex scanner.l
	yacc -d -v parser.y
	g++ -o codegen lex.yy.c y.tab.c -lfl 

spk:
	./codegen < t.c
	riscv32-unknown-elf-gcc -o prog main.c codegen.S
	spike pk prog

clean:
	rm lex.yy.c y.tab.c y.tab.h y.output codegen prog