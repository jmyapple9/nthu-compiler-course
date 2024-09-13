all: scanner

scanner: scanner.l
	flex scanner.l
	gcc lex.yy.c -o $@ -lfl

test:
	./scanner < test2.txt > output.rpt
	golden_scanner < test2.txt > output_golden.rpt
	diff output.rpt output_golden.rpt

clean:
	rm scanner lex.yy.c output.rpt output_golden.rpt
