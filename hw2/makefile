# all: compile test diff
all: compile

compile:
	flex scanner.l
	byacc -d -v parser.y
	gcc -o parser lex.yy.c y.tab.c -lfl

# test:
# 	./parser < test.txt > output.xml
# 	golden_parser < test.txt > golden.xml

# diff:
# 	diff ./output.xml ./golden.xml

clean:
	rm y.tab.h y.tab.c lex.yy.c parser y.output output.xml golden.xml
