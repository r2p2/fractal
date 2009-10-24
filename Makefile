SPECIAL=+debug_info
COMP=erlc ${SPECIAL} -o ebin

all: compile run

compile:
	${COMP}	src/mandelbrot.erl

run:
	erl -noshell -pz ebin -run mandelbrot test 
