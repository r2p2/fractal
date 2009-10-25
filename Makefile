SPECIAL=+debug_info 
COMP=erlc ${SPECIAL} -o ebin

all: compile run

compile:
	${COMP}	src/mandelbrot.erl
	${COMP}	src/fractal.erl
	${COMP}	src/ppool.erl

run:
	erl -noshell -pz ebin -run mandelbrot test 
