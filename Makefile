SPECIAL=+debug_info 
COMP=erlc ${SPECIAL} -o ebin

compile:
	${COMP}	src/mandelbrot.erl
	${COMP}	src/fractal_server.erl
	${COMP}	src/ppool.erl

server:
	erl -cookie 123 -noshell -pz ebin -sname fs -run fractal_server start

client:
	erl -cookie 123 -noshell -pz ebin -sname mb -run mandelbrot start
	
images:
	find ./images/ -iname '*.txt' -exec ruby extern/data2image.rb {} \;

clean:
	rm -rf images/*
