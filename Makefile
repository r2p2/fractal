SPECIAL=+debug_info 
COMP=erlc ${SPECIAL} -o ebin

compile:
	${COMP}	src/mandelbrot.erl
	${COMP}	src/fractal_server.erl
	${COMP}	src/ppool.erl
	${COMP}	src/gui_client.erl
	g++ -o bin/convert extern/main.cpp `Magick++-config --cppflags --cxxflags --ldflags --libs`

server:
	erl -cookie 123 -noshell -pz ebin -sname fs -run fractal_server start

server_profiling:
	erl -cookie 123 -noshell -pz ebin -sname fs -run fractal_server start_profiling

client:
	erl -cookie 123 -noshell -pz ebin -sname mb -run mandelbrot start

client_profiling:
	erl -cookie 123 -noshell -pz ebin -sname mb -run mandelbrot start_profiling
	
client_gui:
	erl -cookie 123 -noshell -pz ebin -sname mbgui -run gui_client start

images:
	find ./images/ -iname '*.txt' -exec bin/convert 50 {} \;

clean:
	rm -rf images/*
