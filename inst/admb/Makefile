all: ../bin/linux/sole ../bin/linux/plaice

../bin/linux/sole: sole
	mv sole ../bin/linux/
	rm sole.cpp sole.htp sole.obj

sole: sole.tpl
	admb sole.tpl
	g++ -std=c++14 -static -O3 -osole sole.obj `locate libadmb-contrib-x86_64-linux-g++8.a`

../bin/linux/plaice: plaice.tpl
	admb plaice.tpl
	mv plaice ../bin/linux/
	rm plaice.cpp plaice.htp plaice.obj

clean:
	rm -f *.cpp *.obj *.htp
