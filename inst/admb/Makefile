all: ../bin/linux/sole ../bin/linux/plaice

../bin/linux/sole: sole.tpl
	admb sole.tpl
	mv sole ../bin/linux/
	rm sole.cpp sole.htp sole.obj

../bin/linux/plaice: plaice.tpl
	admb plaice.tpl
	mv plaice ../bin/linux/
	rm plaice.cpp plaice.htp plaice.obj
