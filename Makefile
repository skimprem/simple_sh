objects = simple_sh.o 
comp = gfortran
std = f2008
src = ./
build = ./build/
install = ~/bin/
exec = simple_sh
shtools = /usr/local
params = -fbackslash 

simple_sh: mkdir $(objects)
	$(comp) -o $(build)$(exec) $(params) $(build)*.o -L$(shtools)/lib/ -lSHTOOLS

mkdir:
	mkdir -p $(build)

simple_sh.o: $(src)simple_sh.f08
	$(comp) -o $(build)simple_sh.o -c -std=$(std) $(params) -I$(shtools)/include/ $(src)simple_sh.f08 -J$(build)

install:
	cp $(build)$(exec) $(install)

uninstall: 
	rm $(install)/$(exec)

clean:
	rm -rf $(build)
