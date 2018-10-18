#objects = sh_expand.o 
comp = gfortran
std = f2008
src = .
build = ./build
install = ~/bin
exec = sh_expand_dh
shtools = /usr/local
params = -fbackslash -fdec-math

sh_expand: mkdir
	$(comp) -o $(build)/$(exec) $(params) -I$(shtools)/include/ $(src)/sh_expand.f08 -J$(build) -L$(shtools)/lib/ -lSHTOOLS -lfftw3

mkdir:
	mkdir -p $(build)

install:
	cp $(build)/$(exec) $(install)

uninstall: 
	rm $(install)/$(exec)

clean:
	rm -rf $(build)
