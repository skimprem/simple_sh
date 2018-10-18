objects = sh_expand_dh.o 
comp = gfortran
std = f2008
src = .
build = ./build
install = ~/bin
exec = sh_expand_dh
shtools = /usr/local
params = -fbackslash 

sh_expand_dh: mkdir $(objects)
	$(comp) -o $(build)/$(exec) $(params) $(build)/*.o -L$(shtools)/lib/ -lSHTOOLS -lfftw3

mkdir:
	mkdir -p $(build)

sh_expand_dh.o: $(src)/sh_expand_dh.f08
	$(comp) -o $(build)/sh_expand_dh.o -c -std=$(std) $(params) -I$(shtools)/include/ $(src)/sh_expand_dh.f08 -J$(build)

install:
	cp $(build)/$(exec) $(install)

uninstall: 
	rm $(install)/$(exec)

clean:
	rm -rf $(build)
