comp = gfortran
std = f2008
src = .
build = ./build
install = ~/bin
shtools = /usr/local
params = -fbackslash -fdec-math

sh_makegrid: mkdir subroutines.o sh_makegrid.o
	$(comp) -o $(build)/sh_makegrid $(params) $(build)/sh_makegrid.o $(build)/subroutines.o -L$(shtools)/lib -lSHTOOLS -lfftw3

sh_expand: mkdir subroutines.o sh_expand.o
	$(comp) -o $(build)/sh_expand $(params) $(build)/sh_expand.o $(build)/subroutines.o -L$(shtools)/lib -lSHTOOLS -lfftw3

sh_expand.o:
	$(comp) -o $(build)/sh_expand.o -c $(src)/sh_expand.f08 -std=$(std) $(params) -I$(shtools)/include -I$(build) -J$(build)

sh_makegrid.o:
	$(comp) -o $(build)/sh_makegrid.o -c $(src)/sh_makegrid.f08 -std=$(std) $(params) -I$(shtools)/include -I$(build) -J$(build)

subroutines.o:
	$(comp) -o $(build)/subroutines.o -c $(src)/subroutines.f08 $(params) -I$(shtools)/include -J$(build)

mkdir:
	mkdir -p $(build)

install:
	cp $(build)/$(exec) $(install)

uninstall: 
	rm $(install)/$(exec)

clean:
	rm -rf $(build)
