comp = gfortran
std = gnu
src = .
build = ./build
install = ~/bin
shtools = /usr/local
lapack = /usr/x86_64-linux-gnu/lapack
params = -fbackslash -fdec-math -O3 -m64 -mcmodel=large

all: sh_makegrid sh_expand

sh_makegrid: mkdir subroutines.o sh_makegrid.o
	$(comp) -std=$(std) $(params) -o $(build)/sh_makegrid $(build)/sh_makegrid.o $(build)/subroutines.o -L$(shtools)/lib -lSHTOOLS -lfftw3

sh_expand: mkdir sh_expand_ls.o subroutines.o sh_expand.o
	$(comp) -std=$(std) $(params) -o $(build)/sh_expand $(build)/sh_expand_ls.o $(build)/sh_expand.o $(build)/subroutines.o -L$(shtools)/lib -lSHTOOLS -lfftw3 -L$(lapack) -llapack -lblas -lm

sh_expand_ls.o:
	$(comp) -std=$(std) -o $(build)/sh_expand_ls.o -c $(src)/sh_expand_ls.f08 -I$(shtools)/include -I$(build) -J$(build)

sh_expand.o:
	$(comp) -std=$(std) $(params) -o $(build)/sh_expand.o -c $(src)/sh_expand.f08 -I$(shtools)/include -I$(build) -J$(build)

sh_makegrid.o:
	$(comp) -std=$(std) $(params) -o $(build)/sh_makegrid.o -c $(src)/sh_makegrid.f08 -I$(shtools)/include -I$(build) -J$(build)

subroutines.o:
	$(comp) -std=$(std) $(params) -o $(build)/subroutines.o -c $(src)/subroutines.f08 -I$(shtools)/include -J$(build)

mkdir:
	mkdir -p $(build)

install:
	cp $(build)/$(exec) $(install)

uninstall: 
	rm $(install)/$(exec)

clean:
	rm -rf $(build)
