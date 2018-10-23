#!/bin/bash

ingrid=grid.in
outgrid=grid.out
outcoeff=coeff
incoeff=$outcoeff
degree=180
step=0.5
#method=int
#method=ls
#method=dh
method=lsq

./build/sh_expand \
  -in $ingrid\
  -out $outcoeff"_"$degree"_"$method".out" \
  -deg $degree \
  -mode $method

#./build/sh_makegrid \
  #-in $incoeff \
  #-out $outgrid \
  #-deg $degree \
  #-mode $method \
  #-step $step
