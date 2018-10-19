#!/bin/bash

ingrid=grid.in
outgrid=grid.out
outcoeff=coeff.out
incoeff=$outcoeff
degree=720
step=0.25
method=int

./build/sh_expand \
  -in $ingrid\
  -out $outcoeff \
  -deg $degree \
  -mode $method

./build/sh_makegrid \
  -in $incoeff \
  -out $outgrid \
  -deg $degree \
  -mode $method \
  -step $step
