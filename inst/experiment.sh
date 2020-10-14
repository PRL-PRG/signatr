#!/usr/bin/env sh

for i in $(seq 100)
do
  VAR=$(shuf -n 1 Makefile)
  PACKAGE=$(echo $VAR | cut -f1 -d' ')
  FUNCTION=$(echo $VAR | cut -f4 -d' ')
  Rscript run-fun.R $PACKAGE $FUNCTION . 1 ./ 1 
done
