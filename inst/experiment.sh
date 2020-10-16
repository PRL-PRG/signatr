#!/usr/bin/env sh

set -e

for i in $(seq 100)
do
  VAR=$(shuf -n 1 functions.CSV)
  PACKAGE=$(echo $VAR | cut -f1 -d',' | xargs basename)
  FUNCTION=$(echo $VAR | cut -f2 -d',')
  DIR=results
  FILE_NAME=$DIR/$PACKAGE::$FUNCTION
  EXIT_CODE=0
  # Rscript run-one.R $PACKAGE $FUNCTION gbov.RDS || EXIT_CODE=$?
  Rscript run-fun.R $PACKAGE $FUNCTION gbov.RDS 2 $FILE_NAME 5 || EXIT_CODE=$?
  echo $EXIT_CODE >> records.txt
done
