clean:
	rm *.csv

trivial:
	Rscript run-fun.R NULL add1 gbov.RDS 1 ./ 5

simple:
	Rscript run-fun.R rrr square gbov.RDS 1 ./ 5

complex:
	Rscript run-fun.R covr report gbov.RDS 1 ./ 5

part:
	Rscript run-fun.R rrr square gbov.RDS 60 ./ 50000000
	
one:
	Rscript run-one.R rrr square gbov.RDS

hundred:
	./experiment.sh