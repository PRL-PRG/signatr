clean:
	rm *_successes.csv *_failures.csv

succ:
	Rscript run-fun.R rrr square . 1 ./ 5

fail:
	Rscript run-fun.R covr report . 1 ./ 5

part:
	Rscript run-fun.R rrr square . 60 ./ 50000000
	
exp:
	Rscript run-fun.R NULL add1 . 1 ./ 5

