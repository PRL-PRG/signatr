clean:
	rm *_successes.csv *_failures.csv

exp:
	Rscript run-fun.R NULL add1 . 1 ./ 5

fail:
	Rscript run-fun.R covr report . 1 ./ 5

succ:
	Rscript run-fun.R rrr square . 1 ./ 5
