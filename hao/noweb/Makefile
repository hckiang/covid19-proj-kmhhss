src: main.R filepath-def.R bayesratio.R mixture-ci.R arw-iterators.R readdat2.R utils.R
main.R: main.Rnw
	notangle -Rmain.R main.Rnw > ../src/main.R
filepath-def.R: main.Rnw
	notangle -Rfilepath-def.R main.Rnw > ../src/filepath-def.R
bayesratio.R: main.Rnw
	notangle -Rbayesratio.R main.Rnw   > ../src/bayesratio.R
mixture-ci.R: main.Rnw
	notangle -Rmixture-ci.R main.Rnw   > ../src/mixture-ci.R
arw-iterators.R: main.Rnw
	notangle -Rarw-iterators.R main.Rnw> ../src/arw-iterators.R
readdat2.R: main.Rnw
	notangle -Rreaddat2.R main.Rnw> ../src/readdat2.R
utils.R: main.Rnw
	notangle -Rutils.R main.Rnw> ../src/utils.R
doc: main.Rnw
	noweave -delay main.Rnw > main.tex
	lualatex -interaction=nonstopmode main.tex
	lualatex -interaction=nonstopmode main.tex
	lualatex -interaction=nonstopmode main.tex

