INCLUDES = ./includes

all: CodeDemo.pdf force

%.pdf : %.tex force
	TEXINPUTS=$(INCLUDES):.: latexmk -pdfxe -interaction=nonstopmode -no-shell-escape $<

%.tex : %.lhs force
	LHS2TEXINPUTS=$(INCLUDES) lhs2TeX -P$(INCLUDES): --poly -o $@ $<

%.hs : %.lhs force
	lhs2TeX -P$(INCLUDES): --newcode -o $@ $<

%.ghci : %.lhs force
	ghci -pgmL lhs2TeX -optL-P$(INCLUDES): -optL--pre $<

%.ghcid : %.lhs force
	ghcid -c "ghci -pgmL lhs2TeX -optL-P$(INCLUDES): -optL--pre" $<

clean :
	rm -f *.aux *.log *.nav *.out *.ptb *.snm *.toc *.xdv *.fls *.fdb_latexmk

force :

.PHONY : force clean
