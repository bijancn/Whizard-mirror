TEX_SOURCES=$(wildcard *.tex)
MP_SOURCES=diagrams.mp plots.mp uml-module-tree.mp uml.mp
MP_TARGETS=$(patsubst %.mp,%-1.mps,$(MP_SOURCES))

.PHONY: remake all clean

remake: $(MP_TARGETS)
	pdflatex muli_manual.tex

all: muli_manual.pdf

muli_manual.pdf: $(TEX_SOURCES) $(MP_TARGETS)
	pdflatex muli_manual.tex
	makeindex muli_manual.idx
	pdflatex muli_manual.tex

%-1.mps: %.mp common.mp
	mpost $<

clean:
	rm -f muli_manual.pdf
	rm -f mptext*
	rm -f mpxerr*
	rm -f *.aux
	rm -f *.mps
	rm -f *.log
	rm -f *.toc
	rm -f *.out
	rm -f *.idx
	rm -f *.ind
	rm -f *.ilg

echo:
	@echo $(MP_SOURCES)
	@echo $(MP_TARGETS)
