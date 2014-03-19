PDF=presentation.pdf
PDFC=pdflatex
VIEWER=evince
SRC=$(wildcard *.tex) $(wildcard */*.tex)

all : $(PDF)

$(PDF): $(PDF:.pdf=.tex) $(SRC)
	pdflatex $<

show: $(PDF)
	$(VIEWER) $<