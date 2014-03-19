PDF=presentation.pdf
PDFC=pdflatex
VIEWER=evince

all : $(PDF)

$(PDF): $(PDF:.pdf=.tex)
	pdflatex $<

show: $(PDF)
	$(VIEWER) $<