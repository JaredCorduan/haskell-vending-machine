##
## Makefile for a specification of the delegation rules, based on:
##
## https://tex.stackexchange.com/questions/40738/how-to-properly-make-a-latex-project
##

# Document name
DOCNAME = Vending

# You want latexmk to *always* run, because make does not have all the info.
# Also, include non-file targets in .PHONY so they are run regardless of any
# file of the given name existing.
.PHONY: $(DOCNAME).pdf $(DOCNAME).ghci all clean

# The first rule in a Makefile is the one executed by default ("make"). It
# should always be the "all" rule, so that "make" and "make all" are identical.
all: $(DOCNAME).pdf $(DOCNAME).hs

##
## CUSTOM BUILD RULES
##

%.tex : %.lhs
	lhs2TeX --poly -o $@ $<

%.hs : %.lhs
	lhs2TeX --newcode --no-pragmas -o $@ $<

ghci:
	ghci -pgmL lhs2TeX -optL--pre $(DOCNAME).lhs

ghcid:
	ghcid -c "ghci -fno-diagnostics-show-caret -pgmL lhs2TeX -optL--pre $(DOCNAME).lhs" -T ":! make"

##
## MAIN LATEXMK RULE
##

# -pdf tells latexmk to generate PDF directly (instead of DVI).
# -pdflatex="" tells latexmk to call a specific backend with specific options.
# -use-make tells latexmk to call make for generating missing files.

# -interaction=nonstopmode keeps the pdflatex backend from stopping at a
# missing file reference and interactively asking you for an alternative.

$(DOCNAME).pdf: $(DOCNAME).tex
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $(DOCNAME).tex

watch: $(DOCNAME).tex
	latexmk -pvc -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $(DOCNAME).tex

clean:
	latexmk -CA
