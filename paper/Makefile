# Latex Makefile (assumes latexmk is available as compiler)

PAPER=paper.tex

all: ## Compile paper
		latexmk -pdf --shell-escape $(PAPER)

clean: ## Clean output files
		latexmk -c
