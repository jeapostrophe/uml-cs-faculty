all: build build/board.pdf

build/board.pdf: build/board-content.tex
	pdflatex board.tex
	mv -f board.pdf $@

build/board-content.tex build: dept.rkt make.rkt
	racket make.rkt
