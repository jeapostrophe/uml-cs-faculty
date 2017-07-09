all: build build/board.pdf

push: build/board.pdf
	cd build && (git commit -m "Update" . ; git push origin gh-pages)

build/board.pdf: build/board-content.tex
	pdflatex board.tex
	mv -f board.pdf $@

build/board-content.tex build: dept.rkt make.rkt build/img build/style.css
	racket make.rkt

build/img: img
	ln -sf ../img $@

build/style.css: style.css
	cp -f $^ $@

labels.pdf: labels.tex
	pdflatex labels.tex

clean:
	rm -f build/board.pdf

remake: clean all
