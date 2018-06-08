reportdeps =  report.tex sources.bib
MAKEFLAGS += --silent
all:
	@mkdir -p tmp
	@mkdir -p build
	@cd build; cmake ..; make
	make report.pdf
report.makefile: $(reportdeps)
	lualatex -shell-escape report.tex
report.pdf: report.makefile $(reportdeps)
	make -j4 -f report.makefile
	./latexrun --latex-cmd lualatex --bibtex-cmd biber report.tex
debug:
	@mkdir -p debug_folder
	@cd debug_folder; cmake -DCMAKE_BUILD_TYPE=Debug ..; make
clean:
	latexmk -c
	rm -rf __pycache__ pythontex-files-report *.pytxcode *.auxlock *.run.xml data *.bbl report.pdf tmp/report-figure* *.figlist *.makefile latex.out *.mod
force:
	touch $(reportdeps)
	make report.pdf
clean-all:
	make clean
	rm -rf build debug debug_folder tmp
