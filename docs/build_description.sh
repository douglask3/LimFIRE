rm Model_description.bbl
rm Model_description.aux
pdflatex Model_description.tex
bibtex Model_description
pdflatex Model_description.tex
pdflatex Model_description.tex
xpdf Model_description.pdf
