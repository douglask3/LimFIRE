## Copy reqired hooks
hooksPath="gitinfoHooks/"
hooksDir="../.git/hooks/"

fnames=`ls $hooksPath`
cp `find $hooksPath | tail -n +2` $hooksDir

for i in `ls $hooksPath`
do
    f=$hooksDir$i
    echo $f
    chmod +x $f
done


rm Model_description.bbl
rm Model_description.aux
pdflatex Model_description.tex
bibtex Model_description
pdflatex Model_description.tex
pdflatex Model_description.tex
xpdf Model_description.pdf
