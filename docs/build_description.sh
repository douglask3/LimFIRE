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

pdflatex Model_description.tex
bibtex Model_description
pdflatex Model_description.tex
pdflatex Model_description.tex

rm Model_description.bbl
rm Model_description.aux
rm Model_description.blg
rm Model_description.log


gnome-open Model_description.pdf
