install.packages('devtools')
library('devtools')

install.packages('raster')
install.packages('colorspace')
install.packages('spam')
install.packages('fields')
install.packages('mapproj')
install.packages('mapdata')
install.packages('Rcpp') # Needed for benchmarkmetrics
install.packages('mapplots')
install.packages('ellipse')
install.packages('vagan')
install.packages("RcppEigen")
install.packages("parallel")
install.packages("snow"i
install.packages("reldist")
install.packages("shape")

install_github("rhyswhitley/r_stash/rstash")

install_bitbucket('douglask3/gitProjectExtras/gitBasedProjects')
install_github('douglask3/benchmarkmetrics/benchmarkMetrics')
install_bitbucket('douglask3/rasterExtraFuns/rasterExtras')
install_bitbucket('douglask3/rasterExtraFuns/rasterPlotFunctions')


source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

install.packages('plotrix')
