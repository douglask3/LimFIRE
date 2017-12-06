running12 <- function(x, ...) 
	layer.apply(12:nlayers(x), function(i) mean(x[[(i-11):i]]))

running12.numeric <- function(x) sapply(12:length(x), function(i) mean(x[(i-11):i]))