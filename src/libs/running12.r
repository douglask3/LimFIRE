running12 <- function(x, ...) 
	layer.apply(12:nlayers(x), function(i) mean(x[[(i-11):i]]))
