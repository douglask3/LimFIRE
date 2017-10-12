Log0 <- function (x, cut = 0.00001) {
	test = x > cut
	x[test] = log(x[test])
	x[!test] = log(cut)
	return(x)
}