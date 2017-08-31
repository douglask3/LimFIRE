Log0 <- function (x) {
	test = x > 0.0000001
	x[test] = log(x[test])
	x[!test] = log(0.0000001)
	return(x)
}