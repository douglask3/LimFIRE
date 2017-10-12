addLocPoints <- function(cex = 1, ...) {
	FUN <- function(Pnts, cex)
		lapply(Pnts, function(i) do.call(points, c(i, pch = 19, cex = cex,  ...)))
	
	hlghtPntsNlack = lapply(hlghtPnts, function(i) {i$col = 'black'; i})
	FUN(hlghtPntsNlack, cex *1.5)
	FUN(hlghtPnts, cex)
}