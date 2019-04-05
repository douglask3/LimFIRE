addLocPoints <- function(cex = 1, shift = 0, controls = c(T, T, T, T), ...) {
	FUN <- function(Pnts, cex)
		lapply(Pnts, function(i) do.call(points, c(i, pch = 19, cex = cex,  ...)))
	
        hlghtPnts = hlghtPnts[controls]
	hlghtPntsNlack = lapply(hlghtPnts, function(i) {i$col = 'black'; i})
	
	if (shift != 0) hlghtPnts = lapply(hlghtPnts, function(i) {i$col = hue_shift(i$col, shift); i})
	
	FUN(hlghtPntsNlack, cex *1.5)
	FUN(hlghtPnts, cex)
}
