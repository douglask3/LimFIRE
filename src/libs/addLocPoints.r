addLocPoints <- function()
	lapply(hlghtPnts, function(i) do.call(points, c(i, pch = 19)))