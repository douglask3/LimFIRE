loadObsSample <- function(...) {
	Obs = openAllObs()
	Obs = applySampleMask(Obs, ...)
	return(Obs)
}

applySampleMask <- function(x, fracSample = NULL) {
	mask = layer.apply(x, function(i) is.na(i[[1]]))
	mask = sum(mask) == 0
	if (!is.null(fracSample)) {
		samples = sample(c(0,1), length(mask[]), TRUE, prob = c(1/fracSample, 1-1/fracSample)) == 1
		mask[samples] = 0.0
	}

	x = sapply(x, function(i) i[mask])

	return(x)
}