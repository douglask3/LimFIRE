findRasterTrend <- function(r, seasonal = FALSE) {
	mask = !is.na(r[[1]])
	findSubsetTrend <- function(ri) {
		vr = ri[mask]

		lmFUN <- function(y) {
			fit = lm(as.vector(y) ~ c(1:length(y)))
			p = summary(fit)[[4]][,4][2]
			dy = coefficients(lm(as.vector(y) ~ c(1:length(y))))[2]
			return(c(gradient = dy, p = p))
		}

		res = apply(vr, 1, lmFUN)
		trend = r[[1:2]]

		trend[[1]][mask] = res[1,]
		trend[[2]][mask] = res[2,]
		return(trend)
	}
	if (seasonal) 
		trends = lapply(1:12, function(mn) findSubsetTrend(r[[seq(mn, nlayers(r), 12)]]))
	else trends = findSubsetTrend(r)
	
	return(trends)	
}

Trend <- function(r, smoothFun = running12, ...) {
	if (!is.null(smoothFun)) r = smoothFun(r, ...)
	mask = !is.na(r[[1]])
	vr = r[mask]

	lmFUN <- function(y) {
		fit = lm(as.vector(y) ~ c(1:length(y)))
		p = summary(fit)[[4]][,4][2]
		dy = coefficients(lm(as.vector(y) ~ c(1:length(y))))[2]
		return(c(gradient = dy, p = p))
	}

	res = apply(vr, 1, lmFUN)
	trend = r[[1:2]]

	trend[[1]][mask] = res[1,]
	trend[[2]][mask] = res[2,]
	
	return(trend)	
}


removeTrend <- function(r, smoothFun = running12, rs, ...) {
	fireUnControl = rs[[2]] * rs[[3]] * rs[[4]]
	fireControl = r * fireUnControl
	r0 = r
	trends = findRasterTrend(r, seasonal = TRUE)
	
	
	removeTrend <- function(i) {
		mn = 12*(i/12-floor(i/12))
		yr = ceiling(i/12)
		if (mn==0) mn = 12
		return(r[[i]] - trends[[mn]][[1]] * yr)
	}
	r  = layer.apply(1:nlayers(r), removeTrend)
		
	#r = layer.apply(1:nlayers(r), function(i) r[[i]] - trend[[1]] * i)
	
	fireTrendRm = r * fireUnControl
	fireTrendRm = fireControl - fireTrendRm
	
	trend = trends[[1]]
	trend[[1]] = sum(fireTrendRm)
	trend[[2]] = mean(layer.apply(trends, function(t) t[[2]]))
	return(trend)
}