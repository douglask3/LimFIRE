findRasterTrend <- function(r, seasonal = FALSE) {
	mask = !is.na(r[[1]])
	
	findSubsetTrend <- function(vr) {
				
		lmFUN <- function(y) {
			fit = lm(as.vector(y) ~ c(1:length(y)))
			return(coefficients(fit)[2])
		}
		
		res = apply(vr, 1, lmFUN)
		return(res)
	}
	vr = r[mask]
	trends = r[[1:2]]
	if (seasonal)  {
		start.time = Sys.time()
		vrs = lapply(1:12, function(mn) vr[, seq(mn, dim(vr)[2], 12)])
		cl = makeCluster(c("localhost","localhost","localhost","localhost"),  type = 'SOCK')
			vtrend = clusterApply(cl, vrs, findSubsetTrend)
		stopCluster(cl)
		
		pvs = lapply(1:12, function(mn) vtrend[[mn]]/apply(vrs[[mn]], 1, sd))
		
		trends = layer.apply(vtrend, function(i) {trends[mask] = i; return(trends)})
		print(Sys.time() - start.time)
	} else trends[mask] = findSubsetTrend(r)
	browser()
	return(trends)	
}

Trend <- function(r, smoothFun = running12, ...) {
	if (!is.null(smoothFun)) r = smoothFun(r, ...)
	return(findRasterTrend(r))
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