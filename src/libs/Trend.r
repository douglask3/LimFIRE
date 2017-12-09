findRasterTrend <- function(r, seasonal = FALSE, mask = NULL, factor = 1) {
	
	if (factor > 1) r = aggregate(r, factor)
	if (is.null(mask)) mask = !is.na(r[[1]])

	findSubsetTrend <- function(vr) {				
		lmFUN <- function(y) {
			fit = lm(as.vector(y) ~ c(1:length(y)))
			trend = coefficients(fit)[2]
			return(c(trend, summary(fit)[[4]][2,4], trend/sd(y)))
		}
		
		res = apply(vr, 1, lmFUN)
		return(res)
	}
	vr = r[mask]
	trends = r[[1:3]]
	
	if (seasonal)  {
		start.time = Sys.time()
		vrs = lapply(1:12, function(mn) vr[, seq(mn, dim(vr)[2], 12)])
		cl = makeCluster(c("localhost","localhost","localhost","localhost"),  type = 'SOCK')
			vtrend = clusterApply(cl, vrs, findSubsetTrend)
		stopCluster(cl)
		browser()
		
		makeTrends <- function(pv, vr) {			
			#sdvs = apply(vr, 1, sd)
			trends[mask] = pv
			trends[[2]][mask] = sdvs/pv
			return(trends)
		}
		
		trends = mapply(makeTrends, vtrend, vrs)
		
		print(Sys.time() - start.time)
	} else trends[mask] = t(findSubsetTrend(vr))
	
	return(trends)	
}

Trend <- function(r, smoothFun = running12, ...) {
	if (!is.null(smoothFun)) r = smoothFun(r, ...)
	return(findRasterTrend(r))
}


removeTrend <- function(r, smoothFun = running12, rs, ...) {
	fireUnControl = rs[[1]] * rs[[2]] * rs[[3]]
	fireControl = r * fireUnControl
	r0 = r
	mask = !is.na(fireControl[[1]])
	trends = findRasterTrend(r, seasonal = TRUE, mask = mask,...)
		
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