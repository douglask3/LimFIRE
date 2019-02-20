findRasterTrend <- function(r, seasonal = FALSE, mask = NULL, factor = 1) {
	if (factor > 1) r = aggregate(r, factor)
	if (is.null(mask)) mask = !is.na(r[[1]])
            else mask = aggregate(mask, factor) == 1	
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
            vtrend = lapply(vrs, findSubsetTrend)
            #cl = makeCluster(c("localhost","localhost","localhost","localhost","localhost","localhost"),  type = 'SOCK')
	#	    vtrend = clusterApply(cl, vrs, findSubsetTrend)
	    #stopCluster(cl)
            cat("start time:", start.time)
                vtrend = mclapply(vrs, findSubsetTrend, mc.cores = getOption("mc.cores", 4L))
            end.time = Sys.time()                
            cat("\nend time:", end.time)	
	    cat("\nDiff:")
            
	    trends = lapply(vtrend, function(i) {trends[mask] = t(i); return(trends)})
            
            print(end.time - start.time)
	} else trends[mask] = t(findSubsetTrend(vr))
	
	return(trends)	
}

Trend <- function(r, smoothFun = running12, ...) {
	if (!is.null(smoothFun)) r = smoothFun(r, ...)
	return(findRasterTrend(r))
}

squishBounds <- function(r, n = nlayers(r)) 
	(r*(n - 1) + 0.5) /n


unsquishBounds <- function(r, n = nlayers(r)) 
	(n * r - 0.5)/(n - 1)

logistic <- function(r, unsquish = TRUE, ...) {
	r = f1(r, 0, 1)
	if (unsquish) r = unsquishBounds(r, ...)
	return(r)
}
	
logitFun <- function(r, squish = TRUE, ...) {
	if (squish) r = squishBounds(r, ...)
	r = log(r/(1-r))
	return(r)
}

removeTrend <- function(r, smoothFun = running12, rs = NULL, ...) {
	
	if (is.null(rs)) fireUnControl = 1
	else fireUnControl = rs[[1]] * rs[[2]] * rs[[3]] 
	fireControl = r * fireUnControl
	
	r0 = r
	mask = !is.na(fireControl[[1]])
	
	r = logitFun(r)
	
	trends = findRasterTrend(r, seasonal = TRUE, mask = mask,...)
	
	removeTrend <- function(i) {
		mn = 12*(i/12-floor(i/12))
		yr = ceiling(i/12)
		if (mn==0) mn = 12
		return(r[[i]] - trends[[mn]][[1]] * yr)
	}
        
	r  = layer.apply(1:nlayers(r), removeTrend)	
	
	r = logistic(r)
	
	#r = layer.apply(1:nlayers(r), function(i) r[[i]] - trend[[1]] * i)
	
	fireTrendRm = r * fireUnControl
	fireTrendDf = fireControl - fireTrendRm
	
	trend = sum(fireTrendDf)
	for (i in trends) trend = addLayer(trend, i[[1]])
	return(trend)
}
