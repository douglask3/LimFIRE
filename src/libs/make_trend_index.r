make_trend_indexX <- function(T, lims) {
	calDTbyDX <- function(i) {
		Ti = T[[i]]
		cj = lims[-i]
		ci = lims[[i]]
		
		F_i = cj[[2]] * cj[[3]] * cj[[4]]
		F_j = F_i * ci
		
		
		fireTrendRm = F_j - Ti[[1]]	
		r = fireTrendRm / F_i
		
		r = logitFun(r)
		
		
		
	}
	
	dtBYdx = layer.apply(2:5, calDTbyDX)
	browser()
	
	
}

make_trend_index_control_month <- function(trend, lim, lims, yr, absTrend = FALSE) {
	clim = logitFun(lim, n = 10000)	
	
	if (absTrend) clim_dt = clim + yr * (abs(trend) + trend * (-1))
	else clim_dt = clim + trend * (-1) * yr
	
	out = logistic(clim_dt, n = 10000)
	return(out)
}

make_trend_index_control <- function(trend, lim, lims, ...) {
	mnIndex = rep(1:12, length.out = nlayers(lim))
	
	for_month <- function(mn, i) {
		mtrend = trend[[mn]]
		lim = lim[[i]]
		lims = layer.apply(lims, function(l) l[[i]])
		yr = ceiling(i/12)
		make_trend_index_control_month(mtrend, lim, lims, yr, ...)
	}
	out = mapply(for_month, mnIndex, 1:nlayers(lim))
	out = layer.apply(out, function(i) i)
	return(out)
}


make_trend_index <- function(trends, files, absTrend = FALSE, ...) {
	
	files = files[[2]]
	files = files[c(1, 2, 4, 3, 5)]
	lims = lapply(files, brick)
	fire = lims[[1]]
	lims = lims[-1]
	trend = trends[-1]
	
	for_control <- function(i, ...) {
		lim = lims[[i]]
		lims = lims[-i]
		
		trend = trend[[i]][[-1]]
		make_trend_index_control(trend, lim, lims, ...)
	}
	Cs = lapply(1:4, for_control, absTrend = FALSE)
	
	calFireMn <- function(i, x)
		x[[1]][[i]] * x[[2]][[i]] * x[[3]][[i]] * x[[4]][[i]]
		
	calFire <- function(x)
		layer.apply(1:nlayers(fire), calFireMn, x)
	
	if (absTrend) {
		Cs2 = lapply(1:4, for_control, absTrend = absTrend)
		TrendFire = calFire(Cs2)
		noTrendFire = calFire(Cs)
	} else {
		TrendFire = calFire(lims)
		noTrendFire = calFire(Cs)		
	}
	
	sTrendFire = sum(TrendFire)
	snoTrendFire = sum(noTrendFire)
	
	index =(sTrendFire - snoTrendFire)/sTrendFire
	index[is.infinite(index)] = NaN
	return(addLayer(index, sTrendFire, snoTrendFire))
	
}