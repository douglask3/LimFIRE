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
	
	out = logistic(clim_dt, unsquish = FALSE)
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
	print(absTrend)
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
	
	if (!is.null(absTrend) && absTrend) {
		Cs2 = lapply(1:4, for_control, absTrend = absTrend)
		TrendFire = calFire(Cs2)
		noTrendFire = calFire(Cs)
	} else {
		TrendFire = calFire(lims)
		noTrendFire = calFire(Cs)		
	}
	
	if (is.null(absTrend)) {
		browser()#
	}
	
	sTrendFire = sum(TrendFire)
	snoTrendFire = sum(noTrendFire)
	
	index =(sTrendFire - snoTrendFire)/sTrendFire
	index[is.infinite(index)] = NaN
	return(addLayer(index, sTrendFire, snoTrendFire))
	
}

control_distance <- function(trends, files, absTrend = FALSE, ...) {
	
	files = files[[2]]
	files = files[c(1, 2, 4, 3, 5)]
	lims = lapply(files, brick)
	
	fire = lims[[1]]
	fire = fire[[(nlayers(fire)-11):nlayers(fire)]]
	
	lims = lims[-1]
	lims = lapply(lims, function(i) i[[(nlayers(i)-11):nlayers(i)]])
	
	trend = trends[-1][1:4]		
	trend = lapply(trend, function(i) i[[-1]])
	
	lims_no_trend = mapply(function(l, t)
							(logistic(logitFun(l, n = 10000) - t * 14, FALSE)), 
					       lims, trend)
	
	potentalLim <- function(i, r) {
		index = (1:4)[-i]
		(1-r[[i]]) *r[[index[1]]] * r[[index[2]]] * r[[index[3]]]
	}
	
	#plims = lapply(1:4, potentalLim, lims)
	#plims_no_trend = lapply(1:4, potentalLim, lims_no_trend)
	#lims_no_trend = lapply(lims_no_trend, function(i) { i[i < 0] = 0; i[i>1] = 1; i})
	
	
	#month_weightingi <- function(mn, i) {
	#	potential = layer.apply(lims[-i], function(j) j[[mn]])
	#	potential = prod(potential)
		#potential = 1/lims[[i]][[nlayers(lims[[i]]) - 12 + mn]]
	#	return(potential)
	#}
	
	#potential = lapply(1:4, function(...) layer.apply(1:12, month_weightingi, ...))
	#end_trendw = mapply('*', end_trend, potential)
	
	#d1 = (lims_no_trend[[1]] * lims_no_trend[[2]] * lims_no_trend[[3]] * lims_no_trend[[4]])
	#d2 = (lims[[1]] * lims[[2]] * lims[[3]] * lims[[4]])
	#d3 = layer.apply(1:12, function(i) max(addLayer(d1[[i]], d2[[i]])))
	#d3[d3 == 0] = 1
	#d1 = d1/d3; d2 = d2/d3
	#mask = d1[[1]] == 1 & d2[[1]] == 0
	#d1[mask] = 0
	#d2[mask] = 0
	
	#yay = lims_no_trend; wow = lims;
	#lims_no_trend = lapply(lims_no_trend, '*', d1)
	#lims = lapply(lims, '*', d2)
	
	
	end_trend = mapply('-', lims, lims_no_trend)
	mtrend = layer.apply(end_trend, '^', 2)
	#atrend = layer.apply(end_trend, function(i) mean(abs(i)))
	
	DAll = sqrt(sum(mtrend^2))#/2
	
	#DAll = DAll / sqrt(sum(layer.apply(potential, mean)^2))
	#browser()
	return(DAll)	
}