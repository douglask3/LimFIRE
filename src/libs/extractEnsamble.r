extractEnsamble <- function(ensamble, id, FUN)  apply(sapply(ensamble, function(i) i[[id]]), 1, FUN)

fisherPval <- function(pvals)
	-2 * sum(log(1 - pvals))
	
grabCommonField <- function(rs, i)  layer.apply(rs, function(r) r[[i]])

summary.ens <- function(ens) {
	grabField <- function(i) grabCommonField(ens, i)
	mn = mean(grabField(1))
	rg = sd.raster(grabField(1))
	
	#rg[[1]] < 0 & rg[[2]] > 0
	#rg = 1 - rg
	
	fisher = fisherPval(grabField(2))
	return( addLayer(mn, fisher, rg))
}

brick.ens <- function(i) layer.apply(i, function(i) i)

meanSD.ens <- function(i, ...) {
	i = brick.ens(i)
	mn = mean(i, na.rm = TRUE)
	sd = sd.raster(i, ...)
	return(addLayer(mn, sd))
}

mean90quant.ens <- function(i, ...) {
	print("yay")
	i = brick.ens(i)
	mn = mean(i, na.rm = TRUE)
	
	vi = values(i)
	mask = apply(vi, 1, function(i) !all(is.na(i)))
	qu = apply(vi[mask,], 1, quantile, c(0.1, 0.9), na.rm = TRUE)
	
	mask = !all(is.na(i))
	qu = layer.apply(1:2, function(i) {mn[mask] = qu[i,]; mn})
	print("wow")
	return(addLayer(mn, qu))
}