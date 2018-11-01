#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

tempF1 = 'temp/limitations4trends-Tree-alphaMax2'
tempF2 = 'temp/removeControl2'
esnambleTemp <- 'temp/ensamble4contols'

tempFile <- function(fnames, extraName = '') {
	fnames = paste(fnames, extraName, sep = '')
	fnames = paste(fnames, c('fire', 'fuel', 'moisture', 'igntions', 'suppression'), '.nc', sep = '-')
}
loadData4Ecosystem_analysis()
niterations = 11

#########################################################################
## Run model                                                           ##
#########################################################################
ens_files = open_ensembles()

removeControl <- function( lims, crm) {
	cnt = brick(lims[[1]])
	lims = lims[-1]
	lims = lapply(lims, brick)
	mnthRm <- function(mn) {
		print(mn)
		#prod(layer.apply(lims[-crm], function(i) i[[mn]]))
		lim = lims[-crm]
		lim[[1]][[mn]] * lim[[2]][[mn]] * lim[[3]][[mn]]
	}
	#browser()
	#cl = makeCluster(c("localhost","localhost","localhost","localhost"),  type = 'SOCK')
	#	exp = clusterApply(cl, 1:12, fun = mnthRm)
	#stopCluster(cl)
	#exp = layer.apply(exp, function(i) i)
	exp = layer.apply(1:(nlayers(lims[[1]])), mnthRm)
	#exp = layer.apply(1:12, mnthRm)
	
	exp = sum(exp)
	cnt = sum(cnt)
	return(exp - cnt)
}

RunControl <- function(crm) {
	
	RunMember <- function(lims, ensN) {
		lims = lims[[2]]
		tempF2A = tempFile(tempF2, paste(niterations, ensN, sep = '-'))[crm + 1]
		print(tempF2A)
		return(runIfNoFile(tempF2A, removeControl, lims, crm))
	}
	
	out = mapply(RunMember, ens_files, 1:length(ensamble)) 
	out = layer.apply(out, function(i) i) /168
	mn = mean(out)
	sd = sd.raster(out, FALSE)
	
	return(addLayer(mn, sd))
}
	
unburnt = lapply(1:4, RunControl)
unburnt[[2]] = unburnt[[2]] * f1(0.0, param('moisture_x0'), -param('moisture_k'))
unburnt[[4]] = unburnt[[4]] * f1(0.0, param('suppression_x0'), -param('suppression_k'))

totalArea.raster <- function(r) {
	AR  = area(r, na.rm = TRUE)
	out = sum(values(AR*r), na.rm = TRUE)/sum(values(AR), na.rm = TRUE)
}



rbf = layer.apply(ensamble, function(i) mean(i[[1]]))
rbfmean = mean(rbf)
rbfAW = totalArea.raster(rbfmean)

pc_unburnt = 100 * sapply(unburnt, totalArea.raster) / rbfAW
browser()