#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

tempF1 = 'temp/limitations4trends-Tree-alphaMax2'
tempF2 = 'temp/removeControl'
esnambleTemp <- 'temp/ensamble4contols'

tempFile <- function(fnames, extraName = '') {
	fnames = paste(fnames, extraName, sep = '')
	fnames = paste(fnames, c('fire', 'fuel', 'moisture', 'igntions', 'suppression'), '.nc', sep = '-')
}

niterations = 11

#########################################################################
## Run model                                                           ##
#########################################################################
findLims <- function(line) {
	print(line)
	fnameLine = paste('paramLine', line, sep = "")
	tempF1A = tempFile(tempF1, fnameLine)
	lims  = runIfNoFile(tempF1A, runLimFIREfromstandardIns, raw = TRUE, pline = line, 
					    test = grab_cache)
	return(lims)
	
}
esnambleTemp = paste(esnambleTemp, niterations, sep = '-')
if (file.exists(esnambleTemp)) {
	load (esnambleTemp)
} else {
	ensamble = lapply(seq(0, 1, length.out = niterations), findLims)
	save(ensamble, file = esnambleTemp)
}

removeControl <- function( lims, crm) {

	mnthRm <- function(mn) {
		print(mn)
		prod(layer.apply(lims[c(-1, -crm - 1)], function(i) i[[mn]]))
	}
	
	exp = layer.apply(1:(nlayers(lims[[1]])), mnthRm)
	cnt = lims[[1]]
	
	exp = sum(exp)
	cnt = sum(cnt)
	return(exp - cnt)
}

RunControl <- function(crm) {
	
	RunMember <- function(lims, ensN) {
		tempF2A = tempFile(tempF2, paste(niterations, ensN, sep = '-'))[crm + 1]
		return(runIfNoFile(tempF2A, removeControl, lims, crm))
	}
	out = mapply(RunMember, ensamble, 1:length(ensamble)) 
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