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

standardLimitation <- function(lims, crm) {
    lims = lims[-1][[crm]]
    print(lims)
    return(mean(brick(lims)))
}

potentialLimiation <- function( lims, crm) {
        print(lims)
        
	cnt = brick(lims[[1]])
	lims = lims[-1]
	lims = lapply(lims, brick)
        
	mnthRm <- function(mn) {
		print(mn)
		#prod(layer.apply(lims[-crm], function(i) i[[mn]]))
                cnt_mn = lims[[1]][[mn]] * lims[[2]][[mn]] * lims[[3]][[mn]] * lims[[4]][[1]]
                f0 = max.raster(cnt[[mn]],na.rm = TRUE) / max.raster(cnt_mn, na.rm = TRUE)
                cnt_mn = cnt_mn * f0		
                pop = cnt_mn/lims[[crm]][[mn]]
                return(pop - cnt_mn)
	}

        mnth <- function(mn) 
            lims[[1]][[mn]] * lims[[2]][[mn]] * lims[[3]][[mn]] * lims[[4]][[1]]
	
	exp = exp0 = layer.apply(1:(nlayers(lims[[1]])), mnthRm)
        #cnt = cnt0 = layer.apply(1:(nlayers(lims[[1]])), mnth)
	exp = mean(exp)
	return(exp)
}

RunControl <- function(crm, FUN = potentialLimiation, name = "potential", fileLayer = 2) {	
	RunMember <- function(lims, ensN) {
		lims = lims[[fileLayer]]
		tempF2A = tempFile(tempF2, paste(name, niterations, ensN, sep = '-'))[crm + 1]
		return(runIfNoFile(tempF2A, FUN, lims, crm, test = TRUE))
	}
        
        #out = mapply(RunMember, ens_files, (1:length(ensamble)))
        print(crm)
	out = mcmapply(RunMember, ens_files, (1:length(ensamble)),
                        mc.cores = getOption("mc.cores", 4L))
        out = layer.apply(out, function(i) i)
	mn = mean(out)
	sd = sd.raster(out, FALSE)
	
	return(addLayer(mn, sd))
}

runType <- function(...)
    lapply(1:4, RunControl, ...)

standard = runType(standardLimitation, "standard")
	
potential = runType(potentialLimiation, "potential3")
potential[[2]] = potential[[2]] * f1(0.0, param('moisture_x0'), -param('moisture_k'))
potential[[4]] = potential[[4]] * f1(0.0, param('suppression_x0'), -param('suppression_k'))
potential[[4]][potential[[4]][[1]] > 1] = NaN
sensitivity = runType(standardLimitation, "sensitivity", 3)

totalArea.raster <- function(r) {
	AR  = area(r, na.rm = TRUE)
	out = sum(values(AR*r), na.rm = TRUE)/sum(values(AR), na.rm = TRUE)
        return(out)
}

limType4Biome <- function(biomeN, cex, lim, index, scaleUp = FALSE) {
    
    cat("biome", biomeN, "\n", "lim", cex, "\n\n")
    lim = lim[[cex]][[index]]
    mask = !is.na(lim[[1]])
    if (biomeN > 1) mask = mask & biomeAssigned == biomeN
    lim[!mask] = NaN
    out = totalArea.raster(lim)
    if (scaleUp) out = out * 168
    return(out)
}

limType <- function(name, index,  ...) {
    out = sapply(c(1,3,2,4), function(i) sapply(1:8, limType4Biome, i, index = index, ...))
    colnames(out) = c('fuel', 'moisture', 'ignitions', 'suppression')
    rownames(out) = names(biomes)
    fname = paste0('outputs/', 'limitation_type_', 
                   name, '_', c('mean', 'sd')[index], '_table', '.csv')
    write.csv(out, fname)
}

limType_Mn_Sd <- function(...) {
    for (index in 1:2) limType(index = index, ...)
}

limType_Mn_Sd('standard', standard, scaleUp = TRUE)
limType_Mn_Sd('potential', potential)
limType_Mn_Sd('sensitivity', sensitivity)
#rbf = layer.apply(ensamble, function(i) mean(i[[1]]))
#rbfmean = mean(rbf)
#rbfAW = totalArea.raster(rbfmean)

#pc_unburnt = 100 * sapply(potential, totalArea.raster) / rbfAW
#
