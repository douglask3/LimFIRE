source("cfg.r")

fnameOut = 'outputs/trendTable'
legLabs = c('Burnt Area', 'Fuel'   , 'Moisture', 'Ignitions', 'Suppression', 'Overall Trend', 'Absolute Trend')


loadData4Ecosystem_analysis()
for (i in 1:length(ensamble)) {
	ens = ensamble[[i]]
	#ens[[1]][[1]]= ens[[1]][[1]] / 100
	ensamble[[i]] = c(ens, trendIndex1[[i]], trendIndex2[[i]])
}

trendInClass <- function(r, mask, biome) {
	mask0 = mask
	
	r = abs(r[[1]])
	mask = mask + !is.na(r)
	mask = mask == max.raster(mask, na.rm = TRUE)
	#if (biome == 2) browser()
	vr = r[mask]
	va = area(r)[mask]
	
	ot = wtd.quantile(vr, c(0.1, 0.25, 0.5, 0.75, 0.9), weight = va) ## area weight
	r[r < -100] = -100
	r[r > 100] = 100
	ot = c(ot, mean = sum(vr * va)/ sum(va))
	return(ot)
}

summerize <- function(class, FUN) {
	class = sapply(class, function(i) i)
	ot = apply(class, 1, FUN)
	
	return(ot)
}

trends4Ecosystem <- function(biome) {
	print(biome)
	mask = biomeAssigned == biome
	qs = sapply(ensamble, function(i) lapply(i, trendInClass, mask, biome))

	mn   = apply(qs, 1, summerize, mean)
	sdev = apply(qs, 1, summerize, sd)
	
	colnames(mn)   = legLabs
	colnames(sdev) = legLabs
	
	return(list(mn, sdev))
}

out = lapply(1:8, trends4Ecosystem)	

extractTab <- function(i) {
	tab =  sapply(out, function(j) j[[i]][3,])
	tab =  rbind(tab,sapply(out, function(j) j[[i]][,7]))
	tab = round(tab * 100, 2)
	colnames(tab) = names(biomes)
	return(tab)
}

mn   = extractTab(1) 
sdev = extractTab(2)

fnameOut = paste(fnameOut, c('mn', 'sdev', 'comb'), '.csv', sep = '-')

write.csv(mn, fnameOut[1])
write.csv(sdev, fnameOut[2])

cmb = mn
cmb[] = paste(mn, sdev, sep = '\n')

write.csv(cmb, fnameOut[3])
# ot = paste(standard.round(mn), standard.round(sd), sep = '+/-')

