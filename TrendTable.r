source("cfg.r")

figName = 'figs/trendHistergrams'
legLabs = c('Burnt Area', 'Fuel'   , 'Moisture', 'Ignitions', 'Suppression', 'Overall Trend')
loadData4Ecosystem_analysis()
for (i in 1:length(ensamble)) ensamble[[i]] = c(ensamble[[i]], trendIndex2[[i]])

trendInClass <- function(r) {
	
	r = abs(r[[1]] )
	mask = !is.na(r)
	vr = r[mask]
	va = area(r)[mask]
	
	ot = wtd.quantile(vr, c(0.1, 0.25, 0.5, 0.75, 0.9), weight = va) ## area weight
	r[r < -100] = -100
	r[r > 100] = 100
	ot = c(ot, mean = sum(vr * va)/ sum(va))
	return(ot)
}

qs = sapply(ensamble[1:10], function(i) lapply(i, trendInClass))

summerize <- function(class, FUN) {
	class = sapply(class, function(i) i)
	ot = apply(class, 1, FUN)
	return(ot)#
}

mn   = apply(qs, 1, summerize, mean)
sdev = apply(qs, 1, summerize, sd  )
	
# ot = paste(standard.round(mn), standard.round(sd), sep = '+/-')

