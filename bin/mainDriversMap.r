negRoot <- function(r) {
	test = r<0
	r[test] = - sqrt(-r[test])
	r[!test] = sqrt(r[!test])
	return(r)
}

mainDriversMap <- function(trend, controls) {
	graphics.off()
	cntrM = layer.apply(controls[-1], mean)
	rootM = layer.apply(cntrM, negRoot)
	
	mask = !is.na(sum(rootM))
	rootMv = layer.apply(rootM, function(i) i[mask])
	
	lims = c(-0.5, -0.2, -0.1, -0.01, 0.01, 0.1, 0.2, 0.5)
	limsr = negRoot(lims)
	cols = make_col_vector(c('blue', 'green', 'red'), limits = limsr) 
	colsv = cols[cut_results(rootMv[[4]], limsr)]
	plot(rootMv[[1]], rootMv[[2]], type = 'n')
	for (cex in c(2, 1, 0.5, 0.4, 0.3, 0.2, 0.1)) points(rootMv[[1]], rootMv[[2]], col = make.transparent(colsv, 0.5), cex = cex, pch = 19	)
	browser()
	mask = (mean(trend) > 1) & (!is.na(trend[[1]]))
	
	plot(cntrM[[1]][!mask], cntrM[[2]][!mask])
}