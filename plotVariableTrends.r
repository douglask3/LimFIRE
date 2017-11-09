#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

fig_fname = 'figs/Trends.png'

tempFile = 'temp/variableTrend'

varnames = c("~alpha~ (= ~AET/PET~)", "EMC", "Cload Cover", "Relative Humidity", "Temperature", "Wet days", "Precipitation", "Bare Ground", "Cropland", "Pasture", "Population Density", "Burnt Area")
names(varnames) = c("alpha", "emc", "Cld", "Hr", "Tas", "Wet", "Prc", "bare", "crop", "pas", "popdens",  "fire") 

limits = list(c(-0.1,-0.05, -0.01, -0.005, 0.005, 0.01, 0.05, 0.1),
			  c(-0.02, -0.01, -0.005, -0.002, 0.002, 0.005, 0.01, 0.02),
			  c(-10, -5, -1, -0.5, -0.1, 0.1, 0.5, 1, 5, 10),
			  c(-10, -5, -1, -0.5, -0.1, 0.1, 0.5, 1, 5, 10),
			  c(-1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1),
			  c(-0.05, -0.01, -0.005, -0.001, 0.001, 0.005, 0.01, 0.05),
			  c(-10, -5, -2, -1, 1, 2, 5, 10),
			  c(-2, -1, -0.5, 0.5, 1, 2),
			  c(-1, -0.1, -0.01, 0.01, 0.1, 1),
			  c(-1, -0.1, -0.01, -0.001, 0.001, 0.01, 0.1, 1),
			  c(-5, -1, -0.1, -0.01, 0.01, 0.1, 1, 5),
			  c(-0.01, -0.005, -0.001, -0.0005, -0.0001, 0.0001, 0.0005, 0.001, 0.005, 0.01) * 100)
			  
cols       = list(alpha   = c("#222200", '#AAAA00', 'white', '#00AAAA', '#000044'),
                  emc     = c('#220022', '#AA00AA', 'white', '#00AAAA', '#002222'),
                  Cld     = c('#FFDDFF', '#880088', 'black', '#008888', '#DDFFFF'),
                  Hr      = c('#222200', '#AAAA00', 'white', '#0000FF', '#000011'),
                  Tas     = c('#000044', '#008888', '#66FF66', '#AAAA00', '#FF2222', '#440000'),
                  Wet     = c('#222200', '#AAAA00', 'white', '#6600CC', '#000044'),
                  Prc     = rev(c('#000044', '#00AAAA', 'white', '#CC8800', '#440000')),
                  bare    = c('#440000', '#DD0077', 'white', '#77DD00', '#004400'),
                  crop    = c('#002222', '#00AAAA', 'white', '#AAAA00', '#222200'),
                  pas     = c('#001144', '#0088CC', 'white', '#CC8800', '#441100'),
                  popdens = c('#004400', '#00FF00', 'white', '#999999'   , 'black'  ),
                  fire    = c('#000022', '#006699', '#00AAFF', 'white', '#FFAA00', '#996600', '#220000'))

units = c('', '', '%', '%', '~DEG~C', 'no. days', 'mm', '%', '%', '%', 'pop~/km2~', '%')
				  
findTrend <- function(r, name) {
	tfname = paste(tempFile, name, '.nc', sep = '-')
	print(tfname)
	return(runIfNoFile(tfname, Trend, r, test = grab_cache))
}


obs = openAllObs()[names(varnames)]

trends = mapply(findTrend, obs, names(varnames))
trends[['fire']][[1]] = trends[['fire']][[1]] * 100
trends[['bare']][[1]] = trends[['bare']][[1]] * (-1)
png(fig_fname, height = 7.3, width = 17, units = 'in', res = 300)
	layout(rbind(c(1, 2, 8, 9),
				 c(3, 6, 10, 11),
				 c(5, 7, 4, 12)))
				 
	par( mar = rep(0, 4), oma = c(3,0,1.3,0))

	plot_vTrend <- function(trend, varname, col, limit, unit) {
		plot_Trend(trend, varname, col, limit, y_range = c(-65, 90))
		add_raster_legend2(col, limit, dat = trend,
						   transpose = FALSE, 
						   plot_loc = c(0.1, 0.8, 0.0, 0.04),  ylabposScling=2.5)
		mtext.units(side = 1, unit, line = -3.5, cex = 0.8)

	}
	mapply(plot_vTrend, trends, varnames, cols, limits, units)
dev.off.gitWatermark()