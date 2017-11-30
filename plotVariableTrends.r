#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

fig_fnames = paste('figs/', c('Variables','Trends'), '.png', sep = '')

tempFileTrend = 'temp/variableTrend'
tempFileVarMn = 'temp/variableMean'

varnames = c("~alpha~ (= ~AET/PET~)", "EMC", "Cload Cover", "Relative Humidity", "Temperature", "Wet days", "Precipitation", "Bare Ground", "Tree Cover", "Cropland", "Pasture", "Population Density", "Lightning", "Burnt Area")
names(varnames) = c("alpha", "emc", "Cld", "Hr", "Tas", "Wet", "Prc", "bare", "tree", "crop", "pas", "popdens",  "Lightn", "fire") 

limits     = list(alpha   = c(-0.1,-0.05, -0.01, -0.005, 0.005, 0.01, 0.05, 0.1),
				  emc     = c(-0.02, -0.01, -0.005, -0.002, 0.002, 0.005, 0.01, 0.02),
				  Cld     = c(-10, -5, -1, -0.5, -0.1, 0.1, 0.5, 1, 5, 10),
				  Hr      = c(-10, -5, -1, -0.5, -0.1, 0.1, 0.5, 1, 5, 10),
				  Tas     = c(-1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1),
				  Wet     = c(-0.05, -0.01, -0.005, -0.001, 0.001, 0.005, 0.01, 0.05),
				  Prc     = c(-10, -5, -2, -1, 1, 2, 5, 10),
				  bare    = c(-2, -1, -0.5, 0.5, 1, 2),
				  tree    = c(-2, -1, -0.5, 0.5, 1, 2),
				  crop    = c(-1, -0.1, -0.01, 0.01, 0.1, 1),
				  pas     = c(-1, -0.1, -0.01, -0.001, 0.001, 0.01, 0.1, 1),
				  popdens = c(-5, -1, -0.1, -0.01, 0.01, 0.1, 1, 5),
				  Lightn  = c(-5, -1, -0.1, -0.01, 0.01, 0.1, 1, 5),
				  fire    = c(-0.01, -0.005, -0.001, -0.0005, -0.0001, 0.0001, 0.0005, 0.001, 0.005, 0.01) * 100)
			  
limits_vs  = list(alpha   = c(0, 0.1, 0.2, .3, .4, .5, .6, .7, .8, .9),
				  emc     = c(0, 0.1, 0.2, .3, .4, .5, .6, .7, .8, .9),
				  Cld     = c(1, 2, 5, 10, 20, 40, 60, 80),
				  Hr      = c(1, 2, 5, 10, 20, 40, 60, 80),
				  Tas     = c(-20, -10, -5, 5, 10, 15, 20, 25, 30),
				  Wet     = c(0, 0.1, 0.2, 0.4, 0.6, 0.8),
				  Prc     = c(0, 1, 2, 5, 10, 20, 50, 100, 200),
				  bare    = c(1, 2, 5, 10, 20, 40, 60, 80),
				  tree    = c(1, 2, 5, 10, 20, 40, 60, 80),
				  crop    = c(1, 2, 5, 10, 20, 40, 60, 80),
				  pas     = c(1, 2, 5, 10, 20, 40, 60, 80),
				  popdens = c(0.01, 0.1, 1, 10, 100, 1000),
				  Lightn  = c(0.01, 0.1, 0.2, 0.5, 1, 2, 5),
				  fire    = c(0, 1, 2, 5, 10, 20, 50))
			  
cols       = list(alpha   = c("#222200", '#AAAA00', 'white', '#00AAAA', '#000044'),
                  emc     = c('#220022', '#AA00AA', 'white', '#00AAAA', '#002222'),
                  Cld     = c('#FFDDFF', '#880088', 'black', '#008888', '#DDFFFF'),
                  Hr      = c('#222200', '#AAAA00', 'white', '#0000FF', '#000011'),
                  Tas     = c('#000044', '#008888', '#66FF66', '#AAAA00', '#FF2222', '#440000'),
                  Wet     = c('#222200', '#AAAA00', 'white', '#6600CC', '#000044'),
                  Prc     = rev(c('#000044', '#00AAAA', 'white', '#CC8800', '#440000')),
                  bare    = c('#440000', '#DD0077', 'white', '#77DD00', '#004400'),
                  tree    = c('#002222', '#00AAAA', 'white', '#AAAA00', '#222200'),
                  crop    = c('#002222', '#00AAAA', 'white', '#AAAA00', '#222200'),
                  pas     = c('#001144', '#0088CC', 'white', '#CC8800', '#441100'),
                  popdens = c('#004400', '#00FF00', 'white', '#999999'   , 'black'  ),
                  Lightn  = c('#004400', '#00FF00', 'white', '#999999'   , 'black'  ),
                  fire    = c('#000022', '#006699', '#00AAFF', 'white', '#FFAA00', '#996600', '#220000'))

units 	       = c(alpha   = '', 
				   emc     = '', 
				   Cld     = '%', 
				   Hr      = '%', 
				   Tas     = '~DEG~C', 
				   Wet     = 'no. days', 
				   Prc     = 'mm', 
				   bare    = '%', 
				   tree    = '%', 
				   crop    = '%',
				   pas     = '%', 
				   popdens ='pop~/km2~', 
				   Lightn  = 'Strikes',
				   fire    = '%')
	
obs = openAllObs()[names(varnames)]

findFun <- function(r, name, tempFile, FUN) {
	tfname = paste(tempFile, name, '.nc', sep = '-')
	return(runIfNoFile(tfname, FUN, r, test = grab_cache))
}
findsFun <- function(...) mapply(findFun, obs, names(varnames), MoreArgs = list(...))

trends = findsFun(tempFileTrend, Trend)
varMns = findsFun(tempFileVarMn,  mean)

trends[['fire']][[1]] = trends[['fire']][[1]] * 100
trends[['bare']][[1]] = trends[['bare']][[1]] * (-1)
varMns[['fire']]      = varMns[['fire']]      * 12 * 100

plotVars <- function(xs, fig_fname, cols, limits, ...) {
	print(fig_fname)
	png(fig_fname, height = 7.3 * 5 / 3, width = 17, units = 'in', res = 300)
		layout(rbind(c(8, 9, 13, 13, 10),
				     c(0, 1, 11, 11, 12),
					 c(2, 3, 0 , 0 , 0 ),
					 c(2, 3, 0 , 14, 14),
					 c(4, 5, 0 , 14, 14),
					 c(6, 7, 0 , 0 , 0)),
					 widths = c(1,1,0.3, 0.7, 1), heights = c(1, 1, 0.3, 0.7, 1, 1))
		
		
		#layout(rbind(c(1, 2, 8, 9),
	#				 c(3, 6, 10, 11),
	##				 c(5, 7, 4, 12)))
					 
		par( mar = rep(0, 4), oma = c(3,0,1.3,0))

		plot_vTrend <- function(x, varname, col, limit, unit) {
			
			plot_Trend(x, varname, col, limit, y_range = c(-65, 90), ...)
			add_raster_legend2(col, limit, dat = x,
							   transpose = FALSE, 
							   plot_loc = c(0.1, 0.8, 0.0, 0.04),  ylabposScling=2.5)
			mtext.units(side = 1, unit, line = -3.5, cex = 0.8)

		}
		mapply(plot_vTrend, xs, varnames, cols, limits, units)
	dev.off.gitWatermark()
}

#cols = mapply(make_col_vector, r = cols, limits = limits)
plotVars(trends, fig_fnames[2], cols, limits)

#index  = lapply(limits, function(i)    which(i>0))
#cols   = mapply(function(v, i) v[i], cols, index)
cols = lapply(cols, function(i) i[ceiling(length(i)/2):length(i)])

plotVars(varMns, fig_fnames[1], cols, limits_vs, scaling = 1)
