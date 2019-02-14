#########################################################################
## cfg                                                                 ##
#########################################################################

source('cfg.r')
graphics.off()

grab_cache = TRUE

fignames = paste('figs/',
                c('inputs_mean','inputs_fireSeason', 'inputs_trend',
                  'measure_mean', 'measure_fire',
                  'input_correlation'),
                '.png', sep = '')

names_input = names(drive_fname)


cols_input = list(bare     = c('white', '#77DD00', '#004400'),
				  alphaMax = c('white', '#AA00AA', '#220022'),
				  tree     = c('white', '#AAAA00', '#113300'),
				  alpha    = c('white', '#AA00AA', '#220022'),
                  emc      = c('white', '#00AAAA', '#002222'),
                  Lightn   = c('black', '#0000FF', 'yellow'  ),
                  pas      = c('white', '#CC8800', '#441100'),
                  popdens  = c('white', 'grey'   , 'black'  ),
                  crop     = c('white', '#AAAA00', '#222200'),
                  fire     = fire_cols)

lims_input = list(bare     = c(0, 20, 40, 60, 80, 90, 95),
                  alphaMax = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4),
				  tree     = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8) * 100,
				  alpha    = c(0.2, 0.4, 0.6, 0.8, 1.0),
                  emc      = c(5, 10, 20, 40, 60, 80),
                  Lightn   = c(0.01, 0.1, 0.2, 0.5, 1, 2, 3),
                  pas      = c(1, 2, 5, 10, 20, 50),
                  popdens  = c(0.01, 0.1, 1, 10, 100, 1000),
                  crop     = c(0.1, 0.3, 1, 3, 10, 30),
                  fire     = fire_lims)
				  
colt_input = list(bare     = c('#220022', '#DD0077', 'white', '#77DD00', '#004400'),
				  alphaMax = c('#440000','#AAAA00', 'white', '#00AAAA', '#220022'),
				  tree     = c('#330011', '#DD0077', 'white', '#AAAA00', '#113300'),
				  alpha    = c('#440000','#AAAA00', 'white', '#00AAAA', '#220022'),
                  emc      = c('#222200', '#AAAA00', 'white', '#00AAAA', '#002222'),
                  Lightn   = c('white', 'white'),#c('black', '#666666', 'white', 'yellow', '#440000'  ),
                  pas      = c('#001144', '#00AAAA', 'white', '#CC8800', '#441100'),
                  popdens  = c('#004400', '#00FF00', 'white', '#AA00AA'   , '#220022'  ),
                  crop     = c('#002222', '#00AAAA', 'white', '#AAAA00', '#222200'),
                  fire     = dfire_cols)

limt_input = list(bare     = c(-1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1),
                  alphaMax = c(-0.05, -0.02, -0.01, -0.001, 0.001, 0.01, 0.02, 0.05),
				  tree     = c(-2, -1, -0.2, -0.1, 0.1, 0.2, 1, 2),
				  alpha    = c(-0.02, -0.01, -0.005, -0.001, 0.001, 0.005, 0.01, 0.02),
                  emc      = c(-0.5, -0.1, -0.05, -0.01, 0.01, 0.05, 0.1, 0.5),
                  Lightn   = c(NaN),
                  pas      = c(-0.1, -0.01, -0.001, 0.001, 0.01, 0.1),
                  popdens  = c(-10, -1, -0.1, -0.01, 0.01, 0.1, 1, 10),
                  crop     = c(-1, -0.5, -0.1, 0.1, 0.5, 1),
                  fire     = c(-0.1, -0.05, -0.01, -0.005, -0.001, 0.001, 0.005, 0.01, 0.05, 0.1))

                 
cols_msure = list(fuel    = c('white', '#33FF33', '#002200'),
                  moisture= c('white', '#3333FF', '#000022'),
                  igntions= c('white', '#FF3333', '#220000'),
                  supress = c('white', '#666666'   , 'black'  ))
                  
lims_msure = list(fuel    = seq(0.1, 0.8, 0.1) * 100,
                  moisture= seq(0.1, 0.6, 0.1) * 100,
                  igntions= c(0.1, 1, 2, 5, 10, 20, 30),
                  supress = c(1, 2, 5, 10, 20, 50))

names_msure = c('a) Fuel continuity', 'b) Fuel moisture', 'c) Ignitions', 'd) Suppression')
units_msure = c('%', '%', '/~km2~', 'index')  

names_input = c("a) Vegetation cover", "b) ~alpha~~_max~", "c) Tree cover", "d) ~alpha~", "e) EMC", 
			    "f) Cloud-ground lightning", "g) Pasture", "h) Population density", 
				"i) Cropland", "j) Burnt area")
units_input = c('%', '', '%', '', '%', 'flashes/~km2~', '%', 'people/~km2~', '%', '%')
                
#########################################################################
## open data                                                           ##
#########################################################################
fire_season_r =  runIfNoFile('temp/fire_season_input_plot.nc', fire_season, test = grab_cache)

## Individual inputs
openMean <- function(fname_in, FUN = mean.stack, fname_ext = '-mean.nc', ...){
    fname_out = replace.str(fname_in , 'outputs/', 'temp/')
    fname_out = replace.str(fname_out, '.nc', fname_ext)
	print(fname_out)
    return(runIfNoFile(fname_out, FUN, fname_in, ..., test = grab_cache))
}

trend.brick <- function(fname) {
	r = brick(fname)
	r_t = Trend(r)
	return(r_t)
}

# open Obs
#Obs = lapply(drive_fname, stack)
#Obs = Obs[names(cols_input)]

# monthly mean                    
Obs_mean = lapply(drive_fname, openMean)
Obs_mean = Obs_mean[names(cols_input)]

# monthly mean during fire season height
Obs_fire = lapply(drive_fname, openMean, fire.stack, '-season.nc', fire_season_r)
Obs_fire = Obs_fire[names(cols_input)]

# monthly mean during fire season height
#Obs_trnd = lapply(drive_fname, openMean, trend.brick, '-trend.nc')
#Obs_trnd = Obs_trnd[names(cols_input)]

## Combined Inputs (i.e, fuel, mositure, igntions, supression measures)
fnames = fnames = c('nnfire', 'fuel', 'moisture', 'igntions', 'supression')
fnames_mod  = paste('temp/', fnames    , '-measuresOnly.nc', sep = '')
fnames_mean = paste('temp/', fnames[-1], '-measuresMean.nc', sep = '')
fnames_fire = paste('temp/', fnames[-1], '-measuresFire.nc', sep = '')
      
	  
#measures = runIfNoFile(fnames_mod, runLimFIREfromstandardIns,
#                       just_measures = TRUE, raw = TRUE, test = grab_cache)
					   
#measures = measures[-1]
#measures_mean = runIfNoFile(fnames_mean, function(i) lapply(i, mean), measures, test = grab_cache)
#measures_fire = runIfNoFile(fnames_fire, function(i) lapply(i, fire.stack, fire_season_r), measures, test = grab_cache)

#########################################################################
## Plot maps                                                           ##
#########################################################################
plot_inputs <- function(Obs, fname, names = names_input, units = units_input,
                        lims = lims_input, cols = cols_input, 
						lmat = NULL, lineMod = 0,...) {
    print(fname)
    
    plot_input <- function(x, lim, col, name, unit= '') {
		if (nlayers(x) == 1) e = NULL else e = x[[2]]
        dev.new()
        plot_raster(x, lim, col, quick = TRUE, limits_error = c(0.05, 0.1), e = e,
							ePatternRes = 40, ePatternThick = 0.5, interior = FALSE,...)
		#addLocPoints()
        mtext.units(name, line = -1 + lineMod * 0.5, adj = 0.1)
		
		if (length(lim) > 1 && !is.na(lim)) {
			standard_legend(col, lim, x, add = TRUE, plot_loc = c(0.37, 0.88, 0.03, 0.06), ylabposScling = 1.25, oneSideLabels = FALSE)
			mtext.units(unit, side = 1, line = -3 + lineMod, adj = 0.75, cex = 0.85)
		}
        browser()
    }
    
    nplts = length(lims)
    nrows = ceiling(sqrt(length(lims)))
	nplts = ceiling(nplts/nrows) * nrows
    
    png(fname, height = 0.8 * 2.2 * nrows, width = 0.8 * 4.5 * ceiling(nplts/nrows), 
	    res = 300, units = 'in')
		
					 
		if (is.null(lmat)) lmat = (matrix(1:nplts, nrow = nrows))
        layout(lmat)#, height = rep(1, 3))

        par(mar = rep(0.5, 4))
        mapply(plot_input, Obs, lims, cols, names, units)
    dev.off()#.gitWatermark()
}

## Plot Individuals
ready4Plot <- function(r, maxBare = 100) {
	r[['bare']][[1]] = maxBare - r[['bare']][[1]]
	r[['tree']][is.na(r[['emc']])] = NaN
	r[['bare']][is.na(r[['emc']])] = NaN
	r[['fire']][[1]] = r[['fire']][[1]] * 100
	return(r)
}

Obs_mean = ready4Plot(Obs_mean)
Obs_fire = ready4Plot(Obs_fire)
#Obs_trnd = ready4Plot(Obs_trnd, 0)
Obs_mean[['fire']] = Obs_mean[['fire']] * 12

#Obs_trnd = lapply(Obs_trnd, function(i) {i[[1]] = i[[1]] * 12; i})

lmat = rbind(c(1, 2, 0),
			 3:5,
			 c(6, 7, 0),
			 8:10)
			 
plot_inputs(Obs_mean, fignames[1], lmat = lmat)
plot_inputs(Obs_fire, fignames[2], lmat = lmat)
browser()
#plot_inputs(Obs_trnd, fignames[3], lmat = lmat, cols = colt_input, lims = limt_input)


## Plot measures
measures_mean[[1]] = measures_mean[[1]] * 100/0.8
measures_mean[[2]] = measures_mean[[2]] * 100
measures_mean[[3]] = measures_mean[[3]] * 12

measures_fire[[1]] = measures_fire[[1]] * 100/0.8
measures_fire[[2]] = measures_fire[[2]] * 100
 
plot_inputs(measures_mean, fignames[4], names_msure, units_msure, lims_msure, cols_msure, lineMod = 0.5) 
lims_msure[[3]] = lims_msure[[3]] / 10   
plot_inputs(measures_fire, fignames[5], names_msure, units_msure, lims_msure, cols_msure, lineMod = 0.5) 

           

#########################################################################
## Plot cross correlarion                                              ##
#########################################################################
 den_dims = c(10, 10)
 
plot_density <- function(i, j) {
    x = lims[[i]]
    y = lims[[j]]
    
    fname = paste(c('temp/', names[c(i,j)], 'density.csv'), collapse = '-')  
    
    find_den <- function() {
        X0 = Obs[[i]]
        Y0 = Obs[[j]]
               
        den = matrix(0, length(x) + 1, length(y) + 1)
        
        for (layer in 1:nlayers(X0)) {
            print(layer)
            X = cut_results(X0[[layer]], x)
            Y = cut_results(Y0[[layer]], y)
            
            msk = !is.na(X) & !is.na(Y)
            X = X[msk]
            Y = Y[msk]
            for (k in 1:length(x)) for (l in 1:length(y))
                den[k, l] = den[k, l] + sum(X == k & Y == l)
        }
        return(den)
    }
    den = runIfNoFile(fname, find_den, test = grab_cache)
    
    image(as.matrix(den), axes = FALSE)
    add_axis <- function(v, side) {
        d = 0.5/(length(v)-1)
        at = seq(d, 1 - d, length.out = length(v))
        axis(at = at, labels = v, side = side)
    }
    add_axis(x, 1)
    add_axis(y, 2)
    title = paste(names[c(i,j)], collapse = ' vs. ')
    mtext(title)
}

nplots = length(Obs) - 1 # -1 to remove fire

pdf(fignames[6], height = 3 * nplots, width = 3 * nplots)
    par(mfrow = c(nplots, nplots), mar = c(1.5, 2.5, 2.5,1.5))
    lapply(1:nplots, function(i) lapply(1:nplots, plot_density, i))
dev.off.gitWatermark()

