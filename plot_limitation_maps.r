#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

fig_fname       = 'figs/limitation_map'
fig_fname_indiv = 'figs/ind_limiataions'

ens_dir = 'D:/Laurens22122018/Documents/work/LimFIRE/outputs/'
ens_dir = 'outputs/'


labs = c('Standard\nlimitation', 'Potential\nlimitation', '\nSensitivity',
         '', '', '')

ens_tfile = 'temp/limitation_maps_ens4'
mod_files = paste(temp_dir, '/LimFIRE_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  sep = '')

niterations = 50
#########################################################################
## Run model                                                           ##
#########################################################################


findParameterLimitation <- function(dir) {
	#mod_files = paste(mod_files, '-paramLine', line, sep = "")
	#rw_mod_files = paste(mod_files,    '-rw', sep = '')
	#lm_mod_files = paste(mod_files,    '-lm', sep = '')
	#sn_mod_files0 = paste(mod_files,    '-sn', sep = '')
	#sn_mod_files = paste(mod_files,    '-sn-ws', sep = '')
    
	mod = open_ensemble(dir)
	lm_mod = lapply(mod[[1]], brick)
	rw_mod = lapply(mod[[2]], brick)
	sn_mod = lapply(mod[[4]], brick)
	#lm_mod = runLimFIRE(lm_mod_files, normalise = TRUE, add21 = TRUE)
	#rw_mod = runLimFIRE(rw_mod_files, raw = TRUE, normalise = TRUE)
	#sn_mod = runLimFIRE(sn_mod_files0, sensitivity = TRUE)

	weightedSensitivity <- function() {
		ws <- function(sn, i) {
			lms = rw_mod[-1]
			lms = lms[-i]
			out = layer.apply(1:nlayers(sn), function(i) sn[[i]] * lms[[1]][[i]] * lms[[2]][[i]] * lms[[3]][[i]])
			return(out)
		}
		sn_mod[2:5] = mapply(ws, sn_mod[2:5], 1:4)
		return(sn_mod)
	}

	#sn_mod = runIfNoFile(paste(sn_mod_files, '.nc', sep = ''), weightedSensitivity, test = grab_cache)

	#########################################################################
	## Annual Average                                                      ##
	#########################################################################
	cal_annual_average <- function(r) {
		fname = paste(dirname(filename(r)), '/', filename.noPath(r, TRUE), '-mean.nc', sep = '')
		print(fname)
		r = runIfNoFile(fname, mean, r, test = grab_cache)
		
		#xx_mod = runIfNoFile(fname, function(x) lapply(x, mean), xx_mod, test = grab_cache)
		#xx_mod[[2]][is.na(xx_mod[[2]])] = 100
		return(r)
	}

	aa_rw_mod = lapply(rw_mod, cal_annual_average)
	aa_lm_mod = lapply(lm_mod, cal_annual_average)
	aa_sn_mod = lapply(sn_mod, cal_annual_average)

	#########################################################################
	## Fire Season                                                         ##
	#########################################################################

	which.maxMonth <- function(r) {   
		fname = paste(dirname(filename(r)), '/', filename.noPath(r, TRUE), '-which.maxMonth.nc', sep = '')
		print(fname)
		
		maxMonth <- function() {
			nyears = nlayers(r) / 12
			
			forYear <- function(yr) {
				index = ((yr - 1) * 12 + 1):(yr * 12)
				y = r[[index]]
				y = which.max(y)
				return(y)
			}
			
			return(layer.apply(1:nyears, forYear))
		}
		out = runIfNoFile(fname, maxMonth, test = grab_cache)
	}
	
	maxMonth = which.maxMonth(lm_mod[[1]])

	maxFireLimiation <- function(x) {
		nyears = nlayers(x) / 12
		out = x[[1]]
		out[] = NaN
		z = values(out)
		forYear <- function(yr) {
			index = ((yr - 1) * 12 + 1):(yr * 12)
			y = values(x[[index]])
			
			mnths = values(maxMonth[[yr]])
			
			for (i in 1:length(mnths))
				if (is.na(mnths[i])) z[i] = mean(y[i,])
					else z[i] = y[i, mnths[i]]
			
			out[] = z
			return(out)
		}
		out = layer.apply(1:nyears, forYear)
		out = mean(out)
		return(out)
	}

	cal_fire_season_average <- function(r) {
		fname = paste(dirname(filename(r)), '/', filename.noPath(r, TRUE), '-fs.nc', sep = '')
		print(fname)
		r = runIfNoFile(fname, maxFireLimiation, r, test = grab_cache)
		return(r)
	}

	fs_rw_mod = lapply(rw_mod, cal_fire_season_average)
	fs_lm_mod = lapply(lm_mod, cal_fire_season_average)
	fs_sn_mod = lapply(sn_mod, cal_fire_season_average)
	
	aa_rw_mod = lapply(aa_rw_mod, function(i) 1 - i) 
	fs_rw_mod = lapply(fs_rw_mod, function(i) 1 - i)
	
	return(list(aa_rw_mod, aa_lm_mod, aa_sn_mod, fs_rw_mod, fs_lm_mod, fs_sn_mod))
}

dirs = list.dirs(ens_dir)
dirs = dirs[grepl('ensemble_', dirs)]
dirs = dirs[!grepl('noVar', dirs)]

ens_tfile = paste(ens_tfile, niterations, '.Rd', sep = '-')
if (file.exists(ens_tfile)) load(ens_tfile) else {
	ensamble = lapply(dirs[1:niterations], findParameterLimitation)
	ensambleSum =  lapply(1:length(ensamble[[1]]),
					      function(i) extractEnsamble(ensamble, i, mean90quant.ens, quantiles = c(0.215, 0.785)))
	save(ensamble, ensambleSum,  file = ens_tfile)
}
#niterations = 21
#ensamble = lapply(seq(0, 1, length.out = niterations), findParameterLimitation)

#########################################################################
## Plotting and tableing                                               ##
###############x##########################################################
#plot_limitations_1by1 <- function(pmod, fname) {
#    mask = sum(layer.apply(pmod[-1], is.na)) > 0
#    lim = seq(0, 0.9, by = 0.1)
#    col = c('white', 'black')
#        
#    plot_limitations_1by1 <- function(mod, name) {
#        mod[mask] = NaN
#        plot_raster(mod, lim, col, quick = TRUE)
#        mtext(name, line = -1.5)
#    }
#    fname = paste(fig_fname_indiv, fname, '.pdf', sep = '')
#    pdf(fname, height = 5.25, width = 9)
#        mat = rbind(c(1, 2), c(3, 4), c(5, 5))
#        layout(mat, height = c(1, 1, 0.33333))
#        par(mar = rep(0, 4))
#        mapply(plot_limitations_1by1, pmod[-1], c('fuel', 'moisture', 'igntions', 'suppression'))
#        standard_legend(col, lim, pmod[[2]], plot_loc = c(0.2,0.8,0.65,0.78))
#    dev.off.gitWatermark()
#}

#plot_limitations_1by1(aa_lm_mod, 'aa')
#plot_limitations_1by1(fs_lm_mod, 'fs')


## function for calculating pcs for table
calculate_weightedAverage <- function(xy, pmod) {
    pmod = layer.apply(pmod, function(i) rasterFromXYZ(cbind(xy, i)))
    pmod = pmod / sum(pmod)
    pmod = layer.apply(pmod, function(i) sum.raster(i * raster::area(i), na.rm = TRUE))
    pmod = unlist(pmod)
    pmod = round(100 * pmod / sum(pmod))
    
}

## Plot limitation or sesativity, and outputting pcs 
plot_pmod <- function(i, index = NULL, normalise = FALSE, controls = rep(T, 4),
                      plotFire = FALSE, ...) {
    pmods = ensambleSum[[i]]
    lab = labs[i]
    let = letters[i]
    let = paste(let, ')', sep = '')
    pmods = pmods[-1] # remove first element of simulated fire
    #pmod = mapply(function(pm, FUN) FUN(pm), pmods, FUNs)
    pmod = pmod0 = lapply(pmods, function(i) i[[1]])
    if (!is.null(index)) {
        #pmod = mapply(function(p, pm, i) pm[[i]] - p, pmod, pmods, index)
	pmod = mapply(function(pm, i) pm[[i]], pmods, index)
	normalise = normalise
	limits = c(0.001, 0.01, 0.1)
	cols = c("FF", "CC", "99", "55", "11")
    }
	
    normalise = TRUE
    limits = c(0.1, 0.5, 0.9)
    cols = rev(c("FF", "CC", "99", "55", "11"))
    
    xy = xyFromCell(pmod[[1]], 1:length(pmod[[1]]))
    pmod = lapply(pmod, values)
    pmod[!controls] = lapply(pmod[!controls], function(i) rep(0, length(i)))
     
    if (plotFire) {
        r = pmod0[controls]
        r = prod(layer.apply(r, function(i) 1-i))
        
        plot_raster_from_raster(r*100, cols = fire_cols, 
                                limits = fire_lims, 
                                x_range = c(-180, 180), y_range = c(-56, 84),
                                quick = TRUE, coast.lwd = NULL, add_legend = FALSE)
        addCoastlineAndIce2map()
    } else  if (sum(controls) == 1) {
        r = pmod0[controls][[1]]
        plot_raster_from_raster(r, cols = c("white", "green"), 
                                limits = c(0.2, 0.4, 0.6, 0.8), 
                                x_range = c(-180, 180), y_range = c(-56, 84),
                                quick = TRUE, coast.lwd = NULL, add_legend = FALSE)
        addCoastlineAndIce2map()
        #e = e, limits_error = 0.5 + 1:length(limits),  
        #ePatternRes = ePatternRes,  ePatternThick = ePatternThick, e_polygon = FALSE,
        
        #plotstandardmap(r, limits = c(0.2, 0.4, 0.6, 0.8),
        #                cols = c("white", "green"), txt = '')
    } else {
        plot_4way(xy[,1], xy[,2], pmod[[2]], pmod[[1]], pmod[[3]], pmod[[4]],
              x_range = c(-180, 180), y_range = c(-60, 90),
              cols = 	cols, limits = limits, 
              ePatternRes = 50, ePatternThick = 0.4,
              add_legend=FALSE, smooth_image=FALSE,smooth_factor=5, normalise = normalise, ...)
    }
    mtext(let, side = 1, adj  = 0.2, line = -2)
    addLocPoints(controls = controls[c(1, 3, 2, 4)])    
    pcs = calculate_weightedAverage(xy, pmod)
	
	polygon(c(-180, -140, -140, -180), c(-60, -60, 30, 30), col = 'white', border = NA)
    #text(lab, x = -160, y = 0, cex = 1.5, srt = 90)
    mtext(lab, line = -3.5, adj = 0.05, side = 2)
    return(pcs)
}


plotAddLimTypes <- function(fname, ...) {
## Set up plotting window
	figName = paste(fig_fname, fname, '.png')	
	png(figName, width = 7.2, height = 6 * 4/3 * 7.2/9, unit = 'in', res = 600)
	layout(rbind(cbind(1:3,4:6),7))#, heights = c(4.5, 4.5, 1))

	par(mar = c(0,0,0,0), oma = c(0,0,1.5,0))

	## Plot and put pcs in table
	pc_out = sapply(1:6, plot_pmod, ...)
	mtext('Annual average', side = 3, outer = TRUE, adj = 0.22, bg = 'white')
	mtext('Fire season'   , side = 3, outer = TRUE, adj = 0.80, bg = 'white')

	colnames(pc_out) = c('annual average raw', 'annual average lim', 'annual average sensitivity', 
			            'fire season raw',    'fire season lim',    'fire season sensitivity')
	rownames(pc_out) = c('Fuel Discontinuity', 'Moisture', 'Ignitions', 'Land use')


	## Add legend
	par(mar = c(3, 10, 0, 8))
	add_raster_4way_legend(cols = rev(c("FF","CC","99","55","11")),
						   labs = c('<- Moisture', 'Fuel ->', 'Igntions ->', 'Land Use'))

	## add footer
	par(fig = c(0, 1, 0, 1), mar = rep(0, 4))
	points(0.5, 0.5, col = 'white', cex = 0.05)
	dev.off()
}

#maxLim <- function(i) i[[1]] + i[[2]]
#minLim <- function(i) i[[1]] - i[[2]]

#plotAddLimTypes('fuel', NULL, controls = c(T, F, F, F))
#plotAddLimTypes('fuel-moisture', NULL, controls = c(T, F, T, F))
#plotAddLimTypes('fuel-moisture-ignitions', NULL, controls = c(T, T, T, F))
#plotAddLimTypes('', NULL)

plotAddLimTypes('fire-fuel', NULL, controls = c(T, F, F, F), plotFire = TRUE)
plotAddLimTypes('fire-fuel-moisture', NULL, controls = c(T, F, T, F), plotFire = TRUE)
plotAddLimTypes('fire-fuel-moisture-ignitions', NULL, controls = c(T, T, T, F), plotFire = TRUE)
plotAddLimTypes('fire-', NULL, plotFire = TRUE)

#plotAddLimTypes('maxFuel', c(3, rep(2, 3)))
#plotAddLimTypes('minFuel', c(2, rep(3, 3)))

#plotAddLimTypes('maxIgni', c(2, 3, 2, 2))
#plotAddLimTypes('minIgni', c(3, 2, 3, 3))

#plotAddLimTypes('maxMist', c(2, 2, 3, 2))
#plotAddLimTypes('minMist', c(3, 3, 2, 3))

#plotAddLimTypes('maxSupp', c(2, 2, 2, 3))
#plotAddLimTypes('minSupp', c(3, 3, 3, 2))
