source("cfg.r")
graphics.off()
obs_file  = 'outputs/fire2000-2014.nc'

temp_file = 'temp/benchmarking_plot_dat.Rd'
grab_cache = FALSE

fire_lims = c(1, 2, 5, 10, 20, 50)
dfire_lims = c(-0.01, -0.005, -0.002, -0.001, 0.001, 0.002, 0.005, 0.01) /10


if (file.exists(temp_file) && grab_cache) {
    load(temp_file) 
}else {
    obs = brick(obs_file)

    aa_mod = ens_annual_average()
    aa_obs = 12 * 100 * mean(obs)

    tr_mod = trend12FF[[1]] / 100

    tfile = paste(outputs_dir, filename.noPath(obs_file, TRUE), '-trendDiff.nc', sep = '')
    tr_obs = runIfNoFile(tfile, removeTrend, obs)
    tr_obs = tr_obs[[1]]/(aa_obs * nlayers(obs)/(100*12))
    tr_obs[is.infinite(tr_obs)] = NaN

    tr_mod_mn = mean(tr_mod)
    tr_mod_sd = sd.raster(tr_mod)

    mask = is.na(tr_obs + tr_mod_mn) | (aa_obs < 0.1)
    tr_obs   [mask] = NaN
    tr_mod_mn[mask] = NaN
    tr_mod_sd[mask] = NaN
    save(aa_mod, aa_obs, tr_mod, tr_obs, tr_mod_mn, tr_mod_sd, file = temp_file)
}

tr_obs = tr_obs/14
tr_mod_mn = tr_mod_mn * 168/14


    
plotMap <- function(...)
	plotStandardMap(..., ePatternRes = 40, ePatternThick = 0.4,
						limits_error = c(1/10, 1/2), ylabposScling=1.5, oneSideLabels = FALSE)

png('figs/benchmarkFigure.png', height = 4.7 * 7.2/10, width = 7.2, units = 'in', res = 300)
	par(mfcol = c(2,2), mar = rep(0,4), oma = c(1.05, 0, 1, 0))
    
	plotMap(aa_obs, '', limits =  fire_lims, cols =  fire_cols, 
				   add_legend = FALSE)
	mtext('GFED4s\nObserved', side = 2, line = -2.3, adj = 0.1)					
	mtext('Annual Average Burnt Area', side = 3)	
    mtext('a)', adj = 0.1, side = 3, line = -1)
						
	plotMap(mean(aa_mod), '', limits =  fire_lims, cols =  fire_cols, 
						e = sd.raster(aa_mod), maxLab = 100, units = '%', labelss = c(0, fire_lims),
                        plot_loc = c(0.35, 0.93 ,0.01, 0.04))
						
	mtext('Reconstructed', side = 2, line = -2.3, adj = 0.1)
    mtext('c)', adj = 0.1, side = 3, line = -1)					
						
	plotMap(100 * tr_obs/nlayers(obs), '', 
					limits =  dfire_lims * 100, cols =  dfire_cols, add_legend = FALSE)					
	mtext('Trend in Burnt Area', side = 3)	
    mtext('b)', adj = 0.1, side = 3, line = -1)
    
	plotMap(tr_mod_mn, '', limits = dfire_lims * 100, cols = dfire_cols,
					e = tr_mod_sd, extend_min = TRUE, extend_max = TRUE, units = '% ~yr-1~',
                    plot_loc = c(0.35, 0.93, 0.01, 0.04))
    mtext('d)', adj = 0.1, side = 3, line = -1)
dev.off()