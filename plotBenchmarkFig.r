source("cfg.r")


obs_file  = 'outputs/fire2000-2014.nc'



fire_lims = c(0,1, 2, 5, 10, 20, 50)
dfire_lims = c(-0.01, -0.005, -0.002, -0.001, 0.001, 0.002, 0.005, 0.01)

obs = brick(obs_file)

aa_mod = ens_annual_average()
aa_obs = 12 * 100 * mean(mod)

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

plotMap <- function(...)
	plotStandardMap(..., ePatternRes = 30, ePatternThick = 0.2, 
						limits_error = c(1/10, 1/2), ylabposScling=1.5, oneSideLabels = NA)

png('figs/benchmarkFigure.png', height = 4.7, width = 10, units = 'in', res = 300)
	par(mfcol = c(2,2), mar = rep(0,4))
	plotMap(aa_obs, '', limits =  fire_lims, cols =  fire_cols, 
				   add_legend = FALSE)
	mtext('GFED4s Observed', side = 2, line = -2, adj = 0.1)					
	mtext('Annual Average Burnt Area', side = 3, line = -1.3)					
						
	plotMap(mean(aa_mod), '', limits =  fire_lims, cols =  fire_cols, 
						e = sd.raster(aa_mod))
						
	mtext('Reconstructed', side = 2, line = -2, adj = 0.1)					
						
	plotMap(100 * tr_obs/nlayers(obs), '', 
					limits =  dfire_lims * 100, cols =  dfire_cols, add_legend = FALSE)
						
	mtext('Trend in Burnt Area', side = 3, line = -1.3)	
	
	plotMap(tr_mod_mn, '', limits = dfire_lims * 100, cols = dfire_cols,
					e = tr_mod_sd)
dev.off()