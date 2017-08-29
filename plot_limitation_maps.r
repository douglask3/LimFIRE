#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = FALSE

fig_fname       = 'figs/limitation_map.png'
fig_fname_indiv = 'figs/ind_limiataions'


labs = c('a) Annual average controls on fire', 'b) Annual average sensitivity',
         'c) Controls on fire during the fire season', 'd) Sensitivity during the fire season')


mod_files = paste(outputs_dir, '/LimFIRE_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  sep = '')

#########################################################################
## Run model                                                           ##
#########################################################################

lm_mod_files = paste(mod_files,    '-lm', sep = '')
sn_mod_files = paste(mod_files,    '-sn', sep = '')
                  

runLimFIRE <- function(fname, ...){
    fname = paste(fname,    '.nc', sep = '')
    return(runIfNoFile(fname, runLimFIREfromstandardIns, test = grab_cache, ...))
}


lm_mod = runLimFIRE(lm_mod_files, normalise = TRUE)
sn_mod = runLimFIRE(sn_mod_files, sensitivity = TRUE)

#########################################################################
## Annual Average                                                      ##
#########################################################################
cal_annual_average <- function(fname, xx_mod) {
    fname = paste(fname, '-aa.nc', sep = '')
    xx_mod = runIfNoFile(fname, function(x) lapply(x, mean), xx_mod, test = grab_cache)
    xx_mod[[2]][is.na(xx_mod[[2]])] = 100
    return(xx_mod)
}

aa_lm_mod = cal_annual_average(lm_mod_files, lm_mod)
aa_sn_mod = cal_annual_average(sn_mod_files, sn_mod)

#########################################################################
## Fire Season                                                         ##
#########################################################################

which.maxMonth <- function(x) {    
    nyears = nlayers(x) / 12
    
    forYear <- function(yr) {
        index = ((yr - 1) * 12 + 1):(yr * 12)
        y = x[[index]]
        y = which.max(y)
        return(y)
    }
    
    return(layer.apply(1:nyears, forYear))
}

maxMonth = runIfNoFile('temp/maxMonth.nc', which.maxMonth, lm_mod[[1]], test = grab_cache)

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

cal_fire_season_average <- function(fname, xx_mod) {
    fname = paste(fname, '-fs.nc', sep = '')
    xx_mod = runIfNoFile(fname, function(x) lapply(x, maxFireLimiation), xx_mod, test = grab_cache)
    xx_mod[[2]][is.na(xx_mod[[2]])] = 100
    return(xx_mod)
}

fs_lm_mod = cal_fire_season_average(lm_mod_files, lm_mod)
fs_sn_mod = cal_fire_season_average(sn_mod_files, sn_mod)

#########################################################################
## Plotting and tableing                                               ##
#########################################################################
plot_limitations_1by1 <- function(pmod, fname) {
    mask = sum(layer.apply(pmod[-1], is.na)) > 0
    lim = seq(0, 0.9, by = 0.1)
    col = c('white', 'black')
        
    plot_limitations_1by1 <- function(mod, name) {
        mod[mask] = NaN
        plot_raster(mod, lim, col, quick = TRUE)
        mtext(name, line = -1.5)
    }
    fname = paste(fig_fname_indiv, fname, '.pdf', sep = '')
    pdf(fname, height = 5.25, width = 9)
        mat = rbind(c(1, 2), c(3, 4), c(5, 5))
        layout(mat, height = c(1, 1, 0.33333))
        par(mar = rep(0, 4))
        mapply(plot_limitations_1by1, pmod[-1], c('fuel', 'moisture', 'igntions', 'suppression'))
        standard_legend(col, lim, pmod[[2]], plot_loc = c(0.2,0.8,0.65,0.78))
    dev.off.gitWatermark()
}

plot_limitations_1by1(aa_lm_mod, 'aa')
plot_limitations_1by1(fs_lm_mod, 'fs')


## function for calculating pcs for table
calculate_weightedAverage <- function(xy, pmod) {
    pmod = layer.apply(pmod, function(i) rasterFromXYZ(cbind(xy, i)))
    pmod = pmod / sum(pmod)
    pmod = layer.apply(pmod, function(i) sum.raster(i * raster::area(i), na.rm = TRUE))
    pmod = unlist(pmod)
    pmod = round(100 * pmod / sum(pmod))
    
}

## Plot limitation or sesativity, and outputting pcs 
plot_pmod <- function(pmod, lab) {
    
    pmod = pmod[-1] # remove first element of simulated fire
    xy = xyFromCell(pmod[[1]], 1:length(pmod[[1]]))
    pmod = lapply(pmod, values)

    plot_4way(xy[,1], xy[,2], pmod[[3]], pmod[[1]], pmod[[2]], pmod[[4]],
              x_range = c(-180, 180), y_range = c(-60, 90),
              cols=rev(c("FF","CC","99","55","11")),
              coast.lwd=par("lwd"),
              add_legend=FALSE, smooth_image=FALSE,smooth_factor=5)
              
    pcs = calculate_weightedAverage(xy, pmod)
    mtext(lab, line = -1, adj = 0.05)
    return(pcs)
}


## Set up plotting window
png(fig_fname, width = 9, height = 6 * 2.75/3, unit = 'in', res = 300)
layout(rbind(1:2,3:4, 5, 5), heights = c(4, 4, 1))

par(mar = c(0,0,0,0), oma = c(0,0,1,0))


## Plot and put pcs in table
pc_out = rbind(
            'annual average limitation'  = plot_pmod(aa_lm_mod, labs[1]), 
            'annual average sensitivity' = plot_pmod(aa_sn_mod, labs[2]),
            'fire season limitation'     = plot_pmod(fs_lm_mod, labs[3]),
            'fire season sensitivity'    = plot_pmod(fs_sn_mod, labs[4]))

colnames(pc_out) = c('Fuel Discontinuity', 'Moisture', 'Ignitions', 'Land use')


## Add legend
par(mar = c(3, 10, 0, 8))
add_raster_4way_legend(cols = rev(c("FF","CC","99","55","11")),
                       labs = c('<- Moisture', 'Fuel ->', 'Igntions ->', 'Land Use'))

## add footer
par(fig = c(0, 1, 0, 1), mar = rep(0, 4))
points(0.5, 0.5, col = 'white', cex = 0.05)
dev.off.gitWatermark()
