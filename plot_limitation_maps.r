source('cfg.r')
if (!exists('AGUplot')) graphics.off()

fig_fname = 'figs/limitation_map.png'

mod_files = paste(outputs_dir, '/LimFIRE_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  sep = '')


   lm_mod_files = paste(mod_files,    '-lm', sep = '')
   sn_mod_files = paste(mod_files,    '-sn', sep = '')
                  
aa_lm_mod_files = paste(lm_mod_files, '-aa.nc', sep = '')
aa_sn_mod_files = paste(sn_mod_files, '-aa.nc', sep = '')
fs_lm_mod_files = paste(lm_mod_files, '-fs.nc', sep = '')
fs_sn_mod_files = paste(sn_mod_files, '-fs.nc', sep = '')

   lm_mod_files = paste(lm_mod_files,    '.nc', sep = '')
   sn_mod_files = paste(sn_mod_files,    '.nc', sep = '')

lm_mod = runIfNoFile(lm_mod_files, runLimFIREfromstandardIns)
sn_mod = runIfNoFile(sn_mod_files, runLimFIREfromstandardIns, sensitivity = TRUE)

#########################################################################
## Annual Average                                                      ##
#########################################################################

aa_lm_mod = runIfNoFile(aa_lm_mod_files, function(x) lapply(x, mean), lm_mod)
aa_sn_mod = runIfNoFile(aa_sn_mod_files, function(x) lapply(x, mean), sn_mod)
aa_lm_mod[[2]][is.na(aa_lm_mod[[2]])] = 100
aa_sn_mod[[2]][is.na(aa_sn_mod[[2]])] = 100


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

maxMonth = runIfNoFile('temp/maxMonth.nc', which.maxMonth, mod[[1]])

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

fs_lm_mod = runIfNoFile(fs_lm_mod_files, function(x) lapply(x, maxFireLimiation), lm_mod)
fs_sn_mod = runIfNoFile(fs_sn_mod_files, function(x) lapply(x, maxFireLimiation), sn_mod)
fs_lm_mod[[2]][is.na(fs_lm_mod[[2]])] = 1


#########################################################################
## Plotting                                                            ##
#########################################################################

## Set up plotting window
 if (!exists('AGUplot')) {
    png(fig_fname, width = 9, height = 6 * 2.75/3, unit = 'in', res = 300)
    layout(rbind(1:2,3:4, 5, 5), heights = c(4, 4, 1))
}

par(mar = c(0,0,0,0), oma = c(0,0,1,0))

plot_4way_standard <- function(xy, pmod) {
    plot_4way(xy[,1], xy[,2], pmod[[3]], pmod[[1]], pmod[[2]], pmod[[4]],
              x_range=c(-180,180),y_range=c(-60,90),
              cols=rev(c("FF","CC","99","55","11")),
              coast.lwd=par("lwd"),
              add_legend=FALSE, smooth_image=FALSE,smooth_factor=5)

}

calculate_weightedAverage <- function(xy, pmod) {
    #pmod[[3]] = pmod[[3]]/4
    pmod = layer.apply(pmod, function(i) rasterFromXYZ(cbind(xy, i)))
    pmod = pmod / sum(pmod)
    pmod = layer.apply(pmod, function(i) sum.raster(i * area(i), na.rm = TRUE))
    pmod = unlist(pmod)
    pmod = round(100 * pmod / sum(pmod))
    
}

## Plot limitation and sesativity

plot_limtations_and_sensativity_plots <- function(lm_pmod, sn_pmod, labs, plot = rep(TRUE, 2)) {
    
    plot_pmod <- function(pmod, lab, plot) {
        xy = xyFromCell(pmod[[1]], 1:length(pmod[[1]]))
        pmod = lapply(pmod, values)
    
        if (plot) plot_4way_standard(xy, pmod)
        pcs = calculate_weightedAverage(xy, pmod)
        if (plot) mtext(lab, line = -1)
        return(pcs)
    }
    lm_pmod = lm_pmod[-1]
    lm_pmod[[3]] = lm_pmod[[3]] / 3
    lm_pmod[[4]] = lm_pmod[[4]] * 0.6
    pcs = plot_pmod(lm_pmod, labs[1], plot[1])
    
    sn_pmod = sn_pmod[-1]
    
    
    #sn2snFire <- function(i) {
    #    index = index[-i]
    #    l = sn_pmod[[i]] 
    #    for (j in lm_pmod[index]) l = l * j
    #    return(l)
    #}
    #index = 1:length(sn_pmod)
    #sn_pmod = lapply(index, sn2snFire)
    
    pcs = rbind(pcs, plot_pmod(sn_pmod, labs[2], plot[2]))

    return(pcs)
}
       

if (!exists('AGUplot')) {
    labs = c('a) Annual average controls on fire', 'b) Annual average sensitivity',
         'c) Controls on fire during the fire season', 'd) Sensitivity during the fire season')
    plot = rep(TRUE, 2)
} else plot = c(FALSE, FALSE, TRUE, FALSE)


pc_aa = plot_limtations_and_sensativity_plots(aa_lm_mod, aa_sn_mod, labs[1:2], plot = plot[1:2])
pc_fs = plot_limtations_and_sensativity_plots(fs_lm_mod, fs_sn_mod, labs[3:4], plot = plot[3:4])



## Add legend
par(mar = c(3 + exists('AGUplot') * 0, 10 - 8 * exists('AGUplot'), 0, 8 - 6 * exists('AGUplot')))
add_raster_4way_legend(cols = rev(c("FF","CC","99","55","11")),
                       labs = c('<- Moisture', 'Fuel ->', 'Igntions ->', 'Land Use'))

## add footer

if (!exists('AGUplot')) {
    par(fig = c(0, 1, 0, 1), mar = rep(0, 4))
    points(0.5, 0.5, col = 'white', cex = 0.05)
    dev.off.gitWatermark()
}