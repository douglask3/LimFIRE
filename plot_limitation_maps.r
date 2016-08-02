source('cfg.r')
graphics.off()

fig_fname = 'figs/limitation_map.png'

mod_files = paste(outputs_dir, '/LimFIRE_test_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  sep = '')
                 
aa_mod_files = paste(mod_files, '-aa.nc', sep = '')
fs_mod_files = paste(mod_files, '-fs.nc', sep = '')
   mod_files = paste(mod_files,    '.nc', sep = '')

mod = runIfNoFile(mod_files, runLimFIREfromstandardIns)

#########################################################################
## Annual Average                                                      ##
#########################################################################

aa_mod = runIfNoFile(aa_mod_files, function(x) lapply(x, mean), mod)
aa_mod[[2]][is.na(aa_mod[[2]])] = 1


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

fs_mod = runIfNoFile(fs_mod_files, function(x) lapply(x, maxFireLimiation), mod)
fs_mod[[2]][is.na(fs_mod[[2]])] = 1


#########################################################################
## Plotting                                                            ##
#########################################################################

## Set up plotting window
png(fig_fname, width = 9, height = 6 * 2.75/3, unit = 'in', res = 300)
layout(rbind(1:2,3:4, 5, 5), heights = c(4, 4, 1))

par(mar = c(0,0,0,0), oma = c(0,0,1,0))


## Plot limitation and sesativity
plot_4way_standard <- function(xy, pmod) {
    plot_4way(xy[,1], xy[,2], pmod[[3]] / 1.5, pmod[[1]], pmod[[2]], pmod[[4]],
              x_range=c(-180,180),y_range=c(-60,90),
              cols=rev(c("FF","CC","99","55","11")),
              coast.lwd=par("lwd"),
              add_legend=FALSE, smooth_image=FALSE,smooth_factor=5)

}

calculate_weightedAverage <- function(xy, pmod) {
    pmod[[3]] = pmod[[3]]/4
    pmod = layer.apply(pmod, function(i) rasterFromXYZ(cbind(xy, i)))
    pmod = pmod / sum(pmod)
    pmod = layer.apply(pmod, function(i) sum.raster(i * area(i), na.rm = TRUE))
    pmod = unlist(pmod)
    pmod = round(100 * pmod / sum(pmod))
    
}

## Plot limitation and sesativity
plot_limtations_and_sensativity_plots <- function(pmod, labs) {
    xy = xyFromCell(pmod[[1]], 1:length(pmod[[1]]))
    pmod = lapply(pmod[-1], values)
    
    plot_4way_standard(xy, pmod)
    pcs = calculate_weightedAverage(xy, pmod)
    mtext(labs[1], line = -1, adj = 0.05)
    
    convert2sensativity <- function(x) 1 - 2*abs(x - 0.5)
    pmod = lapply(pmod, convert2sensativity)
    
    plot_4way_standard(xy, pmod)
    pcs = rbind(pcs, calculate_weightedAverage(xy, pmod))
    mtext(labs[2], line = -1, adj = 0.05)
    return(pcs)
}


labs = c('a) Annual average controls on fire', 'b) Annual average sensitivity',
         'c) Controls on fire during the fire season', 'd) Sensitivity during the fire season')
pc_aa = plot_limtations_and_sensativity_plots(aa_mod, labs[1:2])
pc_fs = plot_limtations_and_sensativity_plots(fs_mod, labs[3:4])


## Add legend
par(mar = c(3, 10, 0, 8))
add_raster_4way_legend(cols = rev(c("FF","CC","99","55","11")),
                       labs = c('<- Moisture', 'Fuel ->', 'Igntions ->', 'Land Use'))

## add footer
par(fig = c(0, 1, 0, 1), mar = rep(0, 4))
points(0.5, 0.5, col = 'white', cex = 0.05)
dev.off.gitWatermark()
