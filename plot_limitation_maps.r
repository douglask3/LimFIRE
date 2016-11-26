source('cfg.r')
graphics.off()

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
test = is.na(aa_lm_mod[[2]]) & !is.na(aa_lm_mod[[3]])

set0 <- function(i) {i[test] = 0; i}
aa_lm_mod = lapply(aa_lm_mod, set0)
aa_sn_mod = lapply(aa_sn_mod, set0)
aa_lm_mod[[2]][test] = 1
aa_sn_mod[[2]][test] = 1


#########################################################################
## Fire Season                                                         ##
#########################################################################

which.maxMonth <- function(x) {
    
    nyears = nlayers(x) / 12
    
    forYear <- function(yr) {
        print(yr)
        index = ((yr - 1) * 12 + 1):(yr * 12)
        y = x[[index]]
        y = which.max(y)
        return(y)
    }
    
    return(layer.apply(1:nyears, forYear))
}

maxMonth = runIfNoFile('temp/LimFIRE_maxMonth.nc', which.maxMonth, lm_mod[[1]])

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
fs_lm_mod = lapply(fs_lm_mod, set0)
fs_sn_mod = lapply(fs_sn_mod, set0)
fs_lm_mod[[2]][test] = 1
fs_sn_mod[[2]][test] = 1

#########################################################################
## Plotting                                                            ##
#########################################################################

## Set up plotting window
png(fig_fname, width = 9, height = 6 * 2.75/3, unit = 'in', res = 300)
layout(rbind(1:2,3:4, 5, 5), heights = c(4, 4, 1))

par(mar = c(0,0,0,0), oma = c(0,0,1,0))


calculate_weightedAverage <- function(xy, pmod, fire) {
    #pmod[[3]] = pmod[[3]]/4
    pmod = layer.apply(pmod, function(i) rasterFromXYZ(cbind(xy, i)))
    
    pmod = pmod / sum(pmod)
    pmod = layer.apply(pmod, function(i)
                       sum.raster(i * area(i) * fire, 
                                  na.rm = TRUE))
    pmod = unlist(pmod)
    
    pmod = round(100 * pmod / sum(pmod))
    
}

## Plot limitation and sesativity
plot_limtations_and_sensativity_plots <- function(lm_pmod, sn_pmod, labs) {
    
    plot_pmod <- function(pmod, lab, ...) {
        c(xy, pmod) := plot_4way_standard(pmod)
        pcs = calculate_weightedAverage(xy, pmod, ...)
        mtext(lab, line = -1, adj = 0.05)
        return(pcs)
    }
    
    scale2zeropnt <- function(x) {
        test = x == 0
        x[test] = NaN
        p = min.raster(x, na.rm = TRUE)
        print(p)
        x = 1-(1 - x)/(1 - p)       
        x[test] = 0
        return(x)
    }
    
    
    lm_pmod = lapply(lm_pmod, scale2zeropnt)
    fire = lm_pmod[[1]]
    lm_pmod = lm_pmod[-1]
    
    
    pcs = plot_pmod(lm_pmod, labs[1], fire)
    
    sn_pmod = lapply(sn_pmod, scale2zeropnt)
    fire    = sn_pmod[[1]]
    sn_pmod = sn_pmod[-1]
       
    
    sn2snFire <- function(i) {
        index = index[-i]
        l = sn_pmod[[i]] 
        for (j in lm_pmod[index]) l = l * (1 - j)
        return(l)
    }
    index = 1:length(sn_pmod)
    
    sn_pmod = lapply(index, sn2snFire)
    sn_pmod[[3]]= sn_pmod[[3]]/2 ## zero-point start correction
    
    pcs = rbind(pcs, plot_pmod(sn_pmod, labs[2], fire))
    return(pcs)
}


labs = c('a) Annual average controls on fire', 'b) Annual average sensitivity',
         'c) Controls on fire during the fire season', 'd) Sensitivity during the fire season')
pc_aa = plot_limtations_and_sensativity_plots(aa_lm_mod, aa_sn_mod, labs[1:2])
pc_fs = plot_limtations_and_sensativity_plots(fs_lm_mod, fs_sn_mod, labs[3:4])


## Add legend
par(mar = c(3, 10, 0, 8))
add_raster_4way_legend(cols = rev(c("FF","CC","99","55","11")),
                       labs = c('<- Moisture', 'Fuel ->', 'Igntions ->', 'Land Use'))

## add footer
par(fig = c(0, 1, 0, 1), mar = rep(0, 4))
points(0.5, 0.5, col = 'white', cex = 0.05)
dev.off.gitWatermark()
