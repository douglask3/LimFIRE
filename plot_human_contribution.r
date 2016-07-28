source('cfg.r')
mod_file = 'outputs/LimFIRE_fire'
fig_file = 'figs/HumanImpactMap.png'

mod_file = paste(mod_file, c('', 'noHumanIngnitions', 'noHumans'), '.nc', sep ='')
 
control = runIfNoFile(mod_file[1], runLimFIREfromstandardIns, fireOnly = TRUE)
noIgnit = runIfNoFile(mod_file[2], runLimFIREfromstandardIns, fireOnly = TRUE, 
                  remove = "pas")
noAnyth = runIfNoFile(mod_file[3], runLimFIREfromstandardIns, fireOnly = TRUE, 
                  remove = c("pas", "crop", "popdens"))

                  
graphics.off()
png(fig_file, width = 12, height = 9, units = 'in', res = 300)
par(mar = c(0,0,0,0))
                 
layout(rbind(c(1,  2,  3),
             c(4,  4,  4), 
             c(0,  5,  7), 
             c(0,  6,  8),
             c(0,  9, 11),
             c(0, 10, 12)),
             height = c(1, 0.3, 1, 0.3, 1, 0.3))

diff_lims  = c(-30, -20, -10, -5, -1, 0, 1) 
diff_cols  = c('#000033', '#0099FF', '#66FFFF', '#FFEE00')

cont_lims1 = c(0, 2, 5, 10, 20, 40, 60, 80)
cont_cols1 = fire_cols

cont_lims2 = cont_lims1
cont_cols2 = c("#FFFFFF", "#00EEFF", "#0022AA", "#000033") 

mtextStandard <- function(...) mtext(...)

standard_legend <- function(cols = fire_cols, lims = fire_lims, dat,
                            plot_loc = c(0.35,0.75,0.65,0.78)) {
    add_raster_legend2(cols, lims, add = FALSE,
               plot_loc = plot_loc, dat = dat,
               transpose = FALSE,
               srt = 0)
} 

standard_legend2 <- function(...)
        standard_legend(plot_loc = c(0.2, 0.9, 0.65, 0.78), ...)
                  
control = aaConvert(control)
mtextStandard('a) Full model burnt area')
noIgnit = aaConvert(noIgnit)
mtextStandard('b) No human ignitions')
noAnyth = aaConvert(noAnyth)
mtextStandard('c) No humans')

standard_legend(dat = control)


noIgnit = control - noIgnit
noAnythi = control - noAnyth

plot_raster(noIgnit, fire_lims /10)
mtextStandard('d) Burnt Area from human ignitions')
standard_legend2(dat = noIgnit, lims = fire_lims/10)
plot_raster(noAnythi, diff_lims, diff_cols)
mtextStandard('e) Contribution of humans')
standard_legend2(diff_cols, diff_lims, dat = noAnyth)


noIgnit = 100 * noIgnit / control
noAnyth = 100 * control / noAnyth

plot_raster(noIgnit, cont_lims1, cont_cols1)
mtextStandard('f) % Contribution of human igntions')
standard_legend2(cont_cols1, cont_lims1, dat = noIgnit)
plot_raster(noAnyth, cont_lims2, cont_cols2)
mtextStandard('g) % Contribution of humans')
standard_legend2(cont_cols2, cont_lims2, dat = noAnyth)

dev.off.gitWatermark()
