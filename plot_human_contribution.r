source('cfg.r')
mod_file = 'outputs/LimFIRE_fire'
fig_file = 'figs/HumanImpactMap.png'

mod_file = paste(mod_file, c('', 'noHumanIngnitions', 'noHumans'), '.nc', sep ='')


diff_lims1 = c(0, 0.1, 1, 2, 5) 
diff_cols1 = fire_cols

diff_lims2 = c(-20, -10, -5, -2, -1, -0.1, 0.1, 1) 
diff_cols2 = c('#000033', '#0022AA',  '#00EEFF', 'white', '#FF7700')


cont_lims1 = c(0, 2, 5, 10, 20, 40, 60, 80)
cont_cols1 = fire_cols

cont_lims2 = cont_lims1
cont_cols2 = c("#FFFFFF", "#00EEFF", "#0022AA", "#000033") 

if (!exists('AGUplot')) {
    labs = c('a) Full model burnt area', 'b) No human ignitions', 'c) No humans', 'd) Burnt Area from human ignitions', 'e) Contribution of humans',
             'f) % Contribution of human igntions', 'g) % Contribution of humans') 
}

control = runIfNoFile(mod_file[1], runLimFIREfromstandardIns, fireOnly = TRUE)
noIgnit = runIfNoFile(mod_file[2], runLimFIREfromstandardIns, fireOnly = TRUE, 
                  remove = "pas")
noAnyth = runIfNoFile(mod_file[3], runLimFIREfromstandardIns, fireOnly = TRUE, 
                  remove = c("pas", "crop", "popdens"))


if (!exists('AGUplot')) {                  
    graphics.off()
    png(fig_file, width = 12, height = 9, units = 'in', res = 300)
                     
    layout(rbind(c(1,  2,  3),
                 c(4,  4,  4), 
                 c(0,  5,  7), 
                 c(0,  6,  8),
                 c(0,  9, 11),
                 c(0, 10, 12)),
                 height = c(1, 0.3, 1, 0.3, 1, 0.3))
    
    mtextStandard <- function(...) mtext(..., line = -2)
} else mtextStandard <- function(...) mtext(..., adj = 0.05)
par(mar = c(0,0,0,0))



standard_legend <- function(cols = fire_cols, lims = fire_lims, dat,
                            plot_loc = c(0.35,0.75,0.65,0.78)) {
    add_raster_legend2(cols, lims, add = FALSE,
               plot_loc = plot_loc, dat = dat,
               transpose = FALSE,
               srt = 0)
} 

standard_legend2 <- function(...)
        standard_legend(plot_loc = c(0.2, 0.9, 0.65, 0.78), ...)


mtext.burntArea <- function(txt = 'Burnt Area (%)') mtext(txt, cex = 0.8, line = -5)                  


if (exists('AGUplot')) plot = FALSE else plot = TRUE     
control = aaConvert(control, plot = plot)
if (plot) mtextStandard(labs[1])
noIgnit = aaConvert(noIgnit, plot = plot)
if (plot) mtextStandard(labs[2])
noAnyth = aaConvert(noAnyth, plot = plot)
if (plot) {
    mtextStandard(labs[3])
    standard_legend(dat = control)
    mtext.burntArea()
}

noIgnit = control - noIgnit
noAnythi = control - noAnyth  

plot_raster(noIgnit, diff_lims2,diff_cols2)
mtextStandard(labs[4])

plot_raster(noAnythi, diff_lims2, diff_cols2)
mtextStandard(labs[5])

standard_legend2(diff_cols2, diff_lims2, dat = noAnyth)
mtext.burntArea('Change in burnt area land(%)')

if (!exists('AGUplot')) {
    noIgnit = 100 * noIgnit / control
    noAnyth = 100 * control / noAnyth

    plot_raster(noIgnit, cont_lims1, cont_cols1)
    mtextStandard(labs[6])
    standard_legend2(cont_cols1, cont_lims1, dat = noIgnit)
    mtext.burntArea('Change in burnt area (%)')

    plot_raster(noAnyth, cont_lims2, cont_cols2)
    mtextStandard(labs[7])
    standard_legend2(cont_cols2, cont_lims2, dat = noAnyth)
    mtext.burntArea('Change in burnt area (%)')
    
    dev.off.gitWatermark()
}
