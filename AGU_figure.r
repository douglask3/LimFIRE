AGUplot = TRUE
graphics.off()   

png("figs/aguplot.png", width = 4.5, height = 6, unit = 'in', res = 300)

layout(rbind(c(1),
             c(2),
             c(3),
             c(4)), heights = c(4, 2.3, 4, 1.333))
             

labs = c('', '', 'Limits on fire', 'Sensitivity of fire')
source("plot_limitation_maps.r")

labs = c('', '', '', 'Impact of humans on fire', 'Impact of human ignition and land use on fire')
source("plot_human_contribution.r")

dev.off()