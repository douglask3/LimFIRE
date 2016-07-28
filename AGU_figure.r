AGUplot = TRUE
graphics.off()   

png("figs/aguplot.png", width = 9, height = 6, unit = 'in', res = 300)

layout(rbind(c(1, 2),
             c(3, 3),
             c(4, 6),
             c(5, 7)), heights = c(4, 2, 4, 1.333))
             

labs = c('', '', 'Limits on fire', 'Sensativity of fire')
source("plot_limitation_maps.r")

source("plot_human_contribution.r")

dev.off.gitWatermark()