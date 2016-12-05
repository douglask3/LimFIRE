#################################################################
## cfg                                                         ##
#################################################################
source('cfg.r')

p1 = 100; p2 = 10
colMstr = "blue"; colFuel = "green"; colFire = "orange"

MstrCols = c('white','cyan', '#00221F')
MstrLims =  c(0.01,0.05, 0.1,0.2,0.3, 0.4)

#################################################################
## Setup plot                                                  ##
#################################################################
graphics.off()
png('figs/unimodal.png', height = 4, width = 8, units = 'in', res = 300)
lmat = cbind(2:5, 1)

layout(lmat, heights = c(1,0.3,1, 0.3))
par(oma = c(0,0,1,0))


#par(family = "HersheySerif", font = 8)

#################################################################
## Line plot                                                   ##
#################################################################
plot.new()

## Axis
arrows( 0.0, -0.1,  1.0, -0.1, col = colMstr, lwd = 5, xpd = TRUE)
mtext('Moisture', side = 1, cex = 1.33)

arrows( 0.0, -0.2,  1.0, -0.2, col = colFuel, lwd = 5, xpd = TRUE)
mtext('Fuel', side = 1, cex = 1.33, line = 1.8)

arrows( 1.0, -0.1,  0.0, -0.1, col = colMstr , lwd = 5, xpd = TRUE)

arrows(-0.1,  0.0, -0.1,  1.0, col = colFire, lwd = 5, xpd = TRUE)
mtext('Fire', side = 2, cex = 1.33, line = -0.3)


## Lines
x = seq(0, 1, by = 0.001)

moisture = 1- LimFIRE.moisture(x, p1, p2)
fuel     = 1- LimFIRE.fuel    (x, p1, p2)

addLine <- function(y, col, lwd = 5, alpha = 0.5)
    lines(x, y, col = make.transparent(col, alpha), lwd = 5)
   
addLine(moisture, colMstr)
addLine(fuel    , colFuel)

addLine(fuel * moisture, colFire)

## Dry/wet Season
i = seq(1, 18, length.out = 180)
i = c((1/rev(i)), i)

for (i in i) {
    mstr = 1 - LimFIRE.moisture(x, p1*i, p2)
    addLine(fuel * mstr, colFire, alpha = 0.95)
    addLine(mstr, colMstr, alpha = 0.95)
    
}

#################################################################
## Seasonality and fire maps                                   ##
#################################################################
par(mar = rep(0,4))

## Open Data
alpha = stack(alphaFile)
fire  = stack(fireFile )

alpha = PolarConcentrationAndPhase(alpha)[[2]]
fire  = mean(fire)*1200

## plot alpha
plot_raster(alpha, quick = TRUE, lims = MstrLims, cols = MstrCols)
standard_legend(cols = MstrCols, lims = MstrLims, dat = alpha)
mtext('Seasonality of vailable moisture', line = -0.5)
## plot fire
plot_raster(fire, quick = TRUE)
standard_legend(dat = fire)
mtext('% Annual Burnt Area', line = -0.5)
dev.off.gitWatermark()