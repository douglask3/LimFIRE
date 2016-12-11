#################################################################
## cfg                                                         ##
#################################################################
source('cfg.r')

figDir = 'figs/unimodal/'

alphaFile = 'outputs/alpha2000-2014.nc'
fireFile  = 'outputs/fire2000-2014.nc'

p1 = 100; p2 = 10
colMstr = "blue"; colFuel = "green"; colFire = "orange"

MstrCols = c('white','cyan', '#00221F')
MstrLims =  c(0.01,0.05, 0.1,0.2,0.3, 0.4)

doff = 0;
dev.off.advance <- function() {
    print(doff)
    print(nplot)
    print('-----')
    if (doff < nplot) {        
        dev.off.gitWatermark()
        doff <<- nplot
        eval.parent(parse(text = "next"), 1)
    } else {        
        nplot <<- nplot + 1
    }
}

#################################################################
## Open Data												   ##
#################################################################
alpha = stack(alphaFile)
fire  = stack(fireFile )

alpha = PolarConcentrationAndPhase(alpha)[[2]]
fire  = mean(fire)*1200

#################################################################
## Setup plot                                                  ##
#################################################################
graphics.off()
endTest = FALSE
while (!endTest) {
nplot = 1
figName = paste(figDir, '/p', doff + 1, '.png', sep = '')
png(figName, height = 4, width = 8, units = 'in', res = 300)
lmat = cbind(2:5, 1)

layout(lmat, heights = c(1,0.3,1, 0.3))
par(oma = c(0,0,1,0))


#par(family = "HersheySerif", font = 8)

#################################################################
## Line plot                                                   ##
#################################################################
plot.new()

## Lines
x = seq(0, 1, by = 0.001)

moisture = 1- LimFIRE.moisture(x, p1, p2)
fuel     = 1- LimFIRE.fuel    (x, p1, p2)

addLine <- function(y, col, lwd = 5, alpha = 0.5)
    lines(x, y, col = make.transparent(col, alpha), lwd = 5)

## Axis
arrows( 0.0, -0.1,  1.0, -0.1, col = colMstr, lwd = 5, xpd = TRUE)
mtext('Moisture', side = 1, cex = 1.33)
dev.off.advance()

arrows(-0.1,  0.0, -0.1,  1.0, col = colFire, lwd = 5, xpd = TRUE)
mtext('Fire', side = 2, cex = 1.33, line = -0.3)
addLine(fuel * moisture, colFire)
dev.off.advance()

arrows( 0.0, -0.2,  1.0, -0.2, col = colFuel, lwd = 5, xpd = TRUE)
mtext('Fuel', side = 1, cex = 1.33, line = 1.8)
dev.off.advance()
   
addLine(moisture, colMstr)
text(0.9, 0.2, 'too wet', col = '#0000AA', cex = 1.5)
dev.off.advance()

addLine(fuel    , colFuel)
text(0.1, 0.2, 'Not enough\nfuel', col = '#00AA00', cex = 1.5)
dev.off.advance()

## Dry/wet Season
pv = seq(1, 18, length.out = 180)
pv = c((1/rev(pv)), pv)

np = 0

addLines <- function(i, alpha = 0.95) {
    mstr = 1 - LimFIRE.moisture(x, p1*i, p2)
    addLine(fuel * mstr, colFire, alpha = alpha)
    addLine(mstr, colMstr, alpha = alpha)
}

addLines(pv[1], 0.5)
addLine(fuel  , colFuel, alpha = 0.3)
dev.off.advance()

plotQuater <- function(m) {
    index  = seq(((m-1) *90) + 1, m *90)
    sapply(pv[index], addLines)
}

plotQuater(1)
dev.off.advance()

plotQuater(2)
dev.off.advance()

plotQuater(3)
dev.off.advance()

plotQuater(4)
addLines(tail(pv, 1), 0.5)
dev.off.advance()
#################################################################
## Seasonality and fire maps                                   ##
#################################################################
par(mar = rep(0,4))

## plot alpha
plot_raster(alpha, quick = TRUE, lims = MstrLims, cols = MstrCols)
standard_legend(cols = MstrCols, lims = MstrLims, dat = alpha)
mtext('Seasonality of available moisture', line = -0.5)
dev.off.advance()

## plot fire
plot_raster(fire, quick = TRUE)
standard_legend(dat = fire)
mtext('% Annual Burnt Area', line = -0.5)
dev.off.gitWatermark()
endTest = TRUE
}