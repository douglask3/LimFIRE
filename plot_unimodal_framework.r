#################################################################
## Setup plot                                                  ##
#################################################################
source("cfg.r")
graphics.off()
png("figs/framework_descrption.png", height = 8, width = 4, res = 300, unit = 'in')
par(mfrow = c(3,1), mar = c(5.1,4, 0, 1), oma = c(0, 0, 2, 0))
plot.new()
#par(family = "HersheySerif", font = 8)

p1 = 100; p2 = 10

colMstr = "blue"; colFuel = "green"; colFire = "orange"

#################################################################
## Axis                                                        ##
#################################################################
arrows( 0.0, -0.1,  1.0, -0.1, col = colMstr, lwd = 5, xpd = TRUE)
mtext('Moisture', side = 1, line = -0.4)

arrows( 0.0, -0.2,  1.0, -0.2, col = colFuel, lwd = 5, xpd = TRUE)
mtext('Fuel', side = 1, line = 2.5)

arrows(-0.1,  0.0, -0.1,  1.0, col = colFire, lwd = 5, xpd = TRUE)
mtext('Fire', side = 2)


#################################################################
## Lines                                                        ##
#################################################################
x = seq(0, 1, by = 0.001)

moisture = 1- LimFIRE.moisture(x, p1, p2)
fuel     = 1- LimFIRE.fuel    (x, p1, p2)

addLine <- function(y, col, lwd = 5, alpha = 0.5)
    lines(x, y, col = make.transparent(col, alpha), lwd = 5)
   
addLine(moisture, colMstr)
addLine(fuel    , colFuel)

###
## Igntions
##

plot.new()
#par(family = "HersheySerif", font = 8)

p1 = 10

ignMstr = "red"

#################################################################
## Axis                                                        ##
#################################################################
arrows( 0.0, -0.1,  1.0, -0.1, col = ignMstr, lwd = 5, xpd = TRUE)
mtext('Ignitions', side = 1, line = -0.4)

arrows(-0.1,  0.0, -0.1,  1.0, col = colFire, lwd = 5, xpd = TRUE)
mtext('Fire', side = 2)


#################################################################
## Lines                                                        ##
#################################################################
x = seq(0, 10, by = 0.001)

ignitions = 1- LimFIRE.ignitions(x, p1)
   
addLine(ignitions, ignMstr)



###
## suppression
##

plot.new()
#par(family = "HersheySerif", font = 8)


p1 = 100; p2 = 10


supMstr = "black"

#################################################################
## Axis                                                        ##
#################################################################
arrows( 0.0, -0.1,  1.0, -0.1, col = supMstr, lwd = 5, xpd = TRUE)
mtext('suppression', side = 1, line = -0.4)

arrows(-0.1,  0.0, -0.1,  1.0, col = colFire, lwd = 5, xpd = TRUE)
mtext('Fire', side = 2)


#################################################################
## Lines                                                        ##
#################################################################
x = seq(0, 1, by = 0.001)

suppression = 1- LimFIRE.supression(x, p1, p2)
   
addLine(suppression, supMstr)#

dev.off.gitWatermark()