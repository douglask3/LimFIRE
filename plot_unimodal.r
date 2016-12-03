#################################################################
## Setup plot                                                  ##
#################################################################
plot.new()
#par(family = "HersheySerif", font = 8)

p1 = 100; p2 = 10

colMstr = "blue"; colFuel = "green"; colFire = "orange"

#################################################################
## Axis                                                        ##
#################################################################
arrows( 0.0, -0.1,  1.0, -0.1, col = colMstr, lwd = 5, xpd = TRUE)
mtext('Moisture', side = 1, cex = 1.33)

arrows( 0.0, -0.2,  1.0, -0.2, col = colFuel, lwd = 5, xpd = TRUE)
mtext('Fuel', side = 1, cex = 1.33, line = 1.8)

arrows( 1.0, -0.1,  0.0, -0.1, col = colMstr , lwd = 5, xpd = TRUE)

arrows(-0.1,  0.0, -0.1,  1.0, col = colFire, lwd = 5, xpd = TRUE)
mtext('Fire', side = 2, cex = 1.33)


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

addLine(fuel * moisture, colFire)

#################################################################
## Dry/wet Season                                              ##
#################################################################
#wet = 1- LimFIRE.moisture(x, p1/6, p2)
#dry = 1- LimFIRE.moisture(x, p1*6, p2)
#
#polygon(c(x, rev(x)), c(wet, rev(dry)), 
#        col = make.transparent('blue', 0.9), border = NA)
        
#addLine(fuel * dry, "red")

i = seq(1, 18, length.out = 180)
i = c((1/rev(i)), i)

for (i in i) {
    mstr = 1 - LimFIRE.moisture(x, p1*i, p2)
    addLine(fuel * mstr, colFire, alpha = 0.95)
    addLine(mstr, colMstr, alpha = 0.95)
    
}