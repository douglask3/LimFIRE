## find transect xys
findTrasectCells <- function(transect, dat) {
    dxy = apply(transect, 2, diff); dxy = dxy[1]/dxy[2]

    findPoints <- function(x, x0, dx, y0) c(x, y0 + (x - x0)*dx)


    if (abs(dxy) > 1) {
        coords = sapply(seq(transect[1,1],transect[2,1],-0.125),
                        findPoints, transect[1,1], (1/dxy), transect[1,2])
    } else {
        coords = sapply(seq(transect[1,2],transect[2,2],0.125),
                        findPoints, transect[1,2],     dxy, transect[1,1])
        coords = coords[2:1, ]
    }
    
    cells = cellFromXY(dat, t(coords))
    return(list(cells, coords))
}
