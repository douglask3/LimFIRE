add_e <- function (e, limits_error, cols_e, invert_e = TRUE, polygons = TRUE, 
    ePatternThick = 4, ePatternRes = 0.7) 
{
    
    cols_e = make.transparent("grey", 1 - 1/(length(limits_error) + 
        1))
    if (polygons) 
        e = disaggregate(e, 5, method = "bilinear")
    else e = aggregate(e, fact = 4, expand = TRUE)
    e = cut_results(e, limits_error)
    if (invert_e) 
        e = invert.raster(e, vals = 1:(length(limits_error) + 
            1))
    add_transp <- function(lim, e, pch, cex, pattern, thick, 
        res) {
        ee = e
        ee[e > lim] = 1
        ee[e <= lim] = 0
        cells = values(ee) == 1
        xy = xyFromCell(ee, which(cells))
        cols = (e[cells] - i)/(nl - 1)
        cols_e = make.transparent("black", 0.33)
        cols = make.transparent("black", 1 - (0.75 * cols))
        if (polygons) {
            image(pattern.raster(ee, pattern, thick = thick, 
                res = res), col = c("transparent", cols_e), add = TRUE)
        }
        else points(xy, pch = pch, col = cols, cex = 0.7 * cex * 
            2)
    }
    nl = length(limits_error) + 1
    for (i in 1:(nl - 1)) add_transp(i, e, c(4, 3, 1, 16, 5, 
        9, 11)[i], c(1, 1, 1.3, 1.3, 1.3, 1, 1)[i], c("Circle", 
        "Circle", "forward-diagonal", "backward-diagonal", "horizontal", 
        "vertical")[i], c(0.1, 0.67, 0.5, 0.5, 0.5, 0.5)[i] * 
        ePatternThick, c(16, 8, 4, 4, 4, 4)[i] * ePatternRes)
}
