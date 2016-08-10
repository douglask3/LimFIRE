add_raster_4way_legend <- function(labs = letters[1:4], limits = c(0.25, 0.5, 0.75),...) {

    limits5 = c(0, limits, 1)
    add_3way <- function(xpos, i) {
        add_raster_3way_legend_for_4way(xpos = xpos, limits = limits, darken_factor = i, ...)      
        text(x = mean(xpos), y = -0.45, paste(limits5[i], '<', labs[4], '<', limits5[i+1]), xpd = TRUE)
    }

    plot(c(0, 1), c(0, 1), type = 'n', axes = FALSE, xlab = '', ylab = '')
    
    add_3way(xpos = c(0.00, 0.20), 1)
    
    text(x = 0.10, y = -0.25, labs[1], xpd = TRUE           )
    text(x = 0.00, y = 0.50, labs[2], xpd = TRUE, srt =  45)
    text(x = 0.20, y = 0.50, labs[3], xpd = TRUE, srt = -45)
    
    text(x = seq(0.0, 0.2, length.out = 5), y = -0.1, rev(limits5), xpd = TRUE)
    
    add_3way(xpos = c(0.33, 0.53), 2) 
    add_3way(xpos = c(0.56, 0.76), 3)    
    add_3way(xpos = c(0.79, 0.99), 4)    
}

add_raster_3way_legend_for_4way <- function(cols, limits = c(0.25, 0.5, 0.75), xpos = c(0,1), 
                                    lighten_factor = 1.4, darken_factor = 1,...) {
    nsq = 200
    limits = limits * nsq
    
    l = 0; u = nsq
    for (i in 1:nsq) {
        if (i %% 2 == 1) l = l + 1
            else         u = u - 1
        
        z = (l:u)
        x = (z/nsq) * diff(xpos) + xpos[1]
        
        y = rep(i / nsq, length(x))
        
        bl = nsq - z - floor(i/2) + 1 
        gr = rep(i, length(z))
        rd = z - floor(i/2)
        
        cut_results_col <- function(x) {
            x = cut_results(x, limits)
            x = cols[x]
            return(x)
        }
        
        bl = cut_results_col(bl)
        gr = cut_results_col(gr)
        rd = cut_results_col(rd)
        
        xcols = paste('#', rd, gr, bl, sep = '')
        #xcols = lighten(xcols, lighten_factor)
        xcols =  darken(xcols,  darken_factor)
        
        points(x, y, col = xcols, pch = 19, cex = 0.01)
    }

}