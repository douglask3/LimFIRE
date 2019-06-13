add_raster_4way_legend <- function(labs = letters[1:4], limits = c(0.25, 0.5, 0.75),
                                   nx = ny * 12.5, ny = 20, ...) {
    
    limits5 = c(0, limits, 1)
    add_3way <- function(i, xpos, yscale = 1) {
        out = add_raster_3way_legend_for_4way(legend, xpos = xpos, yscale = yscale,
                                              limits = limits, hash = i, ...)      
        
        #text(x = mean(xpos) * nx, y = -0.45 * ny, paste(limits5[i], '<', labs[4], '<', limits5[i+1]), xpd = TRUE)
        return(rbind(legend.xyz, out))  
    }

    plot(c(0, nx), c(0, ny), type = 'n', axes = FALSE, xlab = '', ylab = '')
    legend = raster(nrows=ny, ncols=nx, xmn = 0, xmx = 1, ymn = 0, ymx =1)
    legend.xyz = c()
    
    legend.xyz = add_3way(1, xpos = c(0.00, 0.3))    
    legend.xyz = add_3way(2, xpos = c(0.333, 0.533), yscale = 0.67) 
    legend.xyz = add_3way(3, xpos = c(0.567, 0.767), yscale = 0.67)    
    legend.xyz = add_3way(4, xpos = c(0.8, 1.0), yscale = 0.67)     
 
    browser()   
    xy = apply(legend.xyz[,  1:2   ],2,as.numeric)
    z = 1:nrow(xy)
    xyz = cbind(xy, z)
    legend.z = rasterFromXYZ(xyz)
    legend.e = rasterFromXYZ(apply(legend.xyz[,c(1:2,4)],2,as.numeric))
    cols = legend.xyz[,3]
    #limits = as.numeric(unique(legend.xyz[,3])[-1]) - 0.5
    
    plot_raster_from_raster(legend.z, cols = cols, limits = z-0.5, smooth_image = FALSE, coast.lwd = NULL, readyCut = TRUE, 
                             add_legend = FALSE, e = legend.e, invert_e = FALSE,  limits_error = seq(1.5, 3.5),
                             add = TRUE,  ePatternRes = 100,  ePatternThick = 0.4, e_polygon = FALSE)
    
    
    text(2, 0, labs[1], srt = 45, adj = 0, col = 'white')
    text(45, 1.0, labs[3], srt = -45, adj = 1, col = 'white')
    text(25, 17.0, labs[2], srt = 90, adj = 1, col = 'white')
    text(150, 1.3, labs[4])
    arrows(80.3, 2.5, 220.0, 0, lwd = 2) 
    #text(x = 0.10 * nx, y = -0.25 * ny, labs[1], xpd = TRUE           )
    #text(x = 0.00 * nx, y =  0.50 * ny, labs[2], xpd = TRUE, srt =  45)
    #text(x = 0.20 * nx, y =  0.50 * ny, labs[3], xpd = TRUE, srt = -45)
    
    #text(x = seq(0.0, 0.2*nx, length.out = 5), y = -0.1 *ny, rev(limits5), xpd = TRUE)    
}

add_raster_3way_legend_for_4way <- function(legend.z, cols, limits = c(0.25, 0.5, 0.75), 
                                            xpos = c(0,1), yscale = 1,
                                            lighten_factor = 1.4, hash = 1,...) {
    
    nsq = nrow(legend.z)
    xpos = xpos * ncol(legend.z)
    
    l = round(1+nsq*(1-yscale)/2)
    u = round(nsq*(0.5 + yscale/2))
    
    limits = limits * u

    xyz = c()
    nz = 0
    index = l:u
    for (i in index) {
        if (i %% 2 == 1) l = l + 1
            else         u = u - 1
        
        z = (l:u)
        x = (z / nsq * diff(xpos)) + xpos[1]
        y = rep(i, length(x))
        
        bl = u - z - floor((i-l)/2) + 1 
        gr = 2*rep(i-l+1, length(z))
        rd = z - floor(i/2)
        
        
        #if (index[1] != 1) browser()
        
        #gr0 = gr
        #bl = cut_results_col(bl)
        #gr = cut_results_col(gr)
        #rd = cut_results_col(rd)
        
        #xcols = paste('#', rd, gr, bl, sep = '')   
       
        #if (length(x) != length(y) || length(x) != length(xcols)) browser()
        #new_xyz = cbind(x, y, NaN, hash, xcols)

        new_xyz = cbind(x, y, bl, gr, rd)
        if (!is.null(xyz) && ncol(xyz) != ncol(new_xyz)) browser()
        xyz = rbind(xyz, new_xyz)
    }

    cut_results_col <- function(x) {
        x = cut_results(x, limits)
        x = cols[x]
        return(x)
    }

    r = cut_results_col(xyz[,3])
    g = cut_results_col(xyz[,4])    
    b = cut_results_col(xyz[,5])
    xyz[,3] = paste0('#', r, g, b)
    xyz[,4] = hash
    #ucols = unique(xyz[,5])
    #xyz[,3] = sapply(xyz[,5], function(i) which(i == ucols))
    
    return(xyz[,1:4])
}
