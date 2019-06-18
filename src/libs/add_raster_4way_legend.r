add_raster_4way_legend <- function(labs = letters[1:4], limits = c(0.25, 0.5, 0.75),
                                   nx = ny * 2, ny = 800, ...) {
    library(shape)
    limits5 = c(0, limits, 1)
    add_3way <- function(i, xpos, yscale = 1) {
        out = add_raster_3way_legend_for_4way(legend, xpos = xpos, yscale = yscale,
                                              limits = limits, hash = i, ...)      
        return(rbind(legend.xyz, out))  
    }

    
    legend = raster(nrows=ny/2, ncols=nx, xmn = 0, xmx = 1, ymn = 0, ymx =1)
    legend.xyz = c()
    
    legend.xyz = add_3way(1, xpos = 0.00)    
    legend.xyz = add_3way(2, xpos = 0.2, yscale = 0.8) 
    legend.xyz = add_3way(3, xpos = 0.375, yscale = 0.8)    
    legend.xyz = add_3way(4, xpos = 0.55, yscale = 0.8)     
    plot(c(nx*0.025, nx*0.775), range(as.numeric(legend.xyz[,2])), 
         type = 'n', axes = FALSE, xlab = '', ylab = '')
    
    xy = apply(legend.xyz[,  1:2   ],2,as.numeric)
    z = 1:nrow(xy)
    xyz = cbind(xy, z)
    legend.z = rasterFromXYZ(xyz)
    legend.e = rasterFromXYZ(apply(legend.xyz[,c(1:2,4)],2,as.numeric))
    cols = legend.xyz[,3]
       
    plot_raster_from_raster(legend.z, cols = cols, limits = z[-1]-0.5, quick = TRUE, 
                            coast.lwd = NULL, readyCut = TRUE, 
                            add_legend = FALSE, e = legend.e, invert_e = FALSE,
                            limits_error = seq(1.5, 3.5),
                            add = TRUE,  ePatternRes = 28*5,  ePatternThick = 0.4, 
                            e_polygon = FALSE)
    
    
    text  (nx * 0.022, ny * 0.05, labs[1], srt = 45, adj = 0, col = 'white', font = 2)
    text  (nx * 0.230, ny * 0.05, labs[3], srt = -45, adj = 1, col = 'white', font = 2)
    text  (nx * 0.125, ny * 0.90, labs[2], srt = 90, adj = 1, col = 'white', font = 2)
    text  (nx * 0.5, ny * 0.10, labs[4], font = 2)
    lines (nx * c(0.320,0.7), ny * c(0.0, 0.0), lwd = 2, xpd = NA) 
    Arrows(nx * 0.65, ny * 0.0, nx * 0.7, ny * 0.0, lwd = 2, xpd = NA) 
}

add_raster_3way_legend_for_4way <- function(legend.z, cols, limits = c(0.25, 0.5, 0.75), 
                                            xpos = c(0,1), yscale = 1,
                                            lighten_factor = 1.4, hash = 1,...) {
    
    nsq = nrow(legend.z)
    xpos = round(xpos * ncol(legend.z))
    
    l = round(1+nsq*(1-yscale)/2)
    u = round(nsq*(0.5 + yscale/2))

    xyz = c()
    nz = 0
    index = l:ceiling(u/2)
    for (i in index) {
        x = ((l+i):(u-i+1))
        if ( length(x) > 1 && x[1] > x[2]) break()
       
        y = round(rep(i, length(x)))
        
        
        bl = (x - y)/(nsq)
        gr = y/(nsq)
        rd = (nsq - x - y)/(nsq)
        
        new_xyz1 = cbind(x, y*2, rd, gr, bl)
        new_xyz2 = cbind(x, y*2+1, rd, gr, bl)
        if (!is.null(xyz) && ncol(xyz) != ncol(new_xyz1)) browser()
        xyz = rbind(xyz, new_xyz1, new_xyz2)
    }

    xyz[,2] = xyz[,2]*2
    
    fillGap <- function(i, j) {
        closest = which.min(abs(xyz[,1]-i) + abs(xyz[,2] - j))
        gapFill = c(i, j, xyz[closest,3:5])
        return(gapFill)
    }          

    A = xyz[,3]
    B = xyz[,4]*2
    C = xyz[,5]
    
    range01 <- function(x) (x-min(x))/(max(x) - min(x))
    A = range01(A)
    B = range01(B)
    C = range01(C)

    Az = cut_results(A, limits)
    Bz = cut_results(B, limits)
    Cz = cut_results(C, limits)
    zcols = paste("#", cols[Az], cols[Bz], cols[Cz], sep = "")    

    Bzs =  (3*Bz/(Az + Bz + Cz))^0.9
    zcols = lighten(zcols, Bzs, transform = TRUE)
    
    xyz[,3] = zcols 
    xyz[,4] = hash
    xyz[,1] = as.numeric(xyz[,1]) + xpos
    
    return(xyz[,1:4])
}
