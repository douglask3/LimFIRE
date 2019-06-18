add_raster_4way_legend <- function(labs = letters[1:4], limits = c(0.25, 0.5, 0.75),
                                   nx = ny * 2, ny = 800, ...) {
    library(shape)
    limits5 = c(0, limits, 1)
    add_3way <- function(i, xpos, yscale = 1) {
        out = add_raster_3way_legend_for_4way(legend, xpos = xpos, yscale = yscale,
                                              limits = limits, hash = i, ...)      
        
        #text(x = mean(xpos) * nx, y = -0.45 * ny, paste(limits5[i], '<', labs[4], '<', limits5[i+1]), xpd = TRUE)
        return(rbind(legend.xyz, out))  
    }

    
    legend = raster(nrows=ny/2, ncols=nx, xmn = 0, xmx = 1, ymn = 0, ymx =1)
    legend.xyz = c()
    
    legend.xyz = add_3way(1, xpos = 0.00)    
    legend.xyz = add_3way(2, xpos = 0.2, yscale = 0.8) 
    legend.xyz = add_3way(3, xpos = 0.375, yscale = 0.8)    
    legend.xyz = add_3way(4, xpos = 0.55, yscale = 0.8)     
    plot(c(nx*0.025, nx*0.775), range(as.numeric(legend.xyz[,2])), type = 'n', axes = FALSE, xlab = '', ylab = '')
    
    xy = apply(legend.xyz[,  1:2   ],2,as.numeric)
    z = 1:nrow(xy)
    xyz = cbind(xy, z)
    legend.z = rasterFromXYZ(xyz)
    legend.e = rasterFromXYZ(apply(legend.xyz[,c(1:2,4)],2,as.numeric))
    cols = legend.xyz[,3]
    #limits = as.numeric(unique(legend.xyz[,3])[-1]) - 0.5
       
    plot_raster_from_raster(legend.z, cols = cols, limits = z[-1]-0.5, quick = TRUE, 
                            coast.lwd = NULL, readyCut = TRUE, 
                            add_legend = FALSE, e = legend.e, invert_e = FALSE,
                            limits_error = seq(1.5, 3.5),
                            add = TRUE,  ePatternRes = 28,  ePatternThick = 0.4, 
                            e_polygon = FALSE)
    
    
    text  (nx * 0.020, ny * 0.05, labs[1], srt = 45, adj = 0, col = 'white')
    text  (nx * 0.230, ny * 0.05, labs[3], srt = -45, adj = 1, col = 'white')
    text  (nx * 0.125, ny * 0.92, labs[2], srt = 90, adj = 1, col = 'white')
    text  (nx * 0.5, ny * 0.12, labs[4])
    lines (nx * c(0.320,0.7), ny * c(0.01, 0.01), lwd = 2, xpd = NA) 
    Arrows(nx * 0.65, ny * 0.01, nx * 0.7, ny * 0.01, lwd = 2, xpd = NA) 
    #text(x = 0.10 * nx, y = -0.25 * ny, labs[1], xpd = TRUE           )
    #text(x = 0.00 * nx, y =  0.50 * ny, labs[2], xpd = TRUE, srt =  45)
    #text(x = 0.20 * nx, y =  0.50 * ny, labs[3], xpd = TRUE, srt = -45)
    
    #text(x = seq(0.0, 0.2*nx, length.out = 5), y = -0.1 *ny, rev(limits5), xpd = TRUE)    
}

add_raster_3way_legend_for_4way <- function(legend.z, cols, limits = c(0.25, 0.5, 0.75), 
                                            xpos = c(0,1), yscale = 1,
                                            lighten_factor = 1.4, hash = 1,...) {
    
    nsq = nrow(legend.z)
    xpos = round(xpos * ncol(legend.z))
    
    l = round(1+nsq*(1-yscale)/2)
    u = round(nsq*(0.5 + yscale/2))
    
    #limits = limits * u

    xyz = c()
    nz = 0
    index = l:ceiling(u/2)
    for (i in index) {
        #if (i %% 2 == 1) l = l + 1
        #    else         u = u - 1
        #l = i

        x = ((l+i):(u-i+1))
        if ( length(x) > 1 && x[1] > x[2]) break()
        #x = round((z / nsq * diff(xpos)) + xpos[1])
        y = round(rep(i, length(x)))
        #browser()
        #if (yscale != 1) browser()     
        #scaleYscale <- function(x) {
        #    x[x < 0.5] = x[x < 0.5] * yscale
        #    x[x > 0.5] = x[x > 0.5] /  yscale
        #    return(x)
        #}
        
        bl = (x - y)/(nsq)#(u - z - floor((i-l)/2) + 1)/u  
        gr = y/(nsq)#(2*rep(i-l+1, length(z)))/u
        rd = (nsq - x - y)/(nsq)#(z - floor(i))/u

        #bl = scaleYscale(bl)
        #gr = scaleYscale(gr)
        #rd = scaleYscale(rd)
        
        #gr0 = gr
        #bl = cut_results_col(bl)
        #gr = cut_results_col(gr)
        #rd = cut_results_col(rd)
        
        #xcols = paste('#', rd, gr, bl, sep = '')   
       
        #if (length(x) != length(y) || length(x) != length(xcols)) browser()
        #new_xyz = cbind(x, y, NaN, hash, xcols)
        
        new_xyz1 = cbind(x, y*2, rd, gr, bl)
        new_xyz2 = cbind(x, y*2+1, rd, gr, bl)
        if (!is.null(xyz) && ncol(xyz) != ncol(new_xyz1)) browser()
        xyz = rbind(xyz, new_xyz1, new_xyz2)
    }

    cut_results_col <- function(x) {
        #cols = c('00', '99', 'FF')
        x = cut_results(x, limits)
        x = cols[x]
        return(x)
    }

    #xyz0 = xyz
    #xyz = xyz0

    xyz[,2] = xyz[,2]*2
    
    fillGap <- function(i, j) {
        closest = which.min(abs(xyz[,1]-i) + abs(xyz[,2] - j))
        gapFill = c(i, j, xyz[closest,3:5])
        return(gapFill)
    }        
    #xs  = seq(min(xyz[,1]), max(xyz[,1]))
    #ys  = seq(min(xyz[,2]), max(xyz[,2]))
    #xyz = lapply(ys, function(i) t(sapply(xs, fillGap, i)))
    #xyz = do.call(rbind, xyz)
    

    A = xyz[,3]
    B = xyz[,4]*2
    C = xyz[,5]
    
    #mag = A + B + C	
    #A = A/mag
    #B = B/mag
    #C = C/mag

    #Ai = (B + C)/2
    #Bi = (A + C)/2
    #Ci = (A + B)/2

    #A = Ai
    #B = Bi
    #C = Ci
    range01 <- function(x) (x-min(x))/(max(x) - min(x))
    A = range01(A)
    B = range01(B)
    C = range01(C)

    Az = cut_results(A, limits)
    Bz = cut_results(B, limits)
    Cz = cut_results(C, limits)
    zcols = paste("#", cols[Az], cols[Bz], cols[Cz], sep = "")      
    #zcols = lighten(zcols, 0.5)
    
    

    Bzs =  (3*Bz/(Az + Bz + Cz))^0.9

    zcols = lighten(zcols, Bzs, transform = TRUE)
    

    xyz[,3] = zcols 
    xyz[,4] = hash
    xyz[,1] = as.numeric(xyz[,1]) + xpos
    
    #ucols = unique(xyz[,5])
    #xyz[,3] = sapply(xyz[,5], function(i) which(i == ucols))
    
    return(xyz[,1:4])
}
