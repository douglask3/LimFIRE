darken <- function(color, factor=1.4){
    col = col2rgb(color)
    col = col/factor
    col = rgb(t(col), maxColorValue=255)
    return(col)
}

invert.color <- function(color, factor=1.4){	
	col <- col2rgb(color)
	col = sqrt((255^2) - (col^2))
	col <- rgb(t(col), maxColorValue=255)
	return(col)
}

hue_shift <- function(color, shift = -1/6) {
	col = col2rgb(color)
	col = rgb2hsv(col)
	col[1,] = col[1,] + shift
	
	while (min(col[1,]) < 0 || max(col[1,]) > 1) {
		col[1,col[1,] > 1] = col[1,col[1,] > 1] - 1
		col[1,col[1,] < 0] = col[1,col[1,] < 0] + 1
	}
	#browser()
	col <- hsv(t(col))
	return(col)
}

lighten <- function(color, factor=1.4, transform = FALSE){
    col = col2rgb(color)
    col = col2rgb(color)/255
    col0 = col
    transform_and_scale <- function(coli, factori) {
        if (transform)
            if(factori > 1) coli = 1 / (1-coli) else coli = -1/coli
        coli = coli * factori
        if (transform) 
            if(factori > 1) coli = 1 - (1/coli) else coli = -1/coli
        return(col)
    }

   
    if (length(factor) == ncol(col)) col = sweep(col, 2, factor, '*')
        else col = transform_and_scale(col, factor)
    
    col[col > 1] = 1
    
    col = rgb(t(col), maxColorValue=1)
    #col = rgb(t(as.matrix(apply(col, 1, function(x) {x[x>255] = 255; x}))), maxColorValue=1)
    return(col)
}
