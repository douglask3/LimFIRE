darken <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col/factor
    col <- rgb(t(col), maxColorValue=255)
    col
}

lighten <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col*factor
    col <- rgb(t(col), maxColorValue=255)
    col
}