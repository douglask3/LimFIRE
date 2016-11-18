param <- function(p) {
    if (!exists('params_mean')) {
        params = read.csv(coefficants_file)[,-1]
        params_mean = apply(as.matrix(params),2, median)
        params_mean <<- params_mean
    }
    
    param = params_mean[p]
    if (is.na(param)) {
        if (p == 'L') param = 1 else param = 0
    }
    
    return(param)
}