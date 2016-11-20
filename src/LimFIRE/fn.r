f1 <- function(x, x0, k, force_zero = TRUE, force_one = NULL) {
    f1A <- function(xi) 1/(1 + exp(-k * (xi - x0)))
    
    fX = f1A(x)
    if (force_zero) {
        f0 = f1A(0)
        fX  = (fX - f0)/(1 - f0)
        fX[is.na(fX)] = 0.0
    }
    
    if (!is.null(force_one)) {
        f1 = f1(force_one, x0, k, force_zero)
        if (f1 != 0) fX = fX / f1
    }
    return(fX)
}


df1 <- function(x, a, b, d = 0.1) {       
    df1_fun <- function(i) f1(i - d, a, b) - f1(i + d, a, b)
    
    
    xhalf =  -(1/b) * log(1/a)
    dhalf = df1_fun(xhalf)
    
    dx    = df1_fun(x)
    
    return(dx / dhalf)
}

f2 <- function(x, a) {
    x = x * a
    return(x / (x + 1))
}

df2 <- function(x, a, d = 0.1) {       
    df2_fun <- function(i) f2(i - d, a) - f2(i + d, a)
    
    
    dhalf = df2_fun(0)    
    dx    = df2_fun(x)
    
    return(dx / dhalf)
}