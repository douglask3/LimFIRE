f1 <- function(x, x0, k, force_zero = FALSE) {
    f1A <- function(xi) 1/(1 + exp(-k * (xi - x0)))
    
    f1 = f1A(x)
    
    if (force_zero) {
        f0 = f1A(0)
        f1  = (f1 - f0)/(1 - f0)
        f1[is.na(f1)] = 0.0
    }
    return(f1)
}


df1 <- function(x, x0, k, d = 0.0001, normalise = TRUE) {
	
    df1_fun <- function(i) (f1(i + d, x0, k) - f1(i - d, x0, k))/(2*d)
    dx    = df1_fun(x)
    if (normalise) dx = dx / df1_fun(x0)
    return(dx)
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