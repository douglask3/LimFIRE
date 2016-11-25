f1 <- function(x, a, b) 1/(1 + a * exp(-b * x))


df1 <- function(x, a, b, d = 0.1) {       
    df1_fun <- function(i) f1(i - d, a, b) - f1(i + d, a, b)
    
    
    xhalf =  -(1/b) * log(1/a)
    dhalf = df1_fun(xhalf)
    
    dx    = df1_fun(x)
    
    return(dx / dhalf)
}

f2 <- function(x, a, b) {
    x = b + x * a
    return(x / (x + 1))
}

df2 <- function(x, a, d = 0.1) {       
    df2_fun <- function(i) f2(i - d, a) - f2(i + d, a)
    
    
    dhalf = df2_fun(0)    
    dx    = df2_fun(x)
    
    return(dx / dhalf)
}