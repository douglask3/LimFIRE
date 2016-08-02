f1 <- function(x, a, b) {
    f0 = f1A(0, a, b)
    fi = f1A(x, a, b)
    f  = (fi - f0)/(1-f0)
    f[is.na(f)] = 0.0
    return(f)
}


f1A <- function(x, a, b) 1/(1 + a * exp(-b * x))



f2 <- function(x, a, b) {
    x = a + x * b
    return(x / (x + 1))
}
