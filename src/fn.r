f1 <- function(x, a, b) 1/(1 + a * exp(-b * x))

f2 <- function(x, a) {
    x = x * a
    return(x / (x + 1))
}
