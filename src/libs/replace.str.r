replace.str <- function(x, fnd, rpl) {
    x = paste(x, '')
    x = strsplit(x, fnd)[[1]]
    x = paste(x, collapse = rpl)
    x = substr(x, 1, nchar(x) - 1)
    return(x)    
}