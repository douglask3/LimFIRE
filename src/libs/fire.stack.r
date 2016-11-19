fire.stack <- function(x, y) {
    x = x0 = stack(x)
    x0= mean(x0)
    
    x = layer.apply(1:nlayers(y), function(i) {
        index = seq((i - 1) * 12 + 1, i * 12)
        r = x[[index]]
        out = r[[1]]
        r = values(r)
        yj = y[[i]][]
        values(out) = sapply(1:length(yj), function(j) r[j, yj[j]])
        return(out)
    })
    x = mean(x, na.rm = TRUE)
    
    x[is.na(x)] = x0[is.na(x)]
    return(x)
}
