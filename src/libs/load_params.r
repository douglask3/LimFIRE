param <- function(p, FUN = median, ..., pline = NULL, nline = NULL) {
	if (p == 'mxD1') return(NULL)
	if (p == 'mxD2') return(NULL)
    
    params = read.csv(coefficants_file, stringsAsFactors = FALSE)#[,-1]
    test = !is.na(as.numeric(params[,2]))
    params = params[test,]
    params = apply(params, 2, as.numeric)
    
    if (p == "fuel_pg" && !any(grepl(p, colnames(params)))) return(0)
    
    if (is.null(pline) && is.null(nline))  { 
	params_mean = apply(as.matrix(params),2, FUN, ...)
    } else {
	if (is.null(nline)) nline = round(dim(params)[1] * pline)
	if (nline < 1 ) nline = 1
	params_mean =  params[nline,]
    }
    	
    if (class(params_mean) == "numeric")
        params_mean = t(params_mean)
    
    nps = nrow(params_mean)
   
    fillParam <- function(pp) {
        param =  try(params_mean[, pp], silent = TRUE)
        if (class(param) == "try-error") {
            if (pp == 'L') param = rep(1, nps) else param = rep(0, nps)
        }
        return(param)
    }
    params = sapply(p, fillParam)
    
    if (class(params) == "numeric") names(params) = p
        else colnames(params) = p
    return(params)
}

return_all <- function(x) return(x)

paramSample <- function(p, n = 100, ...) param(p, sample, n, FALSE)
