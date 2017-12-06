convert2Num <- function(r, coords = NULL, arear = NULL, mn = TRUE) {
	if (!is.null(coords)) r =  crop(r, coords)
	if ( is.null(arear)) arear = area(r)
	r = sum(values(r * arear),na.rm = TRUE)
	if (mn) r = r /sum(values(arear), na.rm = TRUE)
	return(r)
}

convert2Lines <- function(r, coords = NULL, ...) {
	arear = area(r[[1]])
	if (!is.null(coords)) arear = crop(arear, coords)
	r = layer.apply(r, convert2Num, coords = coords, arear = arear, ...)
	return(unlist(r))
}