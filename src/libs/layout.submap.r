layout.submap <- function(mat, widths = rep.int(1, ncol(mat)),
       heights = rep.int(1, nrow(mat)), csize = 22, skip = c(), ...) {
	
	mat  =  mat * 2 - 1
	skip = skip * 2 - 1
	
	mat[mat < 0] = 0.0
	mat0 = mat
	mat  = apply(mat, 2, function(i) rep(i, each = csize))
	mat  = apply(t(mat), 2, function(i) rep(i, each = csize))
	mat  = t(mat)
	
	addSubmap <- function(i) {
		if (i == 0 || any(skip == i)) return(mat)		
		index = which(mat == i)		
		if (length(index) == 0) return(mat)
		ncol = sum(apply(mat, 2, function(j) any(j == i)))
		nrow = sum(apply(mat, 1, function(j) any(j == i)))
		#if (length(index) == 32) browser()
		index = matrix(index, ncol = ncol)
		
		index = index[-c((1:(1 + nrow/2))), 1:(6 * ncol/csize)]
		
		mat[as.vector(index)] = i + 1
		return(mat)
	}
	
	for (i in unique(as.vector(mat)))
		mat = addSubmap(i)
	
	mati = mat
	ids =  sort(unique(as.vector(mat)))
	for (i in 1:length(ids)) mat[mati == ids[i]] = i
	if (any(mati == 0)) mat = mat - 1
	
	heights = rep(heights, each = csize)
	widths  = rep(widths , each = csize)
	
	layout(mat, widths = widths, heights = heights, ...)
}