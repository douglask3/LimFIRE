strreplace <- function(x, pattern, replace)
	paste(strsplit(x, pattern)[[1]], collapse = replace)