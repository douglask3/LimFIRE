saturate <- function(cols, factor = 0.5) {
	cols = col2rgb(cols)
	cols = rgb2hsv(cols)
	cols[2,] = cols[2,] ^ factor
	cols = hex(colorspace::HSV(cols[1,] * 360, cols[2,], cols[3,]))
	return(cols)
}