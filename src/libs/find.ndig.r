find.ndig <- function(x) {
	i = 0
	if (x > 1) {
		while (x > 1) {x = x / 10; i = i + 1}
	} else {
		while (x < 1) {x = x * 10; i = i - 1}
	}
	return(i)
}