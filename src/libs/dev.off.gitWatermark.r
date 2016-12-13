dev.off.gitWatermark <- function(...) {
	if (!exists('clearWaterMark') || !clearWaterMark) gitWatermark(...)
    dev.off()
}