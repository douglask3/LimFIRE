graphics.off.gitWatermark <- function (...) {
    while ((which <- dev.cur()) != 1) dev.off.gitWatermark(which, ...)
    invisible()
}
