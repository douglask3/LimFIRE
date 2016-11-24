plot_4way_standard <- function(pmod,
                               x_range = c(-180,180),
                               y_range = c(-60,90), ...) {
    xy = xyFromCell(pmod[[1]], 1:length(pmod[[1]]))
    pmod = lapply(pmod, values)
    plot_4way(xy[,1], xy[,2], pmod[[3]], pmod[[1]], pmod[[2]], pmod[[4]],
              x_range = x_range, y_range = y_range,
              cols=rev(c("FF","CC","99","55","11")),
              coast.lwd=par("lwd"),
              add_legend=FALSE, smooth_image=FALSE,smooth_factor=5, ...)

}
