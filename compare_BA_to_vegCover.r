source("cfg.r")

reds6 = c("#FFF7FB", "#EFC6DB", "#D66BAE", "#B52171",
           "#6B0830", '#210203')
#reds9 = c("#FFFBF7", "#F7EBDE", "#EFDBC6", "#E1CA9E", "#D6AE6B", "#C69242", "#B57121",
#          "#9C5108", "#6B3008")

dat = sapply(drive_fname[c('bare', 'fire', 'alphaMax')], brick)

mask = layer.apply(dat, function(i) is.na(i[[1]]))
mask = !any(mask)
mask = sample(which(mask[]), size = 5000, replace = FALSE)

vdat = sapply(dat, function(i) as.vector(i[mask]))
vdat = cbind(vdat, vbiome = rep(biomeAssigned[mask], 168))

mask2 = vdat[,2] > 0 & !is.na(vdat[,4])
vdat = vdat[mask2,]
vdat[,2] = vdat[,2] * 12 * 100
vdat[,1] = 100 - vdat[,1]

p = 3
x = vdat[,1]^p
y = vdat[,2]
plotTrScatter <- function(cols, rep = 1, maxCEX = 2, ...) {
    plot(x, y, xaxt = 'n', xaxs = 'i', yaxs = 'i', xlim = c(0, 100^p), 
         xlab = '', ylab = '', cex = maxCEX,  ...)
    for (j in 1:rep)  for (cex in c(maxCEX, 1, 0.5, 1/4, 1/8, 1/16, 1/32)) 
        points(x, y, col = cols, pch = 19, cex  = cex)
}
png('figs/alphaMax_justification.png', height = 9, width = 6, units = 'in', res = 300)
    layout(rbind(1, 2, 3, 4), height = c(1, 0.2, 1, 0.2))
    par(mar = rep(1, 4), oma = c(3,3,0,0))
    cols = biomesCols[vdat[,4]] 
    cols = make.transparent(cols, 0.9)
    plotTrScatter(cols)
    mtext('a) biomes', adj = 0.1, line = -1.5)

    plot.new()
    txt = sapply(strsplit(names(biomesCols)[-1], '\n'), paste, collapse = '')
    legend('left', txt, pch = 19, 
           col = biomesCols[-1], ncol = 3, xpd = NA, bty = 'n')

    limits = quantile(vdat[,3], seq(0, 1, length.out = 10))[2:9]
    limits = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4)
    cuts   = cut_results(vdat[,3], limits)
    cols0  =  make_col_vector(reds6, ncols = length(limits)+1)
    cols   = make.transparent(cols0[cuts], 0.9)

    plotTrScatter(cols, 4, 1)
    mtext(expression(paste('b) ', alpha[max]/alpha)), adj = 0.1, line = -1.8)

    labels = c(30, 50, 60, 70, 75, 80, 85, 90, 95, 99)
    axis(1, at = labels^p, labels)
    mtext(side = 1, 'VCF total vegetation cover (%)', line = 2.5)
    mtext(outer = TRUE, side = 2, 'GFED4s burnt area (%)', line = 1.5)
    par(mar = c(0, 0, 2, 0))
    standard_legend(cols = reds6, lims = limits, add = FALSE, extend_max = TRUE, dat =vdat[,3],
                    plot_loc = c(0.35,0.75,0.1,0.3))
dev.off()
#cols = unique(unlist(lapply(1:6,function(i) make_col_vector(rev(reds9)[i:(i+1)], ncols = 8-i))))
#cols = unlist(lapply(1:8,function(i) rep(rev(reds9)[i], 9-i)))
#cols = densCols(x, y, colramp = colorRampPalette(rev(cols)))
#plot(y ~ x, pch = 20, cex = 2, col = cols, xlab = '', ylab = '')
