source("cfg.r")
graphics.off()
params = read.csv(coefficants_file)

ps = c("fuel_x0", "fuel_k", "fuel_pw", "fuel_pg",   
"moisture_x0", "moisture_k", "cM", "cMT",
"igntions_x0", "igntions_k", "cP", "cD1",
"suppression_x0", "suppression_k", "cD2",
"max_f_interval_", "sigma")   

names_params = list(expression(W[0]), expression(k[W]), 'p', expression(v[W]),
                    expression(omega[0]), expression(k[omega]), expression(v[EMC]), expression(v[Tree]),
                    expression(ig[0]), expression(k[ig]), expression(v[pas]), expression(v[popDen_ig]),
                    expression(S[0]), expression(k[S]), expression(v[popDen_S]), 
                    expression(F[max]), expression(sigma))
                                       
params = params[ps]

plot_param_pairs <- function(j, i) {
    cat(i, "\t", j, "\n")
    if (i == j) {
        x = params[,i]
        hist(x, 100, xlab = '', ylab = '', yaxt = 'n', main = '')
        mtext(names_params[[i]], line = -1, font = 2)
    } else {
        x = params[,i]; y = params[,j]
        cols = blues9[1:7]
        cols = densCols(x, y, colramp = colorRampPalette(cols))
        plot(y ~ x, pch = 20, cex = 2, col = cols, axes = FALSE, xlab = '', ylab = '')
        mtext(side = 3, line = -1, adj = 0.9, round(cor(x,y)^2,2))
        if (i == 1) axis(2) else axis(2, at = c(-9E9, 9E9), xpd = FALSE)
        if (j == length(ps)) axis(1) else axis(1, at = c(-9E9, 9E9), xpd = FALSE)
    }
}

png('figs/param_corr.png', height = 1.5 * length(ps), width = 2 * length(ps), unit = 'in', res = 300)
    par(mfcol = rep(length(ps), 2), mar = rep(1.5, 4), oma = c(4, 4, 1, 1))
    index = 1:length(ps)
    lapply(index, function(i) lapply(index,plot_param_pairs, i)) 
dev.off()