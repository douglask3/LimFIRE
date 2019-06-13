graphics.off()

blues9  = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", 
            "#4292C6", "#2171B5", "#08519C", "#08306B")
reds9   = c("#FFF7FB", "#F7DEEB", "#EFC6DB", "#E19ECA", "#D66BAE", 
            "#C64292", "#B52171", "#9C0851", "#6B0830")
greens9 = c("#FBFFF7", "#EBF7DE", "#DBEFC6", "#CAE19E", "#AED66B", 
            "#92C642", "#71B521", "#519C08", "#306B08")
source("control4contol.r")
controls = runType(standardLimitation, "controls", fileLayer = 1)

fnames = fnames = c('nnfire', 'fuel', 'moisture', 'igntions', 'supression')
fnames_mod  = paste('temp/', fnames    , '-measuresOnly.nc', sep = '')	  
measures = runIfNoFile(fnames_mod, runLimFIREfromstandardIns,
                       just_measures = TRUE, raw = TRUE, test = grab_cache)[-1]

fuel_cont = mean(measures[[1]])
moisture  = mean(measures[[2]])
moisture = moisture^1.3
params = read.csv(coefficants_file)

y = standard[[1]][[1]] * (standard[[3]][[1]])

plot_xy <- function(x, y, cols = blues9, flip = FALSE, normalise = TRUE, fitLine = NULL) {
    mask = !is.na(x) & !is.na(y)
    x = x[mask] * 100; y = y[mask] * 100
    xlim = c(0, 100); ylim = c(0,100)
    if (normalise) {
        cuts = cut_results(x, seq(0, 100, 1))
        cuts_n = unique(cuts)
        probs = 1/sapply(cuts_n, function(i) sum(i == cuts))
        prob = sapply(cuts, function(i) probs[which(i == cuts_n)])

        index = sample(1:length(x), length(x), replace = TRUE, prob = prob)
        #x =  x[index]
        #y =  y[index]
    }
    if (flip) {
        c(x, y) := list(y, x)
        ylim = range(y)
        xlim = c(0, 100)
    } else if (normalise) {
        xlim = range(x)
        ylim = c(0,100)
    } else {
        xlim = range(x)
        ylim = range(y)
    }
    #cols = blues9
    cols = cols[unlist(mapply(rep, 1:9, 9 + (1:9)^5))]
    #
    cols = densCols(x,y, colramp = colorRampPalette(cols))
    plot(y~x, pch = 20, col = cols, 
         xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim)
    
    if (is.null(fitLine)) {
        fit = lm(y~x)
        y_fit = predict(fit, newdata = list(x = xlim))
        #lines(xlim, y_fit)
        R2 = cor(x, y)^2
        mtext.units(side = 3, adj = 0.9, line = -1.5, paste("~R2~:", round(R2,2)))
    } else {
        xs = seq(0, 1, 0.01)
        x0 = params[,fitLine[[2]]]#[1:200]
        k  = params[,fitLine[[3]]]#[1:200]
        if(flip) {
            k = -k
            x0 = x0 + 0.05
        }
        lines = mapply(function(...) fitLine[[1]](xs, ...), x0, k)
    
        mn  = apply(lines, 1, mean)
        q99 = apply(lines, 1, quantile, 1)
        q01 = apply(lines, 1, quantile, 0)

        Lines <- function(x, y, ...) {
            if (flip) c(x, y) := list(y, x)
            lines(x * 100, y * 100, ...)
        }
        xs = xs
        Lines(xs, mn)
        Lines(xs, q99, lty = 2)
        Lines(xs, q01, lty = 2)   
    }
}

png('figs/fuel_vs_moisture_scatters.png', height = 7, width = 7, res = 300, units = 'in')
    par(mfrow = c(2,2), mar = rep(0.5, 4), oma = c(4,4,0,0))

    ### fuel
    plot_xy(fuel_cont, y, cols = greens9, fitLine = list(LimFIRE.fuel, "fuel_x0", "fuel_k"))
    axis(2)
    mtext.units(side = 2, 'Burnt area (% ~month-1~)', line = 2.5)
    mtext(side = 3, 'a)', adj = 0.1, line = -1.5)

    ### blank
    plot.new()
    
    ### both
    plot_xy(moisture, fuel_cont, cols = reds9, normalise = FALSE)
    mtext(side = 3, 'b)', adj = 0.1, line = -1.5)
    axis(1)
    axis(2)
    mtext(side = 2, 'Moisture (%)', line = 2.5)
    mtext(side = 1, 'Fuel continuity (%)', line = 2.5)

    ### moisture
    plot_xy(moisture, y, blues9, flip = TRUE, 
            fitLine = list(LimFIRE.moisture, "moisture_x0", "moisture_k"))
    axis(1)
    mtext(side = 3, 'c)', adj = 0.1, line = -1.5)
    mtext.units(side = 1, 'Burnt area (% ~month-1~)', line = 2.5)
dev.off()



