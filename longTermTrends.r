source("cfg.r")
## AVHRR
#ftp://anon-ftp.ceda.ac.uk/neodc/esacci/fire/data/burned_area/AVHRR-LTDR/grid/v1.1/1982/19820101-ESACCI-L4_FIRE-BA-AVHRR-LTDR-fv1.1.nc

## MODIS
#http://dap.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/grid/v5.1/2001/20010101-ESACCI-L4_FIRE-BA-MODIS-fv5.1.nc

urlS = rev(c("ftp://anon-ftp.ceda.ac.uk/neodc/esacci/fire/data/burned_area/AVHRR-LTDR/grid/v1.1/",
         "http://dap.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/grid/v5.1/"))

urlE = c("01-ESACCI-L4_FIRE-BA-MODIS-fv5.1.nc",
        "01-ESACCI-L4_FIRE-BA-AVHRR-LTDR-fv1.1.nc")
urlD = c("modis", "avhrr")

yrS = c(2001, 1982, 2001, 2001)
yrE = c(2019, 2018, 2018, 2018)
yrSkip = list(c(), 1994, c(), c())

aaLimits = c(0, 1, 2, 5, 10, 20, 50)
anomLimits1 = c(-2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2)
anomLimits2 = c(1.1, 1.2, 1.5, 2, 5)
anomLimits2 = c(rev(1/anomLimits2), anomLimits2)
aaCols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
anomCols = rev(c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))


whenCols = c("#161843", "#FFFF00", "#a50026")
FUN <- function(urlS, urlE, urlD, yrS, yrE, yrSkip) {
    #if (yrS == 1982) return()
    openYrMon <- function(mn, yr) {
        if (mn < 10) mn = paste0(0, mn)
        print(yr)
        print(mn)
        print("====")
        
        if (any(yrSkip == yr)) yr = yr-1
        url = paste0(urlS, yr, "/", yr, mn, urlE)
        dest = paste0("data/CCI_long_term/burntArea-", urlD, '-', yr, "-", mn, ".nc")

        if (!file.exists(dest) ) {
            
            dwn = try(download.file(url, dest))
            if (class(dwn) == "try-error") {
                url = paste0(urlS, yr, "/new-corrected/", yr, mn, urlE)
                dwn = try(download.file(url, dest))
                if (class(dwn) == "try-error") {
                    try(file.remove(dest))
                    if (yr == yrE) return(invisible())
                    else stop()
                }
            }
        }
        raster(dest, varname = "burned_area")
    }    
    
    dat = layer.apply(yrS:yrE, function(yr) layer.apply(1:12, openYrMon, yr))
    
    whenLimits = seq(12, nlayers(dat), by = 12)
    addUpYear <- function(yr) {
        emn = nlayers(dat) - (yr - 1)*12
        smn = nlayers(dat) - yr*12 + 1
        mean(dat[[smn:emn]])
    }
    adat = layer.apply(floor(nlayers(dat)/12):1, addUpYear)
    #adat =  layer.apply(yrS:yrE, function(yr)
    #                    mean(layer.apply(1:12, openYrMon, yr)))

    darea = (100^2*raster::area(dat))
    
    aadat = 12*mean(adat)/darea
    aadat5 = 12*mean(adat[[(nlayers(adat)-4):nlayers(adat)]])/darea
    
    maxDat = mean(dat[[1:3]])
    wmxDat = maxDat
    wmxDat[] = NaN
    for (mn in 3:(nlayers(dat)-1)) {
        print(mn)
        dati = mean(dat[[(mn-1):(mn+1)]])
        test = dati > maxDat
        maxDat[test] = dati[test]
        wmxDat[test] = mn
    }  
    
    #if (nlayers(dat) == 228)  {
    #    dati = mean(dat[[c(227, 228, 228)]])
    #    test = dati > maxDat
    #    maxDat[test] = dati[test]
    #    wmxDat[test] = mn
    #}
    
    plotFun <- function(r,  limits, cols, title = '', addLeg = FALSE, ...){
        print(title)
        plotStandardMap(r, '', limits, cols = cols, add_legend = FALSE)
        mtext(side = 3, adj = 0.1, title, line = -0.5)
        if (addLeg) 
            standard_legend(cols, limits, r, add = T, ylabposScling = 0.8,
                            plot_loc = c(0.35, 0.85, 0.05, 0.1), ...)

        title = gsub(' ', '_', title)        
        title = gsub('OVER', '/', title)
        file = paste0("outputs/longTermRecord_", urlD, '--', , ".nc") 
        writeRaster.gitInfo(r, filename = file, overwrite = TRUE)
    }
    graphics.off()
    png(paste0("figs/longTermRecord_", urlD, '_', yrS, '-', yrE, ".png"),
        height = 5.1*1.3, width = 7.2*1.3, units = 'in', res = 300)
        par(mar = c(2, 0, 0, 0), mfcol = c(3,2), oma = rep(1, 4))
        
        
        mnEnd = nlayers(dat) - floor(nlayers(dat)/12)*12    
        if (mnEnd != 12) {
            mnStr = paste(month.abb[mnEnd+ 1], '')
            ny = 5
        } else {
            mnStr = paste(month.abb[1], '')
            ny = 4
        } 
        if (mnEnd == 0) 
            mnEnd = paste(month.abb[12]   , '')
        else 
            mnEnd = paste(month.abb[mnEnd]   , '')       
        
        plotFun(aadat, aaLimits, aaCols, paste0(mnStr, yrS, '-', mnEnd, yrE, ' Annual average'))
        plotFun(aadat5,aaLimits, aaCols, paste0(mnStr, yrE-ny, '-', mnEnd, yrE, ' Annual average'))
        plotFun(maxDat/darea,aaLimits, aaCols,
                paste0('Max monthly burnt area', mnStr, yrS, '-', mnEnd, yrE),
                TRUE, extend_max = TRUE, units = '%')

        plotFun(aadat5-aadat, anomLimits1, anomCols,
                paste0(mnStr, yrE - ny, '-', mnEnd, yrE, ' - ', mnStr, yrS, '-', mnEnd, yrE,
                       ' Annual average'),
                TRUE, extend_max = TRUE, extend_min = TRUE, units = '%', oneSideLabels = FALSE)

        plotFun(aadat5/aadat, round(anomLimits2, 1), anomCols,
                paste0(mnStr, yrE - ny, '-', mnEnd, yrE, ' / ', mnStr, yrS, '-', mnEnd, yrE,
                       ' Annual average'),
                TRUE, extend_max = TRUE, oneSideLabels = FALSE)
        
        labelss = (yrS-1900):(yrE-1900)
        test = which(labelss >= 100)[1:3]
        labelss[test] = labelss[test] + 1900    
        
        test = which(labelss > 100)[-(1:3)]
        labelss[test] = labelss[test] - 100
        
        test = labelss < 10
        labelss[test] = paste0('0', labelss[test])
        labelss[-seq(1, length(labelss), by = 3)] = ''
    
        if (labelss[1] > 10 && labelss[1] < 2000) labelss[1] = paste0(19, labelss[1])
        test = nchar(labelss) == 2
        labelss[test] = paste0("`", labelss[test])
        plotFun(wmxDat, seq(12, nlayers(dat)-1, by = 12), cols = whenCols,
                'Month of max. burnt area',
                TRUE, labelss = labelss, oneSideLabels = FALSE)
        
    dev.off()
    
}

mapply(FUN, urlS, urlE, urlD, yrS, yrE, yrSkip)
