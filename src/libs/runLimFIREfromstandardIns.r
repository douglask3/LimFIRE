runLimFIREfromstandardIns <- function(fireOnly = FALSE, remove = NULL, 
                                      ...) {
    
    
    Obs = lapply(drive_fname, stack)
    if (!is.null(remove)) for (i in remove) Obs[[i]][] = 0
    
    params = read.csv(coefficants_file)[,-1]
    params = apply(as.matrix(params),2, mean)
        
    param <- function(p) {
        param = params[p]
        if (is.na(param)) {
            if (p == 'L') param = 1 else param = 0
        }
        return(param)
    }
    
    runMonthly <- function(i) {
        cat("simulating fire for month ", i, "\n")
        LimFIRE(Obs[["npp"   ]][[i]],
                Obs[["alpha" ]][[i]], Obs[["emc"    ]][[i]], 1, 
                Obs[["Lightn"]][[i]], Obs[["pas"    ]][[i]],
                Obs[["crop"  ]][[i]], Obs[["popdens"]][[i]],
                            param('f1'),  param('f2'),  
                param('M'), param('m1'),  param('m2'),  
                param('L'), param('H' ),  param('A' ),
                            param('i1'),  param('i2'),  
                param('P'), param('s1'),  param('s2'), fireOnly, ...)
    }
    if (fireOnly) return(layer.apply(1:nlayers(Obs[[1]]), runMonthly))
    mod = runMonthly(1)
    
    Fire = mod[[1]]
    Fuel = mod[[2]]
    Moisture = mod[[3]]
    Ignitions = mod[[4]]
    Supression = mod[[5]]
    
    for (i in 2:nlayers(Obs[[1]])) {
        mod = runMonthly(i)
        Fire = addLayer(Fire, mod[[1]])
        Fuel = addLayer(Fuel, mod[[2]])
        Moisture = addLayer(Moisture, mod[[3]])
        Ignitions = addLayer(Ignitions, mod[[4]])
        Supression = addLayer(Supression, mod[[5]])
    }
    return(list(Fire, Fuel, Moisture, Ignitions, Supression))
}
