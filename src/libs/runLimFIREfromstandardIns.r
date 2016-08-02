runLimFIREfromstandardIns <- function(fireOnly = FALSE, remove = NULL) {
    Obs = lapply(drive_fname, stack)
    params = read.csv(coefficants_file)[,-1]
    params = apply(as.matrix(params),2, mean)
    
    if (!is.null(remove)) for (i in remove) Obs[[i]][] = 0
    
    runMonthly <- function(i) {
        cat("simulation fire for month ", i, "\n")
        LimFIRE(Obs[["npp"   ]][[i]],
                Obs[["alpha" ]][[i]], Obs[["emc"    ]][[i]], 
                Obs[["Lightn"]][[i]], Obs[["pas"    ]][[i]],
                Obs[["crop"  ]][[i]], Obs[["popdens"]][[i]],
                             params['f1'],  params['f2'],  
                params['M'], params['m1'],  params['m2'],  
                params['H'], params['i1'],  params['i2'], 
                params['P'], params['s1'],  params['s2'], fireOnly)
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
