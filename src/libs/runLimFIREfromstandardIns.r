openAllObs <- function(replace = NULL) {
	Obs = lapply(drive_fname, stack)
	if (!is.null(replace)) {
		if (length(replace) == length(Obs)) Obs = replace
		else Obs[[replace[[1]]]] = replace[[2]]
	}
	return(Obs)
}

runLimFIREfromstandardIns <- function(fireOnly = FALSE, remove = NULL, sensitivity = FALSE, 
                                      mnthIndex = 1:nlayers(Obs[[1]]), raw = FALSE, pline = NULL, nline = NULL, replace = NULL,...) {
    
    Obs = openAllObs(replace)
	if (!is.null(remove)) for (i in remove) Obs[[i]][] = 0

	paramFun <- function(...) param(..., pline = pline, nline = nline)
    #mnthIndex = 1:12#
    
    runMonthly <- function(i) {
        cat("simulating fire for month ", i, "\n")
        out = LimFIRE((100 - Obs[["bare"   ]][[i]])/100, Obs[["alphaMax"]][[i]],
                      Obs[["alpha" ]][[i]], Obs[["emc"    ]][[i]], Obs[["tree"]][[i]] / 100,
                      Obs[["Lightn"]][[i]], Obs[["pas"    ]][[i]],
                      Obs[["crop"  ]][[i]], Obs[["popdens"]][[i]],
					  paramFun("max_f"),
					  paramFun("fuel_pw"), paramFun("fuel_pg"),
								   paramFun(    'fuel_x0'),  paramFun('fuel_k'    ),  
                      paramFun('cM'), paramFun('cMT'),
								   paramFun('moisture_x0'),  -paramFun('moisture_k'),  
                      paramFun('cP'),
                      paramFun('cD1'),
                                  paramFun(   'igntions_x0'),  paramFun('igntions_k'   ),  
					  paramFun('cD2'), paramFun(    'suppression_x0'),  -paramFun('suppression_k'),
					  fireOnly, sensitivity = sensitivity, ...)
        
		if (sensitivity) {
			for (i in 2:length(out)) out[[i]] = 1 - out[[i]]
		} else if (!fireOnly & !raw) {
			mag = sum(layer.apply(out[-1], function(i) i))
			for (i in 2:length(out)) out[[i]] = out[[i]] / mag
        }
		return(out)
    }
    if (fireOnly) return(layer.apply(mnthIndex, runMonthly))
    mod = runMonthly(1)
   
    Fire = mod[[1]]
    Fuel = mod[[2]]
    Moisture = mod[[3]]
    Ignitions = mod[[4]]
    Supression = mod[[5]]
    for (i in mnthIndex[-1]) {
        mod = runMonthly(i)
        Fire = addLayer(Fire, mod[[1]])
        Fuel = addLayer(Fuel, mod[[2]])
        Moisture = addLayer(Moisture, mod[[3]])
        Ignitions = addLayer(Ignitions, mod[[4]])
        Supression = addLayer(Supression, mod[[5]])
    }
	
    return(list(Fire, Fuel, Moisture, Ignitions, Supression))
}
