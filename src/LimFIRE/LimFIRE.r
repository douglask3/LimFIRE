LimFIRE <- function(w, omega_live, omega_dead,
				    Big = 1.0, Lig, pas, crop,
					popdens, maxFire = 1.0,
                    w0, kw, M, omega0, komega, L, P, mxDi= NULL, D, ig0, kig, mxDs = NULL, H, s0, ks, 
                    fireOnly = FALSE, sensitivity = FALSE,
                    just_measures = FALSE, normalise = FALSE, add21 = FALSE) {
    
    if (sensitivity) {
        FUN.fuel       = dLimFIRE.fuel
        FUN.moisture   = dLimFIRE.moisture
        FUN.ignitions  = dLimFIRE.ignitions
        FUN.supression = dLimFIRE.supression    
    } else {
        FUN.fuel       = LimFIRE.fuel
        FUN.moisture   = LimFIRE.moisture
        FUN.ignitions  = LimFIRE.ignitions
        FUN.supression = LimFIRE.supression            
    }
	
	popdensFun <- function(x, i) x/(x+i)
	
	if (!is.null(mxDi)) popdensi = popdensFun(popdens, mxDi)
		else popdensi = popdens
	if (!is.null(mxDs)) popdenss = popdensFun(popdens, mxDs)
		else popdenss = popdens
    
    fuel       = w#Log0(w)
    moisture   = (omega_live + M * omega_dead) / (1 + M)
    if (L == 0) ignitions  =  Lig + P * pas + D * popdensi
	else ignitions  = Big + L * Lig + P * pas + D * popdensi
	ignitions = log(ignitions)
    supression = (crop + H * popdenss)
    
    if (just_measures) {
        fire = fuel
        fire[] = 0.0
        return(list(fire, fuel, moisture, ignitions, supression))
    }   
	
    Fuel       = FUN.fuel      (fuel      , w0    , kw    )
    Moisture   = FUN.moisture  (moisture  , omega0, komega)
    Ignitions  = FUN.ignitions (ignitions , ig0   , kig   )
    Supression = FUN.supression(supression, s0    , ks    )
    #if (sensitivity)  browser()
    Fire = maxFire * Fuel * Moisture * Ignitions * Supression
    
    if (fireOnly) return(Fire)
	
	if (normalise) {	
		 #Fuel = Fuel/FUN.fuel      (0      , w0    , -kw    )
		 #Ignitions = Ignitions/FUN.ignitions      (550       , ig0   , kig   )
		 Moisture = Moisture/FUN.moisture  (0  , omega0, komega)
		 Supression = Supression/FUN.supression  (0  , s0    , ks    )
	}
	
	if (add21) {
		test = Fire > 0
		FuelOut = Moisture * Ignitions * Supression / Fire
		MoistureOut = Fuel * Ignitions * Supression / Fire
		IgnitionsOut = Fuel * Moisture * Supression / Fire
		SupressionOut = Fuel * Moisture * Ignitions / Fire
		Fuel[test] = FuelOut[test]
		Moisture[test] = MoistureOut[test]
		Ignitions[test] = IgnitionsOut[test]
		Supression[test] = SupressionOut[test]
	}
    return(list(Fire, Fuel, Moisture, Ignitions, Supression))
}

LimFIRE.fuel       <- function(...)   f1(...)
LimFIRE.moisture   <- function(...)   f1(...)
LimFIRE.ignitions  <- function(...)   f1(...)
LimFIRE.supression <- function(...)   f1(...)

dLimFIRE.fuel       <- function(...)   df1(...)
dLimFIRE.moisture   <- function(...)   df1(...)
dLimFIRE.ignitions  <- function(...)   df1(...)
dLimFIRE.supression <- function(...)   df1(...)
