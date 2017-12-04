LimFIRE <- function(w, wSeason, omega_live, omega_dead, Tree, 
				    Lig, pas, crop, popdens, 
					maxF = 1.0, fp = 1.0, S, w0, kw, M, Mt, omega0, komega, P, D, ig0, kig, H, s0, ks, 
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
    
    fuel       = ((w + S * (1 - wSeason)) / (1 + S))^fp
    moisture   = (omega_live + M * omega_dead + Mt * Tree) / (1 + M + Mt)
    ignitions  =  Lig + P * pas + D * popdens
    supression = (crop + H * popdens)
    
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
    Fire = maxF * Fuel * Moisture * Ignitions * Supression
    browser()
    if (fireOnly) return(Fire)
	
	if (normalise) {
		 Fuel0 = FUN.fuel      (0      , w0    , kw    )
		 Fuel = (Fuel - Fuel0)/(1-Fuel0)
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
