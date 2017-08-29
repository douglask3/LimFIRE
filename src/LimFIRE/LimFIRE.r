LimFIRE <- function(w, omega_live, omega_dead,
                    Lig, pas, crop, popdens, maxFire,
                    w0, kw, M, omega0, komega, P, D, ig0, kig, H, s0, ks, 
                    fireOnly = FALSE, sensitivity = FALSE,
                    just_measures = FALSE, normalise = FALSE) {
    
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
    
    fuel       = Log0(w)
    moisture   = (omega_live + M * omega_dead) / (1 + M)
    ignitions  = Lig + P * pas + D * popdens
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
    Fire = maxFire * Fuel * Moisture * Ignitions * Supression
    
    if (fireOnly) return(Fire)
	browser()
	if (normalise) {
		 Fuel = Fuel/FUN.fuel      (0      , w0    , -kw    )
		 Ignitions = Ignitions/FUN.ignitions      (0       , ig0   , kig   )
		 Moisture = Moisture/FUN.moisture  (0  , omega0, komega)
		 Supression = Supression/FUN.supression  (0  , s0    , ks    )
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
