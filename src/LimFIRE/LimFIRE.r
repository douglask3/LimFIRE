LimFIRE <- function(w, omega_live, omega_dead,
                    Lig, pas, crop, popdens,
                    w0, kw, M, omega0, komega, P, D, ig0, kig, H, s0, ks, 
                    fireOnly = FALSE, sensitivity = FALSE,
                    just_measures = FALSE) {
    
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
    
    fuel       = w
    moisture   = (omega_live + M * omega_dead) / (1 + M)
    ignitions  = Lig + P * pas + D * popdens   / (1 + P + D)
    supression = (crop + H * popdens)          / (1 + H)
    
    if (just_measures) {
        fire = fuel
        fire[] = 0.0
        return(list(fire, fuel, moisture, ignitions, supression))
    }
    
    Fuel       = FUN.fuel      (fuel      , w0    , kw    )
    Moisture   = FUN.moisture  (moisture  , omega0, komega)
    Ignitions  = FUN.ignitions (ignitions , ig0   , kig   )
    Supression = FUN.supression(supression, s0    , ks    )
    
    Fire = Fuel* Moisture * Ignitions * Supression
    
    if (fireOnly) return(Fire)
    return(list(Fire, Fuel, Moisture, Ignitions, Supression))
}

LimFIRE.fuel       <- function(...)   f1(...)
LimFIRE.moisture   <- function(...) 1-f1(...)
LimFIRE.ignitions  <- function(...)   f1(...)
LimFIRE.supression <- function(...) 1-f1(...)

dLimFIRE.fuel       <- function(...)   df1(...)
dLimFIRE.moisture   <- function(...) 1-df1(...)
dLimFIRE.ignitions  <- function(...)   df1(...)
dLimFIRE.supression <- function(...) 1-df1(...)
