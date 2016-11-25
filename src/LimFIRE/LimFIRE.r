LimFIRE <- function(fuel, moisture_live, moisture_dead,
                    lightning, human_ignitions, 
                    agriculture, popdens,
                    f1, f2, M, m1, m2, H, i1, i2, P, s1, s2, 
                    fireOnly = FALSE, sensitivity = FALSE) {
    
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
    
    moisture   = (moisture_live + M * moisture_dead) / (1 + M)
    ignitions  = (lightning + H * human_ignitions) / (1 + H)
    supression = (agriculture + P * popdens) / (1 + P)
    
    Fuel       = FUN.fuel      (fuel      , f1, f2)
    Moisture   = FUN.moisture  (moisture  , m1, m2)
    Ignitions  = FUN.ignitions (ignitions , i1, i2)
    Supression = FUN.supression(supression, s1, s2)

    Fire = (1 - Fuel) * (1 - Moisture) * (1 - Ignitions) * (1 - Supression)
    
    if (fireOnly) return(Fire)
    return(list(Fire, Fuel, Moisture, Ignitions, Supression))
}

LimFIRE.fuel       <- function(...) 1-f1(...)
LimFIRE.moisture   <- function(...)   f1(...)
LimFIRE.ignitions  <- function(...) 1-f2(...)
LimFIRE.supression <- function(...)   f1(...)

dLimFIRE.fuel       <- function(...) 1-df1(...)
dLimFIRE.moisture   <- function(...)   df1(...)
dLimFIRE.ignitions  <- function(...) 1-df2(...)
dLimFIRE.supression <- function(...)   df1(...)
