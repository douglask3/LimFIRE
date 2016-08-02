LimFIRE <- function(fuel, moisture_live, moisture_dead,
                    lightning, human_ignitions,
                    agriculture, popdens,
                    f1, f2, M, m1, m2, H, i1, i2, P, s1, s2, fireOnly = FALSE) {

    moisture   = (moisture_live + M * moisture_dead) / (1 + M)
    ignitions  = (lightning + H * human_ignitions) / (1 + H)
    supression = (agriculture + P * popdens) / (1 + P)
    
    Fuel       = LimFIRE.fuel      (fuel      , f1, f2)
    Moisture   = LimFIRE.moisture  (moisture  , m1, m2)
    Ignitions  = LimFIRE.ignitions (ignitions , i1, i2)
    Supression = LimFIRE.supression(supression, s1, s2)
    
    
    Fire = (1 - Fuel) * (1 - Moisture) * (1 - Ignitions) * (1 - Supression)
    
    if (fireOnly) return(Fire)
    return(list(Fire, Fuel, Moisture, Ignitions, Supression))
}

LimFIRE.fuel       <- function(...) 1-f1(...)
LimFIRE.moisture   <- function(...)   f1(...)
LimFIRE.ignitions  <- function(...) 1-f2(...)
LimFIRE.supression <- function(...)   f1(...)
