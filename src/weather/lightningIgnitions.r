lightningIgnitions <- function(L, Pwet = NULL) {
    CG     = CGlightn(L)
    
    if (!is.null(Pwet)) CG = dryLightn(CG, Pwet)
    return(CG)
}

CGlightn <- function(L) {
    F = L^(-0.4180)
    F = 0.0408 * F
    F[F > 1] = 1
    F = L * F
    return(F)
}


dryLightn <- function(CG, Pwet) 0.8533 * CG * exp(-2.835 * Pwet)

