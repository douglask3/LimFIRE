fire_season <- function() {
    fire = stack(drive_fname['fire'])
    find_season <- function(i) {
        index = seq((i - 1) * 12 + 1, i * 12)
        fire_yr = fire[[index]]
        no_fire = sum(fire_yr) == 0
        fire_mn = which.max(fire_yr)
        fire_mn[no_fire] = NaN
        return(fire_mn)
    }
    fire_season = layer.apply(seq(1, nlayers(fire)/12), find_season)
    return(fire_season)
}