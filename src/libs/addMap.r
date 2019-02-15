addMap <- function (map_db, projection, x_range, y_range, regions, orientation, 
    add, ...) 
{
    if (is.null(projection)) {
        test = try(map(map_db, interior = FALSE, xlim = range(x_range), 
            ylim = range(y_range), regions = regions, mar = par("mar"), 
            add = add, ...), silent = TRUE)
    }
    else {
        test = try(map(map_db, projection = projection, interior = FALSE, 
            orientation = orientation, xlim = range(x_range), 
            ylim = range(y_range), regions = regions, add = add, ...), 
            silent = TRUE)
    }
    if (add) 
        return(TRUE)
    else return(!class(test) == "try-error")
}