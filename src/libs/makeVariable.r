makeVariable <- function(FUN, vname, nyears) {
    VAR = layer.apply(1:(12*nyears), FUN)
    writeRaster.gitInfo.time(VAR, drive_fname[vname], overwrite = TRUE)
    return(VAR)
}
