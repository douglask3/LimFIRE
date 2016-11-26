source('cfg.r')
cntr = nls.control(warnOnly = TRUE, maxiter = 100)

inter_file_name = 'temp/driving_data.csv'


start_params = list(         w0     =     100, kw     = 1/200,
                    M =   1, omega0 =      10, komega =   0.1,
                    P =   1, ig0    =    0.05,
                    H =   1, s0     =       1, ks     =  0.01)
                    
lower_params = list(         w0     =     0  , kw     =    0,
                    M =   0, omega0 =     0  , komega =    0,
                    P =   0, ig1    =     0  ,
                    H =   0, s0     =     0  , ks     =    0)
                    
upper_params = list(         w0     = 9E9, kw     =  10,
                    M = 9E9, omega0 = 9E9, komega =  10,
                    P = 9E9, ig0    = 100,
                    H = 9E9, s0     = 9E9, ks     =  10)
                    
                    
Obs = ObsRasters2DataFrame()


nls_bootstrap <- function() {
    index = sample(1:ncells, 100000, replace = FALSE)
    dat = Obs[index, ]
    
    dat[, 'emc'] = dat[, 'emc'] * 100
    res = nls(fire ~ LimFIRE(npp, alpha, emc, Lightn, pas, crop, popdens,
                             w0, kw, M, omega0, komega, 
                             P, ig0, H, s0, ks,
                             fireOnly = TRUE, fire = fire), 
              data = dat, algorithm = "port",
              start = start_params, lower = lower_params, upper = upper_params,
              trace = TRUE, control = cntr)

    return(coefficients(res))
}

ncells = dim(Obs)[1]
resStore = c()

for (i in 1:10) {
   cat('\n==================\n',
       ' Bootstrap ', i, 
       '\n==================\n')
    res = nls_bootstrap()
    resStore = rbind(resStore, res)
}

write.csv( resStore, file = coefficants_file)             
