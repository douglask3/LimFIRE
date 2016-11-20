source('cfg.r')
cntr = nls.control(warnOnly = TRUE, maxiter = 100)

inter_file_name = 'temp/driving_data.csv'


start_params = list(         w0     =  1000  , kw     =    1,
                    M =   1, omega0 =     0.3, komega =    5,
                    P =   1, ig0    =     0  , kig    =  100,
                    H =   1, s0     =    50  , ks     =   50)
                    
lower_params = list(         w0     =     0  , kw     =    0.0001,
                    M =   0, omega0 =     0.1, komega =    0,
                    P =   0, ig1    =   -10  , kig    =    0,
                    H =   0, s0     =     0  , ks     =    0)
                    
upper_params = list(         w0     = 10000  , kw     =  9E9,
                    M = 9E9, omega0 =     0.5, komega =  9E9,
                    P = 9E9, ig0    =   10   , kig    =  9E9,
                    H = 9E9, s0     =   100  , ks     =  9E9)
                    
                    
Obs = ObsRasters2DataFrame()


nls_bootstrap <- function() {
    index = sample(1:ncells, 100000, replace = FALSE)
    dat = Obs[index, ]
    
    res = nls(fire ~ LimFIRE(npp, alpha, emc, Lightn, pas, crop, popdens,
                             w0, kw, M, omega0, komega, 
                             P, ig0, kig, H, s0, ks,
                             fireOnly = TRUE), 
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
