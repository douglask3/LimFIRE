source('cfg.r')
cntr = nls.control(warnOnly = TRUE)

inter_file_name = 'temp/driving_data.csv'

start_params = list(         f1 = 100 , f2 = 1/200,
                    M = 1  , m1 = 10  , m2 = 0.1  ,
                    H = 1  , i1 = 1   , i2 = 1    ,
                    P = 1  , s1 = 1   , s2 = 0.01 )
nout = 0.000001
                    
lower_params = list(         f1 = nout, f2 = nout,
                    M = 0  , m1 = nout, m2 = nout,
                    H = 0  , i1 = 0.4 , i2 = nout,
                    P = 0  , s1 = nout, s2 = nout)
                    
upper_params = list(         f1 = 9E9 , f2 = 10   ,
                    M = 9E9, m1 = 9E9 , m2 = 10   ,
                    H = 9E9, i1 = 9E9 , i2 = 9E9  ,
                    P = 9E9, s1 = 9E9 , s2 = 10   )
  
Obs = ObsRasters2DataFrame()



nls_bootstrap <- function() {
    index = sample(1:ncells, 100000, replace = FALSE)
    dat = Obs[index, ]
    res = nls( fire ~ LimFIRE(npp, alpha, emc, Lightn, pas, crop, popdens,
                        f1, f2, M, m1, m2, H, i1, i2, P, s1, s2, fireOnly = TRUE), 
                data = dat, algorithm = "port",
                start = start_params, lower = lower_params, upper = upper_params,
                trace = TRUE, control = cntr)

    return(coefficients(res))
}

nboots = 1
ncells = dim(Obs)[1]
resStore = c()

for (i in 1:100) {
    nboots = nboots + 1
    res = nls_bootstrap()
    resStore = rbind(resStore, res)
}

write.csv( resStore, file = coefficants_file)             
