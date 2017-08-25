source('cfg.r')
graphics.off()

fig_fnames = 'figs/residyals.png'

mod_files = paste(outputs_dir, '/LimFIRE_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  '-lm.nc',
                  sep = '')


vars = list('npp', c('alpha', 'emc'), c('Lightn', 'pas'), 
            c('crop', 'popdens'))

params = list(NULL, 'M', 'H', 'P')

lm_mod = runIfNoFile(mod_files, runLimFIREfromstandardIns)#[-1]
fire1 =  stack(drive_fname['fire'])[[1:12]]
fire2 = lm_mod[[1]][[1:12]]
fire = 0.5 * fire1 + 0.5 * fire2
lm_mod = lm_mod[-1]

plot_residuals <- function(i, vs, ps) {
    index = (1:length(lm_mod))[-i]
    
    resid = fire[[1:12]]
    for (j in index)
        resid = resid / (1 - lm_mod[[j]][[1:12]])
    
    resid[resid>1]=1
    
    vs = mapply(function(i,j) {i = stack(i)[[1:12]]; j * i},
                drive_fname[vs], c(1, param(ps, mean)))
    
    smp =  sample(1:length(vs[[1]]), 100000)
    
    vs = vs[[1]][smp];  resid = resid[smp]
    browser()
}

mapply(plot_residuals, 1:length(lm_mod), vars, params)