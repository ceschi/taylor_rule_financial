#### R code for inflation dynamics 
# Most of the code is an adptation
# from the script used with real data
# in the empirical section of the paper



# set non-scientific format
options("scipen"=10, "digits"=8)


##### run models file calling dynare ####

get_matlab()

run_matlab_script('./simulations_pi/models_main.m',
					verbose = F)

cat('\nModels simulation completed. Importing series and estimating AR(p*).')



##### Import simulated time series #####
sim_inflation <- data.frame(liq = readMat("./simulations_pi/nkdtc_pi_tp.mat") %>% .$pi,
						liq_notp = readMat("./simulations_pi/nkdtc_pi_notp.mat") %>% .$pi,
						nkdsge = readMat("./simulations_pi/gali_pi.mat") %>% .$pi,
            sw = readMat('./simulations_pi/pinf.mat') %>% .$pinf,
            as14 = readMat('./simulations_pi/ascardone_pi.mat') %>% .$pi
            ) %>% as.tibble()

# burn-in drop
sim_inflation <- sim_inflation[-(1:(dim(sim_inflation)[1]*.2)),]

##### set exo lags and list #####

k = 5; llags = 120;

infl <- list(names = list('Liquidity TP',
						  'Liquidity, No TP',
						  'NKDSGE',
              'Smets & Wouters 07',
              'Ascari & Sbordone 14'),
			 exolags = list(),
			 optilags = list(),
       optik = list(),
       plots = list())


## !!!! vectorise this loop and log the results !!!!

for (i in 1:ncol(sim_inflation)){
  tic(infl[['names']][[i]])

  # loop to fill up infl list

  # exogenous lags, k
   infl[['exolags']][[i]] <- lm(data = sim_inflation[,i] %>% 
                                               lagger_bis(lag=k),
                               formula = formula.maker(df = sim_inflation[,i] %>% 
                                                                     lagger_bis(lag=k),
                                                        y = sim_inflation[,i] %>% 
                                                            lagger_bis(lag=k) %>% 
                                                            names(.) %>% first())
                                ) 

  # retrieve optimal lags from tweaked urca, Bayes Info Criterion
  infl[['optilags']][[i]] <- ur.df(sim_inflation[,i] %>% as.matrix(),
                              lags = llags,
                              selectlags = 'BIC') %>% 
                            slot(., 'optilags')

  # estimate AR with optimal lags according to BIC
  infl[['optik']][[i]] <- lm(data = sim_inflation[,i] %>% lagger_bis(lag = infl[['optilags']][[i]]),
                             formula = formula.maker(df = sim_inflation[,i] %>% 
                                                                     lagger_bis(lag=infl[['optilags']][[i]]),
                                                        y = sim_inflation[,i] %>% 
                                                            lagger_bis(lag=infl[['optilags']][[i]]) %>% 
                                                           names(.) %>% first())
                                ) 
  
  df <- infl[['optik']][[i]]  %>% broom::tidy() %>% .[-(1:2),]
  df$term <- 2:(nrow(df)+1)


# stock plots for the coefficients
  infl[['plots']][[i]]  <- ggplot(data = df,
                                  aes(x = term, y = estimate)) +
                           geom_col(aes(y = p.value/10, x = term), colour = 'blue', alpha = 1, width = .1)+
                           geom_hline(aes(yintercept = 0.01/10), colour = 'blue', size = .8)+
                           geom_line(colour = 'black', size = 1) +
                           geom_line(aes(y = estimate + 2*std.error), colour = 'red') + 
                           geom_line(aes(y = estimate - 2*std.error), colour = 'red') +
                           geom_hline(aes(yintercept = 0), colour = 'black', size = .1)+
                           theme_bw() + ylab('Coefficient estimate') + xlab('Lags')+
                           ggtitle(paste0(infl[['names']][[i]], ': optimal lags estimates')) +
                           labs(subtitle = 'Point estimate is the black line, red lines are 2*SE bands, blue bars are p-values, horizontal blue line is 1% sign. threshold.')

  ggsave(filename = paste0(infl[['names']][[i]], ' optilags.pdf'),
         plot = infl[['plots']][[i]],
         device = 'pdf',
         height = 8, width = 14.16, units = 'in',
         path = file.path(graphs_dir))

  cat('\nDone with model '); toc()

  rm(df)
  gc()
}

# move plots

file.rename(from='./simulations_pi/nkdtc_notp_mp.pdf',
            to='./Plots/nkdtc_notp_mp.pdf')
file.rename(from='./simulations_pi/nkdtc_notp_tfp.pdf',
            to='./Plots/nkdtc_notp_tfp.pdf')
file.rename(from='./simulations_pi/nkdtc_tp_mp.pdf',
            to='./Plots/nkdtc_tp_mp.pdf')
file.rename(from='./simulations_pi/nkdtc_tp_tfp.pdf',
            to='./Plots/nkdtc_tp_tfp.pdf')
file.rename(from='./simulations_pi/nkdsge_mp.pdf',
            to='./Plots/nkdsge_mp.pdf')
file.rename(from='./simulations_pi/nkdsge_tfp.pdf',
            to='./Plots/nkdsge_tfp.pdf')
file.rename(from='./simulations_pi/nkdsge_aggressive_mp.pdf',
            to='./Plots/nkdsge_aggressive_mp.pdf')
file.rename(from='./simulations_pi/nkdsge_aggressive_tfp.pdf',
            to='./Plots/nkdsge_aggressive_tfp.pdf')
file.rename(from='./simulations_pi/nkdsge_accommodative_tfp.pdf',
            to='./Plots/nkdsge_accommodative_tfp.pdf')
file.rename(from='./simulations_pi/nkdsge_accommodative_mp.pdf',
            to='./Plots/nkdsge_accommodative_mp.pdf')
file.rename(from='./simulations_pi/nkdtc_z_shock.eps',
            to='./Plots/nkdtc_z_shock.eps')


# housekeeping, cleans all output files

f_output <- sapply(c('gali_recalib', 'nkdtc', 'sw07', 'ascardone14'), 
                   paste0,
                   c('.m', '_results.mat', '_dynamic.m', '_set_auxiliary_variables.m', '_static.m'))
f_output <- c(as.vector(f_output), 'sw07_params.mat',
              'sw07_pindx.mat', 'sw07_steadystate2.m', 'ascardone14_steadystate2.m')

f_output <- sapply('./simulations_pi/', paste0, f_output)


unlink(c('./simulations_pi/gali_recalib', './simulations_pi/nkdtc', './simulations_pi/sw07', './simulations_pi/ascardone14'), recursive = T)
file.remove(f_output)

options("scipen"=0, "digits"=7)
rm(i, f_output)
