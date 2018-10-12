#### R code for inflation dynamics 
# Most of the code is an adptation
# from the script used with real data
# in the empirical section of the paper


##### run models file calling dynare ####

get_matlab()

run_matlab_script('./simulations_pi/models_main.m',
					verbose = F)

# The script above runs several instances
# of Dynare, simulating 4 DSGE models under different
# calibrations and shocks structure.



# move plots

file.rename(from='./simulations_pi/nkdtc_notp_mp.eps',
            to='./Plots/nkdtc_notp_mp.eps')
file.rename(from='./simulations_pi/nkdtc_notp_tfp.eps',
            to='./Plots/nkdtc_notp_tfp.eps')
file.rename(from='./simulations_pi/nkdtc_tp_mp.eps',
            to='./Plots/nkdtc_tp_mp.eps')
file.rename(from='./simulations_pi/nkdtc_tp_tfp.eps',
            to='./Plots/nkdtc_tp_tfp.eps')
file.rename(from='./simulations_pi/nkdsge_mp.eps',
            to='./Plots/nkdsge_mp.eps')
file.rename(from='./simulations_pi/nkdsge_tfp.eps',
            to='./Plots/nkdsge_tfp.eps')
file.rename(from='./simulations_pi/nkdsge_aggressive_mp.eps',
            to='./Plots/nkdsge_aggressive_mp.eps')
file.rename(from='./simulations_pi/nkdsge_aggressive_tfp.eps',
            to='./Plots/nkdsge_aggressive_tfp.eps')
file.rename(from='./simulations_pi/nkdsge_accommodative_tfp.eps',
            to='./Plots/nkdsge_accommodative_tfp.eps')
file.rename(from='./simulations_pi/nkdsge_accommodative_mp.eps',
            to='./Plots/nkdsge_accommodative_mp.eps')
file.rename(from='./simulations_pi/nkdtc_z_shock.eps',
            to='./Plots/nkdtc_z_shock.eps')
file.rename(from='./simulations_pi/nkdtc_tp_tfp_allvar.eps',
            to='./Plots/nkdtc_tp_tfp_allvar.eps')
file.rename(from='./simulations_pi/nkdtc_tp_mp_allvar.eps',
            to='./Plots/nkdtc_tp_mp_allvar.eps')
file.rename(from='./simulations_pi/nkdtc_notp_tfp_allvar.eps',
            to='./Plots/nkdtc_notp_tfp_allvar.eps')
file.rename(from='./simulations_pi/nkdtc_notp_mp_allvar.eps',
            to='./Plots/nkdtc_notp_mp_allvar.eps')

# housekeeping, cleans all output files

f_output <- sapply(c('gali_recalib', 'nkdtc', 'sw07', 'ascardone14'), 
                   paste0,
                   c('.m', '_results.mat', '_dynamic.m', '_set_auxiliary_variables.m', '_static.m'))
f_output <- c(as.vector(f_output), 'sw07_params.mat',
              'sw07_pindx.mat', 'sw07_steadystate2.m', 'ascardone14_steadystate2.m')

f_output <- sapply('./simulations_pi/', paste0, f_output)


unlink(c('./simulations_pi/gali_recalib', './simulations_pi/nkdtc', './simulations_pi/sw07', './simulations_pi/ascardone14'), recursive = T)
file.remove(f_output)

rm(f_output)
