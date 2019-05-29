##### MAIN FILE #####

# This file executes the scripts used in the paper
# "Taylor rule(s) inflation", by E. Franceschi - PSE and Paris 1
# 
# The execution of the full batch of scripts requires
# a working version of Matlab equipped with Dynare.
# 
# The scripts executes the following tasks:
#		- gather data from several online sources,
#			compile a consistent dataset of time series.
#			This dataset is stored in csv format in './Processed data/US_data.txt';
#		- run a set of econometric tests and estimations 
#			over a given number of specifications of the 
#			Taylor rule, plotting results;
#		- simulate with Dynare 4 DSGE models, stock the inflation
# 		series and run the same econometric tests as the real inflation
#			data, plotting the AR properties of these models;
#		- solve and simulate the liquidity NKDSGE model with two monetary rules,
#			generate the IRFs; simulate the standard NKDSGE model for regular,
#			extremely aggressive and almost passive Central Banks. For all these
#			models and cases plot and compare IRFs.
#
#
# These scripts are not developed with code optimization in mind but result
# from modifications over time as the paper developed and changed. Higher speed
# might be easily achieved with vectorisation and parallelisation.
#
# v0.92

##### I - Flags, libraries, and folders #####

# Flagging
# make this interactive at the beginning of the 
# script

# 0 -- the code runs entirely, all different parts composed
# 1 -- when files are run singularily
flag___singular = 0

# 0 -- the code prints out all graphs
# 1 -- graphs are not printed but only produced and stored
flag___plot = 0

# 0 -- optimal lags for inflation are off
# 1 -- optimal lags for inflation are on
flag___optilag = 1

# 0 -- MsM estimation is off
# 1 -- MsM estimation is on, 2 states
# 2 -- MsM estimation is on, 3 states
flag___msm = 1


# Functions
source('functs.R', verbose=F, echo=F)
tic('Total time')
# Directories
source('directories.R', verbose=F, echo=F)




##### II - Data scraping, collection, and stocking #####
# US Data
# pick ahead to set how many quarters ahead 
# to consider for SPF forecasts:
# -1 for previous quarter estimates
# 0 for nowcast
# 1 for one quarter ahead -- default
# 2 for two quarters ahead
# 3 for three quarters ahead
# 4 for one year ahead
tic('Data collection')
ahead <- 1
source("USdatacoll.R", verbose=F, echo=F)
toc()



##### III - Regressions and tests on Taylor Rules #####
tic('Econometrics')
source('USreg.R', verbose=F, echo=F)
toc()


# stuff to develop away from this 
# ##### IV - Analyses on inflation #####
# # AR(k) on several inflation series
# tic('Inflation - real data')
# # exogenous lag
# # ideally cycling through different values
# # like 1 3 5
k=1
# 
# # selector for coefficient
# # AR(r) will be plotted
# # MUST be =<k
r=1
# 
# # select window width for
# # rolling estimates, pick <80
# # to get interesting results
wind=14*4
# 
# source('inflanalysis.R')
# toc()


# 
# ##### V - Inflation simulations #####
# tic('Models and simulations')
# source('dyna.R')
# toc()



##### VI - AR(p*) estimates on simulated inflation ######
# tic('AR(p*) estimates')
# source('sim_pi.R')
# toc()


# ##### VII - Persistence in output gap and FFR ####
# tic('Other persistences')
# k = 5
# r = 1
# wind = 14*4
# source('gap_ffr_persistence.R')
# toc()

##### VIII - VISUALIZATION ####
tic('Graphing')
source('visuals.R', verbose=F, echo=F)
toc()


# housekeeping
rm(temp_dir, data_dir, graphs_dir, 
   working_directory, flag___singular,
   flag___msm, flag___optilag, flag___plot,
   ahead, k, r, wind, j, m)
toc()