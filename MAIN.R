##### MAIN FILE #####

# This file executes the scripts used in the paper
# "Taylor rule and liquidity in financial markets", 
#  by E. Franceschi - PSE and Paris 1
# 
# The scripts executes the following tasks:
#		- gather data from several online sources,
#			compile a consistent dataset of time series.
#			This dataset is stored in csv format in './Processed data/US_data.txt';
#		- run a set of econometric tests and estimations 
#			over a given number of specifications of the 
#			Taylor rule, plotting results.
#			
#	Please note that this script estimates a larger number of specifications:
#	those included in the paper are the following:
#	                      : Number        Name                List number
#	  - Paper's Spec. I   :   1           tr_standard           1
#	  - Paper's Spec. II  :   2           tr_spread_10y_baa     2
#	  - Paper's Spec. III :   3           tr_spread_sp          3
#	  
#	To obtain the results in the paper, uncomment the last lines of code below
#
#
# These scripts are not developed with code optimization in mind but result
# from modifications over time as the paper developed and changed. Higher speed
# might be easily achieved with vectorisation and parallelisation.
#
# v1

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


# 0 -- MsM estimation is off
# 1 -- MsM estimation is on, 2 states
# 2 -- MsM estimation is on, 3 states
flag___msm = 1


# Functions
source('functs.R', verbose=F, echo=F)
tic('Total time')

##### II - Data scraping, collection, and stocking #####
# US Data
# pick 'ahead' to set how many quarters ahead 
# to consider for SPF forecasts:
# -1 for previous quarter estimates
# 0 for nowcast
# 1 for one quarter ahead -- default
# 2 for two quarters ahead
# 3 for three quarters ahead
# 4 for one year ahead
tic('Data collection')
ahead <- 1
source('https://raw.githubusercontent.com/ceschi/us_macro_data/master/USdata_coll.R')
toc()



##### III - Regressions and tests on Taylor Rules #####
tic('Econometrics')
source('USreg.R', verbose=F, echo=F)
toc()


##### VIII - VISUALIZATION ####
tic('Graphing')
source('visuals.R', verbose=F, echo=F)
toc()


# housekeeping
rm(temp_dir, data_dir, graphs_dir, 
   working_directory, flag___singular,
   flag___msm, flag___plot,
   ahead, j, m)
toc()


##### Paper subset of specifications #####
# uncomment the lines below to retrieve the estimates from the paper, in the order
# of presentation as detailed on top of this script.


# reg_print(1)
# reg_print(2)
# reg_print(3)