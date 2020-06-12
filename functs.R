##### Specifically designed functions ####

# install/load packages
instant_pkgs <- function(pkgs) { 
  ## Function loading or installing packages in
  ## current R instance.
  ## Developed by Jaime M. Montana Doncel - V1
  
  
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  if (length(pkgs_miss) == 0) {
    message("\n ...Packages were already installed!\n")
  }
  
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach))  suppressPackageStartupMessages(library(need_to_attach[i], character.only = TRUE))
  }
  
  if (length(need_to_attach) == 0) {
    message("\n ...Packages were already loaded!\n")
  }
}



# A file to gather all home made functions with relative descriptions

reg_call <- function(m){
  # custom function to extract, print and plot 
  # information on estimates of a particular
  # Taylor rule specification. The latter is selected
  # by specifying m iterator.
  
  # sink fnct saves in a txt file
  # the output while printing it out
  # on the command line
  
  # first, stops older sink
  # sink(file = NULL)
  
  
  # # store defaults
  # st_setting <- c(getOption('scipen'), # default: 0
  #                 getOption('digits')) # default: 7
  # 
  # options(scipen = 200,
  #         digits = 4)
  
  sink(file=paste0(file.path(graphs_dir, regressions$messages[[m]]), ' regressions results.txt'),
       append=F,
       split=T,
       type='output')
  
  sa_plot <- function(po){
    # custum function to duplicate, save as pdf 
    # and shut second graphic device
    dev.copy(pdf, po, height=8/1.5, width=14.6/1.5)
    invisible(dev.off())
    # set height=8/1.5 and width=14.6/1.5
    # for LaTeX readable plots
  }
  
  
  # prints the name of the model
  cat(paste0(as.character(regressions$messages[[m]]), '\n'))
  
  # prints the estimated formula
  cat('\n')
  print(regressions$formula[[m]])
  cat('\n\n\nCorrelation matrix for the specification:\n')
  print(regressions$cor[[m]])
  cat('\n')
  
  cat('\nStandard output:\n')
  print(summary(regressions$models[[m]]))
  
  cat('\nBIC\n')
  print(BIC(regressions$models[[m]]))
  
  # prints the number of observations used in the model
  cat(paste0('\nModel estimated with ', nobs(regressions$models[[m]]), ' observations\n\n'))
  
  # prints converted parameters + SE
  print(regressions$params[[m]])

  # plots the residuals + SE bands for stability
  print(regressions$plot[[m]])
  sa_plot(paste0(file.path(graphs_dir, regressions$messages[[m]]), ' resids.pdf'))

  # plots cusum stability diagnostics
  # extract useful ts from object
  stab_ts <- regressions$stab$cusum[[m]]$process
  
  plot(regressions$stab$cusum[[m]], alpha=.01, boundary=T, xaxt="n", lty=6)
  axis(side = 1,
       at = (0:10)/10,
       labels = names(stab_ts)[
         floor(
           seq(from = 1,
               to = length(stab_ts), 
               length.out =  11)
               )
         ], 
       las = 0
       )
  sa_plot(paste0(file.path(graphs_dir, regressions$messages[[m]]), ' CUSUM.pdf'))

  # plots Fstat stability diagnostics
  # extract start and end for plotting dates
  ts_start <- start(regressions$stab$fstat[[m]]$Fstats)[2]
  ts_end <- end(regressions$stab$fstat[[m]]$Fstats)[2]
  fstat_ts_names <- names(stab_ts)[ts_start:ts_end]
  
  plot(regressions$stab$fstat[[m]], xaxt="n", lty=6)
  title(main=paste0(regressions$messages[[m]], ': F-stat stability'),
        sub=paste0('Vertical line indicates date of most likely break: ',
                   regressions$stab$fstatpoints[[m]]),
        cex.sub = 2)
  lines(breakpoints(regressions$stab$fstat[[m]]))
  axis(side = 1,
       at = (0:10)/10,
       labels = fstat_ts_names[
         floor(
           seq(from = 1, 
               to = length(fstat_ts_names),
               length.out = 11)
           )
         ],
       las = 0
       )
  
  sa_plot(paste0(file.path(graphs_dir, regressions$messages[[m]]), ' F-stat.pdf'))

  # prints date of most likely break
  cat('\n\n\n')
  cat(paste0('Most likely singular break occurs at ',
             as.character(regressions$stab$fstatpoints[[m]]), '\n'))
  regressions$stab$fstatcandidates[[m]]

  # optimal number of segment partition,
  # -1 to account for the 0-breaks case
  fstat_dates <- which(summary(regressions$stab$fstatcandidates[[m]])$RSS[2,]==
                         min(summary(regressions$stab$fstatcandidates[[m]])$RSS[2,]), arr.ind=T)-1

  # extracting corresponding nobs and dates
  n_obs <- summary(regressions$stab$fstatcandidates[[m]])$breakpoints[fstat_dates,] %>% na.omit(.)
  multibreaks <- names(regressions$stab$fstatcandidates[[m]]$y)[n_obs] %>% paste(collapse=', ')

  # printing optimal segment partition dates
  cat(paste0('while optimal segmentation points to ', length(n_obs), ' breaks, at dates ', multibreaks))


  # MSwM printig results and plotting
  if (length(regressions$mswm$fit)!=0){
    n_states <- regressions$mswm$fit[[m]]@k
    cat('\n\n\nMarkov Switching model estimation with', n_states, 'states')
    cat('\n')
    cat(summary(regressions$mswm$fit[[m]]))
    cat('\n\nConverted parameters:\n')
    print(regressions$mswm$coefs[[m]])
    cat('\nConverted standard errors:\n')
    print(regressions$mswm$convse[[m]])

    # fine tuning plots
    # extract names of model (ie dates)
    msm_dates <- names(regressions$mswm$fit[[m]]@model$fitted.values)
    
    invisible(dev.off())
    par(mar=c(1.5,5,5,1.5), cex=.4)
    MSwM::plotProb(regressions$mswm$fit[[m]], which=2)
    title(paste0(n_states, '-state MS regimes for ', regressions$messages[[m]]), line=2.3)
    # axis(side = 1,
    #      at = (0:10)/10,
    #      labels = msm_dates[
    #        floor(
    #          seq(from = 1,
    #              to = length(msm_dates),
    #              length.out = 11)
    #        )
    #        ],
    #      las = 0,
    #      pos = -.1
    #      )

    
    sa_plot(file.path(graphs_dir,paste0(regressions$messages[[m]], ' ',
                                        n_states,'-state MSM.pdf')))

    # silently setting margins to default values
    invisible(dev.off())
  }
  
  
  cat('\n\n\nGMM estimates for robustness:\n')
  print(regressions$gmm$params[[m]])

  
  # end spacing
  cat('\n\n\n\n')
      
  
  
  # stopp printing
  sink(file=NULL)
  # # restore defaults
  # options('scipen'=st_setting[1],
  #        'digits'=st_setting[2])
  
  ########## STARGAZER for latex output? #########
}

rollm <- function(df, formula){
  # function to extract and store coefficients 
  # and double SD in a named row tibble
  
  
  # estimates the linear model
  lmod <- summary(lm(data=df, formula=formula))
  
  # extracts point estimates and 2*SD (+- 95%),
  # put info in named row tibble dropping 
  # intercept info from first column
  cofs <- as.tibble(coefficients(lmod)[2:(lmod %>% coefficients() %>% 
                                            t() %>% ncol()),1] %>% t())
  SD2 <- as.tibble(2*coefficients(lmod)[2:(lmod %>% coefficients() %>% 
                                            t() %>% ncol()),2] %>% t())
  
  # adds suffix for bands
  names(SD2) <- paste0(names(SD2), '.SD2')
  
  # merges in one row with names
  estim <- cbind(cofs, SD2)
  
  # outputs
  return(estim)
}

rolloop <- function(df, window=8, lags=1){
  
  # width of the rolling window
  window <- as.integer(window)
  
  # select lags 
  k <- as.integer(lags)
  
  # lags the time series, names it, cuts out NAs
  df <- df %>% lagger(lag=k, na.cut=T)
  # and creates related formula
  formulae <- formula.maker(df, df %>%  names(.) %>% first())
  
  # computes point estimates and 2SD
  # stocks in a dataframe for convenience
  regs <-rollapply(as.data.frame(df),
                   width=window,
                   by.column = F,
                   FUN=function(x, formula) rollm(df=as.data.frame(x), formula=formulae))
  
  # converts and dates the regressions
  regs <- xts(regs, frequency=4, 
              order.by=index(df)[window:length(index(df))])
  return(regs)
}

make_stars <- function(x){
  # ancillary function for
  # printing stars alongside
  # with converted parameters
  
  c
  # pre-allocate 
  signif <- NULL
  
  if (x < .001) {
    signif <- as.factor('***')
  }else if (x < .01 & x >= .001){
    signif <- as.factor('**')
  }else if (x < .05 & x >= .01){
    signif <- as.factor('*')
  }else if (x < .1 & x >= .05){
    signif <- as.factor('.')
  }else if (x>=.1){
    signif <- as.factor('')
  }
  
  return(signif)
}


repara <- function(x, rho=4){
  # function to reparametrize once a lm is estimated 
  # having on the 4th place the persistence parameter for FFR
  # and SE + p-val
  
  
  # ancillary for stars
  make_stars <- function(x){
    
    # pre-allocate 
    signif <- NULL
    
    if (x < .001) {
      signif <- as.factor('***')
    }else if (x < .01 & x >= .001){
      signif <- as.factor('**')
    }else if (x < .05 & x >= .01){
      signif <- as.factor('*')
    }else if (x < .1 & x >= .05){
      signif <- as.factor('.')
    }else if (x>=.1){
      signif <- as.factor('')
    }
    
    return(signif)
  }
  
  # coefs and SE
  params <- coef(summary(x))[,1:2]/(1-coef(x)[rho])
  params[rho,] <- coef(summary(x))[rho, 1:2]
  
  # p-val
  p_val <- coef(summary(x))[,4]
  params <- cbind(params, p_val)
  
  params2 <- data.frame(params,
                        sig=sapply(X = p_val, FUN = make_stars))
  
  
  return(params2)
}

lagger <- function(series, lag, na.cut=F){
  # Takes a time series and creates a matrix with given number
  # of lags, also generating appropriate names
  
  
  matrix <- as.data.frame(matrix(ncol=lag+1, nrow=nrow(series)))
  for (i in 1:lag+1){
    matrix[,i] <- stats::lag(series, k=(i-1))
  }
  names(matrix) <- c(names(series), paste(names(series), 1:lag, sep='.'))
  matrix[, 1] <- series
  matrix <- as.xts(matrix, order.by=index(series))
  
  # conditional to remove NAs from output
  if (na.cut){
    matrix <- na.omit(matrix)
  }
  
  # output
  return(matrix)
}

# Apparently "lagger" works faster
# than "lagger_bis" despited loops
# but only short lags

lagger_bis <- function(series, lag, na.cut=F){
  # Takes a time series and creates a matrix with given number
  # of lags, also generating appropriate names
  # 
  matrix <- embed(as.matrix(series), lag+1)
  matrix <- as.data.frame(matrix)
  names(matrix) <- c(names(series), paste(names(series), 1:lag, sep='.'))
  
  # conditional to remove NAs from output
  if (na.cut){
    matrix <- na.omit(matrix)
  }
  
  # output
  return(matrix)
}

formula.maker <- function(df, y, intercept = T){
  # provided with a df and a dependent variable name
  # this generates a formula for estimation in R, y is the 
  # dependent variable, all the others are considered
  # independent and explanatory ones
  
  if (intercept){
    fomu <- as.formula(paste(y, 
                           paste(names(df)[names(df)!=y], collapse='+'),
                           # paste(c(0,names(df)[names(df)!=y]), collapse='+'),
                           # this prevents to have a constant but breaks the
                           # functioning of the code 
                           sep='~'))
  } else {
    fomu <- as.formula(paste(y,
                             paste(c(0,names(df)[names(df)!=y]), collapse='+'),
                             # this prevents to have a constant but breaks the
                             # functioning of the code somewhere
                             sep='~'))
                }
  
  
  attr(fomu, which='.Environment') <- .GlobalEnv
  return(fomu)
}

##### TRACKING PERSISTENCE OVER TIME #####
persistence_ridges <- function(tseries, window = 24, lags = 8){
  # requires zoo, broom
  if (!require(zoo))    {install.packages('zoo');   library(zoo)}
  if (!require(broom))  {install.packages('broom'); library(broom)}
  
  # check out the nature of the input
  # throw an error if it's not time series class
  if (!(class(tseries)=='ts' || class(tseries)=='xts' || class(tseries) == 'zoo')) error('Wrong object, please provide a time series object (ts, zoo, xts).')
  if (window<=lags*2) warning('\nWrong window/lag sizes: \nto get meaningful estimates window width should be at least twice the lags.')
  
  # define function to be applied rolling over
  bloc_ar <- function(tseries, lags = 8, interc = F, last){
    
    # save out last observation of the series
    # will be the identifier later on
    # last <- time(tseries)[length(tseries)]
    
    # generate a matrix with lags+1 columns
    # to have original series + lagged cols
    # 
    # It outputs a flat matrix, its 
    # length is cut down by lags
    mat_lag <- embed(tseries, lags+1)
    
    
    # estimate linear model without intercept,
    # store the results
    estlm <- lm(data = as.data.frame(mat_lag),
                formula = formula.maker(as.data.frame(mat_lag), 'V1', intercept = interc))
    
    # flip in tidy format the lm output
    # and delete the "statistic" col
    est_tidy <- broom::tidy(estlm)
    est_tidy$statistic <- NULL
    
    # gather all in a dated dataframe:
    # lengths = 8
    # width   = 5
    # names = c('last.date', 'term', 'estimate', 'std.error', 'p.value')
    col <- data.frame(last.date = rep(last, lags),
                      est_tidy)
    
    # output the resulting df
    return(col)
  }
  
  # remove all NAs - experimental
  tseries <- na.omit(tseries)
  
  # this object out_fin will accommodate 
  # the results, iteration by iteration
  # add names and preallocate cells
  out_fin <- matrix(nrow = (length(tseries)-window+1)*lags, ncol = 5)
  out_fin <- as.data.frame(out_fin)
  names(out_fin) <- c('last.date', 'term', 'estimate', 'std.error', 'p.value')
  
  for (i in 1:(length(tseries)-window+1)){
    
    last_date <- time(tseries)[(i+window-1)]
    
    col_fin <- bloc_ar(tseries = tseries[i:(i+window-1)],
                       lags = lags,
                       interc = F,
                       last = last_date)
    
    out_fin[((i-1)*lags + 1):((i)*lags),] <- col_fin
    # out_df <- rbind(out_df,col)
  }
  
  out_fin$term <- as.numeric(
                  gsub(pattern = '[V]',
                       x = out_fin$term,
                       replacement = '')
                            ) - 1
  
  return(out_fin)
}


reg_print <- function(m){
  # custom function to solely print 
  # information on estimates of a particular
  # Taylor rule specification. The latter is selected
  # by specifying m iterator.
  # 
  # Lighter version of previous reg_call function above
  # without plots
  
  
  
  
  # sink fnct saves in a txt file
  # the output while printing it out
  # on the command line
  
  
  # prints the name of the model
  cat(paste0(as.character(regressions$messages[[m]]), '\n'))
  
  # prints the estimated formula
  cat('\n')
  print(regressions$formula[[m]])
  cat('\n\n\nCorrelation matrix for the specification:\n')
  print(regressions$cor[[m]])
  cat('\n')
  
  cat('\nStandard output:\n')
  print(summary(regressions$models[[m]]))
  
  cat('\nBIC\n')
  print(BIC(regressions$models[[m]]))
  
  # prints the number of observations used in the model
  cat(paste0('\nModel estimated with ', nobs(regressions$models[[m]]), ' observations\n\n'))
  
  # prints converted parameters + SE
  print(regressions$params[[m]])
  

  # prints date of most likely break
  cat('\n\n\n')
  cat(paste0('Most likely singular break occurs at ',
             as.character(regressions$stab$fstatpoints[[m]]), '\n'))
  regressions$stab$fstatcandidates[[m]]
  
  # optimal number of segment partition,
  # -1 to account for the 0-breaks case
  fstat_dates <- which(summary(regressions$stab$fstatcandidates[[m]])$RSS[2,]==
                         min(summary(regressions$stab$fstatcandidates[[m]])$RSS[2,]), arr.ind=T)-1
  
  # extracting corresponding nobs and dates
  n_obs <- summary(regressions$stab$fstatcandidates[[m]])$breakpoints[fstat_dates,] %>% na.omit(.)
  multibreaks <- names(regressions$stab$fstatcandidates[[m]]$y)[n_obs] %>% paste(collapse=', ')
  
  # printing optimal segment partition dates
  cat(paste0('while optimal segmentation points to ', length(n_obs), ' breaks, at dates ', multibreaks))
  
  
  # MSwM printig results and plotting
  if (length(regressions$mswm$fit)!=0){
    n_states <- regressions$mswm$fit[[m]]@k
    cat('\n\n\nMarkov Switching model estimation with', n_states, 'states')
    cat('\n')
    cat(summary(regressions$mswm$fit[[m]]))
    cat('\n\nConverted parameters:\n')
    print(regressions$mswm$coefs[[m]])
    cat('\nConverted standard errors:\n')
    print(regressions$mswm$convse[[m]])
  }
  
  
  cat('\n\n\nGMM estimates for robustness:\n')
  print(regressions$gmm$params[[m]])
  
  
  # end spacing
  cat('\n\n\n\n')
}




##### Packages Loader #####

# remove standard packages with same names
# if ('MSwM' %in% installed.packages()){remove.packages('MSwM')}
# if ('urca' %in% installed.packages()){remove.packages('urca')}

# devtools::install_github('sboysel/fredr')
devtools::install_github('ceschi/urcabis')
devtools::install_github('ceschi/MSwMbis')
# devtools::install_version("readxl", version = "1.0.0")
# library(urcabis) # for when the package will be duly updated (pull request 2017)
# in all likelyhood it will never be updated.................


pkgs <- c(
          'broom',
          'car',
          'devtools',
          'ggridges',
          'gmm',
          'httr',
          'lubridate',
          'mFilter',
          'MSwM',
          'stargazer',
          'strucchange',
          'tictoc',
          'urca',
          'viridis')
# fill pkgs with names of the packages to install

instant_pkgs(pkgs)


#### housekeeping ####
rm(pkgs)