##### Output gap and federal fund rate persistence #####



##### Packages #####

if (flag___singular == 1){
  cat('Single file execution')
  source('directories.R')
  source('functs.R')
  source('USdatacoll.R')
  # exogenous lag
  k=5
  
  # selector for coefficient
  # AR(r) will be plotted
  # MUST be <k
  r=1
  
  # select window width for
  # rolling estimates, pick <80
  # to get interesting results
  wind=56 # as Pivetta and Reis, 14y. Alter: 5y
}


##### Subset data and set up lists #####

ygap <- merge(db_US$layoffs,
              db_US$employment_fluct,
              db_US$realtime_gap,
              db_US$expost_gap,
              db_US$ffr
              )

n <- length(names(ygap))

# results collector
output_gap_persistence <- list(names = list('Layoff rate',
                                            'Employment fluctuations',
                                            'Real time output gap',
                                            'Ex post output gap',
                                            'FFR'),
                               unitroot = list(),
                               ark = list(),
                               aropti = list(),
                               aroptilm = list(),
                               plot_aropti = list(),
                               rollm = list(),
                               plot_rollm = list()
                               )


##### Unit root tests #####
if (flag___optilag==1){
  llags <- 18
} else if (flag___optilag==0){
  llags <- 8
}

for (i in 1:n){
  # saves output for every series
  sink(file=paste0(file.path(graphs_dir, output_gap_persistence$names[[i]]), ' AR output gap results.txt'),
       append=F,
       split=T)
  
  ##### Unit root tests #####
  output_gap_persistence[['unitroot']][[i]] <- ur.df(na.omit(ygap[,i]),
                                        # DF test, max lag to consider
                                        lags=llags,
                                        # lags selection criterion, min BIC
                                        selectlags='BIC')
  # storing optimal lags -- forked urca pkg
  
  if (flag___optilag==1) output_gap_persistence[['aropti']][[i]]<- output_gap_persistence[['unitroot']][[i]]@optilags
  
  if (output_gap_persistence[['unitroot']][[i]]@teststat>
      output_gap_persistence[['unitroot']][[i]]@cval[3]) {
    cat(paste0('For ', names(ygap)[i], ' it is not possible to reject \nthe null hypothesis of unit root.\n\n'))
  }else if ((output_gap_persistence[['unitroot']][[i]]@teststat<
             output_gap_persistence[['unitroot']][[i]]@cval[3]) &
            (output_gap_persistence[['unitroot']][[i]]@teststat>
             output_gap_persistence[['unitroot']][[i]]@cval[2])) {
    cat(paste0('For ', names(ygap)[i], ' it is possible to reject \nthe null hypothesis of unit root at 90%.\n\n'))
  }else if((output_gap_persistence[['unitroot']][[i]]@teststat<
            output_gap_persistence[['unitroot']][[i]]@cval[2]) &
           (output_gap_persistence[['unitroot']][[i]]@teststat>
            output_gap_persistence[['unitroot']][[i]]@cval[1])) {
    cat(paste0('For ', names(ygap)[i], ' it is possible to reject \nthe null hypothesis of unit root at 95%.\n\n'))
  }else if(output_gap_persistence[['unitroot']][[i]]@teststat<
           output_gap_persistence[['unitroot']][[i]]@cval[1]) {
    cat(paste0('For ', names(ygap)[i], ' it is possible to reject \nthe null hypothesis of unit root at 99%.\n\n'))
  }
  
  
  
  ##### AR regression with fixed lags k=5 #####
  # k = 5 as standard practice but this might vary
  output_gap_persistence[['ark']][[i]] <- lm(data= (ygap[,i] %>% lagger(lag=k)),
                                formula=formula.maker(df=ygap[,i] %>% lagger(lag=k),
                                                      y= ygap[,i] %>% lagger(lag=k) %>% 
                                                        names(.) %>% first())
  ) #%>% summary() %>% coef()
  cat('\n')
  print(paste0(output_gap_persistence$names[[i]],',  ', k, ' exogenously defined lags'))
  print(summary(output_gap_persistence[['ark']][[i]]))
  
  ##### AR regression with optimal lags #####
  if (flag___optilag==1){
    output_gap_persistence[['aroptilm']][[i]] <- rolloop(df = ygap[,i], window = wind, lags = output_gap_persistence[['aropti']][[i]])
    
    output_gap_persistence[['plot_aropti']][[i]] <- ggplot(data=output_gap_persistence[['aroptilm']][[i]],
                                              aes(x=index(output_gap_persistence[['aroptilm']][[i]]),
                                                  y=output_gap_persistence[['aroptilm']][[i]][,r]))+
      # plot the above with line geom, in black
      geom_line(colour='black', size=1)+
      # adds upper confidence band in red
      geom_line(aes(y=output_gap_persistence[['aroptilm']][[i]][,r]+output_gap_persistence[['aroptilm']][[i]][,output_gap_persistence[['aropti']][[i]]+r]),
                colour='red')+
      # adds lower confidence band in red
      geom_line(aes(y=output_gap_persistence[['aroptilm']][[i]][,r]-output_gap_persistence[['aroptilm']][[i]][,output_gap_persistence[['aropti']][[i]]+r]),
                colour='red')+
      # adds unit root line
      geom_line(aes(y=1), colour='black', size=.8)+
      # plot makeup
      geom_smooth(method='loess', colour='blue')+scale_x_yearqtr(format='%Y Q%q', n=20)+theme_bw()+
      scale_y_continuous()+xlab(' ') + ylab(paste0('AR(',r,') coeff. estimates')) +
      ggtitle(paste0(output_gap_persistence$names[[i]],' - ', output_gap_persistence[['aropti']][[i]], ' optimal lags'))
    
    if  (flag___plot==0) plot(output_gap_persistence[['plot_aropti']][[i]])
    
    # saves graphs in proper directory with names
    ggsave(paste0('AR(',r,') coeff. estimates ', output_gap_persistence[['names']][[i]],
                  ' - ', output_gap_persistence[['aropti']][[i]], ' optimal lags.pdf'),
           output_gap_persistence[['plot_aropti']][[i]],
           device='pdf',
           graphs_dir,
           height=8, width=14.16, units='in')
  }
  ##### AR(k) rolling window regressions #####
  ## consider using map() or apply() function families
  ## to vectorise code and make it faster. This requires
  ## wrapping loop's actions in an ad-hoc function
  ## about window length: 58 is used by Pivetta&Reis as well as Fuhrer, trade off
  ## is about precision and extension of the resulting estimates
  output_gap_persistence[['rollm']][[i]] <- rolloop(df = ygap[,i], window = wind, lags = k)
  
  
  ##### Plots registration #####
  # attaches data, select first column of estimates and time
  output_gap_persistence[['plot_rollm']][[i]] <- ggplot(data=output_gap_persistence[['rollm']][[i]],
                                           aes(x=index(output_gap_persistence[['rollm']][[i]]),
                                               y=output_gap_persistence[['rollm']][[i]][,r]))+
    # plot the above with line geom, in black
    geom_line(colour='black', size=1)+
    # adds upper confidence band in red
    geom_line(aes(y=output_gap_persistence[['rollm']][[i]][,r]+output_gap_persistence[['rollm']][[i]][,k+r]),
              colour='red')+
    # adds lower confidence band in red
    geom_line(aes(y=output_gap_persistence[['rollm']][[i]][,r]-output_gap_persistence[['rollm']][[i]][,k+r]),
              colour='red')+
    # adds unit root line
    geom_line(aes(y=1), colour='black', size=.8)+
    # plot makeup
    geom_smooth(method='loess', colour='blue')+scale_x_yearqtr(format='%Y Q%q', n=20)+theme_bw()+
    scale_y_continuous()+xlab(' ') + ylab(paste0('AR(',r,') coeff. estimates')) + 
    ggtitle(paste0(output_gap_persistence$names[[i]], ' - ', k, ' exogneous lags'))
  
  if  (flag___plot==0) plot(output_gap_persistence[['plot_rollm']][[i]])
  
  # saves graphs in proper directory with names
  ggsave(paste0('AR(',r,') coeff. estimates ', output_gap_persistence[['names']][[i]], '.pdf'),
         output_gap_persistence[['plot_rollm']][[i]],
         device='pdf',
         graphs_dir,
         height=8, width=14.16, units='in')
  
  cat('\n\n\n')
  # stopping printing
  sink()
}





##### Housekeeping ####
rm(ygap, n, i, llags)

if (flag___singular == 1) rm(r, k, wind)