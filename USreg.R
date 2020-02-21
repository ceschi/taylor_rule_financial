##### US regression #####


#### Functions ####
if (flag___singular == 1){
  
  cat('Single file execution')
  source('directories.R')
  source('functs.R')
  source('USdatacoll.R')
}




# consider putting all elements in lists of homogeneous 
# elements and then looping over these ones

regressions <- list(
  formula=list(),
  messages=list(),
  cor=list(),
  models=list(),
  params=list(),
  stab=list(
    cusum=list(),
    fstat=list(),
    fstatpoints=list(),
    fstatcandidates=list()
  ),
  mswm=list(
    fit=list(),
    coefs=list(),
    convse=list()
  ),
  plot=list(),
  gmm=list(
    fit=list(),
    params=list()
  )
)

# Formulas for regressions, appended to first sublist
regressions$formula <- list(
  # 1
    tr_standard =  ffr ~ deflt1 + realtime_gap + ffrb + q1 + q2 + q3,
  # 2
    tr_spread_sp = ffr ~ deflt1 + realtime_gap + ffrb + spread_sp_3m + q1 + q2 + q3,
  # 3
    tr_spread_10y_baa = ffr ~ deflt1 + realtime_gap + ffrb + spread_baa_long + q1 + q2 + q3,
  # 4
    tr_spread_baa_aaa = ffr ~ deflt1 + realtime_gap + ffrb + spread_baa_aaa + q1 + q2 + q3,
  # 5
    tr_spread_10y_aaa = ffr ~ deflt1 + realtime_gap + ffrb + spread_aaa + q1 + q2 + q3,
  # 6
    tr_spread_oldbaa  = ffr ~ deflt1 + realtime_gap + ffrb + spread_baa,# + q1 + q2 + q3,
  # 7
    tr_shrate_WX = shffr ~ deflt1 + realtime_gap + shffrb + q1 + q2 + q3,
  # 8
    tr_shrate_K = kripp_shffr ~ deflt1 + realtime_gap + kripp_shffrb + q1 + q2 + q3
    )

# Strings to indentify models 
regressions$messages <- list(
  # 1
  '1 - Standard TR',
  # 2
  '2 - TR and 3M spread',
  # 3
  '3 - TR with BAA spread',
  # 4
  '4 - TR with BAA-AAA spread',
  # 5
  '5 - TR with AAA-10y spread',
  # 6
  '6 - TR with BAA spread oldver',
  # 7
  '7 - Wu-Xia shadow rate',
  # 8
  '8 - Krippner shadow rate'
)

### Warm-up ####
################
# correlation table: values in 1986Q1:2013Q4
corr_tab <- db_US %>% xts_tbl() %>% select(ffr, ffrb,
                                 deflt, deflt1, 
                                 cpit, cpit1,
                                 coret, coret1, 
                                 realtime_gap, expost_gap, employment_fluct,
                                 spread_sp_3m, spread_baa_long) %>% na.omit(.) %>% cor(.)

##### isolate only those variables that are used
varss <- rapply(object = regressions$formula, f = base::all.vars) %>% unique()

db_US <- db_US[, varss]

##### cycle through data cuts for exogenous sampling
# db_US <- db_US['1981:12/2007:07/']
# db_US <- db_US['/2006:12']


### Looping over different specifications

for (m in 1:length(regressions$formula)){
# for (m in 1:5){
  ##### Simple OLS with stability checks #####

  # lapply
  # fit a linear model
  regressions$models[[m]] <- lm(data=db_US, regressions$formula[[m]])
  
  # lapply
  # rescale parameters
  regressions$params[[m]] <- repara(regressions$models[[m]])
  
  # graphing residuals w/ ggplot2
  regressions$plot[[m]] <- ggplot(data=data.frame(date=residuals(regressions$models[[m]]) %>%
                                                    names() %>% as.yearqtr('%Y Q%q'),
                                                  res=residuals(regressions$models[[m]])),
                                  aes(x=date, y=res)) + 
    geom_line()+theme_bw()+scale_x_yearqtr(format='%Y Q%q', n=20)+
    geom_hline(color='red', yintercept=regressions$models[[m]] %>% residuals() %>% sd() %>% `*`(2))+
    geom_hline(color='red', yintercept=regressions$models[[m]] %>% residuals() %>% sd() %>% `*`(-2))+
    xlab(' ') + ylab('Residuals') + ggtitle(regressions$messages[[m]])+
    theme(axis.text.x = element_text(angle = 90))
  
  # lapply
  # stability checks on OLS
  # CUSUM
  regressions$stab$cusum[[m]]<- efp(formula=regressions$formula[[m]], data=as.data.frame(db_US), type='OLS-CUSUM')
  # modifies title
  regressions[["stab"]][["cusum"]][[m]][["type.name"]] <- paste0(regressions$messages[[m]], ': OLS-based CUSUM test')

  # FStat - Chow test
  regressions$stab$fstat[[m]] <- Fstats(formula=regressions$formula[[m]], data=as.data.frame(db_US))
  # saves most likely date of structural break
  regressions$stab$fstatpoints[[m]] <- regressions[["models"]][[m]][["model"]][regressions$stab$fstat[[m]]$breakpoint,] %>%
    row.names()
  # extracts and saves dates of breaks detected

  regressions$stab$fstatcandidates[[m]] <- breakpoints(regressions$formula[[m]],
                                                       data=as.data.frame(db_US))





  #### Markov Switching models with K states ####
  # looping over formulae to estimate j-state
  # Markov Switching model
  if (flag___msm==1) j <- 2
  if (flag___msm==2) j <- 3

  if (flag___msm!=0){
      regressions$mswm$fit[[m]] <- msmFit(object=regressions$models[[m]],
                                          #data=db_US,
                                          k=j,
                                          sw=rep(T, 1+regressions$formula[[m]] %>% all.vars() %>% length())
                                          )

      # Delta coefs + replace correct \rhohat
      regressions$mswm$coefs[[m]] <- regressions$mswm$fit[[m]]@Coef/(1-regressions$mswm$fit[[m]]@Coef[,4])
      regressions$mswm$coefs[[m]][,4] <- regressions$mswm$fit[[m]]@Coef[,4]

      # Delta SE + add correct SE for \rho
      regressions$mswm$convse[[m]] <- regressions$mswm$fit[[m]]@seCoef/(1-regressions$mswm$fit[[m]]@Coef[,4])
      regressions$mswm$convse[[m]][,4] <- regressions$mswm$fit[[m]]@seCoef[,4]
  }

  
  
  ##### GMM estimates #####
  
  variabs <- regressions$formula[[m]] %>% all.vars()
  temps_db <- db_US %>% as_tibble(name = NULL) %>% select(all_of(variabs)) %>% na.omit()
  attach(temps_db, warn.conflicts = FALSE)
  
  regressions$gmm$fit[[m]] <- gmm(g = regressions$formula[[m]],
                                  x = temps_db,
                                  crit = 1e-50,
                                  # t0 = regressions$models[[m]]$coefficients, # useless because g is formula not function
                                  type = 'twoStep' # twoStep is standard, iterative/cue is another alternative
                                  # itermax = 5000
                                  )
  
  regressions$gmm$params[[m]] <- repara(regressions$gmm$fit[[m]])
  
  regressions$cor[[m]] <- db_US %>% as_tibble()  %>%
    select(regressions$formula[[m]] %>% all.vars()) %>%
    na.omit(.) %>% cor(.)
  
  detach(temps_db)
  # housekeeping
  rm(variabs, temps_db)
}


##### housekeeping
rm(varss)