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
  # var=list(
  #   varfit=list(),
  #   varirf=list()
  # ),
  # svar=list(
  #   svarfit=list(),
  #   svarirf=list()
  # ),
  plot=list(),
  gmm=list(
    fit=list(),
    params=list()
  )
)

# Formulas for regressions, appended to first sublist
regressions$formula <- list(
  # 1
    tr_standard =  ffr ~ deflt1 + realtime_gap + ffrb,
  # 2
    tr_layoff = ffr  ~ deflt1 + layoffs + ffrb,
  # 3
    tr_spread = ffr ~ deflt1 + realtime_gap + ffrb + spread_baa,
  # 4
    tr_treasury = ffr ~ deflt1 + realtime_gap + ffrb + spread_sp_3m,                  # ADDED 1 TO ALL DEFLT
  # 5
    tr_layspread = ffr ~ deflt1 + layoffs + ffrb + spread_sp_3m,
  # 6
    tr_laybaa = ffr ~ deflt1 + layoffs + ffrb + spread_baa,
  # 7
    tr_spf_mean = ffr ~ spf_cpi_h1_mean + realtime_gap + ffrb,
  # 8
    tr_spf_uncert = ffr ~ deflt1 + realtime_gap + ffrb + spf_cpi_h1_iqr,
  # 9
    tr_debt_g = ffr ~ deflt1 + realtime_gap + ffrb + debt_growth,
  # 10
    tr_surplus = ffr ~ deflt1 + realtime_gap + ffrb + surplus_gdp,
  # 11
    tr_spread_long = ffr ~ deflt1 + realtime_gap + ffrb + spread_baa_long,
  # 12
    tr_spread_aaabaa = ffr ~ deflt1 + realtime_gap + ffrb + spread_baa_aaa,
  # 13
    tr_spread_corp = ffr ~ deflt1 + realtime_gap + ffrb + spread_aaa,
  # 14
    tr_laybaa_long = ffr ~ deflt1 + layoffs + ffrb + spread_baa_long
    )

# Strings to indentify models 
regressions$messages <- list(
  # 1
  '1 - Standard TR',
  # 2
  '2 - TR with layoffs replacing output gap',
  # 3 
  '3 - TR and BAA spread',
  # 4
  '4 - TR and 3M spread',
  # 5
  '5 - TR with layoffs and 3M spread',
  # 6
  '6 - TR with layoffs and BAA spread',
  # 7
  '7 - TR with SPF mean expected inflation',
  # 8
  '8 - TR augmented with IQR SPF',
  # 9
  '9 - TR with debt growth',
  # 10
  '10 - TR with surplus',
  # 11
  '11 - TR with long BAA spread',
  # 12
  '12 - TR with BAA-AAA spread',
  # 13
  '13 - TR with AAA-10y spread',
  # 14
  '14 - TR with layoffs and long BAA spread'
)

### Warm-up ####
################
# correlation table: values in 1986Q1:2013Q4
corr_tab <- db_US %>% as_tibble() %>% select(ffr, ffrb,
                                 deflt, deflt1, cpit, cpit1, coret, coret1, 
                                 realtime_gap, layoffs, expost_gap, employment_fluct,
                                 spread_baa, spread_sp_3m, 
                                 spf_cpi_h1_mean, spf_cpi_h1_iqr, 
                                 debt_growth, surplus_gdp) %>% na.omit(.) %>% cor(.)


# temp_ <- db_US
# # db_US <- db_US['1981:12/2007:07/']
# db_US <- db_US['2007:07/']


### Looping over different specifications

for (m in 1:length(regressions$formula)){
# for (m in c(1,2,4,5,9,10,11,12,13,14)){
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
  temps_db <- db_US %>% as_tibble(name = NULL) %>% select(variabs) %>% na.omit()
  attach(temps_db, warn.conflicts = FALSE)
  
  regressions$gmm$fit[[m]] <- gmm(g = regressions$formula[[m]],
                                  x = temps_db,
                                  crit = 1e-18)
  
  regressions$gmm$params[[m]] <- repara(regressions$gmm$fit[[m]])
  
  regressions$cor[[m]] <- db_US %>% as_tibble()  %>%
    select(regressions$formula[[m]] %>% all.vars()) %>%
    na.omit(.) %>% cor(.)
  
  detach(temps_db)
  # housekeeping
  rm(variabs, temps_db)
}