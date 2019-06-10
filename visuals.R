############## Visualization for TR study ###############

if (flag___singular==1) library(ggplot2, xts)


##### Plots of general variables #####

# scale for LaTeX plots
# /1 to have bigger ones
# good for nontext vis

invsc <- 1.5
pdf_width = 14.6/invsc; pdf_height = 8/invsc


# TR variables
plot_trvars <- ggplot(db_US["1945/2020"], aes(x=index(db_US["1945/2020"])))+
  geom_line(aes(y=ffr, color='FFR'), size=1)+
  geom_line(aes(y=rev_defl, color='Act. Infl.'), size=1)+
  geom_line(aes(y=deflt1, color='Exp. Infl.'), size=1)+
  geom_line(aes(y=realtime_gap, color='Gap'), size=1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('US Taylor rule - main components')+
  scale_y_continuous()+
  scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot==0) print(plot_trvars)

ggsave(plot_trvars,
       filename='TRvars.pdf',
       path = graphs_dir, 
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Measures of inflation, revised ones
plot_re_infl <- ggplot(db_US["1945/2020"], aes(x=index(db_US["1945/2020"])))+
  geom_line(aes(y=rev_cpi, colour='Rev. Infl.'),size= 1)+
  geom_line(aes(y=rev_cpi_fe, colour='Rev. Infl. no FE'),size= 1)+
  geom_line(aes(y=rev_pce, colour='Rev. PCE'),size= 1)+
  geom_line(aes(y=rev_pce_fe, colour='Rev. PCE no FE'),size= 1)+
  geom_line(aes(y=rev_defl, colour='Rev. Defl.'),size= 1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Measures of historical inflation')+
  scale_y_continuous()+
  scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))
  
if (flag___plot==0) print(plot_re_infl)

ggsave(plot = plot_re_infl,
      filename='rev_infl.pdf',
      path=graphs_dir,
      device='pdf',
      height = pdf_height, width = pdf_width, units='in')



# Measures of slackness in the economy
plot_slack <- ggplot(db_US["1945/2020"], aes(x=index(db_US["1945/2020"])))+
  geom_line(aes(y=layoffs, colour='Layoff rate'),size= 1)+
  geom_line(aes(y=employment_fluct, colour='NU gap'),size= 1)+
  geom_line(aes(y=realtime_gap, colour='Realtime gap'),size= 1)+
  geom_line(aes(y=expost_gap, colour='ExPost gap'),size= 1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Measures of slackness')+
  scale_y_continuous()+
  scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot==0) print(plot_slack)

ggsave(plot = plot_slack,
       filename='output slack.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')


# Inflation forecasts and nowcasts
plot_nowinf <- ggplot(db_US["1965/2020"], aes(x=index(db_US["1965/2020"])))+
  geom_line(aes(y=cpit, colour='CPI'),size= 1)+
  geom_line(aes(y=coret, colour='Core PCE'),size= 1)+
  geom_line(aes(y=deflt, colour='Deflator'),size= 1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Current period inflation forecasts')+
  scale_y_continuous()+
  scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot==0) print(plot_nowinf)

ggsave(plot = plot_nowinf,
       filename='inflation nowcast.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# one quarter ahead inflation forecasts
plot_hinf <- ggplot(db_US["1965/2020"], aes(x=index(db_US["1965/2020"])))+
  geom_line(aes(y=cpit1, colour='CPI'),size= 1)+
  geom_line(aes(y=coret1, colour='Core PCE'),size= 1)+
  geom_line(aes(y=deflt1, colour='Deflator'),size= 1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('One quarter ahead inflation forecasts')+
  scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot==0) print(plot_hinf)

ggsave(plot = plot_hinf,
       filename='inflation forecasts.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Inflation forecasts coming from SPF
plot_spf_fore <- ggplot(db_US["1980/2020"], aes(x=index(db_US["1980/2020"])))+
  geom_line(aes(y=spf_cpi_h1_mean, colour='SPF CPI mean'),size= 1)+
  geom_line(aes(y=spf_corecpi_h1_mean, colour='SPF core CPI mean'),size= 1)+
  geom_line(aes(y=spf_pce_h1_mean, colour='SPF PCE mean'),size= 1)+
  geom_line(aes(y=spf_corepce_h1_mean, colour='SPF core PCE mean'),size= 1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('One quarter ahead inflation forecasts - SPF cross section means')+
  scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot==0) print(plot_spf_fore)

ggsave(plot = plot_spf_fore,
       filename='SPF inf forecasts.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Inflation forecast disagreement among SPF
plot_spf_iqr <- ggplot(db_US["1980/2020"], aes(x=index(db_US["1980/2020"])))+
  geom_line(aes(y=spf_cpi_h1_iqr, colour='SPF CPI'),size= 1)+
  geom_line(aes(y=spf_corecpi_h1_iqr, colour='SPF core CPI'),size= 1)+
  geom_line(aes(y=spf_pce_h1_iqr, colour='SPF PCE'),size= 1)+
  geom_line(aes(y=spf_corepce_h1_iqr, colour='SPF core PCE'),size= 1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour='IQRs')+
  ggtitle('One quarter ahead inflation forecasts - SPF cross section IQR')+
  scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot==0) print(plot_spf_iqr)

ggsave(plot = plot_spf_iqr,
       filename='disagreement inf forecasts.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Monetary levels
plot_money <- ggplot(db_US["1955/2020"], aes(x=index(db_US["1955/2020"])))+
  geom_line(aes(y=base, colour='Base mon.'),size= 1)+
  geom_line(aes(y=m1, colour='M1'),size= 1)+
  geom_line(aes(y=m2, colour='M2'),size= 1)+
  geom_line(aes(y=m3, colour='M3'),size= 1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Monetary aggregates levels')+
  scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot==0) print(plot_money)

ggsave(plot = plot_money,
       filename='money.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# spreads
plot_spread <- ggplot(db_US["1950/2020"], aes(x=index(db_US["1950/2020"])))+
  geom_line(aes(y=spread_baa, colour='BAA'),size= 1.5)+
  geom_line(aes(y=spread_sp_3m, colour='3m SP'),size= 1)+
  geom_line(aes(y=spread_baa_aaa, colour ='B-A'), size= 1)+
  geom_line(aes(y=spread_baa_long, colour='BAALONG'), size=1)+
  geom_line(aes(y=spread_aaa, colour='AAA'), size=1)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Liquidity spreads - financial instability')+
  scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot==0) print(plot_spread)

ggsave(plot = plot_spread,
       filename='spreads.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')


# Phillips Curve, classic one
plot_phil <- ggplot(db_US, aes(y = rev_cpi, x = unempl_rate, colour = as.Date(index(db_US))))+
      geom_path(size=1) + geom_point(size = 2.5)+
      theme_bw()+xlab('Unemployment rate') + ylab('Revised CPI')+labs(colour = 'Years')+
      ggtitle('Phillips Curve')

if (flag___plot == 0) print(plot_phil)

ggsave(plot = plot_phil,
       filename='phil_curve.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Phillips Curve, layoffs
plot_phil_lay <- ggplot(db_US, aes(y = rev_cpi, x = layoffs, colour = as.Date(index(db_US))))+
  geom_path(size=1) + geom_point(size = 2.5)+
  theme_bw()+xlab('Layoff rate') + ylab('Revised CPI')+labs(colour = 'Years')+
  ggtitle('Phillips Curve - Layoff rate')

if (flag___plot == 0) print(plot_phil_lay)

ggsave(plot = plot_phil_lay,
       filename='phil_curve_lay.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Phillips Curve, employment fluctuations
plot_phil_fluct <- ggplot(db_US, aes(y = rev_cpi, x = employment_fluct, colour = as.Date(index(db_US))))+
  geom_path(size=1) + geom_point(size = 2.5)+
  theme_bw()+xlab('Employment fluctuations') + ylab('Revised CPI')+labs(colour = 'Years')+
  ggtitle('Phillips Curve - Employment Fluctuations around long term rate')

if (flag___plot == 0) print(plot_phil_fluct)

ggsave(plot = plot_phil_fluct,
       filename='phil_curve_fluct.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')


plot_hist_pi <- ggplot(data=db_US)+
  geom_density(aes(x=rev_defl, fill = 'defl'), alpha= .5)+
  geom_density(aes(x=rev_pce, fill = 'pce'), alpha= .5)+
  geom_density(aes(x=rev_cpi, fill = 'cpi'), alpha = .5)+
  labs(' ')+theme_bw()+
  scale_fill_manual( values = c("red","blue", "green"), labels = c('Defl.', 'PCE', 'CPI'), name='Hist. series')+
  xlab('Inflation rates')+
  ggtitle('Distribution of the inflation rates')

if (flag___plot == 0) print(plot_hist_pi)

ggsave(plot = plot_hist_pi,
       filename='pi_kernels.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

plot_defl <- ggplot(db_US["1967/2015"], aes(x=index(db_US["1967/2015"])))+
  geom_line(aes(y = deflt, colour = 't'), size = 1, alpha = .5)+
  geom_line(aes(y = deflt1, colour = 't+1'), size = 1, alpha = .5)+
  geom_line(aes(y = deflt2, colour = 't+2'), size = 1, alpha = .5)+
  geom_line(aes(y = deflt3, colour = 't+3'), size = 1, alpha = .5)+
  geom_line(aes(y = deflt4, colour = 't+4'), size = 1, alpha = .5)+
  geom_line(aes(y = deflt5, colour = 't+5'), size = 1, alpha = .5)+
  geom_line(aes(y = deflt6, colour = 't+6'), size = 1, alpha = .5)+
  geom_line(aes(y = deflt7, colour = 't+7'), size = 1, alpha = .5)+
  geom_line(aes(y = deflt8, colour = 't+8'), size = 1, alpha = .5)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Deflator')+
  scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot == 0) print(plot_defl)

ggsave(plot = plot_defl,
       filename='deflatorh.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')


plot_cpi <- ggplot(db_US["1978/2015"], aes(x=index(db_US["1978/2015"])))+
  geom_line(aes(y = cpit, colour = 't'), size = 1, alpha = .5)+
  geom_line(aes(y = cpit1, colour = 't+1'), size = 1, alpha = .5)+
  geom_line(aes(y = cpit2, colour = 't+2'), size = 1, alpha = .5)+
  geom_line(aes(y = cpit3, colour = 't+3'), size = 1, alpha = .5)+
  geom_line(aes(y = cpit4, colour = 't+4'), size = 1, alpha = .5)+
  geom_line(aes(y = cpit5, colour = 't+5'), size = 1, alpha = .5)+
  geom_line(aes(y = cpit6, colour = 't+6'), size = 1, alpha = .5)+
  geom_line(aes(y = cpit7, colour = 't+7'), size = 1, alpha = .5)+
  geom_line(aes(y = cpit8, colour = 't+8'), size = 1, alpha = .5)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('CPI')+
  scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot == 0) print(plot_cpi)

ggsave(plot = plot_cpi,
       filename='cpih.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')



plot_core <- ggplot(db_US["1985/2015"], aes(x=index(db_US["1985/2015"])))+
  geom_line(aes(y = coret, colour = 't'), size = 1, alpha = .5)+
  geom_line(aes(y = coret1, colour = 't+1'), size = 1, alpha = .5)+
  geom_line(aes(y = coret2, colour = 't+2'), size = 1, alpha = .5)+
  geom_line(aes(y = coret3, colour = 't+3'), size = 1, alpha = .5)+
  geom_line(aes(y = coret4, colour = 't+4'), size = 1, alpha = .5)+
  geom_line(aes(y = coret5, colour = 't+5'), size = 1, alpha = .5)+
  geom_line(aes(y = coret6, colour = 't+6'), size = 1, alpha = .5)+
  geom_line(aes(y = coret7, colour = 't+7'), size = 1, alpha = .5)+
  geom_line(aes(y = coret8, colour = 't+8'), size = 1, alpha = .5)+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('CORE')+
  scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
  theme(axis.text.x = element_text(angle = 90))

if (flag___plot == 0) print(plot_core)

ggsave(plot = plot_core,
       filename='coreh.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# # provocative graph...
# ggplot(db_US["1985/2015"], aes(x=index(db_US["1985/2015"])))+
#   geom_line(aes(y = coret), size = 1, alpha = .5)+
#   geom_line(aes(y = coret1), size = 1, alpha = .5)+
#   geom_line(aes(y = coret2), size = 1, alpha = .5)+
#   geom_line(aes(y = coret3), size = 1, alpha = .5)+
#   geom_line(aes(y = coret4), size = 1, alpha = .5)+
#   geom_line(aes(y = coret5), size = 1, alpha = .5)+
#   geom_line(aes(y = coret6), size = 1, alpha = .5)+
#   geom_line(aes(y = coret7), size = 1, alpha = .5)+
#   geom_line(aes(y = coret8), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit1), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit2), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit3), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit4), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit5), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit6), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit7), size = 1, alpha = .5)+
#   geom_line(aes(y = cpit8), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt1), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt2), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt3), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt4), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt5), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt6), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt7), size = 1, alpha = .5)+
#   geom_line(aes(y = deflt8), size = 1, alpha = .5)+
#   theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
#   ggtitle('really.')+
#   scale_y_continuous()+scale_x_yearqtr(format='%Y Q%q', n=20)+
#   theme(axis.text.x = element_text(angle = 90))


##### LIST OF ADDITIONAL PLOTS #####


# money aggregates growth rates
# kernel density plots for inflations



##### Residuals and results from regressions in USreg.r #####
## of course, source the script beforehand


if (flag___singular==1) source('USreg.r')

# Correlation tab with Stargazer
# formatting

print(stargazer(corr_tab))

# outputs all results in regressions list

for (m in 1:length(regressions$models)){
# for (m in c(1,2,4,5,9,10,11,12,13,14)){  # this for first sample
  # costum function, prints and plots
  # results gathered in regression list
  # and writes results in a txt file
  reg_call(m)
}




##### Plots collector #####
plots <- list(plot_trvars,
              plot_re_infl,
              plot_slack,
              plot_nowinf,
              plot_hinf,
              plot_spf_fore,
              plot_spf_iqr,
              plot_money,
              plot_spread,
              plot_phil,
              plot_phil_lay,
              plot_phil_fluct,
              plot_hist_pi,
              plot_defl,
              plot_cpi,
              plot_core)

##### Housekeeping #####
rm(plot_trvars,
   plot_re_infl,
   plot_slack,
   plot_nowinf,
   plot_hinf,
   plot_spf_fore,
   plot_money,
   plot_spf_iqr,
   plot_spread,
   pdf_height,
   pdf_width,
   plot_phil,
   invsc,
   plot_phil_lay,
   plot_phil_fluct,
   plot_hist_pi,
   plot_defl,
   plot_cpi,
   plot_core
   )


#### PLOTS TO BE REPRODUCED AND STORED ######

#### Comparing measures of uncertainty

# par(mfrow=c(3,1),mar=rep(2, 4))
# attach(ref2)
# plot(na.omit(EPUdisp), type='l', col='red')
# plot(na.omit(spfvar), type='l',col='blue')
# plot(na.omit(spfSD), type='l', col='black')

# plot(spfSD, type='l', col='red', main='spfSD')
# plot(cpinow, type='l', col='blue', main='cpinow')
# plot(cpipost, type='l', col='black', main='cpipost')

# par(mfrow=c(4,1),mar=rep(2, 4))
# plot(spfSD, type='l', col='red', main='spfSD')
# plot(spfrange, type='l', col='blue', main='spfrange')
# plot(spfmedian, type='l', col='black', main='spfmedian')
# plot(spfmean, type='l', col='green', main='spfmean')

# plot(EPUdisp, type='l', col='red', main='EPUdisp')
# plot(spfrange, type='l', col='blue', main='spfrange')
# plot(spfmedian, type='l', col='black', main='spfmedian')
# plot(spfmean, type='l', col='green', main='spfmean')

# par(mfrow=c(2,1),mar=rep(2, 4))
# plot(spfrange, type='l', col='red', main='spfrange')
# plot(EPUdisp, type='l', col='blue', main='EPU')
# par(mfrow=c(1,1))
# detach(ref2)

# dev.off() # resets the device for graphs

# # Full specification of traditional TR
# plot_tayrule<-ggplot()+geom_line(data=ref2, aes(x=time, y=ffr_act, color='FFR'))+
# 	geom_line(data=ref2, aes(x=time, y=cpinow, color='CPI now'))+
# 	geom_line(data=ref2, aes(x=time, y=cpipost, color='CPI post'))+
# 	geom_line(data=ref2, aes(x=time, y=outputpost, color='Y post'))+
# 	geom_line(data=ref2, aes(x=time, y=outputgaphand, color='Y gap'))+
# 	geom_line(data=ref2, aes(x=time, y=trendoutput, color='q-trend'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('All variables')+
# 	theme(legend.position='top')
# print(plot_tayrule)

# # Parismonious TR
# plot_tayrule2<-ggplot()+geom_line(data=ref2, aes(x=time, y=ffr_act, color='FFR'))+
# 	geom_line(data=ref2, aes(x=time, y=cpinow, color='CPI now'))+
# 	geom_line(data=ref2, aes(x=time, y=outputgaphand, color='Y gap'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('Parismonious TR spec')+
# 	theme(legend.position='top')
# print(plot_tayrule2)

# # TR with Philly outputgap
# plot_tayrule3<-ggplot()+geom_line(data=ref2, aes(x=time, y=ffr_act, color='FFR'))+
# 	geom_line(data=ref2, aes(x=time, y=cpinow, color='CPI now'))+
# 	geom_line(data=ref2, aes(x=time, y=outputgaphilly, color='Y gap (Philly)'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('TR with official Philly Ygap')+
# 	theme(legend.position='top')
# print(plot_tayrule3)


# # Philly outputgap vs FRED (potential - actual)
# plot_gap1<-ggplot()+geom_line(data=ref2, aes(x=time, y=outputgaphilly, color='Philly'))+
# 	geom_line(data=ref2, aes(x=time, y=outputgaphand, color='Fred'))+
# 	geom_line(data=ref2, aes(x=time, y=trendoutput, color='q-trend'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('Cfr Philly vs crude Fred vs q-trend')+
# 	theme(legend.position='top')
# print(plot_gap1)

# plot_gap2<-ggplot()+geom_line(data=ref2, aes(x=time, y=outputgaphand, color='Fred'))+
#   geom_line(data=ref2, aes(x=time, y=trendoutput, color='q-trend'))+
#   theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('crude Fred vs q-trend')+
#   theme(legend.position='top')
# print(plot_gap2)

# # check the consistency of longer over shorter
# fed<-lm(outputgaphilly~outputgaphand, data=ref2)
# summary(fed)
# q_trend<-lm(data=ref2, outputgaphilly~trendoutput)
# summary(q_trend)
# crafted <- lm(trendoutput~outputgaphand, ref2)
# summary(crafted)
# # outputgaphand has a greater explanatory power with respect to the official series

# rm(crafted, fed, q_trend)

# # CPI discrepancies
# plot_cpis<-ggplot()+geom_line(data=ref2, aes(y=cpinow, x=time, color='now'))+
# 	geom_line(data=ref2, aes(y=cpipost, x=time, color='post'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color='CPIs')+ggtitle('CPI\'s comparison')+
# 	theme(legend.position='top')
# print(plot_cpis)

# regcpi<-lm(cpipost~cpinow, data=ref2)
# summary(regcpi)
# # almost one-to-one relation
# rm(regcpi)

# # TR with spf
# plot_tayspf<-ggplot()+geom_line(data=ref2, aes(x=time, y=ffr_act, color='FFR'))+
# 	geom_line(data=ref2, aes(x=time, y=cpinow, color='CPI now'))+
# 	geom_line(data=ref2, aes(x=time, y=spfrange, color='spfrange'))+
# 	geom_line(data=ref2, aes(x=time, y=spfvar, color='spfvar'))+
# 	geom_line(data=ref2, aes(x=time, y=spfSD, color='spfSD'))+
# 	geom_line(data=ref2, aes(x=time, y=EPUdisp/100, color='EPU'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('TR with SPF measures')+
# 	theme(legend.position='top')
# print(plot_tayspf)

# #SFPVAR alone
# plot_spfvari<-ggplot(data=ref2, aes(x=time, y=spfvar, color='SPF variance'))+geom_line()+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+theme(legend.position='top')
# print(plot_spfvari)

# # comparison of different risk/uncertainty measures

# plot_risk<-ggplot()+geom_line(data=ref2, aes(x=time, y=scale(vix), color='VIX'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(EPUdisp), color='EPU'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfrange), color='spfrange'))+
# 	theme_bw()+ggtitle('Risk & uncertainty')+labs(' ')+ylab(' ')+theme(legend.position='top')
# print(plot_risk)

# plot_fullrisk<-ggplot()+geom_line(data=ref2, aes(x=time, y=scale(vix), color='VIX'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(EPUdisp), color='EPU'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(vixdif), color='vixdif'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfmedian), color='spfmedian'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfmean), color='spfmean'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfSD), color='spfSD'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfrange), color='spfrange'))+
# 	theme_bw()+ggtitle('Risk & uncertainty')+labs('leg.')+ylab(' ')+
# 	theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_fullrisk)

# # Measures of inflation at different timings and w/ or w/o crisis
# plot_inflationnow <- ggplot()+
# 	geom_line(data=db, aes(x=index(db), y=cpinow, color='cpinow'))+
# 	geom_line(data=db, aes(x=index(db), y=cpit, color='cpit'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# plot_inflationahead <- ggplot()+
# 	geom_line(data=db, aes(x=index(db), y=cpif, color='cpif'))+
# 	geom_line(data=db, aes(x=index(db), y=cpit1, color='cpit1'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# plot_interinflation <- ggplot()+
# 	geom_line(data=db, aes(x=index(db), y=cpit1, color='cpit1'))+
# 	geom_line(data=db, aes(x=index(db), y=cpit, color='cpit'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# print(plot_inflationnow)
# print(plot_inflationahead)
# print(plot_interinflation)


# plot_corefull <- ggplot()+
# 	geom_line(data=db, aes(x=index(db), y=coret, color='core'))+
# 	geom_line(data=db, aes(x=index(db), y=deflt, color='defl'))+
# 	geom_line(data=db, aes(x=index(db), y=cpit, color='cpi'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# plot_corefullnogfc <- ggplot()+
# 	geom_line(data=nogfc, aes(x=index(nogfc), y=coret1, color='core'))+
# 	geom_line(data=nogfc, aes(x=index(nogfc), y=deflt1, color='defl'))+
# 	geom_line(data=nogfc, aes(x=index(nogfc), y=cpit1, color='cpi'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# print(plot_corefull)
# print(plot_corefullnogfc)

# # plots of money aggregates

# plot_money <- ggplot()+geom_line(data=db, aes(y=m1, x=index(db), color='M1'))+
# 	geom_line(data=db, aes(y=m2, x=index(db), color='M2'))+
# 	geom_line(data=db, aes(y=curr, x=index(db), color='Tot. Curr.'))+
# 	geom_line(data=db, aes(y=base, x=index(db), color='Mon. Base'))+
# 	theme_bw()+labs('Monetary aggregates')+ylab('Bls of $')+xlab(' ')+
# 	theme(legend.position='top')
# print(plot_money)

# # plots of fiscal variables
# plot_debtgraph <- ggplot()+geom_line(data=db, aes(x=index(db), y=debt))+
# 	theme_bw()+labs(title='Debt level')+ylab('Bls of $')+xlab(' ')
# print(plot_debtgraph)

# plot_surplusgraph <- ggplot()+geom_line(data=db, aes(x=index(db), y=surplus), color='red')+
# 	theme_bw()+labs(title='Surplus as % of GDP')+ylab(' ')+xlab(' ')+geom_hline( yintercept = 0)
# print(plot_surplusgraph)

# # uncertainty measured as FFR range
# plot_ffr_range <- ggplot()+geom_line(data=db, aes(x=index(db), y=spfrange*3, color='SPF Range'))+
# 	geom_line(data=db, aes(x=index(db), y=ffr_act, color='FFR'))+
# 	theme_bw()+labs(' ')+ylab(' ')+xlab(' ')+
# 	theme(legend.position='top')#+  guides(colour = guide_legend(override.aes = list(size=3))) 
# print(plot_ffr_range)

# # principal rates in game
# plot_rates <- ggplot()+geom_line(data=db, aes(x=index(db), y=deflt1, color='Deflator h=1'))+
# 	geom_line(data=db, aes(x=index(db), y=ffr_act, color='FFR'))+
# 	theme_bw()+ggtitle('Rates')+labs('leg.')+ylab(' ')+xlab(' ')+
# 	theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_rates)

# # plotting data from Michigan survey
# plot_soc_mich_infl <- ggplot()+geom_line(data = db3, aes(x=index(db3), y=deflt4, color='Deflator h=4'))+
#   geom_line(data = db3, aes(x=index(db3), y=soc_e_cpi_1y_ahead, color='SOC inflation expectation')) +
#   geom_line(data = db3, aes(x=index(db3), y=ffr_act, color='FFR')) +
#   theme_bw()+ggtitle('SOC vs SPF')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# plot(plot_soc_mich_infl)

# # levels for SOC sentiment indexes
# plot_soc_lvl <- ggplot()+geom_line(data = db3, aes(x=index(db3), y=actual_soc_ind, color='Actual SOC lvl'))+
#   geom_line(data = db3, aes(x=index(db3), y=expected_soc_ind, color='Expected SOC lvl')) +
#   theme_bw()+ggtitle('SOC Levels')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# plot(plot_soc_lvl)

# # inflation, SOC lvl diff, FFR
# plot_soc_diff <-ggplot()+ geom_line(data = db3, aes(x=index(db3), y=ffr_act, color='FFR'))+
#   geom_line(data = db3, aes(x=index(db3), y=diff_soc_ind, color='SOC lvl difference')) +
#   geom_line(data = db3, aes(x=index(db3), y=deflt1, color='Deflator h=1')) +
#   theme_bw()+ggtitle('Inflation, SOC, FFR')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_soc_diff)

# # inflation, SOC perch, FFR
# plot_soc_perch <-ggplot()+ geom_line(data = db3, aes(x=index(db3), y=ffr_act, color='FFR'))+
#   geom_line(data = db3, aes(x=index(db3), y=perch_actual_soc_ind, color='Actual SOC perc. ch.')) +
#   geom_line(data = db3, aes(x=index(db3), y=perch_expected_soc_ind, color='Expected SOC perc. ch.')) +
#   geom_line(data = db3, aes(x=index(db3), y=deflt1, color='Deflator h=1')) +
#   theme_bw()+ggtitle('Inflation, SOC percentage change, FFR')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_soc_perch)


# plot_shffr <- ggplot()+geom_line(data=db_shadow, aes(x=index(db_shadow), y=ffr_act, color='FFR'))+
#   geom_line(data=db_shadow, aes(x=index(db_shadow), y=shffr, color= 'Wu Xia Shadow rate'))+
#   geom_line(data = db_shadow, aes(x=index(db_shadow), y=deflt1, color='Deflator h=1')) +
#   theme_bw()+ggtitle('Deflator, FFR, Wu Xia Shadow rate')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_shffr)

### housekeeping
# rm(pat, ) 