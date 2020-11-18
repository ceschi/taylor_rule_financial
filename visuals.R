############## Visualization for TR study ###############

if (flag___singular==1) library(ggplot2, xts)


##### Plots of general variables #####

# scale for LaTeX plots
# /1 to have bigger ones
# good for nontext vis

invsc <- 1.5
pdf_width = 14.6/invsc; pdf_height = 8/invsc

db_US <- plotter

# TR variables
plot_trvars <- ggplot(db_US["1945/2020"], aes(x=index(db_US["1945/2020"])))+
  geom_line(aes(y=ffr, color='FFR'),  alpha = .8)+
  geom_line(aes(y=rev_defl_pch, color='Act. Infl.'),  alpha = .8)+
  geom_line(aes(y=deflt1, color='Exp. Infl.'),  alpha = .8)+
  geom_line(aes(y=realtime_gap, color='Gap'),  alpha = .8)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('US Taylor rule - main components')+
  scale_y_continuous()+
  scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
    legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_trvars)

ggsave(plot_trvars,
       filename='TRvars_col.pdf',
       path = graphs_dir, 
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Measures of inflation, revised ones
plot_re_infl <- ggplot(db_US["1945/2020"], aes(x=index(db_US["1945/2020"])))+
  geom_line(aes(y=rev_cpi_pch, colour='Rev. Infl.'),size= 1)+
  geom_line(aes(y=rev_cpi_fe_pch, colour='Rev. Infl. no FE'),size= 1)+
  geom_line(aes(y=rev_pce_pch, colour='Rev. PCE'),size= 1)+
  geom_line(aes(y=rev_pce_fe_pch, colour='Rev. PCE no FE'),size= 1)+
  geom_line(aes(y=rev_defl_pch, colour='Rev. Defl.'),size= 1)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Measures of historical inflation')+
  scale_y_continuous()+
  scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))
  
if (flag___plot==0) print(plot_re_infl)

ggsave(plot = plot_re_infl,
      filename='rev_infl.pdf',
      path=graphs_dir,
      device='pdf',
      height = pdf_height, width = pdf_width, units='in')



# Measures of slackness in the economy
plot_slack <- ggplot(db_US["1945/2020"], aes(x=index(db_US["1945/2020"])))+
  geom_line(aes(y=-layoffs, colour='(-)Layoff rate'),size= 1)+
  geom_line(aes(y=-employment_fluct, colour='(-)NU gap'),size= 1)+
  geom_line(aes(y=realtime_gap, colour='Realtime gap'),size= 1)+
  geom_line(aes(y=expost_gap, colour='ExPost gap'),size= 1)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Measures of slackness')+
  scale_y_continuous()+
  scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_slack)

ggsave(plot = plot_slack,
       filename='output_slack.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')


# Inflation forecasts and nowcasts
plot_nowinf <- ggplot(db_US["1965/2020"], aes(x=index(db_US["1965/2020"])))+
  geom_line(aes(y=cpit, colour='CPI'),size= 1)+
  geom_line(aes(y=coret, colour='Core PCE'),size= 1)+
  geom_line(aes(y=deflt, colour='Deflator'),size= 1)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Current period inflation forecasts')+
  scale_y_continuous()+
  scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_nowinf)

ggsave(plot = plot_nowinf,
       filename='inflation_nowcast.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# one quarter ahead inflation forecasts
plot_hinf <- ggplot(db_US["1965/2020"], aes(x=index(db_US["1965/2020"])))+
  geom_line(aes(y=cpit1, colour='CPI'),size= 1)+
  geom_line(aes(y=coret1, colour='Core PCE'),size= 1)+
  geom_line(aes(y=deflt1, colour='Deflator'),size= 1)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('One quarter ahead inflation forecasts')+
  scale_y_continuous()+scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_hinf)

ggsave(plot = plot_hinf,
       filename='inflation_forecasts.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Inflation forecasts coming from SPF
plot_spf_fore <- ggplot(db_US["1980/2020"], aes(x=index(db_US["1980/2020"])))+
  geom_line(aes(y=spf_cpi_h1_mean, colour='SPF CPI mean'),size = .8)+
  geom_line(aes(y=spf_corecpi_h1_mean, colour='SPF core CPI mean'),size = .8)+
  geom_line(aes(y=spf_pce_h1_mean, colour='SPF PCE mean'),size = .8)+
  geom_line(aes(y=spf_corepce_h1_mean, colour='SPF core PCE mean'),size = .8)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('One quarter ahead inflation forecasts - SPF cross section means')+
  scale_y_continuous()+scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_spf_fore)

ggsave(plot = plot_spf_fore,
       filename='SPF_inf_forecasts.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Inflation forecast disagreement among SPF
plot_spf_iqr <- ggplot(db_US["1980/2020"], aes(x=index(db_US["1980/2020"])))+
  geom_line(aes(y=spf_cpi_h1_iqr, colour='SPF CPI'),size = .8)+
  geom_line(aes(y=spf_corecpi_h1_iqr, colour='SPF core CPI'),size = .8)+
  geom_line(aes(y=spf_pce_h1_iqr, colour='SPF PCE'),size = .8)+
  geom_line(aes(y=spf_corepce_h1_iqr, colour='SPF core PCE'),size = .8)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour='IQRs')+
  ggtitle('One quarter ahead inflation forecasts - SPF cross section IQR')+
  scale_y_continuous()+scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_spf_iqr)

ggsave(plot = plot_spf_iqr,
       filename='disagreement_inf_forecasts.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Monetary growth rates
plot_money <- ggplot(db_US["1955/2020"], aes(x=index(db_US["1955/2020"])))+
  geom_line(aes(y=base_g, colour='Base mon.'), size = .8)+
  geom_line(aes(y=m1_g, colour='M1'), size = .8)+
  geom_line(aes(y=m2_g, colour='M2'), size = .8)+
  geom_line(aes(y=m3_g, colour='M3'), size = .8)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Monetary aggregates growth rates')+
  scale_y_continuous()+scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

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
  geom_line(aes(y=spread_aaa, colour='AAA'),  alpha = .8)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Liquidity spreads - financial instability')+
  scale_y_continuous()+scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_spread)

ggsave(plot = plot_spread,
       filename='spreads.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')


# Phillips Curve, classic one
plot_phil <- ggplot(db_US, aes(y = rev_cpi_pch, x = unempl_rate, colour = as.Date(index(db_US))))+
      geom_path(size = .8) + geom_point(size = 2.5)+
      theme_minimal()+xlab('Unemployment rate') + ylab('Revised CPI')+labs(colour = 'Years')+
      ggtitle('Phillips Curve') +
      theme(legend.position = 'bottom') +
      guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot == 0) print(plot_phil)

ggsave(plot = plot_phil,
       filename='phil_curve.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Phillips Curve, layoffs
plot_phil_lay <- ggplot(db_US, aes(y = rev_cpi_pch, x = layoffs, colour = as.Date(index(db_US))))+
  geom_path(size = .8) + geom_point(size = 2.5)+
  theme_minimal()+xlab('Layoff rate') + ylab('Revised CPI')+labs(colour = 'Years')+
  ggtitle('Phillips Curve - Layoff rate')+
  theme(legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot == 0) print(plot_phil_lay)

ggsave(plot = plot_phil_lay,
       filename='phil_curve_lay.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Phillips Curve, employment fluctuations
plot_phil_fluct <- ggplot(db_US, aes(y = rev_cpi_pch, x = employment_fluct, colour = as.Date(index(db_US))))+
  geom_path(size = .8) + geom_point(size = 2.5)+
  theme_minimal()+xlab('Employment fluctuations') + ylab('Revised CPI')+labs(colour = 'Years')+
  ggtitle('Phillips Curve - Employment Fluctuations around long term rate')+
  theme(legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot == 0) print(plot_phil_fluct)

ggsave(plot = plot_phil_fluct,
       filename='phil_curve_fluct.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')


plot_hist_pi <- ggplot(data=db_US)+
  geom_density(aes(x=rev_defl_pch, fill = 'defl'), alpha= .5)+
  geom_density(aes(x=rev_pce_pch, fill = 'pce'), alpha= .5)+
  geom_density(aes(x=rev_cpi_pch, fill = 'cpi'), alpha = .5)+
  labs(' ')+theme_minimal()+
  scale_fill_viridis_d(labels = c('Defl.', 'PCE', 'CPI'), name='Hist. series')+
  xlab('Inflation rates')+
  ggtitle('Distribution of the inflation rates')+
  theme(legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot == 0) print(plot_hist_pi)

ggsave(plot = plot_hist_pi,
       filename='pi_kernels.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

plot_defl <- ggplot(db_US["1967/2015"], aes(x=index(db_US["1967/2015"])))+
  geom_line(aes(y = deflt, colour = 't'), size = .8, alpha = .5)+
  geom_line(aes(y = deflt1, colour = 't+1'), size = .8, alpha = .5)+
  geom_line(aes(y = deflt2, colour = 't+2'), size = .8, alpha = .5)+
  geom_line(aes(y = deflt3, colour = 't+3'), size = .8, alpha = .5)+
  geom_line(aes(y = deflt4, colour = 't+4'), size = .8, alpha = .5)+
  geom_line(aes(y = deflt5, colour = 't+5'), size = .8, alpha = .5)+
  geom_line(aes(y = deflt6, colour = 't+6'), size = .8, alpha = .5)+
  geom_line(aes(y = deflt7, colour = 't+7'), size = .8, alpha = .5)+
  geom_line(aes(y = deflt8, colour = 't+8'), size = .8, alpha = .5)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('Deflator')+
  scale_y_continuous()+scale_x_date() + 
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot == 0) print(plot_defl)

ggsave(plot = plot_defl,
       filename='deflatorh.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')


plot_cpi <- ggplot(db_US["1978/2015"], aes(x=index(db_US["1978/2015"])))+
  geom_line(aes(y = cpit, colour = 't'), size = .8, alpha = .5)+
  geom_line(aes(y = cpit1, colour = 't+1'), size = .8, alpha = .5)+
  geom_line(aes(y = cpit2, colour = 't+2'), size = .8, alpha = .5)+
  geom_line(aes(y = cpit3, colour = 't+3'), size = .8, alpha = .5)+
  geom_line(aes(y = cpit4, colour = 't+4'), size = .8, alpha = .5)+
  geom_line(aes(y = cpit5, colour = 't+5'), size = .8, alpha = .5)+
  geom_line(aes(y = cpit6, colour = 't+6'), size = .8, alpha = .5)+
  geom_line(aes(y = cpit7, colour = 't+7'), size = .8, alpha = .5)+
  geom_line(aes(y = cpit8, colour = 't+8'), size = .8, alpha = .5)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('CPI')+
  scale_y_continuous()+scale_x_date() + 
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot == 0) print(plot_cpi)

ggsave(plot = plot_cpi,
       filename='cpih.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')



plot_core <- ggplot(db_US["1985/2015"], aes(x=index(db_US["1985/2015"])))+
  geom_line(aes(y = coret, colour = 't'), size = .8, alpha = .5)+
  geom_line(aes(y = coret1, colour = 't+1'), size = .8, alpha = .5)+
  geom_line(aes(y = coret2, colour = 't+2'), size = .8, alpha = .5)+
  geom_line(aes(y = coret3, colour = 't+3'), size = .8, alpha = .5)+
  geom_line(aes(y = coret4, colour = 't+4'), size = .8, alpha = .5)+
  geom_line(aes(y = coret5, colour = 't+5'), size = .8, alpha = .5)+
  geom_line(aes(y = coret6, colour = 't+6'), size = .8, alpha = .5)+
  geom_line(aes(y = coret7, colour = 't+7'), size = .8, alpha = .5)+
  geom_line(aes(y = coret8, colour = 't+8'), size = .8, alpha = .5)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('CORE')+
  scale_y_continuous()+scale_x_date() + 
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot == 0) print(plot_core)

ggsave(plot = plot_core,
       filename='coreh.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

##### Shadow rates #############################################################


plot_shadow <- ggplot(db_US["1954-01/"], aes(x = index(db_US["1954-01/"])))+
  geom_hline(aes(yintercept=0))+
  geom_line(aes(y = shffr, colour = 'Wu Xia'), size = 1.2, alpha = .7)+
  geom_line(aes(y = kripp_shffr, colour = 'Krippner'), size = 1.2, alpha = .7)+
  theme_minimal()+xlab(' ')+ylab(' ')+ labs(colour = "")+
  ggtitle('Shadow rates')+
  scale_y_continuous()+scale_x_date() + 
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1, byrow=TRUE)) +
  geom_vline(xintercept = as.Date("1995-01-01"), linetype = 'dashed') +
  geom_text(aes(x = as.Date("1996-06-30"), y = 12), label = 'Krippner starts', angle = 90, size = 3)+
  geom_vline(xintercept = as.Date("2007-01-01"), linetype = 'dashed') +
  geom_text(aes(x = as.Date("2008-06-30"), y = 12), label = 'Wu-Xia starts', angle = 90, size = 3)+
  geom_vline(xintercept = as.Date("2015-12-31"), linetype = 'dashed') +
  geom_text(aes(x = as.Date("2016-12-31"), y = 12), label = 'Wu-Xia ends', angle = 90, size = 3)

if (flag___plot == 0) print(plot_shadow)

ggsave(plot = plot_shadow,
       filename='shadow_rates.pdf',
       path=graphs_dir,
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

##### LIST OF ADDITIONAL PLOTS #################################################


plot_trvars_all <- ggplot(db_US["1945/2020"], aes(x=index(db_US["1945/2020"])))+
  geom_line(aes(y=ffr, color='FFR'),  alpha = .8)+
  geom_line(aes(y=rev_defl_pch, color='Act. Infl.'),  alpha = .8)+
  geom_line(aes(y=deflt1, color='Exp. Infl.'),  alpha = .8)+
  geom_line(aes(y=realtime_gap, color='Gap'),  alpha = .8)+
  geom_line(aes(y=spread_baa, color='BAA'),  alpha = .8)+
  geom_line(aes(y=spread_sp_3m, color='S&P'),  alpha = .8)+
  theme_minimal()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('US Taylor rule - liquidity augmented')+
  scale_y_continuous()+
  scale_x_date() + 
  geom_hline(yintercept = 0, colour='black')+
  theme(axis.text.x = element_text(angle = 45), 
        legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_trvars)

ggsave(plot_trvars_all,
       filename='TRvars_all_col.pdf',
       path = graphs_dir, 
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

##### Residuals and results from regressions in USreg.r #####
## of course, source the script beforehand


if (flag___singular==1) source('USreg.r')

# Correlation tab with Stargazer
# formatting
stargazer(corr_tab)

# outputs all results in regressions list

for (m in 1:length(regressions$models)){
# for (m in c(1,2,4,5,9,10,11,12,13,14)){  # this for first sample
  # costum function, prints and plots
  # results gathered in regression list
  # and writes results in a txt file
  reg_call(m)
}



# plots for pub -----------------------------------------------------------

# first off just reshape the db to tidy long format with variables of interest

trvars <- db_US %>% 
  xts_tbl() %>% 
  select(date, ffr, rev_defl_pch, deflt1, realtime_gap) %>% 
  tidyr::pivot_longer(cols = -date, names_to = 'var', values_to = 'val') %>% 
  na.omit(.)

trvars_all <- db_US %>% 
  xts_tbl() %>% 
  select(date, ffr, rev_defl_pch, deflt1, realtime_gap, spread_baa, spread_sp_3m) %>% 
  tidyr::pivot_longer(cols = -date, names_to = 'var', values_to = 'val') %>% 
  na.omit(.)

llbls <- c(
  deflt1 = 'Exp. Infl.',
  ffr = 'FFR',
  realtime_gap = 'Gap',
  rev_defl_pch = 'Act. Infl.',
  spread_baa = 'BAA',
  spread_sp_3m = 'S&P'
)



# Fig.1
plot_trvars <- ggplot(trvars, aes(x = date, y = val, colour = var)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(linetype = var), size = .5, alpha = 1) +
  geom_point(aes(shape = var), size = 1, alpha = 1) +
  theme_minimal() + xlab('') + ylab('') +
  ggtitle('US Taylor Rule - Main Components') +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.key.size = unit(2, 'cm')) +
  scale_shape(labels = llbls) +
  scale_linetype(labels = llbls) +
  guides(colour=guide_legend(nrow=1,byrow=TRUE)) +
  scale_colour_viridis_d(labels = llbls,
                         end = .7,
                         option = 'A')

if (flag___plot==0) print(plot_trvars)

ggsave(plot_trvars,
       filename='TRvars.pdf',
       path = graphs_dir, 
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')

# Fig.2
plot_trvars_all <- ggplot(trvars_all, aes(x = date, y = val, colour = var)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(linetype = var), size = .5, alpha = 1) +
  geom_point(aes(shape = var), size = 1, alpha = 1) +
  theme_minimal() + xlab('') + ylab('') +
  ggtitle('US Taylor Rule - Main Components') +
  theme(legend.position = 'bottom', 
        legend.title = element_blank()) +
  scale_shape(labels = llbls) +
  scale_linetype(labels = llbls) +
  scale_colour_viridis_d(labels = llbls,
                         end = .7,
                         option = 'A') +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

if (flag___plot==0) print(plot_trvars)

ggsave(plot_trvars_all,
       filename='TRvars_all.pdf',
       path = graphs_dir, 
       device='pdf',
       height = pdf_height, width = pdf_width, units='in')




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
              plot_core,
              plot_shadow,
              plot_trvars_all)

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
   plot_core,
   plot_shadow,
   plot_trvars_all,
   plotter,
   llbls, 
   m,
   trvars,
   trvars_all
   )
