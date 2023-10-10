library(tidyverse)
library(lfe)
library(stargazer)
library(huxtable)
library(lmtest)
library(sandwich)
library(caret)
library(lubridate)
library(spatstat)
library(plm)
library(haven)
library(data.table)
library(fixest)
library(glue)
library(ggplot2)
library(ggthemes)

##---- Sunab Event Study ----

munip_exp = read_csv('D://Thesis/Data/munip_exp_to_R.csv') #Update before run
munip_rev = read_csv('D://Thesis/Data/munip_rev_to_R.csv') #Update before run
munip_exp_2009 = munip_exp[as.integer(substring(munip_exp$FY_Year, 3, 7), 'int') >= 2010, ]
munip_rev_2009 = munip_rev[as.integer(substring(munip_rev$FY_Year, 3, 7), 'int') >= 2010, ]
sum(is.na(munip_exp_2009$Population_Imputed_1))
sum(is.na(munip_rev_2009$Population_Imputed_1))

#data.frame(summarize(group_by(munip_exp_2009, ADM2_PCODE), n()))[data.frame(summarize(group_by(munip_exp_2009, ADM2_PCODE), n())) == 10]
#keep_no_pop = list(unique(munip_exp_2009[is.na(summarize(group_by(munip_exp_2009, ADM2_PCODE), n())[summarize(group_by(munip_exp_2009, ADM2_PCODE), n()) == 10], 'ADM2_PCODE']))
#munip_exp_2009 = munip_exp_2009[!unlist(munip_exp_2009$ADM2_PCODE) %in% unlist(drop_no_pop), ]
#drop_no_pop = list(unique(munip_rev_2009[is.na(munip_rev_2009$Population_Imputed_1), 'ADM2_PCODE']))
#munip_rev_2009 = munip_rev_2009[!unlist(munip_rev_2009$ADM2_PCODE) %in% unlist(drop_no_pop), ]
#drop_no_unemp = list(unique(munip_exp_2009[is.na(munip_exp_2009$Unemployment_Rate_Imputed_2), 'ADM2_PCODE']))
#munip_exp_2009 = munip_exp_2009[!unlist(munip_exp_2009$ADM2_PCODE) %in% unlist(drop_no_unemp), ]
#drop_no_unemp = list(unique(munip_rev_2009[is.na(munip_rev_2009$Unemployment_Rate_Imputed_2), 'ADM2_PCODE']))
#munip_rev_2009 = munip_rev_2009[!unlist(munip_rev_2009$ADM2_PCODE) %in% unlist(drop_no_unemp), ]
#drop_no_educ = list(unique(munip_exp_2009[is.na(munip_exp_2009$No_of_elementary_school_percap_1), 'ADM2_PCODE']))
#munip_exp_2009 = munip_exp_2009[!unlist(munip_exp_2009$ADM2_PCODE) %in% unlist(drop_no_educ), ]
#drop_no_educ = list(unique(munip_rev_2009[is.na(munip_rev_2009$No_of_elementary_school_percap_1), 'ADM2_PCODE']))
#munip_rev_2009 = munip_rev_2009[!unlist(munip_rev_2009$ADM2_PCODE) %in% unlist(drop_no_educ), ]

colnames(munip_exp_2009)[7] = 'Social_welfare_municipalities_Exp'
colnames(munip_exp_2009)[8] = 'Disaster_relief_municipalities_Exp'
colnames(munip_exp_2009)[10] = 'Health_and_sanitation_municipalities_Exp'
colnames(munip_exp_2009)[12] = 'Unemployment_measures_municipalities_Exp'
colnames(munip_exp_2009)[13] = 'Civil_engineering_works_municipalities_Exp'
colnames(munip_exp_2009)[14] = 'Roads_and_bridges_municipalities_Exp'
colnames(munip_exp_2009)[16] = 'Financial_strength_index'
colnames(munip_exp_2009)[17] = 'Ratio_of_net_excess_of_revenue'
colnames(munip_exp_2009)[18] = 'Ratio_of_current_expenditure_to_revenue'
colnames(munip_exp_2009)[19] = 'Basic_financial_revenue'
colnames(munip_exp_2009)[20] = 'Settlement_of_total_revenue'
colnames(munip_exp_2009)[21] = 'Amount_of_self_financial_resources'
colnames(munip_exp_2009)[22] = 'Settlement_of_total_expenditure'

list_for_loop_2009 = list()

revenues_for_event_study = colnames(munip_rev_2009)[6:15]
expenditures_for_event_study = colnames(munip_exp_2009)[6:22]

munip_exp_2009['time_to_treat_5high'] = ifelse(munip_exp_2009$First_Eq_Period_5high_FY_2010 > 100, 10000, munip_exp_2009$First_Eq_Period_5high_FY_2010)
munip_exp_2009['time_to_treat_5higher'] = ifelse(munip_exp_2009$First_Eq_Period_5higher_FY_2010 > 100, 10000, munip_exp_2009$First_Eq_Period_5higher_FY_2010)
munip_exp_2009['time_to_treat_6low'] = ifelse(munip_exp_2009$First_Eq_Period_6low_FY_2010 > 100, 10000, munip_exp_2009$First_Eq_Period_6low_FY_2010)
munip_exp_2009['time_to_treat_6low_higher'] = ifelse(munip_exp_2009$First_Eq_Period_6low_higher_FY_2010 > 100, 10000, munip_exp_2009$First_Eq_Period_6low_higher_FY_2010)
per_cap_indicator = list(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                         TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

munip_rev_2009['time_to_treat_5high'] = ifelse(munip_rev_2009$First_Eq_Period_5high_FY > 100, 10000, munip_rev_2009$First_Eq_Period_5high_FY)
munip_rev_2009['time_to_treat_5higher'] = ifelse(munip_rev_2009$First_Eq_Period_5higher_FY > 100, 10000, munip_rev_2009$First_Eq_Period_5higher_FY)
munip_rev_2009['time_to_treat_6low'] = ifelse(munip_rev_2009$First_Eq_Period_6low_FY > 100, 10000, munip_rev_2009$First_Eq_Period_6low_FY)
munip_rev_2009['time_to_treat_6low_higher'] = ifelse(munip_rev_2009$First_Eq_Period_6low_higher_FY > 100, 10000, munip_rev_2009$First_Eq_Period_6low_higher_FY)

##---- EXPENDITURE PLOTS 6low----

munip_exp_2009 = drop_na(munip_exp_2009, c(No_of_lower_secondary_school_teacher_percap_1, Unemployment_Rate_Imputed_2, Population_Imputed_1))

form <- function(x, y, string){
  as.formula(glue(paste('{x} ~ ', 'sunab({y}, Period_Now)', string, sep = ''))) 
}

eq = 'time_to_treat_6low_higher'
s = '+ 
  Population_Imputed_1 + Unemployment_Rate_Imputed_2 +
  No_of_elementary_school_percap_1 + NO_of_lower_secondary_school_percap_1 +
  No_of_upper_secondary_schools_percap_1 + No_of_elementary_school_Teacher_1 +
  No_of_lower_secondary_school_teacher_percap_1 + T30_I50_PS_mean | Period_Now'

coefs = c('Period_Now::-5', 'Period_Now::-4', 'Period_Now::-3', 'Period_Now::-2', 'Period_Now::0', 
          'Period_Now::1', 'Period_Now::2', 'Period_Now::3', 'Period_Now::4', 'Period_Now::5',
          'Period_Now::6', 'Period_Now::7')

row_names = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7)

filter_drop = unique(munip_exp_2009[is.na(munip_exp_2009['Welfare_municipalities_Exp']), 'ADM2_PCODE'])
data = munip_exp_2009[!unlist(munip_exp_2009$ADM2_PCODE) %in% unlist(filter_drop), ]

welf_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

welf_munip = rbind(welf_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

disa_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

disa_munip = rbind(disa_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

labr_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

labr_munip = rbind(labr_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

civl_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

civl_munip = rbind(civl_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

road_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

road_munip = rbind(road_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dwel_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

dwel_munip = rbind(dwel_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

ratio_munip = data.table('period' = row_names, 
                         'coefs' = coef(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                              ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                         'se' = feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                      ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                         'lower' = confint(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                                 ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                         'upper' = confint(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                                 ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

ratio_munip = rbind(ratio_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

reve_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

reve_munip = rbind(reve_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

expe_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

expe_munip = rbind(expe_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dfs = list(welf_munip, 
           disa_munip,
           labr_munip,
           expe_munip, 
           civl_munip,
           road_munip,
           dwel_munip,
           ratio_munip,
           reve_munip)

ylabel = c('Log welfare expenditure per capita', 
           'Disaster relief expenditure per capita',
           'Log labor expenditure per capita',
           'Log total expenditure per capita',
           'Log civil engineering expenditures per capita',
           'Log road / bridge expenditures per capita',
           'Dwelling expenditures per capita',
           'Net Excess of Revenue Ratio',
           'Log total revenue per capita')

##---- EXPENDITURE PLOTS LOOPS 6low----

file_names = c('Welfare_Exp', 'Disaster_Relief', 'Labor_Exp', 'Total_Exp', 'Civil_Eng_Exp', 'Road_Bridge_Exp', 'Dwelling_Exp',
               'Net_Excess_Ratio', 'Total_Revenue')

ratio = c(1200, 1200, 1200, 2400, 1200, 1200, 1200, 1200, 2400)
ratio_2 = c(915, 915, 915, 1050, 915, 915, 915, 915, 1050)

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
}


for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  png(filename = glue('{file_names[i]}_{substr(eq, 15, 15)}.png'),
      width = ratio[i], height = ratio_2[i], res = 300)
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
  dev.off()
}

##----REVENUE PLOTS 6low----

munip_rev_2009 = drop_na(munip_rev_2009, c(No_of_lower_secondary_school_teacher_percap_1, Unemployment_Rate_Imputed_2, Population_Imputed_1))

eq = 'time_to_treat_6low_higher'
s = '+ 
  Population_Imputed_1 + Unemployment_Rate_Imputed_2 +
  No_of_elementary_school_percap_1 + NO_of_lower_secondary_school_percap_1 +
  No_of_upper_secondary_schools_percap_1 + No_of_elementary_school_Teacher_1 +
  No_of_lower_secondary_school_teacher_percap_1 + T30_I50_PS_mean | Period_Now'

coefs = c('Period_Now::-5', 'Period_Now::-4', 'Period_Now::-3', 'Period_Now::-2', 'Period_Now::0', 
          'Period_Now::1', 'Period_Now::2', 'Period_Now::3', 'Period_Now::4', 'Period_Now::5',
          'Period_Now::6', 'Period_Now::7')

row_names = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7)

local_tax_munip = data.table('period' = row_names, 
                             'coefs' = coef(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                  ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                             'se' = feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                          ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                             'lower' = confint(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                             'upper' = confint(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_tax_munip = rbind(local_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

local_tr_tax_munip = data.table('period' = row_names, 
                                'coefs' = coef(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                'se' = feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                'lower' = confint(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                'upper' = confint(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_tr_tax_munip = rbind(local_tr_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

local_alloc_tax_munip = data.table('period' = row_names, 
                                   'coefs' = coef(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                   'se' = feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                   'lower' = confint(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                           ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                   'upper' = confint(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                           ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_alloc_tax_munip = rbind(local_alloc_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

rents_and_fees_munip = data.table('period' = row_names, 
                                  'coefs' = coef(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                       ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                  'se' = feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                               ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                  'lower' = confint(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                          ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                  'upper' = confint(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                          ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

rents_and_fees_munip = rbind(rents_and_fees_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

treasury_munip = data.table('period' = row_names, 
                            'coefs' = coef(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                 ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                            'se' = feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                         ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                            'lower' = confint(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                            'upper' = confint(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

treasury_munip = rbind(treasury_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

prefecture_munip = data.table('period' = row_names, 
                              'coefs' = coef(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                   ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                              'se' = feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                           ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                              'lower' = confint(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                      ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                              'upper' = confint(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                      ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

prefecture_munip = rbind(prefecture_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

property_munip = data.table('period' = row_names, 
                            'coefs' = coef(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                 ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                            'se' = feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                         ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                            'lower' = confint(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                            'upper' = confint(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

property_munip = rbind(property_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dfs = list(local_tax_munip, local_tr_tax_munip, local_alloc_tax_munip, rents_and_fees_munip, treasury_munip, prefecture_munip, property_munip)

ylabel = c('Log local tax per capita', 'Log local tax transfers per capita', 'Local allocation tax per capita',
           'Log rent and fees per capita', 'Log treasurydisbursements per capita', 
           'Log prefecturedisbursements per capita', 'Log property income per capita')
file_names = c('local_tax_munip', 'local_tr_tax_munip', 'local_alloc_tax_munip', 'rents_and_fees_munip', 'treasury_munip', 'prefecture_munip', 'property_munip')

##---- REVENUE PLOTS LOOPS 6low----

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
}

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  png(filename = glue('{file_names[i]}_{substr(eq, 15, 15)}.png'),
      width = 1200, height = 915, res = 300)
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
  dev.off()
}

##---- EXPENDITURE PLOTS 5high----

munip_exp_2009 = drop_na(munip_exp_2009, c(No_of_lower_secondary_school_teacher_percap_1, Unemployment_Rate_Imputed_2, Population_Imputed_1))

eq = 'time_to_treat_5higher'
s = '+ 
  Population_Imputed_1 + Unemployment_Rate_Imputed_2 +
  No_of_elementary_school_percap_1 + NO_of_lower_secondary_school_percap_1 +
  No_of_upper_secondary_schools_percap_1 + No_of_elementary_school_Teacher_1 +
  No_of_lower_secondary_school_teacher_percap_1 + T30_I50_PS_mean | Period_Now'

coefs = c('Period_Now::-5', 'Period_Now::-4', 'Period_Now::-3', 'Period_Now::-2', 'Period_Now::0', 
          'Period_Now::1', 'Period_Now::2', 'Period_Now::3', 'Period_Now::4', 'Period_Now::5',
          'Period_Now::6', 'Period_Now::7')

row_names = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7)

filter_drop = unique(munip_exp_2009[is.na(munip_exp_2009['Welfare_municipalities_Exp']), 'ADM2_PCODE'])
data = munip_exp_2009[!unlist(munip_exp_2009$ADM2_PCODE) %in% unlist(filter_drop), ]

welf_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

welf_munip = rbind(welf_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

disa_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

disa_munip = rbind(disa_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

labr_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

labr_munip = rbind(labr_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

civl_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

civl_munip = rbind(civl_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

road_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

road_munip = rbind(road_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dwel_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

dwel_munip = rbind(dwel_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

ratio_munip = data.table('period' = row_names, 
                         'coefs' = coef(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                              ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                         'se' = feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                      ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                         'lower' = confint(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                                 ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                         'upper' = confint(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                                 ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

ratio_munip = rbind(ratio_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

reve_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

reve_munip = rbind(reve_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

expe_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

expe_munip = rbind(expe_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dfs = list(welf_munip, 
           disa_munip,
           labr_munip,
           expe_munip, 
           civl_munip,
           road_munip,
           dwel_munip,
           ratio_munip,
           reve_munip)

ylabel = c('Log welfare expenditure per capita', 
           'Disaster relief expenditure per capita',
           'Log labor expenditure per capita',
           'Log total expenditure per capita',
           'Log civil engineering expenditures per capita',
           'Log road / bridge expenditures per capita',
           'Dwelling expenditures per capita',
           'Net Excess of Revenue Ratio',
           'Log total revenue per capita')

##---- EXPENDITURE PLOTS LOOPS 5high----

file_names = c('Welfare_Exp', 'Disaster_Relief', 'Labor_Exp', 'Total_Exp', 'Civil_Eng_Exp', 'Road_Bridge_Exp', 'Dwelling_Exp',
               'Net_Excess_Ratio', 'Total_Revenue')

ratio = c(1200, 1200, 1200, 2400, 1200, 1200, 1200, 1200, 2400)
ratio_2 = c(915, 915, 915, 1050, 915, 915, 915, 915, 1050)

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
}


for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  png(filename = glue('{file_names[i]}_{substr(eq, 15, 15)}.png'),
      width = ratio[i], height = ratio_2[i], res = 300)
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
  dev.off()
}

##----REVENUE PLOTS 5high----

munip_rev_2009 = drop_na(munip_rev_2009, c(No_of_lower_secondary_school_teacher_percap_1, Unemployment_Rate_Imputed_2, Population_Imputed_1))

eq = 'time_to_treat_5higher'
s = '+ 
  Population_Imputed_1 + Unemployment_Rate_Imputed_2 +
  No_of_elementary_school_percap_1 + NO_of_lower_secondary_school_percap_1 +
  No_of_upper_secondary_schools_percap_1 + No_of_elementary_school_Teacher_1 +
  No_of_lower_secondary_school_teacher_percap_1 + T30_I50_PS_mean | Period_Now'

coefs = c('Period_Now::-5', 'Period_Now::-4', 'Period_Now::-3', 'Period_Now::-2', 'Period_Now::0', 
          'Period_Now::1', 'Period_Now::2', 'Period_Now::3', 'Period_Now::4', 'Period_Now::5',
          'Period_Now::6', 'Period_Now::7')

row_names = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7)

local_tax_munip = data.table('period' = row_names, 
                             'coefs' = coef(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                  ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                             'se' = feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                          ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                             'lower' = confint(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                             'upper' = confint(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_tax_munip = rbind(local_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

local_tr_tax_munip = data.table('period' = row_names, 
                                'coefs' = coef(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                'se' = feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                'lower' = confint(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                'upper' = confint(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_tr_tax_munip = rbind(local_tr_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

local_alloc_tax_munip = data.table('period' = row_names, 
                                   'coefs' = coef(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                   'se' = feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                   'lower' = confint(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                           ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                   'upper' = confint(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                           ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_alloc_tax_munip = rbind(local_alloc_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

rents_and_fees_munip = data.table('period' = row_names, 
                                  'coefs' = coef(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                       ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                  'se' = feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                               ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                  'lower' = confint(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                          ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                  'upper' = confint(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                          ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

rents_and_fees_munip = rbind(rents_and_fees_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

treasury_munip = data.table('period' = row_names, 
                            'coefs' = coef(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                 ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                            'se' = feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                         ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                            'lower' = confint(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                            'upper' = confint(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

treasury_munip = rbind(treasury_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

prefecture_munip = data.table('period' = row_names, 
                              'coefs' = coef(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                   ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                              'se' = feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                           ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                              'lower' = confint(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                      ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                              'upper' = confint(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                      ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

prefecture_munip = rbind(prefecture_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

property_munip = data.table('period' = row_names, 
                            'coefs' = coef(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                 ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                            'se' = feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                         ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                            'lower' = confint(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                            'upper' = confint(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

property_munip = rbind(property_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
dfs = list(local_tax_munip, local_tr_tax_munip, local_alloc_tax_munip, rents_and_fees_munip, treasury_munip, prefecture_munip, property_munip)

ylabel = c('Log local tax per capita', 'Log local tax transfers per capita', 'Local allocation tax per capita',
           'Log rent and fees per capita', 'Log treasurydisbursements per capita', 
           'Log prefecturedisbursements per capita', 'Log property income per capita')
file_names = c('local_tax_munip', 'local_tr_tax_munip', 'local_alloc_tax_munip', 'rents_and_fees_munip', 'treasury_munip', 'prefecture_munip', 'property_munip')

##---- REVENUE PLOTS LOOPS 5high----

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
}

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  png(filename = glue('{file_names[i]}_{substr(eq, 15, 15)}.png'),
      width = 1200, height = 915, res = 300)
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
  dev.off()
}

##---- EXPENDITURE PLOTS 6low MUNICIPALITY FE----

munip_exp_2009 = drop_na(munip_exp_2009, c(No_of_lower_secondary_school_teacher_percap_1, Unemployment_Rate_Imputed_2, Population_Imputed_1))

eq = 'time_to_treat_6low_higher'
s = '+ 
  Population_Imputed_1 + Unemployment_Rate_Imputed_2 +
  No_of_elementary_school_percap_1 + NO_of_lower_secondary_school_percap_1 +
  No_of_upper_secondary_schools_percap_1 + No_of_elementary_school_Teacher_1 +
  No_of_lower_secondary_school_teacher_percap_1 + T30_I50_PS_mean | Period_Now + Prefecture_ID'

coefs = c('Period_Now::-5', 'Period_Now::-4', 'Period_Now::-3', 'Period_Now::-2', 'Period_Now::0', 
          'Period_Now::1', 'Period_Now::2', 'Period_Now::3', 'Period_Now::4', 'Period_Now::5',
          'Period_Now::6', 'Period_Now::7')

row_names = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7)

filter_drop = unique(munip_exp_2009[is.na(munip_exp_2009['Welfare_municipalities_Exp']), 'ADM2_PCODE'])
data = munip_exp_2009[!unlist(munip_exp_2009$ADM2_PCODE) %in% unlist(filter_drop), ]

welf_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

welf_munip = rbind(welf_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

disa_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

disa_munip = rbind(disa_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

labr_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

labr_munip = rbind(labr_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

civl_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

civl_munip = rbind(civl_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

road_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

road_munip = rbind(road_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dwel_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

dwel_munip = rbind(dwel_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

ratio_munip = data.table('period' = row_names, 
                         'coefs' = coef(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                              ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                         'se' = feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                      ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                         'lower' = confint(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                                 ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                         'upper' = confint(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                                 ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

ratio_munip = rbind(ratio_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

reve_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

reve_munip = rbind(reve_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

expe_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

expe_munip = rbind(expe_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dfs = list(welf_munip, 
           disa_munip,
           labr_munip,
           expe_munip, 
           civl_munip,
           road_munip,
           dwel_munip,
           ratio_munip,
           reve_munip)

ylabel = c('Log welfare expenditure per capita', 
           'Disaster relief expenditure per capita',
           'Log labor expenditure per capita',
           'Log total expenditure per capita',
           'Log civil engineering expenditures per capita',
           'Log road / bridge expenditures per capita',
           'Dwelling expenditures per capita',
           'Net Excess of Revenue Ratio',
           'Log total revenue per capita')

##---- EXPENDITURE PLOTS LOOPS 6low MUNICIPALITY FE----

file_names = c('Welfare_Exp', 'Disaster_Relief', 'Labor_Exp', 'Total_Exp', 'Civil_Eng_Exp', 'Road_Bridge_Exp', 'Dwelling_Exp',
               'Net_Excess_Ratio', 'Total_Revenue')

ratio = c(1200, 1200, 1200, 2400, 1200, 1200, 1200, 1200, 2400)
ratio_2 = c(915, 915, 915, 1050, 915, 915, 915, 915, 1050)

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
}


for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  png(filename = glue('{file_names[i]}_{substr(eq, 15, 15)}_fe.png'),
      width = ratio[i], height = ratio_2[i], res = 300)
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
  dev.off()
}

##----REVENUE PLOTS 6low MUNICIPALITY FE----

munip_rev_2009 = drop_na(munip_rev_2009, c(No_of_lower_secondary_school_teacher_percap_1, Unemployment_Rate_Imputed_2, Population_Imputed_1))

eq = 'time_to_treat_6low_higher'
s = '+ 
  Population_Imputed_1 + Unemployment_Rate_Imputed_2 +
  No_of_elementary_school_percap_1 + NO_of_lower_secondary_school_percap_1 +
  No_of_upper_secondary_schools_percap_1 + No_of_elementary_school_Teacher_1 +
  No_of_lower_secondary_school_teacher_percap_1 + T30_I50_PS_mean | Period_Now + Prefecture_ID'

coefs = c('Period_Now::-5', 'Period_Now::-4', 'Period_Now::-3', 'Period_Now::-2', 'Period_Now::0', 
          'Period_Now::1', 'Period_Now::2', 'Period_Now::3', 'Period_Now::4', 'Period_Now::5',
          'Period_Now::6', 'Period_Now::7')

row_names = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7)

local_tax_munip = data.table('period' = row_names, 
                             'coefs' = coef(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                  ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                             'se' = feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                          ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                             'lower' = confint(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                             'upper' = confint(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_tax_munip = rbind(local_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

local_tr_tax_munip = data.table('period' = row_names, 
                                'coefs' = coef(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                'se' = feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                'lower' = confint(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                'upper' = confint(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_tr_tax_munip = rbind(local_tr_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

local_alloc_tax_munip = data.table('period' = row_names, 
                                   'coefs' = coef(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                   'se' = feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                   'lower' = confint(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                           ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                   'upper' = confint(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                           ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_alloc_tax_munip = rbind(local_alloc_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

rents_and_fees_munip = data.table('period' = row_names, 
                                  'coefs' = coef(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                       ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                  'se' = feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                               ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                  'lower' = confint(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                          ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                  'upper' = confint(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                          ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

rents_and_fees_munip = rbind(rents_and_fees_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

treasury_munip = data.table('period' = row_names, 
                            'coefs' = coef(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                 ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                            'se' = feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                         ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                            'lower' = confint(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                            'upper' = confint(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

treasury_munip = rbind(treasury_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

prefecture_munip = data.table('period' = row_names, 
                              'coefs' = coef(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                   ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                              'se' = feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                           ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                              'lower' = confint(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                      ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                              'upper' = confint(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                      ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

prefecture_munip = rbind(prefecture_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

property_munip = data.table('period' = row_names, 
                            'coefs' = coef(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                 ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                            'se' = feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                         ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                            'lower' = confint(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                            'upper' = confint(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

property_munip = rbind(property_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dfs = list(local_tax_munip, local_tr_tax_munip, local_alloc_tax_munip, rents_and_fees_munip, treasury_munip, prefecture_munip, property_munip)

ylabel = c('Log local tax per capita', 'Log local tax transfers per capita', 'Local allocation tax per capita',
           'Log rent and fees per capita', 'Log treasurydisbursements per capita', 
           'Log prefecturedisbursements per capita', 'Log property income per capita')
file_names = c('local_tax_munip', 'local_tr_tax_munip', 'local_alloc_tax_munip', 'rents_and_fees_munip', 'treasury_munip', 'prefecture_munip', 'property_munip')

##---- REVENUE PLOTS LOOPS 6low MUNICIPALITY FE----

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
}

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  png(filename = glue('{file_names[i]}_{substr(eq, 15, 15)}_fe.png'),
      width = 1200, height = 915, res = 300)
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
  dev.off()
}

##---- EXPENDITURE PLOTS 5high MUNICIPALITY FE----

munip_exp_2009 = drop_na(munip_exp_2009, c(No_of_lower_secondary_school_teacher_percap_1, Unemployment_Rate_Imputed_2, Population_Imputed_1))

eq = 'time_to_treat_5higher'
s = '+ 
  Population_Imputed_1 + Unemployment_Rate_Imputed_2 +
  No_of_elementary_school_percap_1 + NO_of_lower_secondary_school_percap_1 +
  No_of_upper_secondary_schools_percap_1 + No_of_elementary_school_Teacher_1 +
  No_of_lower_secondary_school_teacher_percap_1 + T30_I50_PS_mean | Period_Now + Prefecture_ID'

coefs = c('Period_Now::-5', 'Period_Now::-4', 'Period_Now::-3', 'Period_Now::-2', 'Period_Now::0', 
          'Period_Now::1', 'Period_Now::2', 'Period_Now::3', 'Period_Now::4', 'Period_Now::5',
          'Period_Now::6', 'Period_Now::7')

row_names = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7)

filter_drop = unique(munip_exp_2009[is.na(munip_exp_2009['Welfare_municipalities_Exp']), 'ADM2_PCODE'])
data = munip_exp_2009[!unlist(munip_exp_2009$ADM2_PCODE) %in% unlist(filter_drop), ]

welf_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Welfare_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

welf_munip = rbind(welf_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

disa_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('Disaster_relief_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

disa_munip = rbind(disa_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

labr_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('Labour_municipalities_Exp/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

labr_munip = rbind(labr_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

civl_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Civil_engineering_works_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

civl_munip = rbind(civl_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

road_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Roads_and_bridges_municipalities_Exp/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

road_munip = rbind(road_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dwel_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Dwellings_municipalities_Exp+1/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

dwel_munip = rbind(dwel_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

ratio_munip = data.table('period' = row_names, 
                         'coefs' = coef(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                              ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                         'se' = feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                      ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                         'lower' = confint(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                                 ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                         'upper' = confint(feols(form('Ratio_of_net_excess_of_revenue', eq, s), 
                                                 ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

ratio_munip = rbind(ratio_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

reve_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Settlement_of_total_revenue/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

reve_munip = rbind(reve_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

expe_munip = data.table('period' = row_names, 
                        'coefs' = coef(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_exp_2009))[coefs], 
                        'se' = feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                     ~ADM2_PCODE, data = munip_exp_2009)$coeftable[coefs, 2],
                        'lower' = confint(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 1],
                        'upper' = confint(feols(form('log(Settlement_of_total_expenditure/Population_Imputed_1)', eq, s), 
                                                ~ADM2_PCODE, data = munip_exp_2009))[coefs, 2])

expe_munip = rbind(expe_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dfs = list(welf_munip, 
           disa_munip,
           labr_munip,
           expe_munip, 
           civl_munip,
           road_munip,
           dwel_munip,
           ratio_munip,
           reve_munip)

ylabel = c('Log welfare expenditure per capita', 
           'Disaster relief expenditure per capita',
           'Log labor expenditure per capita',
           'Log total expenditure per capita',
           'Log civil engineering expenditures per capita',
           'Log road / bridge expenditures per capita',
           'Dwelling expenditures per capita',
           'Net Excess of Revenue Ratio',
           'Log total revenue per capita')

##---- EXPENDITURE PLOTS LOOPS 5high MUNICIPALITY FE----

file_names = c('Welfare_Exp', 'Disaster_Relief', 'Labor_Exp', 'Total_Exp', 'Civil_Eng_Exp', 'Road_Bridge_Exp', 'Dwelling_Exp',
               'Net_Excess_Ratio', 'Total_Revenue')

ratio = c(1200, 1200, 1200, 2400, 1200, 1200, 1200, 1200, 2400)
ratio_2 = c(915, 915, 915, 1050, 915, 915, 915, 915, 1050)

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
}


for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  png(filename = glue('{file_names[i]}_{substr(eq, 15, 15)}_fe.png'),
      width = ratio[i], height = ratio_2[i], res = 300)
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
  dev.off()
}

##----REVENUE PLOTS 5high MUNICIPALITY FE----

munip_rev_2009 = drop_na(munip_rev_2009, c(No_of_lower_secondary_school_teacher_percap_1, Unemployment_Rate_Imputed_2, Population_Imputed_1))

eq = 'time_to_treat_5higher'
s = '+ 
  Population_Imputed_1 + Unemployment_Rate_Imputed_2 +
  No_of_elementary_school_percap_1 + NO_of_lower_secondary_school_percap_1 +
  No_of_upper_secondary_schools_percap_1 + No_of_elementary_school_Teacher_1 +
  No_of_lower_secondary_school_teacher_percap_1 + T30_I50_PS_mean | Period_Now + Prefecture_ID'

coefs = c('Period_Now::-5', 'Period_Now::-4', 'Period_Now::-3', 'Period_Now::-2', 'Period_Now::0', 
          'Period_Now::1', 'Period_Now::2', 'Period_Now::3', 'Period_Now::4', 'Period_Now::5',
          'Period_Now::6', 'Period_Now::7')

row_names = c(-5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7)

local_tax_munip = data.table('period' = row_names, 
                             'coefs' = coef(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                  ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                             'se' = feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                          ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                             'lower' = confint(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                             'upper' = confint(feols(form('log(Local_taxes/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_tax_munip = rbind(local_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

local_tr_tax_munip = data.table('period' = row_names, 
                                'coefs' = coef(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                     ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                'se' = feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                             ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                'lower' = confint(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                'upper' = confint(feols(form('log(Local_transferred_tax/Population_Imputed_1)', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_tr_tax_munip = rbind(local_tr_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

local_alloc_tax_munip = data.table('period' = row_names, 
                                   'coefs' = coef(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                        ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                   'se' = feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                   'lower' = confint(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                           ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                   'upper' = confint(feols(form('Local_allocation_tax/Population_Imputed_1', eq, s), 
                                                           ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

local_alloc_tax_munip = rbind(local_alloc_tax_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

rents_and_fees_munip = data.table('period' = row_names, 
                                  'coefs' = coef(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                       ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                                  'se' = feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                               ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                                  'lower' = confint(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                          ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                                  'upper' = confint(feols(form('log(Rents_and_fees/Population_Imputed_1)', eq, s), 
                                                          ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

rents_and_fees_munip = rbind(rents_and_fees_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

treasury_munip = data.table('period' = row_names, 
                            'coefs' = coef(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                 ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                            'se' = feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                         ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                            'lower' = confint(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                            'upper' = confint(feols(form('log(Treasury_disbursements/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

treasury_munip = rbind(treasury_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

prefecture_munip = data.table('period' = row_names, 
                              'coefs' = coef(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                   ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                              'se' = feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                           ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                              'lower' = confint(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                      ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                              'upper' = confint(feols(form('log(Prefectural_disbursements/Population_Imputed_1)', eq, s), 
                                                      ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

prefecture_munip = rbind(prefecture_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

property_munip = data.table('period' = row_names, 
                            'coefs' = coef(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                 ~ADM2_PCODE, data = munip_rev_2009))[coefs], 
                            'se' = feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                         ~ADM2_PCODE, data = munip_rev_2009)$coeftable[coefs, 2],
                            'lower' = confint(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 1],
                            'upper' = confint(feols(form('log(Property_income/Population_Imputed_1)', eq, s), 
                                                    ~ADM2_PCODE, data = munip_rev_2009))[coefs, 2])

property_munip = rbind(property_munip, data.table('period' = -1, 'coefs' = 0, 'se' = 0, 'lower' = NA, 'upper' = NA))

dfs = list(local_tax_munip, local_tr_tax_munip, local_alloc_tax_munip, rents_and_fees_munip, treasury_munip, prefecture_munip, property_munip)

ylabel = c('Log local tax per capita', 'Log local tax transfers per capita', 'Local allocation tax per capita',
           'Log rent and fees per capita', 'Log treasurydisbursements per capita', 
           'Log prefecturedisbursements per capita', 'Log property income per capita')
file_names = c('local_tax_munip', 'local_tr_tax_munip', 'local_alloc_tax_munip', 'rents_and_fees_munip', 'treasury_munip', 'prefecture_munip', 'property_munip')

##---- REVENUE PLOTS LOOPS 5high MUNICIPALITY FE----

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
}

for(i in 1:length(dfs)) {
  dat = dfs[[i]]
  ylab = ylabel[i]
  png(filename = glue('{file_names[i]}_{substr(eq, 15, 15)}_fe.png'),
      width = 1200, height = 915, res = 300)
  print(ggplot(data=dat, aes(x=factor(period), y=coefs, group = 1)) +
          theme_clean() +
          theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = .3),
                panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = .3)) +
          geom_line(linewidth = .8) + 
          geom_line(aes(x=factor(period), y=lower, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_line(aes(x=factor(period), y=upper, group = 1), linetype = 'dashed', color = 'blue', linewidth = .8) +
          geom_ribbon(aes(x=factor(period), ymin=lower, ymax=upper), fill='blue', alpha=0.05) +
          geom_vline(xintercept = '-1', linetype = 'dashed', linewidth = .4) +
          geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
          xlab('Years Since Earthquake') +
          ylab(ylab))
  dev.off()
}
