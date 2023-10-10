library(tidyverse)
library(lfe)
library(stargazer)
library(huxtable)
library(lmtest)
library(sandwich)
library(caret)
library(lubridate)
library(spatstat)
library(data.table)
library(plm)  
library(haven)
library(data.table)
library(fixest)
library(fastDummies)
library(glue)
library(modelsummary)

data_condominiums_R = read_csv('D://Thesis/Data/data_condominiums_R.csv') #Update before run
data_buildings_R = read_csv('D://Thesis/Data/data_buildings_R.csv') #Update before run

data_condominiums_R_only_house = read_csv('D://Thesis/Data/data_condominiums_R_only_house.csv') #Update before run
data_buildings_R_only_house = read_csv('D://Thesis/Data/data_buildings_R_only_house.csv') #Update before run

data_buildings_R_only_house['ln_price'] = log(data_buildings_R_only_house['Transaction-price(total)'])
data_condominiums_R_only_house['ln_price'] = log(data_condominiums_R_only_house['Transaction-price(total)'])
data_buildings_R_only_house['Building_Age'] = data_buildings_R_only_house['Year'] - data_buildings_R_only_house['Year of construction']
data_condominiums_R_only_house['Building_Age'] = data_condominiums_R_only_house['Year'] - data_condominiums_R_only_house['Year of construction']
data_condominiums_R['ln_price'] = log(data_condominiums_R['Transaction-price(total)'])
data_condominiums_R['Building_Age'] = data_condominiums_R['Year'] - data_condominiums_R['Year of construction']
data_buildings_R['ln_price'] = log(data_buildings_R['Transaction-price(total)'])
data_buildings_R['Building_Age'] = data_buildings_R['Year'] - data_buildings_R['Year of construction']

data_condominiums_R['eq_risk_6_low_higher'] = 1-(1-data_condominiums_R['T30_I55_PS_mean'])^(1/30)
data_condominiums_R['eq_risk_5_high_higher'] = 1-(1-data_condominiums_R['T30_I50_PS_mean'])^(1/30)
data_condominiums_R = data_condominiums_R %>% arrange(Shi_Chou_Son_ID, Year, Quarter)

data_buildings_R['eq_risk_6_low_higher'] = 1-(1-data_buildings_R['T30_I55_PS_mean'])^(1/30)
data_buildings_R['eq_risk_5_high_higher'] = 1-(1-data_buildings_R['T30_I50_PS_mean'])^(1/30)
data_buildings_R = data_buildings_R %>% arrange(Shi_Chou_Son_ID, Year, Quarter)

data_condominiums_R_only_house['eq_risk_6_low_higher'] = 1-(1-data_condominiums_R_only_house['T30_I55_PS_mean'])^(1/30)
data_condominiums_R_only_house['eq_risk_5_high_higher'] = 1-(1-data_condominiums_R_only_house['T30_I50_PS_mean'])^(1/30)
data_condominiums_R_only_house = data_condominiums_R_only_house %>% arrange(Shi_Chou_Son_ID, Year, Quarter)

data_buildings_R_only_house['eq_risk_6_low_higher'] = 1-(1-data_buildings_R_only_house['T30_I55_PS_mean'])^(1/30)
data_buildings_R_only_house['eq_risk_5_high_higher'] = 1-(1-data_buildings_R_only_house['T30_I50_PS_mean'])^(1/30)
data_buildings_R_only_house = data_buildings_R_only_house %>% arrange(Shi_Chou_Son_ID, Year, Quarter)

data_buildings_R = data_buildings_R %>% 
  rename('Building_structure' ='Building structure',
         'City_Planning' = 'City Planning',
         'Transaction_Period' = 'Transaction period')

data_condominiums_R = data_condominiums_R %>% 
  rename('Building_structure' ='Building structure',
         'City_Planning' = 'City Planning',
         'Transaction_Period' = 'Transaction period')

data_buildings_R_only_house = data_buildings_R_only_house %>% 
  rename('Building_structure' ='Building structure',
         'City_Planning' = 'City Planning',
         'Transaction_Period' = 'Transaction period')

data_condominiums_R_only_house = data_condominiums_R_only_house %>% 
  rename('Building_structure' ='Building structure',
         'City_Planning' = 'City Planning',
         'Transaction_Period' = 'Transaction period')

colnames(data_condominiums_R)[13] = 'Area_m2'
colnames(data_buildings_R)[13] = 'Area_m2'
colnames(data_condominiums_R_only_house)[13] = 'Area_m2'
colnames(data_buildings_R_only_house)[13] = 'Area_m2'

data_condominiums_R['post_quake_dummy'] = ifelse(data_condominiums_R$Period_Now >= data_condominiums_R$First_Period_5higher, 1, 0)
data_buildings_R['post_quake_dummy'] = ifelse(data_buildings_R$Period_Now >= data_buildings_R$First_Period_5higher, 1, 0)
data_condominiums_R_only_house['post_quake_dummy'] = ifelse(data_condominiums_R_only_house$Period_Now >= data_condominiums_R_only_house$First_Period_5higher, 1, 0)
data_buildings_R_only_house['post_quake_dummy'] = ifelse(data_buildings_R_only_house$Period_Now >= data_buildings_R_only_house$First_Period_5higher, 1, 0)

data_condominiums_R['time_to_treat'] = ifelse(data_condominiums_R$First_Period_5high > 100, 10000, data_condominiums_R$First_Period_5high)
data_condominiums_R_only_house['time_to_treat'] = ifelse(data_condominiums_R_only_house$First_Period_5high > 100, 10000, data_condominiums_R_only_house$First_Period_5high)
data_buildings_R['time_to_treat'] = ifelse(data_buildings_R$First_Period_5high > 100, 10000, data_buildings_R$First_Period_5high)
data_buildings_R_only_house['time_to_treat'] = ifelse(data_buildings_R_only_house$First_Period_5high > 100, 10000, data_buildings_R_only_house$First_Period_5high)

data_condominiums_R['Population_Imputed_1'] = data_condominiums_R$Population_Imputed_1 / 10000
data_condominiums_R_only_house['Population_Imputed_1'] = data_condominiums_R_only_house$Population_Imputed_1 / 10000
data_buildings_R['Population_Imputed_1'] = data_buildings_R$Population_Imputed_1 / 10000
data_buildings_R_only_house['Population_Imputed_1'] = data_buildings_R_only_house$Population_Imputed_1 / 10000

formula_post_quake_bench = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy + T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_bench_oh = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy + T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_bench_land = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy + T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_bench_land_oh = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy + T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))

#formula = as.formula(glue('ln_price ~ Nearest_Station_dist_min + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + T30_I55_PS_mean + lag_atleast_one_6_low_or_higher * T30_I55_PS_mean  + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_iv = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * IV_eq_risk_post_q + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
#formula_3 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + atleast_one_6_low_or_higher * T30_I55_PS_mean+ lag_atleast_one_6_low_or_higher * T30_I55_PS_mean  + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_iv_land = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + Area_m2 + FA_Rate + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * IV_eq_risk_post_q + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_land = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + Area_m2 + FA_Rate + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))

first_stage_form_post_quake = as.formula(glue('T30_I50_PS_mean ~ T30_I50_PS_stddev * post_quake_dummy + BC_rate + FA_Rate + Nearest_Station_dist_min + No_Rooms + Area_m2 + Building_Age + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + Building_structure + City_Planning + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
first_stage_form_post_quake_land = as.formula(glue('T30_I50_PS_mean ~ T30_I50_PS_stddev * post_quake_dummy + BC_rate + FA_Rate + Nearest_Station_dist_min + Area_m2 + Building_Age + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + Building_structure + City_Planning + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
fs_cond_p_q = feols(first_stage_form_post_quake, data = data_condominiums_R) 
fs_cond_p_q_oh = feols(first_stage_form_post_quake, data = data_condominiums_R_only_house) 
fs_land_p_q = feols(first_stage_form_post_quake_land, data = data_buildings_R) 
fs_land_p_q_oh = feols(first_stage_form_post_quake_land, data = data_buildings_R_only_house) 
data_condominiums_R['IV_eq_risk_post_q'] = predict(fs_cond_p_q, data_condominiums_R)
data_condominiums_R_only_house['IV_eq_risk_post_q'] = predict(fs_cond_p_q_oh, data_condominiums_R_only_house)
data_buildings_R['IV_eq_risk_post_q'] = predict(fs_land_p_q, data_buildings_R)
data_buildings_R_only_house['IV_eq_risk_post_q'] = predict(fs_land_p_q_oh, data_buildings_R_only_house)



first_stage_form_post_quake_2 = as.formula(glue('T30_I50_PS_mean * post_quake_dummy ~ T30_I50_PS_stddev * post_quake_dummy + BC_rate + FA_Rate + Nearest_Station_dist_min + No_Rooms + Area_m2 + Building_Age + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + Building_structure + City_Planning + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
first_stage_form_post_quake_land_2 = as.formula(glue('T30_I50_PS_mean * post_quake_dummy ~ T30_I50_PS_stddev * post_quake_dummy + BC_rate + FA_Rate + Nearest_Station_dist_min + Area_m2 + Building_Age + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + Building_structure + City_Planning + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
first_stage_cond_post_quake_2 = feols(first_stage_form_post_quake_2, data = data_condominiums_R) 
first_stage_cond_post_quake_oh_2 = feols(first_stage_form_post_quake_2, data = data_condominiums_R_only_house) 
first_stage_land_post_quake_2 = feols(first_stage_form_post_quake_land_2, data = data_buildings_R) 
first_stage_land_post_quake_oh_2 = feols(first_stage_form_post_quake_land_2, data = data_buildings_R_only_house) 
data_condominiums_R['IV_interaction'] = predict(first_stage_cond_post_quake_2, data_condominiums_R)
data_condominiums_R_only_house['IV_interaction'] = predict(first_stage_cond_post_quake_oh_2, data_condominiums_R_only_house)
data_buildings_R['IV_interaction'] = predict(first_stage_land_post_quake_2, data_buildings_R)
data_buildings_R_only_house['IV_interaction'] = predict(first_stage_land_post_quake_oh_2, data_buildings_R_only_house)

#m_2012 = felm(formula_2_1, data = data_condominiums_R[data_condominiums_R$Year >= 2012, ])
#m_2012_iv = felm(formula_2, data = data_condominiums_R[data_condominiums_R$Year >= 2012, ])
#huxreg(mod_hed_2012_feols)

##---- Hedonic Baseline and Extended Models----

mod_hed_p_q_base = felm(formula_post_quake_bench, data = data_condominiums_R)
mod_hed_p_q = felm(formula_post_quake, data = data_condominiums_R)
mod_hed_p_q_iv = felm(formula_post_quake_iv, data = data_condominiums_R)
#huxreg(mod_hed, mod_hed_iv)

#m_2012_oh = felm(formula_2_1, data = data_condominiums_R_only_house[data_condominiums_R_only_house$Year >= 2012, ])
#m_2012_oh_iv = felm(formula_2, data = data_condominiums_R_only_house[data_condominiums_R_only_house$Year >= 2012, ])
#huxreg(m_2012_oh, m_2012_oh_iv)

m_oh_p_q_base = felm(formula_post_quake_bench_oh, data = data_condominiums_R_only_house)
m_oh_p_q = felm(formula_post_quake, data = data_condominiums_R_only_house)
m_oh_p_q_iv = felm(formula_post_quake_iv, data = data_condominiums_R_only_house)
#huxreg(m_oh, m_oh_iv)

#m_land_2012 = felm(formula_4_1, data = data_buildings_R[data_buildings_R$Year >= 2012, ])
#m_land_2012_iv = felm(formula_4, data = data_buildings_R[data_buildings_R$Year >= 2012, ])
#huxreg(m_land_2012, m_land_2012_iv)

m_land_p_q_base = felm(formula_post_quake_bench_land, data = data_buildings_R)
m_land_p_q = felm(formula_post_quake_land, data = data_buildings_R)
m_land_p_q_iv = felm(formula_post_quake_iv_land, data = data_buildings_R)
#huxreg(m_land, m_land_iv)

#m_land_2012_oh = felm(formula_4_1, data = data_buildings_R_only_house[data_buildings_R_only_house$Year >= 2012, ])
#m_land_2012_oh_iv = felm(formula_4, data = data_buildings_R_only_house[data_buildings_R_only_house$Year >= 2012, ])
#huxreg(m_land_2012_oh, m_land_2012_oh_iv)

m_land_oh_p_q_base = felm(formula_post_quake_bench_land_oh, data = data_buildings_R_only_house)
m_land_oh_p_q = felm(formula_post_quake_land, data = data_buildings_R_only_house)
m_land_oh_p_q_iv = felm(formula_post_quake_iv_land, data = data_buildings_R_only_house)
#huxreg(m_land_oh, m_land_oh_iv)

##---- Robustness Check Hedonic Baseline and Extended Models----

formula_post_quake_bench_2008 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy + T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_bench_oh_2008 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy + T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_bench_land_2008 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy + T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_bench_land_oh_2008 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy + T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))

data_condominiums_R_2008 = data_condominiums_R[data_condominiums_R$Year < 2014, ]
data_condominiums_R_only_house_2008 = data_condominiums_R_only_house[data_condominiums_R_only_house$Year < 2014, ]
data_buildings_R_2008 = data_buildings_R[data_buildings_R$Year < 2014, ]
data_buildings_R_only_house_2008 = data_buildings_R_only_house[data_buildings_R_only_house$Year < 2014, ]

#formula = as.formula(glue('ln_price ~ Nearest_Station_dist_min + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + T30_I55_PS_mean + lag_atleast_one_6_low_or_higher * T30_I55_PS_mean  + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_iv_2008 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * IV_eq_risk_post_q + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_2008 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + FA_Rate + No_Rooms + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
#formula_3 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + Area_m2 + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + atleast_one_6_low_or_higher * T30_I55_PS_mean+ lag_atleast_one_6_low_or_higher * T30_I55_PS_mean  + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_iv_land_2008 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + Area_m2 + FA_Rate + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * IV_eq_risk_post_q + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
formula_post_quake_land_2008 = as.formula(glue('ln_price ~ Nearest_Station_dist_min + BC_rate + Area_m2 + FA_Rate + Building_Age + Building_structure + City_Planning + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))

first_stage_form_post_quake_2008 = as.formula(glue('T30_I50_PS_mean ~ T30_I50_PS_stddev * post_quake_dummy + BC_rate + FA_Rate + Nearest_Station_dist_min + No_Rooms + Area_m2 + Building_Age + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + Building_structure + City_Planning + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
first_stage_form_post_quake_land_2008 = as.formula(glue('T30_I50_PS_mean ~ T30_I50_PS_stddev * post_quake_dummy + BC_rate + FA_Rate + Nearest_Station_dist_min + Area_m2 + Building_Age + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + Building_structure + City_Planning + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
first_stage_cond_post_quake_2008 = feols(first_stage_form_post_quake_2008, data = data_condominiums_R_2008) 
first_stage_cond_post_quake_oh_2008 = feols(first_stage_form_post_quake_2008, data = data_condominiums_R_only_house_2008) 
first_stage_land_post_quake_2008 = feols(first_stage_form_post_quake_land_2008, data = data_buildings_R_2008) 
first_stage_land_post_quake_oh_2008 = feols(first_stage_form_post_quake_land_2008, data = data_buildings_R_only_house_2008) 
data_condominiums_R_2008['IV_eq_risk_post_q'] = predict(first_stage_cond_post_quake_2008, data_condominiums_R_2008)
data_condominiums_R_only_house_2008['IV_eq_risk_post_q'] = predict(first_stage_cond_post_quake_oh_2008, data_condominiums_R_only_house_2008)
data_buildings_R_2008['IV_eq_risk_post_q'] = predict(first_stage_land_post_quake_2008, data_buildings_R_2008)
data_buildings_R_only_house_2008['IV_eq_risk_post_q'] = predict(first_stage_land_post_quake_oh_2008, data_buildings_R_only_house_2008)

first_stage_form_post_quake_2_08 = as.formula(glue('T30_I50_PS_mean * post_quake_dummy ~ T30_I50_PS_stddev * post_quake_dummy + BC_rate + FA_Rate + Nearest_Station_dist_min + No_Rooms + Area_m2 + Building_Age + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + Building_structure + City_Planning + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
first_stage_form_post_quake_land_2_08 = as.formula(glue('T30_I50_PS_mean * post_quake_dummy ~ T30_I50_PS_stddev * post_quake_dummy + BC_rate + FA_Rate + Nearest_Station_dist_min + Area_m2 + Building_Age + {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + Building_structure + City_Planning + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
first_stage_cond_post_quake_2_08 = feols(first_stage_form_post_quake_2_08, data = data_condominiums_R) 
first_stage_cond_post_quake_oh_2_08 = feols(first_stage_form_post_quake_2_08, data = data_condominiums_R_only_house) 
first_stage_land_post_quake_2_08 = feols(first_stage_form_post_quake_land_2_08, data = data_buildings_R) 
first_stage_land_post_quake_oh_2_08 = feols(first_stage_form_post_quake_land_2_08, data = data_buildings_R_only_house) 
data_condominiums_R_2008['IV_interaction'] = predict(first_stage_cond_post_quake_2_08, data_condominiums_R_2008)
data_condominiums_R_only_house_2008['IV_interaction'] = predict(first_stage_cond_post_quake_oh_2_08, data_condominiums_R_only_house_2008)
data_buildings_R_2008['IV_interaction'] = predict(first_stage_land_post_quake_2_08, data_buildings_R_2008)
data_buildings_R_only_house_2008['IV_interaction'] = predict(first_stage_land_post_quake_oh_2_08, data_buildings_R_only_house_2008)

#m_2012 = felm(formula_2_1, data = data_condominiums_R[data_condominiums_R$Year >= 2012, ])
#m_2012_iv = felm(formula_2, data = data_condominiums_R[data_condominiums_R$Year >= 2012, ])
#huxreg(mod_hed_2012_feols)

mod_hed_base_08 = felm(formula_post_quake_bench_2008, data = data_condominiums_R_2008)
mod_hed_p_q_08 = felm(formula_post_quake_2008, data = data_condominiums_R_2008)
mod_hed_p_q_iv_08 = felm(formula_post_quake_iv_2008, data = data_condominiums_R_2008)
#huxreg(mod_hed, mod_hed_iv)

#m_2012_oh = felm(formula_2_1, data = data_condominiums_R_only_house[data_condominiums_R_only_house$Year >= 2012, ])
#m_2012_oh_iv = felm(formula_2, data = data_condominiums_R_only_house[data_condominiums_R_only_house$Year >= 2012, ])
#huxreg(m_2012_oh, m_2012_oh_iv)

m_oh_base_08 = felm(formula_post_quake_bench_oh_2008, data = data_condominiums_R_only_house_2008)
m_oh_p_q_08 = felm(formula_post_quake_2008, data = data_condominiums_R_only_house_2008)
m_oh_p_q_iv_08 = felm(formula_post_quake_iv_2008, data = data_condominiums_R_only_house_2008)
#huxreg(m_oh, m_oh_iv)

#m_land_2012 = felm(formula_4_1, data = data_buildings_R[data_buildings_R$Year >= 2012, ])
#m_land_2012_iv = felm(formula_4, data = data_buildings_R[data_buildings_R$Year >= 2012, ])
#huxreg(m_land_2012, m_land_2012_iv)

m_land_base_08 = felm(formula_post_quake_bench_land_2008, data = data_buildings_R_2008)
m_land_p_q_08 = felm(formula_post_quake_land_2008, data = data_buildings_R_2008)
m_land_p_q_iv_08 = felm(formula_post_quake_iv_land_2008, data = data_buildings_R_2008)
#huxreg(m_land, m_land_iv)

#m_land_2012_oh = felm(formula_4_1, data = data_buildings_R_only_house[data_buildings_R_only_house$Year >= 2012, ])
#m_land_2012_oh_iv = felm(formula_4, data = data_buildings_R_only_house[data_buildings_R_only_house$Year >= 2012, ])
#huxreg(m_land_2012_oh, m_land_2012_oh_iv)

m_land_oh_base_08 = felm(formula_post_quake_bench_land_oh_2008, data = data_buildings_R_only_house_2008)
m_land_oh_p_q_08 = felm(formula_post_quake_land_2008, data = data_buildings_R_only_house_2008)
m_land_oh_p_q_iv_08 = felm(formula_post_quake_iv_land_2008, data = data_buildings_R_only_house_2008)
#huxreg(m_land_oh, m_land_oh_iv)

##---- Hedonic Baseline and Extended Tables----

robust_se_benchmark_p_q_base = list(coef(summary(mod_hed_p_q_base, robust=T))[, 2],
                               coef(summary(m_oh_p_q_base, robust=T))[, 2],
                               coef(summary(m_land_p_q_base, robust=T))[, 2],
                               coef(summary(m_land_oh_p_q_base, robust=T))[, 2])

stargazer(mod_hed_p_q_base, m_oh_p_q_base, m_land_p_q_base, m_land_oh_p_q_base,
          out = "doc.doc",
          style = "aer",
          column.labels = c("All Condominiums", "Condominiums Only House", "Building / Land", "Building / Land House"),
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', 
                   'FA_Rate', 'No_Rooms', 'Area_m2', 'Building_Age', 'T30_I50_PS_mean:post_quake_dummy', 
                   'Population_Imputed_1', 'Unemployment_Rate_Imputed_2'),
          #covariate.labels =  c('Building Coverage Rate', 'Floor / Area Rate', 
          #'Number of Rooms', 'Area($m^2$)', 'Age of Building',
          #'Number of Elementry School (Per capita)', 'Number of Lower Secondary School (Per capita)',
          #'Number of Upper Secondary School (Per capita)', 'Number of Elementary School Teacher (Per capita),
          #'Number or Lower Secondary School Teacher (Per capita)',
          #'Post Quake Dummy', 'Earthquake Risk Measure $5^+$', 'Population', 'Unemployment'),
          se = robust_se_benchmark_p_q_base,
          report=("vc*sp")
          , type = 'text'
)

robust_se_benchmark_p_q = list(coef(summary(mod_hed_p_q, robust=T))[, 2],
                              coef(summary(m_oh_p_q, robust=T))[, 2],
                              coef(summary(m_land_p_q, robust=T))[, 2],
                              coef(summary(m_land_oh_p_q, robust=T))[, 2])

stargazer(mod_hed_p_q, m_oh_p_q, m_land_p_q, m_land_oh_p_q,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),
          keep = c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', 
                   'FA_Rate', 'No_Rooms', 'Area_m2', 'Building_Age', 'T30_I50_PS_mean:post_quake_dummy',
                   'Population_Imputed_1', 'Unemployment_Rate_Imputed_2'),
          #covariate.labels =  c('Building Coverage Rate', 'Floor / Area Rate', 
          #'Number of Rooms', 'Area($m^2$)', 'Age of Building',
          #'Number of Elementry School (Per capita)', 'Number of Lower Secondary School (Per capita)',
          #'Number of Upper Secondary School (Per capita)', 'Number of Elementary School Teacher (Per capita),
          #'Number or Lower Secondary School Teacher (Per capita)',
          #'Post Quake Dummy', 'Earthquake Risk Measure $5^+$', 'Population', 'Unemployment'),
          se = robust_se_benchmark_p_q,
          report=("vc*sp")
          , type = 'text'
)

##---- Robustness Check Baseline and Extended Tables----

robust_se_benchmark_p_q_base_08 = list(coef(summary(mod_hed_base_08, robust=T))[, 2],
                                       coef(summary(m_oh_base_08, robust=T))[, 2],
                                       coef(summary(m_land_base_08, robust=T))[, 2],
                                       coef(summary(m_land_oh_base_08, robust=T))[, 2])

stargazer(mod_hed_base_08, m_oh_base_08, m_land_base_08, m_land_oh_base_08,
          out = "doc.doc",
          style = "aer",
          column.labels = c("All Condominiums", "Condominiums Only House", "Building / Land", "Building / Land House"),
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', 
                   'FA_Rate', 'No_Rooms', 'Area_m2', 'Building_Age', 'T30_I50_PS_mean:post_quake_dummy', 
                   'Population_Imputed_1', 'Unemployment_Rate_Imputed_2'),
          #covariate.labels =  c('Building Coverage Rate', 'Floor / Area Rate', 
          #'Number of Rooms', 'Area($m^2$)', 'Age of Building',
          #'Number of Elementry School (Per capita)', 'Number of Lower Secondary School (Per capita)',
          #'Number of Upper Secondary School (Per capita)', 'Number of Elementary School Teacher (Per capita),
          #'Number or Lower Secondary School Teacher (Per capita)',
          #'Post Quake Dummy', 'Earthquake Risk Measure $5^+$', 'Population', 'Unemployment'),
          se = robust_se_benchmark_p_q_base_08,
          report=("vc*sp")
          , type = 'text'
)

robust_se_benchmark_p_q_08 = list(coef(summary(mod_hed_p_q_08, robust=T))[, 2],
                                  coef(summary(m_oh_p_q_08, robust=T))[, 2],
                                  coef(summary(m_land_p_q_08, robust=T))[, 2],
                                  coef(summary(m_land_oh_p_q_08, robust=T))[, 2])

stargazer(mod_hed_p_q_08, m_oh_p_q_08, m_land_p_q_08, m_land_oh_p_q_08,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),
          keep = c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', 
                   'FA_Rate', 'No_Rooms', 'Area_m2', 'Building_Age', 'T30_I50_PS_mean:post_quake_dummy',
                   'Population_Imputed_1', 'Unemployment_Rate_Imputed_2'),
          #covariate.labels =  c('Building Coverage Rate', 'Floor / Area Rate', 
          #'Number of Rooms', 'Area($m^2$)', 'Age of Building',
          #'Number of Elementry School (Per capita)', 'Number of Lower Secondary School (Per capita)',
          #'Number of Upper Secondary School (Per capita)', 'Number of Elementary School Teacher (Per capita),
          #'Number or Lower Secondary School Teacher (Per capita)',
          #'Post Quake Dummy', 'Earthquake Risk Measure $5^+$', 'Population', 'Unemployment'),
          se = robust_se_benchmark_p_q_08,
          report=("vc*sp")
          , type = 'text'
)

##---- Baseline IV Extended----

fs_cond_p_q = felm(first_stage_form_post_quake, data = data_condominiums_R) 
fs_cond_p_q_oh = felm(first_stage_form_post_quake, data = data_condominiums_R_only_house) 
fs_land_p_q = felm(first_stage_form_post_quake_land, data = data_buildings_R) 
fs_land_p_q_oh = felm(first_stage_form_post_quake_land, data = data_buildings_R_only_house) 

robust_se_benchmark_p_q_iv_fs = list(coef(summary(fs_cond_p_q, robust=T))[, 2],
                                     coef(summary(fs_cond_p_q_oh, robust=T))[, 2],
                                     coef(summary(fs_land_p_q, robust=T))[, 2],
                                     coef(summary(fs_land_p_q_oh, robust=T))[, 2])

stargazer(fs_cond_p_q, fs_cond_p_q_oh, fs_land_p_q, fs_land_p_q_oh,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_stddev:post_quake_dummy', 'T30_I50_PS_stddev'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = robust_se_benchmark_p_q_iv_fs,
          report=("vc*sp")
          , type = 'text'
)

robust_se_benchmark_p_q_iv = list(coef(summary(mod_hed_p_q_iv, robust=T))[, 2],
                                 coef(summary(m_oh_p_q_iv, robust=T))[, 2],
                                 coef(summary(m_land_p_q_iv, robust=T))[, 2],
                                 coef(summary(m_land_oh_p_q_iv, robust=T))[, 2])

stargazer(mod_hed_p_q_iv, m_oh_p_q_iv, m_land_p_q_iv, m_land_oh_p_q_iv,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('IV_eq_risk_post_q', 'post_quake_dummy', 'IV_interaction', 'No_Rooms', 'BC_rate', 
                   'FA_Rate', 'No_Rooms', 'Area_m2', 'Building_Age', 'IV_eq_risk_post_q:post_quake_dummy', 
                   'Population_Imputed_1', 'Unemployment_Rate_Imputed_2'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = robust_se_benchmark_p_q_iv,
          report=("vc*sp")
          , type = 'text'
)

##---- Robustness Check IV Extended----

fs_cond_p_q08 = felm(first_stage_form_post_quake_2008, data = data_condominiums_R_2008) 
fs_cond_p_q_oh08 = felm(first_stage_form_post_quake_2008, data = data_condominiums_R_only_house_2008) 
fs_land_p_q08 = felm(first_stage_form_post_quake_land_2008, data = data_buildings_R_2008) 
fs_land_p_q_oh08 = felm(first_stage_form_post_quake_land_2008, data = data_buildings_R_only_house_2008) 

robust_se_benchmark_p_q_iv_fs = list(coef(summary(fs_cond_p_q08, robust=T))[, 2],
                                     coef(summary(fs_cond_p_q_oh08, robust=T))[, 2],
                                     coef(summary(fs_land_p_q08, robust=T))[, 2],
                                     coef(summary(fs_land_p_q_oh08, robust=T))[, 2])

stargazer(fs_cond_p_q08, fs_cond_p_q_oh08, fs_land_p_q08, fs_land_p_q_oh08,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_stddev:post_quake_dummy', 'T30_I50_PS_stddev'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = robust_se_benchmark_p_q_iv_fs,
          report=("vc*sp")
          , type = 'text'
)

robust_se_benchmark_p_q_iv_08 = list(coef(summary(mod_hed_p_q_iv_08, robust=T))[, 2],
                                     coef(summary(m_oh_p_q_iv_08, robust=T))[, 2],
                                     coef(summary(m_land_p_q_iv_08, robust=T))[, 2],
                                     coef(summary(m_land_oh_p_q_iv_08, robust=T))[, 2])

stargazer(mod_hed_p_q_iv_08, m_oh_p_q_iv_08, m_land_p_q_iv_08, m_land_oh_p_q_iv_08,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('IV_eq_risk_post_q', 'post_quake_dummy', 'IV_eq_risk_post_q:post_quake_dummy', 'No_Rooms', 'BC_rate', 
                   'FA_Rate', 'No_Rooms', 'Area_m2', 'Building_Age', 'T30_I50_PS_mean:post_quake_dummy', 
                   'Population_Imputed_1', 'Unemployment_Rate_Imputed_2', 'IV_interaction'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = robust_se_benchmark_p_q_iv_08,
          report=("vc*sp")
          , type = 'text'
)

##---- Characteristics Regressions----

df_cond_no_sales = data_condominiums_R %>% group_by(Transaction_Period, Shi_Chou_Son_ID) %>% summarize('number_of_sales' = n(),
                                                                               Population_Imputed_1 = mean(Population_Imputed_1),
                                                                               Unemployment_Rate_Imputed_2 = mean(Unemployment_Rate_Imputed_2),
                                                                               post_quake_dummy = mean(post_quake_dummy),
                                                                               T30_I50_PS_mean = mean(T30_I50_PS_mean),
                                                                               No_of_elementary_school_percap_1 = mean(No_of_elementary_school_percap_1),
                                                                               NO_of_lower_secondary_school_percap_1 = mean(NO_of_lower_secondary_school_percap_1),
                                                                               No_of_upper_secondary_schools_percap_1 = mean(No_of_upper_secondary_schools_percap_1),
                                                                               No_of_elementary_school_Teacher_1 = mean(No_of_elementary_school_Teacher_1),
                                                                               No_of_lower_secondary_school_teacher_percap_1 = mean(No_of_lower_secondary_school_teacher_percap_1))

df_cond_no_sales_oh = data_condominiums_R_only_house %>% group_by(Transaction_Period, Shi_Chou_Son_ID) %>% summarize('number_of_sales' = n(),
                                                                                                  Population_Imputed_1 = mean(Population_Imputed_1),
                                                                                                  Unemployment_Rate_Imputed_2 = mean(Unemployment_Rate_Imputed_2),
                                                                                                  post_quake_dummy = mean(post_quake_dummy),
                                                                                                  T30_I50_PS_mean = mean(T30_I50_PS_mean),
                                                                                                  No_of_elementary_school_percap_1 = mean(No_of_elementary_school_percap_1),
                                                                                                  NO_of_lower_secondary_school_percap_1 = mean(NO_of_lower_secondary_school_percap_1),
                                                                                                  No_of_upper_secondary_schools_percap_1 = mean(No_of_upper_secondary_schools_percap_1),
                                                                                                  No_of_elementary_school_Teacher_1 = mean(No_of_elementary_school_Teacher_1),
                                                                                                  No_of_lower_secondary_school_teacher_percap_1 = mean(No_of_lower_secondary_school_teacher_percap_1))

df_build_no_sales = data_buildings_R %>% group_by(Transaction_Period, Shi_Chou_Son_ID) %>% summarize('number_of_sales' = n(),
                                                                                                  Population_Imputed_1 = mean(Population_Imputed_1),
                                                                                                  Unemployment_Rate_Imputed_2 = mean(Unemployment_Rate_Imputed_2),
                                                                                                  post_quake_dummy = mean(post_quake_dummy),
                                                                                                  T30_I50_PS_mean = mean(T30_I50_PS_mean),
                                                                                                  No_of_elementary_school_percap_1 = mean(No_of_elementary_school_percap_1),
                                                                                                  NO_of_lower_secondary_school_percap_1 = mean(NO_of_lower_secondary_school_percap_1),
                                                                                                  No_of_upper_secondary_schools_percap_1 = mean(No_of_upper_secondary_schools_percap_1),
                                                                                                  No_of_elementary_school_Teacher_1 = mean(No_of_elementary_school_Teacher_1),
                                                                                                  No_of_lower_secondary_school_teacher_percap_1 = mean(No_of_lower_secondary_school_teacher_percap_1))

df_build_no_sales_oh = data_buildings_R_only_house %>% group_by(Transaction_Period, Shi_Chou_Son_ID) %>% summarize('number_of_sales' = n(),
                                                                                                  Population_Imputed_1 = mean(Population_Imputed_1),
                                                                                                  Unemployment_Rate_Imputed_2 = mean(Unemployment_Rate_Imputed_2),
                                                                                                  post_quake_dummy = mean(post_quake_dummy),
                                                                                                  T30_I50_PS_mean = mean(T30_I50_PS_mean),
                                                                                                  No_of_elementary_school_percap_1 = mean(No_of_elementary_school_percap_1),
                                                                                                  NO_of_lower_secondary_school_percap_1 = mean(NO_of_lower_secondary_school_percap_1),
                                                                                                  No_of_upper_secondary_schools_percap_1 = mean(No_of_upper_secondary_schools_percap_1),
                                                                                                  No_of_elementary_school_Teacher_1 = mean(No_of_elementary_school_Teacher_1),
                                                                                                  No_of_lower_secondary_school_teacher_percap_1 = mean(No_of_lower_secondary_school_teacher_percap_1))



no_sales_form = as.formula(glue('number_of_sales ~ {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))

mod_p_q_sales = felm(no_sales_form, data = df_cond_no_sales)
mod_p_q_sales_oh = felm(no_sales_form, data = df_cond_no_sales_oh)
mod_p_q_sales_b = felm(no_sales_form, data = df_build_no_sales)
mod_p_q_sales_boh = felm(no_sales_form, data = df_build_no_sales_oh)

char_form_room = as.formula(glue('No_Rooms ~ {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
char_form_area = as.formula(glue('Area_m2 ~ {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
char_form_age = as.formula(glue('Building_Age ~ {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))

mod_p_q_area = felm(char_form_area, data = data_condominiums_R)
mod_p_q_area_oh = felm(char_form_area, data = data_condominiums_R_only_house)
mod_p_q_area_b = felm(char_form_area, data = data_buildings_R)
mod_p_q_area_boh = felm(char_form_area, data = data_buildings_R_only_house)

mod_p_q_room = felm(char_form_room, data = data_condominiums_R)
mod_p_q_room_oh = felm(char_form_room, data = data_condominiums_R_only_house)

mod_p_q_age = felm(char_form_age, data = data_condominiums_R)
mod_p_q_age_oh = felm(char_form_age, data = data_condominiums_R_only_house)
mod_p_q_age_b = felm(char_form_age, data = data_buildings_R)
mod_p_q_age_boh = felm(char_form_age, data = data_buildings_R_only_house)

##---- Robustness Check Characteristics Regressions----

df_cond_no_sales_2008 = data_condominiums_R_2008 %>% group_by(Transaction_Period, Shi_Chou_Son_ID) %>% summarize('number_of_sales' = n(),
                                                                                                                 Population_Imputed_1 = mean(Population_Imputed_1),
                                                                                                                 Unemployment_Rate_Imputed_2 = mean(Unemployment_Rate_Imputed_2),
                                                                                                                 post_quake_dummy = mean(post_quake_dummy),
                                                                                                                 T30_I50_PS_mean = mean(T30_I50_PS_mean),
                                                                                                                 No_of_elementary_school_percap_1 = mean(No_of_elementary_school_percap_1),
                                                                                                                 NO_of_lower_secondary_school_percap_1 = mean(NO_of_lower_secondary_school_percap_1),
                                                                                                                 No_of_upper_secondary_schools_percap_1 = mean(No_of_upper_secondary_schools_percap_1),
                                                                                                                 No_of_elementary_school_Teacher_1 = mean(No_of_elementary_school_Teacher_1),
                                                                                                                 No_of_lower_secondary_school_teacher_percap_1 = mean(No_of_lower_secondary_school_teacher_percap_1))

df_cond_no_sales_oh_2008 = data_condominiums_R_only_house_2008 %>% group_by(Transaction_Period, Shi_Chou_Son_ID) %>% summarize('number_of_sales' = n(),
                                                                                                                               Population_Imputed_1 = mean(Population_Imputed_1),
                                                                                                                               Unemployment_Rate_Imputed_2 = mean(Unemployment_Rate_Imputed_2),
                                                                                                                               post_quake_dummy = mean(post_quake_dummy),
                                                                                                                               T30_I50_PS_mean = mean(T30_I50_PS_mean),
                                                                                                                               No_of_elementary_school_percap_1 = mean(No_of_elementary_school_percap_1),
                                                                                                                               NO_of_lower_secondary_school_percap_1 = mean(NO_of_lower_secondary_school_percap_1),
                                                                                                                               No_of_upper_secondary_schools_percap_1 = mean(No_of_upper_secondary_schools_percap_1),
                                                                                                                               No_of_elementary_school_Teacher_1 = mean(No_of_elementary_school_Teacher_1),
                                                                                                                               No_of_lower_secondary_school_teacher_percap_1 = mean(No_of_lower_secondary_school_teacher_percap_1))

df_build_no_sales_2008 = data_buildings_R_2008 %>% group_by(Transaction_Period, Shi_Chou_Son_ID) %>% summarize('number_of_sales' = n(),
                                                                                                               Population_Imputed_1 = mean(Population_Imputed_1),
                                                                                                               Unemployment_Rate_Imputed_2 = mean(Unemployment_Rate_Imputed_2),
                                                                                                               post_quake_dummy = mean(post_quake_dummy),
                                                                                                               T30_I50_PS_mean = mean(T30_I50_PS_mean),
                                                                                                               No_of_elementary_school_percap_1 = mean(No_of_elementary_school_percap_1),
                                                                                                               NO_of_lower_secondary_school_percap_1 = mean(NO_of_lower_secondary_school_percap_1),
                                                                                                               No_of_upper_secondary_schools_percap_1 = mean(No_of_upper_secondary_schools_percap_1),
                                                                                                               No_of_elementary_school_Teacher_1 = mean(No_of_elementary_school_Teacher_1),
                                                                                                               No_of_lower_secondary_school_teacher_percap_1 = mean(No_of_lower_secondary_school_teacher_percap_1))

df_build_no_sales_oh_2008 = data_buildings_R_only_house_2008 %>% group_by(Transaction_Period, Shi_Chou_Son_ID) %>% summarize('number_of_sales' = n(),
                                                                                                                             Population_Imputed_1 = mean(Population_Imputed_1),
                                                                                                                             Unemployment_Rate_Imputed_2 = mean(Unemployment_Rate_Imputed_2),
                                                                                                                             post_quake_dummy = mean(post_quake_dummy),
                                                                                                                             T30_I50_PS_mean = mean(T30_I50_PS_mean),
                                                                                                                             No_of_elementary_school_percap_1 = mean(No_of_elementary_school_percap_1),
                                                                                                                             NO_of_lower_secondary_school_percap_1 = mean(NO_of_lower_secondary_school_percap_1),
                                                                                                                             No_of_upper_secondary_schools_percap_1 = mean(No_of_upper_secondary_schools_percap_1),
                                                                                                                             No_of_elementary_school_Teacher_1 = mean(No_of_elementary_school_Teacher_1),
                                                                                                                             No_of_lower_secondary_school_teacher_percap_1 = mean(No_of_lower_secondary_school_teacher_percap_1))



no_sales_form08 = as.formula(glue('number_of_sales ~ {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))

mod_sales08 = felm(no_sales_form, data = df_cond_no_sales_2008)
mod_sales_oh08 = felm(no_sales_form, data = df_cond_no_sales_oh_2008)
mod_sales_b08 = felm(no_sales_form, data = df_build_no_sales_2008)
mod_sales_boh08 = felm(no_sales_form, data = df_build_no_sales_oh_2008)


char_form_room = as.formula(glue('No_Rooms ~ {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
char_form_area = as.formula(glue('Area_m2 ~ {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))
char_form_age = as.formula(glue('Building_Age ~ {paste(colnames(data_condominiums_R)[145:149], collapse = " + ")} + post_quake_dummy * T30_I50_PS_mean + Population_Imputed_1 + Unemployment_Rate_Imputed_2 | Transaction_Period + Shi_Chou_Son_ID'))

mod_area08 = felm(char_form_area, data = data_condominiums_R_2008)
mod_area_oh08 = felm(char_form_area, data = data_condominiums_R_only_house_2008)
mod_area_b08 = felm(char_form_area, data = data_buildings_R_2008)
mod_area_boh08 = felm(char_form_area, data = data_buildings_R_only_house_2008)

mod_room08 = felm(char_form_room, data = data_condominiums_R_2008)
mod_room_oh08 = felm(char_form_room, data = data_condominiums_R_only_house_2008)

mod_age08 = felm(char_form_age, data = data_condominiums_R_2008)
mod_age_oh08 = felm(char_form_age, data = data_condominiums_R_only_house_2008)
mod_age_b08 = felm(char_form_age, data = data_buildings_R_2008)
mod_age_boh08 = felm(char_form_age, data = data_buildings_R_only_house_2008)

##---- Characteristics Tables----

char_p_q_sales = list(coef(summary(mod_p_q_sales, robust=T))[, 2],
                      coef(summary(mod_p_q_sales_oh, robust=T))[, 2],
                      coef(summary(mod_p_q_sales_b, robust=T))[, 2],
                      coef(summary(mod_p_q_sales_boh, robust=T))[, 2])

stargazer(mod_p_q_sales, mod_p_q_sales_oh, mod_p_q_sales_b, mod_p_q_sales_boh,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'Imputed'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = char_p_q_sales,
          report=("vc*sp")
          , type = 'text'
)

char_p_q_area = list(coef(summary(mod_p_q_area, robust=T))[, 2],
                     coef(summary(mod_p_q_area_oh, robust=T))[, 2],
                     coef(summary(mod_p_q_area_b, robust=T))[, 2],
                     coef(summary(mod_p_q_area_boh, robust=T))[, 2])

stargazer(mod_p_q_area, mod_p_q_area_oh, mod_p_q_area_b, mod_p_q_area_boh,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'Imputed'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = char_p_q_area,
          report=("vc*sp")
          , type = 'text'
)

char_p_q_room = list(coef(summary(mod_p_q_room, robust=T))[, 2],
                    coef(summary(mod_p_q_room_oh, robust=T))[, 2])

stargazer(mod_p_q_room, mod_p_q_room_oh,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'Imputed'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = char_p_q_room,
          report=("vc*sp")
          , type = 'text'
)

char_p_q_age = list(coef(summary(mod_p_q_age, robust=T))[, 2],
                     coef(summary(mod_p_q_age_oh, robust=T))[, 2],
                     coef(summary(mod_p_q_age_b, robust=T))[, 2],
                     coef(summary(mod_p_q_age_boh, robust=T))[, 2])

stargazer(mod_p_q_age, mod_p_q_age_oh, mod_p_q_age_b, mod_p_q_age_boh,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'Imputed'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = char_p_q_age,
          report=("vc*sp")
          , type = 'text'
)

##---- Robustness Check Characteristics Tables----

char_p_q_sales = list(coef(summary(mod_sales08, robust=T))[, 2],
                      coef(summary(mod_sales_oh08, robust=T))[, 2],
                      coef(summary(mod_sales_b08, robust=T))[, 2],
                      coef(summary(mod_sales_boh08, robust=T))[, 2])

stargazer(mod_sales08, mod_sales_oh08, mod_sales_b08, mod_sales_boh08,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'Imputed'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = char_p_q_sales,
          report=("vc*sp")
          , type = 'text'
)

char_p_q_area = list(coef(summary(mod_area08, robust=T))[, 2],
                     coef(summary(mod_area_oh08, robust=T))[, 2],
                     coef(summary(mod_area_b08, robust=T))[, 2],
                     coef(summary(mod_area_boh08, robust=T))[, 2])

stargazer(mod_area08, mod_area_oh08, mod_area_b08, mod_area_boh08,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'Imputed'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = char_p_q_area,
          report=("vc*sp")
          , type = 'text'
)

char_p_q_room = list(coef(summary(mod_room08, robust=T))[, 2],
                     coef(summary(mod_room_oh08, robust=T))[, 2])

stargazer(mod_room08, mod_room_oh08,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'Imputed'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = char_p_q_room,
          report=("vc*sp")
          , type = 'text'
)

char_p_q_age = list(coef(summary(mod_age08, robust=T))[, 2],
                    coef(summary(mod_age_oh08, robust=T))[, 2],
                    coef(summary(mod_age_b08, robust=T))[, 2],
                    coef(summary(mod_age_boh08, robust=T))[, 2])

stargazer(mod_age08, mod_age_oh08, mod_age_b08, mod_age_boh08,
          out = "doc.doc",
          style = "aer",
          #column.labels = c("2012 5high", "2012 5high house", "2012 5high building", "2012 5high house building"),c('post_quake_dummy:T30_I50_PS_mean', 'post_quake_dummy', 'T30_I50_PS_mean', 'No_Rooms', 'BC_rate', ,
          keep = c('post_quake_dummy', 'T30_I50_PS_mean', 'Imputed'),
          #covariate.labels =  c("Dummy 5high", "5high Eq Risk", 'IV Eq Risk', 'Dummy 5high 1q lag',
          #                      "Dummy 5high x Eq Risk", "Dummy 5high 1q lag x Eq Risk",
          #                      "Dummy 5high x IV Eq Risk", "Dummy 5high 1q lag x IV Eq Risk"),
          se = char_p_q_age,
          report=("vc*sp")
          , type = 'text'
)


