# -*- coding: utf-8 -*-
"""
Created on Mon Jul 10 06:42:12 2023

@author: ufukc
"""

import pandas as pd
import os
import numpy as np

def fast(n, axis = 0):
    a = {0:'row', 1:'column'}
    pd.set_option(f'display.max_{a[axis]}s', n)

os.chdir('D:\\Data')

prefecture = [
    'Hokkaido', 'Aomori Prefecture', 'Iwate Prefecture', 'Miyagi Prefecture',
    'Akita Prefecture', 'Yamagata Prefecture', 'Fukushima Prefecture', 'Ibaraki Prefecture',
    'Tochigi Prefecture', 'Gunma Prefecture', 'Saitama Prefecture', 'Chiba Prefecture', 
    'Tokyo', 'Kanagawa Prefecture', 'Niigata Prefecture', 'Toyama Prefecture', 
    'Ishikawa Prefecture','Fukui Prefecture', 'Yamanashi Prefecture', 'Nagano Prefecture', 
    'Gifu Prefecture', 'Shizuoka Prefecture', 'Aichi Prefecture', 'Mie Prefecture',
    'Shiga Prefecture', 'Kyoto Prefecture', 'Osaka Prefecture', 'Hyogo Prefecture',
    'Nara Prefecture', 'Wakayama Prefecture', 'Tottori Prefecture', 'Shimane Prefecture',
    'Okayama Prefecture', 'Hiroshima Prefecture', 'Yamaguchi Prefecture', 'Tokushima Prefecture',
    'Kagawa Prefecture', 'Ehime Prefecture', 'Kochi Prefecture', 'Fukuoka Prefecture', 
    'Saga Prefecture', 'Nagasaki Prefecture', 'Kumamoto Prefecture', 'Oita Prefecture',
    'Miyazaki Prefecture', 'Kagoshima Prefecture', 'Okinawa Prefecture'
          ]

years = np.arange(2008, 2022)
list_japan = []
dict_japan = {}

for y in np.arange(0, len(years)):
    for i in np.arange(0, len(prefecture)):
        string_1 = f'Property Transactions/Japan Transaction Data/{i+1:02}_{prefecture[i]}_{years[y]}1_{years[y]}4.csv'
        df_loop = pd.read_csv(string_1, encoding = "ISO-8859-1", low_memory = False)
        df_loop.insert(5, 'Year', years[y])
        list_japan.append(df_loop)
        
df_japan = pd.concat(list_japan).reset_index(drop = True).copy()
df_japan.head(20)

df_japan.rename({'Maximus Building Coverage Ratio(%)':'BC_rate', 'Maximus Floor-area Ratio(%)':'FA_Rate', 'Nearest stationFDistance(minute)':'Nearest_Station_dist_min', 'Nearest stationFName':'Nearest_Station_name'}, axis = 'columns', inplace = True)
df_inflation = pd.read_csv('Municipality Data/japan_inflation_final.csv', encoding = "ISO-8859-1", low_memory = False)
df_japan = df_japan.merge(df_inflation.iloc[:, 1:3], left_on = 'Transaction period', right_on = 'quarter_to_join', how = 'left')
df_japan['Transaction_price_adjusted'] = df_japan['Transaction-price(total)'] - df_japan['Transaction-price(total)']*df_japan['Change_From_2008_1st']

df_japan_to_view = df_japan.head(100)



uniques = {}

for i in df_japan.columns:
    uniques[i] = df_japan[i].unique()
    
drop_list = ['Renovation', 'Region', 'Transaction-price(Unit price m^2)', 'Land shape', 'Frontage', 'Total floor area(m^2)',
             'Purpose of Use', 'Frontage roadFDirection', 'Frontage roadFClassification', 'Frontage roadFBreadth(m)',
             'Transactional factors', 'Area']

cities_unique = df_japan['City,Town,Ward,Village'].unique().tolist()
len(cities_unique)

risk_years = np.arange(2008, 2023)
list_japan_risk = []

for i in risk_years:
    if i == 2015:
        string_1 = f'Earthquake Risk/Earthquake Risk CSV/StDev/{i-1}_Town_City_Risk_StDev.csv'
    else:
        string_1 = f'Earthquake Risk/Earthquake Risk CSV/StDev/{i}_Town_City_Risk_StDev.csv'
    df_loop_risk = pd.read_csv(string_1, encoding = "ISO-8859-1")
    df_loop_risk.insert(1, 'Year', i)
    list_japan_risk.append(df_loop_risk)
    
df_eq_risk = pd.concat(list_japan_risk).iloc[:, 1:len(df_loop_risk.columns)]

#%%
munip_char_wide = pd.read_csv('Municipality Data\Municipalities_Pop_School_Labor_Wide_2.csv')
to_merge = pd.read_csv('Municipality Data\Municipalities_Pop_School_Labor_Panel_2.csv', low_memory = False, encoding='windows-1252')
munip_char_wide = munip_char_wide[munip_char_wide['ADM2_PCODE'].notna()].reset_index()
 
munip_char_wide['Total_pop_2005_q4th'] = munip_char_wide.Total_pop_2005
munip_char_wide['Total_pop_2010_q4th'] = munip_char_wide.Total_pop_2010
munip_char_wide['Total_pop_2015_q4th'] = munip_char_wide.Total_pop_2015
munip_char_wide['Total_pop_2020_q4th'] = munip_char_wide.Total_pop_2020

munip_char_wide['g_05_10'] = (munip_char_wide.Total_pop_2010_q4th / munip_char_wide.Total_pop_2005_q4th)**(1/20) - 1
munip_char_wide['g_10_15'] = (munip_char_wide.Total_pop_2015_q4th / munip_char_wide.Total_pop_2010_q4th)**(1/20) - 1
munip_char_wide['g_15_20'] = (munip_char_wide.Total_pop_2020_q4th / munip_char_wide.Total_pop_2015_q4th)**(1/20) - 1

list_pop_exclude = ['Total_pop_2005_q4th', 'Total_pop_2010_q4th', 'Total_pop_2015_q4th', 'Total_pop_2020_q4th']

def func(string, series, list_func):
    for i in np.arange(0, len(series), 1):
        list_func.append(string + '_' + str(series[i]))

def func2(series_1, series_2, list_func2):
    for i in np.arange(0, len(series_1), 1):
        for j in np.arange(0, len(series_2)):
            list_func2.append(str(series_1[i]) + '_' + str(series_2[j])) 
        
list_test = []
series_1 = np.arange(2006, 2021)
series_2 = ['q1st', 'q2nd', 'q3rd', 'q4th']
func2(series_1, series_2, list_test)
       

string = 'Total_pop'
list_pop = []
func(string, list_test, list_pop)

for i in np.arange(0, len(list_pop_exclude)):
    if list_pop_exclude[i] in list_pop:
        list_pop.remove(list_pop_exclude[i])

for i in np.arange(0, len(list_pop)):
    x = list_pop[i]
    y = int(x[10:14])
    q = int(x[16])
    if   y > 2015 :
        munip_char_wide[list_pop[i]] = munip_char_wide['Total_pop_2015_q4th']*(1 + munip_char_wide['g_15_20'])**((y - 2015)*4 - (4-q))
    elif y > 2010 :
        munip_char_wide[list_pop[i]] = munip_char_wide['Total_pop_2010_q4th']*(1 + munip_char_wide['g_10_15'])**((y - 2010)*4 - (4-q))
    elif y > 2005 :
        munip_char_wide[list_pop[i]] = munip_char_wide['Total_pop_2005_q4th']*(1 + munip_char_wide['g_05_10'])**((y - 2005)*4 - (4-q))
        
list_pop_full = ['ADM2_PCODE'] + list_pop_exclude + list_pop
list_pop_full.sort()

munip_char_pop = munip_char_wide[list_pop_full]
munip_char_pop_panel = pd.melt(munip_char_pop[list_pop_full].set_index('ADM2_PCODE'), value_vars = munip_char_pop[list_pop_full].iloc[:, 1:len(munip_char_pop[list_pop_full])].columns, value_name = 'Population_Imputed_1', ignore_index = False)
munip_char_pop_panel['Period'] = munip_char_pop_panel.variable.str[16:19] + " quarter " + munip_char_pop_panel.variable.str[10:14]
munip_char_pop_panel['Year'] = munip_char_pop_panel.variable.str[10:14].astype('int')
munip_char_pop_panel = munip_char_pop_panel.reset_index()[['ADM2_PCODE', 'Year', 'Period', 'Population_Imputed_1']]

munip_char_wide['g_05_10_2'] = (munip_char_wide.Total_pop_2010 / munip_char_wide.Total_pop_2005)**(1/5) - 1
munip_char_wide['g_10_15_2'] = (munip_char_wide.Total_pop_2015 / munip_char_wide.Total_pop_2010)**(1/5) - 1
munip_char_wide['g_15_20_2'] = (munip_char_wide.Total_pop_2020 / munip_char_wide.Total_pop_2015)**(1/5) - 1

list_pop_exclude = ['Total_pop_2005', 'Total_pop_2010', 'Total_pop_2015', 'Total_pop_2020']
     
series = np.arange(2005, 2021)
string = 'Total_pop'
list_pop = []
func(string, series, list_pop)

for i in np.arange(0, len(list_pop_exclude)):
    if list_pop_exclude[i] in list_pop:
        list_pop.remove(list_pop_exclude[i])

for i in np.arange(0, len(list_pop)):
    x = list_pop[i]
    y = int(x[10:14])
    if   y > 2015 :
        munip_char_wide[list_pop[i]] = munip_char_wide['Total_pop_2015']*(1 + munip_char_wide['g_15_20_2'])**(y - 2015)
    elif y > 2010 :
        munip_char_wide[list_pop[i]] = munip_char_wide['Total_pop_2010']*(1 + munip_char_wide['g_10_15_2'])**(y - 2010)
    elif y > 2005 :
        munip_char_wide[list_pop[i]] = munip_char_wide['Total_pop_2005']*(1 + munip_char_wide['g_05_10_2'])**(y - 2005)
        
list_pop_full = ['ADM2_PCODE'] + list_pop_exclude + list_pop
list_pop_full.sort()

munip_char_pop_2 = munip_char_wide[list_pop_full]
munip_char_pop_panel_2 = pd.melt(munip_char_pop_2[list_pop_full].set_index('ADM2_PCODE'), value_vars = munip_char_pop_2[list_pop_full].iloc[:, 1:len(munip_char_pop_2[list_pop_full])].columns, value_name = 'Population_Imputed_2', ignore_index = False)
munip_char_pop_panel_2['Year'] = munip_char_pop_panel_2.variable.str[10:14].astype('int')
munip_char_pop_panel_2 = munip_char_pop_panel_2.reset_index()[['ADM2_PCODE', 'Year', 'Population_Imputed_2']]

munip_char_wide['unem_rate_rate_2005'] = munip_char_wide['Unemployed_rate_2005'] / munip_char_wide['Japan_Unemployment_Rate_2005']
munip_char_wide['unem_rate_rate_2010'] = munip_char_wide['Unemployed_rate_2010'] / munip_char_wide['Japan_Unemployment_Rate_2010']
munip_char_wide['unem_rate_rate_2015'] = munip_char_wide['Unemployed_rate_2015'] / munip_char_wide['Japan_Unemployment_Rate_2015']

list_unem = []
strig_unem = 'Unemployed_rate'
series = np.arange(2005, 2021)
func(strig_unem, series, list_unem)
list_exclude = ['Unemployed_rate_2005', 'Unemployed_rate_2010', 'Unemployed_rate_2015']

for i in np.arange(0, len(list_exclude)):
    if list_exclude[i] in list_unem:
        list_unem.remove(list_exclude[i])

for i in np.arange(0, len(list_unem)):
    x = list_unem[i]
    y = int(x[16:20])
    if   y > 2015 :
        munip_char_wide[list_unem[i]] = munip_char_wide['unem_rate_rate_2015'] * munip_char_wide[f'Japan_Unemployment_Rate_{y}']
    elif y > 2010 :
        munip_char_wide[list_unem[i]] = ((munip_char_wide['unem_rate_rate_2015'] - munip_char_wide['unem_rate_rate_2010'])*((y - 2010)/5) + munip_char_wide['unem_rate_rate_2010']) * munip_char_wide[f'Japan_Unemployment_Rate_{y}']
    elif y > 2005 :        
        for j in np.arange(0, len(munip_char_wide)):
            if pd.isna(munip_char_wide.loc[j, 'unem_rate_rate_2005']):
                munip_char_wide.loc[j, list_unem[i]] = munip_char_wide.loc[j, 'unem_rate_rate_2010'] * munip_char_wide.loc[j, f'Japan_Unemployment_Rate_{y}']        
            else:
                munip_char_wide.loc[j, list_unem[i]] = ((munip_char_wide.loc[j, 'unem_rate_rate_2010'] - munip_char_wide.loc[j, 'unem_rate_rate_2005'])*((y - 2010)/5) + munip_char_wide.loc[j, 'unem_rate_rate_2010']) * munip_char_wide.loc[j, f'Japan_Unemployment_Rate_{y}']

list_unem_full = ['ADM2_PCODE'] + list_exclude + list_unem
list_unem_full.sort()

munip_char_unem = munip_char_wide[list_unem_full]
munip_char_unem_panel = pd.melt(munip_char_unem[list_unem_full].set_index('ADM2_PCODE'), value_vars = munip_char_unem[list_unem_full].iloc[:, 1:len(munip_char_unem[list_unem_full])].columns, value_name = 'Unemployment_Rate_Imputed_1', ignore_index = False)
munip_char_unem_panel['Year'] = munip_char_unem_panel.variable.str[16:20].astype('int')
munip_char_unem_panel = munip_char_unem_panel.reset_index()[['ADM2_PCODE', 'Year', 'Unemployment_Rate_Imputed_1']]

munip_char_wide['Unemployed_rate_2005_unimputed'] = munip_char_wide['Unemployed_rate_2005']
    
for j in np.arange(0, len(munip_char_wide)):
    if pd.isna(munip_char_wide.loc[j, 'unem_rate_rate_2005']):
        munip_char_wide.loc[j, 'Unemployed_rate_2005'] = munip_char_wide.loc[j, 'unem_rate_rate_2010'] * munip_char_wide.loc[j, 'Japan_Unemployment_Rate_2005']

list_unem_full = ['ADM2_PCODE'] + list_exclude + list_unem
list_unem_full.sort()

munip_char_unem_2 = munip_char_wide[list_unem_full]
munip_char_unem_2_panel = pd.melt(munip_char_unem_2[list_unem_full].set_index('ADM2_PCODE'), value_vars = munip_char_unem_2[list_unem_full].iloc[:, 1:len(munip_char_unem_2[list_unem_full])].columns, value_name = 'Unemployment_Rate_Imputed_2', ignore_index = False)
munip_char_unem_2_panel['Year'] = munip_char_unem_2_panel.variable.str[16:20].astype('int')
munip_char_unem_2_panel = munip_char_unem_2_panel.reset_index()[['ADM2_PCODE', 'Year', 'Unemployment_Rate_Imputed_2']]

munip_char_panel = munip_char_pop_panel.merge(munip_char_pop_panel_2.merge(munip_char_unem_panel.merge(munip_char_unem_2_panel, on = ['ADM2_PCODE', 'Year'], how = 'left'), 
                                                                           on = ['ADM2_PCODE', 'Year'], how = 'left'), on = ['ADM2_PCODE', 'Year'], how = 'left')
munip_char_panel = munip_char_panel.merge(to_merge[['ADM2_PCODE', 'Year','No_of_elementary_school', 'NO_of_lower_secondary_school', 'No_of_upper_secondary_schools', 'No_of_elementary_school_Teacher', 'No_of_lower_secondary_school_teacher']], on = ['ADM2_PCODE', 'Year'], how = 'left')

munip_char_panel.insert(len(munip_char_panel.columns), 'No_of_elementary_school_percap_1', munip_char_panel['No_of_elementary_school'] / munip_char_panel.Population_Imputed_1)
munip_char_panel.insert(len(munip_char_panel.columns), 'NO_of_lower_secondary_school_percap_1', munip_char_panel['NO_of_lower_secondary_school'] / munip_char_panel.Population_Imputed_1)
munip_char_panel.insert(len(munip_char_panel.columns), 'No_of_upper_secondary_schools_percap_1', munip_char_panel['No_of_upper_secondary_schools'] / munip_char_panel.Population_Imputed_1)
munip_char_panel.insert(len(munip_char_panel.columns), 'No_of_elementary_school_Teacher_1', munip_char_panel['No_of_elementary_school_Teacher'] / munip_char_panel.Population_Imputed_1)
munip_char_panel.insert(len(munip_char_panel.columns), 'No_of_lower_secondary_school_teacher_percap_1', munip_char_panel['No_of_lower_secondary_school_teacher'] / munip_char_panel.Population_Imputed_1)

munip_char_panel.insert(len(munip_char_panel.columns), 'No_of_elementary_school_percap_2', munip_char_panel['No_of_elementary_school'] / munip_char_panel.Population_Imputed_2)
munip_char_panel.insert(len(munip_char_panel.columns), 'NO_of_lower_secondary_school_percap_2', munip_char_panel['NO_of_lower_secondary_school'] / munip_char_panel.Population_Imputed_2)
munip_char_panel.insert(len(munip_char_panel.columns), 'No_of_upper_secondary_schools_percap_2', munip_char_panel['No_of_upper_secondary_schools'] / munip_char_panel.Population_Imputed_2)
munip_char_panel.insert(len(munip_char_panel.columns), 'No_of_elementary_school_Teacher_2', munip_char_panel['No_of_elementary_school_Teacher'] / munip_char_panel.Population_Imputed_2)
munip_char_panel.insert(len(munip_char_panel.columns), 'No_of_lower_secondary_school_teacher_percap_2', munip_char_panel['No_of_lower_secondary_school_teacher'] / munip_char_panel.Population_Imputed_2)
#%%

df_japan.insert(0, 'transaction_ID', np.arange(0, len(df_japan)))

df_japan_land_building = df_japan[(df_japan['Area(m^2)'] != '2,000 m^2 or greater.') & (df_japan['Type'] == 'Residential Land(Land and Building)') & (df_japan['Total floor area(m^2)'] != '2,000 m^2 or greater.') & (df_japan['Total floor area(m^2)'] != 'less than 10 m&sup2.')].dropna(subset = ['Total floor area(m^2)']).copy()
df_japan_land_building['Transaction-price(per_sqr)'] = df_japan_land_building['Transaction_price_adjusted'] / df_japan_land_building['Total floor area(m^2)'].astype('int')
df_japan_condominiums = df_japan[(df_japan['Area(m^2)'] != '2,000 m^2 or greater.') & (df_japan['Type'] == 'Pre-owned Condominiums, etc.')].copy()

df_japan_land_building.drop(drop_list, axis = 1, inplace = True)
df_japan_condominiums.drop(drop_list, axis = 1, inplace = True)

df_japan_condominiums.isna().sum()
df_japan_land_building.isna().sum()
df_japan_land_building.dropna(subset = ['Building structure']).isna().sum()

land_building_year_na = df_japan_land_building[df_japan_land_building['Year of construction'].isna()]
land_building_year_not_na = df_japan_land_building[df_japan_land_building['Year of construction'].notna()]

list_counts = ['Year', 'Prefecture', 'Year of construction', 'Building structure', 'Use', 'City Planning', 
               'BC_rate', 'FA_Rate', 'Transaction period']

col_name_checks = ['dist_full', 'dist_no_na', 'dist_na']
list_dist_checks = []

for i in np.arange(0, len(list_counts)):
    string_column = list_counts[i]
    df_loop_dist = (df_japan_land_building.groupby(string_column).agg({string_column:'count'})/len(df_japan_land_building)).merge((land_building_year_not_na.groupby([string_column]).agg({string_column:'count'})/len(land_building_year_not_na)).merge(land_building_year_na.groupby([string_column]).agg({string_column:'count'})/len(land_building_year_na), left_index = True, right_index = True), left_index = True, right_index = True)
    for y in np.arange(0, 3):
        df_loop_dist.columns = df_loop_dist.columns.str.replace(df_loop_dist.columns[y], col_name_checks[y])
    list_dist_checks.append(df_loop_dist)
    
distribution_checks = pd.concat(list_dist_checks, axis = 0)
for_first_land_building = df_japan_land_building.drop('Layout', axis = 1).dropna().copy()
for_first_condoominum = df_japan_condominiums.dropna().copy()

in_japaneese = []

mapping = pd.read_csv('Mapping Files/Transaction_ID_Maping_Python.csv')[['ID_1', 'ADM2_PCODE']]
df_japan_condominiums = df_japan_condominiums.merge(mapping, left_on = 'City,Town,Ward,Village code', right_on = 'ID_1', how = 'left')
df_japan_land_building = df_japan_land_building.merge(mapping, left_on = 'City,Town,Ward,Village code', right_on = 'ID_1', how = 'left')

df_japan_condominiums = df_japan_condominiums.merge(df_eq_risk, on = ['Year', 'ADM2_PCODE'], how = 'left')
df_japan_land_building = df_japan_land_building.merge(df_eq_risk, on = ['Year', 'ADM2_PCODE'], how = 'left')


#%%
earthquakes_5_plus = pd.read_csv('Earthquake Info\All Earthquakes with 5+ max intensity.csv')
earthquakes_5_minus = pd.read_csv('Earthquake Info\All Earthquakes with 5- max intensity.csv')
earthquakes_2011 = pd.read_csv('Earthquake Info\All Earthquakes with 2011 max intensity.csv')

all_earthquakes = earthquakes_5_plus.merge(earthquakes_5_minus.merge(earthquakes_2011, on = ['Location Name JA', 'Prefecture ID', 'Prefecture Name', 'Shi-Chou-Son ID', 'Shi-Chou-Son Name'], how = 'left'), on = ['Location Name JA', 'Prefecture ID', 'Prefecture Name', 'Shi-Chou-Son ID', 'Shi-Chou-Son Name'], how = 'left')

earthquakes = earthquakes_5_plus.columns[5:len(earthquakes_5_plus.columns)]
earthquakes = earthquakes.append(earthquakes_5_minus.columns[5:len(earthquakes_5_minus.columns)])
earthquakes = earthquakes.append(earthquakes_2011.columns[5:len(earthquakes_2011.columns)])

df_earthquakes = pd.DataFrame()
df_earthquakes['ID'] = earthquakes
df_earthquakes['Year'] = df_earthquakes['ID'].str[:4].astype('int')
df_earthquakes['Quarter'] = np.where(df_earthquakes['ID'].str[5:7].astype('int').isin([1, 2, 3]), '1st quarter', np.where(df_earthquakes['ID'].str[5:7].astype('int').isin([4, 5, 6]), '2nd quarter', np.where(df_earthquakes['ID'].str[5:7].astype('int').isin([7, 8, 9]), '3rd quarter', '4th quarter')))
df_earthquakes['Year_Month'] = df_earthquakes['ID'].str[:4] + '-' + df_earthquakes['ID'].str[5:7]
df_earthquakes['Period'] = df_earthquakes['Quarter'] + ' ' + df_earthquakes['Year'].astype('str')
    
dfs_earthquake = []

for i in earthquakes:
    df_for_earthquake = pd.DataFrame()
    df_for_earthquake['Location_Name_JA'] = all_earthquakes['Location Name JA']
    df_for_earthquake['Prefecture_ID'] = all_earthquakes['Prefecture ID']
    df_for_earthquake['Prefecture_Name'] = all_earthquakes['Prefecture Name']
    df_for_earthquake['Shi_Chou_Son_ID'] = all_earthquakes['Shi-Chou-Son ID']
    df_for_earthquake.loc[:, 'Earthquake_ID'] = i
    df_for_earthquake['Intensity'] = all_earthquakes[i]
    df_for_earthquake.loc[:, 'Period'] = df_earthquakes[df_earthquakes['ID'] == i]['Period'].values[0]
    df_for_earthquake.loc[:, 'Year_Month'] = df_earthquakes[df_earthquakes['ID'] == i]['Year_Month'].values[0]    
    dfs_earthquake.append(df_for_earthquake)


panel_all_earthquakes = pd.concat(dfs_earthquake, axis = 0)    
panel_all_earthquakes['5_low'] = np.where(panel_all_earthquakes['Intensity'].isin(['Seismic Intensity 5-']), 1, 0)
panel_all_earthquakes['5_high'] = np.where(panel_all_earthquakes['Intensity'].isin(['Seismic Intensity 5+']), 1, 0)
panel_all_earthquakes['6_low'] = np.where(panel_all_earthquakes['Intensity'].isin(['Seismic Intensity 6-']), 1, 0)
panel_all_earthquakes['6_high_or_higher'] = np.where(panel_all_earthquakes['Intensity'].isin(['Seismic Intensity 7', 'Seismic Intensity 6+']), 1, 0)
panel_all_earthquakes['6_low_or_higher'] = np.where(panel_all_earthquakes['Intensity'].isin(['Seismic Intensity 7', 'Seismic Intensity 6+', 'Seismic Intensity 6-']), 1, 0)
panel_all_earthquakes['5_high_or_higher'] = np.where(panel_all_earthquakes['Intensity'].isin(['Seismic Intensity 7', 'Seismic Intensity 6+', 'Seismic Intensity 6-', 'Seismic Intensity 5+']), 1, 0)
panel_all_earthquakes['5_low_or_higher'] = np.where(panel_all_earthquakes['Intensity'].isin(['Seismic Intensity 7', 'Seismic Intensity 6+', 'Seismic Intensity 6-', 'Seismic Intensity 5+', 'Seismic Intensity 5-']), 1, 0)

panel_all_earthquakes['Period_2'] = panel_all_earthquakes.Period.str[12:] + ' ' + panel_all_earthquakes.Period.str[:3] + ' quarter'

wide_all_earthquakes_5_high = panel_all_earthquakes.pivot_table(index='Shi_Chou_Son_ID',
                                                                columns='Period_2',
                                                                values= '5_high',
                                                                aggfunc='sum')

wide_all_earthquakes_5_higher = panel_all_earthquakes.pivot_table(index='Shi_Chou_Son_ID',
                                                                  columns='Period_2',
                                                                  values= '5_high_or_higher',
                                                                  aggfunc='sum')

first_earthquake_period_dict = {}
wide_all_earthquakes_5_high_2014 = wide_all_earthquakes_5_high.iloc[:, 24:]
wide_all_earthquakes_5_higher_2014 = wide_all_earthquakes_5_higher.iloc[:, 24:]
wide_all_earthquakes_5_high.insert(56, 'First_Eq_Period_5high', '9999 4th period')
wide_all_earthquakes_5_higher.insert(56, 'First_Eq_Period_5higher', '9999 4th period')
wide_all_earthquakes_5_high_2014.insert(32, 'First_Eq_Period_5high_2014', '9999 4th period')
wide_all_earthquakes_5_higher_2014.insert(32, 'First_Eq_Period_5higher_2014', '9999 4th period')
            
for i in np.arange(0, len(wide_all_earthquakes_5_high.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_5_high)):
        if wide_all_earthquakes_5_high.iloc[j, i] > 0:
           wide_all_earthquakes_5_high.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_5_high.columns[i]
           wide_all_earthquakes_5_high.iloc[j, 56] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_5_high.index[j]] = wide_all_earthquakes_5_high.columns[i]
        if wide_all_earthquakes_5_high.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_5_high.columns)-1), i):
               wide_all_earthquakes_5_high.iloc[j, k] = 0
               
for i in np.arange(0, len(wide_all_earthquakes_5_higher.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_5_higher)):
        if wide_all_earthquakes_5_higher.iloc[j, i] > 0:
           wide_all_earthquakes_5_higher.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_5_higher.columns[i]
           wide_all_earthquakes_5_higher.iloc[j, 56] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_5_higher.index[j]] = wide_all_earthquakes_5_higher.columns[i]
        if wide_all_earthquakes_5_higher.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_5_higher.columns)-1), i):
               wide_all_earthquakes_5_higher.iloc[j, k] = 0
            
for i in np.arange(0, len(wide_all_earthquakes_5_high_2014.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_5_high_2014)):
        if wide_all_earthquakes_5_high_2014.iloc[j, i] > 0:
           wide_all_earthquakes_5_high_2014.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_5_high_2014.columns[i]
           wide_all_earthquakes_5_high_2014.iloc[j, 32] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_5_high_2014.index[j]] = wide_all_earthquakes_5_high_2014.columns[i]
        if wide_all_earthquakes_5_high_2014.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_5_high_2014.columns)-1), i):
               wide_all_earthquakes_5_high_2014.iloc[j, k] = 0
               
for i in np.arange(0, len(wide_all_earthquakes_5_higher_2014.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_5_higher_2014)):
        if wide_all_earthquakes_5_higher_2014.iloc[j, i] > 0:
           wide_all_earthquakes_5_higher_2014.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_5_higher_2014.columns[i]
           wide_all_earthquakes_5_higher_2014.iloc[j, 32] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_5_higher_2014.index[j]] = wide_all_earthquakes_5_higher_2014.columns[i]
        if wide_all_earthquakes_5_higher_2014.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_5_higher_2014.columns)-1), i):
               wide_all_earthquakes_5_higher_2014.iloc[j, k] = 0
               
panel_all_earthquakes_grouped = panel_all_earthquakes.groupby(['Shi_Chou_Son_ID', 'Period_2']).agg(total_5_low = ('5_low', 'sum'),
                                                                                                 total_5_high = ('5_high', 'sum'),
                                                                                                 total_6_low = ('6_low', 'sum'),
                                                                                                 total_6_high_or_higher = ('6_high_or_higher', 'sum'),
                                                                                                 total_6_low_or_higher = ('6_low_or_higher', 'sum'),
                                                                                                 total_5_high_or_higher = ('5_high_or_higher', 'sum'),
                                                                                                 total_5_low_or_higher = ('5_low_or_higher', 'sum')).copy()

panel_all_earthquakes_grouped['atleast_one_5_low'] = np.where(panel_all_earthquakes_grouped['total_5_low'] > 0, 1, 0)
panel_all_earthquakes_grouped['atleast_one_5_high'] = np.where(panel_all_earthquakes_grouped['total_5_high'] > 0, 1, 0)
panel_all_earthquakes_grouped['atleast_one_6_low'] = np.where(panel_all_earthquakes_grouped['total_6_low'] > 0, 1, 0)
panel_all_earthquakes_grouped['atleast_one_6_high_or_higher'] = np.where(panel_all_earthquakes_grouped['total_6_high_or_higher'] > 0, 1, 0)
panel_all_earthquakes_grouped['atleast_one_6_low_or_higher'] = np.where(panel_all_earthquakes_grouped['total_6_low_or_higher'] > 0, 1, 0)
panel_all_earthquakes_grouped['atleast_one_5_high_or_higher'] = np.where(panel_all_earthquakes_grouped['total_5_high_or_higher'] > 0, 1, 0)
panel_all_earthquakes_grouped['atleast_one_5_low_or_higher'] = np.where(panel_all_earthquakes_grouped['total_5_low_or_higher'] > 0, 1, 0)

panel_all_earthquakes_grouped['lag_atleast_one_5_high'] = panel_all_earthquakes_grouped['total_5_high'].shift(1)
panel_all_earthquakes_grouped['lag2_atleast_one_5_high'] = panel_all_earthquakes_grouped['total_5_high'].shift(2)
panel_all_earthquakes_grouped['lag3_atleast_one_5_high'] = panel_all_earthquakes_grouped['total_5_high'].shift(3)
panel_all_earthquakes_grouped['lag_atleast_one_5_high_or_higher'] = panel_all_earthquakes_grouped['total_5_high_or_higher'].shift(1)
panel_all_earthquakes_grouped['lag2_atleast_one_5_high_or_higher'] = panel_all_earthquakes_grouped['total_5_high_or_higher'].shift(2)
panel_all_earthquakes_grouped['lag3_atleast_one_5_hivgh_or_higher'] = panel_all_earthquakes_grouped['total_5_high_or_higher'].shift(3)
panel_all_earthquakes_grouped['lag4_atleast_one_5_high_or_higher'] = panel_all_earthquakes_grouped['total_5_high_or_higher'].shift(4)

panel_all_earthquakes_grouped.insert(len(panel_all_earthquakes_grouped.columns), 'Period', panel_all_earthquakes_grouped.index.get_level_values('Period_2').str[5:8] + ' quarter ' + panel_all_earthquakes_grouped.index.get_level_values('Period_2').str[0:4])
panel_all_earthquakes_grouped = panel_all_earthquakes_grouped.reset_index().set_index(['Shi_Chou_Son_ID', 'Period']).copy()
panel_all_earthquakes_grouped.drop('Period_2', axis =  1, inplace = True)

panel_all_earthquakes_grouped.insert(0, 'Year', panel_all_earthquakes_grouped.index.get_level_values('Period').str[12:16].astype('int'))
panel_all_earthquakes_grouped.insert(1, 'Quarter', panel_all_earthquakes_grouped.index.get_level_values('Period').str[0:3].astype('str'))

panel_all_earthquakes_grouped.rename({panel_all_earthquakes_grouped.columns.values[0]:'Do not use'}, axis = 'columns', inplace = True)

data_condominiums_R = df_japan_condominiums.merge(panel_all_earthquakes_grouped.reset_index(), left_on = ['ADM2_PCODE', 'Transaction period'], right_on= ['Shi_Chou_Son_ID', 'Period'], how = 'inner')
data_buildings_R = df_japan_land_building.merge(panel_all_earthquakes_grouped.reset_index(), left_on = ['ADM2_PCODE', 'Transaction period'], right_on= ['Shi_Chou_Son_ID', 'Period'], how = 'inner')
data_condominiums_R_only_house = df_japan_condominiums[df_japan_condominiums['Use'].str[:5] == 'House'].merge(panel_all_earthquakes_grouped.reset_index(), left_on = ['ADM2_PCODE', 'Transaction period'], right_on= ['Shi_Chou_Son_ID', 'Period'], how = 'inner')
data_buildings_R_only_house = df_japan_land_building[df_japan_land_building['Use'].str[:4] == 'Hous'].merge(panel_all_earthquakes_grouped.reset_index(), left_on = ['ADM2_PCODE', 'Transaction period'], right_on= ['Shi_Chou_Son_ID', 'Period'], how = 'inner')

list_df = [data_condominiums_R, data_buildings_R, data_condominiums_R_only_house, data_buildings_R_only_house]

for z in np.arange(0, 4):
    df = list_df[z]
    df = df.merge(wide_all_earthquakes_5_high['First_Eq_Period_5high'], left_on = 'ADM2_PCODE', right_index = True, how = 'left')
    df = df.merge(wide_all_earthquakes_5_higher['First_Eq_Period_5higher'], left_on = 'ADM2_PCODE', right_index = True, how = 'left')
    df = df.merge(wide_all_earthquakes_5_high_2014['First_Eq_Period_5high_2014'], left_on = 'ADM2_PCODE', right_index = True, how = 'left')
    df = df.merge(wide_all_earthquakes_5_higher_2014['First_Eq_Period_5higher_2014'], left_on = 'ADM2_PCODE', right_index = True, how = 'left')
    df['period_ago_5high'] = (df['Transaction period'].str[12:16].astype(int) - df.First_Eq_Period_5high.str[0:4].astype(int))*4 + df['Transaction period'].str[0:1].astype(int) - df.First_Eq_Period_5high.str[5:6].astype(int) 
    df['period_ago_5higher'] = (df['Transaction period'].str[12:16].astype(int) - df.First_Eq_Period_5higher.str[0:4].astype(int))*4 + df['Transaction period'].str[0:1].astype(int) - df.First_Eq_Period_5higher.str[5:6].astype(int) 
    df['period_ago_5high_2014'] = (df['Transaction period'].str[12:16].astype(int) - df.First_Eq_Period_5high_2014.str[0:4].astype(int))*4 + df['Transaction period'].str[0:1].astype(int) - df.First_Eq_Period_5high_2014.str[5:6].astype(int) 
    df['period_ago_5higher_2014'] = (df['Transaction period'].str[12:16].astype(int) - df.First_Eq_Period_5higher_2014.str[0:4].astype(int))*4 + df['Transaction period'].str[0:1].astype(int) - df.First_Eq_Period_5higher_2014.str[5:6].astype(int) 
    for i in np.arange(-21, 22):
        if i < 0:
            string = f'tre_minus{-i}_5high'
        else:
            string = f'tre_plus{i}_5high'
        if i == -21:
            df[string] = np.multiply((df['period_ago_5high'] <= i) & (df['period_ago_5high'] > -500), 1)
        elif i == 21:
            df[string] = np.multiply((df['period_ago_5high'] >= i) & (df['period_ago_5high'] < 500), 1)
        else:
            df[string] = np.multiply(df['period_ago_5high'] == i, 1)
    for k in ['0_4', '5_8', '9_12', '13_16', '17_20', '0_10', '11_20', '0_20']:
        string_2 = f'tre_plus_{k}_5high'
        df[string_2] = np.multiply((df['period_ago_5high'] >= int(k[0:k.find('_')])) & (df['period_ago_5high'] <= int(k[k.find('_')+1:])), 1)
    string_3 = 'tre_minus_20_plus_20_5high'
    df[string_3] = np.multiply((df['period_ago_5high'] >= -20) & (df['period_ago_5high'] <= 20), 1)
    list_df[z] = df.copy()
    
list_all_df = list_df
list_year = [2008, 2008, 2008, 2008]

for z in np.arange(0, 4):
    df = list_df[z]
    str1 = 'First_Period_5high'
    str2 = 'First_Eq_Period_5high'
    str3 = 'First_Period_5higher'
    str4 = 'First_Eq_Period_5higher'
    str5 = 'First_Period_5high_2014'
    str6 = 'First_Eq_Period_5high_2014'
    str7 = 'First_Period_5higher_2014'
    str8 = 'First_Eq_Period_5higher_2014'
    df[str1] = (df[str2].str[0:4].astype('int') - list_year[z]) * 4 + df[str2].str[5].astype('int')
    df[str3] = (df[str4].str[0:4].astype('int') - list_year[z]) * 4 + df[str4].str[5].astype('int')
    df[str5] = (df[str2].str[0:4].astype('int') - list_year[z]) * 4 + df[str6].str[5].astype('int')
    df[str7] = (df[str4].str[0:4].astype('int') - list_year[z]) * 4 + df[str8].str[5].astype('int')
    df['Period_Now'] = (df['Transaction period'].str[12:16].astype('int') - list_year[z]) * 4 + df['Transaction period'].str[0].astype('int')
    df = df.merge(munip_char_panel.drop('Year', axis = 1), on = ['ADM2_PCODE', 'Period'], how = 'left')
    list_df[z] = df

data_condominiums_R = list_df[0].copy()
data_buildings_R = list_df[1].copy()
data_condominiums_R_only_house = list_df[2].copy()
data_buildings_R_only_house = list_df[3].copy()

data_condominiums_R.isna().sum()
data_buildings_R.isna().sum()

data_condominiums_R.dropna(subset = ['Year of construction', 'Building structure', 'City Planning', 'BC_rate', 'Nearest_Station_dist_min', 'Layout', 'Population_Imputed_1', 'Unemployment_Rate_Imputed_2', 'No_of_elementary_school_percap_1'], inplace = True)
data_condominiums_R_only_house.dropna(subset = ['Year of construction', 'Building structure', 'City Planning', 'BC_rate', 'Nearest_Station_dist_min', 'Layout', 'Population_Imputed_1', 'Unemployment_Rate_Imputed_2', 'No_of_elementary_school_percap_1'], inplace = True)
data_buildings_R.dropna(subset = ['Year of construction', 'Building structure', 'City Planning', 'BC_rate', 'Nearest_Station_dist_min', 'Population_Imputed_1', 'Unemployment_Rate_Imputed_2', 'No_of_elementary_school_percap_1'], inplace = True)
data_buildings_R_only_house.dropna(subset = ['Year of construction', 'Building structure', 'City Planning', 'BC_rate', 'Nearest_Station_dist_min', 'Population_Imputed_1', 'Unemployment_Rate_Imputed_2', 'No_of_elementary_school_percap_1'], inplace = True)
data_condominiums_R = data_condominiums_R[data_condominiums_R['Year of construction'] != 'before the war']
data_condominiums_R['Year of construction'] = data_condominiums_R['Year of construction'].astype('int')
data_condominiums_R_only_house = data_condominiums_R_only_house[data_condominiums_R_only_house['Year of construction'] != 'before the war']
data_condominiums_R_only_house['Year of construction'] = data_condominiums_R_only_house['Year of construction'].astype('int')
data_buildings_R = data_buildings_R[data_buildings_R['Year of construction'] != 'before the war']
data_buildings_R['Year of construction'] = data_buildings_R['Year of construction'].astype('int')
data_buildings_R_only_house = data_buildings_R_only_house[data_buildings_R_only_house['Year of construction'] != 'before the war']
data_buildings_R_only_house['Year of construction'] = data_buildings_R_only_house['Year of construction'].astype('int')

data_condominiums_R = data_condominiums_R[(data_condominiums_R['Layout'] != 'Open Floor') & (data_condominiums_R['Layout'] != 'Duplex')]
data_condominiums_R_only_house = data_condominiums_R_only_house[(data_condominiums_R_only_house['Layout'] != 'Open Floor') & (data_condominiums_R_only_house['Layout'] != 'Duplex')]

to_merge_2 = {'1LDK':2, '3LDK':4, '2LDK':3, '4LDK':5, '1LDK+S':2, '1DK':2, '2DK':3, '1K':2, '2K':3,
            '3DK':4, '3LDK+S':4, '1R':1, '5LDK':6, '2LDK+S':3, '1DK+S':2, '2DK+S':3, '4LDK+S':5, 
            '4DK':5, '1DK+K':2, '3K':4, '3K+S':4, '2LD+S':3, '3DK+S':4, '4K':5, '3LK':4, '3LD':4, 
            '4DK+S':5, 'Studio Apartment':1, '2K+S':3, '2LK':3, '5DK':6, '3LD+S':4, '6LDK':7, '2LD': 3,
            '1R+S':1, '3LDK+K':4, '2L':3, '1L':2, '4L':5, '4L+K':5, '2LK+S':3, '4D':5, '6LDK+S':7, 
            '1LK':2, '6DK':7, '5LDK+S':6, '1LD+S':2, '2D':3, '1K+S':2, '5K':6, '4LDK+K':5, '3D':4, 
            '7LDK':8, '8LDK+S':9, '7LDK+S':8, '8LDK':9, '7DK':8, '5LDK+K':6, '2L+S':3, '2LDK+K':3, 
            '1L+S':2, '1LK+S':2, '1LDK+K':2}

to_m = pd.DataFrame(to_merge_2.items(), columns=['Layout', 'No_Rooms'])
data_condominiums_R = data_condominiums_R.merge(to_m, on = 'Layout', how ='left')
data_condominiums_R_only_house = data_condominiums_R_only_house.merge(to_m, on = 'Layout', how ='left')

data_condominiums_R = data_condominiums_R[(data_condominiums_R['Transaction-price(total)'] > data_condominiums_R['Transaction-price(total)'].quantile(0.025)) & (data_condominiums_R['Transaction-price(total)'] < data_condominiums_R['Transaction-price(total)'].quantile(0.975))]
data_condominiums_R_only_house = data_condominiums_R_only_house[(data_condominiums_R_only_house['Transaction-price(total)'] > data_condominiums_R_only_house['Transaction-price(total)'].quantile(0.025)) & (data_condominiums_R_only_house['Transaction-price(total)'] < data_condominiums_R_only_house['Transaction-price(total)'].quantile(0.975))] 
data_buildings_R = data_buildings_R[(data_buildings_R['Transaction-price(total)'] > data_buildings_R['Transaction-price(total)'].quantile(0.025)) & (data_buildings_R['Transaction-price(total)'] < data_buildings_R['Transaction-price(total)'].quantile(0.975))]
data_buildings_R_only_house = data_buildings_R_only_house[(data_buildings_R_only_house['Transaction-price(total)'] > data_buildings_R_only_house['Transaction-price(total)'].quantile(0.025)) & (data_buildings_R_only_house['Transaction-price(total)'] < data_buildings_R_only_house['Transaction-price(total)'].quantile(0.975))]

data_condominiums_R.to_csv('data_condominiums_R.csv')
data_buildings_R.to_csv('data_buildings_R.csv')
data_condominiums_R_only_house.to_csv('data_condominiums_R_only_house.csv')
data_buildings_R_only_house.to_csv('data_buildings_R_only_house.csv')

a = np.isin(df_japan_land_building['ADM2_PCODE'].unique(), panel_all_earthquakes_grouped.reset_index()['Shi_Chou_Son_ID'].unique())
np.isin(df_japan_land_building[df_japan_land_building.Year >=2012]['Transaction period'].unique(), panel_all_earthquakes_grouped.reset_index()['Shi_Chou_Son_ID'].unique())

data_condominiums_R[data_condominiums_R['Period'] == '2nd quarter 2016'].groupby('Shi_Chou_Son_ID').count().iloc[:, 0].sort_values()


data_buildings_R_only_house.Use.unique()

wide_all_earthquakes_5high = panel_all_earthquakes.pivot_table(index='Shi_Chou_Son_ID',
                                                               columns='Period_2',
                                                               values= '5_high',
                                                               aggfunc='sum')

wide_all_earthquakes_5higher = panel_all_earthquakes.pivot_table(index='Shi_Chou_Son_ID',
                                                               columns='Period_2',
                                                               values= '5_high_or_higher',
                                                               aggfunc='sum')

wide_all_earthquakes_6_low = panel_all_earthquakes.pivot_table(index='Shi_Chou_Son_ID',
                                                               columns='Period_2',
                                                               values= '6_low',
                                                               aggfunc='sum')

wide_all_earthquakes_6_low_higher = panel_all_earthquakes.pivot_table(index='Shi_Chou_Son_ID',
                                                                      columns='Period_2',
                                                                      values= '6_low_or_higher',
                                                                      aggfunc='sum')

wide_all_earthquakes_5_high_2010 = wide_all_earthquakes_5high.iloc[:, 9:]
wide_all_earthquakes_5_higher_2010 = wide_all_earthquakes_5higher.iloc[:, 9:]
wide_all_earthquakes_6_low_2010 = wide_all_earthquakes_6_low.iloc[:, 9:]
wide_all_earthquakes_6_low_higher_2010 = wide_all_earthquakes_6_low_higher.iloc[:, 9:]
wide_all_earthquakes_5_high_2010.insert(len(wide_all_earthquakes_5_high_2010.columns), 'First_Eq_Period_5high_2010', '9999 4th period')
wide_all_earthquakes_5_higher_2010.insert(len(wide_all_earthquakes_5_higher_2010.columns), 'First_Eq_Period_5higher_2010', '9999 4th period')
wide_all_earthquakes_6_low.insert(len(wide_all_earthquakes_6_low.columns), 'First_Eq_Period_6low', '9999 4th period')
wide_all_earthquakes_6_low_higher.insert(len(wide_all_earthquakes_6_low_higher.columns), 'First_Eq_Period_6low_higher', '9999 4th period')
wide_all_earthquakes_6_low_2010.insert(len(wide_all_earthquakes_6_low_2010.columns), 'First_Eq_Period_6low_2010', '9999 4th period')
wide_all_earthquakes_6_low_higher_2010.insert(len(wide_all_earthquakes_6_low_higher_2010.columns), 'First_Eq_Period_6low_higher_2010', '9999 4th period')

for i in np.arange(0, len(wide_all_earthquakes_6_low_2010.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_6_low_2010)):
        if wide_all_earthquakes_6_low_2010.iloc[j, i] > 0:
           wide_all_earthquakes_6_low_2010.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_6_low_2010.columns[i]
           wide_all_earthquakes_6_low_2010.iloc[j, len(wide_all_earthquakes_6_low_2010.columns)-1] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_6_low_2010.index[j]] = wide_all_earthquakes_6_low_2010.columns[i]
        if wide_all_earthquakes_6_low_2010.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_6_low_2010.columns)-1), i):
               wide_all_earthquakes_6_low_2010.iloc[j, k] = 0
               
for i in np.arange(0, len(wide_all_earthquakes_6_low_higher_2010.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_6_low_higher_2010)):
        if wide_all_earthquakes_6_low_higher_2010.iloc[j, i] > 0:
           wide_all_earthquakes_6_low_higher_2010.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_6_low_higher_2010.columns[i]
           wide_all_earthquakes_6_low_higher_2010.iloc[j, len(wide_all_earthquakes_6_low_higher_2010.columns)-1] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_6_low_higher_2010.index[j]] = wide_all_earthquakes_6_low_higher_2010.columns[i]
        if wide_all_earthquakes_6_low_higher_2010.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_6_low_higher_2010.columns)-1), i):
               wide_all_earthquakes_6_low_higher_2010.iloc[j, k] = 0               

for i in np.arange(0, len(wide_all_earthquakes_6_low_higher.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_6_low_higher)):
        if wide_all_earthquakes_6_low_higher.iloc[j, i] > 0:
           wide_all_earthquakes_6_low_higher.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_6_low_higher.columns[i]
           wide_all_earthquakes_6_low_higher.iloc[j, len(wide_all_earthquakes_6_low_higher.columns)-1] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_6_low_higher.index[j]] = wide_all_earthquakes_6_low_higher.columns[i]
        if wide_all_earthquakes_6_low_higher.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_6_low_higher.columns)-1), i):
               wide_all_earthquakes_6_low_higher.iloc[j, k] = 0
               
for i in np.arange(0, len(wide_all_earthquakes_6_low.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_6_low)):
        if wide_all_earthquakes_6_low.iloc[j, i] > 0:
           wide_all_earthquakes_6_low.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_6_low.columns[i]
           wide_all_earthquakes_6_low.iloc[j, len(wide_all_earthquakes_6_low.columns)-1] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_6_low.index[j]] = wide_all_earthquakes_6_low.columns[i]
        if wide_all_earthquakes_6_low.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_6_low.columns)-1), i):
               wide_all_earthquakes_6_low.iloc[j, k] = 0
               
for i in np.arange(0, len(wide_all_earthquakes_5_high_2010.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_5_high_2010)):
        if wide_all_earthquakes_5_high_2010.iloc[j, i] > 0:
           wide_all_earthquakes_5_high_2010.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_5_high_2010.columns[i]
           wide_all_earthquakes_5_high_2010.iloc[j, len(wide_all_earthquakes_5_high_2010.columns)-1] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_5_high_2010.index[j]] = wide_all_earthquakes_5_high_2010.columns[i]
        if wide_all_earthquakes_5_high_2010.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_5_high_2010.columns)-1), i):
               wide_all_earthquakes_5_high_2010.iloc[j, k] = 0

               
for i in np.arange(0, len(wide_all_earthquakes_5_higher_2010.columns)-1):
    for j in np.arange(0, len(wide_all_earthquakes_5_higher_2010)):
        if wide_all_earthquakes_5_higher_2010.iloc[j, i] > 0:
           wide_all_earthquakes_5_higher_2010.iloc[j, i] = 1
           first_eq_period = wide_all_earthquakes_5_higher_2010.columns[i]
           wide_all_earthquakes_5_higher_2010.iloc[j, len(wide_all_earthquakes_5_higher_2010.columns)-1] = first_eq_period
           first_earthquake_period_dict[wide_all_earthquakes_5_higher_2010.index[j]] = wide_all_earthquakes_5_higher_2010.columns[i]
        if wide_all_earthquakes_5_higher_2010.iloc[j, i] > 0:
           for k in np.delete(np.arange(0, len(wide_all_earthquakes_5_higher_2010.columns)-1), i):
               wide_all_earthquakes_5_higher_2010.iloc[j, k] = 0


               
munip_social_exp = pd.read_csv('Municipality Data\Municipalities_All_Expenditures_Panel.csv')
munip_social_exp.rename({'Time':'FY_Year'}, axis = 'columns', inplace = True)
munip_social_exp.insert(len(munip_social_exp.columns), 'pop_period', '1st quarter ' + munip_social_exp.FY_Year.str[2:7])
munip_social_exp = munip_social_exp.merge(munip_char_panel, left_on = ['ADM2_PCODE', 'pop_period'], right_on = ['ADM2_PCODE', 'Period'], how = 'left')
munip_social_exp = munip_social_exp.merge(df_eq_risk.iloc[:, np.r_[0, 9:len(df_eq_risk.columns)]], on = ['ADM2_PCODE', 'Year'], how = 'left')
munip_social_exp = munip_social_exp.merge(wide_all_earthquakes_5_high['First_Eq_Period_5high'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_exp = munip_social_exp.merge(wide_all_earthquakes_5_higher['First_Eq_Period_5higher'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_exp = munip_social_exp.merge(wide_all_earthquakes_5_high_2010['First_Eq_Period_5high_2010'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_exp = munip_social_exp.merge(wide_all_earthquakes_5_higher_2010['First_Eq_Period_5higher_2010'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_exp = munip_social_exp.merge(wide_all_earthquakes_6_low['First_Eq_Period_6low'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_exp = munip_social_exp.merge(wide_all_earthquakes_6_low_higher['First_Eq_Period_6low_higher'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_exp = munip_social_exp.merge(wide_all_earthquakes_6_low_2010['First_Eq_Period_6low_2010'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_exp = munip_social_exp.merge(wide_all_earthquakes_6_low_higher_2010['First_Eq_Period_6low_higher_2010'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_exp['Period_Now'] = munip_social_exp.FY_Year.str[2:].astype('int') - 2007

munip_social_exp['First_Eq_Period_5high_FY'] = np.where(munip_social_exp['First_Eq_Period_5high'].str[5].astype('int') == 1, munip_social_exp['First_Eq_Period_5high'].str[0:4].astype('int') - 2007, munip_social_exp['First_Eq_Period_5high'].str[0:4].astype('int') - 2006)
munip_social_exp['First_Eq_Period_5higher_FY'] = np.where(munip_social_exp['First_Eq_Period_5higher'].str[5].astype('int') == 1, munip_social_exp['First_Eq_Period_5higher'].str[0:4].astype('int') - 2007, munip_social_exp['First_Eq_Period_5higher'].str[0:4].astype('int') - 2006)
munip_social_exp['First_Eq_Period_5high_FY_2010'] = np.where(munip_social_exp['First_Eq_Period_5high_2010'].str[5].astype('int') == 1, munip_social_exp['First_Eq_Period_5high_2010'].str[0:4].astype('int') - 2007, munip_social_exp['First_Eq_Period_5high_2010'].str[0:4].astype('int') - 2006)
munip_social_exp['First_Eq_Period_5higher_FY_2010'] = np.where(munip_social_exp['First_Eq_Period_5higher_2010'].str[5].astype('int') == 1, munip_social_exp['First_Eq_Period_5higher_2010'].str[0:4].astype('int') - 2007, munip_social_exp['First_Eq_Period_5higher_2010'].str[0:4].astype('int') - 2006)
munip_social_exp['First_Eq_Period_6low_FY'] = np.where(munip_social_exp['First_Eq_Period_6low'].str[5].astype('int') == 1, munip_social_exp['First_Eq_Period_6low'].str[0:4].astype('int') - 2007, munip_social_exp['First_Eq_Period_6low'].str[0:4].astype('int') - 2006)
munip_social_exp['First_Eq_Period_6low_higher_FY'] = np.where(munip_social_exp['First_Eq_Period_6low_higher'].str[5].astype('int') == 1, munip_social_exp['First_Eq_Period_6low_higher'].str[0:4].astype('int') - 2007, munip_social_exp['First_Eq_Period_6low_higher'].str[0:4].astype('int') - 2006)
munip_social_exp['First_Eq_Period_6low_FY_2010'] = np.where(munip_social_exp['First_Eq_Period_6low_2010'].str[5].astype('int') == 1, munip_social_exp['First_Eq_Period_6low_2010'].str[0:4].astype('int') - 2007, munip_social_exp['First_Eq_Period_6low_2010'].str[0:4].astype('int') - 2006)
munip_social_exp['First_Eq_Period_6low_higher_FY_2010'] = np.where(munip_social_exp['First_Eq_Period_6low_higher_2010'].str[5].astype('int') == 1, munip_social_exp['First_Eq_Period_6low_higher_2010'].str[0:4].astype('int') - 2007, munip_social_exp['First_Eq_Period_6low_higher_2010'].str[0:4].astype('int') - 2006)
munip_social_exp = munip_social_exp[munip_social_exp['Year'] >= 2010]
munip_social_exp = munip_social_exp[(np.logical_not(munip_social_exp.No_of_lower_secondary_school_teacher_percap_1.isna())) & (np.logical_not(munip_social_exp.Population_Imputed_1.isna())) & (np.logical_not(munip_social_exp.Unemployment_Rate_Imputed_2.isna()))]
filt = (munip_social_exp.value_counts('ADM2_PCODE') == 10)[munip_social_exp.value_counts('ADM2_PCODE') == 10].index
munip_social_exp = munip_social_exp[munip_social_exp.ADM2_PCODE.isin(filt)]
filt = munip_social_exp[munip_social_exp['Settlement of total revenue (municipalities)?1,000 yen?'].isna()].ADM2_PCODE.unique()
munip_social_exp = munip_social_exp[np.logical_not(munip_social_exp.ADM2_PCODE.isin(filt))]


munip_social_exp.to_csv('munip_exp_to_R.csv')

munip_social_rev = pd.read_csv('Municipality Data\Full_Detailed_Revenue_Panel.csv')
munip_social_rev.rename({'Time':'FY_Year'}, axis = 'columns', inplace = True)
munip_social_rev.insert(len(munip_social_rev.columns), 'pop_period', '1st quarter ' + munip_social_rev.FY_Year.str[2:7])
munip_social_rev = munip_social_rev.merge(munip_char_panel, left_on = ['ADM2_PCODE', 'pop_period'], right_on = ['ADM2_PCODE', 'Period'], how = 'left')
munip_social_rev = munip_social_rev.merge(df_eq_risk.iloc[:, np.r_[0, 9:len(df_eq_risk.columns)]], on = ['ADM2_PCODE', 'Year'], how = 'left')
munip_social_rev = munip_social_rev.merge(wide_all_earthquakes_5_high['First_Eq_Period_5high'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_rev = munip_social_rev.merge(wide_all_earthquakes_5_higher['First_Eq_Period_5higher'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_rev = munip_social_rev.merge(wide_all_earthquakes_5_high_2010['First_Eq_Period_5high_2010'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_rev = munip_social_rev.merge(wide_all_earthquakes_5_higher_2010['First_Eq_Period_5higher_2010'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_rev = munip_social_rev.merge(wide_all_earthquakes_6_low['First_Eq_Period_6low'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_rev = munip_social_rev.merge(wide_all_earthquakes_6_low_higher['First_Eq_Period_6low_higher'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_rev = munip_social_rev.merge(wide_all_earthquakes_6_low_2010['First_Eq_Period_6low_2010'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_rev = munip_social_rev.merge(wide_all_earthquakes_6_low_higher_2010['First_Eq_Period_6low_higher_2010'], left_on = 'ADM2_PCODE', right_index = True)
munip_social_rev['Period_Now'] = munip_social_rev.FY_Year.str[2:].astype('int') - 2007

munip_social_rev['First_Eq_Period_5high_FY'] = np.where(munip_social_rev['First_Eq_Period_5high'].str[5].astype('int') == 1, munip_social_rev['First_Eq_Period_5high'].str[0:4].astype('int') - 2007, munip_social_rev['First_Eq_Period_5high'].str[0:4].astype('int') - 2006)
munip_social_rev['First_Eq_Period_5higher_FY'] = np.where(munip_social_rev['First_Eq_Period_5higher'].str[5].astype('int') == 1, munip_social_rev['First_Eq_Period_5higher'].str[0:4].astype('int') - 2007, munip_social_rev['First_Eq_Period_5higher'].str[0:4].astype('int') - 2006)
munip_social_rev['First_Eq_Period_5high_FY_2010'] = np.where(munip_social_rev['First_Eq_Period_5high_2010'].str[5].astype('int') == 1, munip_social_rev['First_Eq_Period_5high_2010'].str[0:4].astype('int') - 2007, munip_social_rev['First_Eq_Period_5high_2010'].str[0:4].astype('int') - 2006)
munip_social_rev['First_Eq_Period_5higher_FY_2010'] = np.where(munip_social_rev['First_Eq_Period_5higher_2010'].str[5].astype('int') == 1, munip_social_rev['First_Eq_Period_5higher_2010'].str[0:4].astype('int') - 2007, munip_social_rev['First_Eq_Period_5higher_2010'].str[0:4].astype('int') - 2006)
munip_social_rev['First_Eq_Period_6low_FY'] = np.where(munip_social_rev['First_Eq_Period_6low'].str[5].astype('int') == 1, munip_social_rev['First_Eq_Period_6low'].str[0:4].astype('int') - 2007, munip_social_rev['First_Eq_Period_6low'].str[0:4].astype('int') - 2006)
munip_social_rev['First_Eq_Period_6low_higher_FY'] = np.where(munip_social_rev['First_Eq_Period_6low_higher'].str[5].astype('int') == 1, munip_social_rev['First_Eq_Period_6low_higher'].str[0:4].astype('int') - 2007, munip_social_rev['First_Eq_Period_6low_higher'].str[0:4].astype('int') - 2006)
munip_social_rev['First_Eq_Period_6low_FY_2010'] = np.where(munip_social_rev['First_Eq_Period_6low_2010'].str[5].astype('int') == 1, munip_social_rev['First_Eq_Period_6low_2010'].str[0:4].astype('int') - 2007, munip_social_rev['First_Eq_Period_6low_2010'].str[0:4].astype('int') - 2006)
munip_social_rev['First_Eq_Period_6low_higher_FY_2010'] = np.where(munip_social_rev['First_Eq_Period_6low_higher_2010'].str[5].astype('int') == 1, munip_social_rev['First_Eq_Period_6low_higher_2010'].str[0:4].astype('int') - 2007, munip_social_rev['First_Eq_Period_6low_higher_2010'].str[0:4].astype('int') - 2006)
munip_social_rev = munip_social_rev[munip_social_rev['Year'] >= 2010]
munip_social_rev = munip_social_rev[(np.logical_not(munip_social_rev.No_of_lower_secondary_school_teacher_percap_1.isna())) & (np.logical_not(munip_social_rev.Population_Imputed_1.isna())) & (np.logical_not(munip_social_rev.Unemployment_Rate_Imputed_2.isna()))]
filt = (munip_social_rev.value_counts('ADM2_PCODE') == 10)[munip_social_rev.value_counts('ADM2_PCODE') == 10].index
munip_social_rev = munip_social_rev[munip_social_rev.ADM2_PCODE.isin(filt)]
filt = munip_social_exp.ADM2_PCODE.unique()
munip_social_rev = munip_social_rev[munip_social_rev.ADM2_PCODE.isin(filt)]

munip_social_rev.to_csv('munip_rev_to_R.csv')

