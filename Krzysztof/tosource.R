## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this package or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_mweek3
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_pjangrp3&lang=en
# https://stackoverflow.com/questions/33322248/how-to-import-a-tsv-file
# https://ec.europa.eu/eurostat/web/population-demography-migration-projections/data/database
## population counts have some e, b, notes next to values
## some countries do not have all the age levels

library(memoise)
library(lubridate)
#lubridate::ymd( "2014-01-01" ) + lubridate::weeks( 60 - 1 ) # lubridate can do date arithmecitc
#[1] "2015-02-18"
library(COVID19)
## USER should edit the below R file
source("RegionSetups.R") ## In this file we have lists with regions/countries that we want to analyze, 
## =======================================================================================
source("read_Eurostat_tsv.R")
source("Countries_YearMortalityC19.R")

source("zzz_USER_SET_VARIABLES.R")
v_countries_orig<-v_countries ## this value gets overwritten when creating plots, but is needed for tables
source("EU_deaths_20210905.R")
v_countries<-v_countries_orig

if(b_load_data){load(datafile_name)}
#mTable_20202021_SEPL<-f_create_COVID19_table(v_countries=c("Poland","Sweden"),v_country_codes=c("PL","SE"),dfMortality=df_EurostatMortalityCounts,dfPopulations=df_EurostatPopulationCounts,year_start=2020,year_end=2021)
#mTable_20202021_SECHPL<-f_create_COVID19_table(v_countries=c("Poland","Sweden","Switzerland"),v_country_codes=c("PL","SE","CH"),dfMortality=df_EurostatMortalityCounts,dfPopulations=df_EurostatPopulationCounts,year_start=2020,year_end=2021)
lTables_Countries_MortalityCOVID19<-f_create_COVID19_table(v_countries=v_countries,v_country_codes=v_country_codes,dfMortality=df_EurostatMortalityCounts,dfPopulations=df_EurostatPopulationCounts,year_start=year_start,year_end=year_end,day_start_start_year_totC19=day_start_start_year_totC19,day_end_end_year=day_end_end_year,week_end_end_year=week_end_end_year,v_baselineyears=v_baselineyears,sex_for_table=sex_for_table,age_group_for_table=age_group_for_table,l_pop_year=l_pop_year,week_start_partial=week_start_partial,week_end_partial=week_end_partial,start_day_partial=start_day_partial,end_day_partial=end_day_partial)
