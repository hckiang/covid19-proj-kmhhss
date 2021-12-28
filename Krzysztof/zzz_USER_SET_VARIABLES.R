## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this package or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .


# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_pjangrp3&lang=en
# https://stackoverflow.com/questions/33322248/how-to-import-a-tsv-file
# https://ec.europa.eu/eurostat/web/population-demography-migration-projections/data/database
## population counts have some e, b, notes next to values
## some countries do not have all the age levels


## ===================================================================
## USER control variables 
## ===================================================================

## ===================================================================
## USER start of variables for plots
## ===================================================================


runnum<-"02" ## a value to be added to the name of every resulting file
cmp_prefix<-"" ## prefix of files created, the final underscore _ has to be in cmp_prefix, i.e. if cmp_prefix!="", then it has to end with _
bprop_to<-TRUE ## TRUE: present the ratio of current mortality to past mortality, FALSE: present past and present mortality scaled per capita

b_week_no_xaxes_lab<-FALSE ## in the graphs should the x axes be labelled by week number (TRUE) or by date (FALSE)

## ===================================================================
b_read_tsv<-TRUE ## should the data be read in from the original Eurostat tsv files (TRUE) or the date is already in an RData file provided in RegionSetups.R
## Eurostat file names downloaded from
## https://ec.europa.eu/eurostat/web/population-demography-migration-projections/data/database
c_deathsfilename<-"Eurostat_Deaths_20210905.tsv" ## the name of the file where data from Eurostat demo_r_mweek3 dataset were saved to
## the data can be viewed at
## https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_mweek3
## but the tsv has to be found on and downloaded from 
## https://ec.europa.eu/eurostat/web/population-demography-migration-projections/data/database
c_popsfilename<-"Eurostat_Populations_20210905.tsv" ## the name of the file where data from Eurostat demo_r_pjangrp3 dataset were saved to
## the data can be viewed at
## https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_pjangrp3&lang=en
## but the tsv has to be found on and downloaded from 
## https://ec.europa.eu/eurostat/web/population-demography-migration-projections/data/database
b_prepforfurther_country<-FALSE ##should regional data be removed from read in tsv files (TRUE) or not (FALSE), removing will save space and speedup further analyses, but require a reread if data is to considered regionally
## ===================================================================

l_list_to_use<-l_PLCHSE
b_do_regions<-TRUE ## should the presentation be done at the regional level (TRUE) or country level (FALSE)
b_keep_TOTAL<-TRUE ## should plots for the TOTAL value also be made (TRUE) or only Male, Female and separate age groups (FALSE)
ag_lev_name_first<-TRUE ## should file names have age before sex (TRUE) or not (FALSE) in filename, i.e. how will they be sorted alphabetically, by age group or by sex
v_years<-2015:2021 ## years for which to take the data
v_target_years<-c("Y2020","Y2021") ## years for which to consider the access mortality, the other years in v_years are taken as baseline years
v_weeks_plot<-NULL ## the week numbers for which the plotting is to be done, first week of v_target_years[1] is week 1, last week of v_target_years[length(v_target_years)] is the final week
#v_weeks_plot<-70:85 ## example of subsetting weeks
b_one_by_one<-FALSE ## should all countries/regions be plotted on one plot (FALSE) or one by one compared with baseline countries/regions (TRUE)
b_jointargetyears<-TRUE ## should the x-axis be (a substed of, if v_weeks_plot is not all) of 1:52 (i.e. target years stacked on each other), (FALSE) or be (a subset of) 1:(length(v_target_years)*53), (i.e. the target period is taken as whole), (TRUE)

num_weeks_to_plot<-90 ## number of weeks to plot if we are joining a couple of years together

## ===================================================================
## USER end of variables for plots
## ===================================================================

## ===================================================================
## USER start of variables for tables
## the table is created in the object lTables_Countries_MortalityCOVID19
## WARNING be careful not to overwrite in multiple runs
## ===================================================================


#datafile_name<-"EurostatMortalityPopulations_20210906_CountriesToSE.RData"
##datafile_name<-"EurostatMortalityPopulations_20210906_WithCH.RData" ## the RData file to load from (only relevant if b_load_data is TRUE)

year_start<-2020 ## start year of tables
year_end<-2021 ## end year of tables
week_end_end_year<-"W32" ## last week for which data was recorded for end year
v_baselineyears<-2015:2019 ## baseline mortality years
l_pop_year<-list(PL=c(y2021=37840001),SE=c(y2021=10379295),CH=c(y2021=8667088)) ##population size for years not in Eurostat demographic tables that are used (i.e. current year is not provided), list names by country/region NUTS code, for each country region list named as yXXXX where XXXX is the year number, NOTICE the starting "y" and then the population size
## one source for current year population size can be e.g. from https://ec.europa.eu/eurostat/databrowser/view/tps00001/default/table?lang=en
week_start_partial<-"W01" ## starting week if we want to create a table for a particular part of the year
week_end_partial<-"W32" ## end week if we want to create a table for a particular part of the year this week number can be greater than 52 if we want to take a time region that exceeds a calender year, e.g. whole winter
## the code will then create tables for the above part of the year for each years, 
## code has not been tested when the period goes over the calender year

## this variable has to be only set if week_end_partial>53 (or 52 depending on year), if the user is interested
## in a time period that goes over the calender year, then the end day of the first period has to be manually set
## essentially as as.Date(paste(year_start+1,as.numeric(strsplit(week_end_partial,"W")[[1]][2]),1,sep=""),"%Y%U%u")
## setting this incorrectly will make the COVID19 data inconsistent with the mortality data
## at the moment there is no possibility to create the table for aggregated years
## however, this can be done manually post-hoc, in the tables one has all the counts for the given years
## (not only per 100k scaled) so one can create data for given years also for partial parts of years as needed
## manually add and then scale by population sizes, one needs to decide how to average the population size
## if years are aggregated 
end_day_partial=as.Date(paste(year_start,as.numeric(strsplit(week_end_partial,"W")[[1]][2]),1,sep=""),"%Y%U%u")

## Variables that are problematic to adjust as COVID19 data will not be comparable,
## only all ages and both sexes COVID19 data is available through the package
## however adjusting these variables will give a tabular view on the mortality
## WARNING: Excess-CF mortality will be RUBBISH
## also population sizes for years missing in Eurostat should be corrected for given group
## only tested for whole population
sex_for_table<-"T" ## for what sex the table should be created for "T" all, "M" males, "F" females (only "T" has been tested)
age_group_for_table="0_Inf" ## for what age group the table should be created for "0_Inf" all, no other age group tested, 

## variables that should not really be set by the user
## they might be useful for experimental purposes, usually only if the COVID19 subtable is required
## changing them will make the mortality data  inconsistent with the COVID19 data (most probably)
day_end_end_year<-as.Date(paste(year_end,as.numeric(strsplit(week_end_end_year,"W")[[1]][2]),1,sep=""),"%Y%U%u") ## last day for which data was recorded for end year
day_start_start_year_totC19<-"2020-01-22" ## first day of first year, essentialy 2020, for which COVID19 data was recorded, should be kept at 2020-01-22 unless there is some special reason
start_day_partial<-as.Date(paste(year_start,(strsplit(week_start_partial,"W")[[1]][2]),1,sep=""),"%Y%U%u") ## the starting day of the partial year period
if (week_start_partial=="W01"){start_day_partial<-paste0(year_start,"-01-01")}
## WARNING IF YOU WANT BEGINNING OF YEAR YOU HAVE TO SET MANUALLY, AS W01 might correspond to another day in the year e.g. 2020W01 according to R corresponds to Epiphany
## ============================================================================

## ===================================================================
## USER end of variables for tables
## ===================================================================

## ===================================================================
## USER end of control variables 
## ===================================================================

## ===================================================================
## Values extracted from the list in the file RegionSetups.R
## DO NOT edit, this code is kept here to explain the meaning of the
## various fields
## countries/regions should be given in NUTS code that are used in Eurostat
## see examples in RegionSetups.R
## unfortunately the usage here of the terms country/regions is not consistent
## in both cases this refers to NUTS codes, it is not that relevant at which level
## ===================================================================
c_RData_file<-l_list_to_use$c_RData_file ## the name of the RData file into which the data should be saved

vymax_prop<-l_list_to_use$vymax_prop ##the maximum height of the y-axes for the different age groups when the mortality is presented proportional to previous years
vymax_pc<-l_list_to_use$vymax_pc ##the maximum height of the y-axes for the different age groups when the mortality is presented per capita

v_countries<-l_list_to_use$v_countries ## the data corresponding the the NUTS codes to be extracted, uesful if we want to do some different analysis, the complete dataset is too large to hold in memory and extract from it
v_country_codes<-l_list_to_use$v_country_codes ## NUTS regions
v_regions_keep<-l_list_to_use$v_regions_keep ## the data corresponding the the NUTS codes the be further analyzed, has to be a subset of v_countries
v_countries_rem<-l_list_to_use$v_countries_rem ## countries to be removed, usually DE is problematic with age groups
c_baseline_country<-l_list_to_use$c_baseline_country ## the baseline countries, can be also be a region
v_colours<-l_list_to_use$v_colours ## colours for each country
names(v_colours)<-l_list_to_use$v_names_colours ## NUTS codes for each country
v_legend_names<-l_list_to_use$v_legend_names ## human friendly labels for each country on the legend
b_load_data<-TRUE ## should the data be loaded from an RData file or they are sitting in memory
datafile_name<-l_list_to_use$c_RData_file ## the RData file to load from (for graphs only relevant if b_load_data is TRUE) BUT has to be set for table
## ===================================================================
## end of extracted variables from RegionSetups.R
## ===================================================================

