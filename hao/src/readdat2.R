suppressPackageStartupMessages({
  library('stringr')
  library('purrr')
  library('reshape2')
  library('tibble')
})

source('utils.R')        # Various utility functions
source('filepath-def.R')
cat('Preprocessing data...\n')
deathraw = read.csv(deathfile)
popuraw = read.csv(popufile)
# Typo in the CSV file, I guess
deathraw$age[deathraw$age=='NK']   = 'UNK'
deathraw$age[deathraw$age=='OTAL'] = 'TOTAL'
# I want level names in death matches populatuion
for (i in seq_along(deathraw$age))
    deathraw$age[i] = str_replace(deathraw$age[i], '-', '_')

loclvl = function (l) nchar(l) - 2                               # NUTS code to NUTS level
yr     = function (s) str_extract(s, '\\d{4}')                   # Get year from XyyyyWww format
wkno   = function (s) str_remove(str_extract(s, 'W\\d{2}'), 'W') # Get week from XyyyyWww format

mkmf = function (deathraw, popu, lvl) {
  allloc = unique(deathraw$geo.time)     # All available NUTS codes
  loc = allloc[loclvl(allloc) %in% lvl]  # Target NUTS codes
  agegrp = unique(deathraw$age)          # Target age group
  sex    = c('F','M')                            # Target sex. we don't use 'T'
  availmask     = deathraw$age %in% agegrp &
                  deathraw$geo.time %in% loc &
                  deathraw$sex %in% sex
  deathraw      = data.frame(deathraw)           # Copy the data frame before changing it.
  deathraw      = deathraw[availmask,]
  deathraw.mlt    = melt(deathraw, id.vars=c('sex', 'age', 'geo.time'))
  deathraw.mlt$yr   = yr(deathraw.mlt$variable)
  deathraw.mlt$popyr= ifelse(deathraw.mlt$yr == '2020', '2019', deathraw.mlt$yr)
  deathraw.mlt$wkno = wkno(deathraw.mlt$variable)
  popu.mlt = melt(popu, id.vars=c('sex', 'age', 'geo.time'))
  popu.mlt$variable = yr(popu.mlt$variable)
  colnames(popu.mlt)[colnames(popu.mlt) == 'variable'] = 'popyr'

  # Inner-joining the two tables
  tmp.mlt = merge(popu.mlt, deathraw.mlt, by = c('sex','age','geo.time','popyr'),
                  suffixes=c('.popu','.death'))
  mdf = tmp.mlt[tmp.mlt$sex == 'M',];  fdf = tmp.mlt[tmp.mlt$sex == 'F',];
  mdf$sex      = NULL;                  fdf$sex      = NULL;
  mdf$variable = NULL;                  fdf$variable = NULL;
  mdf$popyr    = NULL;                  fdf$popyr    = NULL;
  rm(tmp.mlt); rm(popu.mlt); rm(deathraw.mlt)
  # Inner-join again
  mf.mlt = merge(mdf, fdf,
                 by = c('age','geo.time','yr', 'wkno'),
                 suffixes = c('.m', '.f'))
  # Replace the "90" age group to "90_Inf", for later processing
  mf.mlt[['age']][which(mf.mlt[['age']] == '90')] = '90_Inf'
  mf.mlt
}
mf = mkmf(deathraw, popuraw, 0)
mf.1 = mkmf(deathraw, popuraw, 1)
mf.2 = mkmf(deathraw, popuraw, 2)
mf.3 = mkmf(deathraw, popuraw, 3)
coviddeathraw = lapply(coviddeathfiles, read.csv2)
coviddeathraw[['CH']] = NULL
coviddeathraw[['DE']] = NULL
normalise_härje_data = function (coviddeathraw) {
    lapply(coviddeathraw, function (dframe) {
        dframe = {
                     ## Produce an data frame of unique age group with two columns:
                     ## 'AgeGrp_min' and 'AgeGrp_max'
                     ## Remove the row if both min and max are NA
                     dframe = data.frame(dframe)
                     dframe = dframe[!(is.na(dframe[['AgeGrp_min']]) &
                                       is.na(dframe[['AgeGrp_max']])),]
                     ## In UK there is an 0~1 and 1~4 group. Merge them in this case.
                     dframe.ageonly = dframe[c('AgeGrp_min','AgeGrp_max')]
                     if (rowexists(list('AgeGrp_min'=0,'AgeGrp_max'=1), dframe.ageonly) &&
                         rowexists(list('AgeGrp_min'=1,'AgeGrp_max'=4), dframe.ageonly)){
                         which_zeroone = which(dframe[['AgeGrp_min']] == 0 &
                                               dframe[['AgeGrp_max']] == 1)
                         which_onefour = which(dframe[['AgeGrp_min']] == 1 &
                                               dframe[['AgeGrp_max']] == 4)
                         dframe[which_zeroone,'AgeGrp_max']      = 4
                         dframe[which_onefour,'AgeGrp_min'] = 0   # We'll sum them up later
                     }
                     ## If AgeGrp_max is NA and min is not then replace NA with infinity
                     which_veryold = which((!is.na(dframe[['AgeGrp_min']])) &
                                           is.na(dframe[['AgeGrp_max']]))
                     dframe[['AgeGrp_max']][which_veryold] = Inf
                     ## If AgeGrp_min is NA and max is not then replace NA with 0
                     which_baby = which((!is.na(dframe[['AgeGrp_max']])) &
                                         is.na(dframe[['AgeGrp_min']]))
                     dframe[['AgeGrp_min']][which_baby] = 0
                     dframe
                 }
        aggregate(dframe[c('male_covid_death','female_covid_death','total_covid_death')],
                  by  = dframe[c('WeekNo','AgeGrp_min','AgeGrp_max','Year')],
                  FUN = sum)
    })
}
coviddeath = normalise_härje_data(coviddeathraw)
country_age_group_map = lapply(coviddeath, function (dframe) {
    X = unique_row(dframe[c('AgeGrp_min','AgeGrp_max')])
    rownames(X) = NULL
    X
})
