## This file contains the old asymptotic histogram tests.
##

## Run all these and it will save plots to PDF's
## set.seed(777)
set.seed(5201314)
options(stringsAsFactors = T)
suppressPackageStartupMessages({
  library("stringr")
  library("purrr")
  library("reshape2")
  library("gtools")
  library("lattice")
  library("tibble")
  library("latticeExtra")
  library("RColorBrewer")
  library("outliers")
})
source("DiffMenWomen.R")
source("reported_excess_hypothesis.R")

cat("Preprocessing data...\n")
deathraw = read.csv("../dat/output.csv")
levels(deathraw$age)[levels(deathraw$age)=="NK"]   = "UNK"        # Typo in the CSV file, I guess
levels(deathraw$age)[levels(deathraw$age)=="OTAL"] = "TOTAL"
for (i in seq_along(levels(deathraw$age)))                        # I want level names in deathraw matches popu
  levels(deathraw$age)[i] = str_replace(levels(deathraw$age)[i], "-", "_")
popu = read.csv("../dat/2020-06-22_population.csv")

loclvl = function (l) nchar(l) - 2                                     # Region code to NUTS level
yr     = function (s) str_extract(s, "\\d{4}")                         # Get year from XyyyyWww format
wkno   = function (s) str_remove(str_extract(s, "W\\d{2}"), "W")       # Get week from XyyyyWww format

## Set all non-positive/nan/inf/-inf M/F ratio to NA
na.ify    = defmacro(D, mfratio, expr={ D[[mfratio]][is.na(D[[mfratio]]) | is.nan(D[[mfratio]]) | (! is.finite(D[[mfratio]])) | D[[mfratio]] <= 0] = NA })


## Make an M/F ratio table out of the input death toll and population count
mkmf = function (deathraw, popu, lvl) {
  allloc = unique(levels(deathraw$geo.time))     # All available NUTS codes
  loc = allloc[loclvl(allloc) %in% lvl]           # Target NUTS codes
  agegrp = unique(levels(deathraw$age))          # Target age group
  sex    = c("F","M")                            # Target sex. we don't use "T"
  availmask     = deathraw$age %in% agegrp &
                  deathraw$geo.time %in% loc &
                  deathraw$sex %in% sex
  
  deathraw      = data.frame(deathraw)           # Copy the data frame before changing it.
  deathraw      = deathraw[availmask,]
  deathraw.mlt    = melt(deathraw, id.vars=c("sex", "age", "geo.time"))
  deathraw.mlt$yr   = yr(deathraw.mlt$variable)
  deathraw.mlt$popyr= ifelse(deathraw.mlt$yr == "2020", "2019", deathraw.mlt$yr)
  deathraw.mlt$wkno = wkno(deathraw.mlt$variable)
  popu.mlt = melt(popu, id.vars=c("sex", "age", "geo.time"))
  popu.mlt$variable = yr(popu.mlt$variable)
  colnames(popu.mlt)[colnames(popu.mlt) == "variable"] = "popyr"

  tmp.mlt = merge(popu.mlt, deathraw.mlt, by = c("sex","age","geo.time","popyr"),
                  suffixes=c(".popu",".death"))                                                  # Inner-joining the two tables
  mdf = tmp.mlt[tmp.mlt$sex == "M",];  fdf = tmp.mlt[tmp.mlt$sex == "F",];
  mdf$sex      = NULL;                  fdf$sex      = NULL;
  mdf$variable = NULL;                  fdf$variable = NULL;
  mdf$popyr    = NULL;                  fdf$popyr    = NULL;
  rm(tmp.mlt); rm(popu.mlt); rm(deathraw.mlt)
  mf.mlt = merge(mdf, fdf, by = c("age","geo.time","yr", "wkno"), suffixes = c(".m", ".f"))     # Inner-join again
  #mf.mlt$deathpc.m = mf.mlt$value.death.m / mf.mlt$value.popu.m                                # Death per capita male
  #mf.mlt$deathpc.f = mf.mlt$value.death.f / mf.mlt$value.popu.f                                # Death per capita female
  #mf.mlt$mfratio   = mf.mlt$deathpc.m / mf.mlt$deathpc.f                                       # Per-capita M/F ratio
  mf.mlt$mfratio   = (mf.mlt$value.death.m/mf.mlt$value.death.f) * (mf.mlt$value.popu.f/mf.mlt$value.popu.m)  # *** NOT USED AT ALL ***
  na.ify(mf.mlt, "mfratio")
  mf.mlt[which(!is.na(mf.mlt$mfratio)),]
}

## TODO: distribute the unknown... But for Sweden only it's fine because there's no unknowns
excess_wrt19 = function (historical) { # 2020's excess death per-capita of wrt to 2019 per week
  historical19 = data.frame(historical[historical$yr == "2019",])
  historical20 = data.frame(historical[historical$yr == "2020",])
  historical19$yr = NULL;     historical20$yr = NULL
  mged = merge(historical19, historical20, by=c("age","geo.time","wkno"), suffixes=c(".19",".20"))
  mged$exc.m = mged$value.death.m.20 - mged$value.death.m.19
  mged$exc.f = mged$value.death.f.20 - mged$value.death.f.19
  mged$exc.mfratio = mged$exc.m / mged$exc.f
  mged$exc.mfratio.pc = mged$exc.m * 1e6 /mged$value.popu.m.19  / (mged$exc.f* 1e6 /mged$value.popu.f.19)
  na.ify(mged, "exc.mfratio")
  mged
}

excess_wrt_meanpopu = function (historical) { # 2020's excess death per-capita of wrt to 2014~2019's mean
  ## For each unique x = some c("age","geo.time","wkno"), compute the mean of the M, F population size
  historical.mean = as_tibble(historical[0:0,])
  A = unique(historical$age[historical$yr == "2020"])
  G = unique(historical$geo.time[historical$yr == "2020"])
  W = unique(historical$wkno[historical$yr == "2020"])
  `%oc%` = function (x,y) flatten(lapply(x, function(e) lapply(y, function (f) c(as.character(e),as.character(f)))))
  for (x in A %oc% G %oc% W) {
    H = historical[historical$age == x[1] & historical$geo.time == x[2] & historical$wkno == x[3] & historical$yr != "2020",]
    if (nrow(H) == 0) next
    if (length(H) != length(unique(H)))
      error(sprintf("In excess_wrt_nonpop: number of years not right in c(%s,%s,%s)",x[1],x[2],x[3]))
    meanpopu_m  = mean(H$value.popu.m)
    meanpopu_f  = mean(H$value.popu.f)
    meandeath_m = mean(H$value.death.m)
    meandeath_f = mean(H$value.death.f)
    historical.mean = add_row(historical.mean, age=x[1], geo.time= x[2], wkno=x[3],
            yr = do.call(function(...) paste(...,sep=','), as.list(H$yr)),
            value.popu.m=meanpopu_m, value.death.m=meandeath_m,
            value.popu.f=meanpopu_f, value.death.f=meandeath_f,
            mfratio = NA)
  }
  historical20 = data.frame(historical[historical$yr == "2020",])
  historical20$yr = NULL
  mged = merge(historical.mean, historical20, by=c("age","geo.time","wkno"), suffixes=c(".mean",".20"))
  mged$exc.m = mged$value.death.m.20 - mged$value.death.m.mean
  mged$exc.f = mged$value.death.f.20 - mged$value.death.f.mean
  mged$exc.mfratio = mged$exc.m / mged$exc.f
  mged$exc.mfratio.pc = mged$exc.m * 1e6 /mged$value.popu.m.mean  / (mged$exc.f* 1e6 /mged$value.popu.f.mean)
  na.ify(mged, "exc.mfratio")
  mged
}

mf = mkmf(deathraw, popu, 0)
mfexc = excess_wrt19(mf)
mfexc_mpopu = excess_wrt_meanpopu(mf)
## =================================================================================================================
## OBS!
## 1. In the below tests, no Bonferroni are used. If we do Bonferroni and see all histograms as a single hypercube
## then every hypothesis we have will be rejected (which is probably legit because it means the excess is neither
## entirely COVID nor "same as 2019").
## 2. In Sweden there's no 'UNK' so we don't need to distribute the unknown.
## =================================================================================================================
##
## -----------------------------------------
##
## << I >>   Test for Sweden, by age group.
## 
## -----------------------------------------
mf.se      = mfexc[mfexc$age!="TOTAL" & mfexc$geo.time == "SE" & !is.na(mfexc$exc.mfratio) & mfexc$exc.m > 0 & mfexc$exc.f > 0, ]
mf.se.aa   = mfexc[mfexc$age=="TOTAL" & mfexc$geo.time == "SE" & !is.na(mfexc$exc.mfratio) & mfexc$exc.m > 0 & mfexc$exc.f > 0, ]
mf.se.mpop = mfexc_mpopu[mfexc_mpopu$age!="TOTAL" &
                         mfexc_mpopu$geo.time == "SE" &
                         !is.na(mfexc_mpopu$exc.mfratio) &
                         mfexc_mpopu$exc.m > 0 & mfexc_mpopu$exc.f > 0, ]
mf.se.mpop.aa = mfexc_mpopu[mfexc_mpopu$age=="TOTAL" &
                            mfexc_mpopu$geo.time == "SE" &
                            !is.na(mfexc_mpopu$exc.mfratio) &
                            mfexc_mpopu$exc.m > 0 & mfexc_mpopu$exc.f > 0, ]


cat("Start testing... Test 5-1 ~ 8-2 are the important ones and other tests are various experiments. See comment in code \n")
## If excess deaths had 2019 sex ratio then is the observed excess death *too high*?
## That is, binom(n= EXCESS_TOTAL, p= 2019_MF_RATIO) compared to EXCESS_MF_RATIO
##
## OBS: NO NEED TO CORRECT FOR POPULATION SIZE IN THIS CASE.
##
## Discussion: Most tests are not rejected. So it's entirely possible that those are just natural death.
cat("TEST 1-1 results:\n")
tst11 = histtst(exc             = setNames(mf.se$exc.m + mf.se$exc.f, paste0('A',mf.se$age,'W',mf.se$wkno)),
                mfratio         = mf.se$value.death.m.19 / mf.se$value.death.f.19,
                ratio2compare   = mf.se$exc.mfratio,
                xlim            = c(0.01, 10),
                       side     = "right",
                       histplot = "SE_byage_n_exc_p_2019_compare_exc.pdf",
                       alpha    = 0.05)
print(tst11)

## If all excess death had Covid sex ratio then is the observed excess death *too low*?
## That is, binom(n=EXCESS_TOTAL, p=COVID_MF_RATIO) compared to EXCESS_MF_RATIO
##
## NEED TO CORRECT FOR POPULATION SIZE IN THIS CASE.
##
## Discussion: Mostly not too low. That is, it's possible that all excess are from COVID death.
cat("TEST 1-2 results:\n")
tst12 = histtst(exc                     = setNames(mf.se$exc.m + mf.se$exc.f, paste0('A',mf.se$age,'W',mf.se$wkno)),
                mfratio                 = SE_covidDead_MFratio *
                    (popu[popu$geo.time =="SE" & popu$age=="TOTAL" & popu$sex=="F",]$X2019 /
                     popu[popu$geo.time =="SE" & popu$age=="TOTAL" & popu$sex=="M",]$X2019) *
                    (mf.se$value.popu.m.19 / mf.se$value.popu.f.19),
                ratio2compare           = mf.se$exc.mfratio,
                xlim                    = c(0.01, 10),
                side                    = "left",
                histplot                = "SE_byage_n_exc_p_covid_compare_exc.pdf",
                alpha                   = 0.05)
print(tst12)

## ------------ Summarising << I >>
## Combining discussion 1 and 2, we see that from this the data+test alone we can't say
## whether excess can all be attributed to COVID or not. Even if we do strictly Bonferroni,
## treat the test as a "whether-it's-inside-the-hypercube" test, then it would reject both,
## that is, the excess death are neither completely COVID nor completely natural.
## ------------ 

## -------------------------------------------
##
## << II >>   Test for Sweden, pooled all age
## 
## -------------------------------------------
## Now we are going to do the exact same thing as in << I >>.
## Discussion: Basically almost all tests aren't rejected. So it's entirely possible that those are just natural death.
cat("TEST 2-1 results:\n")
tst21 = histtst(exc           = setNames(mf.se.aa$exc.m + mf.se.aa$exc.f, paste0('A',mf.se.aa$age,'W',mf.se.aa$wkno)),
                mfratio       = mf.se.aa$value.death.m.19 / mf.se.aa$value.death.f.19,
                ratio2compare = mf.se.aa$exc.mfratio,
                xlim          = c(0.01, 3),
                side          = "right",
                pdfsize       = 10,
                histplot      = "SE_noage_n_exc_p_2019_compare_exc.pdf",
                alpha         = 0.05)
print(tst21)

## Discussion: Mostly not too extreme. That is, it's possible that all excess are from COVID death.
cat("TEST 2-2 results:\n")
tst22 = histtst(exc             = setNames(mf.se.aa$exc.m + mf.se.aa$exc.f, paste0('A',mf.se.aa$age,'W',mf.se.aa$wkno)),
                mfratio         = SE_covidDead_MFratio,
                ratio2compare   = mf.se.aa$exc.mfratio,
                xlim            = c(0.01, 3),
                side            = "left",
                pdfsize         = 10,
                histplot        = "SE_noage_n_exc_p_covid_compare_exc.pdf",
                alpha           = 0.05)
print(tst22)

## [1] https://www.frontiersin.org/articles/10.3389/fpubh.2020.00152/full
## [2] https://globalhealth5050.org/covid19/sex-disaggregated-data-tracker/


## -------------------------
##
## Per capita V.1: normalise with population size in n=... instead of after rbinom() sampling
##
## -------------------------

## I am sort of skeptical about this: mf.se$exc.m*1e6/mf.se$value.popu.m.19 + mf.se$exc.f*1e6/mf.se$value.popu.f.19.
## What does this mean in a "philosophical" level????
##
## Update 01 July: Krzysztof thinks the normalisation should be done directly after the rbinom(), instead of
##                 like this, which makes lots of sense. But I think despite this formula looks a bit "funny"
##                 we can actually say that this is formula can be interpreted by "had we resample the population
##                 to make it sex-equal before anybody dies, the excess would have look like...". Though, still,
##                 after reducing to common denominator it still looks funny.
cat("TEST 3-1 results:\n")
tst31 = histtst(exc           = setNames(round(mf.se$exc.m*1e6/mf.se$value.popu.m.19 + mf.se$exc.f*1e6/mf.se$value.popu.f.19),
                               paste0('A',mf.se$age,'W',mf.se$wkno)),
                mfratio       = mf.se$value.death.m.19*1e6/mf.se$value.popu.m.19 / (mf.se$value.death.f.19*1e6/mf.se$value.popu.f.19),
                ratio2compare = mf.se$exc.mfratio.pc,
                xlim          = c(0.01, 10),
                side          = "right",
                histplot      = "SEpc_byage_n_exc_p_2019_compare_exc.pdf",
                alpha         = 0.05)
print(tst31)

## !!!!!!!!!!!! OBS !!!!!!!!!!!!!!!
##
## Bonferroni is rejected in this test! Tthough, as I said, this test is sort of wrong or at least weird
##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cat("TEST 3-2 results:\n")
tst32 = histtst(setNames(mf.se$exc.m*1e6/mf.se$value.popu.m.19 + mf.se$exc.f*1e6/mf.se$value.popu.f.19,
                         paste0('A',mf.se$age,'W',mf.se$wkno)),
                SE_covidDead_MFratio *
                  (popu[popu$geo.time=="SE" & popu$age=="TOTAL" & popu$sex=="F",]$X2019 /
                    popu[popu$geo.time=="SE" & popu$age=="TOTAL" & popu$sex=="M",]$X2019),
                mf.se$exc.mfratio.pc, xlim = c(0.01, 10),
                side = "left",
                histplot = "SEpc_byage_n_exc_p_covid_compare_exc.pdf",
                alpha = 0.05)
print(tst32)


cat("TEST 4-1 results:\n")
tst41 = histtst(exc           = setNames(mf.se.aa$exc.m*1e6/mf.se.aa$value.popu.m.19 + mf.se.aa$exc.f*1e6/mf.se.aa$value.popu.f.19,
                               paste0('A',mf.se.aa$age,'W',mf.se.aa$wkno)),
                mfratio       = mf.se.aa$value.death.m.19 / mf.se.aa$value.death.f.19,
                ratio2compare = mf.se.aa$exc.mfratio.pc,
                xlim          = c(0.01, 3),
                side          = "right",
                pdfsize       = 10,
                histplot      = "SEpc_noage_n_exc_p_2019_compare_exc.pdf",
                alpha         = 0.05)
print(tst41)

cat("TEST 4-2 results:\n")
tst42 = histtst(exc      = setNames(mf.se.aa$exc.m*1e6/mf.se.aa$value.popu.m.19 + mf.se.aa$exc.f*1e6/mf.se.aa$value.popu.f.19,
                                    paste0('A',mf.se.aa$age,'W',mf.se.aa$wkno)),
                mfratio  = SE_covidDead_MFratio *
                    (popu[popu$geo.time=="SE" & popu$age=="TOTAL" & popu$sex=="F",]$X2019 /
                     popu[popu$geo.time=="SE" & popu$age=="TOTAL" & popu$sex=="M",]$X2019),
                ratio2compare = mf.se.aa$exc.mfratio.pc,
                xlim     = c(0.01, 3),
                side     = "left",
                pdfsize  = 10,
                histplot = "SEpc_noage_n_exc_p_covid_compare_exc.pdf",
                alpha    = 0.05)
print(tst42)


## -------------------------
##
## Per capita V.2: normalise with population size after rbinom() sampling
##
## -------------------------

cat("TEST 5-1 results:\n")
tst51 = histtst(exc           = setNames(mf.se$exc.m + mf.se$exc.f,
                                         paste0('A',mf.se$age,'W',mf.se$wkno)),
                mfratio       = mf.se$value.death.m.19 / mf.se$value.death.f.19,
                ratio2compare = mf.se$exc.mfratio.pc, xlim = c(0.01, 10),
                popmfratio    = mf.se$value.popu.m.19 / mf.se$value.popu.f.19,
                side          = "right",
                histplot      = "SEpc2_byage_n_exc_p_2019_compare_exc.pdf",
                alpha         = 0.05)
print(tst51) # Note: the printed isn't Bonferroni and Bonferroni still fails if we do it.

cat("TEST 5-2 results:\n")
tst52 = histtst(exc                    = setNames(mf.se$exc.m + mf.se$exc.f, paste0('A',mf.se$age,'W',mf.se$wkno)),
                mfratio                = SE_covidDead_MFratio,
                popmfratio             = mf.se$value.popu.m.19 / mf.se$value.popu.f.19,
                ratio2compare          = mf.se$exc.mfratio.pc, xlim = c(0.01, 10),
                side                   = "left",
                histplot               = "SEpc2_byage_n_exc_p_covid_compare_exc.pdf",
                alpha                  = 0.05)
print(tst52)


cat("TEST 6-1 results:\n")
tst61= histtst(exc            = setNames(mf.se.aa$exc.m + mf.se.aa$exc.f, paste0('A',mf.se.aa$age,'W',mf.se.aa$wkno)),
                mfratio       = mf.se.aa$value.death.m.19 / mf.se.aa$value.death.f.19,
                popmfratio    = mf.se.aa$value.popu.m.19 / mf.se.aa$value.popu.f.19,
                ratio2compare = mf.se.aa$exc.mfratio.pc,
                xlim          = c(0.01, 3),
                side          = "right",
                pdfsize       = 10,
                histplot      = "SEpc2_noage_n_exc_p_2019_compare_exc.pdf",
                alpha         = 0.05)
print(tst61)

cat("TEST 6-2 results:\n")
tst62 = histtst(exc           = setNames(mf.se.aa$exc.m + mf.se.aa$exc.f, paste0('A',mf.se.aa$age,'W',mf.se.aa$wkno)),
                mfratio       = SE_covidDead_MFratio,
                popmfratio    = mf.se.aa$value.popu.m.19 / mf.se.aa$value.popu.f.19,
                ratio2compare = mf.se.aa$exc.mfratio.pc,
                xlim          = c(0.01, 3),
                side          = "left",
                pdfsize       = 10,
                histplot      = "SEpc2_noage_n_exc_p_covid_compare_exc.pdf",
                alpha         = 0.05)
print(tst62)

## -------------------------------------------------------------------
##
## Per capita v2 med mean population of the past instead of only 2019.
##
## -------------------------------------------------------------------


## Doesn't work? (Missing value error)
cat("TEST 7-1 results:\n")
tst71 = histtst(exc           = setNames(round((mf.se.mpop$exc.m + mf.se.mpop$exc.f) * 1e3),
                                         paste0('A',mf.se.mpop$age,'W',mf.se.mpop$wkno)),
                mfratio       = mf.se.mpop$value.death.m.mean / mf.se.mpop$value.death.f.mean,
                ratio2compare = mf.se.mpop$exc.mfratio.pc, xlim = c(0.01, 10),
                popmfratio    = mf.se.mpop$value.popu.m.mean / mf.se.mpop$value.popu.f.mean,
                side          = "right",
                histplot      = "SEpc2mpop_byage_n_exc_p_mpop_compare_exc.pdf",
                alpha         = 0.05)
print(tst71)

cat("TEST 7-2 results:\n")
tst72 = histtst(exc                    = setNames(round((mf.se.mpop$exc.m + mf.se.mpop$exc.f) * 1e3),
                                                  paste0('A',mf.se.mpop$age,'W',mf.se.mpop$wkno)),
                mfratio                = SE_covidDead_MFratio,
                ratio2compare          = mf.se.mpop$exc.mfratio.pc,
                popmfratio             = mf.se.mpop$value.popu.m.mean / mf.se.mpop$value.popu.f.mean,
                side                   = "left",
                histplot               = "SEpc2mpop_byage_n_exc_p_covid_compare_exc.pdf",
                xlim                   = c(0.01, 10),
                alpha                  = 0.05)
print(tst72)

cat("TEST 8-1 results:\n")
tst81= histtst(exc            = setNames(mf.se.mpop.aa$exc.m + mf.se.mpop.aa$exc.f, paste0('A',mf.se.mpop.aa$age,'W',mf.se.mpop.aa$wkno)),
                mfratio       = mf.se.mpop.aa$value.death.m.mean / mf.se.mpop.aa$value.death.f.mean,
                popmfratio    = mf.se.mpop.aa$value.popu.m.mean / mf.se.mpop.aa$value.popu.f.mean,
                ratio2compare = mf.se.mpop.aa$exc.mfratio.pc,
                xlim          = c(0.01, 3),
                side          = "right",
                pdfsize       = 10,
                histplot      = "SEpc2mpop_noage_n_exc_p_2019_compare_exc.pdf",
                alpha         = 0.05)
print(tst81)

cat("TEST 8-2 results:\n")
tst82 = histtst(exc           = setNames(mf.se.mpop.aa$exc.m + mf.se.mpop.aa$exc.f, paste0('A',mf.se.mpop.aa$age,'W',mf.se.mpop.aa$wkno)),
                mfratio       = SE_covidDead_MFratio,
                popmfratio    = mf.se.mpop.aa$value.popu.m.mean / mf.se.mpop.aa$value.popu.f.mean,
                ratio2compare = mf.se.mpop.aa$exc.mfratio.pc,
                xlim          = c(0.01, 3),
                side          = "left",
                pdfsize       = 10,
                histplot      = "SEpc2mpop_noage_n_exc_p_covid_compare_exc.pdf",
                alpha         = 0.05)
print(tst82)



## ## ------------------
## ##
## ## PLOTS of region population gender, 2019 death ratio, COVID death ratio vs national population gender
## ##
## ## ------------------

## ## National '19 death-per-capita ratio vs week
## plot(x = mf.se.aa$wkno,
##      y = mf.se.aa$value.death.m.19*1e6/mf.se.aa$value.popu.m.19 / (mf.se.aa$value.death.f.19*1e6/mf.se.aa$value.popu.f.19),
##      ylim = c(0,3),
##      xlab = "Week number",
##      ylab = "2019 per-capita M/F death ratio",
##      main = "Per-capita M/F death ratio of 2019 by week and age group",
##      type = 'l')
## cl = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
## j = 1
## gp = c()
## for (age in levels(mf.se$age)) {
##   if (!(age %in% c('TOTAL','UNK'))) {
##     tmp = mf.se[mf.se$age  == age,]
##     lines(x = tmp$wkno,
##          y = tmp$value.death.m.19*1e6/tmp$value.popu.m.19 / (tmp$value.death.f.19*1e6/tmp$value.popu.f.19),
##          col = cl[j],
##          type = 'l')
##     gp = c(gp,age)
##     j = j+1
##   }
## }
## legend('topleft', legend=gp, col=cl[1:(length(levels(mf.se$age))-2)], lty = 1, cex = 0.7)

## ## National population M/F ratio vs age
## poprat = list()
## for (age in levels(mf.se$age)) {
##   if (!(age %in% c('TOTAL','UNK'))) {
##     poprat[[age]] = mf.se$value.popu.m.19[mf.se$age == age][1] / mf.se$value.popu.f.19[mf.se$age == age][1]
##   }
## }
## poprat   = set_names( as.vector(tmp <- na.omit(unlist(poprat))), names(tmp))
## barplot(poprat, horiz=T, las=1, col="black", main = "Population M/F ratio vs age group - national level Sweden")


## Now analyse the regions
mflv3 = mkmf(deathraw, popu, c(0,1,2,3))
mfexclv3 = excess_wrt19(mflv3)

agestoplot = levels(mfexclv3$age)[!(levels(mfexclv3$age) %in% "UNK")]
agestoplot = agestoplot[sapply(agestoplot, function (a) { sum(mfexclv3$age == a) > 3})]


pdf("region_outliers.pdf", width = 11, height = 11)
plots = list()

## Actually this is a for loop instead of lapply but I can't use for loop, because R
## (like Go) captures variable pointers instead of the actual value into the closure (`myp`),
##
## Read this if you've never seen this behaviour: https://win-vector.com/2017/02/26/iteration-and-closures-in-r/
lapply( agestoplot, function (age) {
  tmp   = mfexclv3[mfexclv3$age == age,]
  outly = apply(scores(as.data.frame(cbind(tmp$exc.f*1e5/tmp$value.popu.f.19,
                                           tmp$exc.m*1e5/tmp$value.popu.m.19)), type='t', prob=0.999995),
                1, function(x) all(x))
  myp = function(x,y,...) {
    panel.xyplot(x, y, ...)
    panel.text(x[outly], y[outly], labels=tmp$geo.time[outly], col = "red", cex = 0.8)
  }
  plots[[age]] <<- xyplot((tmp$exc.f*1e5/tmp$value.popu.f.19) ~ (tmp$exc.m*1e5/tmp$value.popu.m.19),
         panel = myp,
         xlab = "Male",
         ylab = "Female",
         main = sprintf("Excess death per-hundred-thousand-capita: M vs F by age. Each point is a region", age),
         col = ifelse(outly, "white", "black"))
  NULL
})
print(do.call(c, plots))
dev.off()




##
## This function generates a table of number of NUTS regions
## present in our database for each country, in other words,
## the stuff in dat/countries_summary.ods
##
howmanyregion = function (deathraw, popu) {
  mf0 = mkmf(deathraw, popu, 0)
  mf1 = mkmf(deathraw, popu, 1)
  mf2 = mkmf(deathraw, popu, 2)
  mf3 = mkmf(deathraw, popu, 3)
  cnty = unique(as.character(mf0$geo.time))
  cat(paste0("Country", ";", "NUTS-1", ";", "NUTS-2", ";", "NUTS-3","\n"))
  for (k in seq_along(cnty)) {
    s1 = length(unique(mf1$geo.time[substring(mf1$geo.time,0,2) == cnty[k]]))
    s2 = length(unique(mf2$geo.time[substring(mf2$geo.time,0,2) == cnty[k]]))
    s3 = length(unique(mf3$geo.time[substring(mf3$geo.time,0,2) == cnty[k]]))
    # print(mf3[substring(mf3$geo.time,0,2) == cnty[k],]) # print island
    cat(paste0(countrycode(cnty[k],"eurostat","country.name"), ";", s1, ";", s2, ";", s3, "\n"))
  }
}
## howmanyregion(deathraw, popu)
