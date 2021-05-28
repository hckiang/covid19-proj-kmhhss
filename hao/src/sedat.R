## This file creates some data frames of clean Swedish data. Source() this file.
## To see what data frames are produced please just scroll to the end of the file. :)


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

if (! all(c("deathrawfile", "popufile", "covidswefile", "polishfile") %in% ls())) {
  stop("File path not found. See filepath_def.R")
}

source("DiffMenWomen.R")
source("reported_excess_hypothesis.R")

cat("Preprocessing data...\n")
covid_death_se = read.csv2(covidswefile)
polish = read.csv(polishfile)
deathraw = read.csv(deathrawfile)
popu = read.csv(popufile)

levels(deathraw$age)[levels(deathraw$age)=="NK"]   = "UNK"        # Typo in the CSV file, I guess
levels(deathraw$age)[levels(deathraw$age)=="OTAL"] = "TOTAL"
for (i in seq_along(levels(deathraw$age)))                        # I want level names in deathraw matches popu
  levels(deathraw$age)[i] = str_replace(levels(deathraw$age)[i], "-", "_")


loclvl = function (l) nchar(l) - 2                                     # Region code to NUTS level
yr     = function (s) str_extract(s, "\\d{4}")                         # Get year from XyyyyWww format
wkno   = function (s) str_remove(str_extract(s, "W\\d{2}"), "W")       # Get week from XyyyyWww format

## Set all non-positive/nan/inf/-inf M/F ratio to NA
na.ify    = defmacro(D, mfratio, expr={ D[[mfratio]][is.na(D[[mfratio]]) | is.nan(D[[mfratio]]) | (! is.finite(D[[mfratio]])) | D[[mfratio]] <= 0] = NA })

## Make an M/F ratio table out of the input death toll and population count
mkmf = function (deathraw, popu, lvl) {
  allloc = unique(levels(deathraw$geo.time))     # All available NUTS codes
  loc = allloc[loclvl(allloc) %in% lvl]          # Target NUTS codes
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
  # mf.mlt[which(!is.na(mf.mlt$mfratio)),]
  mf.mlt
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
  A = unique(historical$age[historical$yr == "2020"])
  G = unique(historical$geo.time[historical$yr == "2020"])
  W = unique(historical$wkno[historical$yr == "2020"])
  `%oc%` = function (x,y) flatten(lapply(x, function(e) lapply(y, function (f) c(as.character(e),as.character(f)))))
  k = 1
  r = 1
  COMB = A %oc% G %oc% W
  ## historical.mean = as_tibble(historical[0:0,])
  historical.mean = do.call(tibble,
                                      { 
                                        Y = Map(function (col) {
                                          tpl = if (is.factor(col[1])) "" else col[1]
                                          tpl[] = NA
                                          x = rep(tpl, length(COMB))
                                          x
                                        }, historical)
                                        names(Y) = colnames(historical)
                                        Y
                                      })
  historical.mean = add_column(historical.mean,
                               var.popu.m  = as.double(NA), var.popu.f  = as.double(NA),
                               var.death.m = as.double(NA), var.death.f = as.double(NA),
                               baseline.yr = as.character(NA))
  for (x in COMB) {
    if (r %% 1000 == 0) cat(sprintf('excess_wrt_meanpopu(): %s/%s\n', r, length(COMB)))
    ## H = historical[historical$age == x[1] & historical$geo.time == x[2] & historical$wkno == x[3] & historical$yr != "2020",]
    H = which(historical$age == x[1] & historical$geo.time == x[2] & historical$wkno == x[3] & historical$yr != "2020")
    if (length(H) == 0) { r=r+1; next }
    meanpopu_m  = mean(historical$value.popu.m[H])
    meanpopu_f  = mean(historical$value.popu.f[H])
    meandeath_m = mean(historical$value.death.m[H])
    meandeath_f = mean(historical$value.death.f[H])
    varpopu_m   = var(historical$value.popu.m[H])
    varpopu_f   = var(historical$value.popu.f[H])
    vardeath_m  = var(historical$value.death.m[H])
    vardeath_f  = var(historical$value.death.f[H])
    baselineyr  = do.call(paste, c(as.list(as.character(historical$yr[H])), sep=';'))
    historical.mean[['age']][k]                         = x[1]
    historical.mean[['geo.time']][k]                    = x[2]
    historical.mean[['wkno']][k]                        = x[3]
    historical.mean[['value.popu.m']][k]                = meanpopu_m
    historical.mean[['value.popu.f']][k]                = meanpopu_f
    historical.mean[['value.death.m']][k]               = meandeath_m
    historical.mean[['value.death.f']][k]               = meandeath_f
    historical.mean[['var.popu.m']][k]                  = varpopu_m
    historical.mean[['var.popu.f']][k]                  = varpopu_f
    historical.mean[['var.death.m']][k]                 = vardeath_m
    historical.mean[['var.death.f']][k]                 = vardeath_f
    historical.mean[['baseline.yr']][k]                 = baselineyr
    historical.mean[['mfratio']][k]                     = NA
    k=k+1
    r=r+1
  }
  historical20 = data.frame(historical[historical$yr == "2020",])
  historical20$yr = NULL
  mged = merge(historical.mean[1:(k-1),1:ncol(historical)], historical20, by=c("age","geo.time","wkno"), suffixes=c(".mean",".20"))
  mged = add_column(mged, historical.mean[1:(k-1),(ncol(historical)+1):ncol(historical.mean)])
  mged$exc.m = mged$value.death.m.20 - mged$value.death.m.mean
  mged$exc.f = mged$value.death.f.20 - mged$value.death.f.mean
  mged$exc.mfratio = mged$exc.m / mged$exc.f
  mged$exc.mfratio.pc = mged$exc.m * 1e6 /mged$value.popu.m.mean  / (mged$exc.f* 1e6 /mged$value.popu.f.mean)
  na.ify(mged, "exc.mfratio")
  mged
}

## polish_aggregate = function (polish, agespec, nutlvl = 2) {
##   stopifnot(nutlvl %in% c(2,3))
##   nutcol = sprintf("NUTS%d", nutlvl)
##   nutcodes = sort(unique(as.character(polish[[nutcol]])))
##   ## ages = unique(na.omit(polish$age))
##   ages = names(agespec)
##   wknos = unique(strftime(polish$date, "%V"))
##   res = array(0, dim = c(length(wknos), length(nutcodes), length(ages), 2), dimnames = list(wknos,nutcodes,ages,c("M","F")))
##   ismissing = function (x) is.na(x) || nchar(x) == 0 || is.null(x)
##   for (i in 1:nrow(polish)) {
##     wk = strftime(polish$date[i], "%V")
##     nt = as.character(polish[[nutcol]][i])
##     ag = names(agespec)[which(unlist(lapply(agespec, function (A) as.integer(polish$age[i]) %in% A)))[1]]
##     sx = as.character(polish$sex[i])
##     if (! (ismissing(wk) || ismissing(nt) || ismissing(ag) || ismissing(sx))) {
##       res[wk, nt, ag, sx] = res[wk, nt, ag, sx] + 1
##     } else {
##       cat(sprintf("Polish processor: missing data at row number %d. Skipping.\n", i))
##     }
##   }
##   res
## }
## polish_nut2 = polish_aggregate(polish,
##                                list("LT_50" = 0:49,
##                                     "50_59" = 50:59,
##                                     "60_69" = 60:69,
##                                     "70_74" = 70:74,
##                                     "75_79" = 75:79,
##                                     "80_84" = 80:84,
##                                     "85_89" = 85:89,
##                                     "GE_90" = 90:130),
##                                2)

## SE
#mf = mkmf(deathraw, popu, 0)
#mfexc = excess_wrt_meanpopu(mf)

## !! IMPORTANT.
## SE1
#mf.1 = mkmf(deathraw, popu, 1)
#mfexc.1 = excess_wrt_meanpopu(mf.1)
##mfexc.1 = excess_wrt19(mf.1)
## SE12
#mf.2 = mkmf(deathraw, popu, 2)
#mfexc.2 = excess_wrt_meanpopu(mf.2)
## mfexc.2 = excess_wrt19(mf.2)
#mf.3 = mkmf(deathraw, popu, 3)
#mfexc.3 = excess_wrt_meanpopu(mf.3)



mfexc   = readRDS('../dat/mfexc.rds')
mfexc.1 = readRDS('../dat/mfexc.1.rds')

## The following contains Sweden COVID death count with no male/female counts
se_regions_covid_death = readRDS(swedcovidfile)
