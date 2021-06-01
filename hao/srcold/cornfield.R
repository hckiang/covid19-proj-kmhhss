
set.seed(36390646)

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
  library('ORCI')
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

psi0 = mf.se$value.popu.m.19 / mf.se$value.popu.f.19
x1 = mf.se$exc.m;    n1 = x = mf.se$exc.m + mf.se$exc.f
x2 = 190;            n2 = 280
for (i in seq_along(x1)) {
  ci = Cornfieldexact.CI(x1[i], n1[i], x2, n2, conf=0.95)
  cat("Rejected: ", ci[1] < psi0 && psi0 < ci[2], "\n")
}
