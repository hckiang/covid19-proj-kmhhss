## set.seed(777)
set.seed(5201314)

source("filepath_def.R")
source("sedat.R")
source("mixture_ci.R")

suppressPackageStartupMessages({
    library('lattice')
    library('latticeExtra')
})

##
## To save computation, here are precomputed results.
##
## In attr(mfexc.1, 'deathrawfile') and attr(mfexc.1, 'popufile') you will see the source files being used.
## 
## Returns a posterior distribution of the odd ratios, GIVEN n.
odd_ratio_posterior = function (x, m, y, n, alpha0, beta0, alpha1, beta1, nsamp = 50000) {
  (1 - 1/(1-rbeta(nsamp, alpha0 + x, beta0 + m - x))) / (1 - 1/(1 - rbeta(nsamp, alpha1 + y, beta0 + n - y)))
}

## Example: 
## hist(summary(bayestest(1000, 3000, 2000, 3000, 0.5, 0.5, 0.5, 0.5)))
## should return a distribution centred around 0.25, which is consistent with pen-and-paper
## computation.


## 1. Where does the intrinsic variation in variance goes?
## 
## -> In the z-score versus p-score debate, I believe z-score is better because it accounts for
##    the regional variance.
## -> In this framework, the regional variance in the excess is accounted for in a different way:
##    it is accounted for in the posterior distribution of the 'n'.
## -> One might legitimately ask: what does the distribution of 'n' *mean*, as in physics. That
##    is, if you estimate the speed of light using statistics, the speed of light has to be
##    defined, verifiable; in other words, there should be no room for the goalpost to be shifted.
##    -> I argue that our posterior distribution of 'n' is an estimate of the empirical
##       distribution (a random function).
##    -> Obviously, the posterior distribution 'n' approximates the truth, so no goalpost shifting
##       has happened.
##
## 2. Poisson, and variance in 'n'
## 
## -> On one hand, one can think of the past deaths being normal i.i.d. purely justified by CLT.
## -> On the other hand, one can think of each of them being Poisson. The Poisson assumption adds
##    important information, for the fact that each integer observed is an event happened within
##    a week.
##
## 3. Data pre-processing?????
## -> 1. Smoothing NOT NEEDED IN SWEDEN??: Swedish death are factual, day of death, not day of
##       reporting.
##       (TOCHECK??: https://ourworldindata.org/covid-sweden-death-reporting)
##
##
## 4. Plot 1: M/F ratio time series versus the z-scores: compare the TIME-LOCATION, NOT MAGNITUDES
##
## -> If the *p-value of the z-score* were another surprisal gauge then it makes sense to compute
##    both the score. But having only 4 samples, it is hard to estimate the variance.
## -> But a *surge* in our p-values should intuitively corresponds to a *surge* in z-score,
##    otherwise, something is wrong.
## -> Therefore although the *relative magnitude* of the surge are less of an importance, the
##    *time* of the surge coincide, if the same data were used.
##


## Iterate through all age-week combination given a geographical location

iterat_ageweek = function (target_geo, mfexc.0, mfexc.target, fn, ...) {
  ## Hard coded to use covid_death_se because it seems to be the easiest...
  excess_nonpos   = list()
  excess_notavail = list()
  M      = mfexc.target[mfexc.target$age!="TOTAL" & mfexc.target$geo.time == target_geo, ]
  M.se   = mfexc.0[mfexc$age!="TOTAL" & mfexc$geo.time == "SE", ]
  agegroup_map = list("LT_50" = c("0_4", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44", "45_49"),
                      "50_59" = c("50_54", "55_59"),
                      "60_69" = c("60_64", "65_69"),
                      "70_74" = c("70_74"),
                      "75_79" = c("75_79"),
                      "80_84" = c("80_84"),
                      "85_89" = c("85_89"),
                      "GE_90" = c("90"))
  wkno_uniq = as.character(1:37)##unique(M$wkno)
  results   = list()
  for (age in covid_death_se$Age_Group) {                      # We only have national per-age-group here.
    for (wk in wkno_uniq) {
      ageidx.se = which(M.se$age %in% agegroup_map[[age]] & M.se$wkno == wk)[1]
      ageidx = which(M$age %in% agegroup_map[[age]] & M$wkno == wk)[1]
      stopifnot(length(ageidx.se) == 1 && length(ageidx) == 1) # Really shouldn't happen, otherwise the data is problematic.
      popu.se.m.20    =M.se$value.popu.m.20[ageidx.se];                       popu.se.f.20    =M.se$value.popu.f.20[ageidx.se];
      popu.target.m.20=M$value.popu.m.20[ageidx];                             popu.target.f.20=M$value.popu.f.20[ageidx];
      death.m.baseln  =M$value.death.m.mean[ageidx];                          death.f.baseln  =M$value.death.f.mean[ageidx];
      death.m.20      =M$value.death.m.20[ageidx];                            death.f.20      =M$value.death.f.20[ageidx];
      death.m.excess  =death.m.20 - death.m.baseln;                           death.f.excess  =death.f.20 - death.f.baseln;
      death.m.covid   =covid_death_se$Male[covid_death_se$Age_Group == age];  death.f.covid   =covid_death_se$Female[covid_death_se$Age_Group == age];
      if (is.na(death.m.excess) || is.na(death.f.excess)) {
        excess_notavail[[length(excess_notavail)+1]] = c(wk = wk, age = age)
        cat(sprintf("Excess death not available, skipping (wkno, age) = (%s,%s) \n", wk, age))
      } else if (!(death.m.excess > 0 && death.f.excess > 0)) {
        excess_nonpos[[length(excess_nonpos)+1]] = c(wk = wk, age = age)
        cat(sprintf("Non-positive excess, skipping (wkno, age, excess.m, excess.f) = (%s,%s,%f,%f) \n", wk, age, death.m.excess, death.f.excess))
      } else {
        obj = fn(x=death.m.excess, m=death.m.excess+death.f.excess,
                 y=death.m.covid,  n=death.m.covid+death.f.covid,
              ...)
        info = list(age = age, wk = wk, geo = target_geo, )
        class(obj) = c("geoiter_res")
        results[[length(results)+1]] = obj
        
        p = odd_ratio_posterior(x=death.m.excess, m=death.m.excess+death.f.excess,
                                y=death.m.covid,  n=death.m.covid+death.f.covid,
                                alpha0=1/2, beta0=1/2, alpha1=1/2, beta1=1/2)
        ## This object does not contain any informations about the population size of excess.
        obj = list(age = age, wk = wk, geo = target_geo, ratio_distr = p,
                   H0 = (popu.target.m.20 / popu.target.f.20)/(popu.se.m.20 / popu.se.f.20))
        class(obj) = c("bayes_hist_res")
        results[[length(results)+1]] = obj
      }
    }
  }
  class(results) = c("histreslist")
  results
}

## Sweden per age group
se_bayes = function (target_geo, mfexc.0, mfexc.target) {
  ## Hard coded to use covid_death_se because it seems to be the easiest...
  excess_nonpos   = list()
  excess_notavail = list()
  M      = mfexc.target[mfexc.target$age!="TOTAL" & mfexc.target$geo.time == target_geo, ]
  M.se   = mfexc.0[mfexc$age!="TOTAL" & mfexc$geo.time == "SE", ]
  agegroup_map = list("LT_50" = c("0_4", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44", "45_49"),
                      "50_59" = c("50_54", "55_59"),
                      "60_69" = c("60_64", "65_69"),
                      "70_74" = c("70_74"),
                      "75_79" = c("75_79"),
                      "80_84" = c("80_84"),
                      "85_89" = c("85_89"),
                      "GE_90" = c("90"))
  wkno_uniq = as.character(1:37)##unique(M$wkno)
  results   = list()
  for (age in covid_death_se$Age_Group) {                      # We only have national per-age-group here.
    for (wk in wkno_uniq) {
      ageidx.se = which(M.se$age %in% agegroup_map[[age]] & M.se$wkno == wk)[1]
      ageidx = which(M$age %in% agegroup_map[[age]] & M$wkno == wk)[1]
      stopifnot(length(ageidx.se) == 1 && length(ageidx) == 1) # Really shouldn't happen, otherwise the data is problematic.
      popu.se.m.20    =M.se$value.popu.m.20[ageidx.se];                       popu.se.f.20    =M.se$value.popu.f.20[ageidx.se];
      popu.target.m.20=M$value.popu.m.20[ageidx];                             popu.target.f.20=M$value.popu.f.20[ageidx];
      death.m.baseln  =M$value.death.m.mean[ageidx];                          death.f.baseln  =M$value.death.f.mean[ageidx];
      death.m.20      =M$value.death.m.20[ageidx];                            death.f.20      =M$value.death.f.20[ageidx];
      death.m.excess  =death.m.20 - death.m.baseln;                           death.f.excess  =death.f.20 - death.f.baseln;
      death.m.covid   =covid_death_se$Male[covid_death_se$Age_Group == age];  death.f.covid   =covid_death_se$Female[covid_death_se$Age_Group == age];
      if (is.na(death.m.excess) || is.na(death.f.excess)) {
        excess_notavail[[length(excess_notavail)+1]] = c(wk = wk, age = age)
        cat(sprintf("Excess death not available, skipping (wkno, age) = (%s,%s) \n", wk, age))
      } else if (!(death.m.excess > 0 && death.f.excess > 0)) {
        excess_nonpos[[length(excess_nonpos)+1]] = c(wk = wk, age = age)
        cat(sprintf("Non-positive excess, skipping (wkno, age, excess.m, excess.f) = (%s,%s,%f,%f) \n", wk, age, death.m.excess, death.f.excess))
      } else {
        p = odd_ratio_posterior(x=death.m.excess, m=death.m.excess+death.f.excess,
                                y=death.m.covid,  n=death.m.covid+death.f.covid,
                                alpha0=1/2, beta0=1/2, alpha1=1/2, beta1=1/2)
        ## This object does not contain any informations about the population size of excess.
        obj = list(age = age, wk = wk, geo = target_geo, ratio_distr = p,
                   H0 = (popu.target.m.20 / popu.target.f.20)/(popu.se.m.20 / popu.se.f.20))
        class(obj) = c("bayes_hist_res")
        results[[length(results)+1]] = obj
      }
    }
  }
  class(results) = c("histreslist")
  results
}


pval = function (object, ...) UseMethod("pval")
pval.bayes_hist_res = function (obj) {
  H0 = obj$H0
  2 * min(sum(H0 > obj$ratio_distr), sum(H0 < obj$ratio_distr)) / length(obj$ratio_distr)
}
summary.histreslist = function (obj, ...) {
  H0 = obj$H0
  pvals = sapply(obj, pval)
  wk = integer(length(obj));  age = character(length(obj))
  for (i in 1:length(obj)) {
    wk[i] =as.integer(obj[[i]][['wk']])
    age[i]=obj[[i]][['age']]
  }
  data.frame(wk=wk,age=age,pvals=pvals)
}
returnpval = function (obj, ...) UseMethod("returnpval")
returnpval.histreslist = function (obj, ...) {
  pvals = lapply(obj, pval)

  #sapply(obj, pval)
}


res = se_bayes("SE", mfexc, mfexc)
rdf = summary(res)
res.1 = se_bayes("SE3", mfexc, mfexc.1)
rdf.1 = summary(res.1)
mfexc.1[mfexc.1$age=="GE_90" & mfexc.1$geo.time == 'SE3', ]

plt_wkrange = c(15,32)
##for (ag in unique(rdf.1)) {
ag = 'GE_90'
ardf = rdf.1[rdf.1[['age']]==ag,]
tmp_exc = mfexc.1[mfexc.1$geo.time=='SE3' & mfexc.1$age == 90,
                  c('wkno','value.death.m.20','value.death.f.20',
                    'value.death.m.mean','value.death.f.mean')]
tmp_exc[['exc.total']] = tmp_exc[['value.death.m.20']]+tmp_exc[['value.death.f.20']]-
    (tmp_exc[['value.death.m.mean']]+tmp_exc[['value.death.f.mean']])
plt_pval = xyplot(-log(ardf$pval)~ardf$wk, type='l',col='red')
plt_exc  = xyplot(tmp_exc$exc.total~tmp_exc$wkno, type='l',col='black')
doubleYScale(plt_pval, plt_exc, add.ylab2 = T)

xyplot(ardf$pval~ardf$wk, type='l',
       xlim=plt_wkrange,
       panel=function (x, ...) {
         panel.xyplot(y=ardf$pval,x=ardf$wk,type='l',col='red')
         panel.xyplot(y=(1:16/30),x=ardf$wk,type='l',col='black')
       })

##}
