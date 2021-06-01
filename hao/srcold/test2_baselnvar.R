## set.seed(777)
set.seed(5201314)
library('truncnorm')

source("filepath_def.R")
source("sedat.R")

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
## -> 1. Detrending NOT NEEDED IN SWEDEN??: Swedish death are factual, day of death, not day of
##       reporting.
##       (TOCHECK??: https://ourworldindata.org/covid-sweden-death-reporting)
## -> ???
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
## 5. But how to compute the surprisal in Swedish excess death correctly??
## 
## -> ???
library(parallel)
cl = makePSOCKcluster(8)

## Sweden per age group
se_bayes = function (target_geo, mfexc.0, mfexc.target, use_blvar=T) {
  ## Hard coded to use covid_death_se because it seems to be the easiest...
  excess_nonpos   = list()
  excess_notavail = list()
  M      = mfexc.target[mfexc.target$age!="TOTAL" & mfexc.target$geo.time == target_geo, ]
  M.se   = mfexc.0[mfexc$age!="TOTAL" & mfexc$geo.time == "SE", ]
  agegroup_map = list("LT_50" = c("0_4", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44", "45_49"),
                      "50_59"    = c("50_54", "55_59"),
                      "60_69"    = c("60_64", "65_69"),
                      "70_74"    = c("70_74"),
                      "75_79"    = c("75_79"),
                      "80_84"    = c("80_84"),
                      "85_89"    = c("85_89"),
                      "GE_90" = c("90"))
  wkno_uniq = unique(M$wkno)
  results   = list()
  for (age in covid_death_se$Age_Group) {                      # We only have national per-age-group here.
    for (wk in wkno_uniq) {
      ageidx.se = which(M.se$age %in% agegroup_map[[age]] & M.se$wkno == wk)[1]
      ageidx = which(M$age %in% agegroup_map[[age]] & M$wkno == wk)[1]
      stopifnot(length(ageidx.se) == 1 && length(ageidx) == 1) # Really shouldn't happen, otherwise the data is problematic.
      popu.se.m.20    =M.se$value.popu.m.20[ageidx.se];                       popu.se.f.20    =M.se$value.popu.f.20[ageidx.se];
      popu.target.m.20=M$value.popu.m.20[ageidx];                             popu.target.f.20=M$value.popu.f.20[ageidx];
      death.m.baseln  =M$value.death.m.mean[ageidx];                          death.f.baseln  =M$value.death.f.mean[ageidx];
      death.m.Vbaseln =M$var.death.m[ageidx];                                 death.f.Vbaseln =M$var.death.f[ageidx];
      death.m.20      =M$value.death.m.20[ageidx];                            death.f.20      =M$value.death.f.20[ageidx];
      death.m.excess  =death.m.20 - death.m.baseln;                           death.f.excess  =death.f.20 - death.f.baseln;
      death.m.covid   =covid_death_se$Male[covid_death_se$Age_Group == age];  death.f.covid   =covid_death_se$Female[covid_death_se$Age_Group == age];
      if (is.na(death.m.excess) || is.na(death.f.excess)) {
        excess_notavail[[length(excess_notavail)+1]] = c(wk = wk, age = age)
        cat(sprintf("Excess death not available, skipping (wkno, age) = (%s,%s) \n", wk, age))
      } else if ((!use_blvar)&&(!(death.m.excess > 0 && death.f.excess > 0))) {
        excess_nonpos[[length(excess_nonpos)+1]] = c(wk = wk, age = age)
        cat(sprintf("Non-positive excess, skipping (wkno, age, excess.m, excess.f) = (%s,%s,%f,%f) \n", wk, age, death.m.excess, death.f.excess))
      } else {
        p = if (use_blvar) {
              replicate(50000, {
                ## With a uniform prior [0, population size] put on mu. Then mu ~ truncnorm(0, death_2020, mean, sd)
                ## so -mu + death_2020 = truncnorm(0, death_2020, death_2020 - mean, sd)
                ## This doesn't pass the common-sense check because of nobody died in death_2020 then mu is zero with
                ## probability 1. 
                XA = rtruncnorm(2,
                                a    = c(0,                    0),
                                b    = c(death.m.20,           death.f.20),
                                mean = c(death.m.excess,       death.f.excess),
                                sd   = sqrt(c(death.m.Vbaseln, death.f.Vbaseln)))
                y = odd_ratio_posterior(x=XA[1], m=sum(XA),
                                    y=death.m.covid,  n=death.m.covid+death.f.covid,
                                    alpha0=1/2, beta0=1/2, alpha1=1/2, beta1=1/2, nsamp=1)
                if (any(is.na(y))) browser()
                y
              })
            } else {
              odd_ratio_posterior(x=death.m.excess, m=death.m.excess+death.f.excess,
                                  y=death.m.covid,  n=death.m.covid+death.f.covid,
                                  alpha0=1/2, beta0=1/2, alpha1=1/2, beta1=1/2)
            }
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
  pvals = lapply(obj, pval)
  for (i in 1:length(obj)) {
    cat(obj[[i]]$wk, "\t", obj[[i]]$age, "\t", pvals[[i]], "\n")
  }
}
returnpval = function (obj, ...) UseMethod("pval")
returnpval.histreslist = function (obj, ...) {
  H0 = obj$H0
  sapply(obj, pval)
}


res = se_bayes("SE", mfexc, mfexc)
summary(res)

res.1 = se_bayes("SE2", mfexc, mfexc.1)
summary(res.1)

