iter_ageweek = function (target_geo, get_m, get_r,
                         get_history_death, get_covid_death, get_excess_death,
                         fn,
                         age_groups, ...) {
  excess_nonpos   = list() # These 3 var. contains all error ARW.
  excess_notavail = list()
  covid_notavail  = list()
  wkno_uniq = as.character(1:52)
  results   = list()
  for (age in age_groups) {
    for (wk in wkno_uniq) {
      covidgrp_popsiz.m    =get_r(target_geo, age, wk, 'M')
      covidgrp_popsiz.f    =get_r(target_geo, age, wk, 'F')
      excessgrp_popsiz.m   =get_m(target_geo, age, wk, 'M')
      excessgrp_popsiz.f   =get_m(target_geo, age, wk, 'F')
      ## The following are vectors of numbers, each element represent the
      ## death count of a year. Year is encoded as names of the vectors
      death.m.history      =get_history_death(target_geo, age, wk, 'M')
      death.f.history      =get_history_death(target_geo, age, wk, 'F')
      death.m.excess       =get_excess_death(target_geo, age, wk, 'M')
      death.f.excess       =get_excess_death(target_geo, age, wk, 'F')
      death.m.covid        =get_covid_death(target_geo, age, wk, 'M')
      death.f.covid        =get_covid_death(target_geo, age, wk, 'F')
            if (is.na(death.m.excess) || is.na(death.f.excess)) {
              excess_notavail[[length(excess_notavail)+1]] = c(wk = wk, age = age)
              cat(sprintf('Excess death not available, skip (wkno, age) = (%s,%s) \n', wk, age))
              next
            } else if (!(death.m.excess > 0 && death.f.excess > 0)) {
              excess_nonpos[[length(excess_nonpos)+1]] = c(wk = wk, age = age)
              cat(sprintf('Non-positive excess, skip (wkno,age,excess.m,excess.f) = (%s,%s,%f,%f)\n',
                          wk, age, death.m.excess, death.f.excess))
              next
            }
            if (is.na(death.m.covid) || is.na(death.f.covid)) {
              covid_notavail[[length(covid_notavail)+1]] = c(wk = wk, age = age)
              cat(sprintf('COVID-19 death not available, skipping (wkno, age) = (%s,%s) \n', wk, age))
              next
            }
      obj = fn(death.m.history,   death.m.history + death.f.history,
               death.m.covid,     death.m.covid + death.f.covid,
               death.excess,      death.excess,
               covidgrp_popsiz.m, covidgrp_popsiz.f,
               excessgrp_popsiz.m, excessgrp_popsiz.f,
               ...)
      info = list(age = age, geo = target_geo, wk = wk, result=obj)
      results[[length(results)+1]] = info
    }
  }
  class(results) = c('iter_ageweek_result')
  attr(results,'miss') = list(excess_nonpos=excess_nonpos,
                              excess_notavail=excess_notavail,
                              covid_notavail=covid_notavail)
  results
}
mk_eshist_getter = function (mf, agegrp_map, fromwhichyear = 2020) {
    function (target_geo, age, wk, sex) {
        idx = which(mf$age %in% agegrp_map[[age]] &
                    mf$geo.time == target_geo &
                    mf$wkno == sprintf('%02d', as.integer(wk)) &
                    as.integer(mf$yr) < fromwhichyear)
        if (length(idx) == 0) { return(NA); }
        key = if (sex == 'M') 'value.death.m' else 'value.death.f'
        mfaggr = aggregate(mf[[key]][idx], list(mf[['yr']][idx]),
                           sum, SIMPLIFY=T)
        yrorder = order(mfaggr[['Group.1']])
        result = mfaggr[['x']][yrorder]
        yrname = paste0('Y', mfaggr[['Group.1']][yrorder])
        names(result) = yrname
        result
    }
}
mk_esexcess_getter = function (mf, agegrp_map, whichyear=2020, baseline_fn=mean) {
    get_history = mk_eshist_getter(mf)
    function (target_geo, age, wk, sex) {
        baseline = baseline_fn(get_history(target_geo, age, wk, sex))
        if (is.na(baseline)) return(NA)
        idx = which(mf$age %in% agegrp_map[[age]] &
                    mf$geo.time == target_geo &
                    mf$wkno == sprintf('%02d', as.integer(wk)) &
                    as.integer(mf$yr) == whichyear)
        if (length(idx) == 0) { return(NA); }
        key = if (sex == 'M') 'value.death.m' else 'value.death.f'
        death_total = sum(mf[[key]][idx])
        round(death_total - baseline)
    }
}
mk_m_getter = function (mf, agegrp_map, whichyear=2020) {
    function (target_geo, age, wk, sex) {
        idx = which(mf$age %in% agegrp_map[[age]] &
                    mf$geo.time == target_geo &
                    mf$wkno == sprintf('%02d', as.integer(wk)) &
                    as.integer(mf$yr) == whichyear)
        if (length(idx) == 0) { return(NA); }
        key = if (sex == 'M') 'value.popu.m' else 'value.popu.f'
        total = sum(mf[[key]][idx])
        total
    }
}
mk_national_covid_death_getter = function (coviddeath, whichyear=2020) {
    function (target_geo, age, wk, sex) {
        national_geo = nationalise_NUTS(target_geo)
        if (is.null(coviddeath[[national_geo]])) {
            stop(sprintf('We do not have national-level COVID mortality of "%s"',
                          national_geo))
        }
        agegrp_num = agegrp_tonum(age)
        whichrow = which(coviddeath[[national_geo]][['AgeGrp_min']] == agegrp_num[['AgeGrp_min']] &
                         coviddeath[[national_geo]][['AgeGrp_max']] == agegrp_num[['AgeGrp_max']] &
                         coviddeath[[national_geo]][['WeekNo']] == wk &
                         coviddeath[[national_geo]][['Year']] == whichyear)
        if (length(whichrow) == 0) {
            stop(sprintf(
                  'covid_death_getter: no record for (Geo, Age_min, Age_max, Week, Year) = (%s,%s,%s,%s,%s)',
                  target_geo, agegrp_num[['AgeGrp_min']], agegrp_num[['AgeGrp_max']], wk, whichyear))
        } else if (length(whichrow) > 1) {
            stop(sprintf(
                  'covid_death_getter: >1 records for (Geo, Age_min, Age_max, Week, Year) = (%s,%s,%s,%s,%s)',
                  target_geo, agegrp_num[['AgeGrp_min']], agegrp_num[['AgeGrp_max']], wk, whichyear))
        }
        key = if (sex == 'M') 'male_covid_death' else 'female_covid_death'
        coviddeath[[national_geo]][[key]][whichrow]
    }
}
mk_national_r_getter = function (mf, agegrp_map, whichyear=2020) {
    function (target_geo, age, wk, sex) {
        national_geo = nationalise_NUTS(target_geo)
        idx = which(mf$age %in% agegrp_map[[age]] &
                    mf$geo.time == national_geo &
                    mf$wkno == sprintf('%02d', as.integer(wk)) &
                    as.integer(mf$yr) == whichyear)
        if (length(idx) == 0) { return(NA); }
        key = if (sex == 'M') 'value.popu.m' else 'value.popu.f'
        total = sum(mf[[key]][idx])
        total
    }
}
