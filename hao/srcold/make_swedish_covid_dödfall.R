raw = readxl::read_xlsx('../dat/Folkhalsomyndigheten_covid19_modified_2020-02-07.xlsx')
raw[['week']] = strftime(raw[['Statistikdatum']], '%V')
names(raw)[2] = 'Total'
perweek = aggregate(raw[-c(1,ncol(raw))], by=list(week=raw$week), FUN=sum)
saveRDS(perweek, file='../dat/Folkhalsomyndigheten_covid19_modified_2020-02-07.rds')
