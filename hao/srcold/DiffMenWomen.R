## Study the ratio of (M/(M population size))/(F/(F population size)) deceased
## This is for the whole of Sweden jointly, but driven by Stockholm as it dominates
## For Sweden (M case fatalities)/(F case fatalities)= 2051/1647 = 1.245294 (!) for 2020-05-19
## A ratio as above is not available for the regions of Sweden
## Weekly mortality data is available per Swedish region at: 
## https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_mweek3
## but this needs to be updated still past week 17
## here we have till week 18 (seems complete)
## they key output objets are:
## dfSE_weekly_excessdeaths_MFratio: (M excess deaths wrt to given week in 2019/(M population size in 2020))/(F excess deaths wrt to given week in 2019/(F population size in 2020)) for first 18 weeks of 2019
## dfSE_weekly_2019deaths_MFratio : (M deaths/(M population size in 2019))/(F deaths /(F population size in 2019)) for first 18 weeks of 2019
## the scaling population sizes are appropriately for each age group
## Comparing them we can see:
## > dfSE_weekly_excessdeaths_MFratio
##       week    MF_All     MF_0_64  MF_65_79 MF_80_89     MF_90
##  [1,]    1        NA          NA        NA       NA        NA
##  [2,]    2 0.6902647          NA        NA 1.012672        NA
##  [3,]    3        NA          NA        NA       NA        NA
##  [4,]    4        NA          NA        NA       NA        NA
##  [5,]    5 1.5257645          NA        NA       NA  3.402923
##  [6,]    6        NA          NA        NA       NA        NA
##  [7,]    7        NA          NA        NA       NA        NA
##  [8,]    8        NA          NA        NA       NA        NA
##  [9,]    9 1.1117216          NA 1.2019382       NA  0.749549
## [10,]   10 0.9656413          NA        NA 2.172216 37.610477
## [11,]   11        NA    2.248938        NA       NA        NA
## [12,]   12 5.2784067          NA        NA 2.683621        NA
## [13,]   13 1.2000406    2.519749 2.6177185 1.072950  1.557425
## [14,]   14 1.4005991 1016.948588 2.2202878 1.587153  1.322922
## [15,]   15 1.4389091    3.311430 4.4600820 1.636450  1.395815
## [16,]   16 1.1105951 2853.527735 1.6059587 1.372646  1.451409
## [17,]   17 0.9629954          NA 0.8187325 1.210659  1.358035
## [18,]   18 1.6788238    1.366015 2.0793672 2.620087  2.504723
## 
## > dfSE_weekly_2019deaths_MFratio
##       week    MF_All  MF_0_64 MF_65_79 MF_80_89     MF_90
##  [1,]    1 0.9850712 1.642477 1.240154 1.313024 1.3922473
##  [2,]    2 1.0121797 1.661102 1.644716 1.330196 1.1402604
##  [3,]    3 1.0174448 1.682399 1.554347 1.262970 1.3022368
##  [4,]    4 0.9859623 1.586261 1.273771 1.321777 1.3671058
##  [5,]    5 0.9699236 1.368056 1.630679 1.237637 1.0990484
##  [6,]    6 0.9211485 1.782905 1.534226 1.216276 0.8801903
##  [7,]    7 0.9719470 1.426674 1.460522 1.453538 0.9781819
##  [8,]    8 1.0622552 2.366961 1.505814 1.275112 1.3294512
##  [9,]    9 0.9423758 2.098050 1.441415 1.062157 1.2294721
## [10,]   10 0.9304510 1.573537 1.372427 1.310780 0.9547941
## [11,]   11 1.0223918 1.456240 1.536604 1.397235 1.2710083
## [12,]   12 0.9339207 1.563920 1.439139 1.238703 1.0016962
## [13,]   13 0.9910661 1.430039 1.374032 1.535680 1.0751341
## [14,]   14 0.9450940 1.314927 1.270000 1.312334 1.3137539
## [15,]   15 0.8740294 1.279571 1.147406 1.255579 1.0813766
## [16,]   16 0.9672463 1.533823 1.596856 1.253674 1.0467127
## [17,]   17 0.9759211 1.438105 1.618760 1.291320 1.0688880
## [18,]   18 0.9335553 1.723008 1.617796 1.093447 1.1065249
## 0) There are a lot of NAs dfSE_weekly_excessdeaths_MFratio, these correspond to the situation where at least one of the excess deaths,M or F is <0
##    such cases will require different treatment, i.e. to develop when excess mortality is negative
## 1) The ratio for 2019 is around the covid sex death ratio of 1.25
## 2) A lot of weekly 2020 ratios are similar to the 2019 ones
## 3) There are quite a few exccess mortality ratios in 2020 that are way above the 1.25 Swedish "covid" ratio

#require(dplyr)
#require(ggplot2)
#require(tidyverse)
#require(hrbrthemes)
#require(lubridate)
##require(COVID19) 

## Population data downloaded from SCB 2020-05-18
source("PrepPopCounts.R")

week_2020_end<-18
week_COVID_start_SE<-13

dfSE_deaths_20192020<-read.table("SE_20192020_deaths.csv",sep=";",header=TRUE)

v_old_colnames<-colnames(dfSE_deaths_20192020)
v_new_colnames<-c("DayYear","All_2019","M_0_64_2019","M_65_79_2019","M_80_89_2019",
"M_90_2019","F_0_64_2019","F_65_79_2019","F_80_89_2019","F_90_2019",
"All_2020","M_0_64_2020","M_65_79_2020","M_80_89_2020","M_90_2020",
"F_0_64_2020","F_65_79_2020","F_80_89_2020","F_90_2020", "X",
"Sort","Day","Month")  


v_2020_AgeSexNames<-c("All_2020","M_0_64_2020","M_65_79_2020","M_80_89_2020","M_90_2020",
"F_0_64_2020","F_65_79_2020","F_80_89_2020","F_90_2020")

v_2020_MaleAgeNames<-c("M_0_64_2020","M_65_79_2020","M_80_89_2020","M_90_2020")
v_2020_FemaleAgeNames<-c("F_0_64_2020","F_65_79_2020","F_80_89_2020","F_90_2020")

v_2019_AgeSexNames<-c("All_2019","M_0_64_2019","M_65_79_2019","M_80_89_2019","M_90_2019",
"F_0_64_2019","F_65_79_2019","F_80_89_2019","F_90_2019")
v_2019_MaleAgeNames<-c("M_0_64_2019","M_65_79_2019","M_80_89_2019","M_90_2019")
v_2019_FemaleAgeNames<-c("F_0_64_2019","F_65_79_2019","F_80_89_2019","F_90_2019")



colnames(dfSE_deaths_20192020)<-v_new_colnames
mAges<-data.frame("Column_name"=c("All_2019","M_0_64_2019","M_65_79_2019","M_80_89_2019",
"M_90_2019","F_0_64_2019","F_65_79_2019","F_80_89_2019","F_90_2019",
"All_2020","M_0_64_2020","M_65_79_2020","M_80_89_2020","M_90_2020",
"F_0_64_2020","F_65_79_2020","F_80_89_2020","F_90_2020","M_2019","F_2019","M_2020","F_2020"),
"Min_age"=c(0,0,65,80,90,0,65,80,90,0,0,6,80,90,0,65,80,90,0,0,0,0),
"Max_age"=c(Inf,64,79,89,Inf,64,79,89,Inf,Inf,64,79,89,Inf,64,79,89,Inf,Inf,Inf,Inf,Inf))

mAgeSexStratifiedDeathCols<-c("M_0_64_2019","M_65_79_2019","M_80_89_2019","M_90_2019","F_0_64_2019","F_65_79_2019","F_80_89_2019","F_90_2019","M_0_64_2020","M_65_79_2020","M_80_89_2020","M_90_2020","F_0_64_2020","F_65_79_2020","F_80_89_2020","F_90_2020")
mAllDeathCols<-c("All_2019","M_0_64_2019","M_65_79_2019","M_80_89_2019","M_90_2019","F_0_64_2019","F_65_79_2019","F_80_89_2019","F_90_2019","All_2020","M_0_64_2020","M_65_79_2020","M_80_89_2020","M_90_2020","F_0_64_2020","F_65_79_2020","F_80_89_2020","F_90_2020")


mMonths<-data.frame("SE_month"=c("januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december"),"Use_month"=c(paste0("0",1:9),"10","11","12"))

dfSE_deaths_20192020$date<-apply(dfSE_deaths_20192020[,c("Day","Month"),drop=FALSE],1,function(x,mMonths){paste0("2020-",mMonths[which(mMonths[,1]==x[2]),2],"-",x[1])},mMonths=mMonths)

## treatment of unknown date of death, last row of data frame
n_rowsSE<-nrow(dfSE_deaths_20192020)
dfSE_deaths_20192020$week<-NA
dfSE_deaths_20192020$week[1:(n_rowsSE-1)]<-sapply(dfSE_deaths_20192020$date[-n_rowsSE],function(x){as.numeric(strftime(x, format = "%V"))},simplify=TRUE)
dfSE_deaths_20192020[nrow(dfSE_deaths_20192020),c("DayYear","Day","Month","date","week")]<-NA

## Distribute the deaths according to proportions of dead in a given day

dfSE_deaths_20192020_unknownspread<-dfSE_deaths_20192020
dfSE_deaths_20192020_unknownspread[,mAllDeathCols]<-apply(dfSE_deaths_20192020[,mAllDeathCols,drop=FALSE],2,
	function(x,n_rowsSE){    
	    unknowns<-x[n_rowsSE];    
	    x[-n_rowsSE]<-x[-n_rowsSE]+unknowns*x[-n_rowsSE]/sum(x[-n_rowsSE]) ;   
	    x
},n_rowsSE=n_rowsSE)
## finally remove them so they do not interfere
dfSE_deaths_20192020_unknownspread<-dfSE_deaths_20192020_unknownspread[-n_rowsSE,,drop=FALSE]
## =================================================================

## change to weekly deaths
dfSE_weeklydeaths_20192020_unknownspread<-dfSE_deaths_20192020_unknownspread[FALSE,]
dfSE_weeklydeaths_20192020_unknownspread[1:length(unique(dfSE_deaths_20192020_unknownspread$week)),]<-NA
for (i in 1:length(unique(dfSE_deaths_20192020_unknownspread$week))){
    dfSE_weeklydeaths_20192020_unknownspread[i,]<-NA
    week_num<-sort(unique(dfSE_deaths_20192020_unknownspread$week))[i]
    dfSE_weeklydeaths_20192020_unknownspread[i,mAllDeathCols]<-apply(dfSE_deaths_20192020_unknownspread[which(dfSE_deaths_20192020_unknownspread$week==week_num),mAllDeathCols],2,sum)
    dfSE_weeklydeaths_20192020_unknownspread[i,-c(which(colnames(dfSE_weeklydeaths_20192020_unknownspread)%in%mAllDeathCols))]<-dfSE_deaths_20192020_unknownspread[which(dfSE_deaths_20192020_unknownspread$week==week_num)[1],-c(which(colnames(dfSE_weeklydeaths_20192020_unknownspread)%in%mAllDeathCols))]
}
dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek<-dfSE_weeklydeaths_20192020_unknownspread[1:week_2020_end,]
## first week of 2020 has only five days!
## we could add last two days of 2019 to week 1 2020
## but then definition of week 1 in 2019 would not agree
#dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek[1,v_2020_AgeSexNames]<-dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek[1,v_2020_AgeSexNames]+apply(dfSE_deaths_20192020_unknownspread[c(n_rowsSE-2,n_rowsSE-1),v_2019_AgeSexNames],2,sum)


## Calculate weekly excess deaths
dfSE_weekly_excessdeaths<-matrix(NA,nrow=week_2020_end,ncol=3+length(v_2020_AgeSexNames))
colnames(dfSE_weekly_excessdeaths)<-c("week",v_2020_AgeSexNames,"M","F")
dfSE_weekly_excessdeaths[,"week"]<-dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek$week
dfSE_weekly_excessdeaths[,v_2020_AgeSexNames]<- as.matrix(dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek[,v_2020_AgeSexNames]-dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek[,v_2019_AgeSexNames])
dfSE_weekly_excessdeaths[,"M"]<-apply(dfSE_weekly_excessdeaths[,v_2020_MaleAgeNames],1,sum)
dfSE_weekly_excessdeaths[,"F"]<-apply(dfSE_weekly_excessdeaths[,v_2020_FemaleAgeNames],1,sum)


## calculate the weekly deaths per capita
dfSE_weeklydeaths_per_capita<-dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek
dfSE_weeklydeaths_per_capita<-dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek
dfSE_weeklydeaths_per_capita$M_2019<-apply(dfSE_weeklydeaths_per_capita[,v_2019_MaleAgeNames],1,sum)
dfSE_weeklydeaths_per_capita$F_2019<-apply(dfSE_weeklydeaths_per_capita[,v_2019_FemaleAgeNames],1,sum)
dfSE_weeklydeaths_per_capita$M_2020<-apply(dfSE_weeklydeaths_per_capita[,v_2020_MaleAgeNames],1,sum)
dfSE_weeklydeaths_per_capita$F_2020<-apply(dfSE_weeklydeaths_per_capita[,v_2020_FemaleAgeNames],1,sum)

dfSE_weeklydeaths_per_capita$All_2019<-dfSE_weeklydeaths_per_capita$All_2019/mPopsUse["All","2019y"]
dfSE_weeklydeaths_per_capita$M_0_64_2019<-dfSE_weeklydeaths_per_capita$M_0_64_2019/mPopsUse["M_0_64","2019y"]
dfSE_weeklydeaths_per_capita$M_65_79_2019<-dfSE_weeklydeaths_per_capita$M_65_79_2019/mPopsUse["M_65_79","2019y"]
dfSE_weeklydeaths_per_capita$M_80_89_2019<-dfSE_weeklydeaths_per_capita$M_80_89_2019/mPopsUse["M_80_89","2019y"]
dfSE_weeklydeaths_per_capita$M_90_2019<-dfSE_weeklydeaths_per_capita$M_90_2019/mPopsUse["M_90","2019y"]
dfSE_weeklydeaths_per_capita$F_0_64_2019<-dfSE_weeklydeaths_per_capita$F_0_64_2019/mPopsUse["F_0_64","2019y"]
dfSE_weeklydeaths_per_capita$F_65_79_2019<-dfSE_weeklydeaths_per_capita$F_65_79_2019/mPopsUse["F_65_79","2019y"]
dfSE_weeklydeaths_per_capita$F_80_89_2019<-dfSE_weeklydeaths_per_capita$F_80_89_2019/mPopsUse["F_80_89","2019y"]
dfSE_weeklydeaths_per_capita$F_90_2019<-dfSE_weeklydeaths_per_capita$F_90_2019/mPopsUse["F_90","2019y"]
dfSE_weeklydeaths_per_capita$All_2020<-dfSE_weeklydeaths_per_capita$All_2020/mPopsUse["All","2020y"]
dfSE_weeklydeaths_per_capita$M_0_64_2020<-dfSE_weeklydeaths_per_capita$M_0_64_2020/mPopsUse["M_0_64","2020y"]
dfSE_weeklydeaths_per_capita$M_65_79_2020<-dfSE_weeklydeaths_per_capita$M_65_79_2020/mPopsUse["M_65_79","2020y"]
dfSE_weeklydeaths_per_capita$M_80_89_2020<-dfSE_weeklydeaths_per_capita$M_80_89_2020/mPopsUse["M_80_89","2020y"]
dfSE_weeklydeaths_per_capita$M_90_2020<-dfSE_weeklydeaths_per_capita$M_90_2020/mPopsUse["M_90","2020y"]
dfSE_weeklydeaths_per_capita$F_0_64_2020<-dfSE_weeklydeaths_per_capita$F_0_64_2020/mPopsUse["F_0_64","2020y"]
dfSE_weeklydeaths_per_capita$F_65_79_2020<-dfSE_weeklydeaths_per_capita$F_65_79_2020/mPopsUse["F_65_79","2020y"]
dfSE_weeklydeaths_per_capita$F_80_89_2020<-dfSE_weeklydeaths_per_capita$F_80_89_2020/mPopsUse["F_80_89","2020y"]
dfSE_weeklydeaths_per_capita$F_90_2020<-dfSE_weeklydeaths_per_capita$F_90_2020/mPopsUse["F_90","2020y"]
dfSE_weeklydeaths_per_capita$M_2019<-dfSE_weeklydeaths_per_capita$M_2019/mPopsUse["M","2019y"]
dfSE_weeklydeaths_per_capita$F_2019<-dfSE_weeklydeaths_per_capita$F_2019/mPopsUse["F","2019y"]
dfSE_weeklydeaths_per_capita$M_2020<-dfSE_weeklydeaths_per_capita$M_2020/mPopsUse["M","2020y"]
dfSE_weeklydeaths_per_capita$F_2020<-dfSE_weeklydeaths_per_capita$F_2020/mPopsUse["F","2020y"]


## now calculate weekly per capita excess deaths
## i.e. difference of per capita deaths
dfSE_weekly_excessdeaths_percapita<-matrix(NA,nrow=week_2020_end,ncol=3+length(v_2020_AgeSexNames))
colnames(dfSE_weekly_excessdeaths_percapita)<-c("week",v_2020_AgeSexNames,"M","F")
dfSE_weekly_excessdeaths_percapita[,"week"]<-dfSE_weeklydeaths_per_capita$week
dfSE_weekly_excessdeaths_percapita[,v_2020_AgeSexNames]<- as.matrix(dfSE_weeklydeaths_per_capita[,v_2020_AgeSexNames]-dfSE_weeklydeaths_per_capita[,v_2019_AgeSexNames])
dfSE_weekly_excessdeaths_percapita[,"M"]<-dfSE_weeklydeaths_per_capita$M_2020-dfSE_weeklydeaths_per_capita$M_2019
dfSE_weekly_excessdeaths_percapita[,"F"]<-dfSE_weeklydeaths_per_capita$F_2020-dfSE_weeklydeaths_per_capita$F_2019

## change to per million
dfSE_weekly_excessdeaths_permln <- (10^6)*dfSE_weekly_excessdeaths_percapita
dfSE_weekly_excessdeaths_permln[,"week"] <- dfSE_weekly_excessdeaths_percapita[,"week"]

## downloaded from FH 2020-05-19
SE_covidDeadM<-2051
SE_covidDeadF<-1647
## SE_covidDead_MFratio<-SE_covidDeadF/SE_covidDeadM           # Wrong line before
SE_covidDead_MFratio<-SE_covidDeadM/SE_covidDeadF
## ASSUME that gender acts the same way in all ages!
## For Sweden we do not seem to have COVID deaths per age group
## ===============================================================


## Calulate the MF ratios
dfSE_weekly_excessdeaths_MFratio<-matrix(NA,nrow=nrow(dfSE_weekly_excessdeaths_permln),ncol=6)
colnames(dfSE_weekly_excessdeaths_MFratio)<-c("week","MF_All","MF_0_64","MF_65_79","MF_80_89","MF_90")
dfSE_weekly_excessdeaths_MFratio[,"week"]<-dfSE_weekly_excessdeaths_permln[,"week"]
dfSE_weekly_excessdeaths_MFratio[,"MF_All"]<-dfSE_weekly_excessdeaths_permln[,"M"]/dfSE_weekly_excessdeaths_permln[,"F"]
dfSE_weekly_excessdeaths_MFratio[,"MF_0_64"]<-dfSE_weekly_excessdeaths_permln[,"M_0_64_2020"]/dfSE_weekly_excessdeaths_permln[,"F_0_64_2020"]
dfSE_weekly_excessdeaths_MFratio[,"MF_65_79"]<-dfSE_weekly_excessdeaths_permln[,"M_65_79_2020"]/dfSE_weekly_excessdeaths_permln[,"F_65_79_2020"]
dfSE_weekly_excessdeaths_MFratio[,"MF_80_89"]<-dfSE_weekly_excessdeaths_permln[,"M_80_89_2020"]/dfSE_weekly_excessdeaths_permln[,"F_80_89_2020"]
dfSE_weekly_excessdeaths_MFratio[,"MF_90"]<-dfSE_weekly_excessdeaths_permln[,"M_90_2020"]/dfSE_weekly_excessdeaths_permln[,"F_90_2020"]

## Find those entries where both M and F excess deaths are negativ but M < F
#mFlipMFratio<-apply((dfSE_weekly_excessdeaths_permln[,v_2020_MaleAgeNames]<dfSE_weekly_excessdeaths_permln[,v_2020_FemaleAgeNames])*(dfSE_weekly_excessdeaths_permln[,v_2020_MaleAgeNames]<0)*(dfSE_weekly_excessdeaths_permln[,v_2020_FemaleAgeNames]<0),c(1,2),as.logical)
#dfSE_weekly_excessdeaths_MFratio[,c("MF_All","MF_0_64","MF_65_79","MF_80_89","MF_90")][mFlipMFratio]<-1/(dfSE_weekly_excessdeaths_MFratio[,c("MF_All","MF_0_64","MF_65_79","MF_80_89","MF_90")][mFlipMFratio])

#entries with any negative access mortality made NA, they require a separate study
mNegExcess<-apply((dfSE_weekly_excessdeaths_permln[,v_2020_MaleAgeNames]<0)|(dfSE_weekly_excessdeaths_permln[,v_2020_FemaleAgeNames]<0),c(1,2),as.logical)
dfSE_weekly_excessdeaths_MFratio[,c("MF_0_64","MF_65_79","MF_80_89","MF_90")][mNegExcess]<-NA
dfSE_weekly_excessdeaths_MFratio[unique(c(which(dfSE_weekly_excessdeaths_permln[,"M"]<0),which(dfSE_weekly_excessdeaths_permln[,"F"]<0))),"MF_All"]<-NA

## Calculate per capita ratios in 2019
dfSE_weekly_2019deaths_MFratio<-matrix(NA,nrow=nrow(dfSE_weekly_excessdeaths_permln),ncol=6)
colnames(dfSE_weekly_2019deaths_MFratio)<-c("week","MF_All","MF_0_64","MF_65_79","MF_80_89","MF_90")
dfSE_weekly_2019deaths_MFratio[,"week"]<-dfSE_weekly_excessdeaths_permln[,"week"]
dfSE_weekly_2019deaths_MFratio[,"MF_All"]<-dfSE_weeklydeaths_per_capita$M_2019/dfSE_weeklydeaths_per_capita$F_2019
dfSE_weekly_2019deaths_MFratio[,"MF_0_64"]<-dfSE_weeklydeaths_per_capita$M_0_64_2019/dfSE_weeklydeaths_per_capita$F_0_64_2019
dfSE_weekly_2019deaths_MFratio[,"MF_65_79"]<-dfSE_weeklydeaths_per_capita$M_65_79_2019/dfSE_weeklydeaths_per_capita$F_65_79_2019
dfSE_weekly_2019deaths_MFratio[,"MF_80_89"]<-dfSE_weeklydeaths_per_capita$M_80_89_2019/dfSE_weeklydeaths_per_capita$F_80_89_2019
dfSE_weekly_2019deaths_MFratio[,"MF_90"]<-dfSE_weeklydeaths_per_capita$M_90_2019/dfSE_weeklydeaths_per_capita$F_90_2019

## calculate total MF ratios in 2019 and 2020
SE_excessDeaths_M_permln<-(10^6)*sum(dfSE_weekly_excessdeaths[week_COVID_start_SE:week_2020_end,"M"])/mPopsUse["M","2020y"]
SE_excessDeaths_F_permln<-(10^6)*sum(dfSE_weekly_excessdeaths[week_COVID_start_SE:week_2020_end,"F"])/mPopsUse["F","2020y"]
SE_excessDeaths_MFratio<-SE_excessDeaths_M_permln/SE_excessDeaths_F_permln

SE_2019deaths_M_permln<-(10^6)*sum(dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek[week_COVID_start_SE:week_2020_end,v_2019_MaleAgeNames])/mPopsUse["M","2019y"]
SE_2019deaths_F_permln<-(10^6)*sum(dfSE_weeklydeaths_20192020_unknownspread_till_2020_endweek[week_COVID_start_SE:week_2020_end,v_2019_FemaleAgeNames])/mPopsUse["F","2019y"]
SE_2019deaths_MFratio<-SE_2019deaths_M_permln/SE_2019deaths_F_permln

