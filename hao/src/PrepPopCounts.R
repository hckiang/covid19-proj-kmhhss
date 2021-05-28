## prepare the average population size in each age group and sex for first quarter of 2019 and 2020

## Data from SCB
dfSE_pop_20192020<-read.table("SE_20192020_population.csv",header=TRUE,sep=";")
v_monthly_count_columns<-colnames(dfSE_pop_20192020)[4:ncol(dfSE_pop_20192020)]
v_monthly_count_columns_2019<-v_monthly_count_columns[1:12]
v_monthly_count_columns_2020<-v_monthly_count_columns[13:15]

v_cols_2019_use_for_mean<-c("X2019M01","X2019M02","X2019M03")
v_cols_2020_use_for_mean<-c("X2020M01","X2020M02","X2020M03")

dfSE_pop_20192020$age<-NA
dfSE_pop_20192020$age[2*(1:(nrow(dfSE_pop_20192020)/2))-1]<-0:(nrow(dfSE_pop_20192020)/2-1)
dfSE_pop_20192020$age[2*(1:(nrow(dfSE_pop_20192020)/2))]<-0:(nrow(dfSE_pop_20192020)/2-1)
dfSE_pop_20192020$sex<-NA
dfSE_pop_20192020$sex[2*(1:(nrow(dfSE_pop_20192020)/2))-1]<-"M"
dfSE_pop_20192020$sex[2*(1:(nrow(dfSE_pop_20192020)/2))]<-"F"

mPopCountsAges<-matrix(NA,ncol=length(v_monthly_count_columns),nrow=11)
rownames(mPopCountsAges)<-c("All","M_0_64","M_65_79","M_80_89","M_90","F_0_64","F_65_79","F_80_89","F_90","M","F")
mPopCountsAges["All",]<-apply(dfSE_pop_20192020[,v_monthly_count_columns],2,sum)
colnames(mPopCountsAges)<-v_monthly_count_columns
mPopCountsAges["M_0_64",]<-apply(dfSE_pop_20192020[intersect(which(dfSE_pop_20192020$sex=="M"),which(dfSE_pop_20192020$age<65)),v_monthly_count_columns],2,sum)
mPopCountsAges["M_65_79",]<-apply(dfSE_pop_20192020[intersect(which(dfSE_pop_20192020$sex=="M"),intersect(which(dfSE_pop_20192020$age>64),which(dfSE_pop_20192020$age<80))),v_monthly_count_columns],2,sum)
mPopCountsAges["M_80_89",]<-apply(dfSE_pop_20192020[intersect(which(dfSE_pop_20192020$sex=="M"),intersect(which(dfSE_pop_20192020$age>79),which(dfSE_pop_20192020$age<90))),v_monthly_count_columns],2,sum)
mPopCountsAges["M_90",]<-apply(dfSE_pop_20192020[intersect(which(dfSE_pop_20192020$sex=="M"),which(dfSE_pop_20192020$age>89)),v_monthly_count_columns],2,sum)
mPopCountsAges["M",]<-apply(dfSE_pop_20192020[which(dfSE_pop_20192020$sex=="M"),v_monthly_count_columns],2,sum)
mPopCountsAges["F_0_64",]<-apply(dfSE_pop_20192020[intersect(which(dfSE_pop_20192020$sex=="F"),which(dfSE_pop_20192020$age<65)),v_monthly_count_columns],2,sum)
mPopCountsAges["F_65_79",]<-apply(dfSE_pop_20192020[intersect(which(dfSE_pop_20192020$sex=="F"),intersect(which(dfSE_pop_20192020$age>64),which(dfSE_pop_20192020$age<80))),v_monthly_count_columns],2,sum)
mPopCountsAges["F_80_89",]<-apply(dfSE_pop_20192020[intersect(which(dfSE_pop_20192020$sex=="F"),intersect(which(dfSE_pop_20192020$age>79),which(dfSE_pop_20192020$age<90))),v_monthly_count_columns],2,sum)
mPopCountsAges["F_90",]<-apply(dfSE_pop_20192020[intersect(which(dfSE_pop_20192020$sex=="F"),which(dfSE_pop_20192020$age>89)),v_monthly_count_columns],2,sum)
mPopCountsAges["F",]<-apply(dfSE_pop_20192020[which(dfSE_pop_20192020$sex=="F"),v_monthly_count_columns],2,sum)


mPopsUse<-matrix(NA,ncol=2,nrow=nrow(mPopCountsAges))
colnames(mPopsUse)<-c("2019y","2020y")
rownames(mPopsUse)<-rownames(mPopCountsAges)
mPopsUse[,"2019y"]<-apply(mPopCountsAges[,v_cols_2019_use_for_mean],1,mean)
mPopsUse[,"2020y"]<-apply(mPopCountsAges[,v_cols_2020_use_for_mean],1,mean)