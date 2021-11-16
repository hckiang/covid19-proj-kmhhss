## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this package or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .


f_tabYearMortalityC19<-function(dfMortality,dfPopulations,v_countries,focus_year="y2020",sex="T",age="0_Inf",pop_year=NULL,week_start=NULL,week_end=NULL,baselineyears=paste0("y",2015:2019)){
## focus year should agree with week_start, week_end, the weeks period is labelled by the year of week_end
## we assume week_start to week_end should not be just under 1 year or just over 1 year
## it should be a clear period, either the whole same year, or some shorter that 1 year time period or some definitive longer than 1 year period

    mOutput<-matrix(NA,nrow= 5, ncol=2*length(v_countries))
    colnames(mOutput)<-c(rbind(v_countries,paste0(v_countries,"_100k")))
    rownames(mOutput)<-c("Population_size","Baseline_mortality","Focus_year_mortality","Fold_change","Excess_mortality")
    dfMortality<-cbind(dfMortality,YEAR=sapply(dfMortality$TIME,function(x){as.numeric(strsplit(x,"W")[[1]][1])},simplify=TRUE),WEEK=sapply(dfMortality$TIME,function(x){as.numeric(strsplit(x,"W")[[1]][2])},simplify=TRUE))

    year_span<-1
    if (is.null(week_start)||(is.null(week_start))){
    ## if the week_start and end are not provided make them fail safe
	focus_year_value<-as.numeric(strsplit(focus_year,"y")[[1]][2])
	if (is.null(week_start)){
	    if(!is.null(week_end)){
		y_e<-as.numeric(strsplit(week_end,"W")[[1]][1])
		w_e<-as.numeric(strsplit(week_end,"W")[[1]][2])
		w_s<-max(min(w_e-1,1),1)
		if (w_s<10){w_s<-paste0("0",w_s)}
		if (focus_year_value<=y_e){week_start<-paste0(y_e,"W",w_s)}
		else{week_end<-paste0(focus_year_value,"W",w_s)}			    
	    }else{week_start<-paste0(focus_year_value,"W01")}
	}
	if (is.null(week_end)){
	    y_s<-as.numeric(strsplit(week_start,"W")[[1]][1])
	    w_s<-as.numeric(strsplit(week_start,"W")[[1]][2])	    
	    w_e<-min(max(w_s+1,53),53)
	    if (w_e<10){w_e<-paste0("0",w_e)}
	    if (focus_year_value<=y_s){week_end<-paste0(y_s,"W",w_e)}
	    else{week_end<-paste0(focus_year_value,"W",w_e)}
	}
	y_e<-as.numeric(strsplit(week_end,"W")[[1]][1])
	w_e<-as.numeric(strsplit(week_end,"W")[[1]][2])
    }
    
    ## infer if the period of interest is one year or more
    year_span<-1
    y_e<-as.numeric(strsplit(week_end,"W")[[1]][1])
    w_e<-as.numeric(strsplit(week_end,"W")[[1]][2])
    y_s<-as.numeric(strsplit(week_start,"W")[[1]][1])
    w_s<-as.numeric(strsplit(week_start,"W")[[1]][2])	  
    
    if(y_e<y_s){
	stop("week_end has earlier year than week_start")
    }
    
    if (y_e>y_s){
	## we need to check how many weeks difference
	num_weeks_s<-lubridate::week(paste0(y_s,"-12-31"))
	sum_weeks_between<-num_weeks_s-w_s+1+w_e
	year_span<-y_e-y_s+ifelse(sum_weeks_between>53,1,0)	
    }

    week_start_orig<-week_start
    week_end_orig<-week_end
    
    for (country in v_countries){
	week_start<-week_start_orig
	week_end<-week_end_orig
	if (is.element("COUNTRYREGION",colnames(dfMortality))){
	    dfCountry_mort<-dfMortality[which(dfMortality$COUNTRYREGION==country),]
	}else{
	    dfCountry_mort<-dfMortality[which(dfMortality$COUNTRY==country),]
	}
	dfCountry_mort_tot<-dfCountry_mort[intersect(which(dfCountry_mort$SEX==sex),which(dfCountry_mort$AGE==age)),]
	## assume week_start and end are within a 1 year period

	Country_yearmort<-c()
	y_s<-as.numeric(strsplit(week_start,"W")[[1]][1])
	w_s<-as.numeric(strsplit(week_start,"W")[[1]][2])
	y_e<-as.numeric(strsplit(week_end,"W")[[1]][1])
	w_e<-as.numeric(strsplit(week_end,"W")[[1]][2])
    
	b_have_year<-TRUE
	while (b_have_year){
	    if (y_s %in% dfCountry_mort_tot$YEAR){
	    ## if the week start end period is over a year, then we cannot iterate over the years in our dataset
		yearmort_value<-sum(dfCountry_mort_tot$Value[intersect(which(dfCountry_mort_tot$TIME>=week_start),which(dfCountry_mort_tot$TIME<=week_end))],na.rm=TRUE)
		## "2020W01" < "2020W02" returns TRUE so for this year week structure we may do normal comparisons
		Country_yearmort<-c(Country_yearmort,yearmort_value)
		names(Country_yearmort)<-c(names(Country_yearmort)[-length(Country_yearmort)],paste0("y",y_e))
		y_s<-y_s-year_span
		y_e<-y_e-year_span
		if (w_s<10){w_s<-paste0("0",w_s)}
		if (w_e<10){w_e<-paste0("0",w_e)}
		week_start<-paste0(y_s,"W",w_s)
		week_end<-paste0(y_e,"W",w_e)	  
	    }else{b_have_year<-FALSE}      
	}

	if (is.element("COUNTRYREGION",colnames(dfPopulations))){
	    dfCountry_pop<-dfPopulations[which(dfPopulations$COUNTRYREGION==country),]
	}else{
	    dfCountry_pop<-dfPopulations[which(dfPopulations$COUNTRY==country),]
	}

	Country_pop_tot<-dfCountry_pop[intersect(which(dfCountry_pop$SEX==sex),which(dfCountry_pop$AGE==age)),"Value"]
	names(Country_pop_tot)<-dfCountry_pop[intersect(which(dfCountry_pop$SEX==sex),which(dfCountry_pop$AGE==age)),"TIME"]
	names(Country_pop_tot)<-paste0("y",names(Country_pop_tot))
	if (!is.null(pop_year)){
	    Country_pop_tot<-c(Country_pop_tot,pop_year[[which(names(pop_year)==country)]])
	}
	v_Mortality100k<-Country_yearmort
	for (i in 1:length(v_Mortality100k)){
	    pop_size_value_year<-Country_pop_tot[which(names(Country_pop_tot)==(names(v_Mortality100k)[i]))]
	    if (length(pop_size_value_year)==1){v_Mortality100k[i]<-v_Mortality100k[i]/pop_size_value_year*100000}
	    else{v_Mortality100k[i]<-NA}
	}
	baseline_mortality<-mean(Country_yearmort[baselineyears]) 
	baseline_mortality100k<-mean(v_Mortality100k[baselineyears]) ## we cannot take baseline_mortality and divide by latest population size, need to do this average over scaled population as population size can vary from year to year
    
	j<-which(colnames(mOutput)==country)
	
	mOutput[1,j]<-Country_pop_tot[which(names(Country_pop_tot)==focus_year)]
	mOutput[2,c(j,j+1)]<-c(baseline_mortality,baseline_mortality100k)

	mOutput[3,c(j,j+1)]<-c(Country_yearmort[which(names(Country_yearmort)==focus_year)],v_Mortality100k[which(names(v_Mortality100k)==focus_year)])
	mOutput[4,c(j+1)]<-mOutput[3,j+1]/mOutput[2,j+1]
	mOutput[5,c(j,j+1)]<-mOutput[3,c(j,j+1)]-mOutput[2,c(j,j+1)]
    }
    rownames(mOutput)[1]<-paste0("Population_size_",names(Country_pop_tot)[length(Country_pop_tot)])

    mOutput
}

f_get_COVID19_data<-function(v_countries,v_country_codes,start_date,end_date,v_population){
    start_date<-as.character(start_date)
    end_date<-as.character(end_date)
    mOutput<-matrix(NA,nrow= 2, ncol=2*length(v_countries))
    colnames(mOutput)<-c(rbind(v_country_codes,paste0(v_country_codes,"_100k")))
    rownames(mOutput)<-c("CC","CF")
    for (j in 1:length(v_countries)){
	tibC19<-COVID19::covid19(country=v_countries[j])[,c("date","confirmed","deaths","population")]
	start_vals<-tibC19[which(tibC19$date==start_date),]
	end_vals<-tibC19[which(tibC19$date==end_date),]
	mOutput[1,2*j-1]<-end_vals$confirmed-ifelse((is.na(start_vals$confirmed)||length(start_vals$confirmed)==0),0,start_vals$confirmed)
	mOutput[1,2*j]<-mOutput[1,2*j-1]/v_population[j]*100000
	mOutput[2,2*j-1]<-end_vals$deaths-ifelse((is.na(start_vals$deaths)||length(start_vals$confirmed)==0),0,start_vals$deaths)
	mOutput[2,2*j]<-mOutput[2,2*j-1]/v_population[j]*100000
    }
    mOutput
}
	

f_create_COVID19_table<-function(v_countries,v_country_codes,dfMortality,dfPopulations,year_start=2020,year_end=2021,day_start_start_year_totC19="2020-01-22",day_end_end_year="2021-08-14",week_end_end_year="W32",v_baselineyears=2015:2019,sex_for_table="T",age_group_for_table="0_Inf",l_pop_year=list(PL=c(y2021=37840001),SE=c(y2021=10379295),CH=c(y2021=8667088)),week_start_partial="W01",week_end_partial=week_end_end_year,start_day_partial=as.Date(paste(year_start,as.numeric(strsplit(week_start_partial,"W")[[1]][2]),1,sep=""),"%Y%U%u"),end_day_partial=as.Date(paste(year_start,as.numeric(strsplit(week_end_partial,"W")[[1]][2]),1,sep=""),"%Y%U%u")){
    mOutput_totmort<-vector("list",year_end-year_start+1)
    for(j in 1:(year_end-year_start)){
	mOutput_totmort[[j]]<-f_tabYearMortalityC19(dfMortality=dfMortality,dfPopulations=dfPopulations,v_countries=v_country_codes,focus_year=paste0("y",year_start+j-1),sex=sex_for_table,age=age_group_for_table,pop_year=NULL,week_start=NULL,week_end=NULL,baselineyears=paste0("y",v_baselineyears))    
    }
    if (year_end-year_start+1>1){
	j<-year_end-year_start+1 ## we assume the last year is not complete
	mOutput_totmort[[j]]<-f_tabYearMortalityC19(dfMortality=dfMortality,dfPopulations=dfPopulations,v_countries=v_country_codes,focus_year=paste0("y",year_end),sex=sex_for_table,age=age_group_for_table,pop_year=l_pop_year,week_start=paste0(year_end,"W01"),week_end=paste0(year_end,week_end_end_year),baselineyears=paste0("y",v_baselineyears))    
    }
    names(mOutput_totmort)<-paste0("ytot",year_start:year_end)
    
    mOutput_partmort<-vector("list",year_end-year_start+1)
    for(j in 1:(year_end-year_start)){
	mOutput_partmort[[j]]<-tryCatch({f_tabYearMortalityC19(dfMortality=dfMortality,dfPopulations=dfPopulations,v_countries=v_country_codes,focus_year=paste0("y",year_start+j-1),sex=sex_for_table,age=age_group_for_table,pop_year=NULL,week_start=paste0(year_start+j-1,week_start_partial),week_end=paste0(year_start+j-1,week_end_partial),baselineyears=paste0("y",v_baselineyears))},error=function(e){print(year_start+j-1);print(e)})
    }
    if (year_end-year_start+1>1){
	j<-year_end-year_start+1 ## we assume the last year is not complete
	mOutput_partmort[[j]]<-tryCatch({f_tabYearMortalityC19(dfMortality=dfMortality,dfPopulations=dfPopulations,v_countries=v_country_codes,focus_year=paste0("y",year_start+j-1),sex=sex_for_table,age=age_group_for_table,pop_year=l_pop_year,week_start=paste0(year_start+j-1,week_start_partial),week_end=paste0(year_start+j-1,week_end_partial),baselineyears=paste0("y",v_baselineyears))},error=function(e){print(year_start+j-1);print(e)})
    }    
    names(mOutput_partmort)<-paste0("ypart",year_start:year_end)

    ##mOutput_2020<-f_tabYearMortalityC19(dfMortality=dfMortality,dfPopulations=dfPopulations,v_countries=v_country_codes,focus_year="y2020",sex="T",age="0_Inf",pop_year=NULL,week_start=NULL,week_end=NULL,baselineyears=paste0("y",2015:2019))    
    ##mOutput_2021<-f_tabYearMortalityC19(dfMortality=dfMortality,dfPopulations=dfPopulations,v_countries=v_country_codes,focus_year="y2021",sex="T",age="0_Inf",pop_year=list(PL=c(y2021=37840001),SE=c(y2021=10379295),CH=c(y2021=8667088)),week_start="2021W01",week_end=paste0("2021",week_end_2021),baselineyears=paste0("y",2015:2019))    
    ##mOutput_202W2021end<-f_tabYearMortalityC19(dfMortality=dfMortality,dfPopulations=dfPopulations,v_countries=v_country_codes,focus_year="y2020",sex="T",age="0_Inf",pop_year=NULL,week_start="2020W01",week_end=paste0("2020",week_end_2021),baselineyears=paste0("y",2015:2019))    

    mOutput_C19_all<-f_get_COVID19_data(v_countries,v_country_codes,start_date=day_start_start_year_totC19,end_date=day_end_end_year,v_population=mOutput_totmort[[year_end-year_start+1]][1,which(1:ncol(mOutput_totmort[[year_end-year_start+1]])%%2==1)])
    mOutput_totC19<-vector("list",year_end-year_start+1)
    mOutput_totC19[[1]]<-f_get_COVID19_data(v_countries,v_country_codes,start_date=day_start_start_year_totC19,end_date=paste0(year_start,"-12-31"),v_population=mOutput_totmort[[j]][1,which(1:ncol(mOutput_totmort[[j]])%%2==1)])
    if (year_end-year_start>1){
	for(j in 2:(year_end-year_start)){
	    mOutput_totC19[[j]]<-f_get_COVID19_data(v_countries,v_country_codes,start_date=paste0(year_start+j-1,"-01-01"),end_date=paste0(year_start+j-1,"-12-31"),v_population=mOutput_totmort[[j]][1,which(1:ncol(mOutput_totmort[[j]])%%2==1)])
	}
    }
    if (year_end-year_start+1>1){
	j<-year_end-year_start+1 ## we assume the last year is not complete
	mOutput_totC19[[j]]<-f_get_COVID19_data(v_countries,v_country_codes,start_date=paste0(year_end,"-01-01"),end_date=day_end_end_year,v_population=mOutput_totmort[[j]][1,which(1:ncol(mOutput_totmort[[j]])%%2==1)])
    }
    names(mOutput_totC19)<-paste0("ytotC19",year_start:year_end)

    mOutput_partC19<-vector("list",year_end-year_start+1)

    mOutput_partC19[[1]]<-tryCatch({f_get_COVID19_data(v_countries,v_country_codes,start_date=lubridate::date(max(lubridate::date(start_day_partial),lubridate::date(day_start_start_year_totC19))),end_date=date(lubridate::date(end_day_partial)+lubridate::dyears(j-1)),v_population=mOutput_totmort[[j]][1,which(1:ncol(mOutput_totmort[[j]])%%2==1)])},error=function(e){print(year_start+j-1);print(e)})    
    if (year_end-year_start>1){
	for(j in 2:(year_end-year_start)){
	    mOutput_partC19[[j]]<-tryCatch({f_get_COVID19_data(v_countries,v_country_codes,start_date=lubridate::date(lubridate::date(start_day_partial)+lubridate::dyears(j-1)),end_date=date(lubridate::date(end_day_partial)+lubridate::dyears(j-1)),v_population=mOutput_totmort[[j]][1,which(1:ncol(mOutput_totmort[[j]])%%2==1)])},error=function(e){print(year_start+j-1);print(e)})
	}
    }
    if (year_end-year_start+1>1){
	j<-year_end-year_start+1 ## we assume the last year is not complete
	mOutput_partC19[[j]]<-tryCatch({f_get_COVID19_data(v_countries,v_country_codes,start_date=lubridate::date(lubridate::date(start_day_partial)+lubridate::dyears(j-1)),end_date=lubridate::date(min(lubridate::date(lubridate::date(end_day_partial)+lubridate::dyears(j-1)),lubridate::date(day_end_end_year))),v_population=mOutput_totmort[[j]][1,which(1:ncol(mOutput_totmort[[j]])%%2==1)])},error=function(e){print(year_start+j-1);print(e)})
    }    
    names(mOutput_partC19)<-paste0("ypartC19",year_start:year_end)

    
    #mOutput_C19_2020<-f_get_COVID19_data(v_countries,v_country_codes,start_date=day_start_2020,end_date="2020-12-31",v_population=mOutput_2021[1,which(1:ncol(mOutput_2020)%%2==1)])
    #mOutput_C19_2021<-f_get_COVID19_data(v_countries,v_country_codes,start_date="2021-01-01",end_date=day_end_2021,v_population=mOutput_2021[1,which(1:ncol(mOutput_2021)%%2==1)])

    l_mOutput<-vector("list",year_end-year_start+1)
    names(l_mOutput)<-paste0("year_",year_start:year_end)
    for(j in 1:(year_end-year_start+1)){
	mOutput<-matrix(NA,nrow= 17, ncol=2*length(v_countries))
	colnames(mOutput)<-c(rbind(v_country_codes,paste0(v_country_codes,"_100k")))
	rownames(mOutput)<-c(
	    paste0("Population_size_",year_start+j-1), ##row 1
	    "CC", ##row 2
	    paste0("CC_",year_start+j-1), ##row 3
	    "CF", ##row 4
	    paste0("CF_",year_start+j-1), ##row 5
	    paste0("Baseline_mortality_",v_baselineyears[1],"_",v_baselineyears[length(v_baselineyears)]), ##row 6
	    paste0(year_start+j-1,"_mortality"), ##row 7
	    paste0("Fold_change_",year_start+j-1), ##row 8
	    paste0("Excess_",year_start+j-1), ##row 9
	    paste0("Excess-CF_",year_start+j-1), ##row 10
#	    paste0("Baseline_mortality_",v_baselineyears[1],"_",v_baselineyears[length(v_baselineyears)],"_till_",week_end_end_year), ##row 11
#	    paste0(year_start+j-1,"_mortality_till_",week_end_end_year), ##row 12
#	    paste0("Fold_change_",year_start+j-1"_mortality_till_",week_end_end_year), ##row 13
#	    paste0("Excess_",year_start+j-1"_mortality_till_",week_end_end_year), ##row 14
#	    paste0("Excess-CF_",year_start+j-1"_mortality_till_",week_end_end_year), ##row 15
	    paste0("CC_",year_start+j-1,"_from_",week_start_partial,"_till_",week_end_partial), ##row 11
	    paste0("CF_",year_start+j-1,"_from_",week_start_partial,"_till_",week_end_partial), ##row 12
	    paste0("Baseline_mortality_",v_baselineyears[1],"_",v_baselineyears[length(v_baselineyears)],"_from_",week_start_partial,"_till_",week_end_partial), ##row 13
	    paste0(year_start+j-1,"_mortality_from_",week_start_partial,"_till_",week_end_partial), ##row 14
	    paste0("Fold_change_",year_start+j-1,"_mortality_from_",week_start_partial,"_till_",week_end_partial), ##row 15
	    paste0("Excess_",year_start+j-1,"_mortality_from_",week_start_partial,"_till_",week_end_partial), ##row 16
	    paste0("Excess-CF_",year_start+j-1,"_mortality_from_",week_start_partial,"_till_",week_end_partial) ##row 17
	)
	
    
        mOutput[1,]<-mOutput_totmort[[j]][1,] #"Population_size_given_year"

        mOutput[2,]<-mOutput_C19_all[1,] #total "CC"
	mOutput[3,]<-mOutput_totC19[[j]][1,] #"CC_given_year"
    
	mOutput[4,]<-mOutput_C19_all[2,] # toal "CF"
        mOutput[5,]<-mOutput_totC19[[j]][2,] #"CF_given_year"
        

	mOutput[6,]<-mOutput_totmort[[j]][2,]#"Baseline_mortality"
        mOutput[7,]<-mOutput_totmort[[j]][3,]#"given year_mortality"
        mOutput[8,]<-mOutput_totmort[[j]][4,]#"Fold_change_given_year"
        mOutput[9,]<-mOutput_totmort[[j]][5,]#"Excess_given_year"
	mOutput[10,]<-mOutput[9,]-mOutput[5,]#"Excess-CF_given_year",


	mOutput[11,]<-mOutput_partC19[[j]][1,] #"CC_given_year"
	mOutput[12,]<-mOutput_partC19[[j]][2,] #"CF_given_year"
        mOutput[13,]<-mOutput_partmort[[j]][2,]#"Baseline_mortality_from_week_start_partial_till_week_end_partial
	mOutput[14,]<-mOutput_partmort[[j]][3,]#"given_year_mortality_from_week_start_partial_till_week_end_partial
	mOutput[15,]<-mOutput_partmort[[j]][4,]#"Fold_change_given_year_from_week_start_partial_till_week_end_partial
        mOutput[16,]<-mOutput_partmort[[j]][5,]#"Excess_given_year_from_week_start_partial_till_week_end_partial
        mOutput[17,]<-mOutput[16,]-mOutput[12,]#"Excess-CF_given_from_week_start_partial_till_week_end_partial

	l_mOutput[[j]]<-mOutput
    }
    l_mOutput
}


