## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this package or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .


#f_dopc<-memoise(function(dfdeaths,dfpops,COUNTRY,SEX,v_years){
f_dopc<-function(dfdeaths,dfpops,COUNTRY,SEX,v_years){
    deaths_ct<-dfdeaths[intersect(which(dfdeaths$COUNTRY==COUNTRY),which(dfdeaths$SEX==SEX)),,drop=FALSE]
    pops_ct<-dfpops[intersect(which(dfpops$COUNTRY==COUNTRY),which(dfpops$SEX==SEX)),,drop=FALSE]
    deaths_ct<-cbind(deaths_ct,YEAR=sapply(deaths_ct$TIME,function(x){as.numeric(strsplit(x,"W")[[1]][1])},simplify=TRUE),WEEK=sapply(deaths_ct$TIME,function(x){as.numeric(strsplit(x,"W")[[1]][2])},simplify=TRUE))
    if (length(c(which(deaths_ct$WEEK==99)))>0){
	deaths_ct<-deaths_ct[-c(which(deaths_ct$WEEK==99)),]
    }
    pops_ct<-cbind(pops_ct,YEAR=pops_ct$TIME)
    ag_levels<-unique(pops_ct$AGE)
    v_index_year_keep<-which(deaths_ct$YEAR%in%v_years)

print(COUNTRY)

    if (length(v_index_year_keep)>0){deaths_ct<-deaths_ct[v_index_year_keep,,drop=FALSE]}
    else{stop("Wrong years for study for mortality")}
    v_index_year_keep<-which(pops_ct$YEAR%in%v_years)
    if (length(v_index_year_keep)>0){pops_ct<-pops_ct[v_index_year_keep,,drop=FALSE]}
    else{stop("Wrong years for study for populations")}

    year_levels<-unique(deaths_ct$YEAR)    
    week_levels<-unique(deaths_ct$WEEK)    
    ldeaths_ct_pc<-vector("list",length(ag_levels))
    names(ldeaths_ct_pc)<-ag_levels

    for (i in 1:length(ag_levels)){
	ag_lev<-as.character(ag_levels[[i]])
	ldeaths_ct_pc[[i]]<-list(mdeaths_pc=matrix(NA,ncol=length(year_levels),nrow=length(week_levels)),age_group=ag_lev,min_age=as.numeric(strsplit(as.character(ag_lev),"_")[[1]][1]),max_age=as.numeric(strsplit(as.character(ag_lev),"_")[[1]][2]))
	colnames(ldeaths_ct_pc[[i]]$mdeaths_pc)<-paste0("Y",year_levels)
	rownames(ldeaths_ct_pc[[i]]$mdeaths_pc)<-paste0("W",week_levels)
	for (year_lev in year_levels){
	    year_lev_pop<-year_lev
	    if (year_lev==2021){year_lev_pop<-2020}
	    year_pop<-pops_ct$Value[intersect(which(pops_ct$TIME==year_lev_pop),which(pops_ct$AGE==ag_lev))]
	    if (is.na(year_pop)){ ## UK, did not seem to report population size for 2020 to Eurostat
		year_lev_pop<-year_lev_pop-1
		year_pop<-pops_ct$Value[intersect(which(pops_ct$TIME==year_lev_pop),which(pops_ct$AGE==ag_lev))]
	    }
	    ldeaths_ct_pc[[i]]$mdeaths_pc[paste0("W",deaths_ct$WEEK[intersect(which(deaths_ct$YEAR==year_lev),which(deaths_ct$AGE==ag_lev))]),paste0("Y",year_lev)]<-deaths_ct$Value[intersect(which(deaths_ct$YEAR==year_lev),which(deaths_ct$AGE==ag_lev))]/year_pop
	}
    }
    ldeaths_ct_pc
}
#)

f_create_trajects<-function(ldata,color_traj,bprop_to=FALSE,b_jointargetyears=FALSE,v_weeks_plot=NULL,v_target_years=c("Y2020","Y2021")){
    mdeaths_pc<-ldata$mdeaths_pc
    v_weeks<-sapply(rownames(mdeaths_pc),function(x){as.numeric(strsplit(x,"W")[[1]][2])},simplify=TRUE)

    v_id_target_years<-which(colnames(mdeaths_pc)%in%v_target_years)
    m_to_plot<-matrix(NA,nrow=1+length(v_target_years),ncol=nrow(mdeaths_pc))    
    m_to_plot[1,]<-apply(mdeaths_pc[,-v_id_target_years],1,mean,na.rm=TRUE)
    m_to_plot[2:(length(v_target_years)+1),]<-t(mdeaths_pc[,v_target_years])

    if (bprop_to){
	for (j in 2:(length(v_target_years)+1)){
	    vnotNAs<-which(!is.na(m_to_plot[j,]))
	    m_to_plot[j,vnotNAs]<-m_to_plot[j,vnotNAs]/m_to_plot[1,vnotNAs]#which(!is.na(m_to_plot[2,]))]
	    #m_to_plot[3,which(!is.na(m_to_plot[3,]))]<-m_to_plot[3,which(!is.na(m_to_plot[3,]))]/m_to_plot[1,which(!is.na(m_to_plot[3,]))]
	}
    }
    if (b_jointargetyears){	
	#num_weeks_2021<-length(which(!is.na(m_to_plot[3,])))
	#num_weeks_2020<-length(v_weeks)	
	#if (num_weeks_2021>0){
	#    v_weeks<-c(v_weeks,(num_weeks_2020+1):(num_weeks_2020+num_weeks_2021))
	#	    m_to_plot<-cbind(m_to_plot,matrix(NA,ncol=num_weeks_2021,nrow=3))
	#    m_to_plot[1,(num_weeks_2020+1):(num_weeks_2020+num_weeks_2021)]<-m_to_plot[1,1:num_weeks_2021]
	#    m_to_plot[2,(num_weeks_2020+1):(num_weeks_2020+num_weeks_2021)]<-m_to_plot[3,1:num_weeks_2021]
	#}
	
	num_weeks_first_year<-length(v_weeks)
	num_weeks_joint<-num_weeks_first_year
	for (r in 2:(length(v_target_years))){
	    num_weeks_next_year<-length(which(!is.na(m_to_plot[(r+1),])))
	    if (num_weeks_next_year>0){
		v_weeks<-c(v_weeks,(num_weeks_joint+1):(num_weeks_joint+num_weeks_next_year))
		m_to_plot<-cbind(m_to_plot,matrix(NA,ncol=num_weeks_next_year,nrow=nrow(m_to_plot)))
		m_to_plot[1,(num_weeks_joint+1):(num_weeks_joint+num_weeks_next_year)]<-m_to_plot[1,1:num_weeks_next_year]
	        m_to_plot[2,(num_weeks_joint+1):(num_weeks_joint+num_weeks_next_year)]<-m_to_plot[(r+1),1:num_weeks_next_year]
	        num_weeks_joint<-num_weeks_joint+num_weeks_next_year
	    }
	}
	
	if(!is.null(v_weeks_plot)){
	    v_weeks_plot<-sort(intersect(v_weeks_plot,1:ncol(m_to_plot)))
	    v_weeks<-v_weeks[v_weeks_plot]
	    m_to_plot<-m_to_plot[,v_weeks_plot,drop=FALSE]
	}
	points(v_weeks,m_to_plot[2,],pch=17,type="b",lty=2,col=color_traj)
	m_to_plot<-m_to_plot[2,,drop=FALSE]
    }else{    
	if(!is.null(v_weeks_plot)){
	    v_weeks_plot<-sort(intersect(v_weeks_plot,1:ncol(m_to_plot)))
	    v_weeks<-v_weeks[v_weeks_plot]
	    m_to_plot<-m_to_plot[,v_weeks_plot,drop=FALSE]
	}
	valpch<-17
	vallty<-2
	for (j in 2:(length(v_target_years)+1)){
	    points(v_weeks,m_to_plot[j,],pch=valpch,type="b",lty=vallty,col=color_traj)
	    valpch<-valpch+2
	    vallty<-3
	    #points(v_weeks,m_to_plot[2,],pch=17,type="b",lty=2,col=color_traj)
	    #points(v_weeks,m_to_plot[3,],pch=19,type="b",lty=3,col=color_traj,cex=1.5)
	}
    }
    if (bprop_to){
        abline(h=1,lty=1,col="black",lwd=2)
    }else{
        points(v_weeks,m_to_plot[1,],pch=19,type="l",lty=1,col=color_traj,lwd=2)
    }
    rbind(v_weeks,m_to_plot)
}


v_countries_keep<-l_list_to_use$v_countries_keep
c_outRDatafile<-c_RData_file 

## DO NOT change the two below variables
c_deathsdatasetname<-"demo_r_mweek3"
c_popsdatasetname<-"demo_r_pjangrp3"
## ===================================================================

if (!b_jointargetyears){num_weeks_plot<-55}else{num_weeks_plot<-num_weeks_to_plot}


if (b_read_tsv){
    f_read_Eurostat_tsv(c_deathsfilename=c_deathsfilename,c_popsfilename=c_popsfilename,c_outRDatafile=c_RData_file,b_prepforfurther_country=b_prepforfurther_country,v_regions_keep=v_regions_keep,v_countries_keep=v_countries_keep,c_deathsdatasetname=c_deathsdatasetname,c_popsdatasetname=c_popsdatasetname)
    load(c_RData_file)
}else{
    if (!all(is.element(c("df_EurostatMortalityCounts" , "df_EurostatPopulationCounts"),ls()))){ 
	if ((!is.null(c_RData_file))&&(file.exists(c_RData_file))){load(c_RData_file)}
	else{
	    f_read_Eurostat_tsv(c_deathsfilename=c_deathsfilename,c_popsfilename=c_popsfilename,c_outRDatafile=c_RData_file,b_prepforfurther_country=b_prepforfurther_country,v_regions_keep=v_regions_keep,v_countries_keep=v_countries_keep,c_deathsdatasetname=c_deathsdatasetname,c_popsdatasetname=c_popsdatasetname)	
	    load(c_RData_file)
	}
    }
}

if (b_do_regions){
## A whole country has in COUNTRYREGION the countries code so this will work when trying to compare regions with countries
    df_EurostatMortalityCounts$COUNTRY<-df_EurostatMortalityCounts$COUNTRYREGION
    df_EurostatPopulationCounts$COUNTRY<-df_EurostatPopulationCounts$COUNTRYREGION
}



if (!b_keep_TOTAL){
    if (length(which(df_EurostatMortalityCounts$AGE=="Total"))>0){
	df_EurostatMortalityCounts<-df_EurostatMortalityCounts[-which(df_EurostatMortalityCounts$AGE=="Total"),]
    }
}
if (length(which(is.na(df_EurostatMortalityCounts$AGE)))>0){
    df_EurostatMortalityCounts<-df_EurostatMortalityCounts[-which(is.na(df_EurostatMortalityCounts$AGE)),]
}
if (!b_keep_TOTAL){
    if (length(which(df_EurostatPopulationCounts$AGE=="Total"))>0){
	df_EurostatPopulationCounts<-df_EurostatPopulationCounts[-which(df_EurostatPopulationCounts$AGE=="Total"),]
    }
}
if (length(which(df_EurostatPopulationCounts$AGE=="85_Inf"))>0){
    df_EurostatPopulationCounts<-df_EurostatPopulationCounts[-which(df_EurostatPopulationCounts$AGE=="85_Inf"),]
}
if (length(which(is.na(df_EurostatPopulationCounts$AGE)))>0){
    df_EurostatPopulationCounts<-df_EurostatPopulationCounts[-which(is.na(df_EurostatPopulationCounts$AGE)),]
}


if ((length(v_countries)==1)&&(v_countries=="ALL")){
    v_countries<-intersect(unique(df_EurostatMortalityCounts$COUNTRY),unique(df_EurostatPopulationCounts$COUNTRY))
    if (length(v_countries_rem)>0){v_countries<-setdiff(v_countries,v_countries_rem)}
}else{
    v_countries<-intersect(v_countries,intersect(unique(df_EurostatMortalityCounts$COUNTRY),unique(df_EurostatPopulationCounts$COUNTRY)))
}
if ((length(c_baseline_country)==1)&&is.na(c_baseline_country)){
    c_baseline_country<-v_countries
}
c_baseline_country<-intersect(c_baseline_country,intersect(unique(df_EurostatMortalityCounts$COUNTRY),unique(df_EurostatPopulationCounts$COUNTRY)))
if (length(v_countries_rem)>0){c_baseline_country<-setdiff(c_baseline_country,v_countries_rem)}

if((length(c_baseline_country)==0) || (length(v_countries)==0)){
    stop("No countries to do comparison.")
}

print("Finished preparing, starting to do comparison")
l_CountriesMortalityTimeSeries<-vector("list",length(c_baseline_country))
names(l_CountriesMortalityTimeSeries)<-paste0("baseline_",c_baseline_country)
for (k in 1:length(c_baseline_country)){
    base_country<-c_baseline_country[k]
    if (b_one_by_one){	
	l_CountriesMortalityTimeSeries[[k]]<-vector("list",length(v_countries))
	names(l_CountriesMortalityTimeSeries[[k]])<-v_countries
	for (j in 1:length(v_countries)){
	    cmp_country<-v_countries[j]
	    if (base_country!=cmp_country){
		file_prefix<-paste0(cmp_prefix,base_country,cmp_country)
		dir_name<-paste0(file_prefix,"_",runnum)
		dir.create(paste0("./",dir_name,"/"), showWarnings = FALSE)
		C1_MALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"M",v_years) ;
		C1_FEMALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"F",v_years) ;
		C1_TOT<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"T",v_years)
		
		C2_MALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,cmp_country,"M",v_years);
		C2_FEMALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,cmp_country,"F",v_years);
		C2_TOT<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,cmp_country,"T",v_years)

		ag_levels1<-names(C1_MALE)
		ag_levels2<-names(C2_MALE)
		ag_levels<-intersect(ag_levels1,ag_levels2)
		if (bprop_to){vymax<-vymax_prop}else{vymax<-vymax_pc}
		l_CountriesMortalityTimeSeries[[k]][[j]]<-vector("list",length(ag_levels))
		names(l_CountriesMortalityTimeSeries[[k]][[j]])<-ag_levels
		for (i in 1:length(ag_levels)){
		    ag_lev<-ag_levels[[i]] ;	
		    ymax<-vymax[which(names(vymax)==ag_lev)]
	    	    png_file_name<-paste0("./",dir_name,"/")
	    	    if (bprop_to){png_file_name<-paste0(png_file_name,"prop_")}
	    	    png_file_name<-paste0(png_file_name,dir_name,"_")
	    	    if(ag_lev_name_first){png_file_name<-paste0(png_file_name,ag_lev,"_TOTAL_")}
	    	    else{png_file_name<-paste0(png_file_name,"TOTAL_",ag_lev,"_")}
	    	    png_file_name<-paste0(png_file_name,runnum,".png")
	    	    png(png_file_name)
	    	    #if (bprop_to){png(paste0("./",dir_name,"/","prop_",file_prefix,"_TOTAL","_",ag_lev,"_",runnum,".png"))}else{png(paste0("./",dir_name,"/",file_prefix,"_TOTAL","_",ag_lev,"_",runnum,".png"))}
		    #plot(NA,ylim=c(0,ymax),xlim=c(0,num_weeks_plot),xlab="",ylab="",main=paste0(base_country,cmp_country,"_TOTAL",ag_lev))
		    if (!is.null(v_weeks_plot)){x_to_plot<-c(max(0,min(v_weeks_plot)),min(num_weeks_plot,max(v_weeks_plot)))}
		    else{x_to_plot<-c(0,num_weeks_plot)}
		    
		    if (b_week_no_xaxes_lab){
			plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste0("TOTAL",ag_lev))
		    }else{
			x_to_plot_dates<-lubridate::ymd( "2020-01-01" ) + lubridate::weeks(x_to_plot[1]:x_to_plot[2] - 1 )
			plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste0("TOTAL",ag_lev), xaxt="n")
    			# Add dates to x-axis
			axis(1,at=x_to_plot[1]:x_to_plot[2],labels= format(x_to_plot_dates, "%Y-%m"))
		    }
		    C1_TOT_i<-f_create_trajects(C1_TOT[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
		    C2_TOT_i<-f_create_trajects(C2_TOT[[i]],v_colours[which(names(v_colours)==cmp_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)    
		    legend("topright",legend=v_legend_names[c(which(names(v_colours)==base_country),which(names(v_colours)==cmp_country))],col=v_colours[c(which(names(v_colours)==base_country),which(names(v_colours)==cmp_country))],pch=19,bty="n")
		    dev.off()
	    	    
	    	    png_file_name<-paste0("./",dir_name,"/")
	    	    if (bprop_to){png_file_name<-paste0(png_file_name,"prop_")}
	    	    png_file_name<-paste0(png_file_name,dir_name,"_")
	    	    if(ag_lev_name_first){png_file_name<-paste0(png_file_name,ag_lev,"_MALE_")}
	    	    else{png_file_name<-paste0(png_file_name,"MALE_",ag_lev,"_")}
	    	    png_file_name<-paste0(png_file_name,runnum,".png")
	    	    png(png_file_name)
	    
#	    	    if (bprop_to){png(paste0("./",dir_name,"/","prop_",file_prefix,"_MALE","_",ag_lev,"_",runnum,".png"))}else{png(paste0("./",dir_name,"/",file_prefix,"_MALE","_",ag_lev,"_",runnum,".png"))}
		    #plot(NA,ylim=c(0,ymax),xlim=c(0,num_weeks_plot),xlab="",ylab="",main=paste0(base_country,cmp_country,"_MALE",ag_lev))
		    if (!is.null(v_weeks_plot)){x_to_plot<-c(max(0,min(v_weeks_plot)),min(num_weeks_plot,max(v_weeks_plot)))}
		    else{x_to_plot<-c(0,num_weeks_plot)}
		    if (b_week_no_xaxes_lab){
			plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste0("MALE",ag_lev))
		    }else{
			x_to_plot_dates<-lubridate::ymd( "2020-01-01" ) + lubridate::weeks(x_to_plot[1]:x_to_plot[2] - 1 )
			plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste0("MALE",ag_lev), xaxt="n")
    			# Add dates to x-axis
			axis(1,at=x_to_plot[1]:x_to_plot[2],labels= format(x_to_plot_dates, "%Y-%m"))
		    }
		    C1_M_i<-f_create_trajects(C1_MALE[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
		    C2_M_i<-f_create_trajects(C2_MALE[[i]],v_colours[which(names(v_colours)==cmp_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)    
		    legend("topright",legend=v_legend_names[c(which(names(v_colours)==base_country),which(names(v_colours)==cmp_country))],col=v_colours[c(which(names(v_colours)==base_country),which(names(v_colours)==cmp_country))],pch=19,bty="n")
		    dev.off()

	    	    png_file_name<-paste0("./",dir_name,"/")
	    	    if (bprop_to){png_file_name<-paste0(png_file_name,"prop_")}
	    	    png_file_name<-paste0(png_file_name,dir_name,"_")
	    	    if(ag_lev_name_first){png_file_name<-paste0(png_file_name,ag_lev,"_FEMALE_")}
	    	    else{png_file_name<-paste0(png_file_name,"FEMALE_",ag_lev,"_")}
	    	    png_file_name<-paste0(png_file_name,runnum,".png")
	    	    png(png_file_name)

	    	    #if (bprop_to){png(paste0("./",dir_name,"/","prop_",file_prefix,"_FEMALE","_",ag_lev,"_",runnum,".png"))}else{png(paste0("./",dir_name,"/",file_prefix,"_FEMALE","_",ag_lev,"_",runnum,".png"))}
#		    plot(NA,ylim=c(0,ymax),xlim=c(0,num_weeks_plot),xlab="",ylab="",main=paste0(base_country,cmp_country,"_FEMALE",ag_lev))
		    if (!is.null(v_weeks_plot)){x_to_plot<-c(max(0,min(v_weeks_plot)),min(num_weeks_plot,max(v_weeks_plot)))}
		    else{x_to_plot<-c(0,num_weeks_plot)}
		    if (b_week_no_xaxes_lab){
			plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste0("FEMALE",ag_lev))
		    }else{
			x_to_plot_dates<-lubridate::ymd( "2020-01-01" ) + lubridate::weeks(x_to_plot[1]:x_to_plot[2] - 1 )
			plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste0("FEMALE",ag_lev), xaxt="n")
    			# Add dates to x-axis
			axis(1,at=x_to_plot[1]:x_to_plot[2],labels= format(x_to_plot_dates, "%Y-%m"))
		    }
		    C1_F_i<-f_create_trajects(C1_FEMALE[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
		    C2_F_i<-f_create_trajects(C2_FEMALE[[i]],v_colours[which(names(v_colours)==cmp_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)    
		    legend("topright",legend=v_legend_names[c(which(names(v_colours)==base_country),which(names(v_colours)==cmp_country))],col=v_colours[c(which(names(v_colours)==base_country),which(names(v_colours)==cmp_country))],pch=19,bty="n")
		    dev.off()
		    l_CountriesMortalityTimeSeries[[k]][[j]][[i]]<-vector("list",6)
		    names(l_CountriesMortalityTimeSeries[[k]][[j]][[i]])<-c(paste0(base_country,"_TOTAL"),paste0(cmp_country,"_TOTAL"),paste0(base_country,"_MALE"),paste0(cmp_country,"_MALE"),paste0(base_country,"_FEMALE"),paste0(cmp_country,"_FEMALE"))
		    l_CountriesMortalityTimeSeries[[k]][[j]][[i]][[1]]<-C1_TOT_i
		    l_CountriesMortalityTimeSeries[[k]][[j]][[i]][[2]]<-C2_TOT_i
		    l_CountriesMortalityTimeSeries[[k]][[j]][[i]][[3]]<-C1_M_i
		    l_CountriesMortalityTimeSeries[[k]][[j]][[i]][[4]]<-C2_M_i
		    l_CountriesMortalityTimeSeries[[k]][[j]][[i]][[5]]<-C1_F_i
		    l_CountriesMortalityTimeSeries[[k]][[j]][[i]][[6]]<-C2_F_i
		}
	    }
	}
    }else{
	v_countries<-setdiff(v_countries,base_country)
	dir_name<-paste0(cmp_prefix,paste(c(base_country,v_countries,runnum),collapse="_"))
	dir.create(paste0("./",dir_name,"/"), showWarnings = FALSE)	
	Cb_MALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"M",v_years)
	Cb_FEMALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"F",v_years)
	Cb_TOT<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"T",v_years)
	ag_levels<-names(Cb_MALE)
	if (bprop_to){vymax<-vymax_prop}else{vymax<-vymax_pc}		
	l_CountriesMortalityTimeSeries[[k]]<-vector("list",length(ag_levels))
	names(l_CountriesMortalityTimeSeries[[k]])<-ag_levels
	for (i in 1:length(ag_levels)){
	    l_CountriesMortalityTimeSeries[[k]][[i]]<-vector("list",length(v_countries)+1)
	    names(l_CountriesMortalityTimeSeries[[k]][[i]])<-c(base_country,v_countries)
	    for(j in 1:length(l_CountriesMortalityTimeSeries[[k]][[i]])){
		    l_CountriesMortalityTimeSeries[[k]][[i]][[j]]<-vector("list",3)
		    names(l_CountriesMortalityTimeSeries[[k]][[i]][[j]])<-c(paste0(names(l_CountriesMortalityTimeSeries[[k]][[i]])[j],"_TOTAL"),paste0(names(l_CountriesMortalityTimeSeries[[k]][[i]])[j],"_MALE"),paste0(names(l_CountriesMortalityTimeSeries[[k]][[i]])[j],"_FEMALE"))		
	    }

	    ag_lev<-ag_levels[[i]] ;	
	    ymax<-vymax[which(names(vymax)==ag_lev)]

    	    png_file_name<-paste0("./",dir_name,"/")
    	    if (bprop_to){png_file_name<-paste0(png_file_name,"prop_")}
    	    png_file_name<-paste0(png_file_name,dir_name,"_")
    	    if(ag_lev_name_first){png_file_name<-paste0(png_file_name,ag_lev,"_TOTAL_")}
    	    else{png_file_name<-paste0(png_file_name,"TOTAL",ag_lev,"_")}
    	    png_file_name<-paste0(png_file_name,runnum,".png")
    	    png(png_file_name)

	    #if (bprop_to){png(paste0("./",dir_name,"/","prop_",dir_name,"_TOTAL",ag_lev,"_",runnum,".png"))}else{png(paste0("./",dir_name,"/",dir_name,"_TOTAL",ag_lev,"_",runnum,".png"))}
	    ##plot(NA,ylim=c(0,ymax),xlim=c(0,num_weeks_plot),xlab="",ylab="",main=paste(base_country,v_countries,"_TOTAL",ag_lev,collapse=""))
	    if (!is.null(v_weeks_plot)){x_to_plot<-c(max(0,min(v_weeks_plot)),min(num_weeks_plot,max(v_weeks_plot)))}
	    else{x_to_plot<-c(0,num_weeks_plot)}
	    if (b_week_no_xaxes_lab){
	    	plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste("TOTAL",ag_lev,collapse=""))
	    }else{
		x_to_plot_dates<-lubridate::ymd( "2020-01-01" ) + lubridate::weeks(x_to_plot[1]:x_to_plot[2] - 1 )
		plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste("TOTAL",ag_lev,collapse=""), xaxt="n")
		# Add dates to x-axis
		axis(1,at=x_to_plot[1]:x_to_plot[2],labels= format(x_to_plot_dates, "%Y-%m"))
	    }
	    l_CountriesMortalityTimeSeries[[k]][[i]][[1]][[1]]<-f_create_trajects(Cb_TOT[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
	    if (length(v_countries)>0){
		for (j in 1:length(v_countries)){
		## This is ineffectve as we call f_dopc many times, if takes too long memoise ths function
		    Cc_TOT<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,v_countries[j],"T",v_years)
		    Cc_ag_levels<-names(Cc_TOT)
		    if (ag_lev %in% Cc_ag_levels){
			l_CountriesMortalityTimeSeries[[k]][[i]][[j+1]][[1]]<-f_create_trajects(Cc_TOT[[i]],v_colours[which(names(v_colours)==v_countries[j])],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
		    }
		}
	    }
	    legend("topright",legend=v_legend_names,col=v_colours,pch=19,bty="n")
	    dev.off()

    	    png_file_name<-paste0("./",dir_name,"/")
    	    if (bprop_to){png_file_name<-paste0(png_file_name,"prop_")}
    	    png_file_name<-paste0(png_file_name,dir_name,"_")
    	    if(ag_lev_name_first){png_file_name<-paste0(png_file_name,ag_lev,"_MALE_")}
    	    else{png_file_name<-paste0(png_file_name,"MALE",ag_lev,"_")}
    	    png_file_name<-paste0(png_file_name,runnum,".png")
    	    png(png_file_name)

	    #if (bprop_to){png(paste0("./",dir_name,"/","prop_",dir_name,"_MALE",ag_lev,"_",runnum,".png"))}else{png(paste0("./",dir_name,"/",dir_name,"_MALE",ag_lev,"_",runnum,".png"))}
	    ##plot(NA,ylim=c(0,ymax),xlim=c(0,num_weeks_plot),xlab="",ylab="",main=paste(base_country,v_countries,"_MALE",ag_lev,collapse=""))
	    if (!is.null(v_weeks_plot)){x_to_plot<-c(max(0,min(v_weeks_plot)),min(num_weeks_plot,max(v_weeks_plot)))}
	    else{x_to_plot<-c(0,num_weeks_plot)}
	    if (b_week_no_xaxes_lab){
		plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste("MALE",ag_lev,collapse=""))
	    }else{
		x_to_plot_dates<-lubridate::ymd( "2020-01-01" ) + lubridate::weeks(x_to_plot[1]:x_to_plot[2] - 1 )
		plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste("MALE",ag_lev,collapse=""), xaxt="n")
		# Add dates to x-axis
		axis(1,at=x_to_plot[1]:x_to_plot[2],labels= format(x_to_plot_dates, "%Y-%m"))
	    }
	    l_CountriesMortalityTimeSeries[[k]][[i]][[1]][[2]]<-f_create_trajects(Cb_MALE[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
	    if (length(v_countries)>0){
		for (j in 1:length(v_countries)){
		## This is ineffectve as we call f_dopc many times, if takes too long memoise ths function
		    Cc_MALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,v_countries[j],"M",v_years)
		    Cc_ag_levels<-names(Cc_MALE)
		    if (ag_lev %in% Cc_ag_levels){
			l_CountriesMortalityTimeSeries[[k]][[i]][[j+1]][[2]]<-f_create_trajects(Cc_MALE[[i]],v_colours[which(names(v_colours)==v_countries[j])],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
		    }
		}
	    }
	    legend("topright",legend=v_legend_names,col=v_colours,pch=19,bty="n")
	    dev.off()

    	    png_file_name<-paste0("./",dir_name,"/")
    	    if (bprop_to){png_file_name<-paste0(png_file_name,"prop_")}
    	    png_file_name<-paste0(png_file_name,dir_name,"_")
    	    if(ag_lev_name_first){png_file_name<-paste0(png_file_name,ag_lev,"_FEMALE_")}
    	    else{png_file_name<-paste0(png_file_name,"FEMALE",ag_lev,"_")}
    	    png_file_name<-paste0(png_file_name,runnum,".png")
    	    png(png_file_name)

	    #if (bprop_to){png(paste0("./",dir_name,"/","prop_",dir_name,"_FEMALE",ag_lev,"_",runnum,".png"))}else{png(paste0("./",dir_name,"/",dir_name,"_FEMALE",ag_lev,"_",runnum,".png"))}
##	    plot(NA,ylim=c(0,ymax),xlim=c(0,num_weeks_plot),xlab="",ylab="",main=paste(base_country,v_countries,"_FEMALE",ag_lev,collapse=""))
	    if (!is.null(v_weeks_plot)){x_to_plot<-c(max(0,min(v_weeks_plot)),min(num_weeks_plot,max(v_weeks_plot)))}
	    else{x_to_plot<-c(0,num_weeks_plot)}
	    if (b_week_no_xaxes_lab){
		plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste("FEMALE",ag_lev,collapse=""))
	    }else{
		x_to_plot_dates<-lubridate::ymd( "2020-01-01" ) + lubridate::weeks(x_to_plot[1]:x_to_plot[2] - 1 )
		plot(NA,ylim=c(0,ymax),xlim=x_to_plot,xlab="",ylab="",main=paste("FEMALE",ag_lev,collapse=""), xaxt="n")
		# Add dates to x-axis
		axis(1,at=x_to_plot[1]:x_to_plot[2],labels= format(x_to_plot_dates, "%Y-%m"))
	    }
	    l_CountriesMortalityTimeSeries[[k]][[i]][[1]][[3]]<-f_create_trajects(Cb_FEMALE[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
	    if (length(v_countries)>0){
		for (j in 1:length(v_countries)){
		## This is ineffectve as we call f_dopc many times, if takes too long memoise ths function
		    Cc_FEMALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,v_countries[j],"F",v_years)
		    Cc_ag_levels<-names(Cc_FEMALE)
		    if (ag_lev %in% Cc_ag_levels){
			l_CountriesMortalityTimeSeries[[k]][[i]][[j+1]][[3]]<-f_create_trajects(Cc_FEMALE[[i]],v_colours[which(names(v_colours)==v_countries[j])],bprop_to,b_jointargetyears,v_weeks_plot,v_target_years)
		    }
		}
	    }
	    legend("topright",legend=v_legend_names,col=v_colours,pch=19,bty="n")
	    dev.off()
	}
    }
}


