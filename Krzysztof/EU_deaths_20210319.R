# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_mweek3
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_pjangrp3&lang=en
# https://stackoverflow.com/questions/33322248/how-to-import-a-tsv-file
# https://ec.europa.eu/eurostat/web/population-demography-migration-projections/data/database
## population counts have some e, b, notes next to values
## some countries do not have all the age levels

library(memoise)

runnum<-"01"
cmp_prefix<-""
bprop_to<-TRUE
b_read_tsv<-FALSE
v_countries<-"ALL"
v_countries_rem<-c("DE") ## something is wrong with DE, they do not seem to report deaths by age groups
c_baseline_country<-c("PL","SE") ##NA if not NA than loop pairs in v_countries
#c_baseline_country<-c("SE","PL") ##NA if not NA than loop pairs in v_countries
b_one_by_one<-TRUE

## countries without mortality
##> setdiff(unique(df_EurostatPopulationCounts$COUNTRY),unique(df_EurostatMortalityCounts$COUNTRY))
##[1] "IE" "MK" "TR"
## v_all_countries<-intersect(unique(df_EurostatPopulationCounts$COUNTRY),unique(df_EurostatMortalityCounts$COUNTRY))

v_colours<-rep("green",34) ## v_colours<-rep("green",length(v_all_countries))
v_colours[31]<-"blue" ##SE v_colours[which(v_all_countries=="SE")]<-"blue"
v_colours[27]<-"red" ##PL v_colours[which(v_all_countries=="PL")]<-"red"
names(v_colours)<-c("AL", "AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IS", "IT", "LI", "LT", "LU", "LV", "ME", "MT", "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SI", "SK", "UK")
## names(v_colours)<-v_all_countries
#v_colours<-c("green","red","blue")
#names(v_colours)<-c("DE","PL","SE")

v_years<-2015:2021
vymax_pc<-c("0_4"=0.00005,    "5_9"=0.00001,    "10_14"=0.00001,  "15_19"=0.00005,  "20_24"=0.00005,  "25_29"=0.00005,  
"30_34"=0.00005,  "35_39"=0.0001, "40_44"=0.00012,  "45_49"=0.0002,  "50_54"=0.0003,  "55_59"=0.0005,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.002,  "75_79"=0.003,  "80_84"=0.005,  "85_89"=0.01,  "90_Inf"=0.02)

vymax_prop<-c("0_4"=5,    "5_9"=11,    "10_14"=15,  "15_19"=11,  "20_24"=11,  "25_29"=11,  
"30_34"=11,  "35_39"=11, "40_44"=3,  "45_49"=3,  "50_54"=3,  "55_59"=3,  "60_64"=3, "65_69"=3,  
"70_74"=3,  "75_79"=3,  "80_84"=3,  "85_89"=3,  "90_Inf"=3)


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
	ag_lev<-ag_levels[[i]]
	ldeaths_ct_pc[[i]]<-list(mdeaths_pc=matrix(NA,ncol=length(year_levels),nrow=length(week_levels)),age_group=ag_lev,min_age=as.numeric(strsplit(ag_lev,"_")[[1]][1]),max_age=as.numeric(strsplit(ag_lev,"_")[[1]][2]))
	colnames(ldeaths_ct_pc[[i]]$mdeaths_pc)<-paste0("Y",year_levels)
	rownames(ldeaths_ct_pc[[i]]$mdeaths_pc)<-paste0("W",week_levels)

	for (year_lev in year_levels){
	    year_lev_pop<-year_lev
	    if (year_lev==2021){year_lev_pop<-2020}
	    year_pop<-pops_ct$Value[intersect(which(pops_ct$TIME==year_lev_pop),which(pops_ct$AGE==ag_lev))]
	    ldeaths_ct_pc[[i]]$mdeaths_pc[paste0("W",deaths_ct$WEEK[intersect(which(deaths_ct$YEAR==year_lev),which(deaths_ct$AGE==ag_lev))]),paste0("Y",year_lev)]<-deaths_ct$Value[intersect(which(deaths_ct$YEAR==year_lev),which(deaths_ct$AGE==ag_lev))]/year_pop
	}
    }
    ldeaths_ct_pc
}
#)

f_create_trajects<-function(ldata,color_traj,bprop_to=FALSE){
    mdeaths_pc<-ldata$mdeaths_pc
    v_weeks<-sapply(rownames(mdeaths_pc),function(x){as.numeric(strsplit(x,"W")[[1]][2])},simplify=TRUE)
    m_to_plot<-matrix(NA,nrow=3,ncol=nrow(mdeaths_pc))
    m_to_plot[1,]<-apply(mdeaths_pc[,-c(ncol(mdeaths_pc)-1,ncol(mdeaths_pc))],1,mean,na.rm=TRUE)
    m_to_plot[2:3,]<-t(mdeaths_pc[,c("Y2020","Y2021")])
    if (bprop_to){
	m_to_plot[2,]<-m_to_plot[2,]/m_to_plot[1,]#which(!is.na(m_to_plot[2,]))]
	m_to_plot[3,which(!is.na(m_to_plot[3,]))]<-m_to_plot[3,which(!is.na(m_to_plot[3,]))]/m_to_plot[1,which(!is.na(m_to_plot[3,]))]
    }
    points(v_weeks,m_to_plot[2,],pch=17,type="b",lty=2,col=color_traj)
    points(v_weeks,m_to_plot[3,],pch=19,type="b",lty=3,col=color_traj,cex=1.5)
    if (bprop_to){
	abline(h=1,lty=1,col="black",lwd=2)
    }else{
        points(v_weeks,m_to_plot[1,],pch=19,type="l",lty=1,col=color_traj,lwd=2)
    }
}


if (b_read_tsv){source("read_Eurostat_tsv.R")
}else{load("EurostatMortalityPopulations.RData")}

if (length(which(df_EurostatMortalityCounts$AGE=="Total"))>0){
    df_EurostatMortalityCounts<-df_EurostatMortalityCounts[-which(df_EurostatMortalityCounts$AGE=="Total"),]
}
if (length(which(is.na(df_EurostatMortalityCounts$AGE)))>0){
    df_EurostatMortalityCounts<-df_EurostatMortalityCounts[-which(is.na(df_EurostatMortalityCounts$AGE)),]
}
if (length(which(df_EurostatPopulationCounts$AGE=="Total"))>0){
    df_EurostatPopulationCounts<-df_EurostatPopulationCounts[-which(df_EurostatPopulationCounts$AGE=="Total"),]
}
if (length(which(df_EurostatPopulationCounts$AGE=="85_Inf"))>0){
    df_EurostatPopulationCounts<-df_EurostatPopulationCounts[-which(df_EurostatPopulationCounts$AGE=="85_Inf"),]
}
if (length(which(is.na(df_EurostatPopulationCounts$AGE)))>0){
    df_EurostatPopulationCounts<-df_EurostatPopulationCounts[-which(is.na(df_EurostatPopulationCounts$AGE)),]
}


if (v_countries=="ALL"){
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
for (k in 1:length(c_baseline_country)){
    base_country<-c_baseline_country[k]
    if (b_one_by_one){
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
		for (i in 1:length(ag_levels)){
		    ag_lev<-ag_levels[[i]] ;	
		    ymax<-vymax[which(names(vymax)==ag_lev)]
	    	    if (bprop_to){
	    		png(paste0("./",dir_name,"/","prop_",file_prefix,"_TOTAL","_",ag_lev,"_",runnum,".png"))
	    	    }else{
			png(paste0("./",dir_name,"/",file_prefix,"_TOTAL","_",ag_lev,"_",runnum,".png"))
		    }
		    plot(NA,ylim=c(0,ymax),xlim=c(0,55),xlab="",ylab="",main=paste0(base_country,cmp_country,"_TOTAL",ag_lev))
		    f_create_trajects(C1_TOT[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to)
		    f_create_trajects(C2_TOT[[i]],v_colours[which(names(v_colours)==cmp_country)],bprop_to)    
		    dev.off()
	    	    if (bprop_to){
	    	        png(paste0("./",dir_name,"/","prop_",file_prefix,"_MALE","_",ag_lev,"_",runnum,".png"))
	    	    }else{
			png(paste0("./",dir_name,"/",file_prefix,"_MALE","_",ag_lev,"_",runnum,".png"))
		    }
		    plot(NA,ylim=c(0,ymax),xlim=c(0,55),xlab="",ylab="",main=paste0(base_country,cmp_country,"_MALE",ag_lev))
		    f_create_trajects(C1_MALE[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to)
		    f_create_trajects(C2_MALE[[i]],v_colours[which(names(v_colours)==cmp_country)],bprop_to)    
		    dev.off()
	    	    if (bprop_to){
	    	        png(paste0("./",dir_name,"/","prop_",file_prefix,"_FEMALE","_",ag_lev,"_",runnum,".png"))
	    	    }else{
			png(paste0("./",dir_name,"/",file_prefix,"_FEMALE","_",ag_lev,"_",runnum,".png"))
		    }
		    plot(NA,ylim=c(0,ymax),xlim=c(0,55),xlab="",ylab="",main=paste0(base_country,cmp_country,"_FEMALE",ag_lev))
		    f_create_trajects(C1_FEMALE[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to)
		    f_create_trajects(C2_FEMALE[[i]],v_colours[which(names(v_colours)==cmp_country)],bprop_to)    
		    dev.off()
		}
	    }
	}
    }else{
	v_countries<-setdiff(v_countries,base_country)
	file_prefix<-paste("./",cmp_prefix,base_country,v_countries,runnum,collapse="")
	dir.create(file_prefix, showWarnings = FALSE)	
	Cb_MALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"M",v_years)
	Cb_FEMALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"F",v_years)
	Cb_TOT<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,base_country,"T",v_years)
	ag_levels<-names(CbMALE)
	if (bprop_to){vymax<-vymax_prop}else{vymax<-vymax_pc}		
	for (i in 1:length(ag_levels)){
	    ag_lev<-ag_levels[[i]] ;	
	    ymax<-vymax[which(names(vymax)==ag_lev)]
	    if (bprop_to){
		png(paste0("./",file_prefix,"/","prop_",file_prefix,"_TOTAL",ag_lev,"_",runnum,".png"))
	    }else{
		png(paste0("./",file_prefix,"/",file_prefix,"_TOTAL",ag_lev,"_",runnum,".png"))
	    }
	    plot(NA,ylim=c(0,ymax),xlim=c(0,55),xlab="",ylab="",main=paste(base_country,v_countries,"_TOTAL",ag_lev,collapse=""))
	    f_create_trajects(Cb_TOT[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to)
	    if (length(v_countries)>0){
		for (j in 1:length(v_countries)){
		## This is ineffectve as we call f_dopc many times, if takes too long memoise ths function
		    Cc_TOT<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,v_countries[j],"T",v_years)
		    Cc_ag_levels<-names(Cc_TOT)
		    if (ag_levele %in% Cc_ag_levels){
			f_create_trajects(Cc_TOT[[i]],v_colours[which(names(v_colours)==v_countries[j])],bprop_to)
		    }
		}
	    }
	    dev.off()
	    if (bprop_to){
		png(paste0("./",file_prefix,"/","prop_",file_prefix,"_MALE",ag_lev,"_",runnum,".png"))
	    }else{
		png(paste0("./",file_prefix,"/",file_prefix,"_MALE",ag_lev,"_",runnum,".png"))
	    }
	    plot(NA,ylim=c(0,ymax),xlim=c(0,55),xlab="",ylab="",main=paste(base_country,v_countries,"_MALE",ag_lev,collapse=""))
	    f_create_trajects(Cb_MALE[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to)
	    if (length(v_countries)>0){
		for (j in 1:length(v_countries)){
		## This is ineffectve as we call f_dopc many times, if takes too long memoise ths function
		    Cc_MALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,v_countries[j],"M",v_years)
		    Cc_ag_levels<-names(Cc_MALE)
		    if (ag_levele %in% Cc_ag_levels){
			f_create_trajects(Cc_MALE[[i]],v_colours[which(names(v_colours)==v_countries[j])],bprop_to)
		    }
		}
	    }
	    dev.off()
	    if (bprop_to){
		png(paste0("./",file_prefix,"/","prop_",file_prefix,"_FEMALE",ag_lev,"_",runnum,".png"))
	    }else{
		png(paste0("./",file_prefix,"/",file_prefix,"_FEMALE",ag_lev,"_",runnum,".png"))
	    }
	    plot(NA,ylim=c(0,ymax),xlim=c(0,55),xlab="",ylab="",main=paste(base_country,v_countries,"_FEMALE",ag_lev,collapse=""))
	    f_create_trajects(Cb_MALE[[i]],v_colours[which(names(v_colours)==base_country)],bprop_to)
	    if (length(v_countries)>0){
		for (j in 1:length(v_countries)){
		## This is ineffectve as we call f_dopc many times, if takes too long memoise ths function
		    Cc_FEMALE<-f_dopc(df_EurostatMortalityCounts,df_EurostatPopulationCounts,v_countries[j],"F",v_years)
		    Cc_ag_levels<-names(Cc_FEMALE)
		    if (ag_levele %in% Cc_ag_levels){
			f_create_trajects(Cc_FEMALE[[i]],v_colours[which(names(v_colours)==v_countries[j])],bprop_to)
		    }
		}
	    }
	    dev.off()
	}
    }
}


