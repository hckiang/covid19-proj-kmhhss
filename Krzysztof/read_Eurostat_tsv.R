c_deathsfilename<-"../Eurostat_Deaths_20210317_tsv"
c_deathsdatasetname<-"demo_r_mweek3"
c_popsfilename<-"../Eurostat_Populations_20210319_tsv"
c_popsdatasetname<-"demo_r_pjangrp3"



f_readEurostattsv<-function(filename,dataset,b_prepforfurther_country,b_keepge85=FALSE,b_rev_time_columns=TRUE){
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_mweek3 ## weekly regional level mortality by age and sex
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_pjangrp3 ## annual regional level population size by age and sex
# https://ec.europa.eu/eurostat/web/population-demography-migration-projections/data/database

        init_dataset <- read.table(filename, skip=1,na.strings = ": ",sep="\t")
        first4cols<-t(apply(init_dataset[,1,drop=FALSE],1,function(x){strsplit(x,',')[[1]]}))
        init_dataset<-init_dataset[,-1,drop=FALSE]
	if (dataset=="demo_r_pjangrp3"){ ## annual regional level population size by age and sex
	    first4cols[,c(1,2)]<-first4cols[,c(2,1),drop=FALSE] ## Here we have SEX,NR in mortality NR,SEX
	    if (!b_keepge85){
     		v_yge85 <-which(first4cols[,3]=="Y_GE85")
     		if (length(v_yge85)>0){
     		    first4cols <-first4cols[-v_yge85,,drop=FALSE]
     		    init_dataset <-init_dataset[-v_yge85,,drop=FALSE]
     		}
     	    }
         }
         #first4cols[,2]<-replace(first4cols[,2],first4cols[,2]=="M","MALE)
         ## sex levels are M,F,T
         ## create age levels as I have in analyses code
         first4cols[,3]<-replace(first4cols[,3],first4cols[,3]=="UNK",NA)
         first4cols[,3]<-sapply(first4cols[,3],function(x){
            if (!is.na(x)){
     		if (x=="Y_GE90"){x<-"90_Inf"}
     		else if (x=="Y_LT5"){x<-"0_4"}
     		else if (x=="TOTAL"){x<-"Total"}
     		else if (x=="Y_GE85"){x<-"85_Inf"}
     		else{
     		    y<-strsplit(x,"-")[[1]]
     		    y2<-y[2]
     		    y1<-strsplit(y[1],"Y")[[1]][2]
     		    x<-paste0(y1,"_",y2)
     		}    
     	    }
     	    x
     	},simplify=TRUE)
	first4cols<-cbind(first4cols,t(apply(first4cols[,4,drop=FALSE],1,function(x){z<-strsplit(x,"")[[1]];y<-c(paste(z[1:2],collapse=""),paste(z[-c(1:2)],collapse=""));y})))
	first4cols<-first4cols[,-1,drop=FALSE]

        if (b_prepforfurther_country){
     	    v_regions<-which(first4cols[,5]!="")
     	    if (length(v_regions)>0){
     		init_dataset<-init_dataset[-v_regions,,drop=FALSE]
     		first4cols<-first4cols[-v_regions,,drop=FALSE]
     	    }
     	    print("Done removing regional data")
        }

	if ((dataset=="demo_r_mweek3")||(dataset=="demo_r_pjangrp3")){ ## weekly regional level mortality by age and sex
     	## remove the p in mortality counts
     	## e,b in population counts
     	#HERE WE  GET WARNING  In FUN(newX[, i], ...) : NAs introduced by coercion
     	    print("Doing apply to remove p or other letters")
     	    init_dataset<-apply(init_dataset,c(1,2),function(x){if(!is.na(x)){x<-strsplit(x," ")[[1]][1]};as.numeric(x)})
     	    print("Done removing p")
        }

         df_colnames <- strsplit(readLines(filename, n=1),"\t")[[1]][-1]
         df_colnames<- sapply(df_colnames,function(x){strsplit(x," ")[[1]][1]},simplify=TRUE)
         names(df_colnames)<-NULL
         if(b_rev_time_columns){
	     init_dataset <- init_dataset[,ncol(init_dataset):1,drop=FALSE] ## reverse order of colums so that earliest week is first, i.e. we are chronological
             df_colnames<-rev(df_colnames)## reverse order of colums so that earliest week is first, i.e. we are chronological
    	 }
    	 ## no changes seem necessary if else?
    	 ## else{init_dataset <- init_dataset[,1:ncol(init_dataset),drop=FALSE]}
    	 
    	 df_colnames<-c("SEX","AGE","COUNTRYREGION","COUNTRY","REGION",df_colnames) 
         df_dataset<-cbind(first4cols,init_dataset)
         colnames(df_dataset)<-df_colnames
	if (b_prepforfurther_country){
#	v_regions<-which(df_dataset[,"REGION"]!="")
#	if (length(v_regions)>0){
#	    df_dataset<-df_dataset[-v_regions,]
#	}
	    orig_df_dataset<-df_dataset
	##df_dataset<-cbind(df_dataset[,c("SEX","AGE","COUNTRYREGION","COUNTRY","REGION")],)
	    tmp_df_dataset<-cbind(df_dataset[,c("SEX","AGE","COUNTRY"),drop=FALSE],"TIME"=NA,"Value"=NA)
	    rownames(tmp_df_dataset)<-NULL
	    n_obs<-nrow(orig_df_dataset)
	    ntime_periods<-ncol(orig_df_dataset)-5
	    df_dataset<-tmp_df_dataset
	    for (i in 2:ntime_periods){df_dataset<-rbind(df_dataset,tmp_df_dataset)}
	    df_dataset<-as.data.frame(df_dataset)
	    class(df_dataset$Value)<-"numeric"
	    df_dataset$TIME<-c(sapply(colnames(orig_df_dataset)[6:ncol(orig_df_dataset)],function(x,k){rep(x,k)},k=n_obs,simplify=TRUE))
	    for (i in 6:ncol(orig_df_dataset)){
		df_dataset$Value[((i-5-1)*n_obs+1):((i-5)*n_obs)]<-sapply(orig_df_dataset[,i],function(x){as.numeric(x)},simplify=TRUE)
	    }
	}
    df_dataset
}
 

options(warn=2)
df_EurostatPopulationCounts<-f_readEurostattsv(c_popsfilename,c_popsdatasetname,b_prepforfurther_country=TRUE,b_keepge85=TRUE)
print("Done pops")
df_EurostatMortalityCounts<-f_readEurostattsv(c_deathsfilename,c_deathsdatasetname,b_prepforfurther_country=TRUE,b_keepge85=TRUE)
options(warn=1)

save(df_EurostatPopulationCounts,df_EurostatMortalityCounts,file="EurostatMortalityPopulations.RData")
