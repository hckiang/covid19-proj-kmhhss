## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this package or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

# <-list(
# v_regions_keep=,
# c_RData_file=,
# v_countries=,
# v_countries_rem=c("DE"),
# c_baseline_country=,
# v_colours=,
# v_names_colours=,
# v_legend_names=,
# vymax_pc=,
# vymax_prop=,
# )


l_PL91PL82PL414PL415PL219PL823<-list(
v_regions_keep=c("PL91","PL82","PL414","PL415","PL219","PL823"),
c_RData_file="EurostatMortalityPopulations_20210906_PL_Wojew_01.RData",
v_countries=c("PL91","PL82","PL414","PL415","PL219","PL823"),
v_countries_rem=c("DE"),
c_baseline_country=c("PL82"),
v_colours=c("blue","red","green","black","pink","orange"),
v_names_colours=c("PL91","PL82","PL414","PL415","PL219","PL823"),
v_legend_names=c("Warszawa","Podkarpacie","Koninski","Poznan","Nowotarski","Rzeszow"),
vymax_pc=c("0_4"=0.0001,    "5_9"=0.00005,    "10_14"=0.00005,  "15_19"=0.0001,  "20_24"=0.0001,  "25_29"=0.0001,  
"30_34"=0.0001,  "35_39"=0.0001, "40_44"=0.00012,  "45_49"=0.0002,  "50_54"=0.0002,  "55_59"=0.0006,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.003,  "75_79"=0.006,  "80_84"=0.01,  "85_89"=0.015,  "90_Inf"=0.025, "0_Inf"=0.0005),
vymax_prop=c("0_4"=15,    "5_9"=20,    "10_14"=15,  "15_19"=20,  "20_24"=15,  "25_29"=15,  
"30_34"=30,  "35_39"=20, "40_44"=10,  "45_49"=10,  "50_54"=10,  "55_59"=10,  "60_64"=10, "65_69"=10,  
"70_74"=10,  "75_79"=10,  "80_84"=10,  "85_89"=10,  "90_Inf"=10, "0_Inf"=6)
)


l_PL82PL414PL415PL219<-list(
v_regions_keep=c("PL91","PL82","PL414","PL415","PL219","PL823"),
c_RData_file="EurostatMortalityPopulations_20210906_PL_Wojew_01.RData",
v_countries=c("PL414","PL415","PL219"),
v_countries_rem=c("DE"),
c_baseline_country=c("PL82"),
v_colours=c("green","black","pink"),
v_names_colours=c("PL414","PL415","PL219"),
v_legend_names=c("Koninski","Poznan","Nowotarski"),
vymax_pc=c("0_4"=0.0001,    "5_9"=0.00005,    "10_14"=0.00005,  "15_19"=0.0001,  "20_24"=0.0001,  "25_29"=0.0001,  
"30_34"=0.0001,  "35_39"=0.0001, "40_44"=0.00012,  "45_49"=0.0002,  "50_54"=0.0002,  "55_59"=0.0006,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.003,  "75_79"=0.006,  "80_84"=0.01,  "85_89"=0.015,  "90_Inf"=0.025, "0_Inf"=0.0005),
vymax_prop=c("0_4"=15,    "5_9"=20,    "10_14"=15,  "15_19"=20,  "20_24"=15,  "25_29"=15,  
"30_34"=30,  "35_39"=20, "40_44"=10,  "45_49"=10,  "50_54"=10,  "55_59"=10,  "60_64"=10, "65_69"=10,  
"70_74"=10,  "75_79"=10,  "80_84"=10,  "85_89"=10,  "90_Inf"=10, "0_Inf"=6)
)


l_SE11SE123SE232SE224UKIFR1ITC4<-list(
v_regions_keep=c("CH","SE11","SE123","SE232","SE224","UKI","FR101","ITC4","ES300","PT17","PL911","ITI43","SE","UK","FR","IT","ES","PT","PL","CZ"),
c_RData_file="EurostatMortalityPopulations_20210906_RegionsToSTHSE.RData",
v_countries=c("SE123","SE232","SE224","UKI","FR1","ITC4") ,
v_countries_rem=c("DE"),
c_baseline_country=c("SE11"),
v_colours=c("blue","green","red","black","brown","orange","yellow"),
v_names_colours=c("SE11","SE123","SE232","SE224","UKI","FR1","ITC4"),
v_legend_names=c("Stockholm","OGot","VGot","Skane","London","Paris","Lombardy"),
vymax_pc=c("0_4"=0.0001,    "5_9"=0.00005,    "10_14"=0.00005,  "15_19"=0.0001,  "20_24"=0.0001,  "25_29"=0.0001,  
"30_34"=0.0001,  "35_39"=0.0001, "40_44"=0.00012,  "45_49"=0.0002,  "50_54"=0.0002,  "55_59"=0.0006,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.003,  "75_79"=0.006,  "80_84"=0.01,  "85_89"=0.015,  "90_Inf"=0.025, "0_Inf"=0.0005),
vymax_prop=c("0_4"=15,    "5_9"=20,    "10_14"=15,  "15_19"=20,  "20_24"=15,  "25_29"=15,  
"30_34"=30,  "35_39"=20, "40_44"=10,  "45_49"=10,  "50_54"=10,  "55_59"=10,  "60_64"=10, "65_69"=10,  
"70_74"=10,  "75_79"=10,  "80_84"=10,  "85_89"=10,  "90_Inf"=10, "0_Inf"=6)
)

l_SE11FR101ITC4ES300PT17PL911ITI43<-list(
v_regions_keep=c("CH","SE11","SE123","SE232","SE224","UKI","FR101","ITC4","ES300","PT17","PL911","ITI43","SE","UK","FR","IT","ES","PT","PL","CZ"),
c_RData_file="EurostatMortalityPopulations_20210906_RegionsToSTHSE.RData",
v_countries=c("FR101","ITC4","ES300","PT17","PL911","ITI43"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE11"),
v_colours=c("blue","red","black","orange","yellow","brown","pink"),
v_names_colours=c("SE11","FR101","ITC4","ES300","PT17","PL911","ITI43"),
v_legend_names=c("Stockholm","Paris","Lombardy","Madrid","Lisbon","Warsaw","Rome"),
vymax_pc=c("0_4"=0.0001,    "5_9"=0.00005,    "10_14"=0.00005,  "15_19"=0.0001,  "20_24"=0.0001,  "25_29"=0.0001,  
"30_34"=0.0001,  "35_39"=0.0001, "40_44"=0.00012,  "45_49"=0.0002,  "50_54"=0.0002,  "55_59"=0.0006,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.003,  "75_79"=0.006,  "80_84"=0.01,  "85_89"=0.015,  "90_Inf"=0.025, "0_Inf"=0.0005),
vymax_prop=c("0_4"=15,    "5_9"=20,    "10_14"=15,  "15_19"=20,  "20_24"=15,  "25_29"=15,  
"30_34"=30,  "35_39"=20, "40_44"=10,  "45_49"=10,  "50_54"=10,  "55_59"=10,  "60_64"=10, "65_69"=10,  
"70_74"=10,  "75_79"=10,  "80_84"=10,  "85_89"=10,  "90_Inf"=10, "0_Inf"=6)
)


l_SE11UKI<-list(
v_regions_keep=c("CH","SE11","SE123","SE232","SE224","UKI","FR101","ITC4","ES300","PT17","PL911","ITI43","SE","UK","FR","IT","ES","PT","PL","CZ"),
c_RData_file="EurostatMortalityPopulations_20210906_RegionsToSTHSE.RData",
v_countries=c("UKI"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE11"),
v_colours=c("blue","red"),
v_names_colours=c("SE11","UKI"),
v_legend_names=c("Stockholm","London"),
vymax_pc=c("0_4"=0.0001,    "5_9"=0.00005,    "10_14"=0.00005,  "15_19"=0.0001,  "20_24"=0.0001,  "25_29"=0.0001,  
"30_34"=0.0001,  "35_39"=0.0001, "40_44"=0.00012,  "45_49"=0.0002,  "50_54"=0.0002,  "55_59"=0.0006,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.003,  "75_79"=0.006,  "80_84"=0.01,  "85_89"=0.015,  "90_Inf"=0.025, "0_Inf"=0.0005),
vymax_prop=c("0_4"=15,    "5_9"=20,    "10_14"=15,  "15_19"=20,  "20_24"=15,  "25_29"=15,  
"30_34"=30,  "35_39"=20, "40_44"=10,  "45_49"=10,  "50_54"=10,  "55_59"=10,  "60_64"=10, "65_69"=10,  
"70_74"=10,  "75_79"=10,  "80_84"=10,  "85_89"=10,  "90_Inf"=10, "0_Inf"=6)
)

l_SE11FR101ITC4ES300PT17UKI<-list(
v_regions_keep=c("CH","SE11","SE123","SE232","SE224","UKI","FR101","ITC4","ES300","PT17","PL911","ITI43","SE","UK","FR","IT","ES","PT","PL","CZ"),
c_RData_file="EurostatMortalityPopulations_20210906_RegionsToSTHSE.RData",
v_countries=c("FR101","ITC4","ES300","PT17","UKI"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE11"),
v_colours=c("blue","red","black","brown","pink","orange"),
v_names_colours=c("SE11","FR101","ITC4","ES300","PT17","UKI"),
v_legend_names=c("Stockholm","Paris","Lombardy","Madrid","Lisbon","London"),
vymax_pc=c("0_4"=0.0005,    "5_9"=0.00005,    "10_14"=0.00005,  "15_19"=0.0001,  "20_24"=0.0001,  "25_29"=0.0001,  
"30_34"=0.0001,  "35_39"=0.0001, "40_44"=0.0002,  "45_49"=0.0002,  "50_54"=0.0005,  "55_59"=0.0006,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.003,  "75_79"=0.006,  "80_84"=0.01,  "85_89"=0.015,  "90_Inf"=0.025, "0_Inf"=0.001),
vymax_prop=c("0_4"=15,    "5_9"=20,    "10_14"=15,  "15_19"=20,  "20_24"=15,  "25_29"=15,  
"30_34"=30,  "35_39"=20, "40_44"=15,  "45_49"=10,  "50_54"=5,  "55_59"=5,  "60_64"=5, "65_69"=6,  
"70_74"=8,  "75_79"=10,  "80_84"=10,  "85_89"=10,  "90_Inf"=10, "0_Inf"=6.2)
)

l_SE11FR101ITC4UKI<-list(
v_regions_keep=c("CH","SE11","SE123","SE232","SE224","UKI","FR101","ITC4","ES300","PT17","PL911","ITI43","SE","UK","FR","IT","ES","PT","PL","CZ"),
c_RData_file="EurostatMortalityPopulations_20210906_RegionsToSTHSE.RData",
v_countries=c("FR101","ITC4","UKI"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE11"), ## or SE110
v_colours=c("blue","red","black","orange"),
v_names_colours=c("SE11","FR101","ITC4","UKI"),
v_legend_names=c("Stockholm","Paris","Lombardy","London"),
vymax_pc=c("0_4"=0.0005,    "5_9"=0.00005,    "10_14"=0.00005,  "15_19"=0.0001,  "20_24"=0.0001,  "25_29"=0.0001,  
"30_34"=0.0001,  "35_39"=0.0001, "40_44"=0.0002,  "45_49"=0.0002,  "50_54"=0.0005,  "55_59"=0.0006,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.003,  "75_79"=0.006,  "80_84"=0.01,  "85_89"=0.015,  "90_Inf"=0.025, "0_Inf"=0.001),
vymax_prop=c("0_4"=15,    "5_9"=20,    "10_14"=15,  "15_19"=20,  "20_24"=15,  "25_29"=15,  
"30_34"=30,  "35_39"=20, "40_44"=15,  "45_49"=10,  "50_54"=5,  "55_59"=5,  "60_64"=5, "65_69"=6,  
"70_74"=8,  "75_79"=10,  "80_84"=10,  "85_89"=10,  "90_Inf"=10, "0_Inf"=6.2)
)

l_SE11ITC4<-list(
v_regions_keep=c("CH","SE11","SE123","SE232","SE224","UKI","FR101","ITC4","ES300","PT17","PL911","ITI43","SE","UK","FR","IT","ES","PT","PL","CZ"),
c_RData_file="EurostatMortalityPopulations_20210906_RegionsToSTHSE.RData",
v_countries=c("ITC4"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE11"),
v_colours=c("blue","black"),
v_names_colours=c("SE11","ITC4"),
v_legend_names=c("Stockholm","Lombardy"),
vymax_pc=c("0_4"=0.0005,    "5_9"=0.00005,    "10_14"=0.00005,  "15_19"=0.0001,  "20_24"=0.0001,  "25_29"=0.0001,  
"30_34"=0.0001,  "35_39"=0.0001, "40_44"=0.0002,  "45_49"=0.0002,  "50_54"=0.0005,  "55_59"=0.0006,  "60_64"=0.001, "65_69"=0.002,  
"70_74"=0.003,  "75_79"=0.006,  "80_84"=0.01,  "85_89"=0.015,  "90_Inf"=0.025, "0_Inf"=0.001),
vymax_prop=c("0_4"=15,    "5_9"=20,    "10_14"=15,  "15_19"=20,  "20_24"=15,  "25_29"=15,  
"30_34"=30,  "35_39"=20, "40_44"=15,  "45_49"=10,  "50_54"=5,  "55_59"=5,  "60_64"=5, "65_69"=6,  
"70_74"=8,  "75_79"=10,  "80_84"=10,  "85_89"=10,  "90_Inf"=10, "0_Inf"=6.2)
)


l_SEFRITESPTUK<-list(
v_regions_keep=c("SE","UK","FR","IT","ES","PT","PL","CZ"),
c_RData_file="EurostatMortalityPopulations_20210906_CountriesToSE.RData",
v_countries=c("FR","IT","ES","PT","UK"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE"),
v_colours=c("blue","red","black","brown","pink","orange"),
v_names_colours=c("SE","FR","IT","ES","PT","UK"),
v_legend_names=c("Sweden","France","Italy","Spain","Portugal","UK"),
vymax_pc=c("0_4"=0.00006,    "5_9"=0.000015,    "10_14"=0.000015,  "15_19"=0.00004,  "20_24"=0.00003,  "25_29"=0.00004,  
"30_34"=0.00005,  "35_39"=0.00005, "40_44"=0.00008,  "45_49"=0.00012,  "50_54"=0.0002,  "55_59"=0.0003,  "60_64"=0.0006, "65_69"=0.001,  
"70_74"=0.0015,  "75_79"=0.003,  "80_84"=0.005,  "85_89"=0.01,  "90_Inf"=0.02, "0_Inf"=0.0006),
vymax_prop=c("0_4"=6,    "5_9"=15,    "10_14"=20,  "15_19"=15,  "20_24"=10,  "25_29"=6,  
"30_34"=8,  "35_39"=5, "40_44"=3,  "45_49"=3,  "50_54"=3,  "55_59"=3,  "60_64"=3, "65_69"=3,  
"70_74"=3,  "75_79"=3,  "80_84"=3,  "85_89"=3,  "90_Inf"=3, "0_Inf"=3)
)


l_PLSE<-list(
v_regions_keep=c("PL","SE"),
c_RData_file="EurostatMortalityPopulations_20210906_CountriesToSE.RData",
v_countries=v_countries<- c("PL"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE"),
v_colours=c("blue","red"),
v_names_colours=c("SE","PL"),
v_legend_names=c("Sweden","Poland"),
vymax_pc=c("0_4"=0.00003,    "5_9"=0.000015,    "10_14"=0.000015,  "15_19"=0.00004,  "20_24"=0.00003,  "25_29"=0.00004,  
"30_34"=0.00005,  "35_39"=0.00008, "40_44"=0.0001,  "45_49"=0.00017,  "50_54"=0.0005,  "55_59"=0.0005,  "60_64"=0.001, "65_69"=0.0015,  
"70_74"=0.002,  "75_79"=0.005,  "80_84"=0.005,  "85_89"=0.01,  "90_Inf"=0.015, "0_Inf"=0.0006),
vymax_prop=c("0_4"=4,    "5_9"=10,    "10_14"=15,  "15_19"=15,  "20_24"=6,  "25_29"=4,  
"30_34"=10,  "35_39"=5, "40_44"=3,  "45_49"=3,  "50_54"=3,  "55_59"=2,  "60_64"=2, "65_69"=2,  
"70_74"=2.5,  "75_79"=2.5,  "80_84"=3,  "85_89"=3,  "90_Inf"=3, "0_Inf"=3)
)


l_PLCHSE<-list(
v_regions_keep=c("PL","CH","SE"),
c_RData_file="EurostatMortalityPopulations_20210906_WithCH.RData",
v_countries=c("SE","PL","CH"),
v_country_codes=c("SE","PL","CH"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE"),
v_colours=c("blue","red","green"),
v_names_colours=c("SE","PL","CH"),
v_legend_names=c("Sweden","Poland","Switzerland"),
vymax_pc=c("0_4"=0.00003,    "5_9"=0.000015,    "10_14"=0.000015,  "15_19"=0.00004,  "20_24"=0.00003,  "25_29"=0.00004,  
"30_34"=0.00005,  "35_39"=0.00008, "40_44"=0.0001,  "45_49"=0.00017,  "50_54"=0.0005,  "55_59"=0.0005,  "60_64"=0.001, "65_69"=0.0015,  
"70_74"=0.002,  "75_79"=0.005,  "80_84"=0.005,  "85_89"=0.01,  "90_Inf"=0.015, "0_Inf"=0.0006),
vymax_prop=c("0_4"=4,    "5_9"=10,    "10_14"=15,  "15_19"=15,  "20_24"=6,  "25_29"=4,  
"30_34"=10,  "35_39"=5, "40_44"=3,  "45_49"=3,  "50_54"=3,  "55_59"=2,  "60_64"=2, "65_69"=2,  
"70_74"=2.5,  "75_79"=2.5,  "80_84"=3,  "85_89"=3,  "90_Inf"=3, "0_Inf"=3)
)

l_SECH<-list(
v_regions_keep=c("CH","SE11","SE123","SE232","SE224","UKI","FR101","ITC4","ES300","PT17","PL911","ITI43","SE","UK","FR","IT","ES","PT","PL","CZ"),
c_RData_file="EurostatMortalityPopulations_20210906_WithCH.RData",
v_countries=c("CH"),
v_countries_rem=c("DE"),
c_baseline_country=c("SE"),
v_colours=c("blue","green"),
v_names_colours=c("SE","CH"),
v_legend_names=c("Sweden","Switzerland"),
vymax_pc=c("0_4"=0.00005,    "5_9"=0.000015,    "10_14"=0.00002,  "15_19"=0.00004,  "20_24"=0.00005,  
"25_29"=0.00004,  
"30_34"=0.00005,  "35_39"=0.000005, "40_44"=0.00005,  "45_49"=0.00005,  "50_54"=0.0001,  "55_59"=0.0002,  
"60_64"=0.0003, "65_69"=0.0005,  
"70_74"=0.001,  "75_79"=0.002,  "80_84"=0.0025,  "85_89"=0.006,  "90_Inf"=0.015, "0_Inf"=0.0003),
vymax_prop=c("0_4"=4,    "5_9"=10,    "10_14"=15,  "15_19"=20,  "20_24"=6,  "25_29"=4,  
"30_34"=10,  "35_39"=5, "40_44"=5,  "45_49"=5,  "50_54"=3,  "55_59"=2.2,  "60_64"=2, "65_69"=2,  
"70_74"=2,  "75_79"=2,  "80_84"=2.5,  "85_89"=2.5,  "90_Inf"=2.5, "0_Inf"=2)
)

## countries without mortality
##> setdiff(unique(df_EurostatPopulationCounts$COUNTRY),unique(df_EurostatMortalityCounts$COUNTRY))
##[1] "IE" "MK" "TR"
# v_all_countries<-intersect(unique(df_EurostatPopulationCounts$COUNTRY),unique(df_EurostatMortalityCounts$COUNTRY))
# lAllvSEPL<-list(
# v_regions_keep=NA,
# c_RData_file=NA,
# v_countries="ALL",
# v_countries_rem=c("DE"),
# c_baseline_country=c("SE","PL"),
# v_colours=rep("green",34),
# v_names_colours=v_all_countries,
# ##v_names_colours<-c("AL", "AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IS", "IT", "LI", "LT", "LU", "LV", "ME", "MT", "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SI", "SK", "UK")
# v_legend_names=NA,
# vymax_pc=NA,
# vymax_prop=NA,
# )
# lAllvSEPL$v_colours[31]<-"blue" ##SE v_colours[which(v_all_countries=="SE")]<-"blue"
# lAllvSEPL$v_colours[27]<-"red" ##PL v_colours[which(v_all_countries=="PL")]<-"red"


#l_list_to_use<-l_PLCHSE
#l_list_to_use<-l_PLSE
l_list_to_use<-l_SECH


