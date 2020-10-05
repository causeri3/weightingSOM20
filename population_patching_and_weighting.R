
library(dplyr)
library(writexl)

#add date for export later
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

#import data set
df <- read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/MyOutputs/_2020_Oct_05_clean_data4.csv", head=T, dec=".", sep=",")

#import population data HC (OCHA pre-war settlement list)
pop <-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/PopulationNumbers/Population_per_settlement.csv", head=T, dec=".", sep=",")
colnames(pop)[8]<-"district"
pop$district<-tolower(pop$district)


#IDP Population, DSA 2019
pop_idp <-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/PopulationNumbers/master_lastupdate28072019_idp.csv", head=T, dec=".", sep=",")
colnames(pop_idp)[3]<-"district"
pop_idp$district<-tolower(pop_idp$district)

#IDP population CCCM 2020
pop_idp2 <-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/PopulationNumbers/IDP Site Master List - 28 Sep 2020.csv", head=T, dec=".", sep=",")
colnames(pop_idp2)[1]<-"district"
pop_idp2$district<-tolower(pop_idp2$district)


#change names of districts
pop_idp$district[pop_idp$district=="badhan "]<-"laasqoray"
pop_idp$district[pop_idp$district=="mogadishu_wardhiigleey" | pop_idp$district=="mogadishu_hawl_wadaag" | pop_idp$district=="mogadishu_hamar_jaab_jab" | pop_idp$district=="mogadishu_abdulaziz" | pop_idp$district=="mogadishu_karaan" | pop_idp$district=="mogadishu_shibis" | pop_idp$district=="mogadishu_waaberi" | pop_idp$district=="mogadishu_wadajir" | pop_idp$district=="mogadishu_hamar_weyne" | pop_idp$district=="mogadishu_heliwa" | pop_idp$district=="mogadishu_yaaqshiid" | pop_idp$district=="mogadishu_boondheere" | pop_idp$district=="mogadishu_shangaani"]<-"banadir_other"
pop_idp$district[pop_idp$district=="mogadishu_daynile"]<-"banadir_daynile"
pop_idp$district[pop_idp$district=="mogadishu_dharkenley"]<-"banadir_dharkenley"
pop_idp$district[pop_idp$district=="mogadishu_hodan"]<-"banadir_hodan"
pop_idp$district[pop_idp$district=="mogadishu_kahda"]<-"banadir_kahda"
pop_idp$district[pop_idp$district=="gaalkacyo north" | pop_idp$district=="gaalkacyo south"] <- "gaalkacyo"

pop$district[pop$district=="mogadishu"]<-"banadir_hc"

#check for differently spelled districts
dis<-unique(df$district[df$idp_settlement=="no"])
pop_dis<-unique(pop$district)
all(dis %in% pop_dis)
which(!(dis %in% pop_dis))
dis[which(!(dis %in% pop_dis))]

dis_idp<-unique(df$district[df$idp_settlement=="yes"])
pop_idp_dis<-unique(pop_idp$district)
all(dis_idp %in% pop_idp_dis)
which(!(dis_idp %in% pop_idp_dis))
dis_idp[which(!(dis_idp %in% pop_idp_dis))]

pop_idp_dis2<-unique(pop_idp2$district)
missing_dsa<-dis_idp[which(!(dis_idp %in% pop_idp_dis))]
missing_dsa[which(!(missing_dsa %in% pop_idp_dis2))]

#reformat HC population data into new data frame
pp = pop %>% select(district,Population)
pp = pp %>% group_by(district) %>% summarise(Population = sum(Population))
pp$type<-"HC"
pp$source<-"OCHA pre-war settlement list"

#fix empty entries of population with estimate family*6=individuals
na_index<-which(is.na(pop_idp$number_individuals))
pop_idp$number_individuals[na_index]<- pop_idp$number_families[na_index]*6

#reformat IDP population data into new data frame
pp_idp = pop_idp %>% select(district,number_individuals)
pp_idp = pp_idp %>% group_by(district) %>% summarise(number_individuals = sum(number_individuals))
pp_idp$number_individuals<-round(pp_idp$number_individuals)
colnames(pp_idp)[2]<-"Population"
pp_idp$type<-"IDP"
pp_idp$source<-"DSA IDP Site Master List 2019"


#check which districts for IPD are over 30 surveys and not covered by DSA 2019
dis_idp[which(!(dis_idp %in% pop_idp_dis))]
length(df$hh_size[df$district=="baki" & df$idp_settlement=="IDP Site"])
#need: baki

pop_idp2$Population[pop_idp2$district=="baki"]
#"8.388"
#add baki from DSA 2020 to IDP population data 
pp_idp[nrow(pp_idp) + 1,1] = "baki"
pp_idp[nrow(pp_idp),2] = 8388
pp_idp[nrow(pp_idp),3] = "IDP"
pp_idp[nrow(pp_idp),4] = "DSA IDP Site Master List 2020"


#export patched population to document it
pop_all <- rbind(pp, pp_idp)
write_xlsx(pop_all, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/population_patching_for_weigthing",today,".xlsx"))


#start the weighting

total_surv<-dim(df)[1]
total_pop<-(sum(pp$Population[which(pp$district %in% dis)])+ sum(pp_idp$Population[which(pp_idp$district %in% dis_idp)]))


df$weights<-0

#for idp
for (i in 1:length(pp_idp$district))
{
  index_idp<-which(df$district==pp_idp$district[i] & df$idp_settlement=="yes")
  df$weights[index_idp]<-((pp_idp$Population[i]/total_pop)/(length(df$hh_size[df$district==pp_idp$district[i] & df$idp_settlement=="yes"])/total_surv))
}


#for non-idp
for (i in 1:length(pp$district))
{
  index_hc<-which(df$district==pp$district[i] & df$idp_settlement=="no")
  df$weights[index_hc]<-((pp$Population[i]/total_pop)/(length(df$hh_size[df$district==pp$district[i] & df$idp_settlement=="no"])/total_surv))
}


write.csv(df, file= paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"weighted_data.csv"), row.names=FALSE)
write_xlsx(df, paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"weighted_data.xlsx"))