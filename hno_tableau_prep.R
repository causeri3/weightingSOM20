library(dplyr)

df <- read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/SOM_HNO_PiN_Variables_07102020_DM.csv", head=T, dec=".", sep=",")
df2 <- read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/REACH_SOM2006_JMCNA_IV_Data-Set_August2020_3_clean.csv", head=T, dec=".", sep=",")
#remove funky added on 700k rows since last cleaning
df2=df2[1:10222,]

#for export
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")


#check uuid's
all(df2$X_uuid %in% df$uuid)
length(which((df2$X_uuid %in% df$uuid)))
index<-which(!(df$uuid %in% df2$X_uuid))

#delete uuid's from HNO not in clean data any more
df3<- df[-index,]

#export reduced hno data frame
write.csv(df3, file= paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"reduced_hno_ind.csv"), row.names=FALSE)


#joining in Tableau was so fun, so let's join in R and get rid of doublicated variables
som <- read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/MyOutputs/_2020_Oct_07weighted_data.csv", head=T, dec=".", sep=",")


which( colnames(df)=="Sev..score..Protection.child.separated.1" )
which( colnames(df)=="Sev.score..Access.to.adequate..appropriate.and.functional.sanitation.facilities.1" )
which( colnames(df)=="Sev.score..Access.to.a.sufficient.quantity.of.water.1" )
which( colnames(df)=="Sev.score..Access.to.an.improved.water.source.1" )
which( colnames(df)=="X..of.HHs.having.security.of.tenure.issues.1" )
which( colnames(df)=="X..of.HHs.living.in.inadequate.shelter.conditions..1" )
which( colnames(df)=="X..of.HHs.having.adequate.living.space.1" )

#hno minus double variables
df4<-df3[,-c(24,26,27,29,30,31,33)]

#take relevant informnation from data set (state, regions, weights etc)
som2<-som[,c(1,9,10,11,998,999)]

#join
join<- left_join(som2, df4, by = c("X_uuid" = "uuid"))

#export joined data sets
write.csv(join, file= paste0("C:/Users/Vanessa Causemann/Desktop/REACH/Data/myOutputs/",today,"reduced_joined_hno_ind.csv"), row.names=FALSE)

