# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Ferraro and Hirst 2024 
#Date last updated: May 15 2024
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

#Packages ----
library(sf)
library(ggplot2)
library(tidyverse)
library(rgeos)
library(rgdal)
library(maptools)
library(raster)
library(spatialEco)
library(dplyr)
library(ggpubr)
library(spatialEco)
library(dplyr)



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Data Input-----

#CSV with the Annual Deer Cull Data from the Deer Working Group Report 2019
DCG_Count_Data <- read.csv("Annual Deer Cull info by count area.csv")

#2020 Scotland Habitat Map from Space Intelligence 
Scotland_Raster <- raster("HLCM_2020_SCOTLAND.tif")

#Deer Count Area Shape File from Nature Scot
DCG_Shapefile<-st_read("DCS_DEER_COUNT_AREA.shp")

#Atmospheric Deposition Data from Tomlinson et al. 2021
AtmosphericN<- read.csv("ASSIST_N_dep_kgha_2017.csv")


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Habitat Extraction----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

#= = = = = = 
#.Shapefile of DCAs merged with Space Intelligence ----
#= = = = = = 

#Create a shapefile that merge the DCA shapefile with Space Intelligence
DCG_Data_Short<-unique(DCG_Count_Data[c("COUNTAREANAME", "AREATYPE", "SPECIES")])
DCG_Data_Short<-complete(
  DCG_Data_Short,
  COUNTAREANAME,
  nesting(SPECIES, AREATYPE),)
DCG_Shapefile <- DCG_Shapefile %>%
  mutate(AREA_NAME = recode(AREA_NAME, "Gairloch Conservation Uni" = 'Gairloch Conservation Unit' ))
DCG_Shapefile <- DCG_Shapefile %>%
  mutate(AREA_NAME = recode(AREA_NAME, "Tummel" = 'North Tummel' ))

DCGs_FromPoly<-sort(unique(DCG_Shapefile$AREA_NAME))
DCGs_FromData<-sort(unique(DCG_Data_Short $COUNTAREANAME))
Missing_From_DCGs_FromData<-subset(DCGs_FromPoly, !(DCGs_FromPoly %in% DCGs_FromData))
#Note, "East Ross" "Edinburgh City"   "West Sutherland (Ardvar)" are in the shapefile but not reported in the data 

DCG_Clip<-subset(DCG_Shapefile, (AREA_NAME %in% DCG_Data_Short$COUNTAREANAME))
DCG_Clip <- merge(DCG_Clip, DCG_Data_Short, by.x = 'AREA_NAME', by.y = 'COUNTAREANAME', all.x = TRUE)
DCG_Clip <- subset(DCG_Clip, AREATYPE == "Woodland")
DCG_Clip <- subset(DCG_Clip,SPECIES == "Red")
DCG_Clip <- DCG_Clip[,c(1, 7, 15)]

DCG_Clip_Merged<-DCG_Clip %>% 
  group_by(AREA_NAME) %>%
  summarise(Sum_HECTARES = sum(HECTARES)) 


DCG_Clip_Merged <- shapefile("DCG_Clip_Merged.shp")
DCG_Clip_Merged_pjr <- spTransform(DCG_Clip_Merged, crs(Scotland_Raster))
DCG_Clip_Merged_pjr$AREA_NA <- as.factor(DCG_Clip_Merged_pjr$AREA_NA)


#= = = = = = 
#.Extract Woodland, Ag, and Openrange habitats from each DCA ----
#= = = = = = 
DCG <- unique(DCG_Clip_Merged$AREA_NA)
DCG_Raster_Habitat_Areas<-list()
Output<-list()
for (i in 1:length(DCG)) {
  DCG_Clip<-subset(DCG_Clip_Merged_pjr, AREA_NA == DCG[i],)
  #  DCG_Clip<-subset(DCG_Clip_Merged_pjr, AREA_NA == "Aberdeenshire")
  clipped_raster <- crop(Scotland_Raster, extent(DCG_Clip))
  clipped_raster[is.na(clipped_raster[])] <- 0 # HERE WE ARE CHANGING TRUE NAs TO 0
  DCG_raster <- mask(clipped_raster, DCG_Clip)
  value_freq <- freq(DCG_raster)
  value_freq <- value_freq[rowSums(is.na(value_freq)) == 0, ] 
  res_raster <- res(DCG_raster)
  n_cells <- sum(value_freq)
  cell_area <- res_raster[1] * res_raster[2]
  total_area_m2 <- n_cells * cell_area
  total_area_ha <- total_area_m2 /10000
  DCG_Name<- DCG[i]
  DCG.df<-as.data.frame(values(DCG_raster))
  Woodland_ha<-nrow(DCG.df %>%
                      filter_all(all_vars(. %in% c(14, 15, 16, 17)))) * cell_area / 10000
  OpenRange_ha<-nrow(DCG.df %>%
                       #  filter_all(all_vars(. %in% c(2, 12, 5, 10, 8, 7, 9, 3)))) * cell_area / 10000
                       filter_all(all_vars(. %in% c(12)))) * cell_area / 10000
  Ag_ha<-nrow(DCG.df %>%
                filter_all(all_vars(. %in% c(20, 6)))) * cell_area / 10000
  # total_WOA_ha<-Woodland_ha + OpenRange_ha + Ag_ha
  Output<- cbind(DCG_Name, total_area_ha, Woodland_ha, OpenRange_ha, Ag_ha)
  DCG_Raster_Habitat_Areas<- rbind(DCG_Raster_Habitat_Areas, Output )
}

#


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Nutrient Models----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

#= = = = = = 
#.Get the area of each habitat----
#= = = = = = 

DCG_Count_Data <- read.csv("/Users/kristyferraro/My Drive/Scholarship/Yale/Projects/Scotland Carcass Nutreint Removal/Data/Annual Deer Cull info by count area.csv")

DCG_Count_Data <- DCG_Count_Data[ -which(names(DCG_Count_Data) %in% c("CULLSEASON")) ]
DCG_Count_Data<-DCG_Count_Data[!(DCG_Count_Data$Year_starting < 2010) ,]
#Data just from 2010
DCG_Count_Data<-complete(
  DCG_Count_Data,
  COUNTAREANAME,
  nesting(SPECIES, Year_starting, AREATYPE),
  fill = list( Male_IN  = 0, Male_OUT = 0,Female_IN = 0, Female_OUT = 0, Y_ANY = 0, TOTAL = 0))

DCG_Count_Data<-subset(DCG_Count_Data, COUNTAREANAME != "None")

DCG_Woodland_Data <- subset(DCG_Count_Data, AREATYPE == "Woodland")
DCG_Woodland_Data <- merge(DCG_Woodland_Data, DCG_Raster_Habitat_Areas, by.x = 'COUNTAREANAME', by.y = 'DCG_Name', all.x = TRUE)
DCG_Woodland_Data<-DCG_Woodland_Data[ ,c(1:10, 12) ]
DCG_Woodland_Data <- DCG_Woodland_Data %>%  rename("Habitat_Area" = "Woodland_ha")

DCG_OpenRange_Data <-subset(DCG_Count_Data, AREATYPE == "Open Range")
DCG_OpenRange_Data <- merge(DCG_OpenRange_Data, DCG_Raster_Habitat_Areas, by.x = 'COUNTAREANAME', by.y = 'DCG_Name', all.x = TRUE)
DCG_OpenRange_Data<-DCG_OpenRange_Data[ ,c(1:10, 13) ]
DCG_OpenRange_Data <- DCG_OpenRange_Data %>%  rename("Habitat_Area" = "OpenRange_ha")

DCG_Agriculture_Data <-subset(DCG_Count_Data, AREATYPE == "Agriculture")
DCG_Agriculture_Data <- merge(DCG_Agriculture_Data, DCG_Raster_Habitat_Areas, by.x = 'COUNTAREANAME', by.y = 'DCG_Name', all.x = TRUE)
DCG_Agriculture_Data<-DCG_Agriculture_Data[ ,c(1:10, 14) ]
DCG_Agriculture_Data <- DCG_Agriculture_Data %>%  rename("Habitat_Area" = "Ag_ha")

DCG_Count_Averages_Raster<-rbind(DCG_Woodland_Data,DCG_OpenRange_Data , DCG_Agriculture_Data)
DCG_Count_Averages_Raster<-as.data.frame(DCG_Count_Averages_Raster)

#= = = = = = 
#.Correcting for under counting of carcasses from Deer Working Group Report   
#= = = = = = 

DCG_Count_Averages_Corrected_Raster<-as.data.frame(DCG_Count_Averages_Raster)
#Removing the data that isnt found in a given DCG
DCG_Count_Averages_Corrected_Raster<-DCG_Count_Averages_Corrected_Raster[!(DCG_Count_Averages_Corrected_Raster$COUNTAREANAME=="None"),]
DCG_Count_Averages_Corrected_Raster<-DCG_Count_Averages_Corrected_Raster %>% 
  mutate(Male_IN = ifelse(SPECIES == "Red", Male_IN / .9, Male_IN),
         Male_OUT = ifelse(SPECIES == "Red", Male_OUT / .9, Male_OUT),
         Female_IN = ifelse(SPECIES == "Red", Female_IN / .9, Female_IN),
         Female_OUT = ifelse(SPECIES == "Red", Female_OUT / .9, Female_OUT),
         Y_ANY = ifelse(SPECIES == "Red", Y_ANY / .9, Y_ANY),
         Male_IN = ifelse(SPECIES == "Roe", Male_IN / .4, Male_IN),
         Male_OUT = ifelse(SPECIES == "Roe", Male_OUT / .4, Male_OUT),
         Female_IN = ifelse(SPECIES == "Roe", Female_IN / .4, Female_IN),
         Female_OUT = ifelse(SPECIES == "Roe", Female_OUT / .4, Female_OUT),
         Y_ANY = ifelse(SPECIES == "Roe", Y_ANY / .4, Y_ANY),
         Male_IN = ifelse(SPECIES == "Sika", Male_IN / .75, Male_IN),
         Male_OUT = ifelse(SPECIES == "Sika", Male_OUT / .75, Male_OUT),
         Female_IN = ifelse(SPECIES == "Sika", Female_IN / .75, Female_IN),
         Female_OUT = ifelse(SPECIES == "Sika", Female_OUT / .75, Female_OUT),
         Y_ANY = ifelse(SPECIES == "Sika", Y_ANY / .75, Y_ANY),
         Male_IN = ifelse(SPECIES == "Fallow", Male_IN / .75, Male_IN),
         Male_OUT = ifelse(SPECIES == "Fallow", Male_OUT / .75, Male_OUT),
         Female_IN = ifelse(SPECIES == "Fallow", Female_IN / .75, Female_IN),
         Female_OUT = ifelse(SPECIES == "Fallow", Female_OUT / .75, Female_OUT),
         Y_ANY = ifelse(SPECIES == "Fallow", Y_ANY / .75, Y_ANY),)

DCG_Count_Averages_Corrected_Raster$TOTAL<-rowSums(DCG_Count_Averages_Corrected_Raster[,c("Male_IN", "Male_OUT", "Female_IN", "Female_OUT", "Y_ANY" )])   

DCG_Count_Averages_Corrected_Raster$Habitat_Area<-as.numeric(DCG_Count_Averages_Corrected_Raster$Habitat_Area)

#To get averages for each DCG using the corrected values
DCG_Data_Raster<-DCG_Count_Averages_Corrected_Raster %>%
  group_by(COUNTAREANAME, SPECIES, AREATYPE) %>%
  summarise_at(vars(Male_IN, Male_OUT, Female_IN, Female_OUT, Y_ANY, TOTAL, Habitat_Area), list(Mean = mean))
DCG_Data_Raster<-DCG_Data_Raster %>%  rename("Habitat_Area" = "Habitat_Area_Mean")

#= = = = = = 
#.Deer Models----
#= = = = = = 

#.Red Deer----
Red.DCG_Data_Raster<-subset(DCG_Data_Raster, SPECIES =="Red")

#..Nitrogen
#Note, the constant  would need to be updated if weights changed
Red.DCG_Data_Raster$Male_IN_KgNitrogen<-(Red.DCG_Data_Raster$Male_IN_Mean)*2.5
Red.DCG_Data_Raster$Male_OUT_KgNitrogen<-(Red.DCG_Data_Raster$Male_OUT_Mean)*2.5
Red.DCG_Data_Raster$Female_IN_KgNitrogen<-(Red.DCG_Data_Raster$Female_IN_Mean)*1.86
Red.DCG_Data_Raster$Female_Out_KgNitrogen<-(Red.DCG_Data_Raster$Female_OUT_Mean)*1.86
Red.DCG_Data_Raster$Y_KgNitrogen<-(Red.DCG_Data_Raster$Y_ANY_Mean)*1.01 
Red.DCG_Data_Raster$Sum_of_Means_KgNitrogen<-rowSums(Red.DCG_Data_Raster[,c("Male_IN_KgNitrogen", "Male_OUT_KgNitrogen", "Female_IN_KgNitrogen", "Female_Out_KgNitrogen", "Y_KgNitrogen" )])
Red.DCG_Data_Raster$Mean_N_Kg_ha<-(Red.DCG_Data_Raster$Sum_of_Means_KgNitrogen)/(Red.DCG_Data_Raster$Habitat_Area)

#..Phosphorus
Red.DCG_Data_Raster$Male_IN_KgPhosphorus<-(Red.DCG_Data_Raster$Male_IN_Mean*1.43 + Red.DCG_Data_Raster$Male_IN_Mean*0.69)
Red.DCG_Data_Raster$Male_OUT_KgPhosphorus<-(Red.DCG_Data_Raster$Male_OUT_Mean*1.43  + Red.DCG_Data_Raster$Male_IN_Mean*0.69)
Red.DCG_Data_Raster$Female_IN_KgPhosphorus<-(Red.DCG_Data_Raster$Female_IN_Mean)*1.23
Red.DCG_Data_Raster$Female_Out_KgPhosphorus<-(Red.DCG_Data_Raster$Female_OUT_Mean)*1.23
Red.DCG_Data_Raster$Y_KgPhosphorus<-(Red.DCG_Data_Raster$Y_ANY_Mean)*.65
Red.DCG_Data_Raster$Sum_of_Means_KgPhosphorus<-rowSums(Red.DCG_Data_Raster[,c("Male_IN_KgPhosphorus", "Male_OUT_KgPhosphorus", "Female_IN_KgPhosphorus", "Female_Out_KgPhosphorus", "Y_KgPhosphorus" )])
Red.DCG_Data_Raster$Mean_P_Kg_ha<-(Red.DCG_Data_Raster$Sum_of_Means_KgPhosphorus)/(Red.DCG_Data_Raster$Habitat_Area)

#..Calcium
Red.DCG_Data_Raster$Male_IN_KgCalcium<-(Red.DCG_Data_Raster$Male_IN_Mean*2.39 + Red.DCG_Data_Raster$Male_IN_Mean*1.14)
Red.DCG_Data_Raster$Male_OUT_KgCalcium<-(Red.DCG_Data_Raster$Male_OUT_Mean*2.39 + Red.DCG_Data_Raster$Male_IN_Mean*1.14)
Red.DCG_Data_Raster$Female_IN_KgCalcium<-(Red.DCG_Data_Raster$Female_IN_Mean)*2.06
Red.DCG_Data_Raster$Female_Out_KgCalcium<-(Red.DCG_Data_Raster$Female_OUT_Mean)*2.06
Red.DCG_Data_Raster$Y_KgCalcium<-(Red.DCG_Data_Raster$Y_ANY_Mean)*1.09
Red.DCG_Data_Raster$Sum_of_Means_KgCalcium<-rowSums(Red.DCG_Data_Raster[,c("Male_IN_KgCalcium", "Male_OUT_KgCalcium", "Female_IN_KgCalcium", "Female_Out_KgCalcium", "Y_KgCalcium" )])
Red.DCG_Data_Raster$Mean_Ca_Kg_ha<-(Red.DCG_Data_Raster$Sum_of_Means_KgCalcium)/(Red.DCG_Data_Raster$Habitat_Area)


#.Roe----
Roe.DCG_Data_Raster<-subset(DCG_Data_Raster, SPECIES =="Roe")
#..Nitrogen
Roe.DCG_Data_Raster$Male_IN_KgNitrogen<-(Roe.DCG_Data_Raster$Male_IN_Mean)*0.55
Roe.DCG_Data_Raster$Male_OUT_KgNitrogen<-(Roe.DCG_Data_Raster$Male_OUT_Mean)*0.55
Roe.DCG_Data_Raster$Female_IN_KgNitrogen<-(Roe.DCG_Data_Raster$Female_IN_Mean)*0.52
Roe.DCG_Data_Raster$Female_Out_KgNitrogen<-(Roe.DCG_Data_Raster$Female_OUT_Mean)*0.52
Roe.DCG_Data_Raster$Y_KgNitrogen<-(Roe.DCG_Data_Raster$Y_ANY_Mean)*0.35 
Roe.DCG_Data_Raster$Sum_of_Means_KgNitrogen<-rowSums(Roe.DCG_Data_Raster[,c("Male_IN_KgNitrogen", "Male_OUT_KgNitrogen", "Female_IN_KgNitrogen", "Female_Out_KgNitrogen", "Y_KgNitrogen" )])
Roe.DCG_Data_Raster$Mean_N_Kg_ha<-(Roe.DCG_Data_Raster$Sum_of_Means_KgNitrogen)/(Roe.DCG_Data_Raster$Habitat_Area)

#..Phosphorus
Roe.DCG_Data_Raster$Male_IN_KgPhosphorus<-(Roe.DCG_Data_Raster$Male_IN_Mean*0.35 + Roe.DCG_Data_Raster$Male_IN_Mean*0.03)
Roe.DCG_Data_Raster$Male_OUT_KgPhosphorus<-(Roe.DCG_Data_Raster$Male_OUT_Mean*0.35 + Roe.DCG_Data_Raster$Male_IN_Mean*0.03)
Roe.DCG_Data_Raster$Female_IN_KgPhosphorus<-(Roe.DCG_Data_Raster$Female_IN_Mean)*0.39
Roe.DCG_Data_Raster$Female_Out_KgPhosphorus<-(Roe.DCG_Data_Raster$Female_OUT_Mean)*0.39
Roe.DCG_Data_Raster$Y_KgPhosphorus<-(Roe.DCG_Data_Raster$Y_ANY_Mean)*0.24 
Roe.DCG_Data_Raster$Sum_of_Means_KgPhosphorus<-rowSums(Roe.DCG_Data_Raster[,c("Male_IN_KgPhosphorus", "Male_OUT_KgPhosphorus", "Female_IN_KgPhosphorus", "Female_Out_KgPhosphorus", "Y_KgPhosphorus" )])
Roe.DCG_Data_Raster$Mean_P_Kg_ha<-(Roe.DCG_Data_Raster$Sum_of_Means_KgPhosphorus)/(Roe.DCG_Data_Raster$Habitat_Area)

#..Calcium
Roe.DCG_Data_Raster$Male_IN_KgCalcium<-(Roe.DCG_Data_Raster$Male_IN_Mean*0.59 + Roe.DCG_Data_Raster$Male_IN_Mean*0.05)
Roe.DCG_Data_Raster$Male_OUT_KgCalcium<-(Roe.DCG_Data_Raster$Male_OUT_Mean*0.59 + Roe.DCG_Data_Raster$Male_IN_Mean*0.05)
Roe.DCG_Data_Raster$Female_IN_KgCalcium<-(Roe.DCG_Data_Raster$Female_IN_Mean)*0.56
Roe.DCG_Data_Raster$Female_Out_KgCalcium<-(Roe.DCG_Data_Raster$Female_OUT_Mean)*0.56
Roe.DCG_Data_Raster$Y_KgCalcium<-(Roe.DCG_Data_Raster$Y_ANY_Mean)*0.40 
Roe.DCG_Data_Raster$Sum_of_Means_KgCalcium<-rowSums(Roe.DCG_Data_Raster[,c("Male_IN_KgCalcium", "Male_OUT_KgCalcium", "Female_IN_KgCalcium", "Female_Out_KgCalcium", "Y_KgCalcium" )])
Roe.DCG_Data_Raster$Mean_Ca_Kg_ha<-(Roe.DCG_Data_Raster$Sum_of_Means_KgCalcium)/(Roe.DCG_Data_Raster$Habitat_Area)

#.Sika----
Sika.DCG_Data_Raster<-subset(DCG_Data_Raster, SPECIES =="Sika")
#..Nitrogen
Sika.DCG_Data_Raster$Male_IN_KgNitrogen<-(Sika.DCG_Data_Raster$Male_IN_Mean)*1.04
Sika.DCG_Data_Raster$Male_OUT_KgNitrogen<-(Sika.DCG_Data_Raster$Male_OUT_Mean)*1.04
Sika.DCG_Data_Raster$Female_IN_KgNitrogen<-(Sika.DCG_Data_Raster$Female_IN_Mean)*0.81
Sika.DCG_Data_Raster$Female_Out_KgNitrogen<-(Sika.DCG_Data_Raster$Female_OUT_Mean)*0.81
Sika.DCG_Data_Raster$Y_KgNitrogen<-(Sika.DCG_Data_Raster$Y_ANY_Mean)*0.68
Sika.DCG_Data_Raster$Sum_of_Means_KgNitrogen<-rowSums(Sika.DCG_Data_Raster[,c("Male_IN_KgNitrogen", "Male_OUT_KgNitrogen", "Female_IN_KgNitrogen", "Female_Out_KgNitrogen", "Y_KgNitrogen" )])
Sika.DCG_Data_Raster$Mean_N_Kg_ha<-(Sika.DCG_Data_Raster$Sum_of_Means_KgNitrogen)/(Sika.DCG_Data_Raster$Habitat_Area)

#..Phosphorus
Sika.DCG_Data_Raster$Male_IN_KgPhosphorus<-(Sika.DCG_Data_Raster$Male_IN_Mean*0.59 + Sika.DCG_Data_Raster$Male_IN_Mean*0.13)
Sika.DCG_Data_Raster$Male_OUT_KgPhosphorus<-(Sika.DCG_Data_Raster$Male_OUT_Mean*0.59 + Sika.DCG_Data_Raster$Male_IN_Mean*0.13)
Sika.DCG_Data_Raster$Female_IN_KgPhosphorus<-(Sika.DCG_Data_Raster$Female_IN_Mean)*0.46
Sika.DCG_Data_Raster$Female_Out_KgPhosphorus<-(Sika.DCG_Data_Raster$Female_OUT_Mean)*0.46
Sika.DCG_Data_Raster$Y_KgPhosphorus<-(Sika.DCG_Data_Raster$Y_ANY_Mean)*0.38 
Sika.DCG_Data_Raster$Sum_of_Means_KgPhosphorus<-rowSums(Sika.DCG_Data_Raster[,c("Male_IN_KgPhosphorus", "Male_OUT_KgPhosphorus", "Female_IN_KgPhosphorus", "Female_Out_KgPhosphorus", "Y_KgPhosphorus" )])
Sika.DCG_Data_Raster$Mean_P_Kg_ha<-(Sika.DCG_Data_Raster$Sum_of_Means_KgPhosphorus)/(Sika.DCG_Data_Raster$Habitat_Area)

#..Calcium
Sika.DCG_Data_Raster$Male_IN_KgCalcium<-(Sika.DCG_Data_Raster$Male_IN_Mean*0.98 + Sika.DCG_Data_Raster$Male_IN_Mean*0.2)
Sika.DCG_Data_Raster$Male_OUT_KgCalcium<-(Sika.DCG_Data_Raster$Male_OUT_Mean*0.98 + Sika.DCG_Data_Raster$Male_IN_Mean*0.2)
Sika.DCG_Data_Raster$Female_IN_KgCalcium<-(Sika.DCG_Data_Raster$Female_IN_Mean)*0.76
Sika.DCG_Data_Raster$Female_Out_KgCalcium<-(Sika.DCG_Data_Raster$Female_OUT_Mean)*0.76
Sika.DCG_Data_Raster$Y_KgCalcium<-(Sika.DCG_Data_Raster$Y_ANY_Mean)*0.47 #INSERT VALUE HERE
Sika.DCG_Data_Raster$Sum_of_Means_KgCalcium<-rowSums(Sika.DCG_Data_Raster[c("Male_IN_KgCalcium", "Male_OUT_KgCalcium", "Female_IN_KgCalcium", "Female_Out_KgCalcium", "Y_KgCalcium" )])
Sika.DCG_Data_Raster$Mean_Ca_Kg_ha<-(Sika.DCG_Data_Raster$Sum_of_Means_KgCalcium)/(Sika.DCG_Data_Raster$Habitat_Area)

#.Fallow----
Fallow.DCG_Data_Raster<-subset(DCG_Data_Raster, SPECIES =="Fallow")
#..Nitrogen-
Fallow.DCG_Data_Raster$Male_IN_KgNitrogen<-(Fallow.DCG_Data_Raster$Male_IN_Mean)*1.55
Fallow.DCG_Data_Raster$Male_OUT_KgNitrogen<-(Fallow.DCG_Data_Raster$Male_OUT_Mean)*1.55
Fallow.DCG_Data_Raster$Female_IN_KgNitrogen<-(Fallow.DCG_Data_Raster$Female_IN_Mean)*1.09
Fallow.DCG_Data_Raster$Female_Out_KgNitrogen<-(Fallow.DCG_Data_Raster$Female_OUT_Mean)*1.09
Fallow.DCG_Data_Raster$Y_KgNitrogen<-(Fallow.DCG_Data_Raster$Y_ANY_Mean)*0.66 
Fallow.DCG_Data_Raster$Sum_of_Means_KgNitrogen<-rowSums(Fallow.DCG_Data_Raster[,c("Male_IN_KgNitrogen", "Male_OUT_KgNitrogen", "Female_IN_KgNitrogen", "Female_Out_KgNitrogen", "Y_KgNitrogen" )])
Fallow.DCG_Data_Raster$Mean_N_Kg_ha<-(Fallow.DCG_Data_Raster$Sum_of_Means_KgNitrogen)/(Fallow.DCG_Data_Raster$Habitat_Area)

#..Phosphorus
Fallow.DCG_Data_Raster$Male_IN_KgPhosphorus<-(Fallow.DCG_Data_Raster$Male_IN_Mean*0.86 + Fallow.DCG_Data_Raster$Male_IN_Mean*0.09)
Fallow.DCG_Data_Raster$Male_OUT_KgPhosphorus<-(Fallow.DCG_Data_Raster$Male_OUT_Mean*0.86 + Fallow.DCG_Data_Raster$Male_IN_Mean*0.09)
Fallow.DCG_Data_Raster$Female_IN_KgPhosphorus<-(Fallow.DCG_Data_Raster$Female_IN_Mean)*0.60
Fallow.DCG_Data_Raster$Female_Out_KgPhosphorus<-(Fallow.DCG_Data_Raster$Female_OUT_Mean)*0.60
Fallow.DCG_Data_Raster$Y_KgPhosphorus<-(Fallow.DCG_Data_Raster$Y_ANY_Mean)*0.4 
Fallow.DCG_Data_Raster$Sum_of_Means_KgPhosphorus<-rowSums(Fallow.DCG_Data_Raster[,c("Male_IN_KgPhosphorus", "Male_OUT_KgPhosphorus", "Female_IN_KgPhosphorus", "Female_Out_KgPhosphorus", "Y_KgPhosphorus" )])
Fallow.DCG_Data_Raster$Mean_P_Kg_ha<-(Fallow.DCG_Data_Raster$Sum_of_Means_KgPhosphorus)/(Fallow.DCG_Data_Raster$Habitat_Area)

#..Calcium
Fallow.DCG_Data_Raster$Male_IN_KgCalcium<-(Fallow.DCG_Data_Raster$Male_IN_Mean*1.36 + Fallow.DCG_Data_Raster$Male_IN_Mean*0.14)
Fallow.DCG_Data_Raster$Male_OUT_KgCalcium<-(Fallow.DCG_Data_Raster$Male_OUT_Mean*1.36 + Fallow.DCG_Data_Raster$Male_IN_Mean*0.14)
Fallow.DCG_Data_Raster$Female_IN_KgCalcium<-(Fallow.DCG_Data_Raster$Female_IN_Mean)*1.01
Fallow.DCG_Data_Raster$Female_Out_KgCalcium<-(Fallow.DCG_Data_Raster$Female_OUT_Mean)*1.01
Fallow.DCG_Data_Raster$Y_KgCalcium<-(Fallow.DCG_Data_Raster$Y_ANY_Mean)*.66 
Fallow.DCG_Data_Raster$Sum_of_Means_KgCalcium<-rowSums(Fallow.DCG_Data_Raster[,c("Male_IN_KgCalcium", "Male_OUT_KgCalcium", "Female_IN_KgCalcium", "Female_Out_KgCalcium", "Y_KgCalcium" )])
Fallow.DCG_Data_Raster$Mean_Ca_Kg_ha<-(Fallow.DCG_Data_Raster$Sum_of_Means_KgCalcium)/(Fallow.DCG_Data_Raster$Habitat_Area)

#.Output-
DCG_Data_Raster<-rbind(Red.DCG_Data_Raster, Roe.DCG_Data_Raster, Sika.DCG_Data_Raster, Fallow.DCG_Data_Raster)
DCG_Data_Raster<-as.data.frame(DCG_Data_Raster)


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Data Anlaysis----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


##= = = = = = 
#.Deer Population Stats (Table 2) ----
#= = = = = = 
DCG_Averages_Raster <- DCG_Count_Averages_Corrected_Raster

DCG_Averages_Raster <- DCG_Averages_Raster %>% 
        rename("Species" = "SPECIES",
               "Population" = "TOTAL",
               "Habitat" = "AREATYPE",
               "Year" = "Year_starting")

DCG_Averages_Species_Summary <- DCG_Averages_Raster %>% group_by(Species) %>% 
  summarise(Total_Culled= sum(Population, na.rm = TRUE),)
DCG_Averages_Habitat_Summary <- DCG_Averages_Raster %>% group_by(Habitat) %>% 
  summarise(Total_Culled= sum(Population, na.rm = TRUE),)
Total_Culled<-sum(DCG_Averages_Species_Summary$Total_Culled)

#..Total culled by habitat
DCG_Averages_Species_Habitat_Summary <- DCG_Averages_Raster %>% group_by(Habitat, Species) %>% 
  summarise(Total_Culled= sum(Population, na.rm = TRUE),)

DCG_Averages_Species_Habitat_Summary_Wide<- DCG_Averages_Species_Habitat_Summary %>% 
  pivot_wider(
    names_from = "Habitat", 
    values_from = "Total_Culled",
    values_fn = mean)

Table.2<-DCG_Averages_Species_Habitat_Summary_Wide


#..Averaged culled by habitat
DCG_Averages_AnnualMean_Summary <- DCG_Averages_Raster %>% group_by(Habitat, Species) %>% 
  summarise(mean= mean(Population, na.rm = TRUE),)

DCG_Averages_AnnualMean_Summary_Wide<- DCG_Averages_AnnualMean_Summary %>% 
  pivot_wider(
    names_from = "Habitat", 
    values_from = "mean",
    values_fn = mean)


#= = = = = = 
#.Species Across entire DCG by (No longer a table) ----
#= = = = = = 
DCG_Data_By_DCG_Species <- DCG_Data_Raster %>% group_by(COUNTAREANAME, SPECIES) %>% 
  summarise(KgNitrogen = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
            KgPhosphorus = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
            KgCalcium = sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
  )

DCG_Data_By_DCG_Species <- merge(DCG_Data_By_DCG_Species, DCG_Raster_Habitat_Areas, by.x = 'COUNTAREANAME', by.y = 'DCG_Name')
DCG_Data_By_DCG_Species$total_area_ha<-as.numeric(DCG_Data_By_DCG_Species$total_area_ha)

DCG_Data_By_DCG_Species$KgNitrogen_ha<-DCG_Data_By_DCG_Species$KgNitrogen/DCG_Data_By_DCG_Species$total_area_ha
DCG_Data_By_DCG_Species$KgPhosphorus_ha<-DCG_Data_By_DCG_Species$KgPhosphorus/DCG_Data_By_DCG_Species$total_area_ha

DCG_Data_By_DCG_Species$Calcium_ha<-DCG_Data_By_DCG_Species$KgCalcium/DCG_Data_By_DCG_Species$total_area_ha

DCG_Data_By_DCG_Species





#= = = = = = 
#.Habitat Across entire DCG (Table S4) ----
#= = = = = = 
Summary_Habitat <- DCG_Data_Raster %>% group_by(COUNTAREANAME, AREATYPE) %>% 
  summarise( KgN = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
             KgP = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
             KgC= sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
             KgN_ha = sum(Mean_N_Kg_ha, na.rm = TRUE), 
             KgP_ha = sum(Mean_P_Kg_ha, na.rm = TRUE),
             KgCa_ha = sum(Mean_Ca_Kg_ha, na.rm = TRUE),   )

#HERE REPLACING 0s with NA
Summary_Habitat <- replace(Summary_Habitat, Summary_Habitat==0, NA)

Table.S4<-Summary_Habitat


#= = = = = = 
#.Contry Wide Nutrient Loss (Table 2 and Table S3)----
#= = = = = = 
DCG_Data_By_DCG_Species <- DCG_Data_Raster %>% group_by(COUNTAREANAME, SPECIES) %>% 
  summarise(KgNitrogen = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
            KgPhosphorus = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
            KgCalcium = sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
  )

DCG_Data_By_DCG_Species <- merge(DCG_Data_By_DCG_Species, DCG_Raster_Habitat_Areas, by.x = 'COUNTAREANAME', by.y = 'DCG_Name')
DCG_Data_By_DCG_Species$total_area_ha<-as.numeric(DCG_Data_By_DCG_Species$total_area_ha)

DCG_Data_By_DCG_Species$KgNitrogen_ha<-DCG_Data_By_DCG_Species$KgNitrogen/DCG_Data_By_DCG_Species$total_area_ha
DCG_Data_By_DCG_Species$KgPhosphorus_ha<-DCG_Data_By_DCG_Species$KgPhosphorus/DCG_Data_By_DCG_Species$total_area_ha

DCG_Data_By_DCG_Species$Calcium_ha<-DCG_Data_By_DCG_Species$KgCalcium/DCG_Data_By_DCG_Species$total_area_ha


Nitrogen_Lost<-sum(DCG_Data_By_DCG_Species$KgNitrogen)
Phosphorus_Lost<-sum(DCG_Data_By_DCG_Species$KgPhosphorus)
Calcium_Lost<-sum(DCG_Data_By_DCG_Species$KgCalcium)


Species_Lost <- DCG_Data_Raster %>% group_by( SPECIES) %>% 
  summarise(KgNitrogen = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
            KgPhosphorus = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
            KgCalcium = sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
            KgNitrogen = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
            KgPhosphorus = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
            KgCalcium = sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
            KgN_ha = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE),
            KgP_ha = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
            KgCa_ha = sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
  )

Table.2<-Species_Lost

Habitat_Lost <- DCG_Data_Raster %>% group_by( AREATYPE) %>% 
  summarise(KgNitrogen = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
            KgPhosphorus = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
            KgCalcium = sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
            KgNitrogen = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
            KgPhosphorus = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
            KgCalcium = sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
            KgN_ha = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE),
            KgP_ha = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
            KgCa_ha = sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
  )

Table.S3<-Habitat_Lost




# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Atmopheric N Deposition----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
AtmosphericN$Ndep<- rowSums(AtmosphericN[ , c("grd_NHx_dry", "grd_NOy_dry", "grd_NHx_wet", "grd_NOy_wet")]) 
AtmosphericN_Subset<-AtmosphericN[ , c("x", "y", "Ndep")]  

#Creating the Scotland Raster
AtmosphericN_Smaller<-subset(AtmosphericN_Subset, y > 500000 )
Scotland.NDep_Raster<- rasterFromXYZ(AtmosphericN_Smaller[, c("x", "y", "Ndep")], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs" )

#merging the two
DCG_Clip_Merged_pjr <- spTransform(DCG_Clip_Merged, crs(Scotland.NDep_Raster))
Extent_clipped_raster <- crop(Scotland.NDep_Raster, extent(DCG_Clip_Merged))
DCG_N_raster <- mask(Extent_clipped_raster, DCG_Clip_Merged)
plot(DCG_N_raster)

DCG <- unique(DCG_Clip_Merged$AREA_NA)
DCG_Raster_Habitat_Areas_NDep<-list()
Output<-list()
for (i in 1:length(DCG)) {
  DCG_Clip<-subset(DCG_Clip_Merged, AREA_NA == DCG[i],)
  DCG_Name<- DCG[i]
  Hab_Raster_DCG_Clip<-crop(Scotland_Raster, extent(DCG_Clip))
  Hab_Raster_DCG_Clip[is.na(Hab_Raster_DCG_Clip[])] <- 0 
  Hab_Raster_DCG_Clip <- crop(Hab_Raster_DCG_Clip, extent(DCG_Clip))
  DCG_Hab_Raster_Woodland <- mask(Hab_Raster_DCG_Clip, DCG_Clip)
  DCG_Hab_Raster_OpenRange <- mask(Hab_Raster_DCG_Clip, DCG_Clip)
  DCG_Hab_Raster_Ag <- mask(Hab_Raster_DCG_Clip, DCG_Clip)
  #Woodland IDS 14, 15, 16, 17
  Not_Woodland_IDS<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 18, 19, 20, 21, 22, 23)
  DCG_Hab_Raster_Woodland[DCG_Hab_Raster_Woodland %in% Not_Woodland_IDS] <-NA 
  extent(DCG_Hab_Raster_Woodland)<-extent(DCG_Clip)
  DCG_NDep_Woodland<-crop(Scotland.NDep_Raster, extent(DCG_Hab_Raster_Woodland))
  DCG_NDep_Woodland <- projectRaster(DCG_NDep_Woodland, DCG_Hab_Raster_Woodland)
  DCG_NDep_Woodland_Raster <- mask(DCG_NDep_Woodland, DCG_Hab_Raster_Woodland)
  Woodland_NDep_Av<-as.data.frame(cellStats(DCG_NDep_Woodland_Raster,mean))
  #Open - 12
  Not_OpenRange_IDS<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
  DCG_Hab_Raster_OpenRange[DCG_Hab_Raster_OpenRange %in% Not_OpenRange_IDS] <-NA 
  extent(DCG_Hab_Raster_OpenRange)<-extent(DCG_Clip)
  DCG_NDep_OpenRange<-crop(Scotland.NDep_Raster, extent(DCG_Hab_Raster_OpenRange))
  DCG_NDep_OpenRange <- projectRaster(DCG_NDep_OpenRange, DCG_Hab_Raster_OpenRange)
  DCG_NDep_OpenRange_Raster <- mask(DCG_NDep_OpenRange, DCG_Hab_Raster_OpenRange)
  OpenRange_NDep_Av<-as.data.frame(cellStats(DCG_NDep_OpenRange_Raster,mean))
  #Ag - 6, 20
  Not_Ag_IDS<-c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23)
  DCG_Hab_Raster_Ag[DCG_Hab_Raster_Ag %in% Not_Ag_IDS] <-NA 
  extent(DCG_Hab_Raster_Ag)<-extent(DCG_Clip)
  DCG_NDep_Ag<-crop(Scotland.NDep_Raster, extent(DCG_Hab_Raster_Ag))
  DCG_NDep_Ag <- projectRaster(DCG_NDep_Ag, DCG_Hab_Raster_Ag)
  DCG_NDep_Ag_Raster <- mask(DCG_NDep_Ag, DCG_Hab_Raster_Ag)
  Ag_NDep_Av<-as.data.frame(cellStats(DCG_NDep_Ag_Raster,mean))
  
  Output<- cbind(DCG_Name, Woodland_NDep_Av, OpenRange_NDep_Av, Ag_NDep_Av)
  DCG_Raster_Habitat_Areas_NDep<- rbind(DCG_Raster_Habitat_Areas_NDep, Output )
  
  
}

colnames(DCG_Raster_Habitat_Areas_NDep)[2] ="Woodland"
colnames(DCG_Raster_Habitat_Areas_NDep)[3] ="Open Range"  
colnames(DCG_Raster_Habitat_Areas_NDep)[4] ="Agriculture"

DCG_Raster_Habitat_Areas_NDep_Long <- DCG_Raster_Habitat_Areas_NDep %>% 
  pivot_longer(
    cols =c( "Woodland", "Open Range" , "Agriculture"), 
    names_to = "Habitat",
    values_to = "Mean_NDep"
  )

Summary_Habitat_N <- Summary_Habitat[,-c(3:5, 7:8)]


DCG_NDep_Habitats_Data<- Summary_Habitat_N %>% inner_join(DCG_Raster_Habitat_Areas_NDep_Long, 
                                                          by=c('COUNTAREANAME'='DCG_Name', "AREATYPE" = "Habitat" ))

DCG_NDep_Habitats_Data$Net_N_Change<-DCG_NDep_Habitats_Data$Mean_NDep - DCG_NDep_Habitats_Data$KgN_ha
DCG_NDep_Habitats_Data$Prec<-DCG_NDep_Habitats_Data$KgN_ha/DCG_NDep_Habitats_Data$Mean_NDep*100

Table.S4<-DCG_NDep_Habitats_Data


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Figures----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

#= = = = = = 
#.Fig 1----
#= = = = = = 
#Stacked bar plot of deer populations culled
require(grid)   # for the textGrob() function

my_colors <- RColorBrewer::brewer.pal(6, "Oranges")[3:6]


Ag_Plot<-  DCG_Averages_Raster %>% filter(Habitat == "Agriculture") %>%
  ggplot(aes(fill=Species, y=Population, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = rev(my_colors)) +
  scale_y_continuous(labels =scales::comma) + 
  xlab("Year") +
  ggforce::facet_col(facets = vars(Habitat), 
                     scales = "free_y", 
                     space = "free") +
  ylab("") + 
  theme(strip.text.y = element_text(angle=0))

OpenRange_Plot<-  DCG_Averages_Raster %>% filter(Habitat == "Open Range") %>%
  ggplot(aes(fill=Species, y=Population, x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = rev(my_colors)) +
 scale_y_continuous(labels = scales::comma) + 
    xlab("Year") +
  ggforce::facet_col(facets = vars(Habitat), 
                     scales = "free_y", 
                     space = "free") +
  ylab("") + 
  theme(strip.text.y = element_text(angle=0))

Woodland_Plot<-  DCG_Averages_Raster %>% filter(Habitat == "Woodland") %>%
  ggplot(aes(fill=Species, y=Population,x=factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = rev(my_colors)) +
    scale_y_continuous(labels = scales::comma) + 
    xlab("Year") +
  ggforce::facet_col(facets = vars(Habitat), 
                     scales = "free_y", 
                     space = "free") +
  ylab("") + 
  theme(strip.text.y = element_text(angle=0)) 

quartz()
Pop_Hab<-ggarrange(Ag_Plot + rremove("ylab") + rremove("xlab"), OpenRange_Plot+ rremove("ylab") + rremove("xlab"), Woodland_Plot + rremove("ylab") + rremove("xlab"), ncol = 1,   common.legend = TRUE, legend = "right")  
annotate_figure(Pop_Hab, left = textGrob("Culled Individuals", rot = 90, gp = gpar(cex = 1)), bottom = textGrob("Year", gp = gpar(cex = 1)))

jpeg(file="/Users/kristyferraro/My Drive/Scholarship/Yale/Projects/Scotland Carcass Nutreint Removal/Pop_Hab.jpeg", width=600, height=350)



tiff(file="/Users/kristyferraro/My Drive/Scholarship/Yale/Projects/Scotland Carcass Nutreint Removal/Pop_Hab.tiff", width=6, height=4, units="in", res=300)
plot(Pop_Hab)
dev.off()


#= = = = = = 
#.Fig 2----
#= = = = = = 
Summary_Habitat_Species <- DCG_Data_Raster %>% group_by(AREATYPE, SPECIES) %>% 
  summarise( Nitrogen = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
             Phosphorus = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
             Calcium= sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
             KgN_ha = sum(Mean_N_Kg_ha, na.rm = TRUE), 
             KgP_ha = sum(Mean_P_Kg_ha, na.rm = TRUE),
             KgCa_ha = sum(Mean_Ca_Kg_ha, na.rm = TRUE),   )

Summary_Habitat_Species_Long <- Summary_Habitat_Species %>% 
  pivot_longer(
    cols =c( Nitrogen, Phosphorus, Calcium), 
    names_to = "Nutrient",
    values_to = "Mean_Nutrient_ha"
  )

colnames(Summary_Habitat_Species_Long)[colnames(Summary_Habitat_Species_Long) == "SPECIES"] <- "Species"

Species_Plot_N <-  Summary_Habitat_Species_Long %>% filter(Nutrient == "Nitrogen") %>%
  ggplot(aes(fill=Species, y=Mean_Nutrient_ha, x=AREATYPE)) + 
  geom_bar(stat = "identity")  +
  scale_fill_brewer(palette="Greens", direction =-1)  +
  ggforce::facet_col(facets = vars(Nutrient),  scales = "free_y", space = "free") +  
    scale_y_continuous(labels =scales::comma) + 
  ylab("") +  theme(strip.text.y = element_text(angle=0))

Species_Plot_P <-  Summary_Habitat_Species_Long %>% filter(Nutrient == "Phosphorus") %>%
  ggplot(aes(fill=Species, y=Mean_Nutrient_ha, x=AREATYPE)) + 
  geom_bar(stat = "identity")  +
  scale_fill_manual(values=c("mediumorchid4","orchid4",  "plum3", "thistle2") ) +
  ggforce::facet_col(facets = vars(Nutrient),  scales = "free_y", space = "free") +  
    scale_y_continuous(labels =scales::comma) + 
  ylab("") +  theme(strip.text.y = element_text(angle=0))

Species_Plot_Ca <-  Summary_Habitat_Species_Long %>% filter(Nutrient == "Calcium") %>%
  ggplot(aes(fill=Species, y=Mean_Nutrient_ha, x=AREATYPE)) + 
  geom_bar(stat = "identity")  +
  scale_fill_brewer(palette="Blues", direction =-1)+
  ggforce::facet_col(facets = vars(Nutrient),  scales = "free_y", space = "free") +  
    scale_y_continuous(labels =scales::comma) + 
  ylab("") +  theme(strip.text.y = element_text(angle=0)) 

quartz()
Total_By_Hab<-ggarrange(Species_Plot_N + rremove("ylab") + rremove("xlab"), Species_Plot_P+ rremove("ylab") + rremove("xlab"), Species_Plot_Ca + rremove("ylab") + rremove("xlab"), ncol = 1)
annotate_figure(Total_By_Hab, left = textGrob(bquote("Mass of Nutreint Lost (Kg year"^-1*")"), rot = 90,  gp = gpar(cex = 1)), bottom = textGrob("Land Classification", gp = gpar(cex = 1)))



tiff(file="/Users/kristyferraro/My Drive/Scholarship/Yale/Projects/Scotland Carcass Nutreint Removal/Total_By_Hab.tiff", width=5, height=6, units="in", res=300)
plot(Total_By_Hab)
dev.off()


#= = = = = = 
#.Fig 3----
#= = = = = = 
library(ggnewscale)
DCG_Species <- DCG_Data_Raster %>% group_by(COUNTAREANAME, SPECIES) %>%
  summarise( KgN = sum(Sum_of_Means_KgNitrogen, na.rm = TRUE), 
             KgP = sum(Sum_of_Means_KgPhosphorus, na.rm = TRUE),
             KgC= sum(Sum_of_Means_KgCalcium, na.rm = TRUE),
             KgN_ha = sum(Mean_N_Kg_ha, na.rm = TRUE), 
             KgP_ha = sum(Mean_P_Kg_ha, na.rm = TRUE),
             KgCa_ha = sum(Mean_Ca_Kg_ha, na.rm = TRUE)  )

DCG_Species_Merged_Fig<-st_read("DCG_Clip_Merged.shp")
DCG_Species_Merged_Fig <- DCG_Species_Merged_Fig %>% rename("AREA_NAME" = "AREA_NA")


  
DCG_Species_Merged_Fig<- inner_join( DCG_Species_Merged_Fig, DCG_Species,  by=c('AREA_NAME'='COUNTAREANAME'))

empty_theme<-  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),  axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(),  axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background =  element_blank())

rotate_data <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  rotate_matrix <- function(x){ matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) }
  data %>%dplyr::mutate( geometry = .$geometry * shear_matrix() * rotate_matrix(0) + c(x_add, y_add))}

x = -141.25
color = 'gray40'

#.Fallow
DCG_Fallow_Merged<-subset(DCG_Species_Merged_Fig, SPECIES == "Fallow")
Fallow_N_vals<-c(0.000, 0.10, 0.1937184) #.175. #0.1937184. #
Fallow_P_vals<-c(0.00, 0.06, 0.113) #0.1158213 #.113
Fallow_Ca_vals<-c(0.00, 0.10, 0.186) #0.185

Fallow_Plot <- ggplot() +
  geom_sf(data = DCG_Fallow_Merged %>% rotate_data(), aes(fill=KgN_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "darkseagreen2", high = "darkgreen", name="",  breaks = Fallow_N_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + empty_theme + 
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = DCG_Fallow_Merged %>% rotate_data(y_add = -500000), aes(fill=KgP_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "thistle2", high = "orchid4", name="", breaks = Fallow_P_vals,  labels=function(x) paste0(format(round(x, 2), nsmall = 2)))  + empty_theme +
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = DCG_Fallow_Merged %>% rotate_data(y_add = -1000000), aes(fill=KgCa_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "lightsteelblue1", high = "steelblue4", name="",  breaks = Fallow_Ca_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + empty_theme +   new_scale_fill() + 
  new_scale_color() +
  theme(legend.position = "left", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=15)) 

quartz()
Fallow_Plot

#.Red
DCG_Red_Merged<-subset(DCG_Species_Merged_Fig, SPECIES == "Red")
Red_N_vals<-c(0.001, 0.65, 1.31)
Red_P_vals<-c(0.001, 0.48, 0.95)
Red_Ca_vals<-c(0.001, 0.8, 1.60)


Red_Plot <- ggplot() +
  geom_sf(data = DCG_Red_Merged %>% rotate_data(), aes(fill=KgN_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "darkseagreen2", high = "darkgreen", name="", breaks = Red_N_vals,  labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + empty_theme + 
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = DCG_Red_Merged %>% rotate_data(y_add = -500000), aes(fill=KgP_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "thistle2", high = "orchid4", name="", breaks = Red_P_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2)))  + empty_theme +
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = DCG_Red_Merged %>% rotate_data(y_add = -1000000), aes(fill=KgCa_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "lightsteelblue1", high = "steelblue4", name="", breaks = Red_Ca_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + empty_theme + theme(legend.position = "left", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=15)) 

quartz()
Red_Plot

#.Roe
DCG_Roe_Merged<-subset(DCG_Species_Merged_Fig, SPECIES == "Roe")
Roe_N_vals<-c(0.001, 0.07, 0.14) #.133.   0.1391183.   #.14
Roe_P_vals<-c(0.001, 0.05, 0.1015599) #.10.    0.1015599.  #.
Roe_Ca_vals<-c(0.0001, 0.08, 0.1619625) #.161. #0.1619625

Roe_Plot <- ggplot() +
  geom_sf(data = DCG_Roe_Merged %>% rotate_data(), aes(fill=KgN_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "darkseagreen2", high = "darkgreen",  name="", breaks = Roe_N_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + empty_theme + 
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = DCG_Roe_Merged %>% rotate_data(y_add = -500000), aes(fill=KgP_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "thistle2", high = "orchid4", name="", breaks = Roe_P_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2)))  + empty_theme +
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = DCG_Roe_Merged %>% rotate_data(y_add = -1000000), aes(fill=KgCa_ha), color=NA, show.legend = TRUE) +
  scale_fill_gradient(low = "lightsteelblue1", high = "steelblue4", name="", breaks = Roe_Ca_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + empty_theme + theme(legend.position = "left", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=20)) 

quartz()
Roe_Plot

#Sika
Sika_N_vals<-c(0.00, 0.04, 0.08)
Sika_P_vals<-c(0.00, 0.03, 0.05)
Sika_Ca_vals<-c(0.00, 0.04, 0.07)

DCG_Sika_Merged<-subset(DCG_Species_Merged_Fig, SPECIES == "Sika")
Sika_Plot <- ggplot() +
  geom_sf(data = DCG_Sika_Merged %>% rotate_data(), aes(fill=KgN_ha), color=NA, show.legend = TRUE,  inherit.aes = FALSE) +
  scale_fill_gradient(low = "darkseagreen2", high = "darkgreen",  name="",  breaks = Sika_N_vals,  labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + empty_theme +  
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = DCG_Sika_Merged %>% rotate_data(y_add = -500000), aes(fill=KgP_ha), color=NA, show.legend = TRUE,  inherit.aes = FALSE) +
  scale_fill_gradient(low = "thistle2", high = "orchid4", name="",  breaks = Sika_P_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2)))  + empty_theme +
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = DCG_Sika_Merged %>% rotate_data(y_add = -1000000), aes(fill=KgCa_ha), color=NA, show.legend = TRUE,  inherit.aes = FALSE) +
  scale_fill_gradient(low = "lightsteelblue1", high = "steelblue4", name="", breaks = Sika_Ca_vals, labels=function(x) paste0(format(round(x, 2), nsmall = 2))) + empty_theme + theme(legend.position = "left", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=20)) 
  
quartz()
Sika_Plot


#all together
All_Plots<-ggarrange(Fallow_Plot, Red_Plot, Roe_Plot, Sika_Plot + rremove("x.text"), 
                     ncol = 2, nrow = 2)
quartz()
All_Plots

quartz()
Fallow_Plot
Red_Plot
Roe_Plot
Sika_Plot
#= = = = = = 
#.Fig 4----
#= = = = = = 
Summary_Habitat<-read.csv ("/Users/kristyferraro/My Drive/Scholarship/Yale/Projects/Scotland Carcass Nutreint Removal/Data/DCG_Summary_Raster_Habitat_SuppTable.csv")

DCG_Clip_Merged<-st_read("DCG_Clip_Merged.shp")
DCG_Clip_Merged <- DCG_Clip_Merged %>% rename("AREA_NAME" = "AREA_NA")


DCG_Habitat_Data2 <- inner_join( DCG_Clip_Merged, Summary_Habitat,
        by=c('AREA_NAME'='COUNTAREANAME'))
DCG_Long_Format<- DCG_Habitat_Data2 %>% 
  pivot_longer(
    cols =c( KgN_ha, KgP_ha, KgCa_ha), 
    names_to = "Nutrient",
    values_to = "Mean_Nutrient_Kg_ha"
)
DCG_Long_Format_Trimmed<-DCG_Long_Format[, c("AREA_NAME", "AREATYPE", "Nutrient", "Mean_Nutrient_Kg_ha")]


#.Woodlands----
Woodland_N_vals<-c(0.1, 0.2, 0.3)
Woodland_P_vals<-c(0.05, 0.15, 0.25)
Woodland_Ca_vals<-c(0.1, 0.2, 0.3)

#nitrogen
DCG_Woodlands_N<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgN_ha" & AREATYPE == "Woodland" )
DCG_Woodlands_N_Plot<-ggplot(DCG_Woodlands_N) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "darkseagreen2", high = "darkgreen", name="", breaks = Woodland_N_vals) +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_Woodlands_N_Plot

#Phosphorus
DCG_Woodlands_P<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgP_ha" & AREATYPE == "Woodland" )
DCG_Woodlands_P_Plot<-ggplot(DCG_Woodlands_P) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "thistle2", high = "orchid4", name="", n.breaks = 3) +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_Woodlands_P_Plot

#Calcium
DCG_Woodlands_Ca<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgCa_ha" & AREATYPE == "Woodland" )
DCG_Woodlands_Ca_Plot<-ggplot(DCG_Woodlands_Ca) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "lightsteelblue1", high = "steelblue4", name="", breaks = Woodland_Ca_vals, labels=function(x) paste0(format(round(x, 1), nsmall = 1))) +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_Woodlands_Ca_Plot

#.Open Range----
#nitrogen
DCG_OpenRange_N<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgN_ha" & AREATYPE == "Open Range" )
DCG_OpenRange_N_Plot<-ggplot(DCG_OpenRange_N) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "darkseagreen2", high = "darkgreen", name="") +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_OpenRange_N_Plot


#Phosphorus
DCG_OpenRange_P<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgP_ha" & AREATYPE == "Open Range" )
DCG_OpenRange_P_Plot<-ggplot(DCG_OpenRange_P) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "thistle2", high = "orchid4", name="") +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_OpenRange_P_Plot

#Calcium
OpenRange_Ca_vals<-c(0.001, 0.3, 0.5)

DCG_OpenRange_Ca<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgCa_ha" & AREATYPE == "Open Range" )
DCG_OpenRange_Ca_Plot<-ggplot(DCG_OpenRange_Ca) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "lightsteelblue1", high = "steelblue4", name="",  breaks = OpenRange_Ca_vals,  labels=function(x) paste0(format(round(x, 1), nsmall = 1))) +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_OpenRange_Ca_Plot

#.Ag---- 
#nitrogen
DCG_Ag_N<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgN_ha" & AREATYPE == "Agriculture" )
DCG_Ag_N_Plot<-ggplot(DCG_Ag_N) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "darkseagreen2", high = "darkgreen", name="", labels=function(x) paste0(format(round(x, 1), nsmall = 1))) +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_Ag_N_Plot

#Phosphorus
DCG_Ag_P<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgP_ha" & AREATYPE == "Agriculture" )
DCG_Ag_P_Plot<-ggplot(DCG_Ag_P) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "thistle2", high = "orchid4", name="") +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_Ag_P_Plot

#Calcium
Ag_Ca_vals<-c(0.001, .5, 1)
DCG_Ag_Ca<-subset(DCG_Long_Format_Trimmed, Nutrient == "KgCa_ha" & AREATYPE == "Agriculture" )
DCG_Ag_Ca_Plot<-ggplot(DCG_Ag_Ca) +
  geom_sf(aes(fill = Mean_Nutrient_Kg_ha) ) +
  scale_fill_gradient(low = "lightsteelblue1", high = "steelblue4", name="", breaks = Ag_Ca_vals, labels=function(x) paste0(format(round(x, 1), nsmall = 1))) +
  theme(legend.position = "bottom", legend.direction = "horizontal",  legend.key.height= unit(0.25, 'cm'), legend.margin=margin(t=-5))
quartz()
DCG_Ag_Ca_Plot

