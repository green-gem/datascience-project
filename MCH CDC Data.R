library(dplyr)
library(tidyverse)
#Cleaning up MCH Dataset 
MCH.CDC.Data <- MCH.CDC.Data[-c(482:538), ]
MCH.CDC.Data <- MCH.CDC.Data[ ,-c(1, 3, 5, 7)]
MCH.CDC.Data.Total <- MCH.CDC.Data.Total[,-c(1, 3, 5)]
MCH.CDC.Data.Total %>% rename("Total Birth" = "Births")

#I noticed that the data I downloaded did not include total # of births so merging two datasets (one that has total # of birth counts and the other with low birth wegiht +very low birth weight counts)
df1 <- full_join(MCH.CDC.Data, MCH.CDC.Data.Total, by=c("Year", "County"))
df1<- df1 %>% rename("LBW" = "Births.x", "Total Births" = "Births.y")

#Note: LBW = Low birth weight + Very low birth weight counts; Total Births = Total # of Birth
col_order <- c("Year", "County", "Total Births",
               "LBW", "Average.Birth.Weight", "Standard.Deviation.for.Average.Birth.Weight",
               "Average.Age.of.Mother", "Standard.Deviation.for.Average.Age.of.Mother","Average.LMP.Gestational.Age",
               "Standard.Deviation.for.Average.LMP.Gestational.Age")
df2 <- df1[,col_order]

#renaming counties to match pesticide data county names

df2[df2$County == "Alameda County, CA", "County"] <-"alameda"
df2[df2$County == "Butte County, CA", "County"] <-"butte"
df2[df2$County == "Contra Costa County, CA", "County"] <-"contra costa"
df2[df2$County == "El Dorado County, CA", "County"] <-"el dorado"
df2[df2$County == "Fresno County, CA", "County"] <-"fresno"
df2[df2$County == "Humboldt County, CA", "County"] <-"humboldt"
df2[df2$County == "Imperial County, CA", "County"] <-"imperial"
df2[df2$County == "Kern County, CA", "County"] <-"kern"
df2[df2$County == "Kings County, CA", "County"] <-"kings"
df2[df2$County == "Los Angeles County, CA", "County"] <-"los angeles"
df2[df2$County == "Madera County, CA", "County"] <-"madera"
df2[df2$County == "Marin County, CA", "County"] <-"marin"
df2[df2$County == "Contra Costa County, CA", "County"] <-"mariposa"
df2[df2$County == "Merced County, CA", "County"] <-"merced"
df2[df2$County == "Monterey County, CA", "County"] <-"monterey"
df2[df2$County == "Napa County, CA", "County"] <-"napa"
df2[df2$County == "Orange County, CA", "County"] <-"orange"
df2[df2$County == "Placer County, CA", "County"] <-"placer"
df2[df2$County == "Riverside County, CA", "County"] <-"riverside"
df2[df2$County == "Sacramento County, CA", "County"] <-"sacramento"
df2[df2$County == "San Bernardino County, CA", "County"] <-"san bernardino"
df2[df2$County == "San Diego County, CA", "County"] <-"san diego"
df2[df2$County == "San Francisco County, CA", "County"] <-"san francisco"
df2[df2$County == "San Joaquin County, CA", "County"] <-"san joaquin"
df2[df2$County == "San Luis Obispo County, CA", "County"] <-"san luis obispo"
df2[df2$County == "San Mateo County, CA", "County"] <-"san mateo"
df2[df2$County == "Santa Barbara County, CA", "County"] <-"santa barbara"
df2[df2$County == "Santa Clara County, CA", "County"] <-"santa clara"
df2[df2$County == "Santa Cruz County, CA", "County"] <-"santa cruz"
df2[df2$County == "Shasta County, CA", "County"] <-"shasta"
df2[df2$County == "Solano County, CA", "County"] <-"solano"
df2[df2$County == "Sonoma County, CA", "County"] <-"sonoma"
df2[df2$County == "Stanislaus County, CA", "County"] <-"stanislaus"
df2[df2$County == "Tulare County, CA", "County"] <-"tulare"
df2[df2$County == "Ventura County, CA", "County"] <-"ventura"
df2[df2$County == "Yolo County, CA", "County"] <-"yolo"

MCH_clean_df <- df2[-c(482:523), ]

