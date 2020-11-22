library(tidyverse)
library(stringr)
#importing file 
table_3 <- read.csv("chemicals.csv", stringsAsFactors = FALSE)

#removing comma 
table_3$X2006 <- as.numeric(gsub(",","",table_3$X2006))
table_3$X2007 <- as.numeric(gsub(",","",table_3$X2007))
table_3$X2008 <- as.numeric(gsub(",","",table_3$X2008))
table_3$X2009 <- as.numeric(gsub(",","",table_3$X2009))
table_3$X2010 <- as.numeric(gsub(",","",table_3$X2010))
table_3$X2011 <- as.numeric(gsub(",","",table_3$X2011))
table_3$X2012 <- as.numeric(gsub(",","",table_3$X2012))
table_3$X2013 <- as.numeric(gsub(",","",table_3$X2013))
table_3$X2014 <- as.numeric(gsub(",","",table_3$X2014))

#renaming column names
colnames(table_3)[1] <-"Chemical Name"
colnames(table_3)[2] <-"2006"
colnames(table_3)[3] <-"2007"
colnames(table_3)[4] <-"2008"
colnames(table_3)[5] <-"2009"
colnames(table_3)[6] <-"2010"
colnames(table_3)[7] <-"2011"
colnames(table_3)[8] <-"2012"
colnames(table_3)[9] <-"2013"
colnames(table_3)[10] <-"2014"

#removing the unnecessary column 
table_3 <- table_3[-c(11)]

#making it all lower case
table_3$`Chemical Name` <- table_3$`Chemical Name` %>% str_to_lower()

#Problem: I'm not sure what to do with data that are <1. They were automatically changed to NA  
