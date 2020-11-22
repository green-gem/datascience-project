library(tidyverse)
library(stringr)
#importing file 
table_3 <- read.csv("chemicals.csv", stringsAsFactors = FALSE)

table_3 <- table_3[-c(62:80), ]

#