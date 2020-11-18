install.packages("pdftools")
library(pdftools)
library(readr)
library(tidyverse)
library(stringr)

# contra <- "Contra Costa"
# delnorte
# la
# sanbenito
# sanbern
# sandiego
# sanfran
# sanjoaquin
# sanluis
# sanmateo
# santabarb
# santaclara 
# santacruz <- "Santa Cruz"

doc <- "Year 2014.pdf"
txt <- pdf_text(doc)

# seventeen <- cat(txt[17])
# seventeen

a <- pdf_data(doc)[[17]] # page 17 is where the table is located
#b <- txt %>% read_lines() 
c <- txt[17] %>% 
  read_lines() %>% 
  str_squish()


z <- c[5:32] %>%
  str_replace_all(",", "") %>%
  str_to_lower() %>%
  str_split(boundary("word"))


z  # need to get way to keep county names together

#%>%   strsplit(split = " ")



# sanluis <- z[[14]]
# 
# deed <- str_c(str_subset(sanluis, "[a-z]+"), sep = " 555")
# 
# 
# c[1]
# c[2]
# c[3]
# c[4]
# c[5]
# 
# shasta <- c[23]

#d <- sanluis %>% 
  str_replace_all(",", "") %>%
  str_to_lower() %>%
  str_split("[0-9]")
    
    #boundary("word"))
#"[a-z]\\s+[0-9]"
#"\\s+[0-9]"


loc <- str_locate(sanluis, "[")
head(print(txt))

# wd <- getwd()
# file_name <- paste0(wd, "/", "temp.txt")
# write(txt, file = file_name, sep = "\r")
# dat <- read.table(file_name, skip = 957, nrows = 1091-958)
# 
# lines <- strsplit(txt, '\r') %>% unlist
# tbl2 <- which(grep1('Table 2', lines))[1]
# tbl2
# 
# 
# result <- txt %>% str_split("\n")
# 
# 
# library(pdftools)
# library(tidyverse)
# 
# str(txt)[[1]]

# clean <- function(tabl){
#   tabl <- str_split(tabl, "\r", simplify = T)
#   
#   
#   tabl_start <- stringr::str_which(tabl, "Table 1:")
#   tabl_end <- stringr::str_which(tabl, "Table 2")
#   tabl <- tabl[1, (tabl_start +1 ):(tabl_end - 1)]
#   #tabl <- str_replace_all(table, "\\s{2,}", "|")
#   text_con <- textConnection(table)
#   data_table <- read.csv(text_con, sep = "|")
#   #colnames(data_table) <- c("Condition", "Males", "Females", "Total")
#   #dplyr::mutate(data_table, Country = country_name)
# }
#   
# clean(txt)
