install.packages("pdftools")
library(pdftools)
library(readr)
library(tidyverse)
library(stringr)

# year and pages for table 1
# 2007: 17-18
# 2008: 14-15
# 2009: 14-16
# 2010: 14-15
# 2011: 16-17
# 2012: 16-17
# 2013: 16-17
# 2014: 16-17
# 2015: 15-17

# function to write the pdf data into csv
get_dat <- function(year, indices){

start_table1a <- txt[indices[1]] %>% 
  read_lines() %>% # read in document line by line
  str_squish() # get rid of extra whitespace

starteda <- str_which(start_table1a, "County Pounds Applied") # the last header for the table
endeda <-  length(start_table1a)

tbl1a <- start_table1a[(starteda+1):(endeda-1)] %>% #get rid of the header and page number
  str_replace_all(",", "") %>% # gets rid of commas in numbers
  str_to_lower() %>% 
  str_split(boundary("word")) # splits by whitespace

if(length(indices)>2){
  
  start_table1b <- txt[indices[2]] %>% 
    read_lines() %>% 
    str_squish()
  
  startedb <- str_which(start_table1b, "County Pounds Applied")
  endedb <- length(start_table1b)
  
  tbl1b <- start_table1b[(startedb+1):(endedb-1)] %>% # get rid of the header, the total row, and page number
    str_replace_all(",", "") %>%
    str_to_lower() %>%
    str_split(boundary("word"))
  
  
  start_table1c <- txt[indices[3]] %>% 
    read_lines() %>% 
    str_squish()
  
  startedc <- str_which(start_table1c, "County Pounds Applied")
  endedc <-  str_which(start_table1c, "Yuba")
    #length(start_table1c)
  
  tbl1c <- start_table1c[(startedc+1):(endedc)] %>% # get rid of the header, the total row, and page number
    str_replace_all(",", "") %>%
    str_to_lower() %>%
    str_split(boundary("word"))
} else {
  start_table1b <- txt[indices[2]] %>% 
    read_lines() %>% 
    str_squish()
  
  startedb <- str_which(start_table1b, "County Pounds Applied")
  endedb <- str_which(start_table1b, "Yuba")
  
  tbl1b <- start_table1b[(startedb+1):(endedb)] %>% # get rid of the header, the total row, and page number
    str_replace_all(",", "") %>%
    str_to_lower() %>%
    str_split(boundary("word"))
}


# function to take in the list from before, combine county names, and create a single table
combine_names <- function(listy) {
  
  #initialize empty data frame
  df <- data.frame(matrix(NA, nrow = length(listy), ncol = 5))
  # new column names, shorter than original header names
  names(df) <- c("county", 
                 paste0("lbs_", toString(as.numeric(year)-1)), 
                 paste0("rank_", toString(as.numeric(year)-1)), 
                 paste0("lbs_", year),
                 paste0("rank_", year))

  # goes through each list in the list to create new row in data frame
  for (r in 1:length(listy)) {
    # county name was split in two (i.e. Los Angeles)
    if (length(listy[[r]]) == 6) {
      nim = paste(listy[[r]][1],
                  listy[[r]][2])
      df[r,1] <- nim
      df[r,2] <- listy[[r]][3]
      df[r,3] <- listy[[r]][4]
      df[r,4] <- listy[[r]][5]
      df[r,5] <- listy[[r]][6]
    
    # county name was split in three (i.e. San Luis Obispo)
    } else if (length(listy[[r]]) == 7) {
      nim = paste(listy[[r]][1],
                  listy[[r]][2],
                  listy[[r]][3])

      df[r,1] <- nim
      df[r,2] <- listy[[r]][4]
      df[r,3] <- listy[[r]][5]
      df[r,4] <- listy[[r]][6]
      df[r,5] <- listy[[r]][7]
    # county name was one word, so it wasn't split (i.e. Alameda)
      } else {
      df[r,1] <- listy[[r]][1]
      df[r,2] <- listy[[r]][2]
      df[r,3] <- listy[[r]][3]
      df[r,4] <- listy[[r]][4]
      df[r,5] <- listy[[r]][5]
    }
  }
  return(df)
}

# Combines the two dataframes into 1 to get a full table 1 :)
if(length(indices)>2){
  tabel1 <- bind_rows(combine_names(tbl1a),
                      combine_names(tbl1b),
                      combine_names(tbl1c))
} else{
  tabel1 <- bind_rows(combine_names(tbl1a),
                      combine_names(tbl1b))
}
filename <- paste0("table1_", year, ".csv")
write_csv(tabel1, filename)
}

Year <- "2014" # change based on year
doc <- paste0("Year ", Year, ".pdf")
txt <- pdf_text(doc)

Pages <- c(seq(16, 17, 1)) # change based on year, put the first and last page number
get_dat(Year, Pages)

