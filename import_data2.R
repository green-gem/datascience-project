install.packages("pdftools")
library(pdftools)
library(readr)
library(tidyverse)
library(stringr)

doc <- "Year 2014.pdf"
txt <- pdf_text(doc)

indices <- str_which(txt, "Table 1") 
# only the first 2 references are relevant, as they point to the actual table

start_table1 <- txt[indices[1]] %>% 
  read_lines() %>% # read in document line by line
  str_squish() # get rid of extra whitespace

end_table1 <- txt[indices[2]] %>% 
  read_lines() %>% 
  str_squish()

start16 <- str_which(start_table1, "County Pounds Applied") # the last header for the table
end16 <-  length(start_table1)

start17 <- str_which(end_table1, "County Pounds Applied")
end17 <-  length(end_table1)

tbl1 <- start_table1[(start16+1):(end16-1)] %>% #get rid of the header and page number
  str_replace_all(",", "") %>% # gets rid of commas in numbers
  str_to_lower() %>% 
  str_split(boundary("word")) # splits by whitespace

tbl1_b <- end_table1[(start17+1):(end17-2)] %>% # get rid of the header, the total row, and page number
  str_replace_all(",", "") %>%
  str_to_lower() %>%
  str_split(boundary("word"))


# function to take in the list from before, combine county names, and create a single table
combine_names <- function(listy) {
  
  #initialize empty data frame
  df <- data.frame(matrix(NA, nrow = length(listy), ncol = 5))
  # new column names, shorter than original header names
  names(df) <- c("country", 
                 "usage2013","rank2013", 
                 "usage2014", "rank2014")

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
tabel1 <- bind_rows(combine_names(tbl1),
                   combine_names(tbl1_b))


