library(readr)
library(tidyverse)
# Import all the pesticide data


# Set working directory to where the git project lives
county_ranks16 <- read_delim("table1_county_rank_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
repro_lbs16 <- read_delim("table3_reproductive_lbs_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
repro_acre16 <- read_delim("table4_reproductive_acres_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)


table1_2016 <- county_ranks16 %>% transmute(county = COUNTY, 
                                     lbs_2015 = LBS_2015, rank_2015 = RANK_2015, 
                                     lbs_2016 = LBS_2016, rank_2016 = RANK_2016)
# column 1 is the county
# columns 2-3 have the previous year data
# columns 4-5 have the current year data

# we only want columns 1-3 for the most up-to-date data for all years before 2016
# table1_2015 <- read_csv("table1_2015.csv")[1:3]
# table1_2014 <- read_csv("table1_2014.csv")[1:3]
# table1_2013 <- read_csv("table1_2013.csv")[1:3]
# table1_2012 <- read_csv("table1_2012.csv")[1:3]
# table1_2011 <- read_csv("table1_2011.csv")[1:3]
# table1_2010 <- read_csv("table1_2010.csv")[1:3]
# table1_2009 <- read_csv("table1_2009.csv")[1:3]
# table1_2008 <- read_csv("table1_2008.csv")[1:3]
# table1_2007 <- read_csv("table1_2007.csv")[1:3]


all_dat <- list(read_csv("table1_2007.csv")[1:3],
                read_csv("table1_2008.csv")[1:3],
                read_csv("table1_2009.csv")[1:3],
                read_csv("table1_2010.csv")[1:3],
                read_csv("table1_2011.csv")[1:3],
                read_csv("table1_2012.csv")[1:3],
                read_csv("table1_2013.csv")[1:3],
                read_csv("table1_2014.csv")[1:3],
                read_csv("table1_2015.csv")[1:3])


table1 <- Reduce(function(x, y) left_join(x, y, by = "county"), all_dat)


long_table1 <- table1 %>% pivot_longer(!county, names_to = 'usage', values_to = "value")
table1_ranks <- long_table1 %>% filter(str_starts(usage, "rank"))
table1_lbs <- long_table1 %>% filter(str_starts(usage, "lbs"))
