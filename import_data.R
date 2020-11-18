library(readr)
library(tidyverse)
# Import all the pesticide data


# Set working directory to where the git project lives
county_ranks16 <- read_delim("table1_county_rank_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
repro_lbs16 <- read_delim("table3_reproductive_lbs_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
repro_acre16 <- read_delim("table4_reproductive_acres_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
