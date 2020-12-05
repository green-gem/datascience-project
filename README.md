# Final Project BST 260: The Effects of Pesticide Exposure on Maternal and Child Health by Lara Rostomian, Sonia Kim, and Zainab Soetan



* background info on why we chose this topic, where all the data came from

This project requires the following packages. Please install before running.

dplyr, ggthemes, leaflet, maptools, pdftools, RColorBrewer, readr, rgdal, rgeos, scales, shiny, shinyBS, shinydashboard, sp, stringr, tidyverse


Final Project.Rmd is where the main data wrangling occurs.

import_data2.R contains all the string processing for the pesticide data that as only found in the pdfs (years 2007-2015). The 2016 data had a different data publishing technique, so it was the only one that could easily found in a txt format. The pdfs are currently in a Google Drive folders, as they are too large to upload to GitHub. We used pdftools and string processing to create csv files ("table1_YEAR.csv"), which were then combined in a single table in Final Project.Rmd.