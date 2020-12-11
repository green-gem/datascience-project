# Final Project BST 260: Exploration of Pesticides and Perinatal Outcomes in the State of California 
## Lara Rostomian, Sonia Kim, and Zainab Soetan

Welcome to our GitHub repository! For this project, we were interested in the association between pesticide use and perinatal outcomes in California. We used graphs, maps, and linear regression to visualize our data over time and space. Here are few quick guidance tips as to where each file is located. 

- "Data Prep (& Final RMD)" Folder is where all of our data wrangling occurred. "FinalProject.rmd" is an accumulation of all of our code and visualizations. We have added detailed descriptions for each chunk of code and have uploaded the requested "Background, Overview, and Motivation", "Related Work", "Initial Questions & Research Question Evolution", "Data Sources", "Exploratory Analysis", and "Final Analysis" sections to the top of the RMD file for a clear overview of our project before diving into the code. In this folder you will also find all the .txt and .csv files from the data sources. 

- "Shinyapp"" Folder is where our shiny app lives. The finalized code for our shiny app is in "Sonias app.R"

- import_data2.R contains all the string processing for the pesticide data that as only found in the pdfs (years 2007-2015). The 2016 data had a different data publishing technique, so it was the only one that could easily found in a txt format. The pdfs are currently in a Google Drive folder, as they are too large to upload to GitHub. We used pdftools and string processing to create csv files ("table1_YEAR.csv"), which were then combined in a single table in "FinalProject.rmd".


Please install the following packages in R before running "FinalProject.rmd": dplyr, tidyverse, pdftools, readr, stringr, ggthemes, shiny, shinyBS, RColorBrewer, shinydashboard, sp, rgeos, rgdal, maptools, leaflet, scales, maps, gridExtra, grid


Data Files:

* Maternal and Child Health:
  - MCH CDC Data Total.txt
  - MCH CDC Data.txt
  - NatalityCARE.txt
  - NatalityRACE.txt
  - NatalityTOTAL.txt
  - Preterm birth.txt
  - low-and-very-low-birthweight-by-county-2014-2018 (1).csv
  - preterm-and-very-preterm-births-by-county-2010-2018-3.csv 

* Pesticide:
  - table1_YEAR.csv, where YEAR is the year from 2007 to 2015
  - table1_country_rank_2016.txt
  - table3_reproductive_lbs_2016.txt
  - table4_reproductive_acres_2016.txt 
  
 * STATEFPtoSTATENAME_Key.csv
 
 Our website can be found [here](https://bst260rproject.netlify.app/).
 
 Our screencast can be found [here](https://youtu.be/Tp1LITsx_R4).
