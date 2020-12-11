# Final Project BST 260: The Effects of Pesticide Exposure on Maternal and Child Health by Lara Rostomian, Sonia Kim, and Zainab Soetan

Welcome to our github repository! Here are few quick guidance tips as to where each file is located. 

- "Data Prep (& Final RMD)" Folder is where all of our data wrangling occurred. "FinalProject.rmd" is an accumulation of all of our codes and visualizations. We have added detailed descriptions for each chunk of codes and have uploaded the requested "Background, Overview, and Motivation", "Related Work", "Initial Questions & Research Question Evolution", "Data Sources", "Exploratory Analysis", and "Final Analysis" sections to the top of the RMD file for a clear overview of our project before diving into the code. In this folder you will also find all the .txt and .csv files of from data sources. 

- "Shinyapp"" folder is where our shiny app lies. The finalized code for our shiny app is in "Sonias app.R"

- import_data2.R contains all the string processing for the pesticide data that as only found in the pdfs (years 2007-2015). The 2016 data had a different data publishing technique, so it was the only one that could easily found in a txt format. The pdfs are currently in a Google Drive folders, as they are too large to upload to GitHub. We used pdftools and string processing to create csv files ("table1_YEAR.csv"), which were then combined in a single table in Final Project.Rmd.


This project requires the following packages. Please install before running: 
dplyr, tidyverse, pdftools, readr, stringr, ggthemes, shiny, shinyBS, RColorBrewer, shinydashboard, sp, rgeos, rgdal, maptools, leaflet, scales, maps, gridExtra, grid.


