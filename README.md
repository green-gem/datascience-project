# datascience-project
Filled with notes, data, and other delightful things :)


Final Project.Rmd is where the main data wrangling occurs.

import_data2.R contains all the string processing for the pesticide data that as only found in the pdfs (years 2007-2015). The 2016 data had a different data publishing technique, so it was the only one that could easily found in a txt format. The pdfs are currently in a Google Drive folders, as they are too large to upload to GitHub. We used pdftools and string processing to create csv files ("table1_YEAR.csv"), which were then combined in a single table in Final Project.Rmd.