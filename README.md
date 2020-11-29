# datascience-project
Filled with notes, data, and other delightful things :)


import_data.R is used to get all the pesticide data into R. The 2016 data had a different data publishing technique, so it was the only one that could easily found in a txt format. All previous years required additional processing in import_data2.R


import_data2.R is where all the string processing for the pesticide data that as only found in the pdfs. We used this to create csvs, which were then combined in import_data.R