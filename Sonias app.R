#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(shinyBS)
library(RColorBrewer)
library(shinydashboard)
library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(leaflet)
library(scales)

#data wrangling for 5-year lowbirth weight trend- for the leaflet map
lbwdata<-read.csv(paste0(getwd(),"/low-and-very-low-birthweight-by-county-2014-2018 (1).csv"), header = TRUE, stringsAsFactors = FALSE)
lbwdata <- lbwdata %>% mutate(County = str_to_title(County))
lbwdata$Events[is.na(lbwdata$Events)] <- 0
lbwdata <- lbwdata %>% group_by(Year, County, Total.Births) %>% summarize(Events = sum(Events)) 
lbwdata <- lbwdata %>% filter(!County == "california") 
lbwdata <- lbwdata %>% mutate(Rate = Events/Total.Births)
CaliforniaCounty <- map_data("county", "california")
CaliforniaCounty <- CaliforniaCounty %>% mutate(subregion = str_to_title(subregion))
head(CaliforniaCounty)
california_map <- lbwdata %>% full_join(CaliforniaCounty, by = c("County" = "subregion")) %>% mutate(Rate = Events/Total.Births * 10^2)
california_map <- california_map %>% mutate(County = str_to_title(County))

#Data wrangling for preterm birth- for the leaflet map
ptbirthdata<- read.csv("preterm-and-very-preterm-births-by-county-2010-2018-3.csv", header = TRUE, stringsAsFactors = FALSE)
ptbirthdata$Events[is.na(ptbirthdata$Events)] <- 0
ptbirthdata <- ptbirthdata[,-c(7,8)]
ptbirthdata <- ptbirthdata %>% group_by(Year, County, Total.Births) %>% summarize(Events = sum(Events)) 
ptbirthdata <- ptbirthdata %>% filter(!County == "california")#removing the total count
ptbirthdata <- ptbirthdata %>% mutate(rate_pt = Events/Total.Births * 100)

map <- readOGR(path.expand("cb_2018_us_county_20m.shp"),
               layer = "cb_2018_us_county_20m", stringsAsFactors = FALSE)
Statekey<-read.csv(paste0(getwd(),'/STATEFPtoSTATENAME_Key.csv'), colClasses=c('character'))
map<-merge(x=map, y=Statekey, by="STATEFP", all=TRUE)
SingleState <- subset(map, map$STATENAME %in% c(
    "California"
))

ptbirthdata_2016 <- ptbirthdata %>% filter(Year == "2016") 
spatial_pt <-sp::merge(x=SingleState, y=ptbirthdata_2016, by.x="NAME", by.y="County", by=x)

bin <- c(5.5, 8.2, 9.1, 9.9, Inf)
pal2 <- colorBin(
    palette = "viridis",
    domain = spatial_pt$rate_pt, n=7, bins=bin)

#Data wrangling for preterm birth (CDC)
cdc_pretermbirth <- read.delim("Preterm birth.txt",  sep ="\t", dec=".", header = TRUE, stringsAsFactors = FALSE)
cdc_pretermbirth  <- cdc_pretermbirth [-c(422:472), ]
cdc_pretermbirth  <- cdc_pretermbirth [ ,-c(1, 3, 5)]
cdc_pretermbirth <- cdc_pretermbirth %>% rename("Events" = "Births")
MCH.CDC.Data.Total <- read.delim("MCH CDC Data Total.txt",  sep ="\t", dec=".", header = TRUE, stringsAsFactors = FALSE)
MCH.CDC.Data.Total <- MCH.CDC.Data.Total[,-c(1, 3, 5)]
MCH.CDC.Data.Total <- MCH.CDC.Data.Total %>% rename("total_birth" = "Births")

df1_pt <- full_join(cdc_pretermbirth , MCH.CDC.Data.Total, by=c("Year", "County"))

df1_pt[df1_pt$County == "Alameda County, CA", "County"] <-"alameda"
df1_pt[df1_pt$County == "Butte County, CA", "County"] <-"butte"
df1_pt[df1_pt$County == "Contra Costa County, CA", "County"] <-"contra costa"
df1_pt[df1_pt$County == "El Dorado County, CA", "County"] <-"el dorado"
df1_pt[df1_pt$County == "Fresno County, CA", "County"] <-"fresno"
df1_pt[df1_pt$County == "Humboldt County, CA", "County"] <-"humboldt"
df1_pt[df1_pt$County == "Imperial County, CA", "County"] <-"imperial"
df1_pt[df1_pt$County == "Kern County, CA", "County"] <-"kern"
df1_pt[df1_pt$County == "Kings County, CA", "County"] <-"kings"
df1_pt[df1_pt$County == "Los Angeles County, CA", "County"] <-"los angeles"
df1_pt[df1_pt$County == "Madera County, CA", "County"] <-"madera"
df1_pt[df1_pt$County == "Marin County, CA", "County"] <-"marin"
df1_pt[df1_pt$County == "Contra Costa County, CA", "County"] <-"mariposa"
df1_pt[df1_pt$County == "Merced County, CA", "County"] <-"merced"
df1_pt[df1_pt$County == "Monterey County, CA", "County"] <-"monterey"
df1_pt[df1_pt$County == "Napa County, CA", "County"] <-"napa"
df1_pt[df1_pt$County == "Orange County, CA", "County"] <-"orange"
df1_pt[df1_pt$County == "Placer County, CA", "County"] <-"placer"
df1_pt[df1_pt$County == "Riverside County, CA", "County"] <-"riverside"
df1_pt[df1_pt$County == "Sacramento County, CA", "County"] <-"sacramento"
df1_pt[df1_pt$County == "San Bernardino County, CA", "County"] <-"san bernardino"
df1_pt[df1_pt$County == "San Diego County, CA", "County"] <-"san diego"
df1_pt[df1_pt$County == "San Francisco County, CA", "County"] <-"san francisco"
df1_pt[df1_pt$County == "San Joaquin County, CA", "County"] <-"san joaquin"
df1_pt[df1_pt$County == "San Luis Obispo County, CA", "County"] <-"san luis obispo"
df1_pt[df1_pt$County == "San Mateo County, CA", "County"] <-"san mateo"
df1_pt[df1_pt$County == "Santa Barbara County, CA", "County"] <-"santa barbara"
df1_pt[df1_pt$County == "Santa Clara County, CA", "County"] <-"santa clara"
df1_pt[df1_pt$County == "Santa Cruz County, CA", "County"] <-"santa cruz"
df1_pt[df1_pt$County == "Shasta County, CA", "County"] <-"shasta"
df1_pt[df1_pt$County == "Solano County, CA", "County"] <-"solano"
df1_pt[df1_pt$County == "Sonoma County, CA", "County"] <-"sonoma"
df1_pt[df1_pt$County == "Stanislaus County, CA", "County"] <-"stanislaus"
df1_pt[df1_pt$County == "Tulare County, CA", "County"] <-"tulare"
df1_pt[df1_pt$County == "Ventura County, CA", "County"] <-"ventura"
df1_pt[df1_pt$County == "Yolo County, CA", "County"] <-"yolo"
df1_pt <- df1_pt %>% mutate(County = str_to_title(County))
df1_pt <- df1_pt %>% filter(!is.na("total_birth")) %>% filter(!is.na(Events)) %>% mutate(rate = Events/total_birth * 10^2)

# averagept <-df1_pt %>% select("County", "Year", "rate")
# pesticide_averagept_join <- averagept %>% inner_join(combined_pesticide_use, by = c("County" = "county", "Year" = "usage")) 


#Creating Leaflet
map <- readOGR(path.expand("cb_2018_us_county_20m.shp"),
               layer = "cb_2018_us_county_20m", stringsAsFactors = FALSE)
Statekey<-read.csv(paste0(getwd(),'/STATEFPtoSTATENAME_Key.csv'), colClasses=c('character'))
map<-merge(x=map, y=Statekey, by="STATEFP", all=TRUE)
SingleState <- subset(map, map$STATENAME %in% c(
    "California"
))

lbwdata_2016 <- lbwdata %>% filter(Year == "2016") %>% mutate(Rate = Events/Total.Births*100)
spatial_lbw <-sp::merge(x=SingleState, y=lbwdata_2016, by.x="NAME", by.y="County", by=x)

bins <- c(4.0,6.3,7.6,8.1, Inf)
pal <- colorBin(
    palette = "viridis",
    domain = spatial_lbw$Rate, n=7, bins=bins)



#datawrangling for 2007-2019 cdc low birth weight data
cdc_lowbirthweight <- read.delim("MCH CDC Data.txt",  sep ="\t", dec=".", header = TRUE, stringsAsFactors = FALSE)
cdc_lowbirthweight  <- cdc_lowbirthweight [-c(482:538), ]
cdc_lowbirthweight  <- cdc_lowbirthweight [ ,-c(1, 3, 5, 7)]
MCH.CDC.Data.Total <- read.delim("MCH CDC Data Total.txt",  sep ="\t", dec=".", header = TRUE, stringsAsFactors = FALSE)
MCH.CDC.Data.Total <- MCH.CDC.Data.Total[,-c(1, 3, 5)]
MCH.CDC.Data.Total %>% rename("Total Birth" = "Births")

#I noticed that the data I downloaded did not include total # of births so merging two datasets (one that has total # of birth counts and the other with low birth wegiht +very low birth weight counts)
df1 <- full_join(cdc_lowbirthweight , MCH.CDC.Data.Total, by=c("Year", "County"))
df1<- df1 %>% rename("cases" = "Births.x", "total_births" = "Births.y")

#Note: LBW = Low birth weight + Very low birth weight counts; Total Births = Total # of Birth
col_order <- c("Year", "County", "total_births",
               "cases", "Average.Birth.Weight", "Standard.Deviation.for.Average.Birth.Weight",
               "Average.Age.of.Mother", "Standard.Deviation.for.Average.Age.of.Mother","Average.LMP.Gestational.Age",
               "Standard.Deviation.for.Average.LMP.Gestational.Age")
df2 <- df1[,col_order]
df2[df2$County == "Alameda County, CA", "County"] <-"alameda"
df2[df2$County == "Butte County, CA", "County"] <-"butte"
df2[df2$County == "Contra Costa County, CA", "County"] <-"contra costa"
df2[df2$County == "El Dorado County, CA", "County"] <-"el dorado"
df2[df2$County == "Fresno County, CA", "County"] <-"fresno"
df2[df2$County == "Humboldt County, CA", "County"] <-"humboldt"
df2[df2$County == "Imperial County, CA", "County"] <-"imperial"
df2[df2$County == "Kern County, CA", "County"] <-"kern"
df2[df2$County == "Kings County, CA", "County"] <-"kings"
df2[df2$County == "Los Angeles County, CA", "County"] <-"los angeles"
df2[df2$County == "Madera County, CA", "County"] <-"madera"
df2[df2$County == "Marin County, CA", "County"] <-"marin"
df2[df2$County == "Contra Costa County, CA", "County"] <-"mariposa"
df2[df2$County == "Merced County, CA", "County"] <-"merced"
df2[df2$County == "Monterey County, CA", "County"] <-"monterey"
df2[df2$County == "Napa County, CA", "County"] <-"napa"
df2[df2$County == "Orange County, CA", "County"] <-"orange"
df2[df2$County == "Placer County, CA", "County"] <-"placer"
df2[df2$County == "Riverside County, CA", "County"] <-"riverside"
df2[df2$County == "Sacramento County, CA", "County"] <-"sacramento"
df2[df2$County == "San Bernardino County, CA", "County"] <-"san bernardino"
df2[df2$County == "San Diego County, CA", "County"] <-"san diego"
df2[df2$County == "San Francisco County, CA", "County"] <-"san francisco"
df2[df2$County == "San Joaquin County, CA", "County"] <-"san joaquin"
df2[df2$County == "San Luis Obispo County, CA", "County"] <-"san luis obispo"
df2[df2$County == "San Mateo County, CA", "County"] <-"san mateo"
df2[df2$County == "Santa Barbara County, CA", "County"] <-"santa barbara"
df2[df2$County == "Santa Clara County, CA", "County"] <-"santa clara"
df2[df2$County == "Santa Cruz County, CA", "County"] <-"santa cruz"
df2[df2$County == "Shasta County, CA", "County"] <-"shasta"
df2[df2$County == "Solano County, CA", "County"] <-"solano"
df2[df2$County == "Sonoma County, CA", "County"] <-"sonoma"
df2[df2$County == "Stanislaus County, CA", "County"] <-"stanislaus"
df2[df2$County == "Tulare County, CA", "County"] <-"tulare"
df2[df2$County == "Ventura County, CA", "County"] <-"ventura"
df2[df2$County == "Yolo County, CA", "County"] <-"yolo"
df2 <- df2 %>% filter(!is.na(total_births)) %>% filter(!is.na(cases)) %>% mutate(rate = cases/total_births * 10^2)
df2$County <- df2$County %>% str_to_title()


#Data wrangling for pesticide use trend 
county_ranks16 <- read_delim("table1_county_rank_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
repro_lbs16 <- read_delim("table3_reproductive_lbs_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
repro_acre16 <- read_delim("table4_reproductive_acres_2016.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

table1_2016 <- county_ranks16 %>% transmute(county = COUNTY, 
                                            lbs_2015 = LBS_2015, rank_2015 = RANK_2015, 
                                            lbs_2016 = LBS_2016, rank_2016 = RANK_2016)

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
table1_lbs$usage <- as.numeric(gsub("[^[:digit:]]+", "", table1_lbs$usage))

long_table2 <- table1_2016 %>% pivot_longer(!county, names_to = 'usage', values_to = "value")
table1_ranks_1516 <- long_table2 %>% filter(str_starts(usage, "rank"))
table1_lbs_1516<- long_table2 %>% filter(str_starts(usage, "lbs"))


table1_lbs_1516$usage <- as.numeric(gsub("[^[:digit:]]+", "", table1_lbs_1516$usage))
combined_pesticide_use <- table1_lbs %>% full_join(table1_lbs_1516) 
combined_pesticide_use <- combined_pesticide_use %>% group_by(usage) 
combined_pesticide_use <- combined_pesticide_use %>% arrange(usage)

averagebw <-df2 %>% select("County", "Year", "rate")
pesticide_averagebw_join <- averagebw %>% inner_join(combined_pesticide_use, by = c("County" = "county", "Year" = "usage")) 

averagept <-df1_pt %>% select("County", "Year", "rate")
pesticide_averagept_join <- averagept %>% inner_join(combined_pesticide_use, by = c("County" = "county", "Year" = "usage")) 


#shinyapp 
ui <- fluidPage(
    
    #FIRST TAB (LBW Map Data)
    h3("Exploration of Pesticides and Perinatal Outcomes in California County"),
    tabsetPanel(
        tabPanel("LBW Data Map Exploration in California County", icon = icon("map-pin"),
                 sidebarPanel(
                     radioButtons("outcomeinput", "Select a variable to display and click on a county to see its value:", 
                                  choiceNames = list("% Low Birth Weight", "% Preterm Birth"), 
                                  choiceValues = list("Rate", "rate_pt")),
                     p(strong("Low Birth Weight"), "is defined as babies who are born weighing less than 2500g. They 
                       are at greater risk of experiencing certain long-term complications later in life, including high blood pressure, diabetes, or 
                       developmental delay."),
                     br(),
                     p(strong("Preterm Birth"), "is defined as babies who are born before 37 weeks of pregnancy. Premature birth can also lead to 
                     short-term and long-term complications, including heart, GI, and immune system problems, and many chronic health issues.")
                                  
                       ), #closing sidebarpanel
                 mainPanel(
                     h5(textOutput("caption1")), 
                     leafletOutput("leafletmap")
                          
                    )),
        
        #SECOND TAB- BAR GRAPH comparing LBW and pesticide use by county 
        tabPanel("Comparison of California Low Birth Weight and Pesticide use by County", icon = icon("chart-bar"),
                 sidebarPanel( 
                     p("We compared uses of pesticide and low birth weight/preterm birth outcomes by county in California. The data spans from 2007 to 2016 
                       and we have only included counties that have a county population of >100,000."),
                     br(),
                     radioButtons("outcome2input", "Select a variable to display:", 
                                  choiceNames = list("% Low Birth Weight", "% Preterm Birth"), 
                                  choiceValues = list("Rate", "rate_pt")),
                     #selectizeInput("countyInput", "Choose a County", choices = unique(pesticide_averagebw_join$County), multiple = TRUE, options = list(maxItems = 6)),
                     sliderInput("year2Input", "Year", min = 2007, max = 2016, value = 2007, step = 1, 
                                 sep = "", ticks = FALSE, animate = TRUE),
                 ), #closing sidebarpanel 
                mainPanel(
                    plotOutput("bargraph"),
                    plotOutput("bargraph_pesticide")
                    )#closing mainpanel
                 )#closing tabpanel
    )#closing tabsetpanel
)#closing fluidpage

server <- function(input, output) {
    
    summary_2 <- reactive({
        pesticide_averagebw_join %>% 
            filter(Year %in% input$year2Input)})
    
    summary <- reactive({
        pesticide_averagept_join %>% 
            filter(Year %in% input$year2Input)})
    
    output$bargraph <- renderPlot(
        if(input$outcome2input == "Rate"){
            summary_2() %>% 
                ggplot(aes(County, rate)) + geom_col() + ylab("Low Birth Weight Rate (%)") +xlab("") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1/2)) 
            
        }else{
            summary() %>% 
                ggplot(aes(County, rate)) + geom_col() + ylab("Preterm Birth Rate (%)") +xlab("") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1/2)) 
        }
            
        )
       
    
    output$bargraph_pesticide <- renderPlot(
        summary_2 () %>% 
            ggplot(aes(County, value)) + geom_col() + ylab("Pesticide Use (Pounds)") +xlab("") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1/2)))

    
    output$leafletmap <- renderLeaflet({
        if(input$outcomeinput == "Rate"){
        
        leaflet(spatial_lbw, options = leafletOptions(zoomControl = TRUE, zoomLevelFixed = FALSE, dragging=TRUE, minZoom = 5.3, maxZoom = 9)) %>% 
            setView(lat = 36.778259, lng = -119.417931, zoom = 6) %>%
            addPolygons(color = "Black", weight = 1, smoothFactor = 0.5, 
                        opacity = 1.0, fillOpacity = 0.5, layerId = ~NAME,
                        fillColor = ~pal(Rate), 
                        popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",spatial_lbw$NAME,"</font></center>","<b>% of Low Birth Weight Births: </b>", sprintf("%1.2f%%", spatial_lbw$Rate),"<br/>"))) %>%
            addLegend(pal = pal, values = spatial_lbw$Rate, opacity = 1, title="% Low Birth Weight (Quartiles)")
        }else {
                leaflet(spatial_pt, options = leafletOptions(zoomControl = TRUE, zoomLevelFixed = FALSE, dragging=TRUE, minZoom = 5.3, maxZoom = 9)) %>% 
                    setView(lat = 36.778259, lng = -119.417931, zoom = 6) %>%
                    addPolygons(color = "Black", weight = 1, smoothFactor = 0.5, 
                                opacity = 1.0, fillOpacity = 0.5, layerId = ~NAME,
                                fillColor = ~pal2(rate_pt), 
                                popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",spatial_pt$NAME,"</font></center>","<b>% of Preterm Birth: </b>", sprintf("%1.2f%%", spatial_pt$rate_pt),"<br/>"))) %>%
                    addLegend(pal = pal2, values = spatial_pt$rate_pt, opacity = 1, title="% Preterm Birth (Quartiles)")
            
        }
    })

    output$caption1 <- renderText({"2016 Choropleth Map of California County"})    
       
    
    

    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
