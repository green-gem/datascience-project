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

#data wrangling for 5-year lowbirth weight trend (2014-2018) 
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

#Creating Leaflet
map <- readOGR(path.expand("cb_2018_us_county_20m.shp"),
               layer = "cb_2018_us_county_20m", stringsAsFactors = FALSE)
Statekey<-read.csv(paste0(getwd(),'/STATEFPtoSTATENAME_Key.csv'), colClasses=c('character'))
map<-merge(x=map, y=Statekey, by="STATEFP", all=TRUE)
SingleState <- subset(map, map$STATENAME %in% c(
    "California"
))

lbwdata_2016 <- lbwdata %>% filter(Year == "2016") %>% mutate(Rate = Events/Total.Births*100)
SingleState<-sp::merge(x=SingleState, y=lbwdata_2016, by.x="NAME", by.y="County", by=x)

pal <- colorNumeric(
    palette = "viridis",
    domain = SingleState$Rate, n=7)

leafletmap <- leaflet(SingleState,options = leafletOptions(zoomControl = FALSE, zoomLevelFixed = FALSE, dragging=TRUE, minZoom = 3, maxZoom = 6)) %>% 
    setView(lat = 36.778259, lng = -119.417931, zoom = 5) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                opacity = 1.0, fillOpacity = 0.5, layerId = ~NAME,
                fillColor = ~pal(Rate),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE), 
                popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",SingleState$NAME,"</font></center>","<b>% of Low Birth Weight Births: </b>", sprintf("%1.2f%%", SingleState$Rate),"<br/>"))) %>%
    addLegend(pal = pal, values = SingleState$Rate, opacity = 1)



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
table1_lbs_1516$usage <- as.numeric(gsub("[^[:digit:]]+", "", table1_lbs_1516$usage))
combined_pesticide_use <- table1_lbs %>% full_join(table1_lbs_1516) 
combined_pesticide_use <- combined_pesticide_use %>% group_by(usage) 
combined_pesticide_use <- combined_pesticide_use %>% arrange(usage)

averagebw <-df2 %>% select("County", "Year", "rate")
pesticide_averagebw_join <- averagebw %>% inner_join(combined_pesticide_use, by = c("County" = "county", "Year" = "usage")) 


#shinyapp 
ui <- fluidPage(
    
    #FIRST TAB (LBW Map Data)
    h3("Exploration of Pesticides and Perinatal Outcomes in California County"),
    tabsetPanel(
        tabPanel("LBW Data Map Exploration in California County", icon = icon("map-pin"),
                 sidebarPanel(
                     p(strong("Low Birth Weight"), "is defined as babies who are born weighing less than 2500g. They 
                       are at greater risk of experiencing certain long-term complications later in life, including high blood pressure, diabetes, or 
                       developmental delay. According to CDC, as of 2018, the national percentage of low birthweight rate is 8.28%"),
                     br(),
                     br(),
                     strong("2016 Map Exploration"),
                     p("On the top, we see a map of percentage of low birth weight (Year 2016) in the county of California. Please click on a county to see its specific percentage of low birth weight."), 
                     br(),
                     br(),
                     br(),
                     br(),
                     strong("2014-2018 HeatMap Exploration"),
                      p("The second map is a heatmap of low birth weight in the county of California from 2014-2018. Please click on the play button to see the trend in
                        percentage of low birth weight across 5 years (2014-2018)."), 
                     br(),
                     br(),
                     sliderInput("yearInput", "Year", min = 2014, max = 2018, value = 2014, step = 1, 
                                 sep = "", ticks = FALSE, animate = TRUE)
                       ), #closing sidebarpanel
                 mainPanel(
                     h5(textOutput("caption1")), 
                     leafletOutput("leafletmap"),
                           br(), br(),
                     h5(textOutput("caption2")),
                           plotOutput("lbwheatmap"))),
        
        #SECOND TAB- BAR GRAPH comparing LBW and pesticide use by county 
        tabPanel("Comparison of California Low Birth Weight and Pesticide use by County", icon = icon("chart-bar"),
                 sidebarPanel( 
                     p("We compared uses of pesticide and low birth weight outcomes by county in California. The data spans from 2007 to 2016 
                       and we have only included counties that have a county population of >100,000."),
                     br(),
                     p("Note: The county-specific rates and values are displayed in the table below"),
                     selectizeInput("countyInput", "Choose a County", choices = unique(pesticide_averagebw_join$County), multiple = TRUE, options = list(maxItems = 6)),
                     sliderInput("year2Input", "Year", min = 2007, max = 2016, value = 2007, step = 1, 
                                 sep = "", ticks = FALSE, animate = TRUE),
                 ), #closing sidebarpanel 
                mainPanel(
                    plotOutput("bargraph"),
                          plotOutput("bargraph_pesticide"), 
                               tableOutput("descriptiontable")
                 )#closing mainpanel
                 )#closing tabpanel
    )
)#closing fluidpage

server <- function(input, output) {
    
    summary <- reactive({
        california_map %>% 
            filter(Year %in% input$yearInput)})
    
    # 5-year trend in heatmap (2014-2018)       
    output$lbwheatmap <- renderPlot (
        summary() %>%
            ggplot(aes(long, lat, group = group)) + 
            geom_polygon(aes(fill = Rate), color = "black") +
            scale_fill_viridis_c(name = "% Low Birth Weight \n(<2500g)", option = "B", limits=c(0,15), breaks = c(2,4,6,8,10,12,14)) +
            theme(panel.grid.major = element_blank(), 
                  panel.background = element_blank(),
                  axis.title = element_blank(), 
                  axis.text = element_blank(),
                  axis.ticks = element_blank()) +
            coord_fixed(1.3) 
    )
    
    summary_2 <- reactive({
        pesticide_averagebw_join %>% 
            filter(Year %in% input$year2Input)})
    
    output$bargraph <- renderPlot(
        summary_2() %>% 
            ggplot(aes(County, rate)) + geom_col() + ylab("Low Birth Weight Rate (%)") +xlab("") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1/2))) 
    
    output$bargraph_pesticide <- renderPlot(
        summary_2 () %>% 
            ggplot(aes(County, value)) + geom_col() + ylab("Pesticide Use (Pounds)") +xlab("") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1/2))) 

    a <- reactive({
        pesticide_averagebw_join %>% 
            filter(County %in% input$countyInput,
                   Year %in% input$yearInput)})
    
    output$descriptiontable <- renderTable({a() %>% select("County" = County, "Low Birth Weight Infants(%)" = rate, "Pesticide Use (Pounds)" = value)
    })
    
 
    output$leafletmap <- renderLeaflet({
        leaflet(SingleState, options = leafletOptions(zoomControl = TRUE, zoomLevelFixed = FALSE, dragging=TRUE, minZoom = 5.3, maxZoom = 9)) %>% 
            setView(lat = 36.778259, lng = -119.417931, zoom = 6) %>%
            addPolygons(color = "Black", weight = 1, smoothFactor = 0.5, 
                        opacity = 1.0, fillOpacity = 0.5, layerId = ~NAME,
                        fillColor = ~pal(Rate), 
                        popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",SingleState$NAME,"</font></center>","<b>% of Low Birth Weight Births: </b>", sprintf("%1.2f%%", SingleState$Rate),"<br/>"))) %>%
            addLegend(pal = pal, values = SingleState$Rate, opacity = 1, title="% Low Birth Weight")
        
    })

    output$caption1 <- renderText({"2016 Choropleth Map of California County"})    
    output$caption2 <- renderText({"2014-2018 HeatMap of California County"})    
    

    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
