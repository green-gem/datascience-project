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
#Creating Leaflet
map <- readOGR(path.expand("cb_2018_us_county_20m.shp"),
               layer = "cb_2018_us_county_20m", stringsAsFactors = FALSE)
Statekey<-read.csv(paste0(getwd(),'/STATEFPtoSTATENAME_Key.csv'), colClasses=c('character'))
map<-merge(x=map, y=Statekey, by="STATEFP", all=TRUE)
SingleState <- subset(map, map$STATENAME %in% c(
    "California"
))

lbwdata_2018 <- lbwdata %>% filter(Year == "2018") %>% mutate(Rate = Events/Total.Births*100)
SingleState<-sp::merge(x=SingleState, y=lbwdata_2018, by.x="NAME", by.y="County", by=x)

pal <- colorNumeric(
    palette = "Spectral",
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


#data wrangling for 5-year trend (2014-2018) 
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

#shinyapp 

ui <- fluidPage(
    h3("Exploration of Pesticides and Perinatal Outcomes in California County"),
    tabsetPanel(
        tabPanel("LBW Map Exploration data in California County", icon = icon("map-pin"),
                 sidebarPanel(
                     p(strong("Low Birth Weight"), "is defined as babies who are born weighing less than 2500g. They 
                       are at greater risk of experiencing certain long-term complications later in life, including high blood pressure, diabetes, or 
                       developmental delay."),
                     br(),
                     br(),
                     strong("2018 Map Exploration"),
                     p("On the top, we see a map of percentage of low birth weight (Year 2018) in the county of California. Please click on each county to see the county-specific percentage of low birth weight."), 
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
                 mainPanel(leafletOutput("leafletmap"),
                           br(), br(),
                           plotOutput("lbwheatmap"))),
        tabPanel("California LBW Bar Graph Trend from 2014-2018", icon = icon("chart-bar"),
                 sidebarPanel(
                     selectizeInput("countyInput", "Choose a County", choices = unique(california_map$County), selected = "Los Angeles", multiple = TRUE, options = list(maxItems = 6)),
                     sliderInput("yearInput", "Year", min = 2014, max = 2018, value = 2014, step = 1, 
                                 sep = "", ticks = FALSE, animate = TRUE),
                 ), #closing sidebarpanel 
                mainPanel(plotOutput("bargraph"), 
                               verbatimTextOutput("rate"), class = 'leftAlign'
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
    
    output$bargraph <- renderPlot(
        summary () %>% 
            filter(!is.na(Rate)) %>% 
            ggplot(aes(County, Rate)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1/2))
    )
    
    b <- reactive({
        lbwdata %>% 
            filter(Year %in% input$yearInput) %>% filter(County %in% input$countyInput)})
    output$rate <- renderPrint({
        aggregate(Rate~County, data = b(), sum)
    })
    
 
    
    output$leafletmap <- renderLeaflet({
        leaflet(SingleState, options = leafletOptions(zoomControl = TRUE, zoomLevelFixed = FALSE, dragging=TRUE, minZoom = 5.3, maxZoom = 6)) %>% 
            setView(lat = 36.778259, lng = -119.417931, zoom = 6) %>%
            addPolygons(color = "Black", weight = 1, smoothFactor = 0.5, 
                        opacity = 1.0, fillOpacity = 0.5, layerId = ~NAME,
                        fillColor = ~pal(Rate), 
                        popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",SingleState$NAME,"</font></center>","<b>% of Low Birth Weight Births: </b>", sprintf("%1.2f%%", SingleState$Rate),"<br/>"))) %>%
            addLegend(pal = pal, values = SingleState$Rate, opacity = 1, title="% Low Birth Weight")
        
    })
        
    
    

    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
