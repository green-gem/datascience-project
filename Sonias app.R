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

#data wrangling for 5-year trend (2014-2018) 
lbwdata<-read.csv(paste0(getwd(),"/low-and-very-low-birthweight-by-county-2014-2018 (1).csv"), header = TRUE, stringsAsFactors = FALSE)
lbwdata <- lbwdata %>% mutate(County = str_to_title(County))
lbwdata$Events[is.na(lbwdata$Events)] <- 0
lbwdata <- lbwdata %>% group_by(Year, County, Total.Births) %>% summarize(Events = sum(Events)) 
lbwdata <- lbwdata %>% filter(!County == "california") 
CaliforniaCounty <- map_data("county", "california")
CaliforniaCounty <- CaliforniaCounty %>% mutate(subregion = str_to_title(subregion))
head(CaliforniaCounty)
california_map <- california_map %>% mutate(County = str_to_title(County))
california_map <- lbwdata %>% full_join(CaliforniaCounty, by = c("County" = "subregion")) %>% mutate(Rate = Events/Total.Births * 10^2)

#shinyapp 
ui <- fluidPage(
    theme=shinythemes::shinytheme("slate"),
    titlePanel("Percent of Low Birth Weight (<2500g) in California"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput("countyInput", "Choose a County", choices = unique(california_map$County), selected = "Los Angeles", multiple = TRUE, options = list(maxItems = 6)),
            sliderInput("yearInput", "Year", min = 2014, max = 2018, value = 2014, step = 1, 
                                      sep = "", ticks = FALSE, animate = TRUE),
           ), #closing sidebarpanel
        mainPanel(plotOutput("lbwheatmap"),
                  br(), br(),
                  plotOutput("bargraph"), 
                  verbatimTextOutput("rate")
                 )
        )#closing sidebarlayout
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
        california_map %>% 
            filter(Year %in% input$yearInput) %>% filter(County %in% input$countyInput)})
    output$rate <- renderPrint({
        aggregate(Rate ~ County, data = b(), sum)
    })
    

    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
