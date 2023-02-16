library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(caret)

crimes <- read.csv("./Crimes_2020.csv", header=TRUE, sep=",")
crimes$crimeDate = as.Date(crimes$Date, "%m/%d/%Y")
crimes$month = strftime(as.Date(crimes$Date, "%m/%d/%Y"),"%b")
crimes$crimeHour = format(as_datetime(crimes$Date, format="%m/%d/%Y %I:%M:%S %p"), format="%H")

crimeTypes <- unique(crimes$Primary.Type)
crimesWithLatLong <- crimes %>% filter(Latitude != "")
crimesByHour = crimes %>%
    group_by(Primary.Type, crimeHour) %>%
    summarize(n= n())

ui <- {
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Crimes by month", icon = icon("calendar"), tabName = "crimesByMonth"),
            menuItem("Crimes by location", icon = icon("map"), tabName = "crimesByLocation"),
            menuItem("Crimes by hour", icon = icon("hourglass", lib="glyphicon"), tabName = "crimesByHour"),
            menuItem("Observations", icon = icon("search"), tabName = "observations")
        )
    )
    
    body <- dashboardBody(
        tabItems(
            
          
            tabItem(tabName = "crimesByMonth",
                    h2("Frequency of crime type by month"),
                    selectInput(inputId = 'crimes',
                                label='Select the crime from dropdown',
                                choices=c(crimeTypes),
                                selected='ARSON'),
                    plotOutput("monthPlot")
            ),
            
            
            tabItem(tabName = "crimesByLocation",
                    h2("Location of crimes by date on a map"),
                    dateInput('crimeDate', 'Select the date crime occured',
                              value ='2020-02-12'),
                    # todo: get this from data
                    #min='', max=''),
                    leafletOutput("locationPlot")
            ),
            
            
            tabItem(tabName = "crimesByHour",
                    h2("Type of the crime and the hour of the day when the crime was committed"),
                    plotOutput("hourPlot")
            ),
            
            
            tabItem(tabName = "observations",
                    h2("Common crimes in different communities"),
                    dataTableOutput("crimeLocation")
            )
            
        )
    )
    
    dashboardPage(
        dashboardHeader(title = "Chicago Crimes"),
        sidebar,
        body
    )
}

server <- function(input, output) {
 
    
       
    output$monthPlot <- renderPlot({
        
        by_month <- 
            crimes %>% 
            filter(Primary.Type == input$crimes) %>% 
            group_by(month) %>% 
            summarize(NumberOfCrimes= n())
        
        ggplot(by_month, aes(month, NumberOfCrimes, fill=month)) + geom_bar(stat = "identity")  +
            scale_x_discrete(labels=month.abb)
    })
    
    
    
    output$locationPlot <- renderLeaflet({
        crimesOnDate = crimesWithLatLong %>% filter(crimeDate == input$crimeDate)
        leaflet(crimesOnDate, width = "100%") %>% addTiles() %>%
            addMarkers(lng = ~Longitude, lat = ~Latitude, clusterOptions = markerClusterOptions()) %>%
            addLayersControl(
                baseGroups = c("OSM (default)"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    


    
    output$hourPlot <- renderPlot({
            crimesByHour %>%
            ggplot(aes(crimeHour, Primary.Type, fill=n)) + 
            geom_tile() + 
            scale_fill_gradient(low="#A0A6BE", high="red") 
    })
    
    
    
    
    output$crimeLocation <- renderDataTable({
        crimes %>% 
            group_by(Location.Description, Primary.Type) %>%
            summarise(CrimesCount=n())
    })
}

shinyApp(ui = ui, server = server)