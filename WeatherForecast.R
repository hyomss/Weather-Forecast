#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#DATA: https://openweathermap.org/forecast5
#a83a69e0ab6f56144bdea639f108e78e
#time: UTC
#rain.3h: Rain volume for last 3 hours, mm
#list.clouds.all: Cloudiness, %
#

#OPEN WEATHER DATA CLEANING 
#PREDICTION REGION OF INTEREST : CHAMPAIGN
library(httr)
library(tidyverse)
lat <- "40.116421"
lon <- "-88.243385"
mykey <- "a83a69e0ab6f56144bdea639f108e78e"
path <- sprintf("https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s", lat,lon,mykey)

result <- GET(url = path)
result2 <- content(result, as = "text", encoding = "UTF-8")
weather <- jsonlite::fromJSON(result2,flatten = TRUE)

City<- weather$city$name #Champaign

cmi_weather <- select(weather$list, -c(visibility, weather, pop,dt,main.pressure,main.sea_level,main.grnd_level,wind.deg,wind.gust,sys.pod,main.temp_kf))
cmi_weather['time'] <- as.POSIXct(cmi_weather$dt_txt, tz = "UTC")
cmi_weather$rain.3h <- replace(cmi_weather$rain.3h, is.na(cmi_weather$rain.3h), 0)
date_line <- which(diff(as.Date(cmi_weather$time))!=0)[-5] #index: date change-1

#GROUPBY DATES
cmi_weather['Date'] <-format(cmi_weather$time, "%Y-%m-%d")
stat_data <- cmi_weather %>%
  group_by(Date) %>%
  summarise(
    Min = as.integer(round(min(main.temp_min))),
    Max = as.integer(round(max(main.temp_max))),
    Avg = as.integer(round((Min+Max)/2))
  )

#BUILD SHINY APP 
library(shiny)
library(tidyverse)

ui <- fluidPage(
  div(style = "text-align: center;", 
      titlePanel(paste(City, 'Weather Forecast')),
      h5("This is a 5-day weather forecast for Champaign, IL")), 
  fluidRow(
    column(3, actionButton("temp", "Temperature(F)", style = "width:100%; background-color: #FB9DA7; color: #ffffff; border-color: #FB9DA7;")),
    column(3, actionButton("wind", "Wind Speed(meter/sec)", style = "width:100%;background-color: #FEC868; color: #ffffff; border-color: #FEC868;")),
    column(3, actionButton("cloud", "Cloudiness(%)", style = "width:100%;  background-color: #C2CD87; color: #ffffff; border-color: #C2CD87;")),
    column(3, actionButton("rain", "Rain(mm/3hrs)", style = "width:100%; background-color: skyblue; color: #ffffff; border-color: skyblue;"))
  ),
  p(),
  fluidRow(
    column(9, uiOutput("dynamicPlot")),
    column(3, h4("Daily Temperature"),tableOutput("stat_table"))
  )
)




server <- function(input, output) {
  #PLOT
  plotType <- reactiveVal("none")
  observeEvent(input$temp, {plotType("temp")})
  observeEvent(input$wind, {plotType("wind")})
  observeEvent(input$cloud, {plotType("cloud")})
  observeEvent(input$rain, {plotType("rain")})
  
  output$dynamicPlot <- renderUI({
    switch(plotType(),
           "temp" = plotOutput("weatherPlot"),
           "wind" = plotOutput("windPlot"),
           "cloud" = plotOutput("cloudPlot"),
           "rain" = plotOutput("rainPlot"),
           #tags$img(src = "https://images.rawpixel.com/image_800/czNmcy1wcml2YXRlL3Jhd3BpeGVsX2ltYWdlcy93ZWJzaXRlX2NvbnRlbnQvbHIvZnJqaWxsd2VsbGluZ3RvbjAwMDQyLWltYWdlLWt3eXNxM2VyLmpwZw.jpg", style = "width: 100%")
           tags$h4('Click Buttons Above to See Detail Here',style = "color: gray; text-align: center;")
    )
  })
  
  output$weatherPlot <- renderPlot({
    g1 <- ggplot(data = cmi_weather, aes(x = time)) + 
      geom_line(aes(y = main.feels_like, color = "Feels Like"), linewidth=2) +
      geom_line(aes(y = main.temp, color = "Actual")) +
      scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks = "6 hour") +
      xlab("")+ylab("Temperature(F)") + 
      theme(
        panel.grid.major = element_blank(),  
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        panel.grid = element_blank(), 
        # plot.background = element_rect(fill = "#FFE4C9"),
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 1, color='grey'),
        legend.text = element_text(size=13), 
        legend.title = element_blank()
      )+
      scale_color_manual(values= c( "Actual" = "#728FB4","Feels Like" = "#FB9DA7"))
    for(i in date_line) {
      g1 <- g1 + geom_vline(xintercept = as.numeric(cmi_weather$time[i+1]), color= "grey")
    }
    print(g1)
  })
  
  output$windPlot <- renderPlot({
    g2<- ggplot(data = cmi_weather, aes(x = time, y = wind.speed)) + 
      geom_line(linewidth=2, color ='#FEC868') +
      scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks ="6 hour") +
      xlab("")+ylab("Wind Speed(m/s)") + 
      theme(
        panel.grid.major = element_blank(), 
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14) ,
        panel.grid = element_blank(), 
        # plot.background = element_rect(fill = "#FFE4C9"),
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 1, color='grey'))
    for(i in date_line) {
      g2 <- g2 + geom_vline(xintercept = as.numeric(cmi_weather$time[i+1]), color="grey")
    }
    print(g2)
  })
  output$cloudPlot <- renderPlot({
    g3<-ggplot(data = cmi_weather, aes(x = time, y = clouds.all)) + 
      geom_line(linewidth=2, color ='#C2CD87') +
      scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks ="6 hour")+
      xlab("")+ylab("Cloudiness(%)") + 
      theme(
        panel.grid.major = element_blank(), 
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14) ,
        panel.grid = element_blank(), 
        #plot.background = element_rect(fill = "#FFE4C9")
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 1, color='grey')
      )
    for(i in date_line) {
      g3<- g3 + geom_vline(xintercept = as.numeric(cmi_weather$time[i+1]), color="grey")
    }
    print(g3)
  })
  
  output$rainPlot <- renderPlot({
    g4<-ggplot(data = cmi_weather, aes(x = time, y = rain.3h)) +
      geom_line(linewidth=2, color ='skyblue') +
      scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks ="6 hour")+
      xlab("")+ylab("Rain(mm/3hrs)") +
      theme(
        panel.grid.major = element_blank(), 
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14) ,
        panel.grid = element_blank(), 
        #plot.background = element_rect(fill = "#FFE4C9")
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 1, color='grey')
      )
    for(i in date_line) {
      g4<- g4 + geom_vline(xintercept = as.numeric(cmi_weather$time[i+1]), color="grey")
    }
    print(g4)
  })
  
  #STAT TABLE
  output$stat_table <- renderTable({
    stat_data
  })
}

shinyApp(ui = ui, server = server)