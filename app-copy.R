#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  
  # Application title
  titlePanel("Red Sox and Celtics Attendance Analysis"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c(  "Baseball_Attendance&Weather",
                              "Baseball_Attendance&Weather2017",
                              "Baseball_Attendance&Weather2013",
                              "Baseball_Attendance&Opponent",
                              "Baseball_Attendance&Opponent2017",
                              "Baseball_Attendance&Opponent2013",
                              "Baseball_Attendance&SpecialWeather",
                              "Basketball_Attendance_Weather"
                              # "Basketball_Attendance_Players"
                )),
 
    numericInput("obs", "Number of observations to view:", 10),
    textInput("summary", "Summary:","")
    #checkboxInput("outliers", "Show outliers", FALSE)
    
    ),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("baseball_plot"),
    h4("Observations"),
    tableOutput("view")
  )
  )

Baseball_Attendance_weather<-read.csv("baseball00.csv")  
baseball_alltime<-Baseball_Attendance_weather
baseball_2017<-Baseball_Attendance_weather %>% filter(Year == 2017)
baseball_opp <- baseball_2017 %>% group_by(Opp) %>% summarise(avg_attendance = mean(Attendance))
baseball_opp <- arrange(baseball_opp, desc(avg_attendance))
baseball_2013<-Baseball_Attendance_weather %>% filter(Year == 2013)
baseball_oppo <- baseball_2013 %>% group_by(Opp) %>% summarise(avg_attendance = mean(Attendance))
baseball_oppo <- arrange(baseball_oppo, desc(avg_attendance))
baseball_SpecialWeather<-read.csv("Baseball_weather.csv") %>% select(Gm.,Year,DATE, Tm, Time, Attendance,SNOW,PRCP )
baseball_opponent <- baseball %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),digits = 0))
baseball_opponent <- arrange(baseball_opponent, desc(avg_attendance))

Basketball_Attendance_weather<-read.csv("basketball_weather.csv")
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Baseball_Attendance&Weather"=baseball_alltime,
           "Baseball_Attendance&Weather2017"=baseball_2017,
           "Baseball_Attendance&Weather2013"=baseball_2013,
           "Baseball_Attendance&Opponent"=baseball_opponent,
           "Baseball_Attendance&Opponent2017"=baseball_opp,
           "Baseball_Attendance&Opponent2013"=baseball_oppo,
           "Baseball_Attendance&SpecialWeather"=baseball_SpecialWeather,
           "Basketball_Attendance_Weather"=Basketball_Attendance_weather)
    #"Basketball_Attendance_Players"=basketball_Attendance_Players
  })
  
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
 
  output$caption <- renderPrint(
    if(input$dataset=="Baseball_Attendance&Weather"){
paste0("There appears to be a weak but 
      slightly positive relationship between  average 
      temperature and attendance over the past six seasons")
    }
    else if(input$dataset=="Baseball_Attendance&Weather2017"){
      paste0("222")
    }
  )
    
  # get_data<-reactive({
  #   if(!exists(input$dataset)) return()
  #   obj<-list(data=get(input$dataset),
  #             tempa=input$year)
  #             
  ##   if(check(obj)) return()
  #   obj
  # })
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  # data<-reactive(
  # if(
  #)
  
  output$baseball_plot <- renderPlot({
    n <- input$n
    if(input$dataset=="Baseball_Attendance&Weather"){
      ggplot(baseball_alltime, mapping = aes(x = TAVG, y = Attendance)) +
        geom_point(mapping = aes(color = Year)) +
        geom_smooth()+ggtitle("Temperature vs. Attendance over all")
    }
    else if( input$dataset=="Baseball_Attendance&SpecialWeather"){
      library(gridExtra)
      plot1<-ggplot(baseball_SpecialWeather, aes(PRCP, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Precipitation vs. Attendance (2013-2017)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Precipitation on Game Day (cm)") + 
        ylab("Total Attendance")
      # The snow influences on Red Sox attendence
      plot2<-ggplot(baseball_SpecialWeather, aes(SNOW, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Snow vs. Attendance (2013-2017)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Whether snow on Game Day ") + 
        ylab("Total Attendance")
      grid.arrange(plot1,plot2,ncol=2)
    }
    
    else if(input$dataset=="Baseball_Attendance&Weather2013"){
      ggplot(baseball_2013, aes(x=TAVG,y= Attendance)) + geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + ggtitle("Temperature vs. Attendance in 2012")
    }
    
    else if(input$dataset=="Baseball_Attendance&Weather2017"){
      ggplot(baseball_2017, aes(x=TAVG,y= Attendance)) + geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + ggtitle("Temperature vs. Attendance in 2017")
    }
    else if(input$dataset=="Baseball_Attendance&Opponent"){
      ggplot(baseball_opponent, aes(Opp, avg_attendance)) +
        geom_bar(stat = "identity") + ggtitle("Average Attendance vs. Opponents in Six seasons") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    
    else if(input$dataset=="Baseball_Attendance&Opponent2017"){
      ggplot(baseball_opp, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average attendance vs. opponents in 2017")
    }
    else if(input$dataset=="Baseball_Attendance&Opponent2013"){
      ggplot(baseball_oppo, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average attendance vs. opponents in 2013")
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
