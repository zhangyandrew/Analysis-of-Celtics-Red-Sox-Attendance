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
                choices = c(
                              "Baseball_Attendance&TemperatureByYear",
                              "Baseball_Attendance&OpponentByYear",
                              "Baseball_Attendance&SpecialWeatherByYear",
                              "Basketball_Attendance&TemperatureByYear",
                              "Basketball_Attendance&OpponentByYear",
                              "Basketball_Attendance&SpecialWeatherByYear"
                )),
    sliderInput("Year", "Year:", 
                min=2012, max=2017, value=2012),
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

baseball_alltime <- read.csv("baseball_weather.csv", header = T) %>% filter(X.1 != "@")
baseball11 <- read.csv("baseball_weather.csv", header = T) %>% filter(X.1 != "@")
baseball <- select(baseball_alltime, Gm., Year, DATE, X, Tm, Opp, W.L, Win, Loss, Save, Time, D.N, Attendance, TAVG, TMAX, TMIN)
baseball_2017 <- baseball %>% filter(Year == 2017)
baseball_2016 <- baseball %>% filter(Year == 2016)
baseball_2015 <- baseball %>% filter(Year == 2015)
baseball_2014 <- baseball %>% filter(Year == 2014)
baseball_2013 <- baseball %>% filter(Year == 2013)
baseball_opp <- baseball %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),digits = 0))
baseball_opp <- arrange(baseball_opp, desc(avg_attendance))
baseball_opp17 <- baseball_2017 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),,digits = 0))
baseball_opp17 <- arrange(baseball_opp, desc(avg_attendance))
baseball_opp16 <- baseball_2016 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),digits = 0))
baseball_opp16 <- arrange(baseball_opp16, desc(avg_attendance))
baseball_opp15 <- baseball_2015 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),digits = 0))
baseball_opp15 <- arrange(baseball_opp15, desc(avg_attendance))
baseball_opp14 <- baseball_2014 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),,digits = 0))
baseball_opp14 <- arrange(baseball_opp14, desc(avg_attendance))
baseball_opp13 <- baseball_2013 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),digits = 0))
baseball_opp13<- arrange(baseball_opp, desc(avg_attendance))

baseball_SpecialWeather<- baseball11 %>% select(Gm.,Year,DATE, Tm, Time, Attendance,SNOW,PRCP )%>% filter(PRCP!=0)
baseball_SpecialWeather17 <- baseball_SpecialWeather %>% filter(Year == 2017)
baseball_SpecialWeather16 <- baseball_SpecialWeather %>% filter(Year == 2016)
baseball_SpecialWeather15 <- baseball_SpecialWeather %>% filter(Year == 2015)
baseball_SpecialWeather14 <- baseball_SpecialWeather %>% filter(Year == 2014)
baseball_SpecialWeather13 <- baseball_SpecialWeather %>% filter(Year == 2013)

bball<-read.csv("bball.csv")
bball$Year <- factor(bball$Year)
bball$Opp <- factor(bball$Opp)
bball_2017 <- bball %>% filter(Year == 2017)
bball_2016 <- bball %>% filter(Year == 2016)
bball_2015 <- bball %>% filter(Year == 2015)
bball_2014 <- bball %>% filter(Year == 2014)
bball_2013 <- bball %>% filter(Year == 2013)

bball_opp <- bball %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),digits = 0))
bball_opp <- arrange(bball_opp, desc(avg_attendance))
bball_opp17 <- bball_2017 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),,digits = 0))
bball_opp17 <- arrange(bball_opp17, desc(avg_attendance))
bball_opp16 <- bball_2016 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),,digits = 0))
bball_opp16 <- arrange(bball_opp16, desc(avg_attendance))
bball_opp15 <- bball_2015 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),,digits = 0))
bball_opp15 <- arrange(bball_opp15, desc(avg_attendance))
bball_opp14 <- bball_2014 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),,digits = 0))
bball_opp14 <- arrange(bball_opp14, desc(avg_attendance))
bball_opp13 <- bball_2013 %>% group_by(Opp) %>% summarise(avg_attendance = round(mean(Attendance),,digits = 0))
bball_opp13 <- arrange(bball_opp13, desc(avg_attendance))

basketball_WeatherAll<-read.csv("bball.csv")
basketball_SpecialWeather<- basketball_WeatherAll %>% select(Year,Date, Attendance,AWND,PRCP )
basketball_SpecialWeather17 <- basketball_SpecialWeather %>% filter(Year == 2017)
basketball_SpecialWeather16 <- basketball_SpecialWeather %>% filter(Year == 2016)
basketball_SpecialWeather15 <- basketball_SpecialWeather %>% filter(Year == 2015)
basketball_SpecialWeather14 <- basketball_SpecialWeather %>% filter(Year == 2014)
basketball_SpecialWeather13 <- basketball_SpecialWeather %>% filter(Year == 2013)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput <- reactive({
  #  switch(input$dataset,
  #         "Baseball_Attendance&TemperatureByYear"=baseball_alltime,
  #         "Baseball_Attendance&OpponentByYear"=baseball_opp,
   #        "Baseball_Attendance&SpecialWeatherByYear"=baseball_SpecialWeather
   #        )
    if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2012){
      datasetInput <- baseball
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2013){
      datasetInput<-baseball_2013
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2014){
      datasetInput<-baseball_2014
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2015){
      datasetInput<-baseball_2015
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2016){
      datasetInput<-baseball_2016
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2017){
      datasetInput<-baseball_2017
    }
  else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2012){
      datasetInput<-baseball_opp
  }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2013){
      datasetInput<-baseball_opp13
    }
    
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2014){
      datasetInput<-baseball_opp14
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2015){
      datasetInput<-baseball_opp15
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2015){
      datasetInput<-baseball_opp15
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2016){
      datasetInput<-baseball_opp16
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2017){
      datasetInput<-baseball_opp17
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2012){
      datasetInput<-baseball_SpecialWeather
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2013){
      datasetInput<-baseball_SpecialWeather13
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2014){
      datasetInput<-baseball_SpecialWeather14
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2015){
      datasetInput<-baseball_SpecialWeather15
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2016){
      datasetInput<-baseball_SpecialWeather16
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2017){
      datasetInput<-baseball_SpecialWeather17
    }
   # "Basketball_Attendance&TemperatureByYear",
   # "Basketball_Attendance&OpponentByYear",
   # "Basketball_Attendance&SpecialWeatherByYear"
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2017){
      datasetInput<-basketball_SpecialWeather17
    }
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2016){
      datasetInput<-basketball_SpecialWeather16
    }
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2015){
      datasetInput<-basketball_SpecialWeather15
    }
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2014){
      datasetInput<-basketball_SpecialWeather14
    }
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2013){
      datasetInput<-basketball_SpecialWeather13
    }
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2012){
      datasetInput<-basketball_SpecialWeather
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2012){
      datasetInput<-bball
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2013){
      datasetInput<-bball_2013
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2014){
      datasetInput<-bball_2014
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2015){
      datasetInput<-bball_2015
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2016){
      datasetInput<-bball_2016
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2017){
      datasetInput<-bball_2017
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2012){
      datasetInput<-bball_opp
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2013){
      datasetInput<-bball_opp13
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2014){
      datasetInput<-bball_opp14
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2015){
      datasetInput<-bball_opp15
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2016){
      datasetInput<-bball_opp16
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2017){
      datasetInput<-bball_opp17
    }
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

 # output$caption <- renderPrint(
   # if(input$dataset=="Baseball_Attendance&WeatherAllTime"){
   #   paste0("Year 2012 Represents all the time ")
   # }
   # else if(input$dataset=="Baseball_Attendance&TemperatureByYear"){
   #   paste0("Please select the Year")
   # }
   # else if(input$dataset=="Baseball_Attendance&OpponentByYear"){
   #   paste0("Please select the Year")
   # }
   # else if(input$dataset=="Baseball_Attendance&OpponentAllTime"){
   #   paste0("Year 2012 Represents all the time")
   # }
   # else if(input$dataset=="Baseball_Attendance&SpecialWeatherAllTime"){
   #   paste0("Year 2012 Represents all the time")
  #  }
   # else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"){
   #   paste0("Please select the Year")
   # }
  #    )


  output$baseball_plot <- renderPlot({
    if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2012){
      ggplot(baseball_alltime, mapping = aes(x = TAVG, y = Attendance)) +
        geom_point(mapping = aes(color = Year)) +
        geom_smooth() + 
        ggtitle("Average Temperature vs. Attendance (2012 - 2017)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2017){
      ggplot(baseball_2017, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2017)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2016){
      ggplot(baseball_2016, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2016)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2015){
      ggplot(baseball_2015, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2015)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2014){
      ggplot(baseball_2014, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2014)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&TemperatureByYear"&input$Year==2013){
      ggplot(baseball_2013, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2013)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2012){
      ggplot(baseball_opp, aes(Opp, avg_attendance)) +
        geom_bar(stat = "identity") + ggtitle("Average Attendance vs. Opponents in Six Seasons") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2017){
      ggplot(baseball_opp17, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2017") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2016){
      ggplot(baseball_opp16, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2016") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2015){
      ggplot(baseball_opp15, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2015") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2014){
      ggplot(baseball_opp14, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2014") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&OpponentByYear"&input$Year==2013){
      ggplot(baseball_opp13, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2013") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2012){
      # How the rain/snowy/windy days will affect the attendance of Red Sox, since base game is hypaethral.
      baseball_SpecialWeather<- baseball11 %>% select(Gm.,Year,DATE, Tm, Time, Attendance,SNOW,PRCP )%>% filter(PRCP!=0)
      # The rain influences on Red Sox attendence
      library(gridExtra)
      plot1<-ggplot(baseball_SpecialWeather, aes(PRCP, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Precipitation vs. Attendance (2013-2017)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Precipitation on Game Day (mm)") + 
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
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2017){
      ggplot(baseball_SpecialWeather17, aes(PRCP, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Precipitation vs. Attendance (2017)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2016){
      ggplot(baseball_SpecialWeather16, aes(PRCP, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Precipitation vs. Attendance (2016)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2015){
      ggplot(baseball_SpecialWeather15, aes(PRCP, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Precipitation vs. Attendance (2015)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2014){
      ggplot(baseball_SpecialWeather14, aes(PRCP, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Precipitation vs. Attendance (2014)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
    }
    else if(input$dataset=="Baseball_Attendance&SpecialWeatherByYear"&input$Year==2013){
      ggplot(baseball_SpecialWeather13, aes(PRCP, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Precipitation vs. Attendance (2013)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
    }
    
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2012){
      library(gridExtra)
      plot1<-ggplot(basketball_SpecialWeather, aes(PRCP, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red")+ 
        ggtitle("Precipitation vs. Attendance (2013-2017)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
      plot2<-ggplot(basketball_SpecialWeather, aes(AWND, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red") + 
        ggtitle("Snow vs. Attendance (2013-2017)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Whether snow on Game Day ") + 
        ylab("Total Attendance")
      grid.arrange(plot1,plot2,ncol=2)
    }
    
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2013){
      library(gridExtra)
      plot1<-ggplot(basketball_SpecialWeather13, aes(PRCP, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red")+ 
        ggtitle("Precipitation vs. Attendance (2013)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
      plot2<-ggplot(basketball_SpecialWeather13, aes(AWND, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red") + 
        ggtitle("Snow vs. Attendance (2013)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Whether snow on Game Day ") + 
        ylab("Total Attendance")
      grid.arrange(plot1,plot2,ncol=2)
    }
    
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2014){
      library(gridExtra)
      plot1<-ggplot(basketball_SpecialWeather14, aes(PRCP, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red")+ 
        ggtitle("Precipitation vs. Attendance (2014)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
      plot2<-ggplot(basketball_SpecialWeather14, aes(AWND, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red") + 
        ggtitle("Snow vs. Attendance (2014)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Whether snow on Game Day ") + 
        ylab("Total Attendance")
      grid.arrange(plot1,plot2,ncol=2)
    }
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2015){
      library(gridExtra)
      plot1<-ggplot(basketball_SpecialWeather15, aes(PRCP, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red")+ 
        ggtitle("Precipitation vs. Attendance (2015)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
      plot2<-ggplot(basketball_SpecialWeather15, aes(AWND, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red") + 
        ggtitle("Snow vs. Attendance (2015)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Whether snow on Game Day ") + 
        ylab("Total Attendance")
      grid.arrange(plot1,plot2,ncol=2)
    }
    
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2016){
      library(gridExtra)
      plot1<-ggplot(basketball_SpecialWeather16, aes(PRCP, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red")+ 
        ggtitle("Precipitation vs. Attendance (2016)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
      plot2<-ggplot(basketball_SpecialWeather16, aes(AWND, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red") + 
        ggtitle("Snow vs. Attendance (2016)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Whether snow on Game Day ") + 
        ylab("Total Attendance")
      grid.arrange(plot1,plot2,ncol=2)
    }
    
    else if(input$dataset=="Basketball_Attendance&SpecialWeatherByYear"&input$Year==2017){
      library(gridExtra)
      plot1<-ggplot(basketball_SpecialWeather17, aes(PRCP, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red")+ 
        ggtitle("Precipitation vs. Attendance (2017)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Precipitation on Game Day (mm)") + 
        ylab("Total Attendance")
      plot2<-ggplot(basketball_SpecialWeather17, aes(AWND, Attendance)) + 
        geom_point(mapping=aes(color = Year)) + 
        geom_smooth(method="lm",color = "red") + 
        ggtitle("Snow vs. Attendance (2017)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Whether snow on Game Day ") + 
        ylab("Total Attendance")
      grid.arrange(plot1,plot2,ncol=2)
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2017){
      ggplot(bball_2017, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2017)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2016){
      ggplot(bball_2016, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2016)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2015){
      ggplot(bball_2015, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2015)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2014){
      ggplot(bball_2014, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2014)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (Degrees F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2013){
      bball_2013$TAVG <- (bball_2013$TMAX + bball_2013$TMIN)/2
      ggplot(bball_2013, aes(TAVG, Attendance)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        ggtitle("Average Temperature vs. Attendance (2013)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (°F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&TemperatureByYear"&input$Year==2012){
      ggplot(bball, mapping = aes(x = TAVG, y = Attendance)) +
        geom_point(mapping = aes(color = Year)) +
        geom_smooth() + 
        ggtitle("Average Temperature vs. Attendance (2012 - 2017)") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Average Temperature on Game Day (°F)") + 
        ylab("Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2012){
      ggplot(bball_opp, aes(Opp, avg_attendance)) +
        geom_bar(stat = "identity") + ggtitle("Average Attendance vs. Opponents in Six Seasons") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2017){
      ggplot(bball_opp17, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2017") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2016){
      ggplot(bball_opp16, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2016") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2015){
      ggplot(bball_opp15, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2015") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2014){
      ggplot(bball_opp14, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2014") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    else if(input$dataset=="Basketball_Attendance&OpponentByYear"&input$Year==2013){
      ggplot(bball_opp13, aes(Opp, avg_attendance)) + 
        geom_bar(stat = "identity") +
        ggtitle("Average Attendance vs. Opponents in 2013") +
        theme(axis.title.x = element_text(size = 14)) +
        theme(axis.title.y = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        xlab("Opponent") + 
        ylab("Average Attendance")
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)