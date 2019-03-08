# Breast-Cancer
library(tidyverse)
library(micromapST)

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(maps)
library(shiny)

library(plotly)

library(tidyverse)

library(reshape2)

library(dplyr)

library(maps)
library(rsconnect)
library(zoom)

#global.r

#setwd("~/Desktop/Google Drive/GMU/18FALL/STAT663_18fall/redesign project")
#setwd("C:/Users/micha/Desktop/My_R_Work")
death <- read.csv(
  file = "nchs.csv",
  header = T, as.is = TRUE)
head(death)
tail(death)

death1 <- read.csv(
  file = "nchs.csv",
  header = T, as.is = TRUE)
death1
death2<-filter(death1, State!="United States" &State!= "District of Columbia")
death2



input_1<-death2$Cause.Name
input_2<-death2$Year





# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  
  titlePanel("Leading Death Causes"),
  h1("Age Adjusted Death rates in US States"),
  sidebarLayout(
    
    sidebarPanel( h3("Select Cause Name "),
                  
                  
                  selectizeInput(
                    
                    "cause",
                    
                    label = "Cause Name",
                    
                    choices = unique(input_1),
                    
                    multiple = FALSE,
                    
                    selected = "All causes"
                    
                  ),
                  
                  h3("Select an Year "),
                  
                  sliderInput(inputId ="year",
                              label = "Year:",
                              min = 1999,
                              max = 2016,
                              value = 1999,
                              step = 1,
                              sep= ""
                  )),    
    
    # Show a plot of the generated distribution
    
    
    mainPanel(
      
      
      plotOutput("p1", height=775)
      
      
    )
    
    
  )
  
  
)



# Define server logic required to plot

server <- function(input, output) {
  
  output$p1 <- renderPlot({ dataset<-reactive({death2 %>%
      
      select(Year,State,Age.adjusted.Death.Rate,Cause.Name)%>%
      
      filter(Year == input$year,Cause.Name == input$cause)})
  
  
  panelDesc13 = data.frame(
    type = c("mapcum", "id", "bar"),
    lab1 = c("", "", "Age Adjusted Deaths per 100,000 Residents"),
    lab2 = c("", "", ""),
    lab3 = c("", "", ""),
    col1 = c(NA, NA, "Age.adjusted.Death.Rate")
  )
  
  
  micromapST(dataset(), panelDesc13,
             sortVar = "Age.adjusted.Death.Rate", ascend = FALSE,
             rowNames = "ab", rowNamesCol = 'State',
             bordGrp = "USStatesBG",
             plotNames = "ab")
  
  
  })
  
}


# Run the application

shinyApp(ui = ui, server = server)
