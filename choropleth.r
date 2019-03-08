# Breast-Cancer
library(shiny)
library(plotly)
library(tidyverse)
library(reshape2)
library(dplyr)
library(maps)

#global.r
#setwd("~/Desktop/Google Drive/GMU/18FALL/STAT663_18fall/redesign project")
ideal <- read.csv("nchs.csv",header=T,stringsAsFactors = F)
glimpse(ideal)


ideal<-ideal[,-2]

table(ideal$State)

#df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
#str(df)
# str(death)
# table(death$Cause.Name)

d2<-ideal %>%
  select(Year,State,Age.adjusted.Death.Rate,Cause.Name)%>%
  filter(State!="United States")

d2<-d2 %>%
  filter(State!="District of Columbia")

#d2$code = df$code

d2[52,4]<-"All causes"
d2[5465,4]<-"Influenza and pneumonia"
d2[9051,4]<-"Unintentional injuries"
d2[6352,4]<-"Kidney disease"
d2[4574,4]<-"Heart disease"


input_1<-d2$Cause.Name
input_2<-d2$Year





# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Leading Death Causes"),
  fluidRow(
    h3("Select Cause Name "),
    selectizeInput(
      "cause",
      label = "CauseName",
      choices = unique(input_1),
      multiple = FALSE,
      selected = "All causes"
    ),
    h3("Select an Year "),
    selectizeInput(
      "year",
      label = "Year",
      choices = unique(input_2),
      multiple = FALSE,
      selected = "1999"
    ),
    #
    # Show a plot of the generated distribution

    mainPanel(
      textOutput("CauseName"),
      textOutput("Year"),
      plotlyOutput("plot_id")

    )

  )

)


# Define server logic required to plot
server <- function(input, output) {
  #dataset<-reactive({d2[sample(Year==input$year, Cause.Name== input$cause) ,]})

  d2$hover <- with(d2, paste(State, '<br>', "Age Adjusted Death Rate", Age.adjusted.Death.Rate,'<br>', "Cause Name:", Cause.Name))

  #d2$State
  #df$state
  d2$code = df$code
  # give state boundaries a white border
  dataset<-reactive({d2 %>%
      select(Year,State,Age.adjusted.Death.Rate,Cause.Name,hover)%>%
      filter(Year== input$year,Cause.Name== input$cause)})
  l <- list(color = toRGB("white"), width = 2)
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )


  output$CauseName <- renderText({
    paste("CauseName: ", input$cause , "          ")
  })
  output$Year <- renderText({
    paste("Year: ", input$year , "          ")
  })
  output$plot_id<-renderPlotly({

    plot_geo(dataset() , locationmode = 'USA-states') %>%
      add_trace(
        z = ~Age.adjusted.Death.Rate, text = ~hover, locations = ~State,
        color = ~Age.adjusted.Death.Rate, colors = 'Greens'
      ) %>%
      colorbar(title = "Death Rate") %>%
      layout(title = input$cause ,
             geo = g
      )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
