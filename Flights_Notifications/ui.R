#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# https://shiny.rstudio.com/reference/shiny/latest/fluidPage.html
library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Alert me Flights"),
  
  sidebarLayout(
  
    sidebarPanel(
      conditionalPanel(condition = "input.conditionedPanels == 1",
      selectInput("from", "From Airports:", choices = c("", "London", "Prague", "Milan", "Vienna"), selected = NULL, multiple = FALSE, selectize=FALSE),
      selectInput("to", "To Airports:", choices = c("", "London", "Prague", "Milan", "Vienna"), selected = NULL, multiple = FALSE, selectize=FALSE),
      sliderInput("max_price", label = "Maximum Price:", min = 0, max = 25000, value = c(5000)),
      dateRangeInput("daterange", "Date range:",
                     start  = "2017-07-01",
                     end    = "2017-12-31",
                     min    = "2017-07-01",
                     max    = "2017-12-31",
                     format = "yyyy-mm-dd",
                     separator = "-"),
      textInput("email", "Your Email:", value=""),
      actionButton("done", "Done"),
      actionButton("reset", "Clear")
    ),
    conditionalPanel(condition = "input.conditionedPanels == 2",
                     h2("Places and Prices"),
                     h5("The left plot shows number of times a user selected the airport as a departure place in blue color,
                        and number of times  the user selected the airport as an arrival place in orange color."),
                     h5("The right plot shows what prices the user selected as a maximal price for a flight."))
    ),
    
    mainPanel(
      tabsetPanel(id = "conditionedPanels",
        tabPanel("Email Notification", 
                 fluidRow(column(6, htmlOutput("price_alert")),
                 #fluidRow(column(6, dataTableOutput("price_alert")),
                          column(6, dataTableOutput("under_the_hood"))), value = 1
                 ),
        tabPanel("Subscribed Users", 
                 #fluidRow(column(6, htmlOutput("price_alert")),
                 fluidRow(column(12, dataTableOutput("active_notifications"))), value = 1
                 ),
        tabPanel("Notifications Dashboard",
                 fluidRow(column(6, plotlyOutput("departure_cities")),
                          column(6, plotlyOutput("price_histogram"))), value = 2
                 )
        
        )
      )
    )
  )
)