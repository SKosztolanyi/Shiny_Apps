#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
library(shinyAce)
library(sendmailR)
library(data.table)

dt <- data.table(replicate(10,sample(0:1,1000,rep=TRUE)))

shinyServer(function(input, output, session) {
  
  switch <- reactiveValues(on = 0)
  
  observeEvent(input$reset, {
    switch$on <- 0
  }) 
  observeEvent(input$done,{
    switch$on <- 1
  })
    
    output$price_alert <- renderUI({
      if(is.null(input$done) || input$done==0 || switch$on == 0) return(NULL)
      from <- isolate(input$from)
      to <- isolate(input$to)
      max_price <- isolate(input$max_price)
      min_date <- isolate(input$daterange)
      email <- isolate(input$email)
      
      HTML(paste("<br>You chose flights from", from, "to", to, ",<br/>",
                 "<br>in the date range starting on", paste(min_date, collapse = " and ending on "),  ",<br/>", 
                 "<br>for maximum price of<b>", max_price, "CZK.</b><br/>",
                 "<br> After we find a flight matching your criteria, we will send you an email to the email address you provided.<br/>"))
    })
    
    
    
    output$under_the_hood <- renderDataTable({
      if(from == "London")
      {head(dt)}
    })
    
  })
#})