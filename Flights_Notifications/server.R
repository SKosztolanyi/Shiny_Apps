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
library(dplyr)

# rm(user_input_table)
# initialize empty data frame for user input to form
 user_input_table <- data.frame(UserEmail = character(),
                               From = character(),
                               To = character(),
                               DateFrom = character(),
                               DateTo = character(),
                               #DateFrom = as.Date(date()),
                               #DateTo = as.Date(date()),
                               MaxPrice = integer(),
                               stringsAsFactors=FALSE)

# user_input_table <- data.frame(matrix(ncol = 6, nrow = 0))

headers <- c("from_airport", "to_airport", "departure_time", "arrival_time", "flight_number")
cities <- c("London", "Prague", "Vienna", "Milan")
  
create_daily_schedule <- function(cities, headers) {
  
  datagrid <-  as.data.frame(expand.grid(cities, cities))
  same_index <- if_else(datagrid$Var1 == datagrid$Var2, 1, 0)
  flight_pairs <- datagrid[same_index == 0, ]
  hours <- sample((0:23), 12)
  minutes <- sample((0:59), 12)
  date_departure <- as.POSIXct(paste('2017-01-01 ', as.character(hours), ":", as.character(minutes), ":", "00", sep = ""), format = "%Y-%m-%d %H:%M:%S")
  date_arrival <- date_dep + 2.5*60*60
  flight_number <- paste("FN", as.character(sample((250:390), 12)), sep = "")
  daily_schedule <- cbind(flight_pairs, date_departure, date_arrival, flight_number)
  names(daily_schedule) <- headers
  rownames(daily_schedule) <- NULL

  return(daily_schedule)
}

daily_table <- create_daily_schedule(cities, headers)
daily_table


dt <- data.table(replicate(10,sample(0:1,1000,rep=TRUE)))

shinyServer(function(input, output, session) {
  
  switch <- reactiveValues(on = 0)
  
  observeEvent(input$reset, {
    switch$on <- 0
  }) 
  observeEvent(input$done,{
    switch$on <- 1
  })
  
  
  # make a reactive data frame
  values <- reactiveValues()  
  values$user_input_table <- data.frame(UserEmail = character(),
                                 From = character(),
                                 To = character(),
                                 DateFrom = character(),
                                 DateTo = character(),
                                 #DateFrom = as.Date(date()),
                                 #DateTo = as.Date(date()),
                                 MaxPrice = integer(),
                                 stringsAsFactors=FALSE)
  
  
  
  # add to df
  addData <- observeEvent(input$done, {
    #values$user_input_table <- isolate({
    print(as.character(input$daterange))
    print(as.character(input$daterange[1]))
      #newLine <- c( input$email, input$from, input$to, as.character(input$daterange)[1], as.character(input$daterange)[2], input$max_price)
      
      newLine <- data.frame(UserEmail = input$email,
                            From = input$from,
                            To = input$to,
                            DateFrom = as.character(input$daterange)[1],
                            DateTo = as.character(input$daterange)[2],
                            #DateFrom = as.Date(date()),
                            #DateTo = as.Date(date()),
                            MaxPrice = input$max_price,
                            stringsAsFactors=FALSE)
      
      values$user_input_table <- bind_rows(values$user_input_table, newLine)
      # user_input_table[nrow(user_input_table) + 1,] <- newLine
      # values$user_input_table <- rbind(values$user_input_table, newLine)
      # values$user_input_table[nrow(user_input_table) + 1,] <- newLine
      # rbind(values$user_input_table,newLine)
    #})
  })
  
  
  
  
  output$price_alert <- renderDataTable({
    values$user_input_table
  })
  
  output$active_notifications <- renderDataTable({
    values$user_input_table
  })
  
#     output$price_alert <- renderDataTable({
#       if(is.null(input$done) || input$done==0 || switch$on == 0) return(NULL)
#       from <- isolate(input$from)
#       to <- isolate(input$to)
#       max_price <- isolate(input$max_price)
#       min_date <- isolate(input$daterange)
#       email <- isolate(input$email)
#       
# #      print(length(c(from, to, max_price, min_date, email)))
# #      print(min_date[1])
#       
#       # print message for user
# #      HTML(paste("<br>You chose flights from", from, "to", to, "<br/>",
# #                 "<br>in the date range starting on", paste(min_date, collapse = " and ending on "),  "<br/>", 
# #                 "<br>for maximum price of<b>", max_price, "CZK.</b><br/>",
# #                 "<br> After we find a flight matching your criteria, we will send you an email to the email address you provided.<br/>"))
#       
#       # add user info to table (as a database)
#       if (from != '' & to != '' & length(c(from, to, min_date, email, max_price)) == 6) {
#         # user_input_table <- bind_rows(c( email, from, to, min_date, max_price))
#         print(class(min_date))
#         print(class(as.character(min_date[1])))
#         user_input_table[nrow(user_input_table) + 1,] <- c( email, from, to, as.character(min_date[1]), as.character(min_date[2]), max_price)
#         user_input_table
#       }
#       else{
#         #print('not6')
#       }
#     })
    
    
    
    output$under_the_hood <- renderDataTable({
      {head(dt)}
    })
    
  })
#})