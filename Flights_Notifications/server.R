#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyAce)   # would be used in email sending app
library(sendmailR)  # would be used in email sending app
library(data.table)
library(dplyr)

# rm(user_input_table)
# initialize empty data frame for user input to form
user_input_table <- data.frame(UserEmail = character(),
                               From = character(),
                               To = character(),
                               DateFrom = character(),
                               DateTo = character(),
                               MaxPrice = integer(),
                               stringsAsFactors=FALSE)

headers <- c("from_airport", "to_airport", "departure_time", "arrival_time", "flight_number")
cities <- c("London", "Prague", "Vienna", "Milan")


# create simple flights dataset with random prices 
create_daily_schedule <- function(cities, headers) {
  
  datagrid <-  as.data.frame(expand.grid(cities, cities))
  same_index <- if_else(datagrid$Var1 == datagrid$Var2, 1, 0)
  flight_pairs <- datagrid[same_index == 0, ]
  hours <- sample((6:21), 12)
  minutes <- sample((0:59), 12)
  date_departure <- as.POSIXct(paste('2017-07-01 ', as.character(hours), ":", as.character(minutes), ":", "00", sep = ""), format = "%Y-%m-%d %H:%M:%S")
  date_arrival <- date_departure + 2.5*60*60
  flight_number <- paste("FN", as.character(sample((250:390), 12)), sep = "")
  daily_schedule <- cbind(flight_pairs, date_departure, date_arrival, flight_number)
  names(daily_schedule) <- headers
  rownames(daily_schedule) <- NULL

  return(daily_schedule)
}

daily_table <- create_daily_schedule(cities, headers)
daily_table

one_day_added_table <- daily_table
half_year_flights <- daily_table

while(one_day_added_table$departure_time <= "2017-12-31") {
  one_day_added_table$departure_time = as.POSIXct(one_day_added_table$departure_time) + 24*60*60
  one_day_added_table$arrival_time = as.POSIXct(one_day_added_table$arrival_time) + 24*60*60
  half_year_flights <- bind_rows(half_year_flights, one_day_added_table)
}

# assign price somehow randomly
set.seed(21)
half_year_flights$price <- ceiling(rbeta(nrow(half_year_flights), 4, 2)
                              * rbeta(nrow(half_year_flights), 5, 2)
                              * runif(nrow(half_year_flights), 1, 6)
                              * 5000)

# write.table(half_year_flights, file = "SampleFlightData.csv", sep = ";", row.names = F)

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
                                 MaxPrice = integer(),
                                 stringsAsFactors=FALSE)
  
  
  
  # add to df
  addData <- observeEvent(input$done, {
    #values$user_input_table <- isolate({
    #print(as.character(input$daterange))
    #print(as.character(input$daterange[1]))
      #newLine <- c( input$email, input$from, input$to, as.character(input$daterange)[1], as.character(input$daterange)[2], input$max_price)
      
    # newLine has to be created as a data frame otherwise rowbinding doesn't work
      newLine <- data.frame(UserEmail = input$email,
                            From = input$from,
                            To = input$to,
                            DateFrom = as.character(input$daterange)[1],
                            DateTo = as.character(input$daterange)[2],
                            #DateFrom = as.Date(date()),
                            #DateTo = as.Date(date()),
                            MaxPrice = input$max_price,
                            stringsAsFactors=FALSE)
      
      if (input$from != '' & input$to != '' & input$email != '' & length(c(input$from, input$to, input$daterange, input$email, input$max_price)) == 6) {
      values$user_input_table <- bind_rows(values$user_input_table, newLine)
      }
      # These were not working:
      # user_input_table[nrow(user_input_table) + 1,] <- newLine
      # values$user_input_table <- rbind(values$user_input_table, newLine)
      # values$user_input_table[nrow(user_input_table) + 1,] <- newLine
      # rbind(values$user_input_table,newLine)
    
  })

  
  output$active_notifications <- renderDataTable({
    values$user_input_table
  })
  
     output$price_alert <- renderUI({
       if(is.null(input$done) || input$done==0 || switch$on == 0) return(NULL)
       
       from <- isolate(input$from)
       to <- isolate(input$to)
       max_price <- isolate(input$max_price)
       min_date <- isolate(input$daterange)[1]
       max_date <- isolate(input$daterange)[2]
       email <- isolate(input$email)

       
# #      print(length(c(from, to, max_price, min_date, email)))
# #      print(min_date[1])
#       
#       # print message for user
     HTML(paste("<br>You chose flights from", from, "to", to, "<br/>",
                "<br>in the date range starting on", paste(min_date, " and ending on ", max_date),  "<br/>", 
                "<br>for maximum price of<b>", max_price, "CZK.</b><br/>",
                "<br> After we find a flight matching your criteria, we will send you an email to the email address you provided.<br/>"))
       
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
     })
    
    output$under_the_hood <- renderDataTable({
      
      if(is.null(input$done) || input$done==0 || switch$on == 0) return(NULL)
      
      if (input$from != '' & input$to != '' & input$email != '' & length(c(input$from, input$to, input$daterange, input$email, input$max_price)) == 6) {
        
        # I would like to 
        from <- isolate(input$from)
        to <- isolate(input$to)
        max_price <- isolate(input$max_price)
        min_date <- isolate(input$daterange)[1]
        max_date <- isolate(input$daterange)[2]
        email <- isolate(input$email)  
        
        
      flight_results <- half_year_flights[(half_year_flights$from_airport == from &
                                            half_year_flights$to_airport == to &
                                            as.Date(half_year_flights$departure_time) > min_date &
                                            as.Date(half_year_flights$departure_time) < max_date &
                                            half_year_flights$price < max_price
                                            ) ,]
      
      # show only first three results based on lowest price
      head(flight_results[order(flight_results[,6]), ], 3)
      }
    })
    
  })