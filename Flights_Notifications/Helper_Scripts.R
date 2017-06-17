
user_input_table <- data.frame(UserEmail = character(),
                               From = character(),
                               To = character(),
                               DateFrom = as.Date(character()),
                               DateTo = as.Date(character()),
                               MaxPrice = integer(),
                               stringsAsFactors=FALSE)


# user_input_table <- rbind(user_input_table, data.frame("email", "from", "to", "2017-01-01", "2017-06-12", 110), make.row.names = F)
user_input_table[nrow(user_input_table) + 1,] <- c("email", "from", "to", "2017-01-01", "2017-06-12", 110)
user_input_table



from <- "London"
to <- "Prague"
min_date <- as.Date("2017-07-07")
max_date <- as.Date("2017-09-09")
max_price <- 10000

flight_results <- half_year_flights[(half_year_flights$from_airport == from &
                                       half_year_flights$to_airport == to &
                                       as.Date(half_year_flights$departure_time) > as.Date(min_date) &
                                       as.Date(half_year_flights$departure_time) < as.Date(max_date) &
                                       half_year_flights$price < max_price
)
,]

flight_results