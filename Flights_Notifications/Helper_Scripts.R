
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