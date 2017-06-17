
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


p <- plot_ly(flight_results, type = "histogram", autobinx = FALSE, xbins = list(start = 0, end = 25000, size = 1000), marker = list( color = "green"))
p <- add_trace(p, x = ~ price, autobinx = FALSE, xbins = list(start = 0, end = 25000, size = 500))
p %>% config(displayModeBar = F, showLink = F) %>%
      layout(showlegend = F, barmode = "overlay", yaxis = list(title = "count"),
              xaxis = list(title = "price", showticklabels = T))

# create stacked chart - from and to airports
p <- plot_ly(flight_results, type = "bar")
p <- add_trace(p, x = ~ from_airport, y = ~ from_airport)
p

table(flight_results$from_airport)[3]

ros <- data.frame(name = c("a", "a", "b", "c", "a", "c"))
ros

newros <- ros %>%
  dplyr::group_by(name) %>%
  summarise(n = n())
names(newros) <- c("Forint", "korint")

newros
class(newros)
class(newros$name)
class(newros$n)


p <- plot_ly(as.data.frame(newros), x = ~ name, y = ~n, type = "bar")
p <- add_trace(p, x = ~ n, y = ~ n)
as.character(name)

p

Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)
data