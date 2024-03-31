
pacman::p_load(shiny,tidyverse,shinydashboard,sf, tmap, dplyr, leaflet, ggthemes, reactable, timetk, forecast, zoo,lubridate,
               reactablefmtr, gt, gtExtras, tidyverse, ggplot2)

rent2024NEW <- read_csv("data/rent2024NEW.csv")

rent2024NEW_sf <- st_as_sf(rent2024NEW,
                          coords = c("Longitude", "Latitude"), 
                          crs = 4326) %>% 
  st_transform(crs = 3414)
tmap_options(check.and.fix = TRUE)
Retentialdatamerged <- read_csv("data/Retentialdatamerged.csv")
Retentialdatamerged$LeaseCommencementDate <- as.Date(Retentialdatamerged$LeaseCommencementDate)
Retentialdatamerged$YearMonth <- format(Retentialdatamerged$LeaseCommencementDate, "%Y-%m")
Retentialdatamerged$YearMonthDate <- as.Date(paste0(Retentialdatamerged$YearMonth, "-01"))

mpsz <- st_read(dsn = "data/geospatial", 
                layer = "MP14_SUBZONE_WEB_PL") %>%
  st_transform(crs = 3414)

ui <- dashboardPage(
  dashboardHeader(title = "SINGAPORE RENTAL PRICE ANALYSIS"),
  dashboardSidebar(
    menuItem("PRESENT PRICE", tabName = "PresentPrice", icon = icon("map")),
    menuItem("PAST PRICE", tabName = "PastPrice", icon = icon("chart-line")),
    menuItem("FUTURE PREDICTION", tabName = "FuturePrediction", icon = icon("chart-line")) 
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "PresentPrice",
              fluidRow(
                column(6, 
                       h2("Present Rental Prices"),
                       selectInput("roomSelect", "Number of Bedrooms:", choices = c("1", "2", "3", "4")),
                       leafletOutput("rentMap")
                ),
                column(6,
                       dataTableOutput("rentTable")
                )
              )
      ),
      tabItem(tabName = "PastPrice",
              fluidRow(
                column(4, 
                       h4("Filter Options"),
                       selectInput("planningAreaSelect", "Select Planning Area:",
                                   choices = c("Ang Mo Kio", "Bedok", "Bishan", "Bukit Batok",
                                               "Bukit Merah", "Bukit Panjang", "Bukit Timah",
                                               "Choa Chu Kang", "Clementi", "Downtown Core", 
                                               "Geylang", "Hougang", "Jurong East", 
                                               "Jurong West", "Kallang", "Mandai",
                                               "Marine Parade", "Museum", "Newton",
                                               "Novena", "Orchard", "Outram", "Pasir Ris",
                                               "Punggol", "Queenstown", "River Valley",
                                               "Rochor", "Sembawang", "Sengkang", "Serangoon",
                                               "Tampines", "Tanglin", "Toa Payoh", "Woodlands",
                                               "Yishun")),
                       selectInput("bedroomSelect", "Number of Bedrooms:",
                                   choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4))
                ),
                column(8,
                       h2("Past Rental Price Analysis"),
                       plotOutput("rentalPricePlot")
                )
              )
      ),
      tabItem(tabName = "FuturePrediction",
              fluidRow(
                column(4, 
                       h4("Filter Options"),
                       selectInput("planningAreaSelect", "Select Planning Area:",
                                   choices = c("Ang Mo Kio", "Bedok", "Bishan", "Bukit Batok",
                                               "Bukit Merah", "Bukit Panjang", "Bukit Timah",
                                               "Choa Chu Kang", "Clementi", "Downtown Core", 
                                               "Geylang", "Hougang", "Jurong East", 
                                               "Jurong West", "Kallang", "Mandai",
                                               "Marine Parade", "Museum", "Newton",
                                               "Novena", "Orchard", "Outram", "Pasir Ris",
                                               "Punggol", "Queenstown", "River Valley",
                                               "Rochor", "Sembawang", "Sengkang", "Serangoon",
                                               "Tampines", "Tanglin", "Toa Payoh", "Woodlands",
                                               "Yishun")),
                       selectInput("bedroomSelect", "Number of Bedrooms:",
                                   choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4))
                ),
                column(8,
                       h2("Future Rental Price Analysis"),
                       plotOutput("rentalPriceFuturePrediction")
    )
  )
      )
    )
  )
)
            
server <- function(input, output) {
  output$rentMap <- renderLeaflet({
    filteredData <- rent2024NEW_sf %>% 
      filter(NoOfBedroom == as.numeric(input$roomSelect))
    tmap_mode("view")
    rentMap <- tm_shape(mpsz) +
      tm_borders() +
      tm_shape(filteredData) +
      tm_dots(col= "Median_Rent")
    tmap_leaflet(rentMap)
  })
  output$rentTable <- renderDataTable({
    Retentialdatamerged%>% 
      filter(NoOfBedroom == as.numeric(input$roomSelect)) 
  })
  
  output$rentalPricePlot <- renderPlot({
    rental_data <- Retentialdatamerged %>%
      filter(PlanningArea == input$planningAreaSelect, NoOfBedroom == as.numeric(input$bedroomSelect))
    monthly_median <- aggregate(MonthlyRent ~ YearMonth, data = rental_data, FUN = median)
    highest_median_month <- monthly_median$YearMonth[which.max(monthly_median$MonthlyRent)]
    ggplot(rental_data, aes(x = YearMonthDate, y = MonthlyRent)) +
      #geom_boxplot() +
      geom_smooth(method = "loess", colour = "blue", fill = "grey", alpha = 0.5, size = 1, aes(group = 1)) +
      stat_summary(fun = median, geom = "line", colour = "red", size = 1, aes(group = 1)) +
      geom_vline(xintercept = as.numeric(as.Date(paste0(highest_median_month, "-01"))), color = "red", linetype = "dashed", size = 1) + 
      theme_minimal() +
      labs(title = paste("Monthly Rent for", input$bedroomSelect, "Bedrooms in", input$planningAreaSelect),
           x = "Month by Year",
           y = "Monthly Rent ($)") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  output$rentalPriceFuturePrediction <- renderPlot({
    req(input$planningAreaSelect, input$bedroomSelect)
    rental_data <- Retentialdatamerged %>%
      filter(PlanningArea == input$planningAreaSelect, NoOfBedroom == as.numeric(input$bedroomSelect))
    rental_data <- rental_data %>%
      mutate(YearMonthDate = as.Date(paste0(YearMonth, "-01")),
             YearMonthFormatted = format(YearMonthDate, "%Y-%m"))
    monthly_median <- aggregate(MonthlyRent ~ YearMonthFormatted, data = rental_data, FUN = median)
    ts_data <- ts(monthly_median$MonthlyRent, frequency = 12, 
                  start = c(year(min(rental_data$YearMonthDate)), 
                            month(min(rental_data$YearMonthDate))))
    model <- auto.arima(ts_data)
    forecasted_values <- forecast(model, h = 3)
    last_known_date <- max(rental_data$YearMonthDate)
    future_dates <- seq(from = last_known_date, by = "month", length.out = 4)[-1]
    forecast_df <- data.frame(
      YearMonthDate = future_dates,
      MonthlyRent = forecasted_values$mean
    )
    ggplot() +
      geom_line(data = rental_data, aes(x = YearMonthDate, y = MonthlyRent)) +
      geom_line(data = forecast_df, aes(x = YearMonthDate, y = MonthlyRent), color = "green", linetype = "dashed") +
      theme_minimal() +
      labs(title = paste("Monthly Rent for", input$bedroomSelect, "Bedrooms in", input$planningAreaSelect, "with Forecast"),
           x = "Month by Year",
           y = "Monthly Rent $") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
}


shinyApp(ui, server)




