library(shiny)
library(tidyverse)
library(plotly)
library(reshape2)


co2_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/d901472e203acb4324002a969babfc043395ccfc/owid-co2-data.csv")

co2_by_capita <- co2_data %>%
  group_by(country) %>%
  summarise(co2_per_capita = mean(co2_per_capita, na.rm = TRUE)) %>%
  arrange(-co2_per_capita)
co2_by_capita_100 <- co2_data %>%
  filter(year > 1922) %>%
  group_by(country) %>%
  summarise(co2_per_capita = mean(co2_per_capita, na.rm = TRUE)) %>%
  arrange(-co2_per_capita)

co2_by_capitaX <- slice(co2_by_capita, 1:10)
x <- nrow(co2_by_capitaX)
co2_by_capitaX <- mutate(co2_by_capitaX, "Rank" = c(1:x), .before = country)
colnames(co2_by_capitaX) <- c("Rank", "Country", "CO2 Per Person")

co2_by_capita_100X <- slice(co2_by_capita_100, 1:10)
x <- nrow(co2_by_capita_100X)
co2_by_capita_100X <- mutate(co2_by_capita_100X, "Rank" = c(1:x), .before = country)
colnames(co2_by_capita_100X) <- c("Rank", "Country", "CO2 Per Person")
# I dont use this i just dont have a reason to delete it


total_co2 <- co2_data %>%
  group_by(country) %>%
  summarize(
    cumulative_co2_including_luc =
      max(cumulative_co2_including_luc, na.rm = TRUE)
  ) %>%
  filter(cumulative_co2_including_luc != -Inf) %>%
  arrange(-cumulative_co2_including_luc)
x <- nrow(total_co2)
total_co2 <- total_co2 %>%
  mutate(total_co2, "Rank" = c(1:x), .before = country) %>%
  slice(1:20)
# there were 51 countries that did not have any data for this column
colnames(total_co2) <- c("Rank", "Country/Category", "Total CO2")


oil_share_total <- co2_data %>%
  filter(country == "United States") %>%
  select(share_global_oil_co2) %>%
  mutate(share_global_oil_co2 = mean(share_global_oil_co2, na.rm = TRUE)) %>%
  head(1) %>%
  pull(share_global_oil_co2) %>%
  paste0(" %")
oil_share_100 <- co2_data %>%
  filter(country == "United States" & year > 1921) %>%
  select(share_global_oil_co2) %>%
  mutate(share_global_oil_co2 = mean(share_global_oil_co2, na.rm = TRUE)) %>%
  head(1) %>%
  pull(share_global_oil_co2) %>%
  paste0(" %")
oil_share_50 <- co2_data %>%
  filter(country == "United States" & year > 1971) %>%
  select(share_global_oil_co2) %>%
  mutate(share_global_oil_co2 = mean(share_global_oil_co2, na.rm = TRUE)) %>%
  head(1) %>%
  pull(share_global_oil_co2) %>%
  paste0(" %")
oil_share_25 <- co2_data %>%
  filter(country == "United States" & year > 1996) %>%
  select(share_global_oil_co2) %>%
  mutate(share_global_oil_co2 = mean(share_global_oil_co2, na.rm = TRUE)) %>%
  head(1) %>%
  pull(share_global_oil_co2) %>%
  paste0(" %")


# coal CO2 emmissions based on money
high_coal <- co2_data %>%
  filter(country == "High-income countries" &
    year == max(year)) %>%
  pull(coal_co2) %>%
  paste0(" million tonnes")
med_high_coal <- co2_data %>%
  filter(country == "Upper-middle-income countries" &
    year == max(year)) %>%
  pull(coal_co2) %>%
  paste0(" million tonnes")
med_low_coal <- co2_data %>%
  filter(country == "Lower-middle-income countries" &
    year == max(year)) %>%
  pull(coal_co2) %>%
  paste0(" million tonnes")
low_coal <- co2_data %>%
  filter(country == "Low-income countries" &
    year == max(year)) %>%
  pull(coal_co2) %>%
  paste0(" million tonnes")


emission_types_data <- co2_data %>% select(
  country, year, trade_co2, methane,
  coal_co2, oil_co2, nitrous_oxide,
  gas_co2, co2
)
type_choices <- emission_types_data %>%
  select(
    trade_co2, methane,
    coal_co2, oil_co2, nitrous_oxide,
    gas_co2, co2
  )

country_choices <- unique(emission_types_data$country)

server <- function(input, output) {
  output$summaryCapita <- renderTable({
    return(co2_by_capitaX)
  })
  output$summaryCapita100 <- renderTable({
    return(co2_by_capita_100X)
  })
  output$cumulativeCO2 <- renderTable({
    return(total_co2)
  })
  output$oiltotal <- renderText({
    return(oil_share_total)
  })
  output$oil100 <- renderText({
    return(oil_share_100)
  })
  output$oil50 <- renderText({
    return(oil_share_50)
  })
  output$oil25 <- renderText({
    return(oil_share_25)
  })
  output$highCoal <- renderText({
    return(high_coal)
  })
  output$medHighCoal <- renderText({
    return(med_high_coal)
  })
  output$medLowCoal <- renderText({
    return(med_low_coal)
  })
  output$lowCoal <- renderText({
    return(low_coal)
  })
  output$emissionPlot <- renderPlotly({
    return(x(input$year_choice, input$type_choice))
  })
  output$emissionLines <- renderPlotly({
    emission_df <- emission_types_data %>%
      group_by(country) %>%
      select(
        country, year, trade_co2, methane,
        coal_co2, oil_co2, nitrous_oxide,
        gas_co2, co2
      ) %>%
      filter(country == input$country_choice)

    plot <- ggplot(emission_df, aes_string(
      x = emission_df$year,
      y = input$type
    )) +
      geom_line(color = "red") +
      labs(title = "CO2 Emissions by Country/Category, Year, and Cause") +
      ylab("CO2 Released in Millions of Tonnes") +
      xlab("Years") +
      xlim(input$year_choice[1], input$year_choice[2])
    plot <- ggplotly(plot)

    return(plot)
  })
}
