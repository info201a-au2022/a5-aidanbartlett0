library(shinythemes)
library(shiny)
source("co2server.R")

ui <- shinyUI(fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    "Aidan Bartlett's CO2 Trends",
    tabPanel(
      "Introduction",
      sidebarLayout(
        sidebarPanel(
          h1("General CO2 Emission Information")
        ),
        mainPanel(
          fluidRow(
            column(
              width = 5,
              h2("Cumulative CO2"),
              tableOutput("cumulativeCO2")
            ),
            column(
              width = 7,
              h2("Table Explained"),
              p(
                "The table on the left shows the total CO2 emissions of each country or category
                         over the time that the data set has recorded. The values are ", strong("all"),
                " types of CO2 emissions including land-use change measured in million tonnes.
                         While not all countries/categories
                         may not have an equal amount of data recorded, those areas likely wouldn’t be in
                         the top 20, which I have chosen to show. An interesting point to note is the
                         placement of specific countries such as the United States. The United States has
                         produced approximately 22% of the world’s CO2 in the last ~200 years. That puts
                         the United States at the top of stand-alone countries on this list, having produced
                         more CO2 than the entire EU combined. The top 5 stand-alone countries are shown in
                         the table: ", strong("United States, China, Russia, Brazil, India"), "."
              )
            ),
          ),
          p(" "),
          h2("Coal CO2 Emissions by Income Category"),
          p(
            "I wanted to learn more about how the CO2 emissions from coal differ between countries
                based on income. Thankfully, there is a category in the dataset which groups countries
                together based on income into four categories: high income, medium to high income, medium
                to low income, and low income. I chose to examine the most recent year available for each
                category, which for most was 2021. Before analyzing the data, I would have assumed that the
                lower the income, the more coal is used since electricity might be more scarce, and higher
                income countries are more educated and would know better than to use coal for power since it
                is worse for the enviroment. Those assumptions are fairly reasonable, but the results surprised
                me. High income countries produce ", textOutput("highCoal", inline = TRUE), " of CO2 from coal,
                medium to high income countries produced ", textOutput("medHighCoal", inline = TRUE), ", medium
                to low income produced ", textOutput("medLowCoal", inline = TRUE), ", and low income produced ",
            textOutput("lowCoal", inline = TRUE), ". I never would have thought that the high income countries use
                that much more coal than lower income countries, since this information was completely unexpected,
                it shows the kind of knowledge that can be gained from analyzing data."
          ),
          p(" "),
          h2("United States Global Oil CO2 Emissions over the Years"),
          p(
            "I chose to investigate the CO2 produced by oil, and compare the United States’ CO2 emissions
              from oil to the global oil emissions. There is a row of data for this in the dataset, which
              gives the share of the global emissions by country and year in percentage form. I wanted to
              see how the United States’ share of the global emissions has changed over the years, so I
              averaged all the years in the data and found that over all time, the US has produced",
            textOutput(" oiltotal ", inline = TRUE), "of the
              global oil CO2 emissions. Since there are many countries that do not have data ranging back as
              far as the United States in this dataset, I then averaged over the last 100 year and found that
              the share changes to ", textOutput("oil100", inline = TRUE), ". By 1921, most countries have data available, so this is where the real
              information begins. I then averaged the last 50 years, finding that the share changed significantly
              to ", textOutput("oil50", inline = TRUE), ". Then I found over the last 25 years, it lowered again to ",
            textOutput("oil25", inline = TRUE), ". This data shows that while the
              United States produces a ridiculous portion of the global CO2 emissions by oil, it is improving
              as the years go on."
          )
        )
      )
    ),
    tabPanel(
      "Chart",
      sidebarLayout(
        sidebarPanel(
          h1("Emissions"),
          type_choice <- selectInput(
            label = "Chose an Emission Type",
            inputId = "type",
            choices = colnames(type_choices),
            selected = "co2"
          ),
          country_choice <- selectInput(
            label = "Chose a Country",
            inputId = "country_choice",
            choices = country_choices,     
            selected = "World"
          ),
          year_slider <- sliderInput("year_choice", "View Years in this Range",
                                     min = 1750, max = 2020,
                                     value = c(1750, 2020)
          )
        ),
        mainPanel(
          plotlyOutput("emissionLines"),
          p(""),
          p("This graph allows users to compare different country's 
          emissions in multiple different ways to investigate CO2 trends across the world and over time. This allows 
          users to see the information presented on the previous page in a more visual and interactive way. 
          For example, users can investigate the similarities and differences between large countries that were high
          on the list on the other page like the United States or China. When looking at the total CO2 emissions, 
          it is interesting that The United States had been rising steadily until 2007, when it's emissions started falling. 
          Whereas China had been producing signifigantly less CO2 than the US until it suddenly started exponentially growing 
          in the late 1990's.")
        )
      )
    )
  )
))
