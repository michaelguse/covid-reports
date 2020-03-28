#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Insights"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("co_days",
                        "Number of Days:",
                        min = 1,
                        max = 200,
                        value = 28),
            selectInput("country1",
                        "1st Country",
                        covid19$country_name,
                        selected = "Germany",
                        multiple = FALSE),
            selectInput("country2",
                        "2nd Country",
                        covid19$country_name,
                        selected = "Germany",
                        multiple = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # these libraries are necessary
        library(utils)
        library(httr)
        library(dplyr)
        library(tidyr)
        library(lubridate)
        library(ggplot2)
        library(gridExtra)

        # download the dataset from the ECDC website to a local temporary file
        GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
            authenticate(":", ":", type="ntlm"), 
            write_disk(tf <- tempfile(fileext = ".csv"))
            )

        # read the Dataset sheet into “R”. The dataset will be called "data".
        data <- read.csv(tf)

        # format of loaded dataset from ECDC
    
        # rename columns for simplicity
        data <- data %>%
        rename(
            country_name = `countriesAndTerritories`,
            country_code = geoId,
            date = dateRep
        ) %>%
        mutate(date = parse_date_time(date, orders = c("dmy")))

        covid19 <- data %>%
        group_by(date, country_name) %>%
        arrange(desc(date),
                desc(cases),
                desc(deaths))

        covid19_all <- data %>%
        group_by(date) %>%
        summarize (sum_cases = sum(cases), sum_deaths = sum(deaths)) %>%
        arrange(desc(date), desc(sum_cases), desc(sum_deaths))

        # How many days to didplay for individual countries
        co_days <- input$co_days

        p1 <-
        ggplot(data = covid19_all, mapping = aes(x = date, y = sum_cases)) +
        geom_bar(stat = "identity", color = "red", fill = "red") +
        labs(title = "Global - Daily Cases", x = "Date", y = "Daily Case Count") +
        theme(plot.title = element_text(
            size = 14,
            face = "bold",
            margin = margin(10, 0, 10, 0)
        ))

        p2 <-
        ggplot(
            data = filter(covid19, country_name == input$country1)[1:co_days, ],
            mapping = aes(x = date, y = cases)) +
        geom_bar(stat = "identity", color = "red", fill = "#ff9999") +
        labs(title = paste(input$country1, "- Daily Cases"), x = "Date", y = "Daily Case Count") +
        theme(plot.title = element_text(
            size = 14,
            face = "bold",
            margin = margin(10, 0, 10, 0)
        ))

        p3 <-
        ggplot(
            data = filter(covid19, country_name == input$country2)[1:co_days, ],
            mapping = aes(x = date, y = cases)) +
        geom_bar(stat = "identity", color = "red", fill = "#ff9999") +
        labs(title = paste(input$country2, "- Daily Cases"), x = "Date", y = "Daily Case Count") +
        theme(plot.title = element_text(
            size = 14,
            face = "bold",
            margin = margin(10, 0, 10, 0)
        ))

        # Print all plots onto a single page and arrange them in 2 rows
        grid.arrange(p1, p2, p3, nrow = 3)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
