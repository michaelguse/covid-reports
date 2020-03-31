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

co_days = 100
country1 = "United_States_of_America"
country2 = "Germany"


# format of loaded dataset from ECDC
# rename columns for simplicity
data <- data %>%
rename(
    country_name = `countriesAndTerritories`,
    country_code = geoId,
    date = dateRep
) %>%
mutate(date = parse_date_time(date, orders = "dmy"))

covid19 <- data %>%
group_by(date, country_name) %>%
arrange(desc(date),
        desc(cases),
        desc(deaths))

covid19_all <- data %>%
group_by(date) %>%
summarize (sum_cases = sum(cases), sum_deaths = sum(deaths)) %>%
arrange(desc(date), desc(sum_cases), desc(sum_deaths))

p1 <-
ggplot(data = covid19_all[1:co_days,], mapping = aes(x = date, y = sum_cases)) +
geom_bar(stat = "identity", color = "red", fill = "red") +
labs(title = "Global - Daily Cases", x = "Date", y = "Daily Case Count") +
theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
))

p2 <-
ggplot(
    data = filter(covid19, country_name == country1)[1:co_days,],
    mapping = aes(x = date, y = cases)) +
geom_bar(stat = "identity", color = "red", fill = "#ff9999") +
labs(title = paste(country1, "- Daily Cases"), x = "Date", y = "Daily Case Count") +
theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
))

p3 <-
ggplot(
    data = filter(covid19, country_name == country2)[1:co_days,],
    mapping = aes(x = date, y = cases)) +
geom_bar(stat = "identity", color = "red", fill = "#ff9999") +
labs(title = paste(country2, "- Daily Cases"), x = "Date", y = "Daily Case Count") +
theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
))

# Print all plots onto a single page and arrange them in 2 rows
grid.arrange(p1, p2, p3, nrow = 3)
