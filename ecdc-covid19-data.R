#these libraries are necessary
library(readxl)
library(httr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

#create the URL where the dataset is stored with automatic updates every day
url <-
  paste(
    "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
    format(Sys.time(), "%Y-%m-%d"),
    ".xlsx",
    sep = ""
  )
#url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-21.xlsx", sep = "")

#download the dataset from the website to a local temporary file
GET(url,
    authenticate(":", ":", type = "ntlm"),
    write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”
data <- read_excel(tf)

# format of loaded dataset from ECDC
#
#   DateRep   Day     Month  Year   Cases Deaths `Countries and territories` GeoId
#   <dttm>      <dbl> <dbl>   <dbl> <dbl>  <dbl>   <chr>                                <chr>

#rename columns for simplicity
data <- data %>%
  rename(
    country_name = `Countries and territories`,
    country_code = GeoId,
    date = DateRep,
    day = Day,
    year = Year,
    month = Month,
    cases = Cases,
    deaths = Deaths
  ) %>%
  mutate(date = as_date(date))

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
co_days <- 28

p1 <-
  ggplot(
    data = filter(covid19, country_code == "US")[1:co_days, ],
    mapping = aes(x = date, y = cases)
  ) +
  geom_bar(stat = "identity", color = "red", fill = "#ff9999") +
  labs(title = "DE Daily Cases", x = "Date", y = "Daily Case Count") +
  labs(title = "US Daily Cases", x = "Date", y = "Daily Case Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

p2 <-
  ggplot(
    data = filter(covid19, country_code == "US")[1:co_days, ],
    mapping = aes(x = date, y = deaths),
    color = "black"
  ) +
  geom_bar(stat = "identity", color = "black", fill = "#666666") +
  labs(title = "DE Daily Deaths", x = "Date", y = "Daily Death Count") +
  labs(title = "US Daily Deaths", x = "Date", y = "Daily Death Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

p3 <-
  ggplot(
    data = filter(covid19, country_code == "DE")[1:co_days, ],
    mapping = aes(x = date, y = cases)
  ) +
  geom_bar(stat = "identity", color = "red", fill = "#ff9999") +
  labs(title = "DE Daily Cases", x = "Date", y = "Daily Case Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

p4 <-
  ggplot(
    data = filter(covid19, country_code == "DE")[1:co_days, ],
    mapping = aes(x = date, y = deaths),
    color = "black"
  ) +
  geom_bar(stat = "identity", color = "black", fill = "#666666") +
  labs(title = "DE Daily Deaths", x = "Date", y = "Daily Death Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

p5 <-
  ggplot(data = covid19_all, mapping = aes(x = date, y = sum_cases)) +
  geom_bar(stat = "identity", color = "red", fill = "red") +
  labs(title = "Global Daily Cases", x = "Date", y = "Daily Case Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

p6 <-
  ggplot(
    data = covid19_all,
    mapping = aes(x = date, y = sum_deaths),
    color = "black"
  ) +
  geom_bar(stat = "identity", color = "black", fill = "black") +
  labs(title = "Global Daily Deaths", x = "Date", y = "Daily Death Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

p7 <-
  ggplot(
    data = filter(covid19, country_code == "IT")[1:co_days, ],
    mapping = aes(x = date, y = cases)
  ) +
  geom_bar(stat = "identity", color = "red", fill = "#ff9999") +
  labs(title = "IT Daily Cases", x = "Date", y = "Daily Case Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

p8 <-
  ggplot(
    data = filter(covid19, country_code == "IT")[1:co_days, ],
    mapping = aes(x = date, y = deaths),
    color = "black"
  ) +
  geom_bar(stat = "identity", color = "black", fill = "#666666") +
  labs(title = "IT Daily Deaths", x = "Date", y = "Daily Death Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

# Print all plots onto a single page and arrange them in 2 rows
grid.arrange(p5, p1, p3, p7, p6, p2, p4, p8, nrow = 2)
