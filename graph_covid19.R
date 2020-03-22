library(ggplot2)

ggplot(data = covid19) + 
geom_point(mapping = aes(x = date, y = cases, color = country_code))
