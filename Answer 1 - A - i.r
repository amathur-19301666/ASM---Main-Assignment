# Importing the necessary packages
library(ggplot2)
library(dplyr)
library(ggpubr)
library(MCMCpack)

# Reading the data and processing it
wines = read.csv("winemag-data-130k-v2.csv")
wines = filter(wines, (country == "South Africa" & variety == "Sauvignon Blanc" & price == 15) | (country == "Chile" & variety == "Chardonnay" & price == 15))

# Performing the Shapiro-Wilk normality test
with(wines, shapiro.test(points[country == "Chile"]))
with(wines, shapiro.test(points[country == "South Africa"]))

# Performing the F-test
var.test(points ~ country, data = wines)

# Performing the T-test
t.test(points ~ country, data=wines, var.equal = TRUE)

# Visualizing the box-plot
ggplot(wines) + geom_boxplot(aes(country, points, fill = country)) + geom_jitter(aes(country, points, shape = country))




