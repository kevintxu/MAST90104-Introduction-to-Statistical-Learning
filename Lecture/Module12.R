library(tidyverse)
library(ggplot2)
library(ggfortify)

GT = read.csv(file = "./GlobalTemperatures_lecture.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")
GT = as_data_frame(GT)
GT$Date <- as.Date(GT$Date)
GT$Year <- as.integer(format(GT$Date, "%Y"))
GT$YearGroup =
    cut(GT$Year, breaks = c(1849, 1899, 1939, 1979, 1999, 2020),
labels = c("pre 1900", "pre 1940", "pre 1980", "pre 2000", "after 2000"))
GT$Month = format(GT$Date, '%b')

# do Principal Components on Columns 2 to 9
# summary gives the proportion of variance for each component
PCAtemp <- prcomp(GT[2:9])
summary(PCAtemp)

# PCAtemp$ rotation has the component weightings
PCAtemp$rotation


# Plot of first two Principal Components
autoplot(PCAtemp)
# colour by Year Group
autoplot(PCAtemp, data = GT, colour = "YearGroup")
# colour by Month
autoplot(PCAtemp, data = GT, colour = "Month")