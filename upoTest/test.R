library(ggplot2)
library(tidyverse)
library(dplyr)
library(base)

path <- '../Data/DailyDelhiClimateTrain.csv'
getwd()
setwd("../upoTest")
getwd()
data <- read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
# data <- read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE, row.names = 1)
data$date <- as.Date(data$date)

# index_column <- data$date  # Extract the column as a separate vector
# rownames(data) <- index_column  # Set the extracted column as the row names
# data$date <- NULL

print(dim(data))
print(sapply(data,class))


# Create annual humidity by aggregating the 'humidity' column
annual_hum <- aggregate(humidity ~ format(date, "%Y"), data, mean)
names(annual_hum) <- c("date", "humidity")

# Create monthly humidity by aggregating the 'humidity' column
monthly_hum <- aggregate(humidity ~ format(date, "%Y-%m"), data, mean)
names(monthly_hum) <- c("date", "humidity")

# change directory for plots
setwd("../ResultsPlots")
getwd()

# Set the plot size
X = 14
Y = 4

options(repr.plot.width = X, repr.plot.height = Y)

# Create subplots
par(mfrow = c(2, 1))

# Plot monthly humidity
monthly_hum$date <- as.Date(paste0(monthly_hum$date, "-01"), format = "%Y-%m-%d")

png("month_hum.png", width = X, height = Y, units = "px")
plot(monthly_hum$date, monthly_hum$humidity, type = "l", xlab = "Date", ylab = "Humidity", main = "Monthly Humidity")
dev.off()


# Plot annual humidity
png("annual_hum.png", width = X, height = Y, units = "px")
plot(as.Date(annual_hum$date, format = "%Y"), annual_hum$humidity, type = "l", xlab = "Date", ylab = "Humidity", main = "Annual Humidity")
dev.off()

library(vioplot)

png("violin_hum.png", width = X, height = Y, units = "px")
vioplot(data$humidity ~ as.integer(format(data$date, "%m")), 
        xlab = "Month", ylab = "Humidity",
        main = "Violin Plot of Monthly Humidity",
        col = "lightblue", border = "black")
grid()
dev.off()

# Create annual humidity by aggregating the 'humidity' column
annual_mtemp <- aggregate(meantemp ~ format(date, "%Y"), data, mean)
names(annual_mtemp) <- c("date", "meantemp")

# Create monthly humidity by aggregating the 'humidity' column
monthly_mtemp <- aggregate(meantemp ~ format(date, "%Y-%m"), data, mean)
names(monthly_mtemp) <- c("date", "meantemp")


# Create subplots
par(mfrow = c(2, 1))

# Plot monthly humidity
monthly_mtemp$date <- as.Date(paste0(monthly_mtemp$date, "-01"), format = "%Y-%m-%d")

# png("month_mtemp.png", width = X, height = Y, units = "px")
plot(monthly_mtemp$date, monthly_mtemp$meantemp, type = "l", xlab = "Date", ylab = "Mean Temperature", main = "Monthly Mean Temperature")
# dev.off()


# Plot annual humidity
png("annual_mtemp.png", width = 14, height = 4, units = "px")
plot(as.Date(annual_mtemp$date, format = "%Y"), annual_mtemp$meantemp, type = "l", xlab = "Date", ylab = "Mean Temperature", main = "Annual Mean Temperature")
dev.off()

library(vioplot)

png("violin_mtemp.png", width = X, height = Y, units = "px")
vioplot(data$meantemp ~ as.integer(format(data$date, "%m")), 
        xlab = "Month", ylab = "Mean Temperature",
        main = "Violin Plot of Monthly Mean Temperature",
        col = "orange", border = "black")
grid()

dev.off()

