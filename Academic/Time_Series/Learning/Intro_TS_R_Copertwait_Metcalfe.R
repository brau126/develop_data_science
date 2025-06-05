####################################################
# Book:    Introductory Time Series with R
# Author:  Paul SP Coperwait & Andrew V Metcalfe
# Reader:  Braulio Canedo B
####################################################
########## Libraries ##########
# Plots
library(ggplot2)
# Data transform
library(dplyr)

#          1 Time Series Data
########## 1.4.1 Air Passanger bookings ##########
# import data from R
data("AirPassengers")
AP <- AirPassengers

# to visualize the data
AP # or View(AP)

# print the type of object 
cat("Time of object:", class(AP))

# Plot the initial time serie
#     using plot()
plot(AP, main = "PanAm Air Passangers 1949-1960", ylab = "Passengers (1000's)")

#     using ggplot2()
# first transform the object to a data frame
df_AP <- data.frame(
  time = as.numeric(time(AP)),
  passangers = as.numeric(AP)
)
# generate plot
ggplot(data = df_AP, mapping = aes(x = time, y = passangers)) +
  geom_line(color = "slateblue") +
  labs(title = "PanAm Air Passangers 1949-1960", x = "Time", y = "Passengers (1000's)") + 
  theme_minimal()

# Understand better how the data behaves with cyvles boxplot and aggregated
#     using plot()
layout(1:2) # omit using layout() when coding in a markdown notebook
plot(aggregate(AP), main = "Aggregated PanAm Air Passangers 1949-1960", ylab = "Passangers (1000's)")
boxplot(AP ~ cycle(AP), main ="Boxplot of seasonal values", xlab = "Month")

#   using ggplot2()
# first organize data for aggreggate
agg_AP <- aggregate(AP, FUN = mean)
df_agg_AP <- data.frame(
  time = as.numeric(time(agg_AP)),
  agg_passangers = as.numeric(agg_AP)
)

# organize data for the box plot
df_cyclebox_AP <- data.frame(
  cycle = cycle(AP),
  passangers = as.numeric(AP)
)

# generate plot
layout(1:2) # omit using layout() when coding in a markdown notebook
ggplot(data = df_agg_AP, mapping = aes(x = time, y = agg_passangers)) +
  geom_line(color = "slateblue") +
  labs(title = "Aggregated PanAm Air Passangers 1949-1960", x = "Time", y = "Passangers (1000's)")+
  theme_minimal()

ggplot(data = df_cyclebox_AP, mapping = aes(x = factor(cycle), y = passangers)) + # use factor to identufy as a cetgory
  geom_boxplot(fill = "slateblue1") +
  labs(title = "Boxplot of Seasonal Passangers", x = "Time", y = "Passangers (1000's)") +
  theme_minimal()

########## 1.4.2 Unemployment Maine ##########
# import data set
# maine_path <- "http://www.massey.ac.nz/~pscowper/ts/Maine.dat" # no longer works
maine_path <- "https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/refs/heads/master/Maine.dat"
df_maine_month <- read.table(maine_path, header = TRUE)
attach(df_maine_month) # in orther to make columns accesibles by itself, no need to use maine_month as prefix
cat("The data is stored as a:", class(df_maine_month))

# data si changed from data.frame to ts
ts_maine_month <- ts(df_maine_month$unemploy, start = c(1991, 1), frequency = 12)
cat("The data is stored as a:", class(ts_maine_month))

#     using plot()
# organize data as an aggregated 12 month average
ts_maine_annual <- aggregate(ts_maine_month)/12

# generaate both plots
layout(1:2) # omit using layout() when coding in a markdown notebook
plot(ts_maine_month, main = "Unmenployment Maine", ylab = "Unmenployment (%)")
plot(ts_maine_annual,  main = "Unmenployment Maine Annual Average", ylab = "Unmenployment (%)")
