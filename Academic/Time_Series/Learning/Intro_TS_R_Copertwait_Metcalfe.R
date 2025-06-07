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
plot(AP, main = "PanAm Air Passangers 1949-1960", ylab = "Passengers (1000's)", col = "slateblue")

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
plot(aggregate(AP), main = "Aggregated PanAm Air Passangers 1949-1960", ylab = "Passangers (1000's)", col = "slateblue")
boxplot(AP ~ cycle(AP), main ="Boxplot of seasonal values", xlab = "Month", ylab = "Passangers (1000's)", col = "slateblue1")

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
layout(1:2) # omit using layout() when coding in a markdown
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
attach(df_maine_month) # in order to make columns accesibles by itself, no need to use maine_month as prefix
cat("The data is stored as a:", class(df_maine_month))

# data si changed from data.frame to ts
ts_maine_month <- ts(df_maine_month$unemploy, start = c(1991, 1), frequency = 12)
cat("The data is stored as a:", class(ts_maine_month))

#     using plot()
# organize data as an aggregated 12 month average
ts_maine_annual <- aggregate(ts_maine_month)/12

# generaate both plots
layout(1:2) # omit using layout() when coding in a markdown notebook
plot(ts_maine_month, main = "Unemployment Maine", ylab = "Unemployed (%)", col = "sienna")
plot(ts_maine_annual,  main = "Unemployment Maine Annual Average", ylab = "Unemployed (%)", col = "sienna")

#   using ggplot2()
# organize original data to add time
df_maine_month$time <- as.numeric(time(ts_maine_month)) # note: if working with a R markdown do not use pipe %>%
df_maine_annual <- data.frame(
  time = as.numeric(time(ts_maine_annual)),
  unemploy_annual_avg = as.numeric(ts_maine_annual)
)

# generate plot
ggplot(df_maine_month, mapping = aes(x = time, y = unemploy)) +
  geom_line(color = "sienna") +
  labs(title = "Unemployment Maine", y = "Unemployed (%)") +
  theme_minimal()

ggplot(df_maine_annual, mapping = aes(x = time, y = unemploy_annual_avg)) +
  geom_line(color = "sienna") +
  labs(title = "Unemployment Maine Annual Average", y = "Unemployed (%)") +
  theme_minimal()

# window function to obtain the ts for a month
ts_maine_feb <- window(ts_maine_month, start = c(1991, 2), frequency = TRUE)
ts_maine_aug <- window(ts_maine_month, start = c(1991, 8), frequency = TRUE)

# comparing to the specific months
ts_maine_feb
ts_maine_month

# ratio from window functions
feb_ratio <- mean(ts_maine_feb) / mean(ts_maine_month)
aug_ratio <- mean(ts_maine_aug) / mean(ts_maine_month)

# output results
cat("The ratio for February compared to the mean is:", feb_ratio, ", and for August:", aug_ratio)

# import the data
#USAunemp_path <- "http://www.massey.ac.nz/~pscowper/ts/USunemp.dat"
USAunemp_path <- "https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/refs/heads/master/USunemp.dat"
df_USA_month <- read.table(USAunemp_path, header = TRUE)
#attach(ts_USA_month)
ts_USA_month <- ts(df_USA_month, start = c(1996, 1), end = c(2006, 10), frequency = 12)
cat("Verify class object:", class(ts_USA_month))

#   using plot()
plot(ts_USA_month, main = "Monthly Unemployment USA 1996-2006", ylab = "Unemployed (%)", col = "sienna4")

#   using ggplot2()
# organizing ts as a data frame, add time column
df_USA_month <- data.frame(
  time = as.numeric(time(ts_USA_month)),
  USA_unemp = as.numeric(ts_USA_month)
)

# generate plot
ggplot(df_USA_month, mapping = aes(x = time, y = USA_unemp)) +
  geom_line(color = "sienna4") +
  labs(title = "Monthly Unemployment USA 1996-2006", y = "Unemployed (%)") +
  theme_minimal()
