####################################################
# Book:    Introductory Time Series with R
# Author:  Paul SP Coperwait & Andrew V Metcalfe
# Reader:  Braulio Canedo B
####################################################
########## Libraries ##########
# Plots
library(ggplot2)

#          1 Time Series Data
########## 1.4.1 Air Passanger bookings ##########
# import data from R
data("AirPassengers")
AP <- AirPassengers

# to visualize the data
AP # or View(AP)

# print the type of object 
cat("Time of object:", class(AP))

# plot the initial time serie
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
  labs(tittle = "PanAm Air Passangers 1949-1960", x = "Time", y = "Passengers (1000's)") + 
  theme_minimal()
