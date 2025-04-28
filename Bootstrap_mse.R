########## Librerias ########## 
library(pacman)
library(ggplot2)
library(tidyverse)
library(readr)
library(actuar)

########## Datos ##########
options(scipen = 999) #evitar notacion científica
set.seed(12345)
reclam = rlnorm(20, meanlog = 2.5, sdlog = 2.5) # Distr. de reclamaciones


########## IC de MSE ##########
# Estimación del MSE usando bootstrap
bootstrap_mse <- function(data, n_bootstrap) {
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mse_values)
}

n_bootstrap <- 1000
mse_values_bootstrap <- bootstrap_mse(reclam, n_bootstrap)
mse_estimate <- mean(mse_values_bootstrap)
cat("El MSE es", format(mse_estimate, big.mark = ","), "\n")

ic_mse_inf <- quantile(mse_values_bootstrap, 0.025)
ic_mse_sup <- quantile(mse_values_bootstrap, 0.975)

cat("El IC inferior", format(ic_mse_inf, big.mark = ","), "\n")
cat("El IC superior", format(ic_mse_sup, big.mark = ","), "\n")

# Graficar densidad del MSE
ggplot(mse_values_bootstrap, aes(x = mse_values_bootstrap)) +
  geom_line(fill = seagreen, alpha = 0.5)

plot(density(mse_values_bootstrap))



