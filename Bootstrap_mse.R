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
var_empirico <- quantile(reclam, 0.995)

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

mse_resultados <- as.data.frame(mse_values_bootstrap)
head(mse_resultados, 3)

########## Graficas MSE ggplot2 ##########
# Graficar densidad del MSE
# x11() # comamndo para graficar en nueva terminal
ggplot(mse_resultados) +
  geom_density(aes(x = mse_values_bootstrap, color = "Simualaciones Bootstrap")) +
  geom_vline(aes(xintercept = var_empirico, colour = "VaR Empírico")) +
  geom_vline(aes(xintercept = mse_estimate, color = "MSE Estimado")) +
  geom_vline(aes(xintercept = ic_mse_inf, color = "IC MSE Inferior"), linetype = "longdash") +
  geom_vline(aes(xintercept = ic_mse_sup, color = "IC MSE Superior"), linetype = "longdash") +
  labs(title = "Densidad de MSE con simulacion Bootstrap", color = "Valores") +
  scale_color_manual(values = c("VaR Empírico" = "orange",
                                "MSE Estimado" = "darkgreen",
                                "IC MSE Inferior" = "seagreen",
                                "IC MSE Superior" = "seagreen",
                                "Simualaciones Bootstrap" = "steelblue")) +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", color = "white"))

########## Graficas MSE deafult plot ##########
# Graficar densidad del MSE
x11()
plot(density(mse_values_bootstrap), col = "steelblue")
  abline(v = var_empirico, col = "orange")
  abline(v = mse_estimate, col = "darkgreen")
  abline(v = ic_mse_inf, col = "seagreen", lty = 2)
  abline(v = ic_mse_sup, col = "seagreen", lty = 2)
  legend("topright",
         legend = c("Simulacion Bootstrap","VaR Empirico","MSE Estimado","IC del MSE"),
         col = c("steelblue","orange","darkgreen","seagreen"),
         lty = c(1,1,1,2))





