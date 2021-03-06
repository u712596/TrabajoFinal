#importando dataframe
data = read.csv(file = 'bmw.csv')
data

summary(data)
order(data)

#datos perdidos = 0
#En este caso podr�amos tomar "other" dentro de fuelType como valores omitidos
table(data$fuelType)


#valores at�picos univariados o multivariados
table(data$model)
mean(data$price)
min(data$price)
max(data$price)


is.outlier_z <- function(x, k=2) {
  return(abs(scale(x)) > k)           # scale: (x-media)/desv_est
}

# �ndices (T/F) de los precios at�picos
idx_outliers_z <- is.outlier_z(data$price, k=3)
which(idx_outliers_z)

# Precios at�picos
data$price[idx_outliers_z]

# Registros asociados con los precios at�picos
data[idx_outliers_z, ]

# Outliers usando Box Plots
# """""""""""""""""""""""""
library(dplyr)
library(ggplot2)

# Outliers usando la regla de Tukey
is.outlier <- function(x, k=1.5) {
  return(x < quantile(x,0.25)-k*IQR(x) | x > quantile(x,0.75)+k*IQR(x))
}

# �ndices (T/F) de precios at�picos
idx_outliers <- is.outlier(data$price, k=3)

# precios "outlier"
data$price[idx_outliers]

# Registros asociados con los precios "outliers"
data[idx_outliers,]

# Boxplots de precios // variables
bp1 <- data %>%
  mutate(outlier = ifelse(is.outlier(price), price, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = price)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
bp1

bp2 <- data %>%
  mutate(outlier = ifelse(is.outlier(mileage), price, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = mileage)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
bp2

bp3 <- data %>%
  mutate(outlier = ifelse(is.outlier(tax), price, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = tax)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
bp3

bp4 <- data %>%
  mutate(outlier = ifelse(is.outlier(year), price, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = year)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
bp4

bp5 <- data %>%
  mutate(outlier = ifelse(is.outlier(mpg), price, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = mpg)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
bp5

bp6 <- data %>%
  mutate(outlier = ifelse(is.outlier(engineSize), price, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = engineSize)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
bp6

# install.packages('ggpubr')
library(ggpubr)
# Generar una sola gr�fica
final_plot <- annotate_figure(
  ggarrange(bp1, bp2, bp3, bp4, bp5, bp6, ncol=3, nrow=3),
  top = text_grob("An�lisis Univariado de Valores Extremos", size = 15))
final_plot