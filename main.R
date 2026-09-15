install.packages("readr") 
install.packages("dplyr")

library(readr) # Leitura de arquivos .csv
library(dplyr) # Ver se precisa realmente

nomes_colunas <- c("","instant","dteday","season","weathersit","temp","casual","registered")

data_group <- read_csv("HW1_bike_sharing.csv", skip = 62, n_max = 300)
colnames(data_group) <- nomes_colunas
