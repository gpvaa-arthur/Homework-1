# Instala e carrega o pacote para leitura de arquivos .csv
install.packages("readr") 
library(readr) 

# Registra os nomes das colunas
nomes_colunas <- c("","instant","dteday","season",
                   "weathersit","temp","casual","registered")

# Extrai a amostra de 300 observações
data_group <- read_csv("HW1_bike_sharing.csv", skip = 62, n_max = 300)
colnames(data_group) <- nomes_colunas

# Questão 2
total_user = data_group[, "casual", drop = TRUE] + data_group[, "registered", drop = TRUE]

# Item 1.
anyNA((data_group)) # Retorno no terminal (FALSE)

# Item 2.
mean(data_group[1:10, "temp", drop = TRUE])
mean(data_group[1:10, "casual", drop = TRUE])
mean(data_group[1:10, "registered", drop = TRUE])

median(data_group[1:10, "temp", drop = TRUE])
median(data_group[1:10, "casual", drop = TRUE])
median(data_group[1:10, "registered", drop = TRUE])

install.packages("DescTools") # Possui uma função para a moda
library(DescTools)

Mode(data_group[1:10, "temp", drop = TRUE])
Mode(data_group[1:10, "casual", drop = TRUE])
Mode(data_group[1:10, "registered", drop = TRUE])

# 300 observações
apply(data_group["temp"], 2, mean)
apply(data_group["casual"], 2, mean)
apply(data_group["registered"], 2, mean)

apply(data_group["temp"], 2, median)
apply(data_group["casual"], 2, median)
apply(data_group["registered"], 2, median)

apply(data_group["temp"], 2, Mode)
apply(data_group["casual"], 2, Mode)
apply(data_group["registered"], 2, Mode)

#Item 3.
quartis10 <- quantile(data_group[1:10, "temp", drop = TRUE], probs=c(0.25,0.50,0.75), type = 6)
IQR10 <- unname(quartis10[3]-quartis10[1])  
Ls10 <- unname(quartis10[3]) +1.5*IQR10
Li10 <- unname(quartis10[1]) - 1.5*IQR10

quartis10; IQR10; Ls10; Li10 # Exibe os valores no console

#Amostra inteira
quartis <- quantile(data_group[,"temp", drop = TRUE], probs=c(0.25,0.50,0.75), type = 6)
IQR <- unname(quartis[3]-quartis[1])  
Ls <- unname(quartis[3]) +1.5*IQR
Li <- unname(quartis[1]) - 1.5*IQR

quartis; IQR; Ls; Li # Exibe os valores no console

# Determinar outliers e data respectiva
encontrar_outliers <- function(data_group, ls, li){
  datas <- data_group[, "dteday", drop = TRUE]
  temperatura <- data_group[, "temp", drop = TRUE]
  
  data_temp <- data.frame(NA,NA)
  k <- 1; i <- 1
  while(i<length(temperatura)){
    if(temperatura[i] > ls || temperatura[i] < li){
      data_temp[k, "dteday"] <- datas[i]
      data_temp[k, "temp"]   <- temperatura[i]
      k <- k + 1
    }
  i <- i+1  
  }
  return(data_temp)
}
data_temp <- encontrar_outliers(data_group, Ls, Li)

# Box-Plot
boxplot(data_group[, "temp", drop = TRUE],
        main = "Boxplot de Distribuição de Temperaturas", # Título
        ylab = "Temperatura (°C)", # Legenda no eixo y
        col = "#81C784",           # Cor da caixa 
        border = "#2E7D32",        # Cor da borda     
        medcol = "#D32F2F",        # Cor da mediana    
        medlwd = 3,                # Espessura da mediana         
        boxwex = 0.35,             # Largura da caixa
        frame.plot = FALSE,        # Remove a moldura externa
        outline = TRUE)            # Desenha outliers (se houver)

# Histograma 
hist(data_group[, "temp", drop = TRUE],
     main = "Histograma de Distribuição de Temperaturas",
     xlab = "Temperatura (°C)",
     ylab = "Frequência absoluta",
     col = "#81C784",       
     border = "#2E7D32")  

#Item 5.
Q1_total_user <- unname(quantile(total_user, probs=c(0.25,0.50,0.75), type = 6))[1]
low_usage <- as.integer(total_user < Q1_total_user)
quantidade <- as.numeric(sum(low_usage == 1))
proporcao <- quantidade/300
