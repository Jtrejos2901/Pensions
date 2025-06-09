library(readr)
library(readxl)
library(ggplot2)
library(cowplot)
library(lubridate)
setwd("/Users/joseandres/Desktop/Pensiones/Pensions/Jose C/Bases_profe")
Activos <- read_csv("Activos.csv")
Antiguedades <- 2025-Activos$Año_ingreso
Activos <- Activos[,11:370]

IPC<- read_excel("/Users/joseandres/Desktop/Pensiones/Pensions/Jose C/Supuestos_financieros/Limpieza base de datis/IPC_historico.xlsx")


#Factores de ajuste 
FA<-1/(1+IPC$Variacion_mensual/100)
FA<-c(FA[-1],1)
FA<-FA[360:1]


matriz_FA <- t(matrix(FA, nrow = 1))


# Calcular la acumulación de productos de FA y repetirlo en orden inverso
FA_acumulado <- rev(cumprod(rev(FA)))
resultado <- matrix(rep(FA_acumulado,length(Activos$January.1995)),ncol = 360,nrow = length(Activos$January.1995),byrow = T)
Salarios <- as.matrix(Activos)
Salarios_flat <- resultado*Salarios

Indices <- matrix(0,nrow(Salarios_flat),30)

Salarios_Anual_Media = matrix(0,nrow(Salarios_flat),30)
Contador_Salarios_Anual = matrix(0,nrow(Salarios_flat),30)
matriz_antiguedad = matrix(0,nrow(Salarios_flat),30)
for (i in 1:nrow(Salarios_flat)) {
  for (j in 1:30) {
    Vect_Aux = Salarios_flat[i,(360-12*j+1):(360-12*j+12)]
    Contador_Salarios_Anual[i,j] = sum(Vect_Aux>0)
    Salarios_Anual_Media[i,j] = sum(Vect_Aux) / Contador_Salarios_Anual[i,j]
  }
}

for (j in 1:29) {
  Indices[,j] <- Salarios_Anual_Media[,j]/Salarios_Anual_Media[,j+1]
}


for (j in 1:nrow(Salarios_flat)) {
  anti <- Antiguedades[j]
  vector <- c(rep(0,30-(anti)+1),Indices[j,1:(anti-1)])
  if(anti==1){
    vector <- rep(0,30)
  }
  #vector2 <- Indices[anti:(30-anti)]
  #print(sum(vector2>0))
  print(length(vector))
  print(length(matriz_antiguedad[j,]))
  matriz_antiguedad[j,] <- vector
}

matriz_antiguedad[matriz_antiguedad==0] <- NA


Escala_Salarial <- data.frame(1:30,rev(colMeans(matriz_antiguedad,na.rm = T)))
mean(matriz_antiguedad,na.rm = T)

colnames(Escala_Salarial) <- c("Antigüedad","Escala")
Escala_Salarial[30,2] <- Escala_Salarial[29,2]



gES <- ggplot(data = Escala_Salarial,mapping = aes(x=Antigüedad,y=`Escala`))+geom_line(color="darkblue", linewidth = 1.5)+
  theme_minimal()+
  theme(axis.title.y = element_text(vjust=-0.5,size=rel(1), angle = 90), 
        legend.position = "bottom")+
          scale_y_continuous(limits = c(0.8, 1.5))

ggsave(filename = "ES.pdf",plot = gES,bg = "white")
