library(readxl)
library(tidyverse)
library(lubridate)
library(MESS)
library(xtable)
#library(xlsx)

setwd("/Users/joseandres/Desktop/Pensiones/Pensions/Jose C/Bases_profe")

#----------Activos e Inactivos-------------------

#Se carga la base de datos
Cotizantes <- read_excel("BDCotizantes.xlsx")

#Se separan los salarios que son lo que se va a trabajar
Salarios<-Cotizantes[,c(4:ncol(Cotizantes))]

Salarios[,1:ncol(Salarios)%%12==4]<-Salarios[,1:ncol(Salarios)%%12==4]/2

#Se etiquetan con su mes respectivo 
inicio <- as.Date("1995-01-01")
mes<-format(seq(from=inicio, by="month", length.out=ncol(Salarios)), "%B")
año<-format(seq(from=inicio, by="month", length.out=ncol(Salarios)), "%Y")
colnames(Salarios)<-paste(mes,año,sep = ".")

#Se revisa casos con salarios registrados, para eliminar el caso en que no los tengan 
Cotizantes_V2<-cbind(Cotizantes[1:3],Salarios)
SumInd_Salarios<-rowSums(Salarios>0)
Excluidos_SinSalarios<-Cotizantes_V2[SumInd_Salarios==0,]
Cotizantes_V2<-Cotizantes_V2[SumInd_Salarios>0,]
Salarios<-Salarios[SumInd_Salarios>0,]


#Se revisa la base en busqueda de salarios inferiores al mínimo 

IPC<- read_excel("/Users/joseandres/Desktop/Pensiones/Pensions/Jose C/Supuestos_financieros/Limpieza base de datis/IPC_historico.xlsx")
#Factores de ajuste 
Factores_ajuste<-1/(1+IPC$Nivel)
Factores_ajuste<-c(Factores_ajuste[-1],1)
Factores_ajuste<-Factores_ajuste[30:1]
Factores_acum<-cumprod(Factores_ajuste)
Factores_acum<-Factores_acum[30:1]

#Se generan los salarios mínimos, se asume que estos cambian de manera anual 
Salarios_Minimos<-Factores_acum*10000
Salarios_Minimos<-rep(Salarios_Minimos,times = 12)
Salarios_Minimos<-matrix(Salarios_Minimos,ncol = 12 )
Sal<-as.vector(t(Salarios_Minimos))

#Ya una vez se tiene el histórico, entonces se hacen matriz para compararlos fácilmente 
Salarios_Minimos<-rep(Sal,times = nrow(Salarios))
Salarios_Minimos<-matrix(Salarios_Minimos,ncol = ncol(Salarios) ,byrow = T)

#Se buscan los salarios que generen inconsistencias 
IndicadoraMin<-(Salarios<Salarios_Minimos)&(Salarios>0)
SumIndMin<-rowSums(IndicadoraMin)

N<-nrow(Salarios)#Cantidad de afiliados en la matriz
M<-ncol(Salarios)#Cantidad de periodos en la matriz

PrimeroInvalido<-numeric(N)
UltimoInvalido<-numeric(N)
Minimo_invalido<-numeric(N)
for (i in 1:N) {
 if(SumIndMin[i]>0){
   #Si un valor es inferior al mínimo se suma al anterior
   for (j in M:2) {
     if(Salarios[i,j]>0 & Salarios[i,j]<Salarios_Minimos[i,j]){
       if(sum(Salarios[i,c(1:(j-1))])==0){
         PrimeroInvalido[i]<-1
         break
       }else{
         Salarios[i,(j-1)]<-Salarios[i,(j-1)]+Salarios[i,j]
         Minimo_invalido[i]<-Minimo_invalido[i]+1
         Salarios[i,j]<-0
       }}}
   #En caso de que el último valor sea inferior al mínimo se pasa para adelante hasta tener ya una cuota válida
   for (j in 1:(M-1)) {
     if(Salarios[i,j]>0 & Salarios[i,j]<Salarios_Minimos[i,j]){
       if(sum(Salarios[i,c(j:(M-1))])==0){
         UltimoInvalido[i]<-1
         break
       }else{
         Salarios[i,(j+1)]<-Salarios[i,(j+1)]+Salarios[i,j]
         Salarios[i,j]<-0
         Minimo_invalido[i]<-Minimo_invalido[i]+1
       }}}}}


sum(rowSums((Salarios<Salarios_Minimos)&(Salarios>0))) 
sum(UltimoInvalido)
sum(PrimeroInvalido)
summary(as.factor(Minimo_invalido))

Salarios_Resp<-Salarios 
Salarios<-Salarios_Resp
#Se generan los salarios máximos, se asume que estos cambian de manera anual
Salarios_Maximos<-Factores_acum*5000000
Salarios_Maximos<-rep(Salarios_Maximos,times = 12)
Salarios_Maximos<-matrix(Salarios_Maximos,ncol = 12 )
Sal_Max<-as.vector(t(Salarios_Maximos))

#Ya una vez se tiene el histórico, entonces se hacen matriz para compararlos fácilmente 
Salarios_Maximos<-rep(Sal_Max,times = nrow(Salarios))
Salarios_Maximos<-matrix(Salarios_Maximos,ncol = ncol(Salarios) ,byrow = T)

#Se buscan los salarios que generen inconsistencias 
IndicadoraMax<-(Salarios>Salarios_Maximos)
SumIndMax<-rowSums(IndicadoraMax)
summary(as.factor(SumIndMax))


#Para determinar la cantidad de activos e inactivos a diciembre de cada año, se hará a grandes rasgos para tener una noción del histórico 

#Ya habiendo comprobado que no queda nigún salario en la franja de 0 al mínimo se tiene que si un año tiene una cuota, entonces esta es válida. 

año<-1995
SalariosAnuales<-data.frame(rowSums(Salarios[,1:12]))
colnames(SalariosAnuales)<-as.character(año)

for (k in 1:(ncol(Salarios)/12-1)) {
  Aux<-data.frame(rowSums(Salarios[,(k*12 +1):((k+1)*12)]))
  colnames(Aux)<-as.character(año+k)
  SalariosAnuales<-cbind(SalariosAnuales,Aux )
}

IndActivo<-SalariosAnuales>0#Se observa en que año es activa cada persona 
IndInactivo<-SalariosAnuales<0#Se inicializa en falso 

#Se crea la indicadora de inactivos como tal, 
for (i in 1:N) {
  for (j in 2:ncol(SalariosAnuales)) {
    if(!IndActivo[i,j] & sum(IndActivo[i,1:(j-1)])>0){
      IndInactivo[i,j]<-TRUE
    }}}
  
mat=matrix(ncol = ncol(SalariosAnuales),nrow = N)
Estados<-data.frame(mat)
Estados[1:N,1:ncol(Estados)]<-"No aplica"
Estados[IndActivo]<-"Activo"
Estados[IndInactivo]<-"Inactivo"
Estados<-cbind(Cotizantes_V2[,1],Estados)
colnames(Estados)<-c("ID",colnames(SalariosAnuales))

Estados_largo<-Estados %>% pivot_longer("1995":"2024",names_to = "Año",values_to = "Estado")

Conteos<-Estados_largo %>% group_by(Año,Estado) %>% summarise(Totales=n())

Estados_conteos<-Conteos %>% pivot_wider(names_from = Año, values_from = Totales)
Estados_conteos<-Estados_conteos[c(1,3,2),]

Estados_final<-data.frame(t(Estados_conteos[-1]))
colnames(Estados_final)<-c("Activos","Inactivos","No aplica")
Estados_final[is.na(Estados_final)]<-0
Estados_final2<-Estados_final[,-3]

print(xtable(Estados_final2,digits = c(0)))

#Se asignan los nuevos salarios corregidos y se toma el último valor de la columna de Estados para asignarlo a las personas respectivas 

Cotizantes_V3<-cbind(Cotizantes_V2[1:3],Salarios)
Cotizantes_V3<-cbind(Cotizantes_V3,data.frame(Estado = Estados[,31]))

#Se calcula la fecha de ingreso para cada persona dados los salarios 

mes_entrada<-numeric(N)
for (i in 1:N) {
  for (j in 1:M) {
    if(Salarios[i,j]==0){
      mes_entrada[i]<-mes_entrada[i]+1 
    }else{
      break
    }
  }
}

fecha_entrada<-inicio%m+% months(mes_entrada)

#Se calcula la edad y la antiguedad, se añaden a además de la fecha de ingreso y año de ingreso 

fecha_corte<-as.Date("2024-12-31")
edad<-age(as.Date(Cotizantes_V3$Fec.Nac),fecha_corte)
antiguedad<-age(as.Date(fecha_entrada),fecha_corte)
edad_ingreso<-age(as.Date(Cotizantes_V3$Fec.Nac),as.Date(fecha_entrada))


Cotizantes_V3<-cbind(Cotizantes_V3[1:3],data.frame(Edad= edad, Fecha_ingreso = fecha_entrada, Año_ingreso = format(fecha_entrada, "%Y"), Edad_ingreso = edad_ingreso, Antiguedad = antiguedad), Cotizantes_V3[,4:ncol(Cotizantes_V3)])

#Se generan rangos de edades 
limites <- c(0, 19, 29,39, 49, 59, 69, 79,89, Inf)+1
limites2 <- c(0, 5, 10,15, 20, 25, 30, Inf)
# Definir los nombres de los rangos
nombres_rangos <- c("Menos de 20", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 a 79","80 a 89", "90 o más")
nombres_rangos2 <- c("Menos de 5", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29", "30 o más")

Cotizantes_V3$Rango_Edad <- cut(Cotizantes_V3$Edad, breaks = limites, labels = nombres_rangos, right = FALSE)
Cotizantes_V3<-Cotizantes_V3[,c(1:4,ncol(Cotizantes_V3),5:(ncol(Cotizantes_V3)-1))]

Cotizantes_V3$Rango_Antiguedad <- cut(Cotizantes_V3$Antiguedad, breaks = limites2, labels = nombres_rangos2, right = FALSE)
Cotizantes_V3<-Cotizantes_V3[,c(1:9,ncol(Cotizantes_V3),10:(ncol(Cotizantes_V3)-1))]


#----------Tablas de activos------------------- 
Activos<-Cotizantes_V3[Cotizantes_V3$Estado=="Activo",-ncol(Cotizantes_V3)]

limites3<- c(0, 200000, 400000,600000,800000, 100000, 1200000, 1400000,1600000, 1800000,2000000,2500000,3000000, Inf)
nombres_rangos3<-c("Menos de 200", 
                   "200 a 400", 
                   "400 a 600", 
                   "600 a 800", 
                   "800 a 1 000", 
                   "1 000 a 1 200", 
                   "1 200 a 1 400", 
                   "1 400 a 1 600", 
                   "1 600 a 1 800", 
                   "1 800 a 2 000",
                   "2 000 a 2 500",
                   "2 500 a 3 000",
                   "3 000 o más")

Sal_total_anual<-rowSums(Activos[359:370])
total_salarios<-rowSums(Activos[359:370]>0)
Activos$Salario_prom<-Sal_total_anual/total_salarios
Activos$Rango_Salarial <- cut(Activos$Salario_prom, breaks = limites3, labels = nombres_rangos3, right = FALSE)

write.csv(Activos, "Activos.csv", row.names=FALSE)



#----------Inactivos------------------- 
Inactivos<-Cotizantes_V3[Cotizantes_V3$Estado=="Inactivo",-ncol(Cotizantes_V3)] 
write.csv(Inactivos, "Inactivos.csv", row.names=FALSE)


#----------Pensionados------------------- 

Pensionados <- read_excel("BD Pensionados.xlsx")
Pensionados <- Pensionados[,1:8]
Pensionados$Año_rige = format(Pensionados$`Rige de la Pensión`, "%Y")
Pensionados$Edad <- age(as.Date(Pensionados$FEC_NAC),fecha_corte)
Pensionados$Rango_Edad <- cut(Pensionados$Edad, breaks = limites, labels = nombres_rangos, right = FALSE)
Pensionados<-Pensionados[,c(1:5,10,11,6:9)]

limites4 <- c(0, 100000, 200000,300000,400000, 500000, 600000, 700000,800000, 900000,1000000, Inf)
nombres_rangos4<-c("Menos de 100", 
                   "100 a 200", 
                   "200 a 300", 
                   "300 a 400", 
                   "400 a 500", 
                   "500 a 600", 
                   "600 a 700", 
                   "700 a 800", 
                   "800 a 900", 
                   "900 a 1 000", 
                   "1 000 o más")
Pensionados$Rango_Pension <- cut(Pensionados$MONTO, breaks = limites4, labels = nombres_rangos4, right = FALSE)

Pensionados_valuacion<-Pensionados[,c(1,2,3,5,6,8)]
Pensionados_valuacion$MONTO<-Pensionados_valuacion$MONTO*13
Pensionados_valuacion$COD_TIPO_PENSION[Pensionados_valuacion$COD_TIPO_PENSION=="Vejez"]<-1
Pensionados_valuacion$COD_TIPO_PENSION[Pensionados_valuacion$COD_TIPO_PENSION=="Invalidez"]<-2
Pensionados_valuacion$COD_TIPO_PENSION[Pensionados_valuacion$COD_TIPO_PENSION=="Sucesión"]<-3
Pensionados_valuacion$COD_TIPO_PENSION<-as.numeric(Pensionados_valuacion$COD_TIPO_PENSION)

Pensionados_valuacion$COD_PARENTESCO[Pensionados_valuacion$COD_PARENTESCO=="C"]<-1
Pensionados_valuacion$COD_PARENTESCO[Pensionados_valuacion$COD_PARENTESCO=="H"]<-2
Pensionados_valuacion$COD_PARENTESCO<-as.numeric(Pensionados_valuacion$COD_PARENTESCO)

write.csv(Pensionados_valuacion, "Pensionados_valuacion.csv", row.names=FALSE)
