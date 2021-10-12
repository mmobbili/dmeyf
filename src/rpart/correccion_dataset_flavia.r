kscript           <- "320_rpart_BO"
karch_generacion  <- "./datasetsOri/paquete_premium_202009.csv"
karch_aplicacion  <- "./datasetsOri/paquete_premium_202011.csv"
kBO_iter    <-  200   #cantidad de iteraciones de la Optimizacion Bayesiana

#cargo los datasets
dataset  <- fread(karch_generacion)   #donde entreno
# se cambia el dataset

campos_buenos <-  setdiff(  colnames( dataset),  c("numero_de_cliente","internet","tmobile_app") )

datasetA<-dataset[ , campos_buenos,   with=FALSE ]
dataset<-datasetA
rm(datasetA)

dataset$clase_ternaria<-factor(dataset$clase_ternaria)
#dataset$cmobile_app_trx<-as.numeric(dataset$cmobile_app_trx > 0.5)  #al final lo dejo que patine

#------------------------------------------- correccion margen activos
N<-200
library(infotheo)
variable<-dataset$mactivos_margen
bin_eq_freq <- discretize(variable,"equalfreq",N)
# Nos copiamos el atributo original
bin_eq_freq$Y = variable
# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
for(bin in 1:N){
  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = mean(bin_eq_freq$Y[bin_eq_freq$X==bin])
}
dataset$mactivos_margen_rank<-bin_eq_freq$suavizado
rm(bin_eq_freq)

dapply  <- fread(karch_aplicacion)    #donde aplico el modelo
dapply$cliente_antiguedad<-dapply$cliente_antiguedad-2 # se refiere al mes del entrenamiento

variable<-dapply$mactivos_margen
bin_eq_freq <- discretize(variable,"equalfreq",N)
# Nos copiamos el atributo original
bin_eq_freq$Y = variable
# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
for(bin in 1:N){
  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = mean(bin_eq_freq$Y[bin_eq_freq$X==bin])
}
dapply$mactivos_margen_rank<-bin_eq_freq$suavizado
dataset<-dataset[,-c("mactivos_margen")]
dapply<-dapply[,-c("mactivos_margen")]

#------------------------------------- correccion atm_other y atm
variable<-ifelse(dataset$catm_trx_other==0, 0, dataset$matm_other/dataset$catm_trx_other)
dataset$matm_other_extraccion<-variable
variable<-ifelse(dataset$catm_trx==0, 0, dataset$matm/dataset$catm_trx)
dataset$matm_extraccion<-variable


variable<-ifelse(dapply$catm_trx_other==0, 0, dapply$matm_other/dapply$catm_trx_other)
dapply$matm_other_extraccion<-variable
variable<-ifelse(dapply$catm_trx==0, 0, dapply$matm/dapply$catm_trx)
dapply$matm_extraccion<-variable
rm(variable)

dataset<-dataset[,-c("matm","matm_other")]
dapply<-dapply[,-c("matm","matm_other")]


#-----------------------------------  correcion Master_Finicio mora

library("lubridate")
correccion_habiles <- function(fin_de_mes,x)
{ 
  if (!is.na(x)){
    dias_mora<-as.numeric(x)
    if (dias_mora>0){
      rango<-fin_de_mes-days(seq(0,dias_mora-1))
      feriados<-date(c("2020-07-09","2020-07-10","2020-08-17","2020-10-12","2020-11-23"))#faltan cargar todos los feriados del año
      rangof<-rango[!rango %in% feriados]                                   #se sacan los dias que son feriados de la secuencia
      #print(rangof)
      rm(rango)
      return (sum(!weekdays(rangof) %in% c("Saturday", "Sunday")))
    }
    else{ return (0)    } 
  }
  else {
    return (x)
  }
}

fin_foto_mes<-date(ym(dataset$foto_mes[1]))+months(1) - days(1)
dataset$Master_Finiciomora_h<-ifelse(!is.na(dataset$Master_Finiciomora),dataset$Master_Finiciomora+5,NA)
zz<-data.frame(dataset$Master_Finiciomora_h)
niveles<-levels(factor(zz[[1]]))
niveles_corregido<-rep(0,length(niveles))
for (i in 1:length(niveles)){
  niveles_corregido[i]<-correccion_habiles(fin_foto_mes,niveles[i])
  dataset$Master_Finiciomora_h[dataset$Master_Finiciomora_h == as.numeric(niveles[i])] <- as.numeric(niveles_corregido[i])
}
rm(niveles)
rm(niveles_corregido)
rm(zz)

fin_foto_mes<-date(ym(dapply$foto_mes[1]))+months(1) - days(1)
dapply$Master_Finiciomora_h<-dapply$Master_Finiciomora
zz<-dapply[,"Master_Finiciomora"]
niveles<-levels(factor(zz[[1]]))
niveles_corregido<-rep(0,length(niveles))
for (i in 1:length(niveles)){
  niveles_corregido[i]<-correccion_habiles(fin_foto_mes,as.numeric(niveles[i]))
  dapply$Master_Finiciomora_h[dapply$Master_Finiciomora_h == as.numeric(niveles[i])] <- as.numeric(niveles_corregido[i])
}
rm(niveles)
rm(niveles_corregido)
rm(zz)

dataset<-dataset[,-c("Master_Finiciomora")]
dapply<-dapply[,-c("Master_Finiciomora")]