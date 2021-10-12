#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("parallel")
require("rpart")
library(pROC)
#library(ROCR)
require(rpart.plot)

#cargo los datasets que voy a comparar
setwd("C:/Users/Flavia/Documents/DataScience/dmeyf") 


#dataset1  <- fread( "./datasetsOri/paquete_premium_202009.csv" )
#dataset2  <- fread( "./datasetsOri/paquete_premium_202011.csv" )

dataset1  <- fread( "./datasets/paquete_premium_202009_ext.csv" )
dataset2  <- fread( "./datasets/paquete_premium_202011_ext.csv" )



#campos_buenos<-c("Master_Finiciomora")

#campo<-c("mpayroll","mpasivos_margen","mcuentas_saldo","Visa_msaldototal","mactivos_margen","ctarjeta_debito_transacciones","mcuenta_corriente","Visa_msaldopesos","mrentabilidad_annual","ctarjeta_visa_transacciones","ctrx_quarter","Visa_fechaalta","mcaja_ahorro","Visa_mpagominimo","cproductos","Visa_status","mtarjeta_visa_consumo","cpayroll_trx","mprestamos_personales","cliente_edad")
#campo<-c("ccajas_transacciones", "Master_mpagominimo","internet","tmobile_app","Master_Finiciomora")

#campo<-c("ccajas_transacciones","internet","tpaquete1", "mcaja_ahorro_dolares", "mcajeros_propios_descuentos","mtarjeta_visa_descuentos","ctarjeta_master_descuentos","cmobile_app_trx", "Master_madelantodolares")



ksemillas  <- c(100003, 101207, 103577, 103457, 104089) #reemplazar por las propias semillas


#------------------------------------------------------------------------------

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolSimple  <- function( fold_test, data, param )
{
  #genero el modelo
  modelo  <- rpart("tipo ~ .", 
                   data= data[ fold != fold_test, ], #training  fold==1
                   xval= 0,
                   control= param )
  
  #aplico el modelo a los datos de testing, fold==2
  prediccion  <- predict( modelo, data[ fold==fold_test, ], type = "prob")
  
  prob_A  <- prediccion[, "A"]
  
  result.roc<- pROC::roc(ifelse(data[ fold==fold_test,tipo]=="A",1,0), prob_A) 
  
  #print(auc(result.roc))
  #result.modelo<-summary(modelo)
  variable_importante<-names(modelo$variable.importance[1])
  ganancia_testing<-auc(result.roc)
  
  #impresion elaborada del arbol
  #pdf(file ="./work/Arbol_drift.pdf", paper="usr" )
  #prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
  #dev.off()
  return( list(ganancia_testing,variable_importante))
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( data, param, qfolds, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla )
  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a 5 si posee Linux o Mac OS
  
  zz<-unlist(unlist( ganancias ))
  print(zz)
  ganancia_media<-mean(as.numeric(zz[seq(1,length(zz),2)]))
  
  #devuelvo la primer ganancia y el promedio
  return( list(ganancia_media,zz[[2]]))   #saco el promedio
}
#------------------------------------------------------------------------------


#inicializo la tabla donde voy a dejar los resultados
tb_resultados  <- data.table(AUC=numeric(), variable_mas_drift=character())

campos_malos<-c("numero_de_cliente","foto_mes","clase_ternaria","internet","cmobile_app_trx","tmobile_app")
#campos_malos1<-c("numero_de_cliente","foto_mes","clase_ternaria" )
campos_buenos <-  setdiff(  colnames( dataset1),  campos_malos )

df1<-dataset1[,c(campos_buenos),with=FALSE]
df1[, tipo := as.factor(rep("A",dim(df1)[1]))]
rm(dataset1)

#df1[,cliente_antiguedad:=cliente_antiguedad+2]      #le agrego 2 meses de antiguedad
#df1[,mactivos_margen:=mactivos_margen-774.1267]
#df1[,Master_fultimo_cierre:=Master_fultimo_cierre+5] #le agrego 5 dias de la diferencia de vencimientos
#df1[,Visa_fultimo_cierre:=Visa_fultimo_cierre+5] #le agrego 5 dias de la diferencia de vencimientos

df2<-dataset2[,c(campos_buenos),with=FALSE]
df2[, tipo := as.factor(rep("B",dim(df2)[1]))]
rm(dataset2)

dataset<-rbindlist(list(df1, df2))
rm(df1)
rm(df2)

for(  jj in  seq(1,15))#,5,6,7,8,9,10,11) )
{
  campos_buenos <-  setdiff(  colnames( dataset),  campos_malos )
  
  vmaxdepth<-5
  param_basicos  <- list( "cp"=0, "maxdepth"= vmaxdepth,  "xval"=0, "minsplit"=  80, "minbucket"=5, "maxdepth"= 8)
  gan  <- ArbolesCrossValidation( dataset[,c(campos_buenos),with=FALSE], 
                                  param_basicos, 
                                  qfolds= 5, # 5-fold cross validation
                                  semilla= ksemillas[1] )  #uso solo la primer semilla para particionar el dataset
  zz<-unlist(gan)
  tb_resultados  <- rbind( tb_resultados, list(zz[[1]],zz[[2]]) )
  campos_malos<-c(campos_malos,c(unlist(tb_resultados[jj,variable_mas_drift])))
}

tb_resultados

fwrite(tb_resultados, "./work/DataDrift_rpart_new_dataset.csv")