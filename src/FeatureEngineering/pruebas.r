require("data.table")
# 
 dt <- data.table(num1 = 1:5,                # Example data
                    num2 = c(3,5,7,6,4),
                    char = letters[1:5],
                    fac = as.factor(c("gr1", "gr2", "gr1", "gr3", "gr2")))
# 
# 
# dt[num1==3, num1:=54]

setwd( "C:/Users/Marcos/Documents/Maestria/DMEyF" )

ds_sep_crudo  <- "./datasetsOri/paquete_premium_202009.csv"
ds_nov_crudo  <- "./datasetsOri/paquete_premium_202011.csv"


dataset<-fread(ds_sep_crudo)
dataset_nov<-fread(ds_nov_crudo)

campos_buenos <-  setdiff(  colnames( dataset),  c("numero_de_cliente","internet","tmobile_app", "cmobile_app_trx", "Master_Finiciomora", "Master_fultimo_cierre", "Visa_Finiciomora", "Visa_fultimo_cierre") )



karch_dataset    <- "./datasets/dataset_epic_simple_v008.csv.gz"
ds_1  <- fread(karch_dataset)
