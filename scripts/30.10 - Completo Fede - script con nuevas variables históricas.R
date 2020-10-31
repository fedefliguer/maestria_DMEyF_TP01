library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
library(readr)  # for read_csv
library(knitr)  # for kable
library(lightgbm)

set.seed(1)

#limpio la memoria
rm( list=ls() )
gc()

setwd("/home/fjf_arg_gcloud/buckets/b1/datasetsOri/")

kcampos_separador               <-  "\t"
# karchivo_entrada_7meses_zip     <-  "paquete_premium_201906_202001.txt.gz"
# ds <- fread(karchivo_entrada_7meses_zip, header=TRUE, sep=kcampos_separador)
karchivo_entrada_full     <-  "paquete_premium.txt.gz"
ds <- fread(karchivo_entrada_full, header=TRUE, sep=kcampos_separador)

# Agrego columnas de FE
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/nuevas_columnas.R")
nuevas_columnas(ds)


myfile <- "https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/otros/importances.csv"
importances <- read_csv2(myfile)

importances_max = data.table(importances[, c("Feature", "Maxima")])
importances_max = importances_max[Maxima>0.003]
importances_max$Feature = gsub("__tend","",importances_max$Feature)
importances_max$Feature = gsub("__max","",importances_max$Feature)
importances_max$Feature = gsub("__min","",importances_max$Feature)
importances_max$Feature = gsub("__ravg","",importances_max$Feature)
variables = unique(importances_max$Feature)

ds = ds[, c("foto_mes", "numero_de_cliente", variables, "clase_binaria"), with=FALSE]

# Agrego variables históricas
periodos = c(3,6,12)
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/variables_historicas_mmovil_multiperiod.R")
variables_historicas_fe_multiperiod(ds, periodos)

# Separo en muestras
train = c(201810, 201811, 201812, 201901, 201902, 201903, 201904, 201905, 201906, 201907, 201908, 201909) # Cambiar con el período que querramos entrenar
test = c(201901, 201911) # Cambiar con el período que querramos entrenar

source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/separacion_conjuntos.R")
separacion_conjuntos(ds, train, test)

ds_train[ , clase01 :=  ifelse( clase_binaria=="evento", 1L, 0L)  ]
ds_test[ , clase01 :=  ifelse( clase_binaria=="evento", 1L, 0L)  ]
enero[ , clase01 :=  ifelse( clase_binaria=="evento", 1L, 0L)  ]

nov19 = ds_test[foto_mes == 201911]
ene19 = ds_test[foto_mes == 201901]

campos_buenos  <- setdiff(  colnames(ds_train) ,  c("clase_binaria","clase01") )

dgeneracion  <- lgb.Dataset( data= data.matrix(  ds_train[ , campos_buenos, with=FALSE]),
                             label= ds_train$clase01,
                             free_raw_data= FALSE )

# CASO 1

optimo  <- list( num_iterations= 1468, learning_rate= 0.01503084, feature_fraction= 0.2575957, min_gain_to_split= 0.01428075, num_leaves= 94, lambda_l1= 3.758696, lambda_l2= 0.341236)

modelo  <- lgb.train( data= dgeneracion,
                        objective= "binary",
                        boost_from_average= TRUE,
                        max_bin= 31,
                        num_iterations=    optimo$num_iterations,
                        learning_rate=     optimo$learning_rate,
                        feature_fraction=  optimo$feature_fraction,
                        min_gain_to_split= optimo$min_gain_to_split,
                        num_leaves=        optimo$num_leaves,
                        lambda_l1=         optimo$lambda_l1,
                        lambda_l2=         optimo$lambda_l2
)

prediccion_201901  <- predict( modelo, 
                                 data.matrix( ene19[, campos_buenos, with=FALSE ])                                 )

ganancia_201901  <- sum( (prediccion_201901 > 0.025) * 
                             ene19[, ifelse( clase_binaria=="evento",29250,-750)])

prediccion_201911  <- predict( modelo, 
                                 data.matrix( nov19[, campos_buenos, with=FALSE ])                                 )

ganancia_201911  <- sum( (prediccion_201911_2 > 0.025) * 
                             nov19[, ifelse( clase_binaria=="evento",29250,-750)])

prediccion_202001  <- predict( modelo, 
                                 data.matrix( enero[, campos_buenos, with=FALSE ])                                 )

entrega <-   as.data.table(cbind( "numero_de_cliente"=enero$numero_de_cliente,  "prob" =prediccion_202001) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="./work/lightgbm_new_feature_optimo_entrega_2.csv")
