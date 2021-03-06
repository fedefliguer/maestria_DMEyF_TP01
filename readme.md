# Script parcial

``` r
library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
set.seed(1)

#limpio la memoria
rm( list=ls() )
gc()

#setwd("/home/fjf_arg_gcloud/buckets/b1/datasetsOri/")

kcampos_separador               <-  "\t"
# karchivo_entrada_7meses_zip     <-  "paquete_premium_201906_202001.txt.gz"
# ds <- fread(karchivo_entrada_7meses_zip, header=TRUE, sep=kcampos_separador)
karchivo_entrada_full     <-  "paquete_premium.txt.gz"
ds <- fread(karchivo_entrada_full, header=TRUE, sep=kcampos_separador)

# Agrego columnas de FE
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/nuevas_columnas.R")
nuevas_columnas(ds)
# ds = ds[sample(.N,100000)] # Local. 100.000 rows para hacer la prueba local.

# source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/rankear_decimales.R")
# rankear_decimales(ds)
# 
# # Las siguientes ocho líneas no pude hacer que entren adentro de alguna función, hay que corregirlo.
# ds_ranked = ds_ranked[, !c("foto_mes.1", "numero_de_cliente.1"), with=FALSE]
# ds = ds[, !setdiff(names(ds_ranked),c("numero_de_cliente", "foto_mes")), with=FALSE]
# setkey(ds, "numero_de_cliente", "foto_mes")
# setkey(ds_ranked, "numero_de_cliente", "foto_mes")
# ds <- ds[ds_ranked, nomatch=0]
# nuevo_orden <-  c( setdiff( colnames( ds ) , "clase_binaria" ) , "clase_binaria" )
# setcolorder( ds, nuevo_orden )
# remove(ds_ranked)
# source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/variables_historicas.R")
# variables_historicas(ds, 6) # Cambiar con los períodos de lag

# Agrego variables históricas
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/variables_historicas_mmovil.R")
variables_historicas_mmovil(ds, 6, 6) # Cambiar con los períodos de lag y períodos de memoria de la media movil.

# Separo en muestras
train = c(201909) # Cambiar con el período que querramos entrenar
test = c(201911) # Cambiar con el período que querramos testear
pc_columnas = 0.2 # Con esto corre local. Si queremos usar todas las columnas igualarlo a 1.
pc_filas_train = 0.2 # Con esto corre local. Si queremos usar todas las filas igualarlo a 1.
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/separacion_conjuntos.R")
separacion_conjuntos(ds, train, test, pc_columnas, pc_filas_train)

# Elegir qué modelo correr

# Árbol con grid search
# max_depths = c( 13, 12, 11, 10, 9)
# min_splits = c( 9, 10, 11, 12, 13)
# min_buckets = c( 9, 10, 11, 12, 13)
# source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/modelos/rpart_gridsearch.R")
# rpart_gridsearch(ds_train, ds_test)

# Random forest básico
# source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/modelos/randomForest_basico.R")
# randomForest_basico(ds_train, ds_test)

# Random forest con optimización bayesiana
# Para correr local hay que crear una carpeta que se llame work en el directorio, para correr en la nube ?
# num_trees_BO = c(1, 999) # En dropbox de 1 a 999
# pmtry_BO = c(2, 40) # En dropbox de 2 a 40
# pmin.node.size_BO = c(1, 40) # En dropbox de 1 a 40
# pmax.depth_BO = c(0, 40) # En dropbox de 0 a 20
# numero_iteraciones = 2 # En dropbox es 100
# numero_experimento = 11 # Cada vez que hacemos una nueva, agregar un nro más

#source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/modelos/bo_ranger.R")
#bo_ranger(ds_train, ds_test, numero_experimento, numero_iteraciones)

#LIGHTGBM

# Genero archivo de entrega

if(class(modelo)=="rpart"){
  prediction_enero  <- predict( modelo, enero, type = "prob")
  
  entrega <-   as.data.table(cbind( "numero_de_cliente" = enero[, numero_de_cliente],
                                    "prob" = prediction_enero[, "evento"])
  )
  
  entrega[  ,  estimulo :=  as.integer( prob > 0.025)]
  entrega = entrega[, c("numero_de_cliente", "estimulo")]
}

if(class(modelo)=="ranger"){
  prediction_enero  <- predict( modelo, enero)
  
  entrega <-   as.data.table(cbind( "numero_de_cliente" = enero[, numero_de_cliente],
                                    "prob" = prediction_enero$predictions[, "evento"])
  )
  
  entrega[  ,  estimulo :=  as.integer( prob > 0.025)]
  entrega = entrega[, c("numero_de_cliente", "estimulo")]
}

nombre_archivo = ""
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file=nombre_archivo)
```

