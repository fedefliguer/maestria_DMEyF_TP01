# Script parcial

``` r
library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
library(rpart)
set.seed(1)

#limpio la memoria
rm( list=ls() )
gc()

kcampos_separador               <-  "\t"
karchivo_entrada_7meses_zip     <-  "paquete_premium_201906_202001.txt.gz"
ds <- fread(karchivo_entrada_7meses_zip, header=TRUE, sep=kcampos_separador)
ds_paso0_original = ds # Estos pasos los agrego para no tener que volver a cargar la base si quiero corregir algún paso intermedio.

#karchivo_entrada_full     <-  "paquete_premium.txt.gz"
#ds_full <- fread(karchivo_entrada_full, header=TRUE, sep=kcampos_separador)

source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/nuevas_columnas.R")
nuevas_columnas(ds)

ds_paso1_columnas = ds

source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/rankear_decimales.R")
rankear_decimales(ds)

ds_paso2_rankeada = ds

# Las siguientes ocho líneas no pude hacer que entren adentro de alguna función, hay que corregirlo.
ds_ranked = ds_ranked[, !c("foto_mes.1", "numero_de_cliente.1"), with=FALSE]
ds = ds[, !setdiff(names(ds_ranked),c("numero_de_cliente", "foto_mes")), with=FALSE]
setkey(ds, "numero_de_cliente", "foto_mes")
setkey(ds_ranked, "numero_de_cliente", "foto_mes")
ds <- ds[ds_ranked, nomatch=0]
nuevo_orden <-  c( setdiff( colnames( ds ) , "clase_binaria" ) , "clase_binaria" )
setcolorder( ds, nuevo_orden )
remove(ds_ranked)

source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/variables_historicas.R")
variables_historicas(ds, 6) # Cambiar con los períodos de lag

train = c(201909, 201910) # Cambiar con el período que querramos entrenar
test = c(201911) # Cambiar con el período que querramos testear
pc_columnas = 0.03 # Con esto corre local. Si queremos usar todas las columnas igualarlo a 1 o no asignarlo.
pc_filas_train = 0.5 # Con esto corre local. Si queremos usar todas las filas igualarlo a 1 o no asignarlo.

source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/separacion_conjuntos.R")
separacion_conjuntos(ds, train, test, pc_columnas)

remove(ds_paso0_original, ds_paso1_columnas, ds_paso2_rankeada)
backup_train = ds_train 
backup_test = ds_test
backup_enero = enero 

# A partir de acá se elige el modelo/optimización que se va a correr
max_depths = c(3,4,5)
min_splits = c(5,10,15)
min_buckets = c(5,10,15)
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/modelos/rpart_gridsearch.R")
rpart_gridsearch(ds_train, ds_test)

# Random forest básico

source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/modelos/randomForest_basico.R")
randomForest_basico(ds_train, ds_test)

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
```

