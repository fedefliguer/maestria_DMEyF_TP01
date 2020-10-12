# Script parcial

library( "data.table")
library(ggplot2)
library(skimr)
library(devtools)
set.seed(1)

kcampos_separador               <-  "\t"
karchivo_entrada_7meses_zip     <-  "paquete_premium_201906_202001.txt.gz"
ds <- fread(karchivo_entrada_7meses_zip, header=TRUE, sep=kcampos_separador)

source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/nuevas_columnas.R")
nuevas_columnas(ds)

source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/rankear_decimales.R")
rankear_decimales(ds)
