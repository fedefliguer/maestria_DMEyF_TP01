library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
library(lightgbm)
library(reshape2)
options(scipen = 999)

set.seed(1)

#limpio la memoria
rm( list=ls() )
gc()

setwd("/home/fjf_arg_gcloud/buckets/b1/datasetsOri/")

kcampos_separador               <-  "\t"
karchivo_entrada_full     <-  "paquete_premium.txt.gz"
ds <- fread(karchivo_entrada_full, header=TRUE, sep=kcampos_separador)
ds = ds[foto_mes>201709 & foto_mes<201912]

# Agrego columnas de FE
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/nuevas_columnas.R")
nuevas_columnas(ds)

# Agrego variables históricas
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/variables_historicas_mmovil.R")
variables_historicas_mmovil(ds, 6, 6) # Cambiar con los períodos de lag y períodos de memoria de la media movil.

setwd("/home/fjf_arg_gcloud/buckets/b1/candidatos/")

campos_buenos  <- setdiff(  colnames(ds) ,  c("clase_binaria","clase01") )

m1 = lgb.load("m1.txt")
periodos_m1 = c(201904, 201905, 201906, 201907, 201908, 201909)

m2 = lgb.load("m2.txt")
periodos_m2 = c(201904, 201905, 201906, 201907, 201908, 201909)

m3 = lgb.load("m3.txt")
periodos_m3 = c(201904, 201905, 201906, 201907, 201908, 201909)

m4 = lgb.load("m4.txt")
periodos_m4 = c(201801, 201807, 201808, 201809, 201810, 201811, 201812)

m5 = lgb.load("m5.txt")
periodos_m5 = c(201801, 201807, 201808, 201809, 201810, 201811, 201812)

m6 = lgb.load("m6.txt")
periodos_m6 = c(201801, 201807, 201808, 201809, 201810, 201811, 201812)

m7 = lgb.load("m7.txt")
periodos_m7 = c(201904, 201905, 201906, 201907, 201908, 201909, 201911)

m8 = lgb.load("m8.txt")
periodos_m8 = c(201904, 201905, 201906, 201907, 201908, 201909, 201911)

m9 = lgb.load("m9.txt")
periodos_m9 = c(201904, 201905, 201906, 201907, 201908, 201909, 201911)

resumido <-   as.data.table(cbind("mes" =ds$foto_mes) )

add_prediction = function(modelo, periodos_no_considerar){
  prediction  <- predict(modelo, data.matrix( ds[, campos_buenos, with=FALSE ]))
  prediction_ajustada  <- prediction * ds[, ifelse( foto_mes %in% periodos_no_considerar, 0, 1)]
  name_prediction = deparse(substitute(modelo))
  resumido[, (name_prediction) := prediction_ajustada]
}

add_prediction(m1, periodos_m1)
add_prediction(m2, periodos_m2)
add_prediction(m3, periodos_m3)
add_prediction(m4, periodos_m4)
add_prediction(m5, periodos_m5)
add_prediction(m6, periodos_m6)
add_prediction(m7, periodos_m7)
add_prediction(m8, periodos_m8)
add_prediction(m9, periodos_m9)

punto_de_corte = 0.05

lista_modelos <- setdiff(names(resumido), "mes")
for(i in lista_modelos){
  resumido[, paste("g_", (i)) := (get(i) > punto_de_corte) * ds[, ifelse( clase_binaria=="evento",29250,-750)]]
  } 

agrupado = resumido[, lapply(.SD, sum, na.rm=TRUE), by=mes ]
agrupado <- melt(agrupado, id.vars = "mes")
agrupado$mes = as.factor(agrupado$mes)

ggplot(agrupado, aes(x = mes, y = value, color = variable)) +
  theme_bw() +
  geom_line() + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
