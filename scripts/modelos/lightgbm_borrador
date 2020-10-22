# Líneas a cambiar:
# 21: poner dir del dataset a usar
# 63: poner dir del bucket b1
# 66: nro de experimento
# 69: cant de iteraciones
# 156: configurar BO

library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
require("lightgbm")
require("DiceKriging")
require("mlrMBO")
set.seed(1)

#limpio la memoria
rm( list=ls() )
gc()

#setwd("/home/fjf_arg_gcloud/buckets/b1/datasetsOri/")
setwd("/home/antonellaalopez/buckets/b1/datasets/")

kcampos_separador               <-  "\t"
karchivo_entrada_7meses_zip     <-  "paquete_premium_201906_202001.txt.gz"
ds <- fread(karchivo_entrada_7meses_zip, header=TRUE, sep=kcampos_separador)
#karchivo_entrada_full     <-  "paquete_premium.txt.gz"
#ds <- fread(karchivo_entrada_full, header=TRUE, sep=kcampos_separador)

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
pc_columnas = 0.1 # Con esto corre local. Si queremos usar todas las columnas igualarlo a 1.
pc_filas_train = 0.01 # Con esto corre local. Si queremos usar todas las filas igualarlo a 1.
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/separacion_conjuntos.R")
separacion_conjuntos(ds, train, test, pc_columnas, pc_filas_train)

#LIGHT GBM
setwd("/home/antonellaalopez/buckets/b1")

#en estos archivos queda el resultado
kexperimento <-  "01"   #cambiar esto en cada corrida !
kbayesiana  <-  paste0("./work/lightgbm_BO_10meses_historia_", kexperimento, ".RDATA" )
ksalida     <-  paste0("./work/lightgbm_BO_10meses_historia_salida_", kexperimento, ".txt" )
kBO_iter    <-  2  #cantidad de iteraciones de la Optimizacion Bayesiana

#paso la clase a binaria que tome valores {0,1}  enteros
ds_train[ , clase01 :=  ifelse( clase_binaria=="BAJA+2", 1L, 0L)  ]
ds_test[ , clase01 :=  ifelse( clase_binaria=="BAJA+2", 1L, 0L)  ]
enero[ , clase01 :=  ifelse( clase_binaria=="BAJA+2", 1L, 0L)  ]

#los campos que se van a utilizar
campos_buenos  <- setdiff(  colnames(ds_train) ,  c("clase_binaria","clase01") )

#dejo los datos en el formato que necesita LightGBM
dBO_train  <-   lgb.Dataset( data  = data.matrix(  ds_train[ ,campos_buenos, with=FALSE]),
                             label = ds_train[ ,clase01],
                             free_raw_data=TRUE
)

dBO_test   <-   lgb.Dataset( data  = data.matrix(  ds_test[ ,campos_buenos, with=FALSE]),
                             label = ds_test[ ,clase01],
                             free_raw_data=TRUE
)


###


#------------------------------------------------------------------------------
#esta es la funcion de ganancia, que se busca optimizar
#se usa internamente a LightGBM
fganancia_logistic_lightgbm   <- function(probs, data) 
{
  vlabels <- getinfo(data, "label")
  
  gan <-sum(   (probs > 0.025  ) * 
                 ifelse( vlabels== 1, +29250, -750 )   
  )
  
  return(  list( name = "ganancia", 
                 value =  ifelse(  is.na(gan) , 0, gan) ,
                 higher_better= TRUE 
  )
  )
}


#------------------------------------------------------------------------------
#funcion que va a optimizar la Bayesian Optimization

estimar_lightgbm <- function( x )
{
  set.seed( 102191 )  # para que siempre me de el mismo resultado
  
  modelo <-  lgb.train(data= dBO_train,
                       objective= "binary",  #la clase es binaria
                       eval= fganancia_logistic_lightgbm,  #esta es la fuciona optimizar
                       valids= list( valid= dBO_test),
                       metric= "custom",  #ATENCION   tremendamente importante
                       boost_from_average= TRUE,
                       num_iterations=  10,  #un numero muy grande
                       early_stopping_rounds= as.integer(50 + 5/x$plearning_rate),
                       learning_rate= x$plearning_rate,
                       feature_fraction= x$pfeature_fraction,
                       min_gain_to_split=  x$pmin_gain_to_split,
                       num_leaves=  x$pnum_leaves,
                       lambda_l1= x$plambda_l1,
                       lambda_l2= x$plambda_l2,
                       max_bin= 31,
                       verbosity= -1,
                       verbose= -1
  )
  
  nrounds_optimo <- modelo$best_iter
  ganancia       <- unlist(modelo$record_evals$valid$ganancia$eval)[ nrounds_optimo ] 
  
  attr(ganancia ,"extras" ) <- list("pnum_iterations"= modelo$best_iter)  #esta es la forma de devolver un parametro extra
  
  cat( ganancia, " " )
  
  return( ganancia )
}
#------------------------------------------------------------------------------

#Aqui comienza la configuracion de la Bayesian Optimization

configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  estimar_lightgbm  #esta funcion se debe construir

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  name = "OptimBayesiana",  #un nombre que no tiene importancia
  fn   = funcion_optimizar,  #aqui va la funcion que quiero optimizar
  minimize= FALSE,  #quiero maximizar la ganancia 
  par.set = makeParamSet(
    makeIntegerParam("pnum_leaves",       lower=  8L   , upper= 1023L),
    makeNumericParam("pfeature_fraction", lower=  0.10 , upper=    1.0),
    makeNumericParam("pmin_gain_to_split",lower=  0.0  , upper=   20),
    makeNumericParam("plearning_rate",    lower=  0.01 , upper=    0.1),
    makeNumericParam("plambda_l1",        lower=  0.0  , upper=   10),
    makeNumericParam("plambda_l2",        lower=  0.0  , upper=  100)
  ),
  has.simple.signature = FALSE,  #porque le pase los paratros con makeParamSet
  noisy= TRUE
)

ctrl  <-  makeMBOControl( save.on.disk.at.time = 600,  save.file.path = kbayesiana )
ctrl  <-  setMBOControlTermination(ctrl, iters = kBO_iter )
ctrl  <-  setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control = list(trace = FALSE))



if(!file.exists(kbayesiana))
{
  #lanzo la busqueda bayesiana
  run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  
  #retoma el procesamiento en donde lo dejo
  run <- mboContinue( kbayesiana ) 
}


#obtengo el pnrounds de la mejor corrida
tbl <- as.data.table(run$opt.path)
setorder( tbl, -y)
mejor_pnrounds <- tbl[ 1, pnum_iterations]



cat( "ganancia",          run$y, "\n",
     "nrounds",           mejor_pnrounds, "\n",
     "num_leaves",        run$x$pnum_leaves, "\n",
     "feature_fraction",  run$x$pfeature_fraction, "\n",
     "min_gain_to_split", run$x$pmin_gain_to_split, "\n",
     "learning_rate",     run$x$plearning_rate, "\n",
     "lambda_l1",         run$x$plambda_l1, "\n",
     "lambda_l2",         run$x$plambda_l2, "\n",
     file=ksalida, 
     sep="\t", 
     append=TRUE )

###

# Mejores parámetros

#dejo los datos en el formato que necesita LightGBM
dgeneracion  <- lgb.Dataset( data= data.matrix(  ds_train[ , campos_buenos, with=FALSE]),
                             label= ds_train$clase01,
                             free_raw_data= FALSE )

#genero el modelo - cambiar valores
modelo  <- lgb.train( data= dgeneracion,
                      objective= "binary",
                      boost_from_average= TRUE,
                      max_bin= 31,
                      num_iterations=    mejor_pnrounds,
                      learning_rate=     run$x$plearning_rate,
                      feature_fraction=  run$x$pfeature_fraction,
                      min_gain_to_split= run$x$pmin_gain_to_split,
                      num_leaves=        run$x$pnum_leaves,
                      lambda_l1=         run$x$plambda_l1,
                      lambda_l2=         run$x$plambda_l2
)

dapply <-  enero

#aplico el modelo a los datos sin clase, 202001
prediccion_202001  <- predict( modelo, 
                               data.matrix( dapply[, campos_buenos, with=FALSE ]))

#genero el dataset de entrega
entrega <-   as.data.table(cbind( "numero_de_cliente"=dapply$numero_de_cliente,  "prob" =prediccion_202001) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="./work/lightgbm_entrega_1.csv")
