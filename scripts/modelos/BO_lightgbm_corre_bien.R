# ESTO ASÍ COMO ESTÁ CORRE BIEN


#Optimizacion Bayesiana de hiperparametros
#algoritmo LightGBM
#libreria   lightgbm


library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
require("lightgbm")
require("DiceKriging")
require("mlrMBO")
set.seed(1)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# Carga del ds y FE
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

# Agrego variables históricas
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/variables_historicas_mmovil.R")
variables_historicas_mmovil(ds, 6, 6) # Cambiar con los períodos de lag y períodos de memoria de la media movil.

#LIGHT GBM
ds[ , clase01 :=  ifelse( clase_binaria=="evento", 1L, 0L)  ]


train = c(201909) # Cambiar con el período que querramos entrenar
test = c(201911) 

# Esto es para entrenar el modelo a lo último
enero = ds[foto_mes == 202001,] 
ds_train = ds[foto_mes %in% train]


kexperimento <-  "100"   #cambiar esto en cada corrida !

#en estos archivos queda el resultado
kbayesiana  <-  paste0("./work/lightgbm_BO_", kexperimento, ".RDATA" )
ksalida     <-  paste0("./work/lightgbm_BO_salida_", kexperimento, ".txt" )
kBO_iter    <-  10  #cantidad de iteraciones de la Optimizacion Bayesiana


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
  set.seed( 2210 )  # para que siempre me de el mismo resultado
  
  modelo <-  lgb.train(data= dBO_train,
                       objective= "binary",  #la clase es binaria
                       eval= fganancia_logistic_lightgbm,  #esta es la fuciona optimizar
                       valids= list( valid= dBO_test),
                       metric= "custom",  #ATENCION   tremendamente importante
                       boost_from_average= TRUE,
                       num_iterations=  999999,  #un numero muy grande
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

#Aqui comienza el programa
setwd("/home/antonellaalopez/buckets/b1")


#paso la clase a binaria que tome valores {0,1}  enteros
#ds[ , clase01 :=  ifelse( clase_binaria=="evento", 1L, 0L)  ]

#los campos que se van a utilizar
campos_buenos  <- setdiff(  colnames(ds) ,  c("clase_binaria","clase01") )


ds[ foto_mes %in% train , BO_train:=1]
ds[ foto_mes %in% test ,  BO_test:= 1]

#dejo los datos en el formato que necesita LightGBM
dBO_train  <-   lgb.Dataset( data  = data.matrix(  ds[ BO_train==1, campos_buenos, with=FALSE]),
                             label = ds[ BO_train==1, clase01],
                             free_raw_data=TRUE
)

dBO_test   <-   lgb.Dataset( data  = data.matrix(  ds[ BO_test==1, campos_buenos, with=FALSE]),
                             label = ds[ BO_test==1, clase01],
                             free_raw_data=TRUE
)

#borro el dataset, ya tengo la copia en formato lightgbm
rm( ds )
gc()


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
    makeNumericParam("pmin_gain_to_split",lower=  0L  , upper=   20L),
    makeNumericParam("plearning_rate",    lower=  0.01 , upper=    0.1),
    makeNumericParam("plambda_l1",        lower=  0L  , upper=   10L),
    makeNumericParam("plambda_l2",        lower=  0L  , upper=  100L)
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


## ENTRENAMIENTO DEL MODELO CON LOS MEJORES PARÁMETROS

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
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="./work/lightgbm_entrega_test_2.csv")
