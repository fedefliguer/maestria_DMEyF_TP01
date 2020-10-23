library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
set.seed(1)

setwd("/home/fjf_arg_gcloud/buckets/b1/datasetsOri/")

rm( list=ls() )
gc()

kcampos_separador               <-  "\t"
karchivo_entrada_full     <-  "paquete_premium.txt.gz"
ds <- fread(karchivo_entrada_full, header=TRUE, sep=kcampos_separador)

# Agrego columnas de FE
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/nuevas_columnas.R")
nuevas_columnas(ds)

# Agrego variables históricas
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/variables_historicas_mmovil.R")
variables_historicas_mmovil(ds, 6, 6) # Cambiar con los períodos de lag y períodos de memoria de la media movil.

# Separo en muestras
train = c(201909) # Cambiar con el período que querramos entrenar
test = c(201911) # Cambiar con el período que querramos testear
pc_columnas = 1 # Con esto corre local. Si queremos usar todas las columnas igualarlo a 1.
pc_filas_train = 1 # Con esto corre local. Si queremos usar todas las filas igualarlo a 1.
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/separacion_conjuntos.R")
separacion_conjuntos(ds, train, test, pc_columnas, pc_filas_train)

ds_train[ , clase01 :=  ifelse( clase_binaria=="evento", 1L, 0L)  ]
ds_test[ , clase01 :=  ifelse( clase_binaria=="evento", 1L, 0L)  ]

kexperimento <-  "31"   #cambiar esto en cada corrida !

kbayesiana  <-  paste0("./work/lightgbm_BO_10meses_historia_", kexperimento, ".RDATA" )
ksalida     <-  paste0("./work/lightgbm_BO_10meses_historia_salida_", kexperimento, ".txt" )
kBO_iter    <-  3  #cantidad de iteraciones de la Optimizacion Bayesiana

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

estimar_lightgbm <- function( x )
{
  set.seed( 102191 )
  
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

campos_buenos  <- setdiff(  colnames(backup) ,  c("clase_binaria","clase01") )

dBO_train  <-   lgb.Dataset( data  = data.matrix(  ds_train[, campos_buenos, with=FALSE]),
                             label = ds_train[, clase01],
                             free_raw_data=TRUE
)

dBO_test   <-   lgb.Dataset( data  = data.matrix(  ds_test[, campos_buenos, with=FALSE]),
                             label = ds_test[, clase01],
                             free_raw_data=TRUE
)

rm( ds )

configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  estimar_lightgbm  #esta funcion se debe construir

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
