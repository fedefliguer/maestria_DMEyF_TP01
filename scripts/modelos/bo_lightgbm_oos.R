library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
library("lightgbm")
library("DiceKriging")
library("mlrMBO")
set.seed(1)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# Carga del ds y FE
# setwd("/home/fjf_arg_gcloud/buckets/b1/datasetsOri/")

kcampos_separador               <-  "\t"
karchivo_entrada_full     <-  "paquete_premium.txt.gz"
ds <- fread(karchivo_entrada_full, header=TRUE, sep=kcampos_separador)
# ds = ds[sample(.N,100000)]

# Agrego columnas de FE
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/nuevas_columnas.R")
nuevas_columnas(ds)

myfile <- "https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/otros/importances.csv"
importances <- read.csv2(myfile)

importances_max = data.table(importances[, c("Feature", "Maxima")])
importances_max = importances_max[Maxima>0.005]
importances_max$Feature = gsub("__tend","",importances_max$Feature)
importances_max$Feature = gsub("__max","",importances_max$Feature)
importances_max$Feature = gsub("__min","",importances_max$Feature)
importances_max$Feature = gsub("__ravg","",importances_max$Feature)
variables = unique(importances_max$Feature)

ds = ds[, c("foto_mes", "numero_de_cliente", variables, "clase_binaria"), with=FALSE]

# Agrego variables históricas
periodos = c(6,12)
source_url("https://raw.githubusercontent.com/fedefliguer/maestria_DMEyF_TP01/main/scripts/variables_historicas_mmovil_multiperiod.R")
variables_historicas_fe_multiperiod(ds, periodos)

#LIGHT GBM
ds[ , clase01 :=  ifelse( clase_binaria=="evento", 1L, 0L)  ]
train = c(201904, 201905, 201906, 201907) # Cambiar con el período que querramos entrenar
test = c(201908) 
oos_201909 = ds[foto_mes == 201909]
oos_201910 = ds[foto_mes == 201910]
oos_201911 = ds[foto_mes == 201911]
enero = ds[foto_mes == 202001,] 

campos_buenos  <- setdiff(  colnames(ds) ,  c("clase_binaria","clase01") )

ds[ foto_mes %in% train , BO_train:=1]
ds[ foto_mes %in% test ,  BO_test:= 1]

kexperimento <-  "101"
kbayesiana  <-  paste0("./work/lightgbm_BO_", kexperimento, ".RDATA" )
ksalida     <-  paste0("./work/lightgbm_BO_salida_", kexperimento, ".txt" )
kBO_iter    <-  50  #cantidad de iteraciones de la Optimizacion Bayesiana

fganancia_logistic_lightgbm   <- function(probs, data) 
{
  vlabels <- lightgbm::getinfo(data, "label")
  
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
  set.seed( 2210 )  # para que siempre me de el mismo resultado
  
  modelo <-  lgb.train(data= dBO_train,
                       objective= "binary",
                       eval= fganancia_logistic_lightgbm,
                       valids= list( valid= dBO_test),
                       metric= "custom",
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
  
  oos_201909_p  <- predict(modelo, data.matrix( oos_201909[, campos_buenos, with=FALSE ])                                 )

  oos_201909_g  <- sum((oos_201909_p > 0.025) * oos_201909[, ifelse( clase_binaria=="evento",29250,-750)])

  oos_201910_p  <- predict(modelo, data.matrix( oos_201910[, campos_buenos, with=FALSE ])                                 )
  
  oos_201910_g  <- sum((oos_201909_p > 0.025) * oos_201910[, ifelse( clase_binaria=="evento",29250,-750)])
  
  oos_201911_p  <- predict(modelo, data.matrix( oos_201911[, campos_buenos, with=FALSE ])                                 )
  
  oos_201911_g  <- sum((oos_201909_p > 0.025) * oos_201911[, ifelse( clase_binaria=="evento",29250,-750)])
  
  attr(ganancia ,"extras" ) <- list("pnum_iterations"= modelo$best_iter,
                                    "ganancia_oos_201909" = oos_201909_g,
                                    "ganancia_oos_201910" = oos_201910_g,
                                    "ganancia_oos_201911" = oos_201911_g)
  cat( ganancia, " " )
  
  return( ganancia )
}

#dejo los datos en el formato que necesita LightGBM
dBO_train  <-   lgb.Dataset( data  = data.matrix(  ds[ BO_train==1, campos_buenos, with=FALSE]),
                             label = ds[ BO_train==1, clase01],
                             free_raw_data=TRUE
)

dBO_test   <-   lgb.Dataset( data  = data.matrix(  ds[ BO_test==1, campos_buenos, with=FALSE]),
                             label = ds[ BO_test==1, clase01],
                             free_raw_data=TRUE
)

configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  estimar_lightgbm  #esta funcion se debe construir

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
  run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  
  #retoma el procesamiento en donde lo dejo
  run <- mboContinue( kbayesiana ) 
}

par = as.data.table(run$opt.path)
