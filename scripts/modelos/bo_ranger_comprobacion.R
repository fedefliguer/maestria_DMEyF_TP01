bo_ranger_comprobacion = function(dataset_train, dataset_test, nro_experimento, nro_iteraciones){
  library("data.table")
  library("ranger")
  library("randomForest")
  library("DiceKriging")
  library("mlrMBO")
  
  dataset_train[  , clase_binaria := as.factor(ifelse( clase_binaria=="evento", "evento", "no_evento" )) ]
  dataset_test[  , clase_binaria := as.factor(ifelse( clase_binaria=="evento", "evento", "no_evento" )) ]
  enero[  , clase_binaria := as.factor(ifelse( clase_binaria=="evento", "evento", "no_evento" )) ]
  
  dataset_train  <-  na.roughfix( dataset_train )
  dataset_test  <-  na.roughfix( dataset_test )
  enero  <-  na.roughfix( enero )
  
  kexperimento <-  toString(nro_experimento) 
  
  kbayesiana  <-  paste0(".//comprobacion//ranger_BO_4meses_", kexperimento, ".RDATA" )
  klog        <-  paste0(".//comprobacion//ranger_BO_4meses_", kexperimento, ".txt" )
  ksalida     <-  paste0(".//comprobacion//ranger_BO_4meses_salida_", kexperimento, ".txt" )
  ksubmit     <-  paste0(".//comprobacion//ranger_BO_4meses_submit_", kexperimento, ".csv" )
  kBO_iter    <-  nro_iteraciones
  
  i = 1
  
  estimar_ranger <- function( x )
  {
    modelo  <- ranger( formula= "clase_binaria ~ .",
                       data= dataset_train,
                       probability=   TRUE,  
                       num.trees=     x$pnum.trees,
                       mtry=          x$pmtry,
                       min.node.size= x$pmin.node.size,
                       max.depth=     x$pmax.depth
    )
    
    saveRDS(modelo, paste0(".//comprobacion//", x$pnum.trees, "_", x$pmtry, "_", x$pmin.node.size, "_", x$pmax.depth, "_", "modelo_", i, ".rds"))
    
    prediccion_test  <- predict( modelo, dataset_test )
    
    test_ganancia  <- sum( (prediccion_test$predictions[ , "evento"] > 0.025) * 
                             dataset_test[, ifelse( clase_binaria=="evento",29250,-750)])
    
    test_predichos = sum(fifelse(prediccion_test$predictions[, "evento"] > 0.025, 1, 0))
    
    test_predichos_ok = (test_ganancia + 750 * test_predichos)/30000
    
    test_predichos_mal = test_predichos - test_predichos_ok    
    
    test <-   as.data.table(cbind( "numero_de_cliente" = dataset_test[, numero_de_cliente],
                                   "prob" = prediccion_test$predictions[, "evento"],
                                   "clase_binaria" = dataset_test[, clase_binaria])
    )
    test[  ,  estimulo :=  as.integer( prob > 0.025)]
    fwrite( test, sep=",",  file=paste0(".//comprobacion//", x$pnum.trees, "_", x$pmtry, "_", x$pmin.node.size, "_", x$pmax.depth, "_", "test_", i, ".csv"))
    
    prediction_enero  <- predict( modelo, enero)
    entrega <-   as.data.table(cbind( "numero_de_cliente" = enero[, numero_de_cliente],
                                      "prob" = prediction_enero$predictions[, "evento"])
    )
    entrega[  ,  estimulo :=  as.integer( prob > 0.025)]
    entrega = entrega[, c("numero_de_cliente", "estimulo")]
    fwrite( entrega, sep=",",  file=paste0(".//comprobacion//", x$pnum.trees, "_", x$pmtry, "_", x$pmin.node.size, "_", x$pmax.depth, "_", "enero_", i, ".csv"))
    
    #imprimo los resultados al archivo klog
    cat( file= klog, 
         append= TRUE,
         sep="",
         format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
         x$pnum.trees, "\t",
         x$pmtry, "\t",
         x$pmin.node.size, "\t",
         x$pmax.depth, "\t",
         test_predichos, "\t",
         test_predichos_ok, "\t",
         test_predichos_mal, "\t",
         test_ganancia, "\n" )
    return( test_ganancia )
  }
  
  if(!file.exists( klog ) )
  {
    cat( file= klog, 
         append= FALSE,
         sep="",
         "fecha", "\t",
         "num.trees", "\t",
         "mtry", "\t", 
         "min.node.size", "\t",
         "max.depth", "\t",
         "test_predichos", "\t",
         "test_predichos_ok", "\t",
         "test_predichos_mal", "\t",
         "test_ganancia", "\n")
  }
  
  configureMlr(show.learner.output = FALSE)
  funcion_optimizar <-  estimar_ranger
  
  obj.fun  <- makeSingleObjectiveFunction(
    fn   = funcion_optimizar,
    minimize= FALSE,   #estoy Maximizando la ganancia
    noisy=    TRUE,
    par.set = makeParamSet(
      makeIntegerParam("pnum.trees",     lower=num_trees_BO[1], upper= num_trees_BO[2]),
      makeIntegerParam("pmtry",          lower=pmtry_BO[1], upper= pmtry_BO[2]),
      makeIntegerParam("pmin.node.size", lower=pmin.node.size_BO[1], upper= pmin.node.size_BO[2]),
      makeIntegerParam("pmax.depth",     lower=pmax.depth_BO[1], upper= pmax.depth_BO[2])),
    has.simple.signature = FALSE
  )
  
  ctrl  <-  makeMBOControl( save.on.disk.at.time = 600,  save.file.path = kbayesiana )
  ctrl  <-  setMBOControlTermination(ctrl, iters = kBO_iter )
  ctrl  <-  setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  
  surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control = list(trace = FALSE))
  
  if(!file.exists(kbayesiana))
  {
    run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
  } else {
    run <- mboContinue( kbayesiana ) 
  }
  
  modelo   <<- ranger( formula= "clase_binaria ~ .",
                       data= dataset_train,
                       probability=   TRUE,  #para que devuelva las probabilidades
                       num.trees=     run$x$pnum.trees,
                       mtry=          run$x$pmtry,
                       min.node.size= run$x$pmin.node.size,
                       max.depth=     run$x$pmax.depth
  )
  
  enero <<- enero
}
