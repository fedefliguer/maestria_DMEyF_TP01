# Nombre de archivo: rpart_gridsearch.R
# Función: Corre un árbol de decision iterando sobre un vector de max_dephts, min_splits y min_buckets.

rpart_gridsearch = function(dataset_train, dataset_test){
  mejor_params <- list(  "ganancia"= -1,
                         "maxdepth"= -1,
                         "minsplit"= -1,
                         "minbucket"= -1)
  
  i = 1
  
  for( vmaxdepth  in max_depths)    
  {
    for(vminsplit in  min_splits)
    {
      for(vminbucket in  min_buckets)
      {
        cantidad = length(max_depths)*length(min_splits)*length(min_buckets)
        
        modelo   <-  rpart("clase_binaria ~ .",  
                           data = dataset_train[, !c("foto_mes", "numero_de_cliente"), with=FALSE],
                           model= TRUE,
                           xval=0,
                           cp= 0,
                           maxdepth=  vmaxdepth,
                           minsplit=  vminsplit,
                           minbucket=  vminbucket
        )
        
        prediction_test  <- predict( modelo, dataset_test[, !c("foto_mes", "numero_de_cliente"), with=FALSE], type = "prob")
        
        entrega <-   as.data.table(cbind( "numero_de_cliente" = dataset_test[, numero_de_cliente],
                                          "prob" =prediction_test[, "evento"],
                                          "clase_binaria" = dataset_test[, clase_binaria])
        )
        
        entrega[  ,  estimulo :=  as.integer( prob > 0.025)]
        
        ganancia <- sum(  entrega[ estimulo==1, ifelse( clase_binaria=="evento", 29250, -750) ])
        
        print(paste("Corrida", i, "de", cantidad, ", con parámetro maxdepth = ", vmaxdepth, ", parámetro minsplit = ", vminsplit, " y parámetro minbucket = ", vminbucket, " y resultado ganancia = ", ganancia))
        
        i = i + 1
        if( ganancia > mejor_params$ganancia )
        {
          mejor_params$ganancia  <- ganancia
          mejor_params$maxdepth  <- vmaxdepth
          mejor_params$minsplit  <- vminsplit
          mejor_params$minbucket <- vminbucket
        }
      }
    }
  }
  
  ganancia_test_rpart_gridsearch <<- mejor_params$ganancia
  
  modelo  <<-  rpart ("clase_binaria ~ .",  
                      data = dataset_train[, !c("foto_mes", "numero_de_cliente"), with=FALSE], 
                      model= TRUE,
                      xval=0,
                      cp= 0,
                      maxdepth=   mejor_params$maxdepth,
                      minsplit=   mejor_params$minsplit,
                      minbucket=  mejor_params$minbucket)
}
