# Nombre de archivo: randomForest_basico.R
# Función: Crea un random forest con parámetros fijos.

randomForest_basico = function(dataset_train, dataset_test){
  library(randomForest)
  library(ranger)
  
  dataset_train = data.frame(na.roughfix( dataset_train[, setdiff( colnames( dataset_train ) , "clase_binaria" ), with= FALSE] ), dataset_train$clase_binaria)
  names(dataset_train)[length(names(dataset_train))]<-"clase_binaria" 
  dataset_test = data.frame(na.roughfix( dataset_test[, setdiff( colnames( dataset_test ) , "clase_binaria" ), with= FALSE] ), dataset_test$clase_binaria)
  names(dataset_test)[length(names(dataset_test))]<-"clase_binaria" 
  enero = data.frame(na.roughfix( enero[, setdiff( colnames( enero ) , "clase_binaria" ), with= FALSE] ), enero$clase_binaria)
  names(enero)[length(names(enero))]<-"clase_binaria" 
  
  params  <- list( "num.trees"=      500,  #cantidad de arboles
                   "mtry"=             3,  #cantidad de variables que evalua para hacer un split
                   "min.node.size"=    1,  #hoja mas chica
                   "max.depth"=        0   # 0 significa profundidad infinita
  )
  
  modelo  <- ranger( formula= "clase_binaria ~ .",
                     data= dataset_train,  
                     probability=   TRUE,  #para que devuelva las probabilidades
                     num.trees=     params$num.trees,
                     mtry=          params$mtry,
                     min.node.size= params$min.node.size,
                     max.depth=     params$max.depth
  )
}
