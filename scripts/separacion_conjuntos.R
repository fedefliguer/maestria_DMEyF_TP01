# Nombre de archivo: separacion_conjuntos.R
# Función: Separa en entrenamiento, testeo y enero según inputs

separacion_conjuntos = function(dataset, periodos_train, periodos_test){
  enero <<- ds[foto_mes == 202001,]
  ds_train <<- ds[foto_mes %in% periodos_train]
  ds_test <<- ds[foto_mes %in% periodos_test] 
}
