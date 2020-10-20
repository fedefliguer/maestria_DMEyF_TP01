# Nombre de archivo: separacion_conjuntos.R
# Función: Separa en entrenamiento, testeo y enero según inputs

separacion_conjuntos = function(dataset, periodos_train, periodos_test_1, periodos_test_2, proporcion_columnas=1, proporcion_filas=1){
  enero = ds[foto_mes == 202001,] # La doble flecha es para asignar al entorno global
  ds_train = ds[foto_mes %in% periodos_train]
  ds_test_1 = ds[foto_mes %in% periodos_test_1]
  ds_test_2 = ds[foto_mes %in% periodos_test_2]
  
  nu_filas_train = round(nrow(ds_train) * proporcion_filas, 0)
  ds_train = ds_train[sample(.N,nu_filas_train)]
  
  nu_columnas = round(ncol(ds_train) * proporcion_columnas, 0)
  columnas = unique(c("foto_mes", "numero_de_cliente", sample(names(ds_train), nu_columnas), "clase_binaria"))
  
  ds_train <<- ds_train[, columnas, with=FALSE]
  ds_test_1 <<- ds_test_1[, columnas, with=FALSE]
  ds_test_2 <<- ds_test_2[, columnas, with=FALSE]
  enero <<- enero[, columnas, with=FALSE]
}
