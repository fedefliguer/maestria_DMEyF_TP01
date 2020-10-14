# Nombre de archivo: separacion_conjuntos.R
# Función: Separa en entrenamiento, testeo y enero según inputs

separacion_conjuntos = function(dataset, periodos_train, periodos_test, proporcion_columnas){
  enero = ds[foto_mes == 202001,] # La doble flecha es para asignar al entorno global
  ds_train = ds[foto_mes %in% periodos_train]
  ds_test = ds[foto_mes %in% periodos_test]
  nu_columnas = round(ncol(ds_train) * proporcion_columnas, 0)
  columnas = unique(c("foto_mes", "numero_de_cliente", sample(names(ds_train), nu_columnas), "clase_binaria"))
  ds_train <<- ds_train[, columnas, with=FALSE]
  ds_test <<- ds_test[, columnas, with=FALSE]
  enero <<- enero[, columnas, with=FALSE]
}
