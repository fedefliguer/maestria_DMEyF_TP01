# Nombre de archivo: rankear_todas.R
# Función: Sobre el dataset original, escala todas las columnas.

escalar_todas = function(dataset){
  
  meses = unique(dataset$foto_mes)
  ds_scaled = data.table()
  
  for (mes in meses) {
    ds_mes = dataset[foto_mes==mes,]
    scaled_mes = data.table("numero_de_cliente" = ds_mes$numero_de_cliente, "foto_mes" = ds_mes$foto_mes, scale(ds_mes[, 3:(ncol(ds_mes)-1)]), "clase_binaria" = ds_mes$clase_binaria)
    ds_scaled = rbind(ds_scaled, scaled_mes)
  }
  
  assign(deparse(substitute(dataset)),ds_scaled,envir=.GlobalEnv)
}
