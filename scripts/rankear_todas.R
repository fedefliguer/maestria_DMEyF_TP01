# Nombre de archivo: rankear_todas.R
# Función: Sobre el dataset original, rankea todas las columnas.

rankear_todas = function(dataset){
  
  meses = unique(dataset$foto_mes)
  ds_ranked = data.table()
  
  for (mes in meses) {
    ds_mes = dataset[foto_mes==mes,]
    ranked_mes = data.table("numero_de_cliente" = ds_mes$numero_de_cliente, "foto_mes" = ds_mes$foto_mes, lapply(ds_mes[, 3:(ncol(ds_mes)-1)], rank, ties.method='min'), "clase_binaria" = ds_mes$clase_binaria)
    ds_ranked = rbind(ds_ranked, ds_mes)
  }
  
  assign(deparse(substitute(dataset)),ds_ranked,envir=.GlobalEnv)
}
