# Nombre de archivo: rankear_decimales.R
# Función: Sobre el dataset original, revisa qué columnas son numéricas y no enteras (por lo tanto decimales, por lo tanto asumiremos nominales) y rankea a los usuarios por mes, de menor a mayor. Por ahora sale otro data.table con esa info únicamente (+ cliente y mes) que debe reemplazarse.

rankear_decimales_v2 = function(dataset){
  numericas = unlist(lapply(dataset, is.numeric))  
  no_enteras = !unlist(lapply(dataset, is.integer))  
  rankeables_cols = (no_enteras&numericas)
  rankeables_dataset = dataset[, rankeables_cols, with=FALSE]
  rankeables_dataset$foto_mes = dataset$foto_mes
  rankeables_dataset$numero_de_cliente = dataset$numero_de_cliente
  
  ds_ranked = data.table()
  
  for (periodo in unique(rankeables_dataset$foto_mes)){
    mes = rankeables_dataset[foto_mes == periodo,]
    mes = data.frame(mes[, c("numero_de_cliente", "foto_mes")], lapply(mes, rank, ties.method='min'))
    ds_ranked = rbind(ds_ranked, mes)
    assign(paste("ds_ranked"), ds_ranked, envir = .GlobalEnv)
  }
  
  ds_ranked = ds_ranked[, !c("foto_mes.1", "numero_de_cliente.1"), with=FALSE]
  ds = ds[, !setdiff(names(ds_ranked),c("numero_de_cliente", "foto_mes")), with=FALSE]
  setkey(ds, "numero_de_cliente", "foto_mes")
  setkey(ds_ranked, "numero_de_cliente", "foto_mes")
  ds <- ds[ds_ranked, nomatch=0]
  nuevo_orden <-  c( setdiff( colnames( ds ) , "clase_binaria" ) , "clase_binaria" )
  setcolorder( ds, nuevo_orden )
  remove(ds_ranked)
}
