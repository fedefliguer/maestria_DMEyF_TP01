# Nombre de archivo: rankear_decimales.R
# Función: Sobre el dataset original, revisa qué columnas son numéricas y no enteras (por lo tanto decimales, por lo tanto asumiremos nominales) y rankea a los usuarios por mes, de menor a mayor. Por ahora sale otro data.table con esa info únicamente (+ cliente y mes) que debe reemplazarse.

rankear_decimales = function(dataset){
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
  
  remove(mes, rankeables_cols, rankeables_dataset, no_enteras, numericas, periodo)
  
  dataset = dataset[, !setdiff(names(ds_ranked),c("numero_de_cliente", "foto_mes")), with=FALSE]
  dataset = dataset[, !c("foto_mes.1", "numero_de_cliente.1"), with=FALSE]
  setkey(dataset, "numero_de_cliente", "foto_mes")
  setkey(ds_ranked, "numero_de_cliente", "foto_mes")
  dataset <- dataset[ds_ranked, nomatch=0]
  nuevo_orden <-  c( setdiff( colnames( dataset ) , "clase_binaria" ) , "clase_binaria" )
  setcolorder( dataset, nuevo_orden )
}
