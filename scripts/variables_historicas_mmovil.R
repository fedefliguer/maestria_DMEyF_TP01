# Nombre de archivo: variables_historicas_mmovil.R
# Función: Sobre el dataset original, genera columnas históricas: mínimo, máximo y tendencia en seis meses. Además crea la media movil para variables fijas.

variables_historicas_mmovil = function(dataset, periodos){
  library(Rcpp)
  cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
  {
    /* Aqui se cargan los valores para la regresion */
    double  x[100] ;
    double  y[100] ;
    int n = pcolumna.size();
    NumericVector out( 4*n );
    for(int i = 0; i < n; i++)
    {
      int  libre    = 0 ;
      int  xvalor   = 1 ;
      for( int j= pdesde[i]-1;  j<=i; j++ )
      {
         double a = pcolumna[j] ;
         if( !R_IsNA( a ) ) 
         {
            y[ libre ]= a ;
            x[ libre ]= xvalor ;
            libre++ ;
         }
         xvalor++ ;
      }
      /* Si hay al menos dos valores */
      if( libre > 1 )
      {
        double  xsum  = x[0] ;
        double  ysum  = y[0] ;
        double  xysum = xsum * ysum ;
        double  xxsum = xsum * xsum ;
        double  vmin  = y[0] ;
        double  vmax  = y[0] ;
        for( int h=1; h<libre; h++)
        { 
          xsum  += x[h] ;
          ysum  += y[h] ; 
          xysum += x[h]*y[h] ;
          xxsum += x[h]*x[h] ;
          if( y[h] < vmin )  vmin = y[h] ;
          if( y[h] > vmax )  vmax = y[h] ;
        }
        out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
        out[ i + n ]    =  vmin ;
        out[ i + 2*n ]  =  vmax ;
        out[ i + 3*n ]  =  ysum / libre ;
      }
      else
      {
        out[ i       ]  =  NA_REAL ; 
        out[ i + n   ]  =  NA_REAL ;
        out[ i + 2*n ]  =  NA_REAL ;
        out[ i + 3*n ]  =  NA_REAL ;
      }
    }
    return  out;
  }')

  setorder( dataset,  numero_de_cliente, foto_mes )
  ventana_regresion  <- periodos
  last <- nrow( dataset )
  kcampo_id_idx  <-  match( "numero_de_cliente", names(dataset) )
  vector_ids   <- dataset[[  kcampo_id_idx  ]]
  vector_desde <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1
  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
  columnas_no_procesar  <- c( "numero_de_cliente", "foto_mes", "clase_binaria" )
  columnas_originales <-  copy(colnames( dataset ))
  columnas_originales_a_procesar  <- setdiff( columnas_originales,  columnas_no_procesar  )  

  for(  campo  in  columnas_originales_a_procesar )
  {
    campo_idx     <-   match( campo,  names(dataset) )
    col_original  <-   dataset[[  campo_idx  ]]
    nueva_col     <- fhistC( col_original, vector_desde ) 
    dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
    dataset[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
    dataset[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
  }

  rollear = c("active_quarter","cproductos","tcuentas","mcuenta_corriente_adicional","mcuenta_corriente","mcaja_ahorro","mcaja_ahorro_adicional","mcaja_ahorro_dolares","mdescubierto_preacordado","mcuentas_saldo","ctarjeta_debito_transacciones","mautoservicio","ctarjeta_visa_transacciones","mtarjeta_visa_consumo","mtarjeta_master_consumo","mpayroll","mpayroll2","ccuenta_debitos_automaticos","mcuenta_debitos_automaticos","ctarjeta_visa_debitos_automaticos","ctarjeta_master_debitos_automaticos","mttarjeta_master_debitos_automaticos","cpagodeservicios","mpagodeservicios","cpagomiscuentas","mpagomiscuentas","ccajeros_propios_descuentos","mcajeros_propios_descuentos","ctarjeta_visa_descuentos","mtarjeta_visa_descuentos","ctarjeta_master_descuentos","mtarjeta_master_descuentos","ccomisiones_mantenimiento","mcomisiones_mantenimiento","ccomisiones_otras","mcomisiones_otras","cforex","cforex_buy","mforex_buy","cforex_sell","mforex_sell","ctransferencias_recibidas","mtransferencias_recibidas","ctransferencias_emitidas","mtransferencias_emitidas","cextraccion_autoservicio","mextraccion_autoservicio","ccheques_depositados","mcheques_depositados","ccheques_emitidos","mcheques_emitidos","ccheques_depositados_rechazados","mcheques_depositados_rechazados","ccheques_emitidos_rechazados","mcheques_emitidos_rechazados","ccallcenter_transacciones","chomebanking_transacciones","ccajas_transacciones","ccajas_consultas","ccajas_depositos","ccajas_extracciones","ccajas_otras","catm_trx","matm","catm_trx_other","matm_other","ctrx_quarter","Master_msaldototal","Master_msaldopesos","Master_msaldodolares","Master_mconsumospesos","Master_mconsumosdolares","Master_mlimitecompra","Master_madelantopesos","Master_madelantodolares","Master_mpagado","Master_mpagospesos","Master_mpagosdolares","Master_fechaalta","Master_mconsumototal","Master_cconsumos","Master_cadelantosefectivo","Visa_msaldototal","Visa_msaldopesos","Visa_msaldodolares","Visa_mconsumospesos","Visa_mconsumosdolares","Visa_mlimitecompra","Visa_madelantopesos","Visa_madelantodolares","Visa_mpagado","Visa_mpagospesos","Visa_mpagosdolares","Visa_fechaalta","Visa_mconsumototal","Visa_cconsumos","Visa_cadelantosefectivo")
  dataset[, paste0(rollear, "__ravg") := frollmean(.SD, periodos),
     .SDcols = rollear,
     by = numero_de_cliente]
  
  nuevo_orden <-  c("numero_de_cliente", "foto_mes", sort(setdiff( colnames( ds ) , c("clase_binaria", "numero_de_cliente", "foto_mes"))), "clase_binaria")
  setcolorder( ds, nuevo_orden )
  }
