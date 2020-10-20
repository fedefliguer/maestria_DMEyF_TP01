library( "data.table")
library(ggplot2)
library(devtools)
library(Rcpp)
library(randomForest)
set.seed(1)

#limpio la memoria
rm( list=ls() )
gc()

setwd("/home/fjf_arg_gcloud/buckets/b1/datasetsOri/")

kcampos_separador               <-  "\t"
karchivo_entrada_full     <-  "paquete_premium.txt.gz"
ds <- fread(karchivo_entrada_full, header=TRUE, sep=kcampos_separador)

ds[  , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
ds[  , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito
ds  <-  na.roughfix( ds )

ds = ds[foto_mes>201811]

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

columnas_originales <-  copy(colnames( ds ))

ds[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
ds[ , mv_status02       := Master_status +  Visa_status ]
ds[ , mv_status03       := pmax( ifelse( is.na(Master_status), 9, Master_status) , ifelse( is.na(Visa_status), 9, Visa_status) ) ]
ds[ , mv_status04       := ifelse( is.na(Master_status), 9, Master_status)  +  ifelse( is.na(Visa_status), 9, Visa_status)  ]
ds[ , mv_status05       := ifelse( is.na(Master_status), 9, Master_status)  +  10*ifelse( is.na(Visa_status), 9, Visa_status)  ]

ds[ , mv_status06       := ifelse( is.na(Visa_status), 
                                   ifelse( is.na(Master_status), 9, Master_status), 
                                   Visa_status)  ]

ds[ , mv_status07       := ifelse( is.na(Master_status), 
                                   ifelse( is.na(Visa_status), 9, Visa_status), 
                                   Master_status)  ]

ds[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

ds[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
ds[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
ds[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
ds[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
ds[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
ds[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
ds[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
ds[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
ds[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
ds[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
ds[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
ds[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
ds[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
ds[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
ds[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
ds[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
ds[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
ds[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
ds[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
ds[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
ds[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
ds[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
ds[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
ds[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
ds[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
ds[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
ds[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
ds[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
ds[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
ds[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
ds[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
ds[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
ds[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
ds[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
ds[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

ds  <-  na.roughfix( ds )

columnas_extendidas <-  copy( setdiff(  colnames(ds), columnas_originales ) )

#ordeno por  numero_de_cliente y foto_mes
setorder( ds,  numero_de_cliente, foto_mes )

#Esta es la cantidad de meses que utilizo para la historia
ventana_regresion  <- 6

last <- nrow( ds )
kcampo_id_idx  <-  match( "numero_de_cliente", names(ds) )

vector_ids   <- ds[[  kcampo_id_idx  ]]

vector_desde <- seq( -ventana_regresion+2,  nrow(ds)-ventana_regresion+1 )
vector_desde[ 1:ventana_regresion ]  <-  1

for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }

columnas_no_procesar  <- c( "numero_de_cliente", "foto_mes", "clase_binaria" )
columnas_originales_a_procesar  <- setdiff( columnas_originales,  columnas_no_procesar  )  

for(  campo  in  columnas_originales_a_procesar )
{
  campo_idx     <-   match( campo,  names(ds) )
  col_original  <-   ds[[  campo_idx  ]]
  
  nueva_col     <- fhistC( col_original, vector_desde ) 
  
  ds[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
  ds[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
  ds[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
}
nuevo_orden <-  c( setdiff( colnames( ds ) , "clase_binaria" ) , "clase_binaria" )
setcolorder( ds, nuevo_orden )

columnas_extendidas_a_procesar  <- setdiff( columnas_extendidas,  columnas_no_procesar  )  

for(  campo  in  columnas_extendidas_a_procesar )
{
  campo_idx     <-   match( campo,  names(ds) )
  col_original  <-   ds[[  campo_idx  ]]
  
  nueva_col     <- fhistC( col_original, vector_desde ) 
  
  ds[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
  ds[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
  ds[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
}

nuevo_orden <-  c( setdiff( colnames( ds ) , "clase_binaria" ) , "clase_binaria" )
setcolorder( ds, nuevo_orden )

library(ranger)

#genero el modelo de Random Forest con la libreria ranger
params  <- list( "num.trees"=      500, #cantidad de arboles
                 "mtry"=             3, 
                 "min.node.size"=    1, # hoja mas chica
                 "max.depth"=        0  # 0 significa profundidad infinita
)

ds  <-  na.roughfix( ds )

modelo  <- ranger( formula= "clase_binaria ~ .",
                   data= ds[foto_mes %in% c(201906, 201907, 201908, 201909), ],  #aqui considero los 6 meses de 201906 a 201912
                   probability=   TRUE,  #para que devuelva las probabilidades
                   num.trees=     params$num.trees,
                   mtry=          params$mtry,
                   min.node.size= params$min.node.size,
                   max.depth=     params$max.depth
)

#aplico el modelo a los datos sin clase, 202001
prediccion_202001  <- predict(  modelo, ds[foto_mes==202001, ] )

#genero el dataset de entrega
entrega  <- as.data.table(cbind( "numero_de_cliente"= ds[ foto_mes==202001, numero_de_cliente],  
                                 "prob"= prediccion_202001$predictions[, "POS"]) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], 
        sep= ",",
        file= "entrega_2010_2.csv" )
