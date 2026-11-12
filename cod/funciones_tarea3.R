library(dplyr)

alerta.movil <- function(df, ventana, k.alerta){
  
  df.final <- df |> 
    group_by(fecha) |> 
    summarise(n.tx = n()) |> 
    ungroup() |> 
    arrange(fecha)
    
  n <- nrow(df.final)
  df.final$media <- NA
  df.final$desv <- NA
  df.final$alerta <- NA
  
  for(t in (ventana + 1):n){
    datos_ventana <- df.final$n.tx[(t- ventana):(t-1)]
    
    df.final$media[t] <- mean(datos_ventana, na.rm = TRUE)
    df.final$desv[t] <- sd(datos_ventana, na.rm = TRUE)
    df.final$alerta[t] <- df.final$n.tx[t] > (df.final$media[t] + k.alerta*df.final$desv[t])
    
  }
  
  df.final <- df.final |> 
    select(fecha, n.tx, media, desv, alerta)
  return(df.final)
}