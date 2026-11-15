library(dplyr)
# la funcion recibe en df: dataframe, ventana: numeric, k.alerta: numeric
# retorna un data frame con columnas: fecha, n.tx, media, desv, alerta
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


#recibe un vector de montos y retorna su MAD
benford.mad <- function(x){
  
  d <-  floor(x/(10^floor(log10(x))))
  
  conteos <- table(factor(d, levels = 1:9))
  p.d <- as.numeric(conteos)/length(x)
  B.d <- log10(1+ (1/(1:9)))
  
  mad <- mean(abs(p.d-B.d))
  
  return(mad)
}


# recibe el df de transacciones, un canal, y el df de cantones y retorna un df con
# codigo de canton, canton, n, mad y clase

benford.canton <- function(df, canal, cantones) {
  
  canal.objetivo <-  canal
  
  df.res <- df |> 
    filter(canal == canal.objetivo) |> 
    group_by(cod_canton) |> 
    summarise(
      n = n(),
      mad = benford.mad(monto_crc),
    
    ) |> 
    left_join(cantones, by = "cod_canton") |> 
    mutate(
      clase = case_when(
        mad <= 0.006 ~ "conformidad cercana",
        mad <= 0.012 ~ "conformidad aceptable",
        mad <= 0.015 ~ "conformidad marginal",
        .default = "no conformidad"
      )
    ) |> 
    select(cod_canton, canton, n, mad, clase) |> 
    arrange(desc(mad))
  
  return(df.res)
}
  















