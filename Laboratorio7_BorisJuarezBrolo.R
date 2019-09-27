require(dplyr)
require(tidyverse)
require(stringr)
require(reshape2)
require(lubridate)
require(ggplot2)

#Preparando el dataset
df <- read_csv("~/UFM/Economía/Data Wrangling/Laboratorio7_BorisJuarezBrolo/Lab7/c1.csv") 

df <- df %>% select(-X23, -X24, -X25, -X26, -X27, -X28)
  str(df)
  
  # Formateando y factorizando fecha y codigo
  df$Fecha <- dmy(df$Fecha)
  df$Cod <- as.factor(df$Cod)
  
  # Tipos de servicio disponibles
  df$Cod %>% levels()
  
  ## Removiendo las unidades monetarias Q
  Monetario <- c("Camion_5", "Pickup", "Moto", "factura", "directoCamion_5", "directoPickup", "directoMoto", "fijoCamion_5", "fijoPickup", "fijoMoto")
  for (x in Monetario) {
    vec<- df %>% pull(x)
    df[,x] <-  str_replace(string = vec ,pattern = "Q-", replacement = "")
    df[,x] <-  str_replace(string = vec ,pattern = "Q", replacement = "")
    df[,x] <- as.numeric(df %>% pull(x))
  }
  
  #Limpiando la data
  df <- gather(df, key= "Equipo_G", value= "Gasto Total", "Camion_5", "Pickup", "Moto", na.rm = TRUE, factor_key = TRUE)
  df <- gather(df, key= "Directo", value= "Gasto Directo", "directoCamion_5", "directoPickup", "directoMoto", na.rm = TRUE)
  df <- gather(df, key= "Fijo", value= "Gasto Fijo", "fijoCamion_5", "fijoPickup", "fijoMoto", na.rm = TRUE)
  df <- gather(df, key = "Periodo", value ="Hi", "5-30", "30-45", "45-75", "75-120", "120+" , na.rm = TRUE, factor_key = TRUE)
  df<- df %>% select(-Directo, -Fijo, -Hi)
  str(df)
  
  #Estado de Resultados Breve
  PL <- df %>% summarise(Ventas = sum(factura), `Gastos Directos`= -sum(`Gasto Directo`),
                         `Gastos Fijos`= -sum(`Gasto Fijo`), `Gastos Totales`= -sum(`Gasto Total`))
  PL <- PL %>% mutate(UtilidadOperativa= Ventas + `Gastos Totales`)
  PL <- gather(PL, key="Cuenta", value = "Monto") 
  
  PL
  
  #Tarifas y Costo de perdida ante mantenimiento o reparación
  Unitario<- df %>% group_by(ID) %>% summarise(Tarifa=sum(factura), Costo_Por_poste= sum(`Gasto Total`)) %>% 
    ungroup() %>% summarise(`Tarifa`=sum(Tarifa)/n(), Costo_Por_poste= sum(Costo_Por_poste)/n()) 
  
  Unitario
  
  #Grafico del ingreso mensual acorde al monto facturado
  
  df %>% mutate(Ingreso_mensual= factura- `Gasto Total`) %>% 
    group_by( Cod, Mes=month(Fecha, label = TRUE)) %>% summarise(factura=sum(factura)/n())%>% 
    ggplot(aes(x=Mes, y=factura, color=Cod)) + geom_point(size=4) + labs(title="Ingreso Mensual") + 
    theme(axis.text.x = element_text(angle = 70))
 
  
  # Evaluación de perdida o números rojos
  df %>% group_by(Mes=month(Fecha, label = TRUE)) %>% 
    summarise(Ventas=sum(factura), `Gastos Directos`= sum(`Gasto Directo`),
              `Gastos Fijos`= sum(`Gasto Fijo`), `Gastos Totales`= sum(`Gasto Total`),
              Utilidad_Operativa= sum(factura)-sum(`Gasto Total`)) %>% select(Mes, `Gastos Fijos`, `Gastos Directos`, Utilidad_Operativa,Ventas) %>% 
    gather(key= "Cuenta", value = "Monto", -Mes) %>%  
    ggplot(aes(x=Mes,y=Monto/1000, fill=Cuenta)) +geom_col(position = "stack")+ labs(title="Estado de Resultados Gráfico (cifras en millares)")
 
  
  # Dias Promedio para realizar mantenimiento
  ID_Unicos <- unique(df$ID)
  Prom_Poste <- c()
  #for(x in ID_Unicos){
    vec<- df %>% filter(ID== x) %>% select(Fecha) %>% arrange(Fecha) %>% pull(Fecha)
    dias_Poste <- c()
    if(length(vec)>1){
      for(j in 1:length(vec)-1){
        dias_Poste <- c(dias_Poste, vec[j+1]-vec[j]) 
      }
      Prom_Poste <- c(Prom_Poste,mean(dias_Poste))
    }
    #else{
      Prom_Poste <- c(Prom_Poste,0)
    #}
  #}
  Promedio_Mantenimiento <- mean(Prom_Poste)
  Promedio_Mantenimiento

  # Centros de operación y su respectiva carga.
  df %>% group_by(Mes= month(Fecha, label = TRUE), origen=as.factor(origen)) %>% 
    ggplot(aes(x=Mes, fill=origen)) + geom_bar(position = "dodge") + 
    labs(title="Ordenes por Centro de operación")
  
  # Ordenes por Poste
  df %>% group_by(ID) %>% count(name = "ordenes") %>% arrange(desc(ordenes)) %>% 
    ungroup() %>% select(ID,ordenes) %>% arrange(desc(ordenes))%>% head (25) %>% View()
  

  
  # Ordenes por Tipo de Vehiculo
  df %>% group_by(origen=as.factor(origen), Vehiculo) %>% 
    count() %>% arrange(origen, n) %>% mutate(Acum=cumsum(origen)) %>% mutate

  
  df %>% mutate(Ganancia= factura- `Gasto Total`) %>% group_by(Vehiculo) %>% 
    summarise(Ventas=sum(factura),Ganancia= sum(Ganancia),  Porcentaje=(sum(factura)-sum(`Gasto Total`))/sum(factura)) %>% 
    View()
  
  
  
  




