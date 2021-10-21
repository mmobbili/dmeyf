#Resolucion desafío cazatalentos 14k Matias Poullain

rm(list=ls())
gc()


tiempo <- Sys.time() #Para ver el tiempo transcurrido

#Bibliotecas:
library(data.table)
library(tidyverse)

#Defino los jugadores

mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) #intencionalmente el mejor esta al final

#Funcion de tirar hasta errar:
#Siempre se empieza con un tiro (lo emboque o lo erre), se suman tiros si acierta

tirar_hasta_errar <- function(prob){
  tiros <- 1
  while(runif(1)<prob){# & tiros <= 5){
    
    tiros <- tiros + 1
  }
  return(tiros)
}


###Funcion que devuelve data.table de los resultados de los jugadores:
##Parametros:
#vector.jugadores: Puede ser un vector de la probabilidad de tiro de los jugadores o bien un data.table de los resultados de la vuelta anterior
#veces: Numero de rondas de tiros hasta errar que cada jugador debe realizar cuando se inicia la prueba
#veces.rep: Numero de rondas de tiros hasta errar que cada jugador debe realizar en las rondas posteriores a la primera
#mejor.ronda.anterior: los que seran evaluados en la siguiente ronda, elegidos segun los resultados de las rondas anteriores
devuelvo.tabla.tiros <- function(vector.jugadores, veces = NULL, veces.rep = NULL, mejor.ronda.anterior = NULL){
  
  #Diferencio primera ronda (solo vector de jugadores) de las siguientes rondas (data.table de los resultados):
  if(!is.data.table(vector.jugadores)){
    #Genero la planilla
    data <- setDT(expand.grid(jugadord = vector.jugadores, ronda = 1:veces))
    #Simulo los tiros
    data$ID <- 1:nrow(data)
    data[, tiros := tirar_hasta_errar(jugadord), by = ID]
    data$ID <- NULL
    
    #Guardo la cantidad de tiros totales realizados
    n.tiros <- sum(data$tiros)
    
    
  }else{
    #Genero planilla de las vueltas que no son las primeras
    data2 <- setDT(expand.grid(jugadord = mejor.ronda.anterior, ronda = max(vector.jugadores$ronda) + 1:veces.rep))
    #Simulo los tiros
    data2$ID <- 1:nrow(data2)
    data2[, tiros := tirar_hasta_errar(jugadord), by = ID]
    data2$ID <- NULL
    #Guardo la cantidad de tiros totales realizados
    n.tiros <- sum(data2$tiros)
    
    
    #Junto data.table de resultados de la vuelta n con los de la vuelta n-1
    data <- rbind(vector.jugadores, data2)
    
  }
  
  #Ordeno los jugadores segun el promedio de tiros realizados en las rondas
  jugadores.ordenados <- data[, .(tiros2 = mean(tiros)), by = jugadord] %>%
    arrange(desc(tiros2)) %>%
    pull(jugadord)
  
  
  #Devuelvo la planilla de todo el concurso hasta el momento, el numero de tiros realizados en la vuelta y los jugadores posicionados segun su media de tiros
  return(list(planilla = data, n.tiros = n.tiros, posiciones = jugadores.ordenados))
  
}

##Iteraciones:

set.seed( 102191012 )

n.tiros.totales <- c() #Guardo los tiros totales realizados en cada iteracion
posicion.mejor <- c() #Guardo en que posicion salio el mejor en cada iteracion
jugadores.elegidos <- c() #Guardo los jugadores que se eligieron como el mejor en cada ronda

for(i in 1:10000){
  cat("\rIteracion", i)
  #Primera ronda de 20 vueltas cada jugador:
  prueba <- devuelvo.tabla.tiros(jugadores, veces = 20)
  jugadores.it <- prueba$planilla #Planilla luego de la segunda ronda
  n.tiros <- prueba$n.tiros #Inicio la cantidad de tiros
  posiciones <- prueba$posiciones #Posiciones de los jugadores
  
  #Siguientes rondas
  #Los jugadores que tiran son los que estan posicionados de 1 a "ultimo"
  #En cada ronda, las posiciones pueden cambiar y los jugadores de 1-"ultimo" de la ronda n-1 pueden no ser los mismos que lso de la ronda n
  #En cada ronda se tiran 10 tiros
  for(ultimo in c(95, 80, 70, 50, 30, 20, 10)){
    ronda <- 1
    while(ronda <= 9){
      prueba <- devuelvo.tabla.tiros(jugadores.it, veces.rep = 1, mejor.ronda.anterior = posiciones[1:ultimo]) #No todos los jugadores juegan
      jugadores.it <- prueba$planilla
      n.tiros <- n.tiros + prueba$n.tiros
      posiciones <- prueba$posiciones
      ronda <- ronda + 1
    }
  }
  
  #Para este punto, el jugador tiene muchas posibilidades de estar entre los 10 primeros
  #Los 10 primeros juegan hasta llegar a 13900 tiros totales (aunque se pueden pasar)
  while(n.tiros < 13900){
    prueba <- devuelvo.tabla.tiros(jugadores.it, veces.rep = 1, mejor.ronda.anterior = posiciones[1:8])
    jugadores.it <- prueba$planilla
    n.tiros <- n.tiros + prueba$n.tiros
    posiciones <- prueba$posiciones
  }
  
  #Guardo los resultados de la iteracion
  jugadores.elegidos <- c(jugadores.elegidos, posiciones[1])
  posicion.mejor <- c(posicion.mejor, which(posiciones == mejor))
  n.tiros.totales <- c(n.tiros.totales, n.tiros)
}


cat("\nMayor cantidad de tiros realizados:", max(n.tiros.totales))
cat("\nMenor cantidad de tiros realizados:", min(n.tiros.totales))
cat("\nMedia de la cantidad de tiros realizados:", mean(n.tiros.totales))
cat("\nPeor posicion del mejor jugador:", max(posicion.mejor))
cat("\nPosicion media del mejor jugador:", mean(posicion.mejor))
cat("\nVeces que finalmente se eligio al mejor jugador:", sum(jugadores.elegidos == mejor))
print(Sys.time() - tiempo)