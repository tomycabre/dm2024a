# Intento de Solucion del desafio 15k
# que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
# pero crea estructura sobre la cual trabajar

# Limpio la memoria
rm(list = ls())
gc()

require("data.table")

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}

# Variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores <- c()
GLOBAL_tiros_total <- 0

# Crea el juego
# a cada jugador se le pone un numero de 1 a 100 en la espalda
# debajo de ese numero esta el indice_de_enceste que NO puede ser visto por el cazatalentos
gimnasio_init <- function() {
  GLOBAL_jugadores <<- sample(c((501:599) / 1000, 0.7))
  GLOBAL_tiros_total <<- 0
}

# se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
# devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar <- function(pids, pcantidad) {
  GLOBAL_tiros_total <<- GLOBAL_tiros_total + length(pids) * pcantidad
  res <- mapply(ftirar, GLOBAL_jugadores[pids], pcantidad)
  
  return(res)
}

# El cazatalentos decide a que jugador llevarse
# devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto <- function(pid) {
  return(list("tiros_total" = GLOBAL_tiros_total,
              "acierto" = as.integer(GLOBAL_jugadores[pid] == 0.7)))
}

#------------------------------------------------------------------------------

Estrategia_B <- function() {
  # Estrategia
  # Se juegan varias rondas
  # En cada ronda, los jugadores que participan, tiran X tiros
  # De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  # Se elige el mejor jugador de la sexta ronda
  
  gimnasio_init()
  
  # Esta es la planilla del cazatalentos
  # El id es el número que tiene en la espalda cada jugador
  planilla_cazatalentos <- data.table("id" = 1:100)
  
  # Ronda 1
  ids_juegan1 <- 1:100
  planilla_cazatalentos[ids_juegan1, tiros1 := 50]
  resultado1 <- gimnasio_tirar(ids_juegan1, 50)
  planilla_cazatalentos[ids_juegan1, aciertos1 := resultado1]
  
  # Ronda 2
  quantil1 <- planilla_cazatalentos[ids_juegan1, quantile(aciertos1, probs = 0.30, names = FALSE)]
  ids_juegan2 <- planilla_cazatalentos[ids_juegan1][aciertos1 > quantil1, id]
  planilla_cazatalentos[ids_juegan2, tiros2 := 55]
  resultado2 <- gimnasio_tirar(ids_juegan2, 55)
  planilla_cazatalentos[ids_juegan2, aciertos2 := resultado2]
  planilla_cazatalentos[ids_juegan2, aciertos_totales_2 := aciertos1 + aciertos2]
  
  # Ronda 3
  quantil2 <- planilla_cazatalentos[ids_juegan2, quantile(aciertos_totales_2, probs = 0.35, names = FALSE)]
  ids_juegan3 <- planilla_cazatalentos[ids_juegan2][aciertos_totales_2 > quantil2, id]
  planilla_cazatalentos[ids_juegan3, tiros3 := 65]
  resultado3 <- gimnasio_tirar(ids_juegan3, 65)
  planilla_cazatalentos[ids_juegan3, aciertos3 := resultado3]
  planilla_cazatalentos[ids_juegan1, aciertos_totales_3 := aciertos1 + aciertos2 + aciertos3]
  
  # Ronda 4
  quantil3 <- planilla_cazatalentos[ids_juegan3, quantile(aciertos_totales_3, probs = 0.60, names = FALSE)]
  ids_juegan4 <- planilla_cazatalentos[ids_juegan3][aciertos_totales_3 >= quantil3, id]
  planilla_cazatalentos[ids_juegan4, tiros4 := 85]
  resultado4 <- gimnasio_tirar(ids_juegan4, 85)
  planilla_cazatalentos[ids_juegan4, aciertos4 := resultado4]
  planilla_cazatalentos[ids_juegan4, aciertos_totales_4 := aciertos1 + aciertos2 + aciertos3 + aciertos4]
  
  # Ronda 5
  quantil4 <- planilla_cazatalentos[ids_juegan4, quantile(aciertos_totales_4, probs = 0.70, names = FALSE)]
  ids_juegan5 <- planilla_cazatalentos[ids_juegan4][aciertos_totales_4 >= quantil4, id]
  tiros <- floor((14999 - GLOBAL_tiros_total) / length(ids_juegan5))
  if (tiros < 0) {
    tiros <- 0
  }
  planilla_cazatalentos[ids_juegan5, tiros5 := tiros]
  resultado5 <- gimnasio_tirar(ids_juegan5, tiros)
  planilla_cazatalentos[ids_juegan5, aciertos5 := resultado5]
  planilla_cazatalentos[ids_juegan5, aciertos_totales_5 := aciertos1 + aciertos2 + aciertos3 + aciertos4 + aciertos5]
  
  # Epílogo
  pos_mejor <- planilla_cazatalentos[, which.max(aciertos_totales_5)]
  id_mejor <- planilla_cazatalentos[pos_mejor, id]
  veredicto <- gimnasio_veredicto(id_mejor)
  
  return(veredicto)
}
#------------------------------------------------------------------------------

# Aquí hago la Estimación Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed(100151) # debe ir una sola vez, ANTES de los experimentos

tabla_veredictos <- data.table(tiros_total = integer(), acierto = integer())

for (experimento in 1:10000) {
  if (experimento %% 1000 == 0) cat(experimento, " ") # desprolijo, pero es para saber por dónde voy
  
  veredicto <- Estrategia_B()
  
  tabla_veredictos <- rbind(tabla_veredictos, veredicto)
}

cat("\n")

tiros_total <- tabla_veredictos[, max(tiros_total)]
tasa_eleccion_correcta <- tabla_veredictos[, mean(acierto)]

tiros_total
tasa_eleccion_correcta

# Es una sábana corta ...
