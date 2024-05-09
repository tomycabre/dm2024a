# Carga de librerías
library(data.table)
library(rpart)
library(rpart.plot)

# Establecimiento del directorio de trabajo
setwd("C:/Users/54911/Desktop/Coding/zITBAdatamining")

# Carga del conjunto de datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# División del conjunto de datos en entrenamiento y aplicación
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

# Construcción del modelo
modelo <- rpart(
  formula = clase_ternaria ~ .,
  data = dtrain,
  control = rpart.control(cp = -0.3, minsplit = 10, minbucket = 5, maxdepth = 5)
)

# Visualización del árbol de decisión
rpart.plot(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0)

# Aplicación del modelo a los datos de aplicación
prediccion <- predict(modelo, newdata = dapply, type = "prob")

# Agregar la probabilidad de BAJA+2 a los datos de aplicación
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# Definir la función de ganancia
ganancia <- function(baja2, baja1, continua) {
  return(117000 * baja2 - 3000 * (baja1 + continua))
}

# Calcular la ganancia para diferentes umbrales
umbral_optimo <- 0
ganancia_maxima <- -Inf

for (umbral in seq(0.01, 0.5, by = 0.01)) {
  dapply[, Predicted := as.numeric(prob_baja2 > umbral)]
  
  # Calcular la ganancia
  ganancia_actual <- ganancia(
    sum(dapply$Predicted == 1 & dapply$clase_ternaria == "BAJA+2"),
    sum(dapply$Predicted == 1 & dapply$clase_ternaria == "BAJA+1"),
    sum(dapply$Predicted == 0 & dapply$clase_ternaria == "CONTINUA")
  )
  
  # Actualizar umbral óptimo si se encuentra una ganancia mayor
  if (ganancia_actual > ganancia_maxima) {
    ganancia_maxima <- ganancia_actual
    umbral_optimo <- umbral
  }
}

# Asignar predicciones utilizando el umbral óptimo
dapply[, Predicted := as.numeric(prob_baja2 > umbral_optimo)]

# Visualización del umbral óptimo
cat("Umbral óptimo:", umbral_optimo, "\n")
cat("Ganancia máxima:", ganancia_maxima, "\n")

# Generación del archivo para Kaggle
dir.create("./exp/")
dir.create("./exp/KA2001")

fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_001_optimizado.csv",
       sep = ",")
