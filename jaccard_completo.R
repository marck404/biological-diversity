### calcular diversidad beta
### Marcos Torres Vivanco y Luis Fernando Pardo Sixtos

rm(list=ls())

## cargamos los datos
datos <- read.csv("MatrizBeta_Marisa_coord.csv")
datos01 <- as.matrix(datos[,4:49]) 



sums <- as.vector(datos01%*%matrix(rep(1,ncol(datos01)), ncol=1))
prods <- prods<-datos01%*%t(datos01)

## definimos funciones para maximo sin 1 y minimo sin 0
is.one <- function(a){
  if(a==1){
    x <- TRUE
  } else {
    x <- FALSE
  }
  return(x)
}

vis.one <- Vectorize(is.one)

is.zero <- function(a){
  if(a==0){
    x <- TRUE
  } else {
    x <- FALSE
  }
  return(x)
}

vis.zero <- Vectorize(is.zero)

min_scero <- function(x){
  min(x[!vis.zero(x)])
}

max_suno <- function(x){
  max(x[!vis.one(x)])
}

## Calculamos la matriz con todos los indices de Jaccard
Jac<-t(t(-prods)+sums)
Jac<-Jac+sums
Jac<-prods/(Jac)
Jac[is.nan(Jac)]<-1

## Calculamos los datos solicitados promedio, minimo, maximo, desviacion estandar
Jac_promedio <-apply(Jac, 1, mean)
Jac_minimo <- apply(Jac, 1, min_scero)
Jac_maximo <- apply(Jac, 1, max_suno)
Jac_desviacion_estandar <- apply(Jac,1,sd)

## creamos un data frame con columnas los datos solicitados para cada pixel
Jac <- matrix(c(Jac_promedio,Jac_minimo,Jac_maximo,Jac_desviacion_estandar),
              ncol = 4)
Jac <- as.data.frame(Jac)
columnas <- c('Promedio', 'Minimo','Maximo','desv. estand.')
names(Jac) <- columnas



View(Jac)


write.csv(Jac,file = "Jaccard.csv")






















