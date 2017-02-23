# Modificacion para Cursos del FCFM


Sys.setlocale("LC_ALL", "UTF-8")
library(data.table)
#library(reshape)
library(RCurl)
library(plyr)
library(splitstackshape)
library(RMySQL)
library(psych)
exportTables <- function(x){
  salida <- read.csv(textConnection(x,encoding=c("","bytes","UTF-8")), header = TRUE,fileEncoding = "UTF-8")
  return(salida)
}

toJSONarray <- function(dtf){
  clnms <- colnames(dtf)
  name.value <- function(i){
    quote <- '';
    if(!class(dtf[, i]) %in% c('numeric', 'integer')){
      quote <- '"';
    }
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x, collapse=', ')})
  objs <- paste('{', objs, '}')
  res <- paste('[', paste(objs, collapse=', '), ']')
  return(res)
}


insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

tableInvert <- function(x){
  salida = data.frame()
  dias <- c("LUNES","MARTES","MIERCOLES","JUEVES","VIERNES")
  for ( i in seq(1:5)){
    valor <- as.data.frame(x[i])
    colnames(valor)[1] <-"HORA"
    z <- melt(valor,"HORA")
    z <- subset(z, value != "")
    z$dia = dias[i]
    salida <- rbind(salida,z)
  }
  return(salida)
}

#Leer el archivo desde Google Docs

google_docs="https://docs.google.com/spreadsheets/d/1QYHDeZoWcMZjZJo0f0f4RV-UThQS5ahk2sqdh9iqFPc/pub?gid=";

formato = "&single=true&output=csv"


gids = c("1271806036","1097022579","1442796562","609821330","1342824950")


hojas1 = lapply(gids, function(x) { return (paste(google_docs,as.character(x),formato,sep="") )})



archivos1 = lapply(hojas1,getURLContent)

tablas1 <- lapply(archivos1,exportTables)


## Convierto con reshape

salida1 <- tableInvert(tablas1)


##
salida <- data.frame()

salida <- rbind(salida,salida1)

dt2 = data.table(salida)


# dividimos la columna  salon en varios campos

salida <- cSplit(salida, splitCols = "value", sep = "-", direction = "wide", drop = FALSE)

#Eliminamos la X de los salones
salida$variable <- gsub('X',"",salida$variable)


## Renombramos los campos
salida<- rename(salida, c("variable"="salon","value_1"="materia", "value_2" = "profesor", "value_3" = "seccion"))

salida$seccion[is.na(salida$seccion)] <- 1
#Eliminamos la columna sobrante
salida$value <- NULL


#Aun no se si eliminar esta parte

#mydb = dbConnect(MySQL(), user='ale', password='a9dfe2fm93.', dbname='FCFM-Cursos', host='localhost')

#salida3 <- as.data.frame.matrix(salida2)
#json <- toJSONarray(salida3)

#write(json,file="lista.json")

write.csv(salida,file="horarios.csv",row.names = FALSE)
