# Modificacion para Cursos del FCFM

Sys.setlocale("LC_ALL", "UTF-8")
library(data.table)
#library(reshape)
library(RCurl)
library(plyr)

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
    colnames(valor)[1] <-"hora"
    z <- melt(valor,"hora")
    z <- subset(z, value != "")
    z$dia = dias[i]
    salida <- rbind(salida,z)
  }
  return(salida)
}

#Leer el archivo desde Google Docs

google_docs="https://docs.google.com/spreadsheets/d/1QYHDeZoWcMZjZJo0f0f4RV-UThQS5ahk2sqdh9iqFPc/pub?gid=";

formato = "&single=true&output=csv"
#Son los GIDs de las hojas del libro que estamos importando

gids = c("1271806036","1097022579","1442796562","609821330","1342824950")

hojas1 = lapply(gids, function(x) { return (paste(google_docs,as.character(x),formato,sep="") )})

archivos1 = lapply(hojas1,getURLContent)

tablas1 <- lapply(archivos1,exportTables)


## Convierto con reshape

salida <- tableInvert(tablas1)

salida<- rename(salida, c("variable"="salon", "value" = "materia"))
write.csv(salida,file="~/horarios.csv",row.names = FALSE)


