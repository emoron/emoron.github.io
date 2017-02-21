# Modificacion para Cursos del FCFM

# first try Windows CP1252, although that's almost surely not supported on Mac:
#Sys.setlocale("LC_ALL", "pt_PT.1252") # Make sure not to omit the `"LC_ALL",` first argument, it will fail.
#Sys.setlocale("LC_ALL", "pt_PT.CP1252") # the name might need to be 'CP1252'

# next try IS08859-1(/'latin1'), this works for me:
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

#cars_csv = getURL(paste(google_docs, cars_key, sep = ""))

gids = c("1271806036","1097022579","1442796562","609821330","1342824950")


#hojas1 = lapply(edificioo, function(x) { return (paste(google_docs,as.character(x),formato,sep="") )})
#hojas2 = lapply(edificioi, function(x) { return (paste(google_docs,as.character(x),formato,sep="") )})
#hojas3 = lapply(edificion, function(x) { return (paste(google_docs,as.character(x),formato,sep="") )})
#hojas4 = lapply(edificiok, function(x) { return (paste(google_docs,as.character(x),formato,sep="") )})

hojas1 = lapply(gids, function(x) { return (paste(google_docs,as.character(x),formato,sep="") )})



archivos1 = lapply(hojas1,getURLContent)

tablas1 <- lapply(archivos1,exportTables)


## Convierto con reshape

salida1 <- tableInvert(tablas1)


##
salida <- data.frame()

salida <- rbind(salida,salida1)
#salida <- rbind(salida,salida2)
#salida <- rbind(salida,salida3)
#salida <- rbind(salida,salida4)

salida<- rename(salida, c("variable"="salon", "value" = "materia"))
salidadt = data.table(salida)
dt2 <-salidadt
#dt2$duracion = 3
#horas = c("7","8","9","10","11","12","13","14","15","16","17","18","19","20")

#salones = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","")
#for(i in 1:16){
#    dt2[salon == paste("X",as.character(i),sep=""), salon := as.character(i)]
    #dt[horario == paste("H",as.character(i),sep=""),horario := as.numeric(horas[i])]
#}


salida2<- dt2

salida3 <- as.data.frame.matrix(salida2)
json <- toJSONarray(salida3)

#write(json,file="lista.json")

write.csv(salida,file="~/horarios.csv",row.names = FALSE)
