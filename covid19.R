install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("stringr")

library(stringr)
library(DataExplorer)
library(ggplot2)
setwd("/home/prog/R/covir_R")

covid19 <- read.csv("/home/gabriel/prog/R/covid_R/COVID19_JULIO2020.csv", sep =",", header=TRUE, encoding="UTF-8")

#CODIGOS DE PAISES
nations <- read.csv("/home/gabriel/prog/R/covid_R/docs/nations.csv", sep =",", header=TRUE, encoding="UTF-8")
colnames(nations)[1] <- "COUNTRY"
colnames(nations)[3] <- "CODE"
nations$COUNTRY <-toupper(nations$COUNTRY)
nations$COUNTRY <- str_replace(nations$COUNTRY, "Á", "A")
nations$COUNTRY <- str_replace(nations$COUNTRY, "Å", "A")
nations$COUNTRY <- str_replace(nations$COUNTRY, "É", "E")
nations$COUNTRY <- str_replace(nations$COUNTRY, "Í", "I")
nations$COUNTRY <- str_replace(nations$COUNTRY, "Ô", "O")
nations$COUNTRY <- str_replace(nations$COUNTRY, "Ó", "O")
nations$COUNTRY <- str_replace(nations$COUNTRY, "Ú", "U")
nations$COUNTRY <- str_replace(nations$COUNTRY, "''", "")
nations$COUNTRY <- str_replace(nations$COUNTRY, "�", "A")
# Modificar a mano A?RABE==ARABE E A?NDICO==INDICO
write.csv(nations, "/home/gabriel/prog/R/covid_R/nations.csv", fileEncoding = "UTF-8", row.names = FALSE)

#ATENCION
covid19$ATENCION[covid19$ATENCION == "N/A"] <- "No Registrado"
covid19$ATENCION <- str_replace(covid19$ATENCION, "Hospital UCI", "UCI")
DM_ATENCION <- covid19[c(6,6)]
colnames(DM_ATENCION)[1] <- "IDATENCION"
colnames(DM_ATENCION)[2] <- "NOMBRE"
DM_ATENCION <- DM_ATENCION[!duplicated(DM_ATENCION$NOMBRE),]
X=nrow(DM_ATENCION)
DM_ATENCION$IDATENCION=1:X
write.csv(DM_ATENCION, "/home/gabriel/prog/R/covid_R/DM_ATENCION.csv", fileEncoding = "UTF-8", row.names = FALSE)
covid19<-merge(covid19, DM_ATENCION, by.x = "ATENCION", by.y = "NOMBRE", all.x = TRUE)
covid19<-covid19[,-c(1)]

#SEXO
covid19$SEXO <- str_replace(covid19$SEXO,"m","M")
covid19$SEXO <- str_replace(covid19$SEXO,"f","F")
DM_SEXO <- covid19[c(7,7)]
colnames(DM_SEXO)[1] <- "IDSEXO"
colnames(DM_SEXO)[2] <- "NOMBRE"
DM_SEXO <- DM_SEXO[!duplicated(DM_SEXO$NOMBRE),]
X = nrow(DM_SEXO)
DM_SEXO$IDSEXO = 1:X
write.csv(DM_SEXO, "/home/gabriel/prog/R/covid_R/DM_SEXO.csv", fileEncoding = "UTF-8", row.names = FALSE)
covid19<-merge(covid19, DM_SEXO, by.x = "SEXO", by.y = "NOMBRE", all.x = TRUE)
covid19<-covid19[,-c(1)]

#TIPO
covid19$TIPO[covid19$TIPO == "N/A"] <- "No Registrado"
covid19$TIPO <- str_replace(covid19$TIPO,"relacionado","Relacionado")
covid19$TIPO <- str_replace(covid19$TIPO,"RELACIONADO","Relacionado")
covid19$TIPO <- str_replace(covid19$TIPO,"En Estudio","En estudio")
DM_TIPO <- covid19[c(7,7)]
colnames(DM_TIPO)[1] <- "IDTIPO"
colnames(DM_TIPO)[2] <- "NOMBRE"
DM_TIPO <- DM_TIPO[!duplicated(DM_TIPO$NOMBRE),]
X=nrow(DM_TIPO)
DM_TIPO$IDTIPO=1:X
write.csv(DM_TIPO, "/home/gabriel/prog/R/covid_R/DM_TIPO.csv", fileEncoding = "UTF-8", row.names = FALSE)
covid19<-merge(covid19, DM_TIPO, by.x = "TIPO", by.y = "NOMBRE", all.x = TRUE)
covid19<-covid19[,-c(1)]

#ESTADO
covid19$ESTADO[covid19$ESTADO == "N/A"] <- "No Registrado"
covid19$ESTADO <- str_replace(covid19$ESTADO,"leve","Leve")
DM_ESTADO <- covid19[c(7,7)]
colnames(DM_ESTADO)[1] <- "IDESTADO"
colnames(DM_ESTADO)[2] <- "NOMBRE"
DM_ESTADO <- DM_ESTADO[!duplicated(DM_ESTADO$NOMBRE),]
X = nrow(DM_ESTADO)
DM_ESTADO$IDESTADO = 1:X
write.csv(DM_ESTADO, "/home/gabriel/prog/R/covid_R/DM_ESTADO.csv", fileEncoding = "UTF-8", row.names = FALSE)
covid19<-merge(covid19, DM_ESTADO, by.x = "ESTADO", by.y = "NOMBRE", all.x = TRUE)
covid19<-covid19[,-c(1)]

#PAIS
covid19$PAIS[covid19$PAIS==""]<-"COLOMBIA"
covid19$PAIS <- str_replace(covid19$PAIS,"ESTADOS UNIDOS DE AMÉRICA","ESTADOS UNIDOS")
covid19$PAIS <- str_replace(covid19$PAIS,"ESTADOS UNIDOS DE AMERICA","ESTADOS UNIDOS")
covid19$PAIS <- str_replace(covid19$PAIS,"MÉXICO","MEXICO")
covid19$PAIS <- str_replace(covid19$PAIS,"PANAMÁ","PANAMA")
covid19$PAIS <- str_replace(covid19$PAIS,"PERÚ","PERU")
covid19$PAIS <- str_replace(covid19$PAIS,"CANADÁ","CANADA")
covid19$PAIS <- str_replace(covid19$PAIS,"ARABIA SAUDÍ","ARABIA SAUDITA")

#CODIGO PAIS
nations <- read.csv("/home/gabriel/prog/R/covid_R/nations.csv", sep =",", header=TRUE, encoding="UTF-8")
covid19<-merge(covid19,nations, by.x = "PAIS", by.y = "COUNTRY", all.x = TRUE)
covid19 <- covid19[, c(2:11, 1, 12:13)]
covid19$ISOCODE <- paste(covid19$CODE, covid19$ISO3, sep = "-")
covid19 <- covid19[,-c(12)]
covid19 <- covid19[,-c(12)]

#DIMENSION PAIS
DM_PAIS <- covid19[c(12,11,12)]
colnames(DM_PAIS)[1] <- "IDPAIS"
colnames(DM_PAIS)[2] <- "NOMBRE"
colnames(DM_PAIS)[3] <- "ISO3"

DM_PAIS <- DM_PAIS[!duplicated(DM_PAIS$NOMBRE),]
X = nrow(DM_PAIS)
DM_PAIS$IDPAIS = 1:X
covid19<-merge(covid19, DM_PAIS, by.x = "PAIS", by.y = "NOMBRE", all.x = TRUE)
write.csv(DM_PAIS, "/home/gabriel/prog/R/covid_R/DM_PAIS.csv", fileEncoding = "UTF-8", row.names = FALSE)
covid19 <- covid19[,-c(12)]
covid19 <- covid19[,-c(13)]
covid19 <- covid19[,-c(1)]

#CIUDAD
covid19$CIUDAD[is.na(covid19$CIUDAD)]<-"NA"
DM_CIUDAD <- covid19[c(3,4)]
colnames(DM_CIUDAD)[1] <- "IDCIUDAD"
colnames(DM_CIUDAD)[2] <- "NOMBRE"
DM_CIUDAD <- DM_CIUDAD[!duplicated(DM_CIUDAD$NOMBRE),]
write.csv(DM_CIUDAD, "/home/gabriel/prog/R/covid_R/DM_CIUDAD.csv", fileEncoding = "UTF-8", row.names = FALSE)
covid19 <- merge(covid19, DM_CIUDAD, by.x = "CIUDAD", by.y = "NOMBRE", all.x = TRUE)
covid19 <- covid19[,-c(1)]

#DEPARTAMENTO
colnames(covid19)[4] <- "DEPARTAMENTO"
covid19$IDDEPTO<-as.integer(covid19$IDCIUDAD/1000)
covid19$IDDEPTO[covid19$DEPARTAMENTO == "Barranquilla D.E."] <- "8001"
covid19$IDDEPTO[covid19$DEPARTAMENTO == "Buenaventura D.E."] <- "76109"
covid19$IDDEPTO[covid19$DEPARTAMENTO == "Cartagena D.T. y C."] <- "13001"
covid19$IDDEPTO[covid19$DEPARTAMENTO == "Santa Marta D.T. y C."] <- "47001"
covid19$IDDEPTO[is.na(covid19$DEPARTAMENTO)]<-"NA"
DM_DEPTO <- covid19[c(12,4)]
colnames(DM_DEPTO)[1] <- "IDDEPTO"
colnames(DM_DEPTO)[2] <- "NOMBRE"
DM_DEPTO <- DM_DEPTO[!duplicated(DM_DEPTO$NOMBRE),]
write.csv(DM_DEPTO, "/home/gabriel/prog/R/covid_R/DM_DEPTO.csv", fileEncoding = "UTF-8", row.names = FALSE)
covid19<-covid19[,-c(4)]
covid19<-covid19[,-c(3)]

#MODIFICAR IDCIUDAD
covid19$IDCIUDAD <- as.integer(covid19$IDCIUDAD) - (as.integer(covid19$IDDEPTO) * 1000)

#CATEGORIZAR EDAD
covid19$EDAD[covid19$EDAD == ""] <- 1
covid19$CEDAD <- cut(as.integer(covid19$EDAD), breaks = c(-1, 15, 18, 62, 120), labels = c("NIÑO", "JOVEN", "ADULTO", "ADULTO_MAYOR"))
covid19$CEDAD <-as.character(covid19$CEDAD)
covid19$CEDAD[covid19$CEDAD <= "NIÑO" & covid19$IDSEXO == 1] <- "NIÑA"
DM_CEDAD <- covid19[c(11,11)]
colnames(DM_CEDAD)[1] <- "IDCEDAD"
colnames(DM_CEDAD)[2] <- "NOMBRE"
DM_CEDAD <- DM_CEDAD[!duplicated(DM_CEDAD$NOMBRE),]
X = nrow(DM_CEDAD)
DM_CEDAD$IDCEDAD = 1:X
covid19<-merge(covid19, DM_CEDAD, by.x = "CEDAD", by.y = "NOMBRE", all.x = TRUE)
covid19<-covid19[,-c(1)]
write.csv(DM_CEDAD, "/home/gabriel/prog/R/covid_R/DM_CEDAD.csv", fileEncoding = "UTF-8", row.names = FALSE)

colnames(covid19)[1] <- "IDCASO"

date <- data.frame(fecha = as.Date(covid19$FECHA))
weekday <- weekdays(date$fecha)
covid19$DIASEMANA <- weekdays(as.Date(covid19$FECHA))






covid19 <- read.csv("/home/gabriel/prog/R/covid_R/COVID19_JULIO2020.csv", sep =",", header=TRUE, encoding="UTF-8")
years <- as.Date(covid19$FECHA)
covid19$YEAR <- format(years, "%Y")
covid19$BIRTH <- as.integer(covid19$YEAR) - as.integer(covid19$EDAD)

signo <- c('MONO', 'GALLO', 'PERRO', 'CERDO', 'RATA', 'BUFALO', 'TIGRE', 'CONEJO', 'DRAGON', 'SERPIENTE', 'CABALLO', 'CABRA')

covid19$ZCHINO <- signo[(covid19$BIRTH%%12+1)]
covid19<-covid19[,-c(13)]
covid19<-covid19[,-c(13)]

write.csv(covid19, "/home/gabriel/prog/R/covid_R/COVID19_JULIO2020.csv", fileEncoding = "UTF-8", row.names = FALSE)


# DATABASE
install.packages("RPostgreSQL")
install.packages("DBI")
install.packages("sf")
library(RPostgreSQL)
library(DBI)
library(sf)

pgdrv <- dbDriver(drvName = "PostgreSQL")
db <-DBI::dbConnect(pgdrv,
                    dbname="covid19",
                    host="localhost", port=5433,
                    user = 'gabriel',
                    password = '621gm848620')

dbListTables(db)
DBI::dbGetQuery(conn=db, statement="create table DM_ATENCION(IDATENCION serial primary key, NOMBRE TEXT);")
DBI::dbGetQuery(conn=db, statement="create table DM_CEDAD(IDCEDAD serial primary key, NOMBRE TEXT);")
DBI::dbGetQuery(conn=db, statement="create table DM_CIUDAD(IDCIUDAD serial primary key, NOMBRE TEXT);")
DBI::dbGetQuery(conn=db, statement="create table DM_DEPTO(IDDEPTO serial primary key, NOMBRE TEXT);")
DBI::dbGetQuery(conn=db, statement="create table DM_ESTADO(IDESTADO serial primary key, NOMBRE TEXT);")
DBI::dbGetQuery(conn=db, statement="create table DM_PAIS(IDPAIS serial primary key, NOMBRE TEXT, ISO3 TEXT);")
DBI::dbGetQuery(conn=db, statement="create table DM_SEXO(IDSEXO serial primary key, NOMBRE TEXT);")
DBI::dbGetQuery(conn=db, statement="create table DM_TIPO(IDTIPO serial primary key, NOMBRE TEXT);")

DBI::dbGetQuery(conn=db, statement="copy DM_ATENCION (IDATENCION, NOMBRE) from '/home/gabriel/prog/R/covid_R/DM_ATENCION.csv' delimiter ',' csv header;")

DBI::dbGetQuery(conn=db, statement="copy covid19_julio2020 (IDCASO, FECHA, EDAD, IDATENCION, IDSEXO, IDTIPO, IDESTADO, IDPAIS, IDCIUDAD, IDDEPTO, DIASEMANA, IDCEDAD) from '/home/gabriel/prog/R/covid_R/COVID19_JULIO2020.csv' delimiter ',' csv header;")

DBI::dbGetQuery(db,"SELECT * FROM dm_pais")




