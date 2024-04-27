library(rvest)
library(dplyr)

################################################################################
#                                                                              #
#             Funciones de scrapping para extraer datos BC y AC                #
#                                                                              #
################################################################################

extraeSigloNegLunar <- function(yearbase){
  
  #yearbase = 100
  
  yearbase2 = yearbase + 99
  
  yearbase_aux <- ifelse(yearbase < 1000 & yearbase >= 100,
                     paste0("0", yearbase),
                     yearbase)
  
  yearbase2_aux <- ifelse(yearbase2 < 1000 & yearbase2 >= 100,
                     paste0("0", yearbase2),
                     yearbase2)
  
  yearbase_aux <- ifelse(yearbase < 100 & yearbase >= 10,
                     paste0("00", yearbase),
                     yearbase_aux)
  
  yearbase2_aux <- ifelse(yearbase2 < 100 & yearbase2 >= 10,
                      paste0("00", yearbase2),
                      yearbase2_aux)
  
  yearbase_aux <- ifelse(yearbase < 10 & yearbase >= 0,
                     paste0("000", yearbase),
                     yearbase_aux)
  
  yearbase2_aux <- ifelse(yearbase2 < 10 & yearbase2 >= 1,
                      paste0("000", yearbase2),
                      yearbase2_aux)
  
  
  # URL de la página
  url <- paste0("https://eclipsewise.com/lunar/LEcatalog/LE-",
                yearbase2_aux,"--",yearbase_aux,".html")
  
  # Leer las tablas de la página
  tables <- url %>%
    read_html() %>%
    html_table()
  
  label <- as.character(tables[[1]][2,])
  
  df <- tables[[1]][c(3: nrow(tables[[1]])),]
  
  for (i in c(2: length(tables))){
    
    if (length(tables[[i]]) > 10){
      
      df <- rbind(df, tables[[i]][c(3: nrow(tables[[i]])),])
    }
    
  }
  
  names(df) <- label
  
  return(df)
  
}

extraeSigloPosLunar <- function(yearbase){
  
  #yearbase = 1
  
  yearbase2 = yearbase + 99
  
  yearbase_aux <- ifelse(yearbase < 1000 & yearbase >= 100,
                         paste0("0", yearbase),
                         yearbase)
  
  yearbase2_aux <- ifelse(yearbase2 < 1000 & yearbase2 >= 100,
                          paste0("0", yearbase2),
                          yearbase2)
  
  yearbase_aux <- ifelse(yearbase < 100 & yearbase >= 10,
                         paste0("00", yearbase),
                         yearbase_aux)
  
  yearbase2_aux <- ifelse(yearbase2 < 100 & yearbase2 >= 10,
                          paste0("00", yearbase2),
                          yearbase2_aux)
  
  yearbase_aux <- ifelse(yearbase < 10 & yearbase >= 0,
                         paste0("000", yearbase),
                         yearbase_aux)
  
  yearbase2_aux <- ifelse(yearbase2 < 10 & yearbase2 >= 1,
                          paste0("000", yearbase2),
                          yearbase2_aux)
  
  
  # URL de la página
  url <- paste0("https://eclipsewise.com/lunar/LEcatalog/LE",
                yearbase_aux,"-",yearbase2_aux,".html")
  
  # Leer las tablas de la página
  tables <- url %>%
    read_html() %>%
    html_table()
  
  label <- as.character(tables[[1]][2,])
  
  df <- tables[[1]][c(3: nrow(tables[[1]])),]
  
  for (i in c(2: length(tables))){
    
    if (length(tables[[i]]) > 10){
      
      df <- rbind(df, tables[[i]][c(3: nrow(tables[[i]])),])
    }
    
  }
  
  names(df) <- label
  
  return(df)
  
}

################################################################################
################################################################################
################################################################################


################################################################################
#                                                                              #
#                   Paso 1: Se extrae el Siglo I BC                            #
#                                                                              #
#Se extrae este año que bajo criterio histórico sería -1 para ini-cializar el  #
#bucle que viene a continuación                                                #
#                                                                              #
################################################################################

#Se llama a la url del siglo primero BC
url <- paste0("https://eclipsewise.com/lunar/LEcatalog/LE-0099-0000.html")

tables <- url %>%
  read_html() %>%
  html_table()

#Nombre de columnas
label <- as.character(tables[[1]][2,])

#Datos del siglo primero BC
df <- tables[[1]][c(3: nrow(tables[[1]])),]

#Se extraen los datos de cada uno de los años del siglo primero BC
for (i in c(2: length(tables))){
  
  if (length(tables[[i]]) > 10){
    
    df <- rbind(df, tables[[i]][c(3: nrow(tables[[i]])),])
  }
  
}

#Se aplica una denominación por columna análoga a la de origen
names(df) <- label

################################################################################
#                                                                              #
#                   Paso 2: Extracción datos BC                                #
#                                                                              #
#Se extraen todos los datos desde el 3000 BC mediante el siguiente bucle       #
#                                                                              #
################################################################################

for (i in seq(100, 2900, 100)){
  
  df <- rbind(df, extraeSigloNegLunar(i))  
  
}

################################################################################
#                                                                              #
#                   Paso 3: Extracción datos AC                                #
#                                                                              #
#Se extraen todos los datos desde el 3000 AC mediante el siguiente bucle y se  #
#unen a los anteriores                                                         #
#                                                                              #
################################################################################

for (i in seq(1, 2901, 100)){
  
  df <- rbind(df, extraeSigloPosLunar(i))  
  
}



################################################################################
#                                                                              #
#                   Paso 4: Data Cleaning                                      #
#                                                                              #
#Se homogeneiza la fecha y los nombres de las columans                         #
#                                                                              #
################################################################################


#Se crean 2 conjuntos de datos separando la información del siglo I BC del resto

df_0 <- df[c(251:253),] #Información del siglo I BC o año 0 según criterio Fred
df <- df[c(-253:-251),] #Resto de la información

#Datos BC sin año -1 o 0 según Fred Espenak

dfNeg <- df %>% 
  filter(substr(`Calendar Date`, 1, 1) == "-")

dfPos <- df %>% 
  filter(substr(`Calendar Date`, 1, 1) != "-")

#Homogeneización a tiempos históricos de los datos negativos. Hay que restar 1
#y aplicar el formato -yyyy-mm-dd en la fecha

#Se resta 1 a las fechas BC
yearHist <- ifelse(substr(dfNeg$`Calendar Date`, 1, 1) %in% c("-", "0"),
                   (as.numeric(substr(dfNeg$`Calendar Date`, 2, 5)) + 1)*(-1),
                   as.numeric(substr(dfNeg$`Calendar Date`, 2, 5)))

yearHistL <- ifelse(nchar(yearHist) == 4 ,
                    paste0("-0", substr(yearHist, 2, 4)),
                    as.character(yearHist))

yearHistL <- ifelse(nchar(yearHist) == 3,
                    paste0("-00", substr(yearHist, 2, 4)),
                    yearHistL)

yearHistL <- ifelse(nchar(yearHist) == 2,
                    paste0("-000", substr(yearHist, 2, 4)),
                    yearHistL)

yearHistL <- ifelse(nchar(yearHist) == 1,
                    paste0("-0000", substr(yearHist, 2, 4)),
                    yearHistL)

yearHistL <- paste0(yearHistL,
                    substr(dfNeg$`Calendar Date`, 6, nchar(dfNeg$`Calendar Date`)) 
)

aux <- data.frame(yearHistL)

dfNeg$`Calendar Date` <- yearHistL

#Se aplica -1 al año 0

df_0$`Calendar Date` <- paste0("-", substr(df_0$`Calendar Date`, 1, 3), "1 ",
                               substr(df_0$`Calendar Date`, 6, nchar(df_0$`Calendar Date`))
)

#Se unen toda la información BC
df <- rbind(dfNeg, df_0, dfPos)

#Se pasa a número la parte del mes que forma la fecha, por ejemplo Dec pasa a 12
#y Nov pasaría a 11 ...

HistDate <- gsub(" Dec ", "-12-", df$`Calendar Date`)
HistDate <- gsub(" Nov ", "-11-", HistDate)
HistDate <- gsub(" Oct ", "-10-", HistDate)
HistDate <- gsub(" Sep ", "-09-", HistDate)
HistDate <- gsub(" Aug ", "-08-", HistDate)
HistDate <- gsub(" Jul ", "-07-", HistDate)
HistDate <- gsub(" Jun ", "-06-", HistDate)
HistDate <- gsub(" May ", "-05-", HistDate)
HistDate <- gsub(" Apr ", "-04-", HistDate)
HistDate <- gsub(" Mar ", "-03-", HistDate)
HistDate <- gsub(" Feb ", "-02-", HistDate)
HistDate <- gsub(" Jan ", "-01-", HistDate)

#Se sustituye la columna por el vector
df$`Calendar Date` <- HistDate

#Se ordenan los BC

dfNeg <- df[which(substr(df$`Calendar Date`, 1, 1) == "-"),]
dfNeg <- dfNeg[order(dfNeg$`Calendar Date`, decreasing = T),]

#Se ordenan los AC

dfPos <- df[which(substr(df$`Calendar Date`, 1, 1) != "-"),]
dfPos <- dfPos[order(dfPos$`Calendar Date`, decreasing = F),]

#Se obtiene el dataset final
MoonEclipsesDates <- rbind(dfNeg, dfPos)

#Se crean nombres de columnas adecuados para ser tratados por apps

names(MoonEclipsesDates) <- c("HistDate", "TDGreatestEclipse", "ΔT",
                              "ΔTSigma", "LunaNum", "SarosNum",
                              "EclType", "QSE", "Gamma", "PenMag",
                              "UmMag", "PenDurm", "ParDurm","TotalDurm", 
                              "ZenLat", "ZenLong")

save(MoonEclipsesDates, file = "MoonEclipsesDates.rda")