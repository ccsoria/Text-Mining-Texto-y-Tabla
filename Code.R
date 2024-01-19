

################################################################################
# Configuracion previa
################################################################################

# Instalar paquetes.
install.packages("tesseract")
install.packages("tidyverse")
install.packages("magick")
install.packages("pdftools")
install.packages("purrr")
install.packages("stringr")
install.packages("writexl")

# Habilitar paquetes instalados.
paquetes <- c("tesseract","tidyverse","magick","pdftools","purrr","stringr","writexl")
lapply(paquetes, library, character.only = TRUE)

# Limpiar interfaz de R.
rm(list=ls())

# Carpeta.
input <- "C:/Users/Cesar/Desktop/Investigaciones/Mesas de dialogo/Productos/"


################################################################################
# Recorte de PDF
################################################################################
# El PDF contiene dos partes que es dificil de leer para el software. Se decide
# dividir en dos el PDF en texto y tabla. Esto permite una mejor manipulacion de datos.

# Archivo
acta <- paste0(input,"Producto 1 - Acta_Formato Sugerido.pdf")
# Nuevos archivos
acta_1 <- paste0(input,"acta_1.pdf")
acta_2 <- paste0(input,"acta_2.pdf")

# Leer el PDF
pdf_contenido <- pdf_text(acta)

# Posicion del punto de corte
corte <- grep("Siendo las 12:30...", pdf_contenido)

# Se divide el PDF en dos partes
acta_1 <- pdf_subset(acta, pages = 1:(corte), output = acta_1)
acta_2 <- pdf_subset(acta, pages = (corte+1):length(pdf_contenido), output = acta_2)


################################################################################
# Primera parte: Texto
################################################################################
# Idioma de OCR. Se puede descargar el castellano.
# tesseract_download("spa")
lector <- tesseract(language = "spa")

# Manipulacion del acta
texto_1 <- pdftools::pdf_convert(acta_1,dpi=1100)
texto_2 <- image_read(texto_1)
texto_2 <- texto_2 %>%
  image_scale("1800") %>%
  image_crop("1700x1800+40+40")

# Extraccion de datos
texto_3 <- tesseract::ocr(image = texto_2, engine = lector)

# Ordenar datos
doc_data <- texto_3 %>% str_split('\n') %>% unlist()

# Crear una base de datos
doc_data <- as.data.frame(doc_data)
colnames(doc_data) <- c("text")

# Limpieza de textos
#=============================================
#Se quita la primera observacion
doc_data <- doc_data %>% filter(str_detect(doc_data$text,"Página 1 de ")!=T)

#Se identifican los espacios donde se encuentran las firmas
doc_data <- doc_data %>% mutate(borrar = ifelse(text=="FIRMAS",1,0))
for(pagina_final in 1:1000){
  for(pagina_actual in 1:1000){
    if(pagina_actual<=pagina_final){
      doc_data$borrar[doc_data$text==paste0("Página ",pagina_actual," de ",pagina_final)] <- 2
    }
  }
}

#ID de cada oracion
doc_data$index = 1:dim(doc_data)[1]

#Se eliminan los espacios donde se encuentran las firmas
for(obs in 1:dim(doc_data)[1]){
  doc_data$borrar[which(doc_data$index==obs & doc_data$borrar[obs]==0 & doc_data$borrar[obs-1]==1)] <- 1
}
doc_data$borrar[doc_data$borrar==2] <- 1
doc_data <- doc_data %>% filter(borrar==0) %>% select(text)

# Sistematizar de textos
#=============================================
#En esta parte pueden ocurrir errores porque se encuentran tildes que dificultan el codigo.

#ID de cada oracion
doc_data$index = 1:dim(doc_data)[1]

#Sesion
doc_data <- doc_data %>% mutate(sesion = if_else(substring(text,1,7)=="Sesión:" | substring(text,1,7)=="Sesion:",
                                                 substring(text,first=8),NA_character_))
doc_data <- doc_data %>% mutate(sesion = min(sesion,na.rm = T))

#Region
doc_data <- doc_data %>% mutate(region = if_else(substring(text,1,7)=="Región:" | substring(text,1,7)=="Region:",
                                                 substring(text,first=8),NA_character_))
doc_data <- doc_data %>% mutate(region = min(region,na.rm = T))

#Provincia
doc_data <- doc_data %>% mutate(provincia = if_else(substring(text,1,10)=="Provincia:",substring(text,first=11),NA_character_))
doc_data <- doc_data %>% mutate(provincia = min(provincia,na.rm = T))

#Distrito
doc_data <- doc_data %>% mutate(distrito = if_else(substring(text,1,9)=="Distrito:",substring(text,first=10),NA_character_))
doc_data <- doc_data %>% mutate(distrito = min(distrito,na.rm = T))

#Lugar espec?fico
doc_data <- doc_data %>% mutate(lugar = if_else(substring(text,1,17)=="Lugar específico:" | substring(text,1,17)=="Lugar especifico:",
                                                substring(text,first=18),NA_character_))
doc_data <- doc_data %>% mutate(lugar = min(lugar,na.rm = T))

#Fecha de reuni?n
doc_data <- doc_data %>% mutate(fecha = if_else(substring(text,1,17)=="Fecha de reunión:" | substring(text,1,17)=="Fecha de reunion:",
                                                substring(text,first=18),NA_character_))
doc_data <- doc_data %>% mutate(fecha = min(fecha,na.rm = T))

#Institucion
doc_data <- doc_data %>% mutate(institucion = ifelse(text=="Instituciones presentes:",1,0))
for(obs in 1:dim(doc_data)[1]){
  doc_data$institucion[which(doc_data$index==obs & doc_data$institucion[obs-1]==1)] <- 1
}
doc_data$institucion[doc_data$text=="Instituciones presentes:"] <- 0
doc_data$institucion[doc_data$text=="AGENDA U ORDEN DEL DÍA" | doc_data$text=="AGENDA U ORDEN DEL DIA"] <- 2
for(obs in 1:dim(doc_data)[1]){
  doc_data$institucion[which(doc_data$index==obs & doc_data$institucion[obs-1]==2)] <- 2
}
doc_data$institucion[doc_data$institucion==2] <- 0
doc_data <- doc_data %>% mutate(institucion = ifelse(institucion==1,text,0))
institucion <- doc_data %>% select(c(institucion)) %>% filter(institucion!="0")
institucion$index = 1:dim(institucion)[1]

#Agenda
doc_data <- doc_data %>% mutate(agenda = ifelse(doc_data$text=="AGENDA U ORDEN DEL DÍA" | doc_data$text=="AGENDA U ORDEN DEL DIA",1,0))
for(obs in 1:dim(doc_data)[1]){
  doc_data$agenda[which(doc_data$index==obs & doc_data$agenda[obs-1]==1)] <- 1
}
doc_data$agenda[doc_data$text=="AGENDA U ORDEN DEL DÍA" | doc_data$text=="AGENDA U ORDEN DEL DIA"] <- 0
doc_data$agenda[doc_data$text=="PETITORIOS, PEDIDOS O SOLICITUDES"] <- 2
for(obs in 1:dim(doc_data)[1]){
  doc_data$agenda[which(doc_data$index==obs & doc_data$agenda[obs-1]==2)] <- 2
}
doc_data$agenda[doc_data$agenda==2] <- 0
doc_data <- doc_data %>% mutate(agenda = ifelse(agenda==1,text,0))
agenda <- doc_data %>% select(c(agenda)) %>% filter(agenda!="0")
agenda$index = 1:dim(agenda)[1]

#Pedidos
doc_data <- doc_data %>% mutate(pedido = ifelse(doc_data$text=="PETITORIOS, PEDIDOS O SOLICITUDES",1,0))
for(obs in 1:dim(doc_data)[1]){
  doc_data$pedido[which(doc_data$index==obs & doc_data$pedido[obs-1]==1)] <- 1
}
doc_data$pedido[doc_data$text=="PETITORIOS, PEDIDOS O SOLICITUDES"] <- 0
doc_data$pedido[doc_data$text=="ACUERDOS"] <- 2
for(obs in 1:dim(doc_data)[1]){
  doc_data$pedido[which(doc_data$index==obs & doc_data$pedido[obs-1]==2)] <- 2
}
doc_data$pedido[doc_data$pedido==2] <- 0
doc_data <- doc_data %>% mutate(pedido = ifelse(pedido==1,text,0))
pedido <- doc_data %>% select(c(pedido)) %>% filter(pedido!="0")
pedido$index = 1:dim(pedido)[1]

#Acuerdos. Notar que es diferente a las variables anteriores.
doc_data <- doc_data %>% mutate(acuerdo = ifelse(doc_data$text=="ACUERDOS",1,0))
for(obs in 1:dim(doc_data)[1]){
  doc_data$acuerdo[which(doc_data$index==obs & doc_data$acuerdo[obs-1]==1)] <- 1
}
doc_data$acuerdo[doc_data$text=="ACUERDOS"] <- 0
doc_data$acuerdo[str_detect(doc_data$text,"Siendo las")==T & doc_data$acuerdo==1] <- 2
for(obs in 1:dim(doc_data)[1]){
  doc_data$acuerdo[which(doc_data$index==obs & doc_data$acuerdo[obs-1]==2)] <- 2
}
doc_data$acuerdo[doc_data$acuerdo==2] <- 0
doc_data <- doc_data %>% mutate(acuerdo = ifelse(acuerdo==1,text,0))
acuerdo <- doc_data %>% select(c(acuerdo)) %>% filter(acuerdo!="0")
acuerdo$index = 1:dim(acuerdo)[1]

# Union de bases de datos
pre_base <- doc_data %>% select(c(index,sesion,region,provincia,distrito,lugar,fecha))
nombres_base <- list(pre_base,institucion,agenda,pedido,acuerdo)
final_parte1 <- nombres_base %>% reduce(full_join,by='index') %>% filter(!is.na(institucion) | !is.na(agenda) | !is.na(pedido) | !is.na(acuerdo))


################################################################################
# Segunda parte: Tabla
################################################################################

raw_text <- pdf_text(acta_2)

limpiar_tabla <- function(raw) {
  # dividir las páginas individuales
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  
  # concatenar las páginas divididas
  raw <- reduce(raw, c)
  
  # encontrar índices de las filas deseadas (N del 1 al 12)
  table_rows <- stringr::str_which(raw, "\\b(?:1|2|3|4|5|6|7|8|9|10|11|12)\\b")
  
  # extraer la porción de texto de la tabla
  table <- raw[table_rows]
  
  # Construir la tabla y eliminar caracteres especiales
  table <- str_replace_all(table, "\\s{2,}", "|")
  
  # Dividir cada fila por el delimitador "|"
  table <- str_split(table, "\\|")
  
  # Asegurarse de que todas las filas tengan la misma cantidad de elementos
  max_columns <- max(lengths(table))
  table <- lapply(table, function(x) {
    if (length(x) < max_columns) {
      x <- c(x, rep(NA, max_columns - length(x)))
    }
    return(x)
  })
  
  # Convertir la lista a un dataframe
  data_table <- as.data.frame(do.call(rbind, table), stringsAsFactors = FALSE)
  
  # Crear una lista de nombres de columnas
  colnames(data_table) <- paste0("V", seq_along(colnames(data_table)))
  
  data_table
}

final_parte2 <- map_df(raw_text, limpiar_tabla)

# Desde el N 10 en adelante no se separo bien los datos, para ello solo se separa el numero del nombre:
for (col in names(final_parte2)) {
  # Utilizar expresiones regulares para eliminar números seguidos por espacio
  final_parte2[[col]] <- gsub("\\d+\\s", "", final_parte2[[col]])
}

# Mover los datos para ordenar la base desde la fila 12 hasta el final
final_parte2$V4[c(12:nrow(final_parte2))] <- final_parte2$V3[c(12:nrow(final_parte2))]
final_parte2$V3[c(12:nrow(final_parte2))] <- final_parte2$V2[c(12:nrow(final_parte2))]
final_parte2$V2[c(12:nrow(final_parte2))] <- final_parte2$V1[c(12:nrow(final_parte2))]

#Eliminar los NA y variables que no interesan
final_parte2<- na.omit(final_parte2)
final_parte2 <- final_parte2[, -which(names(final_parte2) == "V1")]
final_parte2 <- final_parte2[, -which(names(final_parte2) == "V4")]

#Renombrar las variables
names(final_parte2)[names(final_parte2) == "V2"] <- "nombre"
names(final_parte2)[names(final_parte2) == "V3"] <- "cargo"


################################################################################
# Exportacion de los resultados
################################################################################

write_xlsx(final_parte1,paste0(input,"Parte_1.xlsx"))
write_xlsx(final_parte2,paste0(input,"Parte_2.xlsx"))
