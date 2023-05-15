library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(XML)
library(rvest)
library(httr)
library(SwimmeR)
library(png)
library(leaflet)
library(shinyWidgets)
library(proj4)
library(tidyverse)
library(tseries)
library(leaflet.extras)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(sparkline)
library(reshape2)
library(data.table)
library(ggiraph)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(utf8)
library(formattable)
library(mapview)
library(webshot)
#webshot::install_phantomjs()
#install.packages("dplyr")
#library(tinytex)
#update.packages(ask = FALSE, checkBuilt = TRUE)
#tinytex::tlmgr_update()
#tinytex::reinstall_tinytex()
#https://spatialreference.org/ref/epsg/32717/proj4js/
#rlang::last_error()
#Import Data
Agua_cruda <- read.csv("www/db000_agua_cruda.csv", encoding="UTF-8")
colnames(Agua_cruda)[1]="Central"
#which(is.na(Agua_cruda), arr.ind=TRUE)
#Agua_cruda[is.na(Agua_cruda)]=0
#write.csv2(Agua_cruda[,2:ncol(Agua_cruda)],"base/db000_agua_cruda.csv")
Agua_residual  <- read.csv("www/db000_agua_residual.csv", encoding="UTF-8")
colnames(Agua_residual)[1]="Central"
#Agua_residual[is.na(Agua_residual)]=0
#write.csv2(Agua_residual[,2:ncol(Agua_residual)],"www/db000_agua_residual.csv")
Agua_potable <- read.csv("www/db000_agua_potable.csv", encoding="UTF-8")
colnames(Agua_potable)[1]="Central"
#Agua_potable[is.na(Agua_potable)]=0
#write.csv2(Agua_potable[,2:ncol(Agua_potable)],"www/db000_agua_potable.csv")
Suelohist <-read.csv("www/db102_suelos.csv", encoding="UTF-8")
colnames(Suelohist)[1]="Central"
#Suelohist[is.na(Suelohist)]=0
#write.csv2(Suelohist[,2:ncol(Suelohist)],"www/db102_suelos.csv")
Agua_rio<- read.csv("www/db000_agua_rio.csv", encoding="UTF-8")
colnames(Agua_rio)[1]="Central"
#Agua_rio[is.na(Agua_rio)]=0
#write.csv2(Agua_rio[,2:ncol(Agua_rio)],"www/db000_agua_rio.csv")
Coordenadas <- read.csv("www/Puntos de monitoreo.csv", encoding="UTF-8")
colnames(Coordenadas)[1]="Central"
Lixiviados <- read.csv("www/db101_lixiviados.csv", encoding="UTF-8")
colnames(Lixiviados)[1]="Central"
#Lixiviados[is.na(Lixiviados)]=0
#write.csv2(Lixiviados[,2:ncol(Lixiviados)],"www/db101_lixiviados.csv")

Agua_cruda$Medición=gsub(",",".",Agua_cruda$Medición)
Agua_residual$Medición=gsub(",",".",Agua_residual$Medición)
Agua_potable$Medición=gsub(",",".",Agua_potable$Medición)
Suelohist$Medición=gsub(",",".",Suelohist$Medición)
Agua_rio$Medición=gsub(",",".",Agua_rio$Medición)
Lixiviados$Medición=gsub(",",".",Lixiviados$Medición)
################Ingresar tablas

t_Agua_cruda <- read.csv("www/norma_ac.csv", encoding="UTF-8")
#colnames(t_Agua_cruda)[1]="Parámetro"
#Agua_rio[t_Agua_cruda$Límite_máximo==" ---"]=0
#write.csv2(t_Agua_cruda[,2:ncol(t_Agua_cruda)],"www/norma_ac.csv")
t_Agua_residual  <- read.csv("www/norma_ar.csv", encoding="UTF-8")
#colnames(t_Agua_residual)[1]="Parámetro"
#t_Agua_residual[is.na(t_Agua_residual)]=0
#write.csv2(t_Agua_residual[,2:ncol(t_Agua_residual)],"www/norma_ar.csv")
t_Agua_potable <- read.csv("www/norma_ap.csv", encoding="UTF-8")
#colnames(t_Agua_potable)[1]="Parámetro"
#t_Agua_potable[is.na(t_Agua_potable)]=0
#write.csv2(t_Agua_potable[,2:ncol(t_Agua_potable)],"www/norma_ap.csv")
t_Suelohist <-read.csv("www/norma_suelo.csv", encoding="UTF-8")
#colnames(t_Suelohist)[1]="Parámetro"
#t_Suelohist[is.na(t_Suelohist)]=0
#write.csv2(t_Suelohist[,2:ncol(t_Suelohist)],"www/norma_suelo.csv")
t_Agua_rio<- read.csv("www/norma_ario.csv", encoding="UTF-8")
#colnames(t_Agua_rio)[1]="Parámetro"
#t_Agua_rio[like("---"),]=0
#write.csv2(t_Agua_rio[,2:ncol(t_Agua_rio)],"www/norma_ario.csv")
t_Lixiviados <- read.csv("www/norma_lixiviados.csv", encoding="UTF-8")
#colnames(t_Lixiviados)[1]="Norma_tabla"
#t_Lixiviados[is.na(t_Lixiviados)]=0
#write.csv2(t_Lixiviados[,2:ncol(t_Lixiviados)],"www/norma_lixiviados.csv")

########## Limpieza de tablas
#t_Suelohist$Límite_mínimo[is.na(t_Suelohist$Límite_mínimo)]=0
##################Unir tablas
Agua_cruda <- merge(Agua_cruda,t_Agua_cruda,by.x=c("Parámetro"), by.y=c("Parámetro"))
Agua_residual  <- merge(Agua_residual,t_Agua_residual,by.x=c("Parámetro"), by.y=c("Parámetro"))
Agua_potable <- merge(Agua_potable,t_Agua_potable,by.x=c("Parámetro"), by.y=c("Parámetro"))
Suelohist <-merge(Suelohist,t_Suelohist,by.x=c("Parámetro"), by.y=c("Parámetro"))
Agua_rio<- merge(Agua_rio,t_Agua_rio,by.x=c("Parámetro"), by.y=c("Parámetro"))
Lixiviados <- merge(Lixiviados,t_Lixiviados, by.x=c("Parámetro","Norma_tabla"), by.y=c("Parámetro","Norma_tabla"))
##########Reordenar tablas
Agua_cruda <- Agua_cruda[, c(2,3,4,5,6,7,8,1,12,13,14,9,10,11)]
Agua_residual  <-Agua_residual[, c(2,3,4,5,6,7,8,1,12,13,14,9,10,11)]
Agua_potable <- Agua_potable[, c(2,3,4,5,6,7,8,1,12,13,14,9,10,11)]
Suelohist <-Suelohist[, c(2,3,4,5,6,7,8,1,12,13,14,9,10,11)]
Agua_rio<- Agua_rio[, c(2,3,4,5,6,7,8,1,12,13,14,9,10,11)]
Lixiviados <- Lixiviados[, c(2,3,4,5,6,7,8,1,12,13,14,9,10,11)]
##########declarar como numerico

Lixiviados$Medición=round(as.double(Lixiviados$Medición),4)
Lixiviados$Límite_mínimo=round(as.double(Lixiviados$Límite_mínimo),4)
Lixiviados$Límite_máximo=round(as.double(Lixiviados$Límite_máximo),4)
Agua_rio$Medición=round(as.double(Agua_rio$Medición),4)
Agua_rio$Límite_mínimo=round(as.double(Agua_rio$Límite_mínimo),4)
Agua_rio$Límite_máximo=round(as.double(Agua_rio$Límite_máximo),4)
Agua_rio[Agua_rio$Límite_máximo==" ---"]=0
Agua_potable$Medición=round(as.double(Agua_potable$Medición),4)
Agua_potable$Límite_mínimo=round(as.double(Agua_potable$Límite_mínimo),4)
Agua_potable$Límite_máximo=round(as.double(Agua_potable$Límite_máximo),4)

Agua_residual$Medición=round(as.double(Agua_residual$Medición),4)

Agua_residual$Límite_mínimo=round(as.double(Agua_residual$Límite_mínimo),4)
Agua_residual$Límite_máximo=round(as.double(Agua_residual$Límite_máximo),4)
Agua_cruda$Medición=round(as.double(Agua_cruda$Medición),4)
Agua_cruda$Límite_mínimo=round(as.double(Agua_cruda$Límite_mínimo),4)
Agua_cruda$Límite_máximo=round(as.double(Agua_cruda$Límite_máximo),4)
Suelohist$Medición=round(as.double(Suelohist$Medición),4)
Suelohist$Límite_mínimo=round(as.double(Suelohist$Límite_mínimo),4)
Suelohist$Límite_máximo=round(as.double(Suelohist$Límite_máximo),4)
##Base Total
base=rbind(Agua_rio,Agua_potable,Agua_residual,Agua_cruda,Lixiviados,Suelohist)


########## Define UI for application that draws a histogram
mydata=Coordenadas
proj4string="+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
xy=data.frame(x=Coordenadas$X_coord,y=Coordenadas$Y_coord)
pj=project(data.frame(x=Coordenadas$X_coord,y=Coordenadas$Y_coord),proj4string,inverse = T)
mydata$Lat=pj$y
mydata$Long=pj$x
Grupos <- mydata %>% split(mydata$Recurso)

pal <- colorFactor(topo.colors(8), domain=c(mydata$Recurso%>%unique()))

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                
                titlePanel("Evaluación del cumplimiento normativo de las variables agua y suelo"),
                navbarPage("Celec Sur",
                           
                           tabPanel(icon("home"), 
                                    fluidRow(column(
                                      br(),
                                      tags$a(href='https://www.uazuay.edu.ec/',tags$img(src="UDA.png",width="170px",height="70px")),
                                      width=2),
                                      column(
                                        br(),
                                        
                                        p("A partir de datos producto de monitoreos periódicos de los Recursos agua y suelo 
                                     por parte de la empresa CELEC SUR, y bajo el cumplimento de la normativa ambiental 
                                     vigente, se busca estructurar una interfaz que permita generar reportes automáticos 
                                     de las mediciones de los Parámetros del índice de la calidad del agua y calidad del 
                                     suelo, en las que se podrá conocer el comportamiento de los Parámetros en distintos 
                                     puntos de muestreo; y si cumplen con los límites máximos permisibles requeridos por 
                                     la normativa. Con esto se espera poder facilitar un sistema interactivo que facilite 
                                     la visualización de los datos de monitoreo de agua y suelo.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                        width=8),
                                      column(
                                        br(),
                                        tags$a(href='https://www.celec.gob.ec/celecsur/',tags$img(src="celec.png",width="120px",height="70px")),
                                        width=2),
                                      
                                    ),
                                    br(),
                                    hr(),
                                    p(em("Developed by"),br("Marlon Yanez"),style="text-align:center; font-family: times")
                           ),
                           tabPanel(
                             "Guía de usuario",
                             fluidRow(column(
                               br(),
                               tags$a(href='https://www.uazuay.edu.ec/',tags$img(src="UDA.png",width="170px",height="70px")),
                               width=2),
                               column(titlePanel(h1("Manual de usuario",style="text-align:center; font-family: times")),width=8),
                               column(
                                 br(),
                                 tags$a(href='https://www.celec.gob.ec/celecsur/',tags$img(src="celec.png",width="120px",height="70px")),
                                 width=2)),
                             br(),
                             column(12, align="center", p("Este aplicativo permite al usuario visualizar los datos de monitoreo de los parámetros del Índice de
                              calidad del Agua y Suelo, y el cumplimiento de la normativa ambiental vigente. Además, permite
                              generar reportes automáticos y a partir de esos resultados brinda un plan de acción correctivo para
                              los parámetros que superan los límites máximos establecidos en la norma.",
                                                          br(),br(),
                                                          "Esta aplicación cuenta con 7 pestañas principales:",
                                                          br(),br(),
                                                          strong("1."),"Agua Cruda",
                                                          br(),br(),
                                                          strong("2."),"Agua Residual",
                                                          br(),br(),
                                                          strong("3."),"Agua Potable",
                                                          br(),br(),
                                                          strong("4."),"Agua de Río",
                                                          br(),br(),
                                                          strong("5."),"Lixiviados",
                                                          br(),br(),
                                                          strong("6."),"Suelo",
                                                          br(),br(),
                                                          strong("7."),"Descargas de Reportes",
                                                          br(),br(),
                                                          "Esta página web cuenta con elementos interactivos que permite al usuario realizar las siguientes
                                acciones:",
                                                          br(),br(),
                                                          strong("1."),"Visualizar espacialmente los puntos de monitoreo por: central, punto de medición y
                                recurso.",
                                                          br(),br(),
                                                          strong("2."),"Seleccionar: la central, punto de medición, tipo de recurso y fecha.",
                                                          br(),br(),
                                                          strong("3."),"Filtrar tablas según la fecha de interés.",
                                                          br(),br(),
                                                          strong("4."),"Observar gráficos de los valores de los parámetros medidos vs el limite máximo de la",
                                                          br(),br(),
                                                          "Este aplicativo forma parte del trabajo de titulación “Análisis del cumplimiento de la norma
                                ambiental de la calidad del agua y suelo en embalses artificiales de las Centrales Hidroeléctricas de
                                CELEC SUR” que se vincula al Convenio de cooperación interinstitucional para la ampliación de base
                                de datos, generación de reportes periódicos de variables ambientales en soporte a CELEC SUR.",
                                                          style="text-align:justify;color:black;background-color:lavender;padding:25px;border-radius:5px;width: 80%")),
                             br(),
                             hr(),
                             p(em("Developed by"),br("Jenny Wambanguito & Nancy Muñoz"),style="text-align:center; font-family: times")
                           ),
                           tabPanel(
                             "Mapa de puntos",
                             titlePanel(p("Geovisor", style = "color:#3474A7")),
                             fluidRow(width=12,column(p("Las fuentes utilizadas para analizar el área y evaluar las variables  seleccionadas, han sido trabajos realizados por CELEC EP. Según estos estudios, la Central Hidroeléctrica Paute Molino está ubicada en el límite de las provincias del Cañar, Chimborazo, Azuay y Morona Santiago, a 125 Km de la ciudad de Cuenca, la capital azuaya y a 50 Km de Santiago de Méndez, perteneciente a la provincia de Morona Santiago",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),width = 12)
                                      ,column(leafletOutput("mapa", height = 700), width = 6)
                                      ,column(dataTableOutput("Table"),width=6)
                             )
                             
                           ),
                           tabPanel(
                             "Agua cruda",
                             
                             titlePanel("Datos historicos de los monitoreos de agua cruda"),
                             column(br(),
                                    
                                    p("De conformidad con la normativa ambiental vigente, se realiza el monitoreo de la calidad del agua cruda, para 
                                      verificar el cumplimiento de los  límites permisibles de los parámetros establecidos en la Tabla 1: Criterios de 
                                      Calidad de fuentes de agua para consumo humano y 
                                      doméstico.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                    width=12),
                             fluidRow(column(12, tabsetPanel(type="tab",
                                                             tabPanel("Datos por fecha",
                                                                      column(selectInput("c_a_c","Selecciona la Central",choices = c(unique(Agua_cruda$Central)), selected = "paute_mazar"),width = 4),
                                                                      column(uiOutput("pestana_ac2"),width = 4),
                                                                      column(uiOutput("pestana_ac3"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_a_c")),width = 12),
                                                                      column(sparklineOutput("test_spark_a_c"),
                                                                             withSpinner(dataTableOutput("tabla_a_c")),width = 12)
                                                                      
                                                             ),
                                                             tabPanel("Datos por parámetro",
                                                                      column(selectInput("c_a_c_2","Selecciona la Central",choices = c(unique(Agua_cruda$Central)), selected = "paute_mazar"),width = 4),
                                                                      column(uiOutput("pestana_ac2_2"),width = 4),
                                                                      column(uiOutput("pestana_ac3_2"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_a_c_2")),width = 12),
                                                                      column(sparklineOutput("test_spark_a_c_2"),
                                                                             withSpinner(dataTableOutput("tabla_a_c_2")),width = 12))
                             )))
                           ),
                           tabPanel(
                             "Agua residual",
                             titlePanel("Datos historicos de los monitoreos de agua residual"),
                             column(br(),
                                    
                                    p("Son los líquidos de composición variada provenientes de usos doméstico, municipal, industrial, comercial, agrícola, pecuario 
                                      o de otra índole, ya sea pública o privada y que por tal motivo haya sufrido degradación en su calidad original. La misma que 
                                      es regulada por la normativa ambiental vigente donde indica los límites permisibles que deben cumplir los diferentes parámetros
                                      caracterizados, de acuerdo a la tabla 9 “Límites de descarga a un cuerpo de agua dulce del Anexo 1, LIBRO VI, 
                                      TULSMA.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                    width=12),
                             fluidRow(column(12, tabsetPanel(type="tab",
                                                             tabPanel("Datos por fecha",
                                                                      column(selectInput("c_a_r","Selecciona la Central",choices = c(unique(Agua_residual$Central)), selected = "paute_sopladora"),width = 4),
                                                                      column(uiOutput("pestana_ar2"),width = 4),
                                                                      column(uiOutput("pestana_ar3"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_a_r")),width = 12),
                                                                      column(sparklineOutput("test_spark_a_r"),
                                                                             withSpinner(dataTableOutput("tabla_a_r")),width = 12)),
                                                             tabPanel("Datos por parámetro",
                                                                      column(selectInput("c_a_r_2","Selecciona la Central",choices = c(unique(Agua_residual$Central)), selected = "paute_mazar"),width = 4),
                                                                      column(uiOutput("pestana_ar2_2"),width = 4),
                                                                      column(uiOutput("pestana_ar3_2"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_a_r_2")),width = 12),
                                                                      column(sparklineOutput("test_spark_a_r_2"),
                                                                             withSpinner(dataTableOutput("tabla_a_r_2")),width = 12))
                             )))),
                           tabPanel(
                             "Agua potable",
                             titlePanel("Datos historicos de los monitoreos de agua potable"),
                             column(br(),
                                    
                                    p("El agua cuyas características físicas, químicas y microbiológicas han sido tratadas a fin de garantizar que 
                                      esta sea apta para el consumo humano y que cumpla con los criterios de calidad descritos en la Norma NTE INEN 
                                      1108",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                    width=12),
                             fluidRow(column(12, tabsetPanel(type="tab",
                                                             tabPanel("Datos por fecha",
                                                                      column(selectInput("c_a_p","Selecciona la Central",choices = c(unique(Agua_potable$Central)), selected = "paute_sopladora"),width = 4),
                                                                      column(uiOutput("pestana_ap2"),width = 4),
                                                                      column(uiOutput("pestana_ap3"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_a_p")),width = 12),
                                                                      column(sparklineOutput("test_spark_a_p"),
                                                                             withSpinner(dataTableOutput("tabla_a_p")),width = 12)),
                                                             tabPanel("Datos por parámetro",
                                                                      column(selectInput("c_a_p_2","Selecciona la Central",choices = c(unique(Agua_potable$Central)), selected = "paute_mazar"),width = 4),
                                                                      column(uiOutput("pestana_ap2_2"),width = 4),
                                                                      column(uiOutput("pestana_ap3_2"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_a_p_2")),width = 12),
                                                                      column(sparklineOutput("test_spark_a_p_2"),
                                                                             withSpinner(dataTableOutput("tabla_a_p_2")),width = 12))
                             )))),
                           tabPanel(
                             "Agua de rio",
                             titlePanel("Datos historicos de los monitoreos de agua de río"),
                             column(br(),
                                    
                                    p("Los ríos poseen características propias de calidad,  y ésta es variable con el tiempo, la caracterización de la 
                                      calidad del agua de un río implica la determinación del grado, el nivel o la intensidad de la contaminación que 
                                      posee, la cual puede ser de origen físico, químico o biológico, para el seguimiento y control de la calidad de 
                                      este recurso se aplica la tabla 2: Criterios de calidad admisibles para la preservación de la vida acuática y 
                                      silvestres en aguas dulces, marinas y de estuarios del anexo 1, LIBRO VI, TULSMA.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                    width=12),
                             fluidRow(column(12, tabsetPanel(type="tab",
                                                             tabPanel("Datos por fecha",
                                                                      column(selectInput("c_r","Selecciona la Central",choices = c(unique(Agua_rio$Central)), selected = "paute_sopladora"),width = 4),
                                                                      column(uiOutput("pestana_r2"),width = 4),
                                                                      column(uiOutput("pestana_r3"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_r")),width = 12),
                                                                      column(sparklineOutput("test_spark_r"),
                                                                             withSpinner(dataTableOutput("tabla_r")),width = 12)),
                                                             tabPanel("Datos por parámetro",
                                                                      column(selectInput("c_rio_2","Selecciona la Central",choices = c(unique(Agua_rio$Central)), selected = unique(Agua_rio$Central)[1]),width = 4),
                                                                      column(uiOutput("pestana_rio2_2"),width = 4),
                                                                      column(uiOutput("pestana_rio3_2"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_rio_2")),width = 12),
                                                                      column(sparklineOutput("test_spark_rio_2"),
                                                                             withSpinner(dataTableOutput("tabla_rio_2")),width = 12)) 
                             )))),
                           tabPanel(
                             "Lixiviados",
                             titlePanel("Datos historicos de los monitoreos lixiviados"),
                             column(br(),
                                    
                                    p("La normativa ambiental vigente establece la obligatoriedad del tratamiento de lixiviados en las fuentes de agua superficiales 
                                      y subterráneas, y la importancia de la eliminación de contaminantes nocivos y sustancias orgánicas que pueden impactar 
                                      negativamente al ecosistema, los valores y parámetros para su análisis están establecidos en la Tabla 8. Límites de descarga al 
                                      sistema de alcantarillado público y la Tabla 9. Límites de descarga a un cuerpo de agua dulce",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                    width=12),
                             fluidRow(column(12, tabsetPanel(type="tab",
                                                             tabPanel("Datos por fecha",
                                                                      column(selectInput("c_l","Selecciona la Central",choices = c(unique(Lixiviados$Central)), selected = "paute_sopladora"),width = 4),
                                                                      column(uiOutput("pestana_l2"),width = 4),
                                                                      column(uiOutput("pestana_l3"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_l")),width = 12),
                                                                      column(sparklineOutput("test_spark_l"),
                                                                             withSpinner(dataTableOutput("tabla_l")),width = 12)),
                                                             tabPanel("Datos po parámetro",
                                                                      column(selectInput("c_l_2","Selecciona la Central",choices = c(unique(Lixiviados$Central)), selected = "paute_mazar"),width = 4),
                                                                      column(uiOutput("pestana_l2_2"),width = 4),
                                                                      column(uiOutput("pestana_l3_2"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_l_2")),width = 12),
                                                                      column(sparklineOutput("test_spark_l_2"),
                                                                             withSpinner(dataTableOutput("tabla_l_2")),width = 12))
                             )))),
                           tabPanel(
                             "Suelo",
                             titlePanel("Datos historicos de los moitoreos de suelo"),
                             column(br(),
                                    
                                    p("La caracterización de suelos en las centrales hidroeléctricas y en cumplimiento de la Norma de calidad ambiental del recurso 
                                      suelo y criterios de remediación para suelos contaminados, determina los límites permisibles de contaminantes en función del uso 
                                      del suelo, y toma en cuenta los parámetros establecidos en la Tabla 2. Criterios de remediación (valores máximos 
                                      permisibles) ",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                    width=12),
                             fluidRow(column(12, tabsetPanel(type="tab",
                                                             tabPanel("Datos por fecha",
                                                                      column(selectInput("c_s","Selecciona la Central",choices = c(unique(Suelohist$Central)), 
                                                                                         selected = "paute_mazar"),width = 4),
                                                                      column(uiOutput("pestana_s2"),width = 4),
                                                                      column(uiOutput("pestana_s3"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_s")),width = 12),
                                                                      column(sparklineOutput("test_spark"),
                                                                             withSpinner(dataTableOutput("tabla_s")),width = 12)
                                                                      
                                                             ),
                                                             tabPanel("Datos por parámetro",
                                                                      column(selectInput("c_s_p","Selecciona la Central",choices = c(unique(Suelohist$Central)), 
                                                                                         selected = "paute_mazar"),width = 4),
                                                                      column(uiOutput("pestana_s_p2"),width = 4),
                                                                      column(uiOutput("pestana_s_p3"), width = 4),
                                                                      column(withSpinner(girafeOutput("barras_s2")),width = 12),
                                                                      column(sparklineOutput("test_spark2"),
                                                                             withSpinner(dataTableOutput("tabla_s2")),width = 12))
                             )))),
                           tabPanel(
                             "Descargar reporte",
                             fluidRow(column(width=3,
                                             selectInput("c_des", "Elige la Central:", 
                                                         choices = c("Mazar","Molino","Sopladora"),selected = "Mazar")),
                                      column(uiOutput("pestana_des2"),width = 3),
                                      column(uiOutput("pestana_des3"),width = 3),
                                      column(uiOutput("pestana_des4"),width = 3),
                                      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                      column(width=12,align="center",radioButtons("filetype", "Tipo de archivo",
                                                                                  choices = c("Word", "PDF"))) ,
                                      #column(width=12,align="center",radioButtons("filetype", "Tipo de archivo", choices = c("Word", "PDF"))) ,
                                      column(width=12,align="center",downloadButton("report", "Descargar reporte"))
                             ),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             hr(),
                             fluidRow(column(
                               br(),
                               tags$a(href='https://www.uazuay.edu.ec/',tags$img(src="UDA.png",width="150px",height="50px")),
                               width=2),
                               column(width=8),
                               column(
                                 br(),
                                 tags$a(href='https://www.celec.gob.ec/celecsur/',tags$img(src="celec.png",width="100px",height="50px")),
                                 width=2))
                           )
                ))

