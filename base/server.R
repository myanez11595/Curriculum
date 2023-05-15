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

server<-function(input, output){
  
  #Mapa  
  output$mapa <- renderLeaflet({
    content= paste("<p>","<b>","CENTRAL:","</b>","",Grupos$`Agua cruda`$Central,"</p>",
                   "<p>","<b>","UBICACION:","</b>","",mydata$Punto_Medición,"</p>",
                   "<p>","<b>","TIPO DE MUESTRA:","</b>","",mydata$Recurso,"</p>",
                   "<p>","<b>","ESTE:","</b>","",Grupos$`Agua cruda`$X_coord,"  ","<b>","NORTE:","</b>","",Grupos$`Agua cruda`$Y_coord,"</p>")
    leaflet() %>% addProviderTiles(providers$Esri.WorldImagery,group = "Satelite")%>%addProviderTiles(providers$OpenTopoMap, group = "Topo") %>% 
      addCircleMarkers(data=Grupos$`Agua cruda`, lat = ~Grupos$`Agua cruda`$Lat, lng=Grupos$`Agua cruda`$Long, color=~pal(Recurso), fillOpacity=0.5, label = lapply(content, HTML), group = "Agua cruda" ) %>%
      addCircleMarkers(data=Grupos$`Agua de río`, lat = ~Grupos$`Agua de río`$Lat, lng=Grupos$`Agua de río`$Long, color=~pal(Recurso), fillOpacity=0.5, label = lapply(content, HTML), group = "Agua de río" )%>%
      addCircleMarkers(data=Grupos$`Agua potable`, lat = ~Grupos$`Agua potable`$Lat, lng=Grupos$`Agua potable`$Long, color=~pal(Recurso), fillOpacity=0.5, label = lapply(content, HTML), group = "Agua potable" )%>% 
      addCircleMarkers(data=Grupos$`Agua residual`, lat = ~Grupos$`Agua residual`$Lat, lng=Grupos$`Agua residual`$Long, color=~pal(Recurso), fillOpacity=0.5, label = lapply(content, HTML), group = "Agua residual" )%>% 
      addCircleMarkers(data=Grupos$Lixiviados, lat = ~Grupos$Lixiviados$Lat, lng=Grupos$Lixiviados$Long, color=~pal(Recurso), fillOpacity=0.5, label = lapply(content, HTML), group = "Lixiviados" )%>% 
      addCircleMarkers(data=Grupos$`Suelo`, lat = ~Grupos$`Suelo`$Lat, lng=Grupos$`Suelo`$Long, color=~pal(Recurso), fillOpacity=0.5, label = lapply(content, HTML), group = "Suelo" )%>% 
      addLegend(data=mydata,"bottomright", pal=pal, values=mydata$Recurso, title = "Leyenda", opacity=0.5,group = "Leyenda")%>% 
      addLayersControl( overlayGroups = c("Satelite","Topo","Agua cruda","Agua de río","Agua potable", "Agua residual","Lixiviados","Suelo","Leyenda"), options=layersControlOptions(collapsed = F,labels="Layers",title = "Layers",color="blue"))%>%
      addGraticule(interval = 20,
                   sphere = FALSE,
                   style = list(color = "#333", weight = 1),
                   layerId = NULL,
                   group = NULL,
                   options = pathOptions(pointerEvents = "none", clickable = FALSE))%>%
      addScaleBar()%>% 
      addMeasurePathToolbar(options = measurePathOptions(imperial = T,
                                                         minPixelDistance = 100,
                                                         showDistances = F))%>%
      addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) 
    
  })
  #Tabla del Mapa
  output$Table<-renderDataTable({
    
    datatable(mydata,options=list(scrollX=T,scrollY=T,autoWidth=T,lengthMenu=c(15,30,nrow(mydata))))
    
  })
  #Pestana 2
  observeEvent(input$c_s,{
    req(input$c_s)
    output$pestana_s2<-renderUI({
      if(input$c_s == " "){
        print("selecciona una Central")
      } else {
        s_s1<-Suelohist %>% dplyr::filter(grepl(input$c_s, Suelohist$Central))
        opciones= sort(unique(c(s_s1$Punto_de_medición)))
        selectInput("u_s","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3
  observeEvent(input$u_s,{
    req(input$u_s)
    output$pestana_s3<-renderUI({
      s_s1<-Suelohist %>% dplyr::filter(grepl(input$u_s, Suelohist$Punto_de_medición)&grepl(input$c_s, Suelohist$Central))
      opciones= sort(unique(c(s_s1$Fecha)))
      selectInput("f_s","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  
  #Grafico de suelo
  observeEvent(input$f_s,{
    req(input$f_s)
    output$barras_s= renderGirafe({
      s_s1=Suelohist %>% dplyr::filter(grepl(input$c_s, Suelohist$Central) & grepl(input$u_s, Suelohist$Punto_de_medición) & grepl(input$f_s, Suelohist$Fecha))
      s_s1$parametro_unidad=str_c(s_s1$Parámetro," ",s_s1$Unidad)
      s_s1$id=1:nrow(s_s1)
      d_s=data.frame(s_s1$parametro_unidad,
                     s_s1$Medición) 
      colnames(d_s)[1]="parametro_unidad"
      g_s=ggplot(d_s,aes(x=parametro_unidad,y = s_s1$Medición,tooltip = paste("Medición:",s_s1$Medición), data_id = s_s1$Medición)) +
        geom_hline_interactive(aes(yintercept = s_s1$Límite_máximo, linetype = "Límite máximo",tooltip =paste("L. máximo:",s_s1$Límite_máximo), data_id = s_s1$Límite_máximo), colour = "Red", size=1)+
        geom_hline_interactive(aes(yintercept = s_s1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_s1$Límite_mínimo), data_id = s_s1$id), colour = "Green", size=1) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(size = 0.5, width = 0.5,fill="#0073C2FF")+
        facet_wrap(~d_s$parametro_unidad, ncol = 5, scales = "free")+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=2))
      g_s <- g_s + theme(legend.text=element_text(size=12),legend.title=element_text(face="bold",size=13),axis.title.x = element_text(size=16),axis.title.y = element_text(size=16,angle=90),plot.title = element_text(size=18,face="bold"),axis.text.x=element_blank()) 
      g_s<- g_s + labs(title = "Muestreo por puntos") + ylab("Resultado") + xlab("Parámetro medido")
      g_d= girafe(ggobj = g_s, width_svg = 16, height_svg = 7,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })  
  })
  #Tabla de suelo
  observeEvent(input$f_s,{
    req(input$f_s)
    output$tabla_s=renderDataTable({
      s_s1=Suelohist %>% dplyr::filter(grepl(input$c_s, Suelohist$Central) & grepl(input$u_s, Suelohist$Punto_de_medición) & grepl(input$f_s, Suelohist$Fecha))
      s_s1$Medición=as.numeric(s_s1$Medición)
      s_s1$Límite_mínimo=as.numeric(s_s1$Límite_mínimo)
      s_s1$Límite_máximo=as.numeric(s_s1$Límite_máximo)
      s_s1=mutate(s_s1,Cumplimiento = if_else(s_s1$Medición > s_s1$Límite_mínimo & s_s1$Medición<=s_s1$Límite_máximo,"C","NC"))
      s_s1$ID <- seq.int(nrow(s_s1))
      s_1_1=gather(s_s1, Tipo, Valor, c(10,13,11))
      s_1_2 <- data.frame(s_1_1)
      sparkline_df_2 <- s_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_2 <- merge(s_s1, sparkline_df_2, by = 'ID')
      sparkline_df_2 <- sparkline_df_2[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"  
      
      sl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      
      s_s1 <- datatable(as.data.frame(sparkline_df_2), 
                        rownames = FALSE, 
                        options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                       fnDrawCallback = sl_bar))
      s_s1=s_s1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                  backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
      
    })
  })
  #Pestana 2 agua cruda
  observeEvent(input$c_a_c,{
    req(input$c_a_c)
    output$pestana_ac2<-renderUI({
      if(exists(input$c_a_c)){
        
      } else {
        s_a_c1<-Agua_cruda %>% dplyr::filter(grepl(input$c_a_c, Agua_cruda$Central))
        opciones= sort(unique(c(s_a_c1$Punto_de_medición)))
        selectInput("u_a_c","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua cruda
  observeEvent(input$u_a_c,{
    req(input$u_a_c)
    output$pestana_ac3<-renderUI({
      s_a_c1<-Agua_cruda %>% dplyr::filter(grepl(input$u_a_c, Agua_cruda$Punto_de_medición)&grepl(input$c_a_c, Agua_cruda$Central))
      opciones= sort(unique(c(s_a_c1$Fecha)))
      selectInput("f_a_c","Selecciona una Fecha:",choices = opciones, selected = opciones[1]) 
    })
  })
  #Grafico de agua cruda
  observeEvent(input$f_a_c,{
    req(input$f_a_c)
    output$barras_a_c= renderGirafe({
      s_a_c1=Agua_cruda %>% dplyr::filter(grepl(input$c_a_c, Agua_cruda$Central) & grepl(input$u_a_c, Agua_cruda$Punto_de_medición) & grepl(input$f_a_c, Agua_cruda$Fecha))
      s_a_c1$parametro_unidad=str_c(s_a_c1$Parámetro," ",s_a_c1$Unidad)
      s_a_c1$id=1:nrow(s_a_c1)
      d_s=data.frame(s_a_c1$parametro_unidad,
                     s_a_c1$Medición) 
      colnames(d_s)[1]="parametro_unidad"
      g_s=ggplot(d_s,aes(x=parametro_unidad,y = s_a_c1$Medición,tooltip = paste("Medición:",s_a_c1$Medición), data_id = s_a_c1$Medición)) +
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_máximo, linetype = "Límite máximo",tooltip =paste("L. máximo:",s_a_c1$Límite_máximo), data_id = s_a_c1$Límite_máximo), colour = "Red", size=1)+
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_a_c1$Límite_mínimo), data_id = s_a_c1$id), colour = "Green", size=1) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(size = 0.5, width = 0.5,fill="#0073C2FF")+
        facet_wrap(~d_s$parametro_unidad, ncol = 5, scales = "free")+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=2))
      g_s <- g_s + theme(legend.text=element_text(size=12),legend.title=element_text(face="bold",size=13),axis.title.x = element_text(size=16),axis.title.y = element_text(size=16,angle=90),plot.title = element_text(size=18,face="bold"),axis.text.x=element_blank()) 
      g_s<- g_s + labs(title = "Muestreo por puntos") + ylab("Resultado") + xlab("Parámetro medido")
      g_d= girafe(ggobj = g_s, width_svg = 16, height_svg = 7,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua cruda
  observeEvent(input$f_a_c,{
    req(input$f_a_c)
    output$tabla_a_c=renderDataTable({
      s_a_c1=Agua_cruda %>% dplyr::filter(grepl(input$c_a_c, Agua_cruda$Central) & grepl(input$u_a_c, Agua_cruda$Punto_de_medición) & grepl(input$f_a_c, Agua_cruda$Fecha))
      s_a_c1$Medición=as.numeric(s_a_c1$Medición)
      s_a_c1$Límite_mínimo=as.numeric(s_a_c1$Límite_mínimo)
      s_a_c1$Límite_máximo=as.numeric(s_a_c1$Límite_máximo)
      s_a_c1=mutate(s_a_c1,Cumplimiento = if_else(s_a_c1$Medición > s_a_c1$Límite_mínimo & s_a_c1$Medición<=s_a_c1$Límite_máximo,"C","NC"))
      s_a_c1$ID <- seq.int(nrow(s_a_c1))
      a_c_1_1=gather(s_a_c1, Tipo, Valor, c(10,13,11))
      a_c_1_2 <- data.frame(a_c_1_1)
      sparkline_df_a_c <- a_c_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_a_c <- merge(s_a_c1, sparkline_df_a_c, by = 'ID')
      sparkline_df_a_c <- sparkline_df_a_c[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"  
      
      a_cl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      
      s_a_c1 <- datatable(as.data.frame(sparkline_df_a_c), 
                          rownames = FALSE, 
                          options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                         fnDrawCallback = a_cl_bar))
      s_a_c1=s_a_c1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                      backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #Pestana 2 agua residual
  observeEvent(input$c_a_r,{
    req(input$c_a_r)
    output$pestana_ar2<-renderUI({
      if(input$c_a_r == " "){
        print("selecciona una Central")
      } else {
        s_a_r1<-Agua_residual %>% dplyr::filter(grepl(input$c_a_r, Agua_residual$Central))
        opciones= sort(unique(c(s_a_r1$Punto_de_medición)))
        selectInput("u_a_r","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua residual
  observeEvent(input$u_a_r,{
    req(input$u_a_r)
    output$pestana_ar3<-renderUI({
      s_a_r1<-Agua_residual %>% dplyr::filter(grepl(input$u_a_r, Agua_residual$Punto_de_medición)&grepl(input$c_a_r, Agua_residual$Central))
      opciones= sort(unique(c(s_a_r1$Fecha)))
      selectInput("f_a_r","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  #Grafico de agua residual
  observeEvent(input$f_a_r,{
    req(input$f_a_r)
    output$barras_a_r= renderGirafe({
      s_a_r1=Agua_residual %>% dplyr::filter(grepl(input$c_a_r, Agua_residual$Central) & grepl(input$u_a_r, Agua_residual$Punto_de_medición) & grepl(input$f_a_r, Agua_residual$Fecha))
      s_a_r1$parametro_unidad=str_c(s_a_r1$Parámetro," ",s_a_r1$Unidad)
      s_a_r1$id=1:nrow(s_a_r1)
      d_s=data.frame(s_a_r1$parametro_unidad,
                     s_a_r1$Medición) 
      colnames(d_s)[1]="parametro_unidad"
      g_s=ggplot(d_s,aes(x=parametro_unidad,y = s_a_r1$Medición,tooltip = paste("Medición:",s_a_r1$Medición), data_id = s_a_r1$Medición)) +
        geom_hline_interactive(aes(yintercept = s_a_r1$Límite_máximo, linetype = "Límite máximo",tooltip =paste("L. máximo:",s_a_r1$Límite_máximo), data_id = s_a_r1$Límite_máximo), colour = "Red", size=1)+
        geom_hline_interactive(aes(yintercept = s_a_r1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_a_r1$Límite_mínimo), data_id = s_a_r1$id), colour = "Green", size=1) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(size = 0.5, width = 0.5,fill="#0073C2FF")+
        facet_wrap(~d_s$parametro_unidad, ncol = 5, scales = "free")+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=2))
      g_s <- g_s + theme(legend.text=element_text(size=12),legend.title=element_text(face="bold",size=13),axis.title.x = element_text(size=16),axis.title.y = element_text(size=16,angle=90),plot.title = element_text(size=18,face="bold"),axis.text.x=element_blank()) 
      g_s<- g_s + labs(title = "Muestreo por puntos") + ylab("Resultado") + xlab("Parámetro medido")
      g_d= girafe(ggobj = g_s, width_svg = 16, height_svg = 7,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua residual
  observeEvent(input$f_a_r,{
    req(input$f_a_r)
    output$tabla_a_r=renderDataTable({
      s_a_r1=Agua_residual %>% dplyr::filter(grepl(input$c_a_r, Agua_residual$Central) & grepl(input$u_a_r, Agua_residual$Punto_de_medición) & grepl(input$f_a_r, Agua_residual$Fecha))
      s_a_r1$Medición=as.numeric(s_a_r1$Medición)
      s_a_r1$Límite_mínimo=as.numeric(s_a_r1$Límite_mínimo)
      s_a_r1$Límite_máximo=as.numeric(s_a_r1$Límite_máximo)
      s_a_r1=mutate(s_a_r1,Cumplimiento = if_else(s_a_r1$Medición > s_a_r1$Límite_mínimo & s_a_r1$Medición<=s_a_r1$Límite_máximo,"C","NC"))
      s_a_r1$ID <- seq.int(nrow(s_a_r1))
      a_r_1_1=gather(s_a_r1, Tipo, Valor, c(10,13,11))
      a_r_1_2 <- data.frame(a_r_1_1)
      sparkline_df_a_r <- a_r_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_a_r <- merge(s_a_r1, sparkline_df_a_r, by = 'ID')
      sparkline_df_a_r <- sparkline_df_a_r[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"  
      
      a_rl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      
      s_a_r1 <- datatable(as.data.frame(sparkline_df_a_r), 
                          rownames = FALSE, 
                          options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                         fnDrawCallback = a_rl_bar))
      s_a_r1=s_a_r1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                      backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #Pestana 2 agua potable
  observeEvent(input$c_a_p,{
    req(input$c_a_p)
    output$pestana_ap2<-renderUI({
      if(input$c_a_p == " "){
        print("selecciona una Central")
      } else {
        s_a_p1<-Agua_potable %>% dplyr::filter(grepl(input$c_a_p, Agua_potable$Central))
        opciones= sort(unique(c(s_a_p1$Punto_de_medición)))
        selectInput("u_a_p","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua potable
  observeEvent(input$u_a_p,{
    req(input$u_a_p)
    output$pestana_ap3<-renderUI({
      s_a_p1<-Agua_potable %>% dplyr::filter(grepl(input$u_a_p, Agua_potable$Punto_de_medición) & grepl(input$c_a_p, Agua_potable$Central))
      opciones= sort(unique(c(s_a_p1$Fecha)))
      selectInput("f_a_p","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  #Grafico de agua potable
  observeEvent(input$f_a_p,{
    req(input$f_a_p)
    output$barras_a_p= renderGirafe({
      s_a_p1=Agua_potable %>% dplyr::filter(grepl(input$c_a_p, Agua_potable$Central) & grepl(input$u_a_p, Agua_potable$Punto_de_medición) & grepl(input$f_a_p, Agua_potable$Fecha))
      s_a_p1$parametro_unidad=str_c(s_a_p1$Parámetro," ",s_a_p1$Unidad)
      s_a_p1$id=1:nrow(s_a_p1)
      d_s=data.frame(s_a_p1$parametro_unidad,
                     s_a_p1$Medición) 
      colnames(d_s)[1]="parametro_unidad"
      g_s=ggplot(d_s,aes(x=parametro_unidad,y = s_a_p1$Medición,tooltip = paste("Medición:",s_a_p1$Medición), data_id = s_a_p1$Medición)) +
        geom_hline_interactive(aes(yintercept = s_a_p1$Límite_máximo, linetype = "Límite máximo",tooltip =paste("L. máximo:",s_a_p1$Límite_máximo), data_id = s_a_p1$Límite_máximo), colour = "Red", size=1)+
        geom_hline_interactive(aes(yintercept = s_a_p1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_a_p1$Límite_mínimo), data_id = s_a_p1$id), colour = "Green", size=1) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(size = 0.5, width = 0.5,fill="#0073C2FF")+
        facet_wrap(~d_s$parametro_unidad, ncol = 5, scales = "free")+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=2))
      g_s <- g_s + theme(legend.text=element_text(size=12),legend.title=element_text(face="bold",size=13),axis.title.x = element_text(size=16),axis.title.y = element_text(size=16,angle=90),plot.title = element_text(size=18,face="bold"),axis.text.x=element_blank()) 
      g_s<- g_s + labs(title = "Muestreo por puntos") + ylab("Resultado") + xlab("Parámetro medido")
      g_d= girafe(ggobj = g_s, width_svg = 16, height_svg = 7,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua potable
  observeEvent(input$f_a_p,{
    req(input$f_a_p)
    output$tabla_a_p=renderDataTable({
      s_a_p1=Agua_potable %>% dplyr::filter(grepl(input$c_a_p, Agua_potable$Central) & grepl(input$u_a_p, Agua_potable$Punto_de_medición) & grepl(input$f_a_p, Agua_potable$Fecha))
      s_a_p1$Medición=as.numeric(s_a_p1$Medición)
      s_a_p1$Límite_mínimo=as.numeric(s_a_p1$Límite_mínimo)
      s_a_p1$Límite_máximo=as.numeric(s_a_p1$Límite_máximo)
      s_a_p1=mutate(s_a_p1,Cumplimiento = if_else(s_a_p1$Medición > s_a_p1$Límite_mínimo & s_a_p1$Medición<=s_a_p1$Límite_máximo,"C","NC"))
      s_a_p1$ID <- seq.int(nrow(s_a_p1))
      a_p_1_1=gather(s_a_p1, Tipo, Valor, c(10,13,11))
      a_p_1_2 <- data.frame(a_p_1_1)
      sparkline_df_a_p <- a_p_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_a_p <- merge(s_a_p1, sparkline_df_a_p, by = 'ID')
      sparkline_df_a_p <- sparkline_df_a_p[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"  
      
      a_pl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      
      s_a_p1 <- datatable(as.data.frame(sparkline_df_a_p), 
                          rownames = FALSE, 
                          options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                         fnDrawCallback = a_pl_bar))
      s_a_p1=s_a_p1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                      backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #Pestana 2 lixiviados
  observeEvent(input$c_l,{
    req(input$c_l)
    output$pestana_l2<-renderUI({
      if(input$c_l == " "){
        print("selecciona una Central")
      } else {
        s_l1<-Lixiviados %>% dplyr::filter(grepl(input$c_l, Lixiviados$Central))
        opciones= sort(unique(c(s_l1$Punto_de_medición)))
        selectInput("u_l","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua potable
  observeEvent(input$u_l,{
    req(input$u_l)
    output$pestana_l3<-renderUI({
      s_l1<-Lixiviados  %>% dplyr::filter(grepl(input$u_l, Lixiviados$Punto_de_medición)&grepl(input$c_l, Lixiviados$Central))
      opciones= sort(unique(c(s_l1$Fecha)))
      selectInput("f_l","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  #Grafico de agua potable
  observeEvent(input$f_l,{
    req(input$f_l)
    output$barras_l= renderGirafe({
      s_l1=Lixiviados %>% dplyr::filter(grepl(input$c_l, Lixiviados$Central) & grepl(input$u_l, Lixiviados$Punto_de_medición) & grepl(input$f_l, Lixiviados$Fecha))
      s_l1$parametro_unidad=str_c(s_l1$Parámetro," ",s_l1$Unidad)
      s_l1$id=1:nrow(s_l1)
      d_s=data.frame(s_l1$parametro_unidad,
                     s_l1$Medición) 
      colnames(d_s)[1]="parametro_unidad"
      g_s=ggplot(d_s,aes(x=parametro_unidad,y = s_l1$Medición,tooltip = paste("Medición:",s_l1$Medición), data_id = s_l1$Medición)) +
        geom_hline_interactive(aes(yintercept = s_l1$Límite_máximo, linetype = "Límite máximo",tooltip =paste("L. máximo:",s_l1$Límite_máximo), data_id = s_l1$Límite_máximo), colour = "Red", size=1)+
        geom_hline_interactive(aes(yintercept = s_l1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_l1$Límite_mínimo), data_id = s_l1$id), colour = "Green", size=1) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(size = 0.5, width = 0.5,fill="#0073C2FF")+
        facet_wrap(~d_s$parametro_unidad, ncol = 5, scales = "free")+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=2))
      g_s <- g_s + theme(legend.text=element_text(size=12),legend.title=element_text(face="bold",size=13),axis.title.x = element_text(size=16),axis.title.y = element_text(size=16,angle=90),plot.title = element_text(size=18,face="bold"),axis.text.x=element_blank()) 
      g_s<- g_s + labs(title = "Muestreo por puntos") + ylab("Resultado") + xlab("Parámetro medido")
      g_d= girafe(ggobj = g_s, width_svg = 16, height_svg = 7,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua potable
  observeEvent(input$f_l,{
    req(input$f_l)
    output$tabla_l=renderDataTable({
      s_l1=Lixiviados %>% dplyr::filter(grepl(input$c_l, Lixiviados$Central) & grepl(input$u_l, Lixiviados$Punto_de_medición) & grepl(input$f_l, Lixiviados$Fecha))
      s_l1$Medición=as.numeric(s_l1$Medición)
      s_l1$Límite_mínimo=as.numeric(s_l1$Límite_mínimo)
      s_l1$Límite_máximo=as.numeric(s_l1$Límite_máximo)
      s_l1=mutate(s_l1,Cumplimiento = if_else(s_l1$Medición > s_l1$Límite_mínimo & s_l1$Medición<=s_l1$Límite_máximo,"C","NC"))
      s_l1$ID <- seq.int(nrow(s_l1))
      l_1_1=gather(s_l1, Tipo, Valor, c(10,13,11))
      l_1_2 <- data.frame(l_1_1)
      sparkline_df_l <- l_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_l <- merge(s_l1, sparkline_df_l, by = 'ID')
      sparkline_df_l <- sparkline_df_l[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"  
      
      ll_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      
      s_l1 <- datatable(as.data.frame(sparkline_df_l), 
                        rownames = FALSE, 
                        options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                       fnDrawCallback = ll_bar))
      s_l1=s_l1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                  backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #Pestana 2 agua de rio
  observeEvent(input$c_r,{
    req(input$c_r)
    output$pestana_r2<-renderUI({
      if(input$c_r == " "){
        print("selecciona una Central")
      } else {
        s_r1<-Agua_rio %>% dplyr::filter(grepl(input$c_r, Agua_rio$Central))
        opciones= sort(unique(c(s_r1$Punto_de_medición)))
        selectInput("u_r","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua de rio
  observeEvent(input$u_r,{
    req(input$u_r)
    output$pestana_r3<-renderUI({
      s_r1<-Agua_rio %>% dplyr::filter(grepl(input$u_r, Agua_rio$Punto_de_medición) & grepl(input$c_r, Agua_rio$Central))
      opciones= sort(unique(c(s_r1$Fecha)))
      selectInput("f_r","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  #Grafico de agua de rio
  observeEvent(input$f_r,{
    req(input$f_r)
    output$barras_r= renderGirafe({
      s_r1=Agua_rio %>% dplyr::filter(grepl(input$c_r, Agua_rio$Central) & grepl(input$u_r, Agua_rio$Punto_de_medición) & grepl(input$f_r, Agua_rio$Fecha))
      s_r1$parametro_unidad=str_c(s_r1$Parámetro," ",s_r1$Unidad)
      s_r1$id=1:nrow(s_r1)
      d_s=data.frame(s_r1$parametro_unidad,
                     s_r1$Medición) 
      colnames(d_s)[1]="parametro_unidad"
      g_s=ggplot(d_s,aes(x=parametro_unidad,y = s_r1$Medición,tooltip = paste("Medición:",s_r1$Medición), data_id = s_r1$Medición)) +
        geom_hline_interactive(aes(yintercept = s_r1$Límite_máximo, linetype = "Límite máximo",tooltip =paste("L. máximo:",s_r1$Límite_máximo), data_id = s_r1$Límite_máximo), colour = "Red", size=1)+
        geom_hline_interactive(aes(yintercept = s_r1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_r1$Límite_mínimo), data_id = s_r1$id), colour = "Green", size=1) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(size = 0.5, width = 0.5,fill="#0073C2FF")+
        facet_wrap(~d_s$parametro_unidad, ncol = 5, scales = "free")+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=2))
      g_s <- g_s + theme(legend.text=element_text(size=12),legend.title=element_text(face="bold",size=13),axis.title.x = element_text(size=16),axis.title.y = element_text(size=16,angle=90),plot.title = element_text(size=18,face="bold"),axis.text.x=element_blank()) 
      g_s<- g_s + labs(title = "Muestreo por puntos") + ylab("Resultado") + xlab("Parámetro medido")
      g_d= girafe(ggobj = g_s, width_svg = 16, height_svg = 7,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua de rio
  observeEvent(input$f_r,{
    req(input$f_r)
    output$tabla_r=renderDataTable({
      s_r1=Agua_rio %>% dplyr::filter(grepl(input$c_r, Agua_rio$Central) & grepl(input$u_r, Agua_rio$Punto_de_medición) & grepl(input$f_r, Agua_rio$Fecha))
      s_r1$Medición=as.numeric(s_r1$Medición)
      s_r1$Límite_mínimo=as.numeric(s_r1$Límite_mínimo)
      s_r1$Límite_máximo=as.numeric(s_r1$Límite_máximo)
      s_r1=mutate(s_r1,Cumplimiento = if_else(s_r1$Medición > s_r1$Límite_mínimo & s_r1$Medición<=s_r1$Límite_máximo,"C","NC"))
      s_r1$ID <- seq.int(nrow(s_r1))
      r_1_1=gather(s_r1, Tipo, Valor, c(10,13,11))
      r_1_2 <- data.frame(r_1_1)
      sparkline_df_r <- r_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_r <- merge(s_r1, sparkline_df_r, by = 'ID')
      sparkline_df_r <- sparkline_df_r[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"  
      
      rl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      
      s_r1 <- datatable(as.data.frame(sparkline_df_r), 
                        rownames = FALSE, 
                        options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                       fnDrawCallback = rl_bar))
      s_r1=s_r1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                  backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #########################################Filtrado por parametro#########################################################
  #Pestana 2 agua cruda
  observeEvent(input$c_a_c_2,{
    req(input$c_a_c_2)
    output$pestana_ac2_2<-renderUI({
      if(input$c_a_c_2 == " "){
        print("selecciona una Central")
      } else {
        s_a_c1<-Agua_cruda %>% dplyr::filter(grepl(input$c_a_c_2, Agua_cruda$Central))
        opciones= sort(unique(c(s_a_c1$Punto_de_medición)))
        selectInput("u_a_c_2","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua cruda
  observeEvent(input$u_a_c_2,{
    req(input$u_a_c_2)
    output$pestana_ac3_2<-renderUI({
      s_a_c1<-Agua_cruda %>% dplyr::filter(grepl(input$u_a_c_2, Agua_cruda$Punto_de_medición)&grepl(input$c_a_c_2, Agua_cruda$Central))
      opciones= sort(unique(c(s_a_c1$Parámetro)))
      selectInput("p_a_c","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  
  #Grafico de agua cruda
  observeEvent(input$p_a_c,{
    req(input$p_a_c)
    output$barras_a_c_2= renderGirafe({
      s_a_c1=Agua_cruda %>% dplyr::filter(grepl(input$c_a_c_2, Agua_cruda$Central) & grepl(input$u_a_c_2, Agua_cruda$Punto_de_medición) & grepl(input$p_a_c, Agua_cruda$Parámetro))
      s_a_c1=s_a_c1[with(s_a_c1, order(Fecha)),]
      s_a_c1$parametro_unidad=str_c(s_a_c1$Parámetro," ",s_a_c1$Unidad)
      uni=unique(s_a_c1$parametro_unidad)
      d_s=data.frame(s_a_c1$Fecha,
                     s_a_c1$Medición)  
      colnames(d_s)[1]<-"Fecha"
      dates <- d_s$Fecha[order(as.Date(d_s$Fecha,format="%d/%m/%Y"))]
      d_s$Fecha <- factor(d_s$Fecha, labels = dates,  ordered = T)
      g_s=g_s=ggplot(d_s,aes(x=Fecha,y = s_a_c1$Medición,tooltip =paste("Medición:",s_a_c1$Medición), data_id = Fecha)) +
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_máximo, linetype ="Límite máximo",tooltip =paste("L. máximo:",s_a_c1$Límite_máximo), data_id = s_a_c1$Límite_máximo), colour = "Red", size=0.75, show.legend = TRUE)+
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_a_c1$Límite_mínimo), data_id = s_a_c1$Límite_mínimo), colour = "Green", size=0.75,show.legend = TRUE) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(fill="#0073C2FF", size = 0.5, width = 0.3)+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=1))
      g_s=g_s+theme(legend.text=element_text(size=7),legend.title=element_text(face="bold",size=8),axis.title.x = element_text(size=10),axis.title.y = element_text(size=10,angle=90),plot.title = element_text(size=12,face="bold"),axis.text.x=element_text(color = "black", size=8, angle=30, vjust=.8, hjust=0.8))
      g_s=g_s+ labs(title = s_a_c1$parametro_unidad) + ylab("Resultado") +  xlab("Fecha de Medición")
      g_d= girafe(ggobj = g_s, width_svg = 10, height_svg = 4,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua cruda
  observeEvent(input$p_a_c,{
    req(input$p_a_c)
    output$tabla_a_c_2=renderDataTable({
      s_a_c1=Agua_cruda %>% dplyr::filter(grepl(input$c_a_c_2, Agua_cruda$Central) & grepl(input$u_a_c_2, Agua_cruda$Punto_de_medición) & grepl(input$p_a_c, Agua_cruda$Parámetro))
      s_a_c1=s_a_c1[with(s_a_c1, order(Fecha)),]
      s_a_c1$Medición=as.numeric(s_a_c1$Medición)
      s_a_c1$Límite_mínimo=as.numeric(s_a_c1$Límite_mínimo)
      s_a_c1$Límite_máximo=as.numeric(s_a_c1$Límite_máximo)
      s_a_c1=mutate(s_a_c1,Cumplimiento = if_else(s_a_c1$Medición > s_a_c1$Límite_mínimo & s_a_c1$Medición<=s_a_c1$Límite_máximo,"C","NC"))
      s_a_c1$ID <- seq.int(nrow(s_a_c1))
      s_a_c1=s_a_c1[rev(order(as.Date(s_a_c1$Fecha,format="%d/%m/%Y %H:%M"))),]
      a_c_1_1=gather(s_a_c1, Tipo, Valor, c(10,13,11))
      a_c_1_2 <- data.frame(a_c_1_1)
      sparkline_df_a_c <- a_c_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_a_c <- merge(s_a_c1, sparkline_df_a_c, by = 'ID')
      sparkline_df_a_c <- sparkline_df_a_c[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"      
      
      a_cl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      sparkline_df_a_c=sparkline_df_a_c[order(as.Date(sparkline_df_a_c$Fecha,format="%d/%m/%Y %H:%M")),]
      s_a_c1 <- datatable(as.data.frame(sparkline_df_a_c), 
                          rownames = FALSE, 
                          options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                         fnDrawCallback = a_cl_bar))
      s_a_c1=s_a_c1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                      backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #Pestana 2 agua residual
  observeEvent(input$c_a_r_2,{
    req(input$c_a_r_2)
    output$pestana_ar2_2<-renderUI({
      if(input$c_a_r_2 == " "){
        print("selecciona una Central")
      } else {
        s_a_c1<-Agua_residual %>% dplyr::filter(grepl(input$c_a_r_2, Agua_residual$Central))
        opciones= sort(unique(c(s_a_c1$Punto_de_medición)))
        selectInput("u_a_r_2","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua residual
  observeEvent(input$u_a_r_2,{
    req(input$u_a_r_2)
    output$pestana_ar3_2<-renderUI({
      s_a_c1<-Agua_residual %>% dplyr::filter(grepl(input$u_a_r_2, Agua_residual$Punto_de_medición)&grepl(input$c_a_r_2, Agua_residual$Central))
      opciones= sort(unique(c(s_a_c1$Parámetro)))
      selectInput("p_a_r","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  
  #Grafico de agua residual
  observeEvent(input$p_a_r,{
    req(input$p_a_r)
    output$barras_a_r_2= renderGirafe({
      s_a_c1=Agua_residual %>% dplyr::filter(grepl(input$c_a_r_2, Agua_residual$Central) & grepl(input$u_a_r_2, Agua_residual$Punto_de_medición) & grepl(input$p_a_r, Agua_residual$Parámetro))
      s_a_c1$parametro_unidad=str_c(s_a_c1$Parámetro," ",s_a_c1$Unidad)
      uni=unique(s_a_c1$parametro_unidad)
      d_s=data.frame(s_a_c1$Fecha,
                     s_a_c1$Medición)  
      colnames(d_s)[1]<-"Fecha"
      dates <- d_s$Fecha[order(as.Date(d_s$Fecha,format="%d/%m/%Y"))]
      d_s$Fecha <- factor(d_s$Fecha, labels = dates,  ordered = T)
      g_s=g_s=ggplot(d_s,aes(x=Fecha,y = s_a_c1$Medición,tooltip =paste("Medición:",s_a_c1$Medición), data_id = Fecha)) +
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_máximo, linetype ="Límite máximo",tooltip =paste("L. máximo:",s_a_c1$Límite_máximo), data_id = s_a_c1$Límite_máximo), colour = "Red", size=0.75, show.legend = TRUE)+
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_a_c1$Límite_mínimo), data_id = s_a_c1$Límite_mínimo), colour = "Green", size=0.75,show.legend = TRUE) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(fill="#0073C2FF", size = 0.5, width = 0.3)+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=1))
      g_s=g_s+theme(legend.text=element_text(size=7),legend.title=element_text(face="bold",size=8),axis.title.x = element_text(size=10),axis.title.y = element_text(size=10,angle=90),plot.title = element_text(size=12,face="bold"),axis.text.x=element_text(color = "black", size=8, angle=30, vjust=.8, hjust=0.8))
      g_s=g_s+ labs(title = s_a_c1$parametro_unidad) + ylab("Resultado") +  xlab("Fecha de Medición")
      g_d= girafe(ggobj = g_s, width_svg = 10, height_svg = 4,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua residual
  observeEvent(input$p_a_r,{
    req(input$p_a_r)
    output$tabla_a_r_2=renderDataTable({
      s_a_c1=Agua_residual %>% dplyr::filter(grepl(input$c_a_r_2, Agua_residual$Central) & grepl(input$u_a_r_2, Agua_residual$Punto_de_medición) & grepl(input$p_a_r, Agua_residual$Parámetro))
      s_a_c1$Medición=as.numeric(s_a_c1$Medición)
      s_a_c1$Límite_mínimo=as.numeric(s_a_c1$Límite_mínimo)
      s_a_c1$Límite_máximo=as.numeric(s_a_c1$Límite_máximo)
      s_a_c1=mutate(s_a_c1,Cumplimiento = if_else(s_a_c1$Medición > s_a_c1$Límite_mínimo & s_a_c1$Medición<=s_a_c1$Límite_máximo,"C","NC"))
      s_a_c1$ID <- seq.int(nrow(s_a_c1))
      s_a_c1=s_a_c1[rev(order(as.Date(s_a_c1$Fecha,format="%d/%m/%Y %H:%M"))),]
      a_c_1_1=gather(s_a_c1, Tipo, Valor, c(10,13,11))
      a_c_1_2 <- data.frame(a_c_1_1)
      sparkline_df_a_c <- a_c_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_a_c <- merge(s_a_c1, sparkline_df_a_c, by = 'ID')
      sparkline_df_a_c <- sparkline_df_a_c[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"      
      a_cl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      sparkline_df_a_c=sparkline_df_a_c[order(as.Date(sparkline_df_a_c$Fecha,format="%d/%m/%Y %H:%M")),]
      s_a_c1 <- datatable(as.data.frame(sparkline_df_a_c), 
                          rownames = FALSE, 
                          options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                         fnDrawCallback = a_cl_bar))
      s_a_c1=s_a_c1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                      backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #Pestana 2 agua potable
  observeEvent(input$c_a_p_2,{
    req(input$c_a_p_2)
    output$pestana_ap2_2<-renderUI({
      if(input$c_a_p_2 == " "){
        print("selecciona una Central")
      } else {
        s_a_c1<-Agua_potable %>% dplyr::filter(grepl(input$c_a_p_2, Agua_potable$Central))
        opciones= sort(unique(c(s_a_c1$Punto_de_medición)))
        selectInput("u_a_p_2","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua potable
  observeEvent(input$u_a_p_2,{
    req(input$u_a_p_2)
    output$pestana_ap3_2<-renderUI({
      s_a_c1<-Agua_potable %>% dplyr::filter(grepl(input$u_a_p_2, Agua_potable$Punto_de_medición)&grepl(input$c_a_p_2, Agua_potable$Central))
      opciones= sort(unique(c(s_a_c1$Parámetro)))
      selectInput("p_a_p","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  
  #Grafico de agua potable
  observeEvent(input$p_a_p,{
    req(input$p_a_p)
    output$barras_a_p_2= renderGirafe({
      s_a_c1=Agua_potable %>% dplyr::filter(grepl(input$c_a_p_2, Agua_potable$Central) & grepl(input$u_a_p_2, Agua_potable$Punto_de_medición) & grepl(input$p_a_p, Agua_potable$Parámetro))
      s_a_c1$parametro_unidad=str_c(s_a_c1$Parámetro," ",s_a_c1$Unidad)
      uni=unique(s_a_c1$parametro_unidad)
      d_s=data.frame(s_a_c1$Fecha,
                     s_a_c1$Medición)  
      colnames(d_s)[1]<-"Fecha"
      dates <- d_s$Fecha[order(as.Date(d_s$Fecha,format="%d/%m/%Y"))]
      d_s$Fecha <- factor(d_s$Fecha, labels = dates,  ordered = T)
      g_s=ggplot(d_s,aes(x=Fecha,y = s_a_c1$Medición,tooltip =paste("Medición:",s_a_c1$Medición), data_id = Fecha)) +
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_máximo, linetype ="Límite máximo",tooltip =paste("L. máximo:",s_a_c1$Límite_máximo), data_id = s_a_c1$Límite_máximo), colour = "Red", size=0.75, show.legend = TRUE)+
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_a_c1$Límite_mínimo), data_id = s_a_c1$Límite_mínimo), colour = "Green", size=0.75,show.legend = TRUE) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(fill="#0073C2FF", size = 0.5, width = 0.3)+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=1))
      g_s=g_s+theme(legend.text=element_text(size=7),legend.title=element_text(face="bold",size=8),axis.title.x = element_text(size=10),axis.title.y = element_text(size=10,angle=90),plot.title = element_text(size=12,face="bold"),axis.text.x=element_text(color = "black", size=8, angle=30, vjust=.8, hjust=0.8))
      g_s=g_s+ labs(title = s_a_c1$parametro_unidad) + ylab("Resultado") +  xlab("Fecha de Medición")
      g_d= girafe(ggobj = g_s, width_svg = 10, height_svg = 4,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua potable
  observeEvent(input$p_a_p,{
    req(input$p_a_p)
    output$tabla_a_p_2=renderDataTable({
      s_a_c1=Agua_potable %>% dplyr::filter(grepl(input$c_a_p_2, Agua_potable$Central) & grepl(input$u_a_p_2, Agua_potable$Punto_de_medición) & grepl(input$p_a_p, Agua_potable$Parámetro))
      s_a_c1$Medición=as.numeric(s_a_c1$Medición)
      s_a_c1$Límite_mínimo=as.numeric(s_a_c1$Límite_mínimo)
      s_a_c1$Límite_máximo=as.numeric(s_a_c1$Límite_máximo)
      s_a_c1=mutate(s_a_c1,Cumplimiento = if_else(s_a_c1$Medición > s_a_c1$Límite_mínimo & s_a_c1$Medición<=s_a_c1$Límite_máximo,"C","NC"))
      s_a_c1$ID <- seq.int(nrow(s_a_c1))
      s_a_c1=s_a_c1[rev(order(as.Date(s_a_c1$Fecha,format="%d/%m/%Y %H:%M"))),]
      a_c_1_1=gather(s_a_c1, Tipo, Valor, c(10,13,11))
      a_c_1_2 <- data.frame(a_c_1_1)
      sparkline_df_a_c <- a_c_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_a_c <- merge(s_a_c1, sparkline_df_a_c, by = 'ID')
      sparkline_df_a_c <- sparkline_df_a_c[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"      
      a_cl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      
      #s_a_c1=my_data %>% arrange(desc(Sepal.Length))
      sparkline_df_a_c=sparkline_df_a_c[order(as.Date(sparkline_df_a_c$Fecha,format="%d/%m/%Y %H:%M")),]
      s_a_c1 <- datatable(as.data.frame(sparkline_df_a_c), 
                          rownames = FALSE, 
                          options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                         fnDrawCallback = a_cl_bar))
      
      s_a_c1=s_a_c1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                      backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
      
      
    }) 
  })
  #Pestana 2 agua rio
  observeEvent(input$c_rio_2,{
    req(input$c_rio_2)
    output$pestana_rio2_2<-renderUI({
      if(input$c_rio_2 == " "){
        print("selecciona una Central")
      } else {
        s_a_c1<-Agua_rio %>% dplyr::filter(grepl(input$c_rio_2, Agua_rio$Central))
        opciones= sort(unique(c(s_a_c1$Punto_de_medición)))
        selectInput("u_a_rio_2","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 agua rio
  observeEvent(input$u_a_rio_2,{
    req(input$u_a_rio_2)
    output$pestana_rio3_2<-renderUI({
      s_a_c1<-Agua_rio %>% dplyr::filter(grepl(input$u_a_rio_2, Agua_rio$Punto_de_medición)&grepl(input$c_rio_2, Agua_rio$Central))
      opciones= sort(unique(c(s_a_c1$Parámetro)))
      selectInput("p_a_rio","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  
  #Grafico de agua rio
  observeEvent(input$p_a_rio,{
    req(input$p_a_rio)
    output$barras_rio_2= renderGirafe({
      s_a_c1=Agua_rio %>% dplyr::filter(grepl(input$c_rio_2, Agua_rio$Central) & grepl(input$u_a_rio_2, Agua_rio$Punto_de_medición) & grepl(input$p_a_rio, Agua_rio$Parámetro))
      s_a_c1$parametro_unidad=str_c(s_a_c1$Parámetro," ",s_a_c1$Unidad)
      uni=unique(s_a_c1$parametro_unidad)
      d_s=data.frame(s_a_c1$Fecha,
                     s_a_c1$Medición)  
      colnames(d_s)[1]<-"Fecha"
      dates <- d_s$Fecha[order(as.Date(d_s$Fecha,format="%d/%m/%Y"))]
      d_s$Fecha <- factor(d_s$Fecha, labels = dates,  ordered = T)
      g_s=g_s=ggplot(d_s,aes(x=Fecha,y = s_a_c1$Medición,tooltip =paste("Medición:",s_a_c1$Medición), data_id = Fecha)) +
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_máximo, linetype ="Límite máximo",tooltip =paste("L. máximo:",s_a_c1$Límite_máximo), data_id = s_a_c1$Límite_máximo), colour = "Red", size=0.75, show.legend = TRUE)+
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_a_c1$Límite_mínimo), data_id = s_a_c1$Límite_mínimo), colour = "Green", size=0.75,show.legend = TRUE) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(fill="#0073C2FF", size = 0.5, width = 0.3)+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=1))
      g_s=g_s+theme(legend.text=element_text(size=7),legend.title=element_text(face="bold",size=8),axis.title.x = element_text(size=10),axis.title.y = element_text(size=10,angle=90),plot.title = element_text(size=12,face="bold"),axis.text.x=element_text(color = "black", size=8, angle=30, vjust=.8, hjust=0.8))
      g_s=g_s+ labs(title = s_a_c1$parametro_unidad) + ylab("Resultado") +  xlab("Fecha de Medición")
      g_d= girafe(ggobj = g_s, width_svg = 10, height_svg = 4,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de agua potable
  observeEvent(input$p_a_rio,{
    req(input$p_a_rio)
    output$tabla_rio_2=renderDataTable({
      s_a_c1=Agua_rio %>% dplyr::filter(grepl(input$c_rio_2, Agua_rio$Central) & grepl(input$u_a_rio_2, Agua_rio$Punto_de_medición) & grepl(input$p_a_rio, Agua_rio$Parámetro))
      s_a_c1$Medición=as.numeric(s_a_c1$Medición)
      s_a_c1$Límite_mínimo=as.numeric(s_a_c1$Límite_mínimo)
      s_a_c1$Límite_máximo=as.numeric(s_a_c1$Límite_máximo)
      s_a_c1=mutate(s_a_c1,Cumplimiento = if_else(s_a_c1$Medición > s_a_c1$Límite_mínimo & s_a_c1$Medición<=s_a_c1$Límite_máximo,"C","NC"))
      s_a_c1$ID <- seq.int(nrow(s_a_c1))
      s_a_c1=s_a_c1[rev(order(as.Date(s_a_c1$Fecha,format="%d/%m/%Y %H:%M"))),]
      a_c_1_1=gather(s_a_c1, Tipo, Valor, c(10,13,11))
      a_c_1_2 <- data.frame(a_c_1_1)
      sparkline_df_a_c <- a_c_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_a_c <- merge(s_a_c1, sparkline_df_a_c, by = 'ID')
      sparkline_df_a_c <- sparkline_df_a_c[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"      
      a_cl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      sparkline_df_a_c=sparkline_df_a_c[order(as.Date(sparkline_df_a_c$Fecha,format="%d/%m/%Y %H:%M")),]
      s_a_c1 <- datatable(as.data.frame(sparkline_df_a_c), 
                          rownames = FALSE, 
                          options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                         fnDrawCallback = a_cl_bar))
      s_a_c1=s_a_c1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                      backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #Pestana 2 lixiviados 2
  observeEvent(input$c_l_2,{
    req(input$c_l_2)
    output$pestana_l2_2<-renderUI({
      if(input$c_l_2 == " "){
        print("selecciona una Central")
      } else {
        s_a_c1<-Lixiviados %>% dplyr::filter(grepl(input$c_l_2, Lixiviados$Central))
        opciones= sort(unique(c(s_a_c1$Punto_de_medición)))
        selectInput("u_a_l_2","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 lixiviados 2
  observeEvent(input$u_a_l_2,{
    req(input$u_a_l_2)
    output$pestana_l3_2<-renderUI({
      s_a_c1<-Lixiviados %>% dplyr::filter(grepl(input$u_a_l_2, Lixiviados$Punto_de_medición)&grepl(input$c_l_2, Lixiviados$Central))
      opciones= sort(unique(c(s_a_c1$Parámetro)))
      selectInput("p_a_l","Selecciona una Fecha:",choices = opciones, selected = opciones[1])
    })
  })
  
  #Grafico de lixiviados 2
  observeEvent(input$p_a_l,{
    req(input$p_a_l)
    output$barras_l_2= renderGirafe({
      s_a_c1=Lixiviados %>% dplyr::filter(grepl(input$c_l_2, Lixiviados$Central) & grepl(input$u_a_l_2, Lixiviados$Punto_de_medición) & grepl(input$p_a_l, Lixiviados$Parámetro))
      s_a_c1$parametro_unidad=str_c(s_a_c1$Parámetro," ",s_a_c1$Unidad)
      uni=unique(s_a_c1$parametro_unidad)
      d_s=data.frame(s_a_c1$Fecha,
                     s_a_c1$Medición)  
      colnames(d_s)[1]<-"Fecha"
      dates <- d_s$Fecha[order(as.Date(d_s$Fecha,format="%d/%m/%Y"))]
      d_s$Fecha <- factor(d_s$Fecha, labels = dates,  ordered = T)
      g_s=g_s=ggplot(d_s,aes(x=Fecha,y = s_a_c1$Medición,tooltip =paste("Medición:",s_a_c1$Medición), data_id = Fecha)) +
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_máximo, linetype ="Límite máximo",tooltip =paste("L. máximo:",s_a_c1$Límite_máximo), data_id = s_a_c1$Límite_máximo), colour = "Red", size=0.75, show.legend = TRUE)+
        geom_hline_interactive(aes(yintercept = s_a_c1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_a_c1$Límite_mínimo), data_id = s_a_c1$Límite_mínimo), colour = "Green", size=0.75,show.legend = TRUE) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(fill="#0073C2FF", size = 0.5, width = 0.3)+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=1))
      g_s=g_s+theme(legend.text=element_text(size=7),legend.title=element_text(face="bold",size=8),axis.title.x = element_text(size=10),axis.title.y = element_text(size=10,angle=90),plot.title = element_text(size=12,face="bold"),axis.text.x=element_text(color = "black", size=8, angle=30, vjust=.8, hjust=0.8))
      g_s=g_s+ labs(title = s_a_c1$parametro_unidad) + ylab("Resultado") +  xlab("Fecha de Medición")
      g_d= girafe(ggobj = g_s, width_svg = 10, height_svg = 4,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de lixiviados 2
  observeEvent(input$p_a_l,{
    req(input$p_a_l)
    output$tabla_l_2=renderDataTable({
      s_a_c1=Lixiviados %>% dplyr::filter(grepl(input$c_l_2, Lixiviados$Central) & grepl(input$u_a_l_2, Lixiviados$Punto_de_medición) & grepl(input$p_a_l, Lixiviados$Parámetro))
      s_a_c1$Medición=as.numeric(s_a_c1$Medición)
      s_a_c1$Límite_mínimo=as.numeric(s_a_c1$Límite_mínimo)
      s_a_c1$Límite_máximo=as.numeric(s_a_c1$Límite_máximo)
      s_a_c1=mutate(s_a_c1,Cumplimiento = if_else(s_a_c1$Medición > s_a_c1$Límite_mínimo & s_a_c1$Medición<=s_a_c1$Límite_máximo,"C","NC"))
      s_a_c1$ID <- seq.int(nrow(s_a_c1))
      s_a_c1=s_a_c1[rev(order(as.Date(s_a_c1$Fecha,format="%d/%m/%Y %H:%M"))),]
      a_c_1_1=gather(s_a_c1, Tipo, Valor, c(10,13,11))
      a_c_1_2 <- data.frame(a_c_1_1)
      sparkline_df_a_c <- a_c_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_a_c <- merge(s_a_c1, sparkline_df_a_c, by = 'ID')
      sparkline_df_a_c <- sparkline_df_a_c[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"      
      a_cl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      sparkline_df_a_c=sparkline_df_a_c[order(as.Date(sparkline_df_a_c$Fecha,format="%d/%m/%Y %H:%M")),]
      s_a_c1 <- datatable(as.data.frame(sparkline_df_a_c), 
                          rownames = FALSE, 
                          options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                         fnDrawCallback = a_cl_bar))
      s_a_c1=s_a_c1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                      backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
    })
  })
  #Pestana 2 suelo
  observeEvent(input$c_s_p,{
    req(input$c_s_p)
    output$pestana_s_p2<-renderUI({
      if(input$c_s_p == " "){
        print("selecciona una Central")
      } else {
        s_s1<-Suelohist %>% dplyr::filter(grepl(input$c_s_p, Suelohist$Central))
        opciones= sort(unique(c(s_s1$Punto_de_medición)))
        selectInput("u_s_p","Selecciona un punto:",choices = opciones, selected = opciones[1])
      }
    })
  })
  #Pestana 3 suelo
  observeEvent(input$u_s_p,{
    req(input$u_s_p)
    output$pestana_s_p3<-renderUI({
      s_s1<-Suelohist %>% dplyr::filter(grepl(input$u_s_p, Suelohist$Punto_de_medición)&grepl(input$c_s_p, Suelohist$Central))
      opciones= sort(unique(c(s_s1$Parámetro)))
      selectInput("p_s_p","Selecciona un Parámetro:",choices = opciones, selected = opciones[1])
    })
  })
  #Grafico de suelo2
  observeEvent(input$p_s_p,{
    req(input$p_s_p)
    output$barras_s2= renderGirafe({
      
      s_s1=Suelohist %>% dplyr::filter(grepl(input$c_s_p, Suelohist$Central) & grepl(input$u_s_p, Suelohist$Punto_de_medición) & grepl(input$p_s_p, Suelohist$Parámetro))
      s_s1$parametro_unidad=str_c(s_s1$Parámetro," ",s_s1$Unidad)
      uni=unique(s_s1$parametro_unidad)
      d_s=data.frame(s_s1$Fecha,
                     s_s1$Medición)  
      colnames(d_s)[1]<-"Fecha"
      dates <- d_s$Fecha[order(as.Date(d_s$Fecha,format="%d/%m/%Y"))]
      d_s$Fecha <- factor(d_s$Fecha, labels = dates,  ordered = T)
      g_s=g_s=ggplot(d_s,aes(x=Fecha,y = s_s1$Medición,tooltip =paste("Medición:",s_s1$Medición), data_id = Fecha)) +
        geom_hline_interactive(aes(yintercept = s_s1$Límite_máximo, linetype ="Límite máximo",tooltip =paste("L. máximo:",s_s1$Límite_máximo), data_id = s_s1$Límite_máximo), colour = "Red", size=0.75, show.legend = TRUE)+
        geom_hline_interactive(aes(yintercept = s_s1$Límite_mínimo, linetype = "Límite mínimo",tooltip =paste("L. mínimo:",s_s1$Límite_mínimo), data_id = s_s1$Límite_mínimo), colour = "Green", size=0.75,show.legend = TRUE) + 
        guides(linetype=guide_legend(override.aes=list(colour = c("red","green"))))+ 
        geom_col_interactive(fill="#0073C2FF", size = 0.5, width = 0.3)+
        scale_linetype_manual(name = "Límites", values = c("Límite máximo" = 1, "Límite mínimo" = 1),guide = "legend")+
        guides(fill = guide_legend(ncol=1))
      g_s=g_s+theme(legend.text=element_text(size=7),legend.title=element_text(face="bold",size=8),axis.title.x = element_text(size=10),axis.title.y = element_text(size=10,angle=90),plot.title = element_text(size=12,face="bold"),axis.text.x=element_text(color = "black", size=8, angle=30, vjust=.8, hjust=0.8))
      g_s=g_s+ labs(title = s_s1$parametro_unidad) + ylab("Resultado") +  xlab("Fecha de Medición")
      g_d= girafe(ggobj = g_s, width_svg = 10, height_svg = 4,options = list(opts_selection(type = "single", only_shiny = FALSE)))
    })
  })
  #Tabla de suelo2
  observeEvent(input$p_s_p,{
    req(input$p_s_p)
    output$tabla_s2=renderDataTable({
      s_s1=Suelohist %>% dplyr::filter(grepl(input$c_s_p, Suelohist$Central) & grepl(input$u_s_p, Suelohist$Punto_de_medición) & grepl(input$p_s_p, Suelohist$Parámetro))
      s_s1$Medición=as.numeric(s_s1$Medición)
      s_s1$Límite_mínimo=as.numeric(s_s1$Límite_mínimo)
      s_s1$Límite_máximo=as.numeric(s_s1$Límite_máximo)
      s_s1=mutate(s_s1,Cumplimiento = if_else(s_s1$Medición > s_s1$Límite_mínimo & s_s1$Medición<=s_s1$Límite_máximo,"C","NC"))
      s_s1$ID <- seq.int(nrow(s_s1))
      
      s_1_1=gather(s_s1, Tipo, Valor, c(10,13,11))
      s_1_2 <- data.frame(s_1_1)
      sparkline_df_2 <- s_1_2 %>%
        group_by(ID) %>%
        summarize(v_his = paste0(Valor, collapse = ",")) 
      sparkline_df_2 <- merge(s_s1, sparkline_df_2, by = 'ID')
      sparkline_df_2 <- sparkline_df_2[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,16,15)]
      colDefs <- list(list(className = 'dt-center',targets = c(14), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      
      bar_string <- "type: 'bar', colorMap: ['#00CC00','#0033FF', '#ff2e2eff'], width: 50, height: 25, barWidth: 20, barSpacing:5, highlightColor: 'orange', tooltipFormat: '{{offset:levels}} : {{value}}', tooltipValueLookups: { levels: { '0':'Límite_mínimo', '1':'Medición', '2': 'Límite_máximo' }}"  
      
      sl_bar <- JS(sprintf("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', {%s})}", bar_string)) 
      sparkline_df_2=sparkline_df_2[order(as.Date(sparkline_df_2$Fecha,format="%d/%m/%Y %H:%M")),]
      s_s1 <- datatable(as.data.frame(sparkline_df_2), 
                        rownames = FALSE, 
                        options = list(lengthMenu=c(5,10,15),pageLength=10,autoWidth=T,scrollX = TRUE,columnDefs = colDefs, 
                                       fnDrawCallback = sl_bar))
      s_s1=s_s1 %>%   formatStyle("Cumplimiento", target = 'row', 
                                  backgroundColor = styleEqual(c("NC","C"), c("#FFA54A", "white")))
      
    })
  })
  #Tablas descargas
  
  
  #Pestana 2 descarga
  observeEvent(input$c_des,{
    req(input$c_des)
    output$pestana_des2<-renderUI({
      s_s1<-base %>% dplyr::filter(grepl(input$c_des, base$Central))
      opciones= sort(unique(c(s_s1$Recurso)))
      selectInput("r_des","Selecciona un recurso:",choices = opciones, selected = opciones[1])
    })
  })
  #Pestana 3 descarga
  observeEvent(input$r_des,{
    req(input$r_des)
    output$pestana_des3<-renderUI({
      s_s1<-base %>% dplyr::filter(grepl(input$c_des, base$Central)&grepl(input$r_des, base$Recurso))
      opciones= sort(unique(c(s_s1$Frecuencia)))
      selectInput("f_des","Selecciona la frecuencia:",choices = opciones, selected = opciones[1])
    })
  })
  #Pestana 4 descarga
  observeEvent(input$f_des,{
    req(input$f_des)
    output$pestana_des4<-renderUI({
      s_s1<-base %>% dplyr::filter(grepl(input$c_des, base$Central)&grepl(input$r_des, base$Recurso)&grepl(input$f_des, base$Frecuencia))
      anos=as.POSIXct(s_s1$Fecha,format="%d/%m/%Y %H:%M")
      years=format(anos,format="%Y")
      opciones= sort(unique(c(years)))
      selectInput("a_des","Selecciona un año:",choices = opciones, selected = opciones[1])
    })
  })
  
  #Graficos Descarga
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename<- function(){
      paste("Reporte",input$r_des,Sys.Date(),switch(
        input$filetype, PDF = ".pdf", Word = ".docx"
      ),sep = " ")
    },
    
    content = function(file) {
      if (input$filetype=="PDF"){
        #### Progressing indicator
        withProgress(message = 'Descargando...', value = 0, {
          for (i in 1:15) {
            incProgress(1/15)
            Sys.sleep(0.01)
          }
          td <- tempdir()
          ## End of progression
          src <- normalizePath('www/report.Rmd')
          tempLogo <- normalizePath( "Ecu-celec.png")
          tempLogo2 <- normalizePath( "datos.png")
          tempLogo3 <- normalizePath( "pie.png")
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'www/report.Rmd', overwrite = TRUE)
          file.copy(tempLogo,'Ecu-celec.png', overwrite = TRUE)
          file.copy(tempLogo2,'datos.png', overwrite = TRUE)
          file.copy(tempLogo3,'pie.png', overwrite = TRUE)
          params <- list(g_1 = input$c_des, g_2 = input$r_des, g_3 = input$f_des, g_4 = input$a_des)
          
          library(rmarkdown)
          out <- render('www/report.Rmd', params = params, pdf_document())
          file.rename(out, file)
          
        })
        ### below is the end of pdf content
      }else{
        withProgress(message = 'Descargando...', value = 0, {
          for (i in 1:15) {
            incProgress(1/15)
            Sys.sleep(0.01)
          }
          
          ## End of progression
          src <- normalizePath('www/report.Rmd')
          
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'www/report.Rmd', overwrite = TRUE)
          
          params <- list(g_1 = input$c_des, g_2 = input$r_des, g_3 = input$f_des, g_4 = input$a_des)
          
          library(rmarkdown)
          out <- render('www/report.Rmd', params = params, word_document())
          file.rename(out, file)
        })
      }
      
    }
  )
}

#tinytex::install_tinytex()
