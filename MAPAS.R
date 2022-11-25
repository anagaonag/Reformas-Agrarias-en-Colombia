library(haven)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library("RColorBrewer")
library(wesanderson)
library(rgdal)
library(raster)
library(dplyr)
library(spData)
library(stringr)
library(tidyr)
library(readxl)

setwd("C:/Users/LENOVO/Desktop/ESTUDIO/Maestría en Economía-UROSARIO/Médotodos Computacionales para Políticas Públicas/PROYECTO/MAPAS")
getwd()
#Lectura de la base de datos

victimas=read.csv("C:/Users/LENOVO/Desktop/ESTUDIO/Maestría en Economía-UROSARIO/Médotodos Computacionales para Políticas Públicas/PROYECTO/bases/victimas.csv")
desplazamiento=read.csv("C:/Users/LENOVO/Desktop/ESTUDIO/Maestría en Economía-UROSARIO/Médotodos Computacionales para Políticas Públicas/PROYECTO/bases/desplazamientoforzado.csv")
violencia=read.csv("C:/Users/LENOVO/Desktop/ESTUDIO/Maestría en Economía-UROSARIO/Médotodos Computacionales para Políticas Públicas/PROYECTO/bases/violencia.csv")
restitucion=read.csv("C:/Users/LENOVO/Desktop/ESTUDIO/Maestría en Economía-UROSARIO/Médotodos Computacionales para Políticas Públicas/PROYECTO/bases/restitución.csv")


mapa=st_read("C:/Users/LENOVO/Desktop/ESTUDIO/Maestría en Economía-UROSARIO/Médotodos Computacionales para Políticas Públicas/PROYECTO/MAPAS//mpio/mpio.shp")



#Se homogeniza la variable para el merge
colnames(mapa)[colnames(mapa) == "MPIOS"] = "CodigoDANE"
colnames(desplazamiento)[colnames(desplazamiento) == "Codigo_Muniicipio"] = "CodigoDANE"
colnames(violencia)[colnames(violencia) == "Codigo.DANE.de.Municipio"] = "CodigoDANE"
colnames(victimas)[colnames(victimas) == "Cod.en.texto"] = "CodigoDANE"

#Se garantiza que todos los valores sean iguales para el merge
mapa$CodigoDANE=str_remove(mapa$CodigoDANE, "^0+")
mapa$CodigoDANE=as.numeric(mapa$CodigoDANE)

desplazamiento$CodigoDANE=str_remove(desplazamiento$CodigoDANE, "^0+")
desplazamiento$CodigoDANE=as.numeric(desplazamiento$CodigoDANE)

violencia$CodigoDANE=str_remove(violencia$CodigoDANE, "^0+")
violencia$CodigoDANE=as.numeric(violencia$CodigoDANE)

victimas$CodigoDANE=str_remove(victimas$CodigoDANE, "^0+")
victimas$CodigoDANE=as.numeric(victimas$CodigoDANE)



#VICTIMAS
victimas_TOTAL=aggregate(x=victimas$No..Víctimas.por.Declaración,by=list(victimas$CodigoDANE),FUN=sum)
victimas_TOTAL1=aggregate(x=victimas$Desplazamiento.Forzado,by=list(victimas$CodigoDANE),FUN=sum)

victimas_TOTAL2=aggregate(x=victimas$Desaparición.forzada,by=list(victimas$CodigoDANE),FUN=sum)
victimas_TOTAL3=aggregate(x=victimas$Abandono.o.Despojo.Forzado.de.Tierras,by=list(victimas$CodigoDANE),FUN=sum)
victimas_TOTAL4=aggregate(x=victimas$Vincuación.de.Niños..Niñas.y.Adolescentes.a.actividades.relacionadas.con.grupos.armados,by=list(victimas$CodigoDANE),FUN=sum)


colnames(victimas_TOTAL)=c('CodigoDANE','Número de victimas')
colnames(victimas_TOTAL1)=c('CodigoDANE','Desplazamiento forzado')
colnames(victimas_TOTAL2)=c('CodigoDANE','Desaparición forzada')
colnames(victimas_TOTAL3)=c('CodigoDANE','Despojo forzado de tierras')
colnames(victimas_TOTAL4)=c('CodigoDANE','vinculación de niños al conflicto')

map_victimas=inner_join(mapa,victimas_TOTAL)
map_victimas1=inner_join(mapa,victimas_TOTAL1)
map_victimas2=inner_join(mapa,victimas_TOTAL2)
map_victimas3=inner_join(mapa,victimas_TOTAL3)
map_victimas4=inner_join(mapa,victimas_TOTAL4)

tm_shape(map_victimas)+
  tm_fill(col="Número de victimas",title="Victimas 2017",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,1000, 5000,10000, 40000,80000,720000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1.2,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.01,0.13),legend.width=0.8,legend.height=1)


tm_shape(map_victimas1)+
  tm_fill(col="Desplazamiento forzado",title=" Desplazamiento forzado 2017",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,400, 1000,5000, 10000,20000,700000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1.2,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.01,0.13),legend.width=0.8,legend.height=1)


tm_shape(map_victimas2)+
  tm_fill(col="Desaparición forzada",title=" Desaparición forzada 2017",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,10, 20,50, 70,100,30000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.02,0.13),legend.width=1,legend.height=1)

tm_shape(map_victimas3)+
  tm_fill(col="Despojo forzado de tierras",title="Despojo forzado de tierras",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,1, 2,3, 4,5,70000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.02,0.13),legend.width=1,legend.height=1)

tm_shape(map_victimas4)+
  tm_fill(col="vinculación de niños al conflicto",title="vinculación de niños al conflicto 2017",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,3, 10,25,50, 100,4000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.02,0.13),legend.width=1,legend.height=1)

#DESPLAZAMIENTO EN EL TIEMPO
map_DESPLAZAMINETO=inner_join(mapa,desplazamiento)

tm_shape(map_DESPLAZAMINETO)+
  tm_fill(col="Periodo_1980_1988",title="Desplazamiento 1980-1988 ",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,3, 10,25,50, 100,6000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.02,0.13),legend.width=1,legend.height=1)

tm_shape(map_DESPLAZAMINETO)+
  tm_fill(col="Periodo_1989_1996",title="Desplazamiento 1989-1996 ",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,10,50, 100,500,1000,50000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.02,0.13),legend.width=1,legend.height=1)

tm_shape(map_DESPLAZAMINETO)+
  tm_fill(col="Periodo_1997_2004",title="Desplazamiento 1997-2004 ",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,200,1000,3000, 5000,10000,18000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.02,0.13),legend.width=1,legend.height=1)

#RESTITUCIÓN
map_RESTITUC=inner_join(mapa,restitucion)
tm_shape(map_RESTITUC)+
  tm_fill(col="NumeroDeSolicitudes",title="Numero de Solicitudes",
          palette=c("#339900","#cccc00","#ffff33","#ffcc00","#ff6600","#cc0000"),
          breaks = c(0,50,100,300,500,1000,3000),legend.show = TRUE,sizes.legend=0.21,legend.hist = FALSE)+
  tm_borders(col="black", lwd=0.3)+tm_layout(legend.title.size =1,
                                             legend.position=c("left","top"),scale=0.8,
                                             inner.margins = c(0.02,0.13),legend.width=1,legend.height=1)
#violencia

violencia_1991_2001=violencia[violencia$Ano %in% c(1991, 1992, 1992,1993,1994,1995,1996,1997,1998,1999,
                                                   2000,2001), ]
violencia_2004_2010=violencia[violencia$Ano %in% c(2002, 2003,2004 ,2005,2006,2007,2008,2009,
                                                   2010), ]
violencia_2011_2022=violencia[violencia$Ano %in% c(2012, 2013,2014 ,2015,2016,2017,2018,2019,
                                                   2020,2021,2022), ]


violencia_1991_2001=aggregate(x=violencia_1991_2001$Total.de.Víctimas.del.Caso,by=list(violencia_1991_2001$CodigoDANE),FUN=sum)

violencia_2004_2010=aggregate(x=violencia_2004_2010$Total.de.Víctimas.del.Caso,by=list(violencia_2004_2010$CodigoDANE),FUN=sum)

violencia_2011_2022=aggregate(x=violencia_2011_2022$Total.de.Víctimas.del.Caso,by=list(violencia_2011_2022$CodigoDANE),FUN=sum)

map_violencia1=inner_join(mapa,violencia_1991_2001)
map_violencia2=inner_join(mapa,violencia_2004_2010)
map_violencia3=inner_join(mapa,violencia_2011_2022)
