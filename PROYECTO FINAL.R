library(remotes)

remotes::install_github("r-spatial/rgee") 

library(reticulate)
use_python("Ubicacion de miniconda")
reticulate:: py_config()


library(rgee) 
ee_clean_user_credentials('Usuaeio de GEE')
ee_users()
ee_Authenticate() 

ee_Initialize('Usuario de GEE',drive = T)
library(googledrive)



############################################################################33
#DELIMITACION DEL AREA DE ESTUDIO


library(mapedit)
roi <- mapedit::editMap()
roi_ee <- roi %>% sf_as_ee()
#ahora vamos a extraer la geometria del shape
Map$centerObject(roi_ee)
Map$addLayer(roi_ee)

puntos<- ee$Geometry$Point(-73.69 , 10.83)
##############################################################################3
#parametros devisualizacion del indice de nieve

paleta_de_colores <- colorRampPalette(c('white','blue'))
vizualizacion <- list(min= -0.5 , max = 0.5 , palette = paleta_de_colores(2))


##################################################################################
#EXTRACCION DE IMAGENES Y FILTRADO DE LAS MISMAS

#datos para 1987-2022

Landsat_4 <- ee$ImageCollection("LANDSAT/LT04/C02/T1_L2")$
  filterDate('1988-01-01', '1988-12-31')$
  filterBounds(ee$Geometry$Point(-73.69 , 10.83))$
  sort('CLOUD_COVER')$
  first()$
  clip(roi_ee)


Landsat_5 <- ee$ImageCollection("LANDSAT/LT05/C02/T1_L2") $
  filterDate('1999-01-01', '1999-12-31')$
  filterBounds(ee$Geometry$Point(-73.69 , 10.83))$
  sort('CLOUD_COVER')$
  first()$
  clip(roi_ee)


Landsat__5 <-ee$ImageCollection("LANDSAT/LT05/C02/T1_L2")$
  filterDate('2010-01-01', '2010-12-31')$
  filterBounds(ee$Geometry$Point(-73.69 , 10))$
  sort('CLOUD_COVER')$
  first()$
  clip(roi_ee)



Landsat_8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2") $
  filterDate('2018-01-01', '2018-12-31')$
  filterBounds(ee$Geometry$Point(-73.69 , 10))$
  sort('CLOUD_COVER')$
  first()$
  clip(roi_ee)


Landsat_9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2") $
  filterDate('2022-01-01', '2022-12-31')$
  filterBounds(ee$Geometry$Point(-73.69 , 10))$
  sort('CLOUD_COVER')$
  first()$
  clip(roi_ee)



################################################################################
#indice de nieve

NIEVEL4 <- Landsat_4$normalizedDifference(c('SR_B2','SR_B5'))

NIEVEL5 <- Landsat_5$normalizedDifference(c('SR_B2','SR_B5'))

NIEVEL_5 <- Landsat__5$normalizedDifference(c('SR_B2','SR_B5'))

NIEVEL8 <- Landsat_8$normalizedDifference(c('SR_B3','SR_B6'))

NIEVEL9 <- Landsat_9$normalizedDifference(c('SR_B3','SR_B6'))

###############################################################################
#vizualizacion

Map$centerObject(roi_ee)
Map$addLayer(eeObject = NIEVEL4, visParams = vizualizacion, name = '1988')+
  Map$addLayer(eeObject = NIEVEL5, visParams = vizualizacion, name = '1999')+
  Map$addLayer(eeObject = NIEVEL_5, visParams = vizualizacion, name = '2010')+
  Map$addLayer(eeObject = NIEVEL8, visParams = vizualizacion, name = '2018')+
  Map$addLayer(eeObject = NIEVEL9, visParams = vizualizacion, name = '2022')



#################################################################################
#convertirlo del servidor de GEE al servidor local
library(sp)
library(raster)
library(stars)
library(sf)
library(sp)


N_1988 <- ee_as_raster(image = NIEVEL4,
                       via = "drive",
                       scale = 10)
COLOR <- colorRampPalette(c('white','blue'))


#########################################################################
#GUARDAR EL RASTER GENERADO


setwd('D:/NIXON/universidad/QGIS/curso de RGEE/PROYECTO')

library(raster)

writeRaster(N_1988, '1988')


###############################################################################
#-CALCULO DEL INDICE DE FORMA CALCULADORA RASTER RASTER


calculo <- Landsat_9 %>%  ee$Image$select(c("SR_B3",
                                            "SR_B6", 
                                            "SR_B7"))


viz <-list(min = 1 , max = 65455 , bands = c('SR_B7','SR_B6','SR_B3'))


Map$addLayer(eeObject = calculo ,  visParams = viz , 'intento 1')


calculo1 <- ee_as_raster(image = calculo,
                         via = "drive",
                         scale = 10)

names(calculo1)


verde <- calculo1$SR_B3
N1 <- calculo1$SR_B6


NDSI <- (verde - N1)/(verde + N1)


RECLASIFICAR <- reclassify(NDSI , matrix(c(-1,0.4 ,1,0.4 ,1,2), ncol = 3, byrow = TRUE))

shape <- rasterToPolygons(RECLASIFICAR , dissolve = TRUE) %>% st_as_sf() 

plot(RECLASIFICAR)

setwd('D:/NIXON/universidad/QGIS/curso de RGEE/PROYECTO/NDSI/')


writeRaster(RECLASIFICAR, '2022.tif')



