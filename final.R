library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(dplyr)
library(stringr)
library(tidyverse)
library(spatstat)
library(here)
library(rgeos)
library(GISTools)
library(tmap)
library(geojson)
library(geojsonio)
library(janitor)
library(ggplot2)
library(ggfortify)
library(vars)
library(ggmap)
library(mapview)
library(jsonlite)
library(spatstat)


#Greter Manchester boundary
boundaryline<-st_read(here("Boundary.shp")) 
summary(boundaryline)
GMboundary<- boundaryline%>%
  st_transform(., 27700)


#Read Airbnb data in 2019 and 2020
Airbnb2019 <- read_csv("https://github.com/IreneYang725/GISFINAL/blob/main/GM2019.csv?raw=true") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)
Airbnb2020 <- read_csv("https://github.com/IreneYang725/GISFINAL/blob/main/GM2020.csv?raw=true") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)

#join function 
Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(GMboundary,.)%>%
    add_count(CODE, name="rooms_in_borough") 
  
  return(output)
}

#join the Airbnb with boundary
Airbnb2020 <- Joinfun(Airbnb2020, GMboundary)
Airbnb2019<- Joinfun(Airbnb2019, GMboundary)

Airbnb2020 <- Airbnb2020%>%
  group_by(.,CODE, NAME)%>%
  summarise(`Accomodation count` = unique(rooms_in_borough))
Airbnb2019 <- Airbnb2019%>%
  group_by(.,CODE, NAME)%>%
  summarise(`Accomodation count` = unique(rooms_in_borough))


#Airbnb in GM
tmap_mode("plot")
breaks = c(0, 300, 600, 900, 1200, 1500, 1800)
tm2 <- tm_shape(Airbnb2020) + 
  tm_polygons("Accomodation count",
              breaks=breaks, 
              palette="PuBu") + 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits('Airbnb in Greater Manchester 2020', position=c(0,0.85), size=1)
legend <- tm_shape(Airbnb2020) +
  tm_polygons("Accomodation count",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)+
  tm_credits("", position=c(0.0,0.0))

t2=tmap_arrange(tm2, legend, ncol=2)
t2

tmap_mode("plot")
breaks1 = c(0, 300, 600, 900, 1200, 1500, 2000)
tm3 <- tm_shape(Airbnb2019) + 
  tm_polygons("Accomodation count",
              breaks=breaks1, 
              palette="PuBu") + 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits('Airbnb in Greater Manchester 2020', position=c(0,0.85), size=1)
legend <- tm_shape(Airbnb2019) +
  tm_polygons("Accomodation count",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)+
  tm_credits("", position=c(0.0,0.0))

t3=tmap_arrange(tm2, legend, ncol=2)
t3
#plot the point
GM2020<- st_read(here::here("GM2020.geojson")) %>%
  st_transform(.,27700)
GM2020<- distinct(GM2020)
GM2020<- GM2020[GMboundary,]

tmap_mode("view")
tm_shape(GMboundary) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(GM2020) +
  tm_dots(col = "green",size = 0.001)

GM2019<- st_read(here::here("GM2019.geojson")) %>%
  st_transform(.,27700)
GM2019<- distinct(GM2019)
GM2019<- GM2019[GMboundary,]

tmap_mode("view")
tm_shape(GMboundary) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(GM2019) +
  tm_dots(col = "green",size = 0.001)

#analysis the point parttern to occupancy
GM2019notrented <- read_csv("GM2019.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(availability_30 =='30'|availability_60 =='60')

GM2020notrented <- read_csv("GM2020.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>% 
  filter(availability_30=='30'|availability_60 =='60')

tmap_mode("view")
tm_shape(GMboundary) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(GM2019notrented) +
  tm_dots(col = "blue",size=0.009)


tmap_mode("view")
tm_shape(GMboundary) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(GM2020notrented) +
  tm_dots(col = "blue",size=0.009)

window <- as.owin(GMboundary)
plot(window)

GM2019notrented<- GM2019notrented %>%
  as(., 'Spatial')

GM2019notrented.PPP <- ppp(x=GM2019notrented@coords[,1],
                          y=GM2019notrented@coords[,2],
                          window=window)


GM2019notrented.PPP  %>%
  plot(.,pch=20,cex=0.1, 
       main="Not rented Airbnb house in 2019")

GM2020notrented<- GM2020notrented %>%
  as(., 'Spatial')

GM2020notrented.PPP <- ppp(x=GM2020notrented@coords[,1],
                           y=GM2020notrented@coords[,2],
                           window=window)



GM2019notrented.PPP %>%
  density(., sigma=1000) %>%
  plot(main="KDE map of 0 booking in 2019")

GM2020notrented.PPP %>%
  density(., sigma=1000) %>%
  plot(main="KDE map of 0 booking in 2020")


