
## Packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(sf)
library(here)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(highcharter)
library(readxl)
library(cluster)
library(sparcl)
library(NbClust)
library(compareGroups)


## Read in the ward shape file
## This has been filtered outside of this app
wards <- st_read(here("wards2019_sos.shp"))

## Read in the district shape file
## This has been filtered outside of this app and is primarily so that if
## a user deselects all wards and crime types they are just presented with an outline
## of Southend

district <- st_read(here("district2019_sos.shp"))

## Read in the lsoa shape file
## This has been filtered outside of this app and will be used to show the deprivation

lsoa <- st_read(here("lsoa2011_sos.shp"))


## Read in the crime data
## This has been combined outside of this app
crime <- read.csv(here("crime.csv"))

## Extract month
crime <- crime %>% mutate(Month = lubridate::ymd(Month))

## Some preping for the colours
## Define number of colours
nb.cols <- 14
## Create colour ramp
## brewer.pal(no. of colours from palette, "Palette")
mypalette <- colorRampPalette(brewer.pal(10, "Paired"))(nb.cols)
## Define a pal colour setting for use in leaflet
pal <- colorFactor(palette = mypalette , domain = crime$Crime.type)


## Colours for the deprivation group
nb.cols_dep <- 10
mypalette_dep <- colorRampPalette(brewer.pal(10, "RdYlGn"))(nb.cols_dep)
pal_dep <- colorFactor(palette = mypalette_dep, domain = lsoa$IMD)


## Colours for population density
bins_den <- c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000)
pal_den <- colorBin("YlOrRd", domain = lsoa$DENSITY, bins = bins_den)


## Function for the mode
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
