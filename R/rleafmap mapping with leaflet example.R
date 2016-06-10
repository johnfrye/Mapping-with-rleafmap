
# Environment -------------------------------------------------------------

rm(list=ls())
setoptions()
wd <- getwd()
ad <- setdirs()[1]
dd <- setdirs()[2]
gd <- setdirs()[3]
# compdir <- setdirs()[6]
rptdir <- fp(wd, "reports")
wd

ses.info <- sessionInfo()
time <- time_stamp()

# packages ----------------------------------------------------------------

pkgs = c("devtools", "purrr", "plyr", 
         "rpart", "magrittr", "httr", "xlsx", "readxl", "openxlsx", "data.table", 
         "reshape2", "dplyr", "stringr", "stringi", "rio", "smutilities", 
         "WorksheetFunctions", "rleafmap", "spatstat", "maptools")
inst = lapply(pkgs, library, character.only = TRUE)

# map ---------------------------------------------------------------------

# note that velov is a spatial object as defined by package 'sp'
data(velov)

# Create a tile layer object
stamen.bm <- basemap("stamen.toner")

# Create a data layer object (points)
velov.sta <- spLayer(velov, stroke = F, popup = velov$stations.name)

# Generate the map
writeMap(stamen.bm, dir=gd, prefix= "Station Map",
         velov.sta, width = 550, height = 250,
         setView = c(45.76, 4.85), setZoom = 12)

# Create a different data layer object (points)
velov.map <- spLayer(velov, stroke = FALSE,
                     fill.col= c("red", "green"),
                     popup = velov$NAME)

# Generate the map
writeMap(stamen.bm, dir=gd, prefix= "Station Map - Colored",
         velov.map, width = 550, height = 250,
         setView = c(45.76, 4.85), setZoom = 12)

# Layers Control ----------------------------------------------------------

# Compute a grid of station density
win <- owin(xrange = bbox(velov)[1,] + c(-0.01,0.01),
            yrange = bbox(velov)[2,] + c(-0.01,0.01))

velov.ppp <- ppp(coordinates(velov)[,1], coordinates(velov)[,2],
                 window = win)

velov.ppp.d <- density(velov.ppp, sigma = 0.005)
velov.d <- as.SpatialGridDataFrame.im(velov.ppp.d)
velov.d$v[velov.d$v < 10^3] <- NA

# Create two tile layer objects
stamen.bm <- basemap("stamen.toner")
mapquest.bm <- basemap("mapquest.map")

# Create two data layer objects (stations and density grid)
# note:  this will throw a warning.  It's not a problem:
# "The coordinate system of the grid has not been recognized. 
# It is assumed to be EPSG:4326"  
velov.sta <- spLayer(velov, stroke = FALSE, fill.col = 1,
                     size = 3, popup = velov$NAME)
velov.den <- spLayer(velov.d, layer = "v",
                     cells.alpha = seq(0.1, 0.8, length.out = 12))

# Creating an UI object with a layer control
my.ui <- ui(layers = "topright")

#Generate the map
writeMap(stamen.bm, dir=gd, prefix= "Layered Map",
         mapquest.bm, velov.sta, velov.den,
         width = 550, height = 250, interface = my.ui,
         setView = c(45.76, 4.85), setZoom = 12) 

# Work with popups --------------------------------------------------------

# dataset of the distribution of camp sites in France.
data(campsites)

# some setup
gcol <- rev(heat.colors(5))
gcut <- cut(campsites$N.CAMPSITES, breaks = c(-1, 20, 40, 60, 80, 1000))
region.col <- gcol[as.numeric(gcut)]

# extend popups with HTML tags
pop <- paste(campsites$DEP.NAME, " (", campsites$DEP.CODE, ") <br>",
             campsites$N.1, "★ <br>", campsites$N.2, "★★ <br>", 
             campsites$N.3, "★★★ <br>", campsites$N.4, "★★★★ <br>", 
             campsites$N.5, "★★★★★", sep = "")

# set the basemap
mapquest.bm <- basemap("mapquest.map")      
cs.lay <- spLayer(campsites, fill.col = region.col, popup = pop)

my.ui <- ui(attrib.text = "Campsites Data: <a href='http://www.ign.fr/'>IGN</a>, <a href='http://atout-france.fr/'>ATOUT France</a>")

writeMap(mapquest.bm, dir=gd, prefix= "Map with Popups",
         cs.lay, width = 550, height = 350,
         interface = my.ui, setView = c(46.5, 3), setZoom = 5)