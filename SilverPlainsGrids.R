# Task = create hex grids for designing Cattai Swamp sambar surveys
# Adapted from Kosciuszko survey grids, AJB 01/11/2019
library(sp); library(lattice); library(rgdal); library(rgeos); library(raster)

write_shapefiles <- F

# Define coordinate reference system as GDA94
crs3112 <- CRS("Extent: 112.85, -43.70, 153.69, -9.86
Proj4: +proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
crsWGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs")
coordRefSys <- crs3112

# Load region boundary
dsn <- getwd()
region  <- readOGR(dsn=dsn, layer="SilverPlainsRoughBoundary_3112")

#############################################################
###### Create hexagonal grid with X m between centre points

# cellsize argument = 2* incircle radius of a hexagon
x <- 800
HexPts <-spsample(x = region, type="hexagonal", cellsize=x, offset=c(0.5,0.5))  
 notes <- c(1:length(HexPts))
 HexPtsDF <- SpatialPointsDataFrame(HexPts, data=as.data.frame(notes), proj4string = coordRefSys, match.ID=F) 
 HexPtsDF$cell <- as.factor(c(1:length(HexPtsDF)))     # add unique cell ID for each cell
 HexPtsDF$label  <- factor(rep("na", length(HexPtsDF))) 
 HexPtsDF$site  <- factor(rep("na", length(HexPtsDF)))  # create a field to allow reserve name to be entered later
 HexPtsDF

HexPols <- HexPoints2SpatialPolygons(HexPts)
plot(HexPols)
plot(region, lwd=3, add=F)
plot(HexPts, add=T)

# Convert grid to Spatial Polygons Data Frame and write as a shapefile
HexPols <- SpatialPolygonsDataFrame(HexPols, data=as.data.frame(names(HexPols)), match.ID=F)
 HexPols$cell <- as.factor(c(1:length(HexPols)))     # add unique cell ID for each cell
 HexPols$site  <- factor(rep("na", length(HexPols)))  # create a field to allow reserve name to be entered later
 HexPols

BufferDist <- 50
Buff <- gBuffer(HexPts, width=BufferDist)
        plot(Buff, add=T)
        Buffdf <- SpatialPolygonsDataFrame(Buff, data=as.data.frame(1), match.ID=F)
        plot(Buffdf)

if(write_shapefiles == T){          
        writeOGR(Buffdf, dsn=dsn, layer=paste0("HexGrid", x, "m_Buffer", BufferDist, "m_3112"), driver="ESRI Shapefile")
        writeOGR(HexPols, dsn=dsn, layer=paste0("HexGrid_", x, "m_3112"), driver="ESRI Shapefile")        
        writeOGR(HexPtsDF, dsn=dsn, layer=paste0("HexPts_", x, "m_3112"), driver="ESRI Shapefile")
}
nrow(HexPols)
