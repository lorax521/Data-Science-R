#THIS: http://lab.fengxiao.info/2016/11/23/ENM-in-R-workshop.html

library(rgbif)
library(dismo)
library(rJava)
library(raster)
library(rgeos)

us1 <- getData('GADM', country = 'US', level=1)
usa <- gUnaryUnion(us1[(us1$NAME_1 != "Hawaii") & (us1$NAME_1 != "Alaska"),])
mx <- getData('GADM', country = 'MX', level=0)
aoi <- gUnionCascaded(usa, mx)
aoi <- gUnion(usa, mx)
ras <- getData('worldclim', res=10, var='bio')
bioVar1 <- mask(ras[[1]], aoi)
bioVar12 <- mask(ras[[12]], aoi)
stack <- stack(bioVar1, bioVar12)


# downloads specie data from gbif
download <- gbif('Asclepias')
download <- gbif('Asclepias asperula')
download <- gbif('Asclepias oenotheroides')
download <- gbif('Asclepias capricornu')
download <- gbif('Asclepias viridis')
download <- gbif('Danaus plexippus')
# removes duplicate records
print(table(duplicated(download[c("lon","lat")])))
occu <- download[!duplicated(download[c("lon","lat")]),]
# filters years
occy <- occu[(occu$year >= 2000) & (occu$year <= 2010),]
# filters only geoferenced records
occ <- occy[((!is.na(occy$lat)) | (!is.na(occy$lon))),]
# creates coordinate pairs
coordinates(occ) <- ~ lon + lat
coords <- coordinates(occ)

# create and load shapefile
shapefile(occ,"C:/Users/jrain/Documents/Graduate_School/Research/Monarch_Butterflies/Data/test/asclepias2.shp", overwrite=T)
shp <- shapefile("C:/Users/jrain/Documents/Graduate_School/Research/Monarch_Butterflies/Data/test/asclepias2.shp")

# load raster

# assign crs
crs(shp) <- crs(stack)
occ <- gIntersection(shp, aoi)
plot(ras[[1]])
plot(shp, col="blue", pch=19, cex=.5, add=T)
# MAXENT
#trainning the model
model <- maxent(x=stack, p=occ)
#projecting model
prediction <- predict(model, stack)




"""
make array of species names
make array of environmental variables

for each specie:
  get records
  filter records
  for each permutation of environmental variables:
    create maxent model
    get model scores
  keep best SDM
"""
  

