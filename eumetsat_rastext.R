eumetsat_rastext <- function(dir,layer,crs){
  load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
      sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("raster","sf")
  
  load(packages)
  library(raster)
  library(sf)
  data <- nc_open(paste(dir,paste(layer,"nc",sep="."), sep="/"))
  coord <- nc_open(paste(dir,"geo_coordinates.nc", sep="/"))
  spdf <- SpatialPointsDataFrame(
    coords=data.frame(x= as.vector(ncvar_get(coord, "longitude")),
                      y= as.vector(ncvar_get(coord, "latitude"))),
    data=data.frame(z = as.vector(ncvar_get(data, as.character(lapply(layer, toupper))))),
  proj4string=crs(crs))
  rast.back <- raster(ext=extent(spdf), 
                      nrows=data[["dim"]][["rows"]][["len"]],
                      ncols=data[["dim"]][["columns"]][["len"]])
  rast <- rasterize(spdf@coords, rast.back, spdf@data[["z"]], fun=mean)
  rm(data, coord, spdf, rast.back)
  return(rast)
}