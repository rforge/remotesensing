# Author: Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3

properPath <- function(path, changeBS=TRUE){
    if (changeBS){
        path <- gsub("\\\\", "/", path)
    }
    lastchar <- substr(path,nchar(path),nchar(path))
    if (lastchar=="/"){
        path <- substr(path,1,nchar(path)-1)
    }
    return(path)
}

raster2SGDF <- function(baseraster, vals=NULL){
    require(rgdal)
    gtop <- GridTopology(c(xmin(baseraster)+(xres(baseraster)/2),ymin(baseraster)+(yres(baseraster)/2)),c(xres(baseraster),yres(baseraster)),c(ncol(baseraster),nrow(baseraster)))
    proj <- CRS(projection(baseraster))
    if (is.null(vals)){
        rnew <- SpatialGridDataFrame(gtop, as.data.frame(getValues(baseraster)), proj4string=proj)
    } else if (is.vector(vals) & length(vals)==ncell(baseraster)){
        rnew <- SpatialGridDataFrame(gtop, as.data.frame(vals), proj4string=proj)
    } else if (is.matrix(vals) & length(vals)==ncell(baseraster)){
        vals <- as.vector(t(vals))
        rnew <- SpatialGridDataFrame(gtop, as.data.frame(vals), proj4string=proj)
    } else {
        cat("Length of vals does not match ncells of raster.\n Creating a blank SpatialGridDataFrame instead.\n")
        flush.console()
        rnew <- SpatialGridDataFrame(gtop, as.data.frame(rep(NA,ncell(baseraster))), proj4string=proj)    
    }     
    return(rnew)            
}
