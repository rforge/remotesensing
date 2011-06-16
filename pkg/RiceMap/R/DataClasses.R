# Authors: Jorrel Khalil S. Aunario
# Date: 18 May 2011

setClass ("modis.data", 
    representation(
        product = "character",
        acqdate =  "character",
        zone = "character",
        version = "character",
        proddate = "character",
        projection = "character",
        extent = "Extent",
        ncols = "numeric",
        nrows = "numeric" ,
        imgvals = "data.frame"),
    prototype(
        product = "",
        acqdate =  "",
        zone = "",
        version = "",
        proddate = "",
        projection = "",
        extent = extent(1,10,1,10),
        ncols = 2400,
        nrows = 2400, 
        imgvals = NULL),
    package="RiceMap"
)
