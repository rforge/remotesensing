# Author: Alice Laborte
# International Rice Research Institute
#Date: February 2009

#Read metadata text file
.readMetadata <- function(filename) {
    meta <- readIniFile(filename)
    meta <- meta[,-1]
    meta[,2] <- gsub('\"', "", meta[,2])
    return(meta)
}

getLandsatCPF <- function(filename) {
#Read the Landsat calibration paramter file (CPF) via ftp
    
    metainfo <- .readMetadata(filename)
    
    cpf_filename <- metainfo[metainfo[,1]=="CPF_FILE_NAME",2]
    sensor       <- metainfo[metainfo[,1]=="SENSOR_ID",2]
    
    if (sensor == "ETM+") {
        ftpdir <- "ftp://edclpdsftp.cr.usgs.gov/pub/data/CPF/ETM/"
    } else if (sensor=="TM"){  
        ftpdir <- "ftp://edclpdsftp.cr.usgs.gov/pub/data/CPF/TM/TM_CPF/" # there are 2 (3) dirs, it this the right one?
    } else {
        stop("Sensor not yet implemented!")
    }
    
    n <- nchar(cpf_filename)
    cpf_filename <- paste(substr(cpf_filename, 1, n-3), ".", substr(cpf_filename,n-1,n), sep="")
    
    download.file(paste(ftpdir, cpf_filename, sep=""), cpf_filename)

    cpf <- .readMetadata(cpf_filename)
    unlink(cpf_filename)
    return(cpf)
}

# parse Landsat metadata file and extract needed parameters
landsat <- function(filename) 
{
    
    pars     <- .readMetadata(filename)
    pathname <- dirname(filename)
    newMTL   <- length(pars[pars[,1]=="LANDSAT_SCENE_ID",2])==1 # NEW if TRUE
    
    if (newMTL)
    {
	LandsatSceneID        <- pars[pars[,1]=="LANDSAT_SCENE_ID",2] 
	spacecraft            <- pars[pars[,1]=="SPACECRAFT_ID",2] 
        sensor                <- pars[pars[,1]=="SENSOR_ID",2]
        acquisition_date      <- pars[pars[,1]=="DATE_ACQUIRED",2]
        product_creation_date <- pars[pars[,1]=="FILE_DATE",2]
        sun_elevation         <- as.numeric(pars[pars[,1]=="SUN_ELEVATION",2])
        sun_azimuth           <- as.numeric(pars[pars[,1]=="SUN_AZIMUTH",2])
        cpf_filename          <- pars[pars[,1]=="CPF_NAME",2]
        meta_filename         <- pars[pars[, 1] == "METADATA_FILE_NAME", 2]
        row                   <- sprintf("%03d",as.numeric(pars[pars[,1]=="WRS_ROW",2]))
        path                  <- sprintf("%03d",as.numeric(pars[pars[,1]=="WRS_PATH",2]))
        zone                  <- paste("p",path,"r",row,sep="")
        acquisition_time      <- pars[pars[,1]=="SCENE_CENTER_TIME",2]
        band_filenames        <- pars[grep(pars[,1],pattern="^FILE_NAME_BAND_.*$"),2]
    } else
    {
	LandsatSceneID        <- strsplit(pars[pars[,1]=="BAND1_FILE_NAME",2],"_")[[1]][1]
        spacecraft            <- toupper(pars[pars[,1]=="SPACECRAFT_ID",2]) 
        namedim               <- nchar(spacecraft)
        spacecraft            <- paste(substr(spacecraft,1,namedim-1),substr(spacecraft,namedim,namedim),sep="_")
        
        sensor                <- pars[pars[,1]=="SENSOR_ID",2]
        sensor                <- gsub(pattern="\\+",replacement="",x=sensor)
        
        acquisition_date      <- pars[pars[,1]=="ACQUISITION_DATE",2]
        product_creation_date <- pars[pars[,1]=="PRODUCT_CREATION_TIME",2]
        sun_elevation         <- as.numeric(pars[pars[,1]=="SUN_ELEVATION",2])
        sun_azimuth           <- as.numeric(pars[pars[,1]=="SUN_AZIMUTH",2])
        cpf_filename          <- pars[pars[,1]=="CPF_FILE_NAME",2]
        meta_filename         <- pars[pars[, 1] == "METADATA_L1_FILE_NAME", 2]
        starting_row          <- pars[pars[,1]=="STARTING_ROW",2]
        ending_row            <- pars[pars[,1]=="ENDING_ROW",2]
        path                  <- sprintf("%03d",as.numeric(pars[pars[,1]=="WRS_PATH",2]))
        row                   <- sprintf("%03d", as.numeric(ifelse(starting_row==ending_row,starting_row,paste(starting_row,"-",ending_row, sep=""))))
        zone                  <- paste("p",path, "r",row,sep="")
        acquisition_time      <- pars[pars[,1]=="SCENE_CENTER_SCAN_TIME",2]
        band_filenames        <- pars[grep(pars[,1],pattern="^BAND.*_FILE_NAME$"),2]
    }

    bandcomb <- if (sensor=="ETM")
    {
        "123456678"
    } else if (sensor=="TM")
    {
        "1234567"
    } else if (sensor=="MSS")
    {
        "4567"
    }
    n_bands <- as.integer(nchar(bandcomb))

    bandn    <- rep(NA,n_bands)
    bandnOLD <- rep(NA,n_bands)

    for (i in 1:n_bands)
    {
       bandn[i]    <- paste("BAND_", substr(bandcomb,i,i), sep="")
       bandnOLD[i] <- paste("BAND",  substr(bandcomb,i,i), sep="")
    }
        
    if (sensor == "ETM") 
    {                        
        bandn[6] <- "BAND_6_VCID_1"
        bandn[7] <- "BAND_6_VCID_2"

        bandnOLD[6] <- "BAND61"
        bandnOLD[7] <- "BAND62"
    }

    lmax <- lmin <- qcalmax <- qcalmin  <- rep(NA, n_bands)
    band_filenames <- paste(pathname, band_filenames, sep='/')

    if(newMTL)        
    {
        for (i in 1:n_bands) 
        {
            lmax[i] <- as.numeric(pars[pars[,1]==paste("RADIANCE_MAXIMUM_",bandn[i], sep=""),2])
            lmin[i] <- as.numeric(pars[pars[,1]==paste("RADIANCE_MINIMUM_",bandn[i], sep=""),2])
            qcalmax[i] <- as.numeric(pars[pars[,1]==paste("QUANTIZE_CAL_MAX_",bandn[i], sep=""),2])
            qcalmin[i] <- as.numeric(pars[pars[,1]==paste("QUANTIZE_CAL_MIN_",bandn[i], sep=""),2])
        }
    } else
    {
        for (i in 1:n_bands) 
        {
            lmax[i] <- as.numeric(pars[pars[,1]==paste("LMAX_",bandnOLD[i],sep=""),2])
            lmin[i] <- as.numeric(pars[pars[,1]==paste("LMIN_",bandnOLD[i],sep=""),2])
            qcalmax[i] <- as.numeric(pars[pars[,1]==paste("QCALMAX_",bandnOLD[i],sep=""),2])
            qcalmin[i] <- as.numeric(pars[pars[,1]==paste("QCALMIN_",bandnOLD[i],sep=""),2])
        }
    }
        
    names(lmax) <- names(lmin) <- names(qcalmax) <- names(qcalmin) <- names(band_filenames) <- bandn    

    # cpf <- getLandsatCPF(metafile) # this works now!
    # acquisition_time <- cpf[cpf[,1]=="Turn_Around_Time",2] # what is this value?
    
    if (sensor == "ETM")
    {            
        img <- new("LandsatETMp")    
        mainbands <- c("BAND_1","BAND_2","BAND_3","BAND_4","BAND_5","BAND_7")
        img <- addLayer(img,  as.list( band_filenames[mainbands] ) )
        names(img) <- mainbands
        img@thermal <- stack( as.list( band_filenames[ c("BAND_6_VCID_1", "BAND_6_VCID_2") ] ) )
        names(img@thermal) <-  c("BAND_6_VCID_1", "BAND_6_VCID_2") 
        img@panchromatic <- raster(band_filenames["BAND_8"])
        names(img@panchromatic) <- "BAND_8"
    
    } else if (sensor == "TM")
    {
        img <- new("LandsatTM")
        mainbands <- c("BAND_1","BAND_2","BAND_3","BAND_4","BAND_5","BAND_7")
        img <- addLayer(img,  as.list( band_filenames[mainbands] ) )
        names(img) <- mainbands
        img@thermal <- raster(band_filenames["BAND_6"]) 
        names(img@thermal) <- "BAND_6"
    }  else 
    {    
        stop('this function only works for the ETM+ and TM sensors, other sensors to be done later...')
    }            
    img@sensor@scene_id <- LandsatSceneID
    img@sensor@spacecraft <- spacecraft
    img@sensor@name <- sensor
    img@sensor@product_creation_date <- product_creation_date
    img@sensor@acquisition_date <- acquisition_date
    img@sensor@sun_elevation <- sun_elevation
    img@sensor@sun_azimuth <- sun_azimuth
    img@sensor@cpf_filename <- cpf_filename
    img@sensor@metafile <- meta_filename
    img@sensor@acquisition_time <- acquisition_time
    img@sensor@band_filenames <- band_filenames
    img@sensor@zone <- zone
    img@sensor@lmax <- lmax
    img@sensor@lmin <- lmin
    img@sensor@qcalmax <- qcalmax
    img@sensor@qcalmin <- qcalmin
    
    img@calibrated <- FALSE    
    img@calibration <- "none"
    img@unit <- "DN"

    img@thermal_calibrated <- FALSE    
    img@thermal_calibration <- "none"
    img@thermal_unit <- "DN"
    
    img@layers <- lapply(img@layers, function(x){ NAvalue(x) <- 0; return(x)} )
    
    return (img)
 }
 
 
 
