# Authors: Alice Laborte, Yann Chemin, Robert Hijmans
# International Rice Research Institute
# Date: February 2009
# version 2, August 2010

# Landsat and ASTER Calibration
# Conversion of DN to radiance and to reflectance
# Conversion  of thermal DN to temperature
# Sources of calibration parameters :  
#   Landsat 1-4 MSS: Markham, B.L. and Barker, J.L. 1986. Landsat MSS and TM post-calibration dynamic ranges, exoatmospheric reflectances and at-satellite temperatures. Landsat Technical Note 1, August 1986.
#   Landsat 4 & 5 TM: Chander, G., and Markham, B. 2003. Revised Landsat-5 TM radiometric calibration procedures and postcalibration dynamic ranges. IEEE Transactions on Geoscience and Remote Sensing, 41, 2674-2677
# Chander, G., Markham, B.L, Barsi, J.A. 2007. Revised Landsat-5 Thematic Mapper radiometric calibration. IEEE Transactions on Geoscience and Remote Sensing Letters, 4, 490-494
#   Landsat 7 ETM+:    Landsat 7 Science Data Users Handbook. Chapter 9 - Calibration parameter file. URL: http://landsathandbook.gsfc.nasa.gov/handbook/handbook_htmls/chapter9/chapter9.html
#   ASTER:  ASTER Surface Reflectance/Radiance VNIR/SWIR Product. URL: http://asterweb.jpl.nasa.gov/content/03_data/01_Data_Products/release_aster_surface_reflectance.htm


#  \item{sun_elevation}{sun elevation in degrees}
#  \item{radiance}{spectral radiance at the sensor's aperture, W/(m2 * sr * micrometer)}
#  \item{ESUN}{mean solar exoatmospheric irradiances, W/(m2 * sr * micrometer)}
#  \item{radiance_thermal}{thermal band spectral radiance at the sensor's aperture,  W / (m2 * sr * micrometer)}
#  \item{K}{vector of two numeric values. Landsat thermal calibration constants K1, K2, returned by K_landsat }


dn2rad <- function(x, filename='', ...) 
{
    #Conversion of DN to radiance 
    #  \item{gain}{band-specific rescaling gain factor, W / (m2 * sr * micrometer) /DN }
    #  \item{bias}{band-specific rescaling bias factor, W / (m2 * sr * micrometer) }
    if (x@calibrated) 
    {
        stop('This object has already been calibrated')
    }

    gb      <- .getGainBias(x)
    gain    <- gb[, "gain"][names(x)]
    bias    <- gb[, "bias"][names(x)]

    if (length(gain) != nlayers(x)) 
    {
        stop('length(gain) != nlayers(x)')
    }
    
    if (filename!='')
    {
        if (strsplit(tolower(basename(filename)),"\\.")[[1]][1]=='default')
        {
            ftype <- strsplit(basename(filename),"\\.")[[1]]
            ftype <- ftype[length(ftype)]
            filename <- file.path(dirname(filename), paste(x@sensor@scene_id, "_rad.",ftype,sep=""))
            cat("Using default filename:", filename, "\n")
        }
    }

    radiance     <- calc(x, function(x){ t(t(x) * gain + bias) }, filename=filename, forcefun=TRUE,...)

    radiance      <- stack(radiance)
    x@layers      <- radiance@layers
    x@calibrated  <- TRUE
    x@calibration <- 'radiance'
    x@unit        <- 'W/m2 sr um'
    return(x)
}


#Conversion of DN to reflectance
dn2ref  <- function( x, filename='', ... ) 
{
    if (x@calibrated)
    {
        stop('This object has already been calibrated')
    }

    if (! inherits(x, 'Landsat'))
    {
        stop('only available for Landsat objects')    
    }

    gb   <- .getGainBias(x)
    gain <- gb[,"gain"][names(x)]
    bias <- gb[,"bias"][names(x)]
    
    if (length(gain) != nlayers(x))
    {
        stop('length(gain) != nlayers(x)')
    }
    getDS <- function(doy)
    {
        # ds = earth to sun distance in astronomical units}
        return ( 1.0 + 0.01672 * sin( 2 * base::pi * ( doy - 93.5 ) / 365 ) )
    }
        
    ESUN    <- .esun(x@sensor@spacecraft, x@sensor@name)[names(x)]
    doy     <- as.integer(format(as.Date(x@sensor@acquisition_date),"%j"))
    ds      <- getDS(doy)
    xfac    <- (base::pi * ds * ds) / (ESUN * cos ((90 - x@sensor@sun_elevation) * base::pi/180))
    
    if (filename!='')
    {
        if (strsplit(tolower(basename(filename)),"\\.")[[1]][1]=='default')
        {
            ftype <- strsplit(basename(filename),"\\.")[[1]]
            ftype <- ftype[length(ftype)]
            filename <- file.path(dirname(filename), paste(x@sensor@scene_id, "_ref.",ftype,sep=""))
            cat("Using default filename:", filename, "\n")
        }
    }
        
    reflectance <- calc(x, function(x){ t(((t(x) * gain + bias)) * xfac) }, filename=filename, forcefun=TRUE, ... ) 
    reflectance <- stack(reflectance)
    x@layers    <- reflectance@layers

    x@calibrated  <- TRUE
    x@calibration <- 'reflectance'
    x@unit        <- 'factor'
    
    return(x)
}


#Conversion of DN to temperature
dn2temp <- function(x, filename='', ...)
{

    if (! inherits(x, 'Landsat'))
    {
        stop('only available for Landsat objects')    
    }

    if (x@sensor@name == "ETM")
    {
        b <- c("BAND_6_VCID_1","BAND_6_VCID_2")
    } else if (x@sensor@name  == "TM")
    {
        b <- "BAND_6"  
    } else
    {
        stop('not done yet')
    }

    if (x@thermal_calibrated)
    {
        stop('This object has already been calibrated')
    }
    
    K_landsat <- function(spacecraft, sensor)
    {
    #Landsat thermal calibration constants K1, K2
        if (spacecraft == "LANDSAT_4" & sensor == "TM") { K <-  c(671.62, 1284.3) }
        else if (spacecraft == "LANDSAT_5" & sensor == "TM") { K <-  c(607.76, 1260.56) }
        else if (spacecraft == "LANDSAT_7") { K <- c(666.09, 1282.71 ) }
        else K <- NA
        return (K)
    }

    K <- K_landsat(x@sensor@spacecraft, x@sensor@name)

    gb        <- .getGainBias(x)
    gain    <- gb[, "gain"][b]
    bias    <- gb[, "bias"][b]

    if (filename!='')
    {
        if (strsplit(tolower(basename(filename)),"\\.")[[1]][1]=='default')
        {
            ftype <- strsplit(basename(filename),"\\.")[[1]]
            ftype <- ftype[length(ftype)]
            filename <- file.path(dirname(filename), paste(x@sensor@scene_id, "_LST.",ftype,sep=""))
            cat("Using default filename:", filename, "\n")
        }
    }

    # radiance    
    temp <- calc(x@thermal, fun=function(x){ K[2] / (log ((K[1] / (t(t(x)*gain+bias))) + 1.0)) }, filename=filename, forcefun=TRUE, ...)
    
    if (x@sensor@name == "ETM") 
    {
        temp <- stack(temp)
    } 
    
    x@thermal <- temp
    x@thermal_calibrated <- TRUE
    x@thermal_calibration <- 'LST' # ?
    x@thermal_unit <- 'K'
    
    return(x)
}


.GainBias <- function(spacecraft, sensor, acquisition_date, product_creation_date) 
{
# Calculate gain & bias for Landsat MSS, Landsat TM and aster
    
    prod_creation_date <- as.Date(product_creation_date)    
    chk <- TRUE
    
    if (sensor == "MSS" ) 
    {
        if (spacecraft == "LANDSAT_1") 
        {
            lmin  <- c(0.0, 0.0, 0.0, 0.0)
            lmax <- c(248.0, 200.0, 176.0, 153.0) 
        } else if (spacecraft == "LANDSAT_2") 
        {
            if ( prod_creation_date < as.Date("1975-07-16")) 
            {
                lmin  <- c(10.0, 7.0, 7.0, 5.0)
                lmax <- c(210.0, 156.0, 140.0, 138.0) 
            } else 
            {
                lmin  <- c(8.0, 6.0, 6.0, 4.0)
                lmax <- c(263.0, 176.0, 152.0, 130.0) 
            }
        } else if (spacecraft == "LANDSAT_3") 
        {
            if (prod_creation_date < as.Date("1978-06-01", "%m/%d/%Y")) 
            {
                lmin  <- c(4.0, 3.0, 3.0, 1.0)
                lmax <- c(220.0, 175.0, 145.0, 147.0) 
            } else 
            {
                lmin  <- c(4.0, 3.0, 3.0, 1.0)
                lmax <- c(259.0, 179.0, 149.0, 128.0) 
            }
        } else if (spacecraft == "LANDSAT_4") 
        {
            if (acquisition_date < as.Date("1982-08-26")) 
            {
                lmin  <- c(2.0, 4.0, 4.0, 3.0)
                lmax <- c(250.0, 180.0, 150.0, 133.0) 
            } else if (acquisition_date < as.Date("1983-04-01")) 
            {
                lmin  <- c(2.0, 4.0, 4.0, 3.0)
                lmax <- c(230.0, 180.0, 130.0, 133.0) 
            } else 
            {
                lmin  <- c(4.0, 4.0, 5.0, 4.0)
                lmax <- c(238.0, 164.0, 142.0, 116.0) 
            }
        } else if (spacecraft == "LANDSAT_5") 
        {
            if (acquisition_date < as.Date("1984-04-06")) 
            {
                lmin  <- c(4.0, 3.0, 4.0, 2.0)
                lmax <- c(240.0, 170.0, 150.0, 127.0) 
            } else if (acquisition_date < as.Date("1984-11-08")) 
            {
                lmin  <- c(3.0, 3.0, 4.0, 3.0)
                lmax <- c(268.0, 179.0, 159.0, 123.0)
            } else 
            {
                lmin  <- c(3.0, 3.0, 5.0, 3.0)
                lmax <- c(268.0, 179.0, 148.0, 123.0) 
            }
        } else 
        {
            stop('MSS image with unknown spacecraft')
        }
        
        if (spacecraft %in% c("LANDSAT_1", "LANDSAT_2", "LANDSAT_3") & acquisition_date < as.Date("1979-02-01")) 
        {
            qcalmax <- c(127, 127, 127, 63)
        } else if (spacecraft == "LANDSAT_4" & prod_creation_date < as.Date("1982-10-22")) 
        {
            qcalmax <- c(127, 127, 127, 63)
        } else 
        { 
            qcalmax <- c(127, 127, 127, 127)
        }
        gain <- (lmax - lmin) / qcalmax
        bias <- lmin
        bandnames <- c("BAND_1","BAND_2","BAND_3","BAND_4")
        
        
    } else if (sensor == "TM") 
    {
    
        if (spacecraft == "LANDSAT_4") 
        {

            if (prod_creation_date < as.Date("1983-08-01")) 
            {
                lmin  <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 2, -0.15) 
                lmax <- c(158.42, 308.17, 234.63, 224.32, 32.42, 15.64, 17.0) 
            
            } else if (prod_creation_date < as.Date("1984-01-15")) 
            {
                lmin  <- c(0.0, 0.0, 0.0, 0.0, 0.0, 4.84, 0.0) 
                lmax <- c(142.86, 291.25, 225.0, 214.29, 30.0, 12.4, 15.93) 
            
            } else 
            {
                lmin  <- c(-1.5, -2.8, -1.2, -1.5, -0.37, 1.238, -0.15) 
                lmax <- c(152.1, 296.8, 204.3, 206.2, 27.19, 15.6, 14.38) 
            }
            
            qcalmax <- 255
            gain <- (lmax - lmin) / qcalmax
            bias <- lmin
            
        } else if (spacecraft == "LANDSAT_5") 
        {
            if (prod_creation_date < as.Date("2003-05-04")) 
            {
                gain <- c(0.602431, 1.175100, 0.805765, 0.814549, 0.108078, 0.055158, 0.056980)
                bias <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 1.2378, -0.15) 
            } else if (prod_creation_date < as.Date("2007-04-01"))  
            {
                gain <- c(0.762824, 1.442510, 1.039880, 0.872588, 0.119882, 0.055158, 0.065294)
                bias <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 1.2378, -0.15) 
            } else if (acquisition_date < as.Date("1991-12-31")) 
            {
                gain <- c(0.668706, 1.317020, 1.039880, 0.872588, 0.119882, 0.055158, 0.065294)
                bias <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 1.2378, -0.15) 
            } else 
            {
                gain <- c(0.762824, 1.442510, 1.039880, 0.872588, 0.119882, 0.055158, 0.065294)
                bias <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 1.2378, -0.15) 
            }
        } else 
        {
            stop('TM image with unknown spacecraft')
        }
        
        bandnames <- c("BAND_1","BAND_2","BAND_3","BAND_4","BAND_5","BAND_6","BAND_7")
        
    } else if (sensor == "Aster") 
    {
    
        gain <- c(0.676, 0.708, 0.862, 0.2174, 0.0696, 0.0625, 0.0597, 0.0417, 0.0318)
        bias <- c(-0.676, -0.708, -0.862, -0.2174, -0.0696, -0.0625, -0.0597, -0.0417, -0.0318)
        bandnames <- c("BAND_1","BAND_2","BAND_3","BAND_4","BAND_5","BAND_6","BAND_7","BAND_8","BAND_9")
        
    } 
    result <- cbind(gain,bias)
    rownames(result) <- bandnames 
    return (result)
}


.getGainBias <- function(x) 
{
    if (is.na(x@sensor@lmax[1])) 
    {
        gb <- .GainBias(x@sensor@spacecraft, x@sensor@name, x@sensor@acquisition_date, x@sensor@product_creation_date) 
    } else 
    {
        gain <- (x@sensor@lmax - x@sensor@lmin) / (x@sensor@qcalmax - x@sensor@qcalmin)
        bias <- x@sensor@lmin - gain * x@sensor@qcalmin
        gb   <- cbind(gain,bias)
    }
    return(gb)
}



.esun <- function(spacecraft, sensor) 
{
# Calculate the sun exo-atmospheric irridiance [W/m2/sr].  This is used for processing surface reflectance.

# spacecraft LANDSAT_1
#    sensor MSS        (BAND_1, BAND_2, BAND_3, BAND_4)
#spacecraft LANDSAT_2
#    sensor MSS        (BAND_1, BAND_2, BAND_3, BAND_4)
#spacecraft LANDSAT_3
#    sensor MSS        (BAND_1, BAND_2, BAND_3, BAND_4)
#spacecraft LANDSAT_4
#    sensor MSS        (BAND_1, BAND_2, BAND_3, BAND_4)
#    sensor TM        (BAND__id BAND_1, BAND_2, BAND_3, BAND_4, BAND_5, NA,BAND_7)
#spacecraft LANDSAT_5
#    sensor MSS        (BAND_1, BAND_2, BAND_3, BAND_4)
#    sensor TM        (BAND_1, BAND_2, BAND_3, BAND_4, BAND_5, NA,BAND_7)
#spacecraft LANDSAT_7
#    sensor ETM        (BAND_1, BAND_2, BAND_3, BAND_4, BAND_5, NA,NA,BAND_7, BAND_8)
#spacecraft Terra
#    sensor Aster        (BAND_1, BAND_2, BAND_3, BAND_4, BAND_5, NA,BAND_7, BAND_8, BAND_9)
    
    if (sensor == "MSS") 
    {
        if (spacecraft == "LANDSAT_1") { ESUN <- c(1852.0, 1584.0, 1276.0, 904.0) }
        else if (spacecraft == "LANDSAT_2") { ESUN <- c(1856.0, 1559.0, 1269.0, 906.0) }
        else if (spacecraft == "LANDSAT_3") { ESUN <- c(1860.0, 1571.0, 1289.0, 910.0) }
        else if (spacecraft == "LANDSAT_4") { ESUN <- c(1851.0, 1593.0, 1260.0, 878.0) }
        else if (spacecraft == "LANDSAT_5") { ESUN <- c(1849.0, 1595.0, 1253.0, 870.0) }
        names(ESUN)    <- c("BAND_1","BAND_2","BAND_3","BAND_4")
        
    } else if (sensor == "TM")  
    {
        if (spacecraft == "LANDSAT_4") 
        { 
            ESUN <- c(1957.0, 1825.0, 1557.0, 1033.0, 214.9, NA, 80.72)  
        } else if (spacecraft == "LANDSAT_5") 
        {
            ESUN <- c(1957.0, 1826.0, 1554.0, 1036.0, 215.0, NA, 80.67)  
        }
        names(ESUN)    <- c("BAND_1","BAND_2","BAND_3","BAND_4", "BAND_5","BAND_6","BAND_7")
        
    } else if (sensor == "ETM")
    { 
        if (spacecraft == "LANDSAT_7")
        { 
            ESUN <- c(1969.0, 1840.0, 1551.0, 1044.0, 225.7, NA, NA, 82.07, 1368.00) 
            names(ESUN)    <- c("BAND_1","BAND_2","BAND_3","BAND_4", "BAND_5","BAND_6_VCID_1","BAND_6_VCID_2","BAND_7","BAND_8")
        }
    }
    #else    if (sensor == "Aster")  {
    #    if (spacecraft == "Terra") { ESUN <- c(1828.0, 1559.0, 1045.0, 226.73, 86.50, NA, 74.72, 66.41, 59.83) }
    #}    
    return (ESUN)
}



### not used ?

..rad2ref <- function(radiance, ds, sun_elevation, ESUN) 
{
# not used?
#Conversion of Radiance to Reflectance Top Of Atmosphere for Landsat  TM, ETM+ and Aster
    xfac <- (base::pi * ds * ds) / (ESUN * cos ((90 - sun_elevation)* base::pi/180))
    #reflectance <- (radiance * pi * ds * ds) / (ESUN * cos ((90 - sun_elevation)* pi/180))
    reflectance <- calc(radiance, fun = function(x) {t(t(x)*xfac)}, forcefun=TRUE)
    #reflectance <- radiance / ((cos((90-sun_elevation)*pi/180)/(pi*ds*ds))*ESUN)
    return (reflectance)
}

..rad2temp <- function(radiance_thermal, K) 
{
## not used ?
#Calculate surface temperature for Landsat 
    return(K[2] / (log ((K[1] / radiance_thermal) + 1.0)))
}

