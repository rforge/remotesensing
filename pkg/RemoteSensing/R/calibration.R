# Authors: Alice Laborte & Yann Chemin
# International Rice Research Institute
#Date: February 2009

#Landsat and ASTER Calibration
#Conversion of DN to radiance and to reflectance
#Conversion  of thermal DN to temperature
#Sources of calibration parameters :  
#	Landsat 1-4 MSS: Markham, B.L. and Barker, J.L. 1986. Landsat MSS and TM post-calibration dynamic ranges, exoatmospheric reflectances and at-satellite temperatures. Landsat Technical Note 1, August 1986.
# 	Landsat 4 & 5 TM: Chander, G., and Markham, B. 2003. Revised Landsat-5 TM radiometric calibration procedures and postcalibration dynamic ranges. IEEE Transactions on Geoscience and Remote Sensing, 41, 2674-2677
#                                       Chander, G., Markham, B.L, Barsi, J.A. 2007. Revised Landsat-5 Thematic Mapper radiometric calibration. IEEE Transactions on Geoscience and Remote Sensing Letters, 4, 490-494
#        Landsat 7 ETM+:    Landsat 7 Science Data Users Handbook. Chapter 9 - Calibration parameter file. URL: http://landsathandbook.gsfc.nasa.gov/handbook/handbook_htmls/chapter9/chapter9.html
#       ASTER:                    ASTER Surface Reflectance/Radiance VNIR/SWIR Product. URL: http://asterweb.jpl.nasa.gov/content/03_data/01_Data_Products/release_aster_surface_reflectance.htm

#Conversion of DN to reflectance
dn2ref  <- function(SatImgObject, band, outfilename) {
	DN			<- rasterFromFile(x@band_filenames[band])
	gain 		<- SatImgObject@gain[band]
	bias 		<- SatImgObject@bias[band]
	lmax 		<- SatImgObject@lmax[band]
	lmin 	      		<- SatImgObject@lmin[band]
	qcalmax 		<- SatImgObject@qcalmax[band]
	qcalmin  		<-SatImgObject@qcalmin[band]
	doy			<- SatImgObject@doy
	sun_elevation	<- SatImgObject@sun_elevation
	ESUN		<- esun(SatImgObject@spacecraft_id, SatImgObject@sensor_id)
	names(ESUN)	<- SatImgObject@bands
	radiance 		<- dn2rad(DN, gain, bias, lmax, lmin, qcalmax, qcalmin)
	doy                 	<- as.integer(format(SatImgObject@acquisition_date,"%j"))
		reflectance 	<- rad2ref(radiance, doy, sun_elevation, ESUN[band])
	#reflectance <- calc(radiance, fun=rad2ref(radiance, doy, sun_elevation, ESUN[band]), filename = outfilename)
	return(reflectance)
}

#Conversion of DN to temperature
dn2temp <- function(SatImgObject, band) {
	DN			<- rasterFromFile(x@band_filenames[band])
	gain 		<- SatImgObject@gain[band]
	bias 		<- SatImgObject@bias[band]
	lmax 		<- SatImgObject@lmax[band]
	lmin 	      		<- SatImgObject@lmin[band]
	qcalmax 		<- SatImgObject@qcalmax[band]
	qcalmin  		<-SatImgObject@qcalmin[band]
	radiance 		<- dn2rad(DN, gain, bias, lmax, lmin, qcalmax, qcalmin)
	temp	 	<- rad2temp(radiance, SatImgObject)
	temp	 	<- rad2temp(radiance, SatImgObject)
	#temp <- calc(radiance, fun=rad2temp(radiance, SatImgObj), filename = outfilename)
	return(temp)
}

#Conversion of DN to radiance 
dn2rad <- function(DN, gain, bias, lmax, lmin, qcalmax, qcalmin) {
	if (is.na(gain)) {
		if (!is.na(lmax)) {
			gain <- (lmax - lmin) / (qcalmax - qcalmin)
			bias <- lmin - gain * qcalmin
			radiance <- gain * DN + bias
		}
		else radiance <- NA 
	}
	else radiance <- gain * DN + bias
	return (radiance)
}

#Conversion of Radiance to Reflectance Top Of Atmosphere for Landsat  TM, ETM+ and Aster
 rad2ref <- function(radiance, doy, sun_elevation, ESUN) {
	ds <- ( 1.0 + 0.01672 * sin( 2 * pi * ( doy - 93.5 ) / 365 ) )
	#reflectance <- (radiance * pi * ds * ds) / (ESUN * cos ((90 - sun_elevation)* pi/180))
	reflectance <- radiance / ((cos((90-sun_elevation)*pi/180)/(pi*ds*ds))*ESUN)
	return (reflectance)
}

# Calculate gain & bias for Landsat MSS, Landsat TM and aster
GainBias <- function(spacecraft_id, sensor_id, acquisition_date, product_creation_date) {
	
chk <- TRUE
if (sensor_id == "MSS" ) {
	if (spacecraft_id == "Landsat1") {
		lmin  <- c(0.0, 0.0, 0.0, 0.0)
		lmax <- c(248.0, 200.0, 176.0, 153.0) }
	else if (spacecraft_id == "Landsat2")
		if (product_creation_date < "07/16/1975") {
			lmin  <- c(10.0, 7.0, 7.0, 5.0)
			lmax <- c(210.0, 156.0, 140.0, 138.0) }
		else {
			lmin  <- c(8.0, 6.0, 6.0, 4.0)
			lmax <- c(263.0, 176.0, 152.0, 130.0) }
	else if (spacecraft_id == "Landsat3")
		if (product_creation_date < as.Date("06/01/1978", "%m/%d/%Y")) {
			lmin  <- c(4.0, 3.0, 3.0, 1.0)
			lmax <- c(220.0, 175.0, 145.0, 147.0) }
		else {
			lmin  <- c(4.0, 3.0, 3.0, 1.0)
			lmax <- c(259.0, 179.0, 149.0, 128.0) }
	else if (spacecraft_id == "Landsat4")
		if (acquisition_date < as.Date("08/26/1982", "%m/%d/%Y")) {
			lmin  <- c(2.0, 4.0, 4.0, 3.0)
			lmax <- c(250.0, 180.0, 150.0, 133.0) }
		else if (acquisition_date < as.Date("04/01/83","%m/%d/%Y")) {
			lmin  <- c(2.0, 4.0, 4.0, 3.0)
			lmax <- c(230.0, 180.0, 130.0, 133.0) }
		else {
			lmin  <- c(4.0, 4.0, 5.0, 4.0)
			lmax <- c(238.0, 164.0, 142.0, 116.0) }
	else if (spacecraft_id == "Landsat5")
		if (acquisition_date < as.Date("04/06/1984", "%m/%d/%Y")) {
			lmin  <- c(4.0, 3.0, 4.0, 2.0)
			lmax <- c(240.0, 170.0, 150.0, 127.0) }
		else if (acquisition_date < as.Date("11/8/1984", "%m/%d/%Y")) {
			lmin  <- c(3.0, 3.0, 4.0, 3.0)
			lmax <- c(268.0, 179.0, 159.0, 123.0) }
		else {
			lmin  <- c(3.0, 3.0, 5.0, 3.0)
			lmax <- c(268.0, 179.0, 148.0, 123.0) }
	else chk <- FALSE
	
	if (chk) {
		if (spacecraft_id %in% c("Landsat1", "Landsat2", "Landsat3") & acquisition_date < as.Date("02/01/1979","%m/%d/%Y")) qcalmax <- c(127, 127, 127, 63)
		else if (spacecraft_id == "Landsat4" & product_creation_date < as.Date("10/22/1982", "%m/%d/%Y")) qcalmax <- c(127, 127, 127, 63)
		else qcalmax <- c(127, 127, 127, 127)
		gain <- (lmax - lmin) / qcalmax
		bias <- lmin
		bandnames <- c("band1","band2","band3","band4")
	}	
}
else if (sensor_id == "TM") {
	if (spacecraft_id == "Landsat4") {
		if (product_creation_date < as.Date("08/01/1983", "%m/%d/%Y")) {
			lmin  <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 2, -0.15) 
			lmax <- c(158.42, 308.17, 234.63, 224.32, 32.42, 15.64, 17.0) }
		else if (product_creation_date < as.Date("1/15/1984", "%m/%d/%Y")) {
			lmin  <- c(0.0, 0.0, 0.0, 0.0, 0.0, 4.84, 0.0) 
			lmax <- c(142.86, 291.25, 225.0, 214.29, 30.0, 12.4, 15.93) }
		else {
			lmin  <- c(-1.5, -2.8, -1.2, -1.5, -0.37, 1.238, -0.15) 
			lmax <- c(152.1, 296.8, 204.3, 206.2, 27.19, 15.6, 14.38) }
		qcalmax <- 255
		gain <- (lmax - lmin) / qcalmax
		bias <- lmin
	}
	else if (spacecraft_id == "Landsat5") {
		if (product_creation_date < as.Date("05/04/2003", "%m/%d/%Y")) {
			gain <- c(0.602431, 1.175100, 0.805765, 0.814549, 0.108078, 0.055158, 0.056980)
			bias <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 1.2378, -0.15) }
		else if (product_creation_date < as.Date("04/01/2007", "%m/%d/%Y"))  {
			gain <- c(0.762824, 1.442510, 1.039880, 0.872588, 0.119882, 0.055158, 0.065294)
			bias <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 1.2378, -0.15) }
		else if (acquisition_date < as.Date("12/31/1991", "%m/%d/%Y")) {
			gain <- c(0.668706, 1.317020, 1.039880, 0.872588, 0.119882, 0.055158, 0.065294)
			bias <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 1.2378, -0.15) }
		else  {
			gain <- c(0.762824, 1.442510, 1.039880, 0.872588, 0.119882, 0.055158, 0.065294)
			bias <- c(-1.52, -2.84, -1.17, -1.51, -0.37, 1.2378, -0.15) }
	}
	else chk <- FALSE
	
	if (chk)  bandnames <- c("band1","band2","band3","band4","band5","band6","band7")
}
else if (sensor_id == "Aster") {
	gain <- c(0.676, 0.708, 0.862, 0.2174, 0.0696, 0.0625, 0.0597, 0.0417, 0.0318)
	bias <- c(-0.676, -0.708, -0.862, -0.2174, -0.0696, -0.0625, -0.0597, -0.0417, -0.0318)
	bandnames <- c("band1","band2","band3","band4","band5","band6","band7","band8","band9")
}
else chk <- FALSE

if (chk) {
	result <- cbind(gain,bias)
	rownames(result)<-bandnames }
else result <- NA

return (result)
}

# Calculate the sun exo-atmospheric irridiance [W/m2/sr].  This is used for processing surface reflectance.
esun <- function(spacecraft_id, sensor_id) {
# Spacecraft_id Landsat1
#	Sensor_id MSS		(band1, band2, band3, band4)
#Spacecraft_id Landsat2
#	Sensor_id MSS		(band1, band2, band3, band4)
#Spacecraft_id Landsat3
#	Sensor_id MSS		(band1, band2, band3, band4)
#Spacecraft_id Landsat4
#	Sensor_id MSS		(band1, band2, band3, band4)
#	Sensor_id TM		(band_id band1, band2, band3, band4, band5, NA,band7)
#Spacecraft_id Landsat5
#	Sensor_id MSS		(band1, band2, band3, band4)
#	Sensor_id TM		(band1, band2, band3, band4, band5, NA,band7)
#Spacecraft_id Landsat7
#	Sensor_id ETM+		(band1, band2, band3, band4, band5, NA,NA,band7, band8)
#Spacecraft_id Terra
#	Sensor_id Aster		(band1, band2, band3, band4, band5, NA,band7, band8, band9)
	
	if (spacecraft_id == "Landsat1") {
		 if (sensor_id == "MSS")     { ESUN <- c(1852.0, 1584.0, 1276.0, 904.0) }
		 }
	else if (spacecraft_id == "Landsat2") {
		 if (sensor_id == "MSS")     { ESUN <- c(1856.0, 1559.0, 1269.0, 906.0) }
		}
	else if (spacecraft_id == "Landsat3") {
		 if (sensor_id == "MSS")     { ESUN <- c(1860.0, 1571.0, 1289.0, 910.0) }
		 }
	else if (spacecraft_id == "Landsat4") {
		if (sensor_id == "MSS")      { ESUN <- c(1851.0, 1593.0, 1260.0, 878.0)}
		else if (sensor_id == "TM") { ESUN <- c(1957.0, 1825.0, 1557.0, 1033.0, 214.9, NA, 80.72) }
		}
	else if (spacecraft_id == "Landsat5") {
		if (sensor_id == "MSS")      { ESUN <- c(1849.0, 1595.0, 1253.0, 870.0) }
		else if (sensor_id == "TM") {	ESUN <- c(1957.0, 1826.0, 1554.0, 1036.0, 215.0, NA, 80.67) }
		}
	else if (spacecraft_id == "Landsat7") {
		if (sensor_id == "ETM+")    { ESUN <- c(1969.0, 1840.0, 1551.0, 1044.0, 225.7, NA, NA, 82.07, 1368.00) }
		}
	#else if (spacecraft_id == "Terra")
	#	if (sensor_id == "Aster")    { ESUN <- c(1828.0, 1559.0, 1045.0, 226.73, 86.50, NA, 74.72, 66.41, 59.83) }
	return (ESUN)
}

#Calculate surface temperature for Landsat 
rad2temp <- function(radiance_thermal, SatImgObject) {
	spacecraft_id <- SatImgObject@spacecraft_id
	sensor_id <- SatImgObject@sensor_id
	
	if (sensor_id %in% c("TM", "ETM+")) {
		K <- K_landsat(spacecraft_id, sensor_id)
		temp <- K[2] / (log ((K[1] / radiance_thermal) + 1.0))
	}
	return(temp)
}

#Landsat thermal calibration constants K1, K2
K_landsat <- function(spacecraft_id, sensor_id) {
	if (spacecraft_id == "Landsat4" & sensor_id == "TM") { K <-  c(671.62, 1284.3) }
	else if (spacecraft_id == "Landsat5" & sensor_id == "TM") { K <-  c(607.76, 1260.56) }
	else if (spacecraft_id == "Landsat7") { K <- c(666.09, 1282.71 ) }
	else K<- NA
	return (K)
}