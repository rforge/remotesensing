# Author:s Yann Chemin & Alice Laborte
# IRRI
#13 February 2009

#Landsat and ASTER Calibration
#Conversion of DN to radiance and to reflectance
#Conversion to temperature

dn2rad_landsat5 <- function(c_year,c_month,c_day,year,band,DN) {
#Conversion of DN to Radiance for Landsat 5TM
#c_year, c_month, c_day are from the NLAPS report of processing completion.
#year, month and day are the satellite overpass characteristics
#dn2rad_landsat5<- function(c_year,c_month,c_day,year,month,day,band,DN)
	if (c_year<2003) gain_mode<-1
	else if (c_year==2003) 
		if (c_month<5)
			gain_mode<-1
		else if (c_month==5)
			if (c_day<5)
				gain_mode<-1
			else
				gain_mode<-2
		else
			gain_mode<-2
	else if (c_year>2003 & c_year<2007)
		gain_mode<-2
	else if (c_year==2007)
		if (c_month<5)
			gain_mode<-2
		else if (year<1992)
			gain_mode<-3
		else
			gain_mode<-4
	if (gain_mode==1)
		if (band==1)
			gain <- 0.602431
			bias <- -1.52
		if (band==2)
			gain <- 1.175100
			bias <- -2.84
		if (band==3)
			gain <- 0.805765
			bias <- -1.17
		if (band==4)
			gain <- 0.814549
			bias <- -1.51
		if (band==5)
			gain <- 0.108078
			bias <- -0.37
		if (band==6)
			gain <- 0.055158
			bias <- 1.2378
		if (band==7)
			gain <- 0.056980
			bias <- -0.15
	if (gain_mode==2)
		if (band==1)
			gain <- 0.762824
			bias <- -1.52
		if (band==2)
			gain <- 1.442510
			bias <- -2.84
		if (band==3)
			gain <- 1.039880
			bias <- -1.17
		if (band==4)
			gain <- 0.872588
			bias <- -1.51
		if (band==5)
			gain <- 0.119882
			bias <- -0.37
		if (band==6)
			gain <- 0.055158
			bias <- 1.2378
		if (band==7)
			gain <- 0.065294
			bias <- -0.15
	if (gain_mode==3)
		if (band==1)
			gain <- 0.668706
			bias <- -1.52
		if (band==2)
			gain <- 1.317020
			bias <- -2.84
		if (band==3)
			gain <- 1.039880
			bias <- -1.17
		if (band==4)
			gain <- 0.872588
			bias <- -1.51
		if (band==5)
			gain <- 0.119882
			bias <- -0.37
		if (band==6)
			gain <- 0.055158
			bias <- 1.2378
		if (band==7)
			gain <- 0.065294
			bias <- -0.15
	if (gain_mode==4)
		if (band==1)
			gain <- 0.762824
			bias <- -1.52
		if (band==2)
			gain <- 1.442510
			bias <- -2.84
		if (band==3)
			gain <- 1.039880
			bias <- -1.17
		if (band==4)
			gain <- 0.872588
			bias <- -1.51
		if (band==5)
			gain <- 0.119882
			bias <- -0.37
		if (band==6)
			gain <- 0.055158
			bias <- 1.2378
		if (band==7)
			gain <- 0.065294
			bias <- -0.15
	result <- gain * DN + bias
	return (result)
}


 dn2rad_landsat7<- function( Lmin, LMax, QCalMax, QCalmin, DN ) {
	#Conversion of DN to Radiance for Landsat 7ETM+
	#http//ltpwww.gsfc.nasa.gov/IAS/handbook/handbook_htmls/chapter11/chapter11.html#section11.3 
	#dn2rad_landsat7( Lmin, LMax, QCalMax, QCalmin, DN )
	gain 	<- (LMax-Lmin)/(QCalMax-QCalmin)
	offset 	<- Lmin
	result 	<- gain * (1.0*DN - QCalmin) + offset
	return (result)
}

 rad2ref_landsat7<- function( radiance, doy, sun_elevation, kexo ) {
	#Conversion of Radiance to Reflectance Top Of Atmosphere for Landsat 7ETM+
	#rad2ref_landsat7( radiance, doy, sun_elevation, kexo )
	ds <- ( 1.0 + 0.01672 * sin( 2 * pi * ( doy - 93.5 ) / 365 ) )
	result <- (radiance/((cos((90-sun_elevation)*pi/180)/(pi*ds*ds))*kexo))
	return (result)
}

 tempk_landsat7<- function( l6 ) {
	#Surface temperature for Landsat 7ETM+
	#tempk_landsat7( l6 )
	result <- 1282.71 / (log ((666.09 / l6) + 1.0))
	return (result)
}

 tempk_landsat5<- function( l6 ) {
	#Surface temperature for Landsat 5TM
	#tempk_landsat5( l6 )
	result <- 1260.56 / (log ((607.76 / l6) + 1.0))
	return (result)
}

 tempk_landsat4<- function( l6 ) {
	#Surface temperature for Landsat 5TM
	#tempk_landsat4( l6 )
	result <- 1284.30 / (log ((671.62 / l6) + 1.0))
	return (result)
}


dn2rad_aster <- function( Lgain, Loffset, DN ) {
	#Conversion of DN to Radiance for Aster
	#rad2ref_aster(  Lgain, Loffset, DN  )
	result <- Lgain * DN + Loffset
	return (result)
}

rad2ref_aster <- function( radiance, doy, sun_elevation, kexo ) {
	#Conversion of Radiance to Reflectance for ASTER
	#rad2ref_aster( radiance, doy, sun_elevation, kexo )
	ds <- ( 1.0 + 0.01672 * sin( 2 * pi * ( doy - 93.5 ) / 365 ) )
	result <- radiance / ( ( cos ( ( 90.0 - sun_elevation ) * pi / 180.0 ) / ( pi * ds * ds ) ) * kexo )
	return (result)
}

Lgain <- function( spacecraft_id, sensor_id, band_id) {
	#Gain for Dn to Rad conversion
	#This is used for processing radiance of Terra Aster images.
	#Spacecraft_id Terra
	#	Sensor_id Aster
	#		band_id band1, band2, band3, band4, band5, band7, band8, band9
	#Lgain(spacecraft_id, sensor_id, band_id)
	if (spacecraft_id == "Terra")
		if (sensor_id == "Aster")
			if (band_id == "band1")
				lgain <- 0.676
			else if (band_id == "band2")
				lgain <- 0.862
			else if (band_id == "band3")
				lgain <- 0.217
			else if (band_id == "band4")
				lgain <- 0.0696
			else if (band_id == "band5")
				lgain <- 0.0696
			else if (band_id == "band7")
				lgain <- 0.0696
			else if (band_id == "band8")
				lgain <- 0.0696
			else if (band_id == "band9")
				lgain <- 0.0318
			else
				lgain <- 0.0
		else
			lgain <- 0.0
	else
		lgain <- 0.0
	return (lgain)
}

Loffset <- function( spacecraft_id, sensor_id, band_id) {
	#Offset for Dn to Rad conversion
	#This is used for processing radiance of Terra Aster images.
	#Spacecraft_id Terra
	#	Sensor_id Aster
	#		band_id band1, band2, band3, band4, band5, band7, band8, band9
	#Lgain(spacecraft_id, sensor_id, band_id)
	if (spacecraft_id == "Terra")
		if (sensor_id == "Aster")
			if (band_id == "band1")
				loffset <- -0.676
			else if (band_id == "band2")
				loffset <- -0.862
			else if (band_id == "band3")
				loffset <- -0.217
			else if (band_id == "band4")
				loffset <- -0.0696
			else if (band_id == "band5")
				loffset <- -0.0696
			else if (band_id == "band7")
				loffset <- -0.0696
			else if (band_id == "band8")
				loffset <- -0.0696
			else if (band_id == "band9")
				loffset <- -0.0318
			else
				loffset <- 0.0
		else
			loffset <- 0.0
	else
		loffset <- 0.0
	return(loffset)
}

kexo <- function( spacecraft_id, sensor_id, band_id) {
	#Sun exo-atmospheric irridiance [W/m2/sr]
	#This is used for processing surface reflectance.
	#Spacecraft_id Landsat1
	#	Sensor_id MSS
	#		band_id band1, band2, band3, band4
	#Spacecraft_id Landsat2
	#	Sensor_id MSS
	#		band_id band1, band2, band3, band4
	#Spacecraft_id Landsat3
	#	Sensor_id MSS
	#		band_id band1, band2, band3, band4
	#Spacecraft_id Landsat4
	#	Sensor_id MSS
	#		band_id band1, band2, band3, band4
	#	Sensor_id TM
	#		band_id band1, band2, band3, band4, band5, band7
	#Spacecraft_id Landsat5
	#	Sensor_id MSS
	#		band_id band1, band2, band3, band4
	#	Sensor_id TM
	#		band_id band1, band2, band3, band4, band5, band7
	#Spacecraft_id Landsat7
	#	Sensor_id ETM+
	#		band_id band1, band2, band3, band4, band5, band7, band8
	#Spacecraft_id Terra
	#	Sensor_id Aster
	#		band_id band1, band2, band3, band4, band5, band7, band8, band9
	#Sources:  Landsat 1-4 MSS: Markham, B.L. and Barker, J.L. 1986. Landsat MSS and TM post-calibration dynamic ranges, exoatmospheric reflectances and at-satellite temperatures. Landsat Technical Note 1, August 1986.
	#                Landsat 4 & 5 TM: Chander, G., and Markham, B. 2003. Revised Landsat-5 TM radiometric calibration procedures and postcalibration dynamic ranges. IEEE Transactions on Geoscience and Remote Sensing, 41, 2674-2677
	if (spacecraft_id == "Landsat1")
		 if (sensor_id == "MSS")
			if (band_id == "band1")
				kexo <- 1852
			else if (band_id == "band2")
				kexo <- 1584
			else if (band_id == "band3")
				kexo <- 1276
			else if (band_id == "band4")
				kexo <- 904
			else
				kexo <- 0.0
		else
			kexo <- 0.0	
	else if (spacecraft_id == "Landsat2")
		 if (sensor_id == "MSS")
			if (band_id == "band1")
				kexo <- 1856
			else if (band_id == "band2")
				kexo <- 1559
			else if (band_id == "band3")
				kexo <- 1269
			else if (band_id == "band4")
				kexo <- 906
			else
				kexo <- 0.0
		else
			kexo <- 0.0	
	else if (spacecraft_id == "Landsat3")
		 if (sensor_id == "MSS")
			if (band_id == "band1")
				kexo <- 1860
			else if (band_id == "band2")
				kexo <- 1571
			else if (band_id == "band3")
				kexo <- 1289
			else if (band_id == "band4")
				kexo <- 910
			else
				kexo <- 0.0
		else
			kexo <- 0.0	
	else if (spacecraft_id == "Landsat4")
		if (sensor_id == "MSS")
			if (band_id == "band1")
				kexo <- 1851
			else if (band_id == "band2")
				kexo <- 1593
			else if (band_id == "band3")
				kexo <- 1260
			else if (band_id == "band4")
				kexo <- 878
			else
				kexo <- 0.0
		else if (sensor_id == "TM")
			if (band_id == "band1")
				kexo <- 1957.0
			else if (band_id == "band2")
				kexo <- 1825.0
			else if (band_id == "band3")
				kexo <- 1557.0
			else if (band_id == "band4")
				kexo <- 1033.0
			else if (band_id == "band5")
				kexo <- 214.9
			else if (band_id == "band7")
				kexo <- 80.72
			else
				kexo <- 0.0
		else
			kexo <- 0.0
	else if (spacecraft_id == "Landsat5")
		if (sensor_id == "MSS")
			if (band_id == "band1")
				kexo <- 1849
			else if (band_id == "band2")
				kexo <- 1595
			else if (band_id == "band3")
				kexo <- 1253
			else if (band_id == "band4")
				kexo <- 870
			else
				kexo <- 0.0
		else if (sensor_id == "TM")
			if (band_id == "band1")
				kexo <- 1957.0
			else if (band_id == "band2")
				kexo <- 1826.0
			else if (band_id == "band3")
				kexo <- 1554.0
			else if (band_id == "band4")
				kexo <- 1036.0
			else if (band_id == "band5")
				kexo <- 215.0
			else if (band_id == "band7")
				kexo <- 80.67
			else
				kexo <- 0.0
		else
			kexo <- 0.0
	else if (spacecraft_id == "Landsat7")
		if (sensor_id == "ETM+")
			if (band_id == "band1")
				kexo <- 1969.0
			else if (band_id == "band2")
				kexo <- 1840.0
			else if (band_id == "band3")
				kexo <- 1551.0
			else if (band_id == "band4")
				kexo <- 1044.0
			else if (band_id == "band5")
				kexo <- 225.7
			else if (band_id == "band7")
				kexo <- 82.07
			else if (band_id == "band8")
				kexo <- 1385.64 # Self calculated value...
			else
				kexo <- 0.0
		else
			kexo <- 0.0
	else if (spacecraft_id == "Terra")
		if (sensor_id == "Aster")
			if (band_id == "band1")
				kexo <- 1828.0
			else if (band_id == "band2")
				kexo <- 1559.0
			else if (band_id == "band3")
				kexo <- 1045.0
			else if (band_id == "band4")
				kexo <- 226.73
			else if (band_id == "band5")
				kexo <- 86.50
			else if (band_id == "band7")
				kexo <- 74.72
			else if (band_id == "band8")
				kexo <- 66.41
			else if (band_id == "band9")
				kexo <- 59.83
			else
				kexo <- 0.0
		else
			kexo <- 0.0
	else
		kexo <- 0.0
	return (kexo)
}