# Author: Yann Chemin
# IRRI
# License GPL3
# Veon 1, October 2008

#MODIS Quality Assessment Extractor
#Makes Human-readable images of Quality Assessment binary bits from MOD09A/Q products.
#MOD09A@500m does not have "cloud" and "diff_orbit_from_500m" options.

modis.qc250a<-function(pixel)
 #MODLAND QA Bits 250m Unsigned Int bits[0-1]
{
	# MODLAND QA Bits 250m Unsigned Int bits[0-1]
	#00 -> class 0: Corrected product produced at ideal quality -- all bands
	#01 -> class 1: Corrected product produced at less than idel quality -- some or all bands
	#10 -> class 2: Corrected product NOT produced due to cloud effect -- all bands
	#11 -> class 3: Corrected product NOT produced due to other reasons -- some or all bands maybe fill value (Note that a value of [11] overrides a value of [01])
	
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}
	pixel <- bitAnd(pixel,3)
	return(pixel)
}

modis.qc250b<-function(pixel)
 #Cloud State 250m Unsigned Int bits[2-3]
{
	# Cloud State 250m Unsigned Int bits[2-3]
	#00 -> class 0: Clear -- No clouds
	#01 -> class 1: Cloudy
	#10 -> class 2: Mixed
	#11 -> class 3: Not Set ; Assumed Clear
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitShiftR(pixel,2)
	pixel <- bitAnd(pixel,3)
	return(pixel)
}


modis.qc250c<-function(pixel,bandno) 
#Band-wise Data Quality 250m Unsigned Int bits[0-1]
{
	# Band-wise Data Quality 250m Unsigned Int bits[0-1]
	#0000 -> class 0: highest quality
	#0111 -> class 1: noisy detector
	#1000 -> class 2: dead detector; data interpolated in L1B
	#1001 -> class 3: solar zenith ><- 86 degrees
	#1010 -> class 4: solar zenith ><- 85 and < 86 degrees
	#1011 -> class 5: missing input
	#1100 -> class 6: internal constant used in place of climatological data for at least one atmospheric constant
	#1101 -> class 7: correction out of bounds, pixel constrained to extreme allowable value
	#1110 -> class 8: L1B data faulty
	#1111 -> class 9: not processed due to deep ocean or cloud
	#Class 10-15: Combination of bits unused
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitShiftR(pixel,(4 + (4*(bandno-1))))
	pixel <- bitAnd(pixel,15)
	return(pixel)
}
	
modis.qc250d<-function(pixel) 
#Atmospheric correction 250m Unsigned Int bit[12]
{
	# Atmospheric correction 250m Unsigned Int bit[12]
	#0 -> class 0: Not Corrected product
	#1 -> class 1: Corrected product
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitShiftR(pixel,12)
	pixel <- bitAnd(pixel,1)
	return(pixel)
 }


modis.qc250e<-function(pixel)
 #Adjacency correction 250m Unsigned Int bit[13]
{
	# Adjacency correction 250m Unsigned Int bit[13]
	#0 -> class 0: Not Corrected product
	#1 -> class 1: Corrected product
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitShiftR(pixel,13)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}
	
modis.qc250f<-function(pixel)
 #Different orbit from 500m product, 250m Unsigned Int bit[14]
{
	# Different orbit from 500m product, 250m Unsigned Int bit[14]
	#0 -> class 0: same orbit as 500m
	#1 -> class 1: different orbit from 500m
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitShiftR(pixel,14)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}

modis.qc500a<-function(pixel) 
#MODLAND QA Bits 500m long int bits[0-1]
{
	# MODLAND QA Bits 500m long int bits[0-1]
	#00 -> class 0: Corrected product produced at ideal quality -- all bands
	#01 -> class 1: Corrected product produced at less than idel quality -- some or all bands
	#10 -> class 2: Corrected product NOT produced due to cloud effect -- all bands
	#11 -> class 3: Corrected product NOT produced due to other reasons -- some or all bands mayb be fill value (Note that a value of [11] overrides a value of [01])
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitAnd(pixel,3)
	return(pixel)
}
	
modis.qc500c<-function(pixel,bandno)
 #Band-wise Data Quality 500m long Int 
{
	# Band-wise Data Quality 500m long Int 
	#bits[2-5][6-9][10-13][14-17][18-21][22-25][26-29]
	#0000 -> class 0: highest quality
	#0111 -> class 1: noisy detector
	#1000 -> class 2: dead detector; data interpolated in L1B
	#1001 -> class 3: solar zenith ><- 86 degrees
	#1010 -> class 4: solar zenith ><- 85 and < 86 degrees
	#1011 -> class 5: missing input
	#1100 -> class 6: internal constant used in place of climatological data for at least one atmospheric constant
	#1101 -> class 7: correction out of bounds, pixel constrained to extreme allowable value
	#1110 -> class 8: L1B data faulty
	#1111 -> class 9: not processed due to deep ocean or cloud
	#Class 10-15: Combination of bits unused
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitShiftR(pixel,(2 + (4*(bandno-1))))
	pixel <- bitAnd(pixel,15)
	return(pixel)
}
	
modis.qc500d<-function(pixel)
 #Atmospheric correction 500m long Int bit[30]
{
	# Atmospheric correction 500m long Int bit[30]
	#0 -> class 0: Not Corrected product
	#1 -> class 1: Corrected product
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitShiftR(pixel,30)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}

modis.qc500e<-function(pixel)
 #Adjacency correction 500m long Int bit[31]
{
	# Adjacency correction 500m long Int bit[31]
	#0 -> class 0: Not Corrected product
	#1 -> class 1: Corrected product
	if (!(require(bitops))) {stop("You need to install the bitops package to use this function")}

	pixel <- bitShiftR(pixel,31)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}

