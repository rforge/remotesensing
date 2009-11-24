# Author: Yann Chemin
# IRRI
# License GPL3
# Veon 1, October 2008

#MODIS State Quality Assessment Extractor
#Makes Human-readable images of State Quality Assessment binary bits from MOD09A 500m products (Vermote et al., 2008), this is the proper place to find cloud related information.
#Vermote E.F., Kotchenova S.Y., Ray J.P. MODIS Surface Reflectance User's Guide. Veon 1.2. June 2008. MODIS Land Surface Reflectance Science Computing Facility. Homepage

modis.sqa500a<-function(pixel) 
#Cloud State unsigned int bits[0-1]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#Cloud State unsigned int bits[0-1]
	#00 -> class 0: clear
	#01 -> class 1: cloudy
	#10 -> class 2: mixed
	#11 -> class 3: not set, assumed clear
	pixel <- bitAnd(pixel,3)
	return(pixel)
}
	
modis.sqa500b<-function(pixel) 
#cloud shadow unsigned int bits[2]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#cloud shadow unsigned int bits[2]
	#0 -> class 0: yes
	#1 -> class 1: no
	pixel <- bitShiftR(pixel,2)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}
	
modis.sqa500c<-function(pixel)
 #LAND/WATER FLAG unsigned int bits[3-5]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#LAND/WATER FLAG unsigned int bits[3-5]
	#000 -> class 0: Shallow ocean
	#001 -> class 1: Land
	#010 -> class 2: Ocean coastlines and lake shorelines
	#011 -> class 3: Shallow inland water
	#100 -> class 4: Ephemeral water
	#101 -> class 5: Deep inland water
	#110 -> class 6: Continental/moderate ocean
	#111 -> class 7: Deep ocean
	pixel <- bitShiftR(pixel,3)
	pixel <- bitAnd(pixel,7)
	return(pixel)
}

modis.sqa500d<-function(pixel)
 #AEROSOL QUANTITY unsigned int bits[6-7]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#AEROSOL QUANTITY unsigned int bits[6-7]
	#00 -> class 0: Climatology
	#01 -> class 1: Low
	#10 -> class 2: Average
	#11 -> class 3: High
	pixel <- bitShiftR(pixel,6)
	pixel <- bitAnd(pixel,3)
	return(pixel)
}

modis.sqa500e<-function(pixel)
 #CIRRUS DETECTED unsigned int bits[8-9]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#CIRRUS DETECTED unsigned int bits[8-9]
	#00 -> class 0: None
	#01 -> class 1: Small
	#10 -> class 2: Average
	#11 -> class 3: High
	pixel <- bitShiftR(pixel,8)
	pixel <- bitAnd(pixel,3)
	return(pixel)
}

modis.sqa500f<-function(pixel)
 #Internal Cloud Algorithm Flag unsigned int bits[10]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#Internal Cloud Algorithm Flag unsigned int bits[10]
	#0 -> class 0: Cloud
	#1 -> class 1: No cloud
	pixel <- bitShiftR(pixel,10)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}

modis.sqa500g<-function(pixel)
 #Internal Fire Algorithm Flag unsigned int bits[11]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#Internal Fire Algorithm Flag unsigned int bits[11]
	#0 -> class 0: Fire 
	#1 -> class 1: No fire
	pixel <- bitShiftR(pixel,11)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}

modis.sqa500h<-function(pixel,bandno)
 #MOD35 snow/ice flag unsigned int bits [12]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#MOD35 snow/ice flag unsigned int bits [12]
	#0 -> class 0: Yes (snow)
	#1 -> class 1: No
	pixel <- bitShiftR(pixel,12)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}

modis.sqa500i<-function(pixel)
 #Pixel adjacent to cloud unsigned int bits[13]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#Pixel adjacent to cloud unsigned int bits[13]
	#0 -> class 0: Yes
	#1 -> class 1: No
	pixel <- bitShiftR(pixel,13)
	pixel <- bitAnd(pixel,1)
	return(pixel)
	
}
	
modis.sqa500j<-function(pixel)
 #BRDF correction performed unsigned int bits[14]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#BRDF correction performed unsigned int bits[14]
	#0 -> class 0: Yes
	#1 -> class 1: No
	pixel <- bitShiftR(pixel,14)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}


modis.sqa500k<-function(pixel) 
#Internal Snow Mask unsigned int bits[15]
{
	if (!(require(bitops))) { stop('this function needs the bitops package to be installed') }
	#Internal Snow Mask unsigned int bits[15]
	#0 -> class 0: Snow
	#1 -> class 1: No snow
	pixel <- bitShiftR(pixel,15)
	pixel <- bitAnd(pixel,1)
	return(pixel)
}
