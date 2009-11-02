# Authors: Alice Laborte
# International Rice Research Institute
# Date: March 2009

# Reference:   Irish, R.R., undated, Landsat 7 automatic cloud cover assessment. URL: http://landsathandbook.gsfc.nasa.gov/handbook/pdfs/ACCA_SPIE_paper.pdf

#remove clouds using Landsat TM/ETM+ refstack and thermal band
removeClouds <- function (refstack, traster, filename) {

	cloudM <- cloudMask(refstack, traster)
	nocloudM <- is.na(cloudM)
	NAvalue(nocloudM) <- 0
	if (dataContent(nocloudM) == 'all') {
		nocloudM[nocloudM==0] <- NA
	} 
	#nocloudM[nocloudM==0] <- NA
	maskedS <- applyMask2Stack(refstack, nocloudM, filename)
	
 	return(maskedS)
}

#Apply mask to raster
applyMask2Stack <- function (rstack, mask, filename) {
	if (minValue(mask) != 1 | maxValue(mask) != 1)  {			#mask is not 1 and NA
		mask <- mask > 0
		#mask <- mask / mask
	}
	NAvalue(mask) <- 0
	if (dataContent(mask) == 'all') {
		mask[mask==0] <- NA
	} 
	#mask[mask==0] <- NA
	maskedS <- new("RasterStack")
	
	for (i in 1:nlayers(rstack)) {
		rs		<- raster(rstack, i)
		masked 	<- rs * mask
		if (!missing(filename)) {
			fname <- paste(filename,"_",i,sep="") 
            masked  <- writeRaster(masked, filename=fname, overwrite=TRUE) 

			}	
		maskedS	<- addLayer(maskedS, masked)
	}	
	if (!missing(filename)) {
		filename(maskedS) <- filename
		maskedS <- stackSave(maskedS)
	}

 	return(maskedS)
}


#Create cloud mask for Landsat 7 (ETM+) image
cloudMask <- function (refstack, traster) {

#Pass 1
	band2 	<- raster(refstack,2)
	band3 	<- raster(refstack,3)
	band4 	<- raster(refstack,4)
	band5 	<- raster(refstack,5)
#	ncellInput<- length(!is.na(band2[]))
	band6 	<- disaggregate(traster, fact=2)
	band6	<-setExtent(band6, extent(band2), keepres=TRUE, snap=TRUE)
	band6All 	<- band6
	ncellInput <- ncell(band6) - cellStats(band6, 'countNA')
	
# Filter 1: brightness threshold
	cloudFilter <- band3 >= 0.08
	NAvalue(cloudFilter) <- 0
	if (dataContent(cloudFilter) == 'all') {
		cloudFilter[cloudFilter==0] <- NA
	} 	
	#cloudFilter[cloudFilter==0] <- NA
	notCloud 	<- band3 < 0.08
	
# Filter 2: ndsi
	band2	<- band2 * cloudFilter
	band5	<- band5 * cloudFilter
	NDSI 	<- ndsi(band2, band5)
	snow 	<- NDSI > 0.7
	notCloud 	<- notCloud + snow
	cloudFilter<- NDSI  <= 0.7
	#cloudFilter[cloudFilter==0] <- NA
	
# Filter 3: traster threshold
	band6 	<- band6 * cloudFilter
	notCloud 	<- notCloud + (band6 > 300)
	cloudFilter<- band6  <= 300
	#cloudFilter[cloudFilter==0] <- NA
	
# Filter 4: band 5/6 composite
	band5	<- band5 * cloudFilter
	band6	<- band6 * cloudFilter
	band56c	<- (1 - band5) * band6
	cloudAmb <- band56c > 225
	cloudFilter<- band56c <= 225
	#cloudFilter[cloudFilter==0] <- NA
	
# Filter 5: band 4/3 ratio
	band3	<- band3 * cloudFilter
	band4	<- band4 * cloudFilter
	chk		<- band4 / band3 
	cloudAmb	<- cloudAmb + (chk > 2.0)
	cloudFilter	<- chk <= 2.0
	#cloudFilter[cloudFilter==0] <- NA
	
# Filter 6: band 4/2 ratio
	band2	<- band2 * cloudFilter
	band4	<- band4 * cloudFilter
	chk		<- (band4 / band2) 
	cloudAmb	<- cloudAmb + (chk > 2.0)
	cloudFilter<- chk <= 2.0
	#cloudFilter[cloudFilter==0] <- NA
	
# Filter 7: band 4/5 ratio
	band4	<- band4 * cloudFilter
	band5	<- band5 * cloudFilter
	chk		<- band4 / band5
	cloudAmb	<- cloudAmb + (chk > 1.0)
	cloudFilter<- chk > 1.0
	#cloudFilter[cloudFilter==0] <- NA
	
# Filter 8: band 5/6 composite
	band56c		<- band56c * cloudFilter
	warmCloud 	<- band56c >= 210
	coldCloud 	<- band56c < 210

# Pass 2
	snowProp <- cellStats(snow, 'sum') / ncellInput
	if (snowProp > 0.01) {
		cloudAmb <- cloudAmb + warmCloud
		cloudFilter <- coldCloud
		#cloudFilter[cloudFilter == 0] <- NA
	}
	band6 		<- band6 * cloudFilter
	if (is.finite(band6@data@min)) {
		cloudMinTemp 	<- minValue(band6)
		cloudMaxTemp	<- maxValue(band6)
	}
	else {
		cloudMinTemp 	<- cellStats(band6, 'min')
		cloudMaxTemp	<- cellStats(band6, 'max')
	}
	cloudAvgTemp	<- cellStats(band6, 'mean')
	cloudSdTemp	<- cellStats(band6, 'sd')
	#cloudSkewTemp <- 0
	
	#skew <- function(z, zmean, zsd) {
	#	z <- z[!is.na(z)]
	#	skew <- sum((z - zmean) ^3)
	#	skew <- ((skew / zsd)^3)/ length(z)
	#	return(skew)
	#}

	cloudSkewTemp <- cellStats(band6, 'skew', zmean=cloudAvgTemp, zsd=cloudSdTemp)

	#cloudSkewTemp <- skew(band6[], cloudAvgTemp, cloudSdTemp)
	desertProp <- ((cellStats(cloudFilter, 'sum')) / ncellInput ) * 1.0
	coldCloudProp	<- ((cellStats(coldCloud, 'sum')) / ncellInput) * 1.0
	
	if (desertProp > 0.5 & coldCloudProp > 0.004 & cloudAvgTemp < 295) {
		tmax <- quantile(band6,  na.rm=TRUE, probs=0.9875)
		tU 	<- quantile(band6,  na.rm=TRUE, probs=0.975)
		tL 	<- quantile(band6,  na.rm=TRUE, probs=0.835)
		if (cloudSkewTemp > 0) {
			shift <- (max(cloudSkewTemp,1))* cloudSdTemp
			tU 	<- max(tU + shift, tmax)
			tL 	<- tL + shift 
		}
		
		ct			<- cloudAmb
		NAvalue(ct) <- 0
		if (dataContent(ct) == 'all') {
			ct[ct==0] <- NA
		} 
		#ct[ct==0]		<- NA
		band6P2		<- band6All * ct
		cloudU 		<- band6P2 < tU & band6P2 >= tL
		NAvalue(cloudU) <- 0
		if (dataContent(cloudU) == 'all') {
			cloudU[cloudU==0] <- NA
		}
		#cloudU[cloudU==0]	<- NA
		band6U 		<- band6P2  * cloudU
		band6UProb 	<- cellStats(band6U, 'sum') / ncellInput
		
		cloudL 		<- band6P2 < tL
		NAvalue(cloudL) <- 0
		if (dataContent(cloudL) == 'all') {
			cloudL[cloudL==0] <- NA
		}
		#cloudL[cloudL==0] <- NA
		band6L 		<- band6P2  * cloudL
		band6LProb 	<- cellStats(band6L, 'sum')/ncellInput
		mband6U 	<- cellStats(band6U, 'mean')
		if (band6UProb > 0.4 | mband6U > 295 |  snowProp > 0.01) {
			cloudT 		<- band6P2 < tL
			band6T 		<- band6P2  * cloudT
			NAvalue(band6T) <- 0
			if (dataContent(band6T) == 'all') {
				band6T[band6T==0] <- NA
			}
			#band6T[band6T==0] <- NA
			band6TProb 	<- cellStats(band6T, 'sum')/ncellInput
			
			if (band6UProb > 0.4 | mband6U > 295) {
				cloudMask <- cloudFilter
				if (band6LProb > 0.4 | cellStats(band6L, 'mean') > 295) {
					cloudMask <- cloudFilter
				}
				else {
					cloudMask <- (cloudFilter == 1) + (cloudL == 1) 
					cloudMask <- cloudMask > 0
				}
			}	
			else {
				cloudMask <- (cloudFilter == 1) + (cloudL == 1) + (cloudU == 1)
				cloudMask <- cloudMask > 0
			}
		}
		
	}
	#pass two is bypassed
	else cloudMask <- coldCloud
	
	#majority analysis on o values in cloudMask
	maj <- focal(cloudMask, fun=modal, ngb=3, keepdata=TRUE)
	
	#cloudMask[cloudMask==0] <- maj[cloudMask==0]
	
	cloudMask <- (cloudMask + maj) > 0
	
	#cloudMask[cloudMask==0] <- NA
	return(cloudMask)
}