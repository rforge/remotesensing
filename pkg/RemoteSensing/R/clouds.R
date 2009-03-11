# Authors: Alice Laborte
# International Rice Research Institute
# Date: March 2009

# Reference:   Irish, R.R., undated, Landsat 7 automatic cloud cover assessment. URL: http://landsathandbook.gsfc.nasa.gov/handbook/pdfs/ACCA_SPIE_paper.pdf

#Apply mask to raster
applyMask2Stack <- function (rstack, mask, outfilename) {
	
	}

 	return(maskedS)
}

#Create cloud mask for Landsat 7 (ETM+) image
cloudMask <- function (reflectance, temperature) {

#Pass 1
	band2 	<- asRasterLayer(reflectance,2)
	band3 	<- asRasterLayer(reflectance,3)
	band4 	<- asRasterLayer(reflectance,4)
	band5 	<- asRasterLayer(reflectance,5)
	ncellInput<- length(!is.na(band2[]))
	band6 	<- disaggregate(temperature, fact=2)
	band6	<-setExtent(band6, getBbox(band2), keepres=FALSE, snap=FALSE)
	band6All 	<- band6
	
# Filter 1: brightness threshold
	cloudFilter <- band3 >= 0.08
	cloudFilter[cloudFilter==0] <- NA
	notCloud 	<- band3 < 0.08
	
# Filter 2: ndsi
	band2	<- band2 * cloudFilter
	band5	<- band5 * cloudFilter
	NDSI 	<- ndsi(band2, band5)
	snow 	<- NDSI > 0.7
	notCloud 	<- notCloud + snow
	snow[snow==0] <- NA
	cloudFilter<- NDSI  <= 0.7
	cloudFilter[cloudFilter==0] <- NA
	
# Filter 3: temperature threshold
	band6 	<- band6* cloudFilter
	notCloud 	<- notCloud + (band6 > 300)
	cloudFilter<- band6  <= 300
	cloudFilter[cloudFilter==0] <- NA
	
# Filter 4: band 5/6 composite
	band5	<- band5 * cloudFilter
	band6	<- band6 * cloudFilter
	band56c	<- (1 - band5) * band6
	cloudAmb <- band56c > 225
	cloudFilter<- band56c <= 225
	cloudFilter[cloudFilter==0] <- NA
	
# Filter 5: band 4/3 ratio
	band3	<- band3 * cloudFilter
	band4	<- band4 * cloudFilter
	chk		<- band4 / band3
	cloudAmb	<- cloudAmb + (chk > 2.0)
	cloudFilter	<- chk <= 2.0
	cloudFilter[cloudFilter==0] <- NA
	
# Filter 6: band 4/2 ratio
	band2	<- band2 * cloudFilter
	band4	<- band4 * cloudFilter
	chk		<- band4 / band2
	cloudAmb	<- cloudAmb + (chk > 2.0)
	cloudFilter<- chk <= 2.0
	cloudFilter[cloudFilter==0] <- NA
	
# Filter 7: band 4/5 ratio
	band4	<- band4 * cloudFilter
	band5	<- band5 * cloudFilter
	chk		<- band4 / band5
	cloudAmb	<- cloudAmb + (chk > 1.0)
	cloudFilter<- chk > 1.0
	cloudFilter[cloudFilter==0] <- NA
	
# Filter 8: band 5/6 composite
	band56c		<- band56c * cloudFilter
	warmCloud 	<- band56c >= 210
	coldCloud 	<- band56c < 210

# Pass 2
	snowProp <- cellStats(snow, sum, na.rm=TRUE) / ncellInput
	if (snowProp > 0.01) {
		cloudAmb <- cloudAmb + warmCloud
		cloudFilter <- coldCloud
		cloudFilter[cloudFilter == 0] <- NA
	}
	band6 		<- band6 * cloudFilter
	if (is.finite(band6@data@min)) {
		cloudMinTemp 	<- band6@data@min
		cloudMaxTemp	<- band6@data@max 
	}
	else {
		cloudMinTemp 	<- cellStats(band6, min, na.rm=TRUE)
		cloudMaxTemp	<- cellStats(band6, max, na.rm=TRUE) }
	cloudAvgTemp	<- cellStats(band6, mean, na.rm=TRUE)
	cloudSdTemp	<- cellStats(band6, sd, na.rm=TRUE)
	cloudSkewTemp <- 0
	
	skew <- function(z, zmean, zsd) {
		z <- z[!is.na(z)]
		skew <- 0
		for (i in 1:length(z)) {
			skew <- skew + ((z[i] - zmean) ^3)
		}
		skew <- ((skew / zsd)^3)/ length(z)
	return(skew)
	}
	cloudSkewTemp <- skew(band6[], cloudAvgTemp, cloudSdTemp)
	desertProp <- ((cellStats(cloudFilter, sum, na.rm=TRUE)) / ncellInput ) * 1.0
	coldCloudProp	<- ((cellStats(coldCloud, sum, na.rm=TRUE)) / ncellInput) * 1.0
	
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
		ct[ct==0]		<- NA
		band6P2		<- band6All * ct
		cloudU 		<- band6P2 < tU & band6P2 >= tL
		cloudU[cloudU==0]	<- NA
		band6U 		<- band6P2  * cloudU
		band6UProb 	<- cellStats(band6U, sum, na.rm=TRUE) / ncellInput
		
		cloudL 		<- band6P2 < tL
		cloudL[cloudL==0] <- NA
		band6L 		<- band6P2  * cloudL
		band6LProb 	<- cellStats(band6L, sum, na.rm=TRUE)/ncellInput
	
		if (band6UProb > 0.4 | mean(band6U[]) > 295 |  snowProp > 0.01) {
			cloudT 		<- band6P2 < tL
			band6T 		<- band6P2  * cloudT
			band6T[band6T==0] <- NA
			band6TProb 	<- cellStats(band6T, sum, na.rm=TRUE)/ncellInput
			
			if (band6UProb > 0.4 | cellStats(band6U, mean, na.rm=TRUE) > 295) {
				cloudMask <- cloudFilter
				if (band6LProb > 0.4 | cellStats(band6L, mean, na.rm=TRUE) > 295) {
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
	maj <- neighborhood(cloudMask, fun=modal, ngb=3, keepdata=TRUE)
	
	#cloudMask[cloudMask==0] <- maj[cloudMask==0]
	
	for (i in 1:ncell(cloudMask)) {
		if (!is.na(cloudMask[i]) & !cloudMask[i]) {
			cloudMask[i] <- maj[i]
		}
	}
	
	cloudMask[cloudMask==0] <- NA
	return(cloudMask)
}