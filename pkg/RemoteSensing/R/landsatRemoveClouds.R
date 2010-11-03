# Authors: Alice Laborte & Robert Hijmans
# International Rice Research Institute
# Date: March 2009

# Reference:  Irish, R.R., undated, Landsat 7 automatic cloud cover assessment. 
# http://landsathandbook.gsfc.nasa.gov/handbook/pdfs/ACCA_SPIE_paper.pdf

#remove clouds using Landsat TM/ETM+ refstack and thermal band

removeClouds <- function (x, filename='', ...) {


	cloudMask <- .cloudMask(x)
	nocloudM <- reclass(cloudMask, c(NA,NA,1))
	y <- mask(x, nocloudM)
	x@layers <- stack(y)@layers
 	return(x)
}



#Create cloud mask for Landsat 7 (ETM+) image
.cloudMask <- function(x) {

	if ( ! inherits(x, 'LandsatETMp') ) {
		stop('only implemented for ETM+')
	}

	if (! x@thermal_callibrated & x@callibrated & x@callibration=='reflectance')  {
		stop('not a callibrated image')
	}

	band6 <- disaggregate(raster(x@thermal, 1), fact=2)
	band6 <- crop(band6, extent(x))
	
	
#Pass 1
	band2 	   <- raster(x,2)
	band3 	   <- raster(x,3)
	band4 	   <- raster(x,4)
	band5 	   <- raster(x,5)

# change made based on suggestion by Matteo Mattiuzzi	
	band6All   <- band6
	ncellInput <- ncell(band6) - cellStats(band6, 'countNA')
	
# Filter 1: brightness threshold
	cloudFilter <- band3 >= 0.08
# Ojo: cloudFilter = FALSE = 0 is 'there are clouds'
	
	if (inMemory(cloudFilter)) {
		cloudFilter[cloudFilter==0] <- NA
	} else {
		NAvalue(cloudFilter) <- 0
	}
	
#	notCloud 	<- band3 < 0.08
	
# Filter 2: ndsi
	NDSI 	<- overlay(band2, band5, cloudFilter, fun=function(x,y,z){ return(ndsi(x,y)*z) } )
	snow 	<- NDSI > 0.7
#	notCloud 	<- overlay(notCloud, snow, fun=sum)
#	cloudFilter <- NDSI  <= 0.7
	cloudFilter <- ! snow
	
# Filter 3: traster threshold
#	band6 	    <- overlay(band6, cloudFilter, fun=function(x,y){ return(x*y) } )
#	notCloud 	<- notCloud + (band6 > 300)
	cloudFilter <- ( band6 * cloudFilter ) <= 300
	
# Filter 4: band 5/6 composite
#	band6	<- overlay(band6, cloudFilter, fun=function(x,y){return(x*y)})
#	band56c	<- overlay(band5, band6, fun=function(x,y){return((1 - x) * y)})
	
	band56c	<- overlay(band5, band6, cloudFilter, fun=function(x,y,z){ return((1 - x) * y * z) })
	
#	cloudAmb <- band56c > 225
	cloudFilter <- band56c <= 225
	cloudAmb <- !cloudFilter
	
# Filter 5: band 4/3 ratio
#	band3 	<- overlay(band3, cloudFilter, fun=function(x,y){ return(x*y) } )
#	band4 	<- overlay(band4, cloudFilter, fun=function(x,y){ return(x*y) } )
	chk 	<- overlay(band4, band3, cloudFilter, fun=function(x,y,z){ return( (x/y) * z) } )

	cloudAmb	<- cloudAmb + (chk > 2.0)
	cloudFilter	<- chk <= 2.0
	
# Filter 6: band 4/2 ratio
#	band2 	<- overlay(band2, cloudFilter, fun=function(x,y){ return(x*y) })
#	band4 	<- overlay(band4, cloudFilter, fun=function(x,y){ return(x*y) })
#	chk 	<- overlay(band4, band2, fun=function(x,y){ return(x/y) })
	chk 		<- overlay(band4, band2, cloudFilter, fun=function(x,y,z){ return((x/y) * z) })
	cloudAmb	<- cloudAmb + (chk > 2.0)
	cloudFilter <- chk <= 2.0
	
# Filter 7: band 4/5 ratio
#	band4 	<- overlay(band4, cloudFilter, fun=function(x,y){ return(x*y) } )
#	band5 	<- overlay(band5, cloudFilter, fun=function(x,y){ return(x*y) } )
#	chk 	<- overlay(band4, band5, fun=function(x,y){ return(x/y) })
	chk 		<- overlay(band4, band5, cloudFilter, fun=function(x,y,z){ return((x/y) * z) })
	cloudAmb	<- cloudAmb + (chk > 1.0)
	cloudFilter<- chk > 1.0
	
# Filter 8: band 5/6 composite
	band56c 	<- overlay(band56c, cloudFilter, fun=function(x,y){ return(x*y) })
	warmCloud 	<- band56c >= 210
	coldCloud 	<- band56c < 210

# Pass 2
	snowProp <- cellStats(snow, 'sum') / ncellInput
	if (snowProp > 0.01) {
		cloudAmb <- overlay(cloudAmb, warmCloud, fun=sum)
		cloudFilter <- coldCloud
	}

#	band6	 	<- overlay(band6, cloudFilter, fun=function(x,y){ return(x*y) })
	band6 <- band6 * cloudFilter
	if (is.finite(band6@data@min)) {
		cloudMinTemp 	<- minValue(band6)
		cloudMaxTemp	<- maxValue(band6)
	} else {
		cloudMinTemp 	<- cellStats(band6, 'min')
		cloudMaxTemp	<- cellStats(band6, 'max')
	}
	cloudAvgTemp	<- cellStats(band6, 'mean')
	cloudSdTemp		<- cellStats(band6, 'sd')

	cloudSkewTemp 	<- cellStats(band6, 'skew', zmean=cloudAvgTemp, zsd=cloudSdTemp)

	desertProp		<- ((cellStats(cloudFilter, 'sum')) / ncellInput ) * 1.0
	coldCloudProp	<- ((cellStats(coldCloud, 'sum')) / ncellInput) * 1.0
	
	if (desertProp > 0.5 & coldCloudProp > 0.004 & cloudAvgTemp < 295) {
		tmax <- quantile(band6,  na.rm=TRUE, probs=0.9875)
		tU 	<- quantile(band6,  na.rm=TRUE, probs=0.975)
		tL 	<- quantile(band6,  na.rm=TRUE, probs=0.835)
		if (cloudSkewTemp > 0) {
			shift <- overlay(cloudSkewTemp,cloudSdTemp,fun=function(x,y){ return(max(x,1)*y) })
			#tU	<- max(tU + shift, tmax)
			tU 	<- overlay(tU, shift, tmax, fun=function(x,y,z) { return(max(x+y,z)) } )
			tL	<- tL + shift 
		}
		
		ct			<- cloudAmb
		NAvalue(ct) <- 0
		if (inMemory(ct)) {
			ct[ct==0] <- NA
		} 
		# band6P2 	<- overlay(band6All, ct, fun=function(x,y){ return(x*y) })
		band6P2 <- band6All * ct
		cloudU  <- band6P2 < tU & band6P2 >= tL
		NAvalue(cloudU) <- 0
		if (inMemory(cloudU)) {
			cloudU[cloudU==0] <- NA
		}
#		band6U	 	<- overlay(band6P2, cloudU, fun=function(x,y){ return(x*y) })
		band6U	 	<- band6P2 * cloudU
		band6UProb 	<- cellStats(band6U, 'sum') / ncellInput
		
		cloudL 		<- band6P2 < tL
		NAvalue(cloudL) <- 0
		if (inMemory(cloudL)) {
			cloudL[cloudL==0] <- NA
		}
		#band6L	 	<- overlay(band6P2, cloudL, fun=function(x,y){ return(x*y) })
		band6L	 	<- band6P2 * cloudL
		band6LProb 	<- cellStats(band6L, 'sum')/ncellInput
		mband6U 	<- cellStats(band6U, 'mean')
		if (band6UProb > 0.4 | mband6U > 295 |  snowProp > 0.01) {
			cloudT 		<- band6P2 < tL
			 
			#band6T 	<- band6P2  * cloudT
			
			#band6T 	was missing here. What should it be???
			#based on the above, presumably
			band6T 	<- band6P2  * cloudT
			# and we no longer need:
			#band6T	 	<- overlay(band6T, cloudT, fun=function(x,y){ return(x*y) })
			if (inMemory(band6T)) {
				band6T[band6T==0] <- NA
			} else {
				NAvalue(band6T) <- 0
			}

			band6TProb 	<- cellStats(band6T, 'sum')/ncellInput
			
			if (band6UProb > 0.4 | mband6U > 295) {
				cloudMask <- cloudFilter
				if (band6LProb > 0.4 | cellStats(band6L, 'mean') > 295) {
					cloudMask <- cloudFilter
				} else {
					cloudMask <- (cloudFilter == 1) + (cloudL == 1) 
					cloudMask <- cloudMask > 0
				}
			} else {
				cloudMask <- (cloudFilter == 1) + (cloudL == 1) + (cloudU == 1)
				cloudMask <- cloudMask > 0
			}
		} else {
			## WHAT GOES HERE ??? 
			## just a guess, needs to be checked
			cloudMask <- coldCloud
			
		
		}
	} else {
		#pass two is bypassed
		cloudMask <- coldCloud
	}
	
	cat(class(cloudMask))
	
	#majority analysis on 0 values in cloudMask
	maj <- focal(cloudMask, fun=modal, ngb=3, keepdata=TRUE)
	cloudMask <- overlay(cloudMask, maj, fun=sum)
	cloudMask <- reclass(cloudMask, c(-Inf,0,FALSE, 0, Inf, TRUE))
	#cloudMask <- (cloudMask + maj) > 0
	
	return(cloudMask)
}


