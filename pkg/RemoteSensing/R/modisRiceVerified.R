# Authors: Angelo Carlo pacheco, Robert Hijmans
# International Rice Research Institute
# Date :  Oct 2009
# Version 0.1
# Licence GPL v3

# MAX EVI

#fields required:
	# perhapsPath = path where the perhaps rice is located, ex: "/media/Tera1_5/SEA2008/perhapsRice"
	# curYearPath = path where the EVI of the current year is located, ex: "/media/Tera1_5/SEA2008/veg"
	# prevYearPath = path where the EVI of the previous year is located, ex: "/media/Tera1_5/SEA2007/veg"
	# tileNumber = tile number of the Modis you need to process, ex: "h27v09"

#file naming conventions:
	# for perhaps rice: "perhapsrice_h27v09_2008.grd"
	# for EVI: "A2008001_h27v09_evi.grd"

evi_verify <- function(perhapsPath, curYearPath, prevYearPath, outPath, tileNumber) {

	# thresholds:
	upperT <- 0.75   #considered as the highest EVI value of rice
	riceT <- 0.55    #considered as half of the maximum rive EVI
	floodT <- 0.20   #considered as maximum value for flooded

	inpath = paste(perhapsPath, "/", sep="")
	vegpath = paste(curYearPath, "/", sep="")
	prevPath = paste(prevYearPath, "/", sep="")

	outpath <- paste(outPath, "/", sep="")
	dir.create(outpath, showWarnings = FALSE)

	print(paste("Verifying using evi: ", inpath, sep=""))

	pat <- paste("perhapsrice_.*", tileNumber, sep="")
	file <- list.files(path=inpath, pattern=pat)
	print(file[1])
	a <- raster(paste(inpath,file[1],sep=""))
	val <- getValues(a)

	#riceEvi <- raster(paste(inpath,file,sep=""))
	#riceEvi <- riceEvi)
	#riceEvi[riceEvi] <- 0

	riceEvi <- matrix(0, ncol=ncol(a), nrow=nrow(a))

	evipix <- vector( length=46 )

	#loading of evi grd
	# -------------------------------------oct to dec--------------------------------------
	print("Now reading evi files...")

	pat <- paste(tileNumber, "_evi.grd", sep="")
	files <- list.files(path=prevPath, pattern=pat)
	k <- length(files)
	k <- k-11
	ii1 <- raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii2 <- raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii3 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii4 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii5 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii6 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii7 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii8 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii9 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii10 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii11 <-  raster(paste(prevPath, files[k], sep=""))
	k <- k+1
	ii12 <-  raster(paste(prevPath, files[k], sep=""))
	
	#--------------------------------jan to oct------------------------------------------------

	pat <- paste(tileNumber, "_evi.grd", sep="")
	files <- list.files(path=vegpath, pattern=pat)

	if(length(files)<34){
		stop("Insufficient number of evi files...")
	}

	k <- 1
	i1 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i2 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i3 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i4 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i5 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i6 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i7 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i8 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i9 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i10 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i11 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i12 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i13 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i14 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i15 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i16 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i17 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i18 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i19 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i20 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i21 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i22 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i23 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i24 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i25 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i26 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i27 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i28 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i29 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i30 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i31 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i32 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i33 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	i34 <-  raster(paste(vegpath, files[k], sep=""))
	k <- k+1
	
	print("Reading evi files done...")
	print("Validation started ...")

	ctr <- 1
	leng <- length(val)
	print(paste("Number of pixels to be checked:", leng))

	for(i in 1:length(val)){
		print(i/length(val)*100)
		if ( val[i]==1 ) { #rice pixel checking
#			print(paste("checking pixel #: ", i, "| Remaining: ", (leng-i), sep=""))
	
			j <- 1
	
	# ----------------------------------------------------------------------------------------------------
	# -----------------------oct prev year----------------------------------------------------------------
	
			evipix[j] <- ii1[i]
			j <- j+1
			evipix[j] <- ii2[i]
			j <- j+1
			evipix[j] <- ii3[i]
			j <- j+1
			evipix[j] <- ii4[i]
			j <- j+1
			evipix[j] <- ii5[i]
			j <- j+1
			evipix[j] <- ii6[i]
			j <- j+1
			evipix[j] <- ii7[i]
			j <- j+1
			evipix[j] <- ii8[i]
			j <- j+1
			evipix[j] <- ii9[i]
			j <- j+1
			evipix[j] <- ii10[i]
			j <- j+1
			evipix[j] <- ii11[i]
			j <- j+1
			evipix[j] <- ii12[i]
			j <- j+1
	
	# ----------------------------------------------------------------------------------------------------
	# -----------------------til sept current year--------------------------------------------------------
	
			#j <- 0
			evipix[j] <- i1[i]
			j <- j+1
			evipix[j] <- i2[i]
			j <- j+1
			evipix[j] <- i3[i]
			j <- j+1
			evipix[j] <- i4[i]
			j <- j+1
			evipix[j] <- i5[i]
			j <- j+1
			evipix[j] <- i6[i]
			j <- j+1
			evipix[j] <- i7[i]
			j <- j+1
			evipix[j] <- i8[i]
			j <- j+1
			evipix[j] <- i9[i]
			j <- j+1
			evipix[j] <- i10[i]
			j <- j+1
			evipix[j] <- i11[i]
			j <- j+1
			evipix[j] <- i12[i]
			j <- j+1
			evipix[j] <- i13[i]
			j <- j+1
			evipix[j] <- i14[i]
			j <- j+1
			evipix[j] <- i15[i]
			j <- j+1
			evipix[j] <- i16[i]
			j <- j+1
			evipix[j] <- i17[i]
			j <- j+1
			evipix[j] <- i18[i]
			j <- j+1
			evipix[j] <- i19[i]
			j <- j+1
			evipix[j] <- i20[i]
			j <- j+1
			evipix[j] <- i21[i]
			j <- j+1
			evipix[j] <- i22[i]
			j <- j+1
			evipix[j] <- i23[i]
			j <- j+1
			evipix[j] <- i24[i]
			j <- j+1
			evipix[j] <- i25[i]
			j <- j+1
			evipix[j] <- i26[i]
			j <- j+1
			evipix[j] <- i27[i]
			j <- j+1
			evipix[j] <- i28[i]
			j <- j+1
			evipix[j] <- i29[i]
			j <- j+1
			evipix[j] <- i30[i]
			j <- j+1
			evipix[j] <- i31[i]
			j <- j+1
			evipix[j] <- i32[i]
			j <- j+1
			evipix[j] <- i33[i]
			j <- j+1
			evipix[j] <- i34[i]
	
	# ------------------------------------------------------------------------------------------------------
			#print(evipix2007[1:10])
			#print(evipix2008[1:20])
			#print(evipix[1:46])
	
			flag <- 0                                                            #reset flag (flag used in mapping rice)
			l <- 46
			while(TRUE){
				if(! is.na( evipix[l] )){                                             #check NA
					if(evipix[l] < upperT ){                                         #check peak season
						if( evipix[l] > riceT ){                               #check if evi > 0.30, candidate
							for(bak in 1:7){                               #go back 8 8-day composite
								if( (l-bak) < 1 ){
									break
								}
								if( ! is.na( evipix[ (l-bak) ] ) ){                          #check if NA
									if( evipix[ (l-bak) ] < floodT ) {                     #check if flooded
										flag <- 1
										break
									}
								}
							}
						}
					}
                                                                                                       #Find new peak
				}
				l <- l-1                                                               #increment
				if(l<1 || flag==1){                                                    #end loop if threshold not found
					break
				}
			}

			if(flag==1){
				riceEvi[i] <- 1                                                       #if flag is 1, rice pixel is verified as really rice
				#print("Flag = 1 ...")                                                         #insufficient data or pattern not found
			}
			if(flag==0){
				riceEvi[i] <- NA                                                       #if flag is 0, rice pixel is not verified
				#print("Insufficient data ... (NA) ")                                                         #insufficient data or pattern not found
			}
	
			#print(paste("FLAG: ", flag, sep="") )                                         #if flag is 1, it is rice...
	
			#print("----             ----")
			#ctr <- ctr+1
	
			#Sys.sleep(3)
	
		}
	}

	print("Validation done ...")

	rasMat <- a
	rasMat <- setValues(rasMat, as.vector(riceEvi))
                                                                                                    #creation of file "reallyRice.grd"
	dataType(rasMat) <- "FLT4S"
	filename(rasMat) <- paste(outpath, tileNumber, '_reallyRice.grd', sep='')
	writeRaster(rasMat, overwrite=TRUE)

}


# For Calling the MAX EVI function

library(raster)
start <- function(){


	perhapsPath <- "E:/RUN/SEA/2008/rice/h26v05/xiao7"
	curYearPath <- "E:/RUN/SEA/2008/veg/h26v05/cleaned"
	prevYearPath <- "E:/RUN/SEA/2007/veg/h26v05/"
	outPath <- "E:/RUN/SEA/2008/reallyRice"
	
	tileNumber <- "h26v05"
	print(tileNumber)
	evi_verify(perhapsPath, curYearPath, prevYearPath, outPath, tileNumber)
}
