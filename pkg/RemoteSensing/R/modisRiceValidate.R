# Authors: Angelo Carlo Pachec and Robert J. Hijmans 
# International Rice Research Institute
# Date :  December 2009
# Version 0,1
# Licence GPL v3


modisRiceValidate <- function(perhapsPath, curYearPath, prevYearPath, outPath, tileNumber){

	# thresholds:
	upperT <- 0.75   #considered as the highest EVI value of rice; adjustable
	riceT <- 0.40    #considered as half of the maximum rice EVI; adjustable
	floodT <- 0.20   #considered as maximum value for flooded; adjustable

	inpath = paste(perhapsPath, "/", sep="")
	vegpath = paste(curYearPath, "/", sep="")
	prevPath = paste(prevYearPath, "/", sep="")

	outpath <- paste(outPath, "/", sep="")
	dir.create(outpath, showWarnings = FALSE)

	print(paste("Verifying using evi: ", inpath, sep=""))

	pat <- paste("perhapsrice_.*", tileNumber, "_[0-9]*.tif", sep="")
	perhapsRice <- list.files(path=inpath, pattern=pat)
	pRice <- raster(paste(inpath,perhapsRice[1],sep=""))
plot(pRice)
x11()

	riceRast <- raster(paste(inpath,perhapsRice[1],sep=""))
	riceRast[] <- 0
	riceRast2 <- 1:length(riceRast[])
	riceRast2[] <- 0

	pat <- paste(tileNumber, "_evi-cleaned.grd", sep="")
	files <- list.files(path=prevPath, pattern=pat)
	files <- paste(prevPath, files, sep="")
	
	pat <- paste(tileNumber, "_evi-cleaned.grd", sep="")
	files2 <- list.files(path=vegpath, pattern=pat)
	files2 <- paste(vegpath, files2, sep="")
	
	allfiles <- c(files, files2)
	k <- length(allfiles)
	
	#l  <- k-10
	# k <- l-45
	# stck <- stack( allfiles[k:l] )
	
	stck <- stack( allfiles[1:k] )
	vals <- getValues(stck, 1)
	rice <- vals[,1]


	for( r in 1:nrow(stck) ){
		print( paste("=============================Row:", r) )
		vals <- getValues(stck, r)
		pVec <- getValues(pRice, r)
		rice[] <- 0
		
		for(i in 1:length(pVec[])){
			#print( paste("cell:", i) )
			if( length(pVec[i])==0 ){
				z=1
			}
			else if( is.na(pVec[i]) ){
				z=1
			}
			else if(pVec[i]!=1){
				z=1 #not rice
			}
			else{
				flag=0
				for(j in 46:8){
					if(is.na(vals[i,j])){
						z=1  #no value (cloud)
					}
					else if( vals[i,j]<upperT & vals[i,j]>riceT){  # candidate for rice (proceed backtracking)

						#print(paste(">>", vals[i,j]))
						#print(vals[i,(j-1):(j-7)])

						for(bak in (j-1):(j-7)){
							if(! is.na(vals[i,bak]) ){
								if(vals[i,bak]<=floodT){
									riceRast2[cellFromRowCol(riceRast, r, i)] <- 1
									#print("Rice found!")
									flag=1
									break
								}
							}
						}
					}
					if(flag!=1){
						riceRast2[cellFromRowCol(riceRast, r, i)] <- NA
					}
				}
			}
		}
	}
	
	riceRast <- setValues(riceRast, riceRast2[])

	plot(riceRast)
	
	fnameRast <- paste(outPath, "/reallyRice_", tileNumber, "_", substr(perhapsRice, 20,23), ".tif", sep="")
	writeRaster(riceRast, filename=fnameRast, format="GTiff", datatype= "INT1S", overwrite=T)

}
