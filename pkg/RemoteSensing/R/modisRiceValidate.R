# Authors: Jorrel Khalil Aunario, Angelo Carlo Pachec and Robert J. Hijmans 
# International Rice Research Institute
# Date :  December 2009
# Version 0,1
# Licence GPL v3
require(rgdal)

modisRiceValidate <- function(perhapsPath, curYearPath, prevYearPath, outPath, tileNumber){

	# thresholds:
	upperT <- 0.75   #considered as the highest EVI value of rice; adjustable
	riceT <- 0.40    #considered as half of the maximum rice EVI; adjustable
	floodT <- 0.25   #considered as maximum value for flooded; adjustable

	#curYearPath = paste(curYearPath, "/", sep="")
	#prevYearPath = paste(prevYearPath, "/", sep="")

	if (!file.exists(outPath)) dir.create(outPath, recursive = TRUE)

	cat("Verifying using evi: ", perhapsPath, "\n", sep="")
	flush.console()

	pat <- paste("perhapsrice_.*", tileNumber, "_[0-9]*", sep="")
	perhapsRice <- list.files(perhapsPath, pattern=pat)
	if (length(perhapsRice)>1){
	    getthis <- c(grep(".grd", perhapsRice), grep(".tif", perhapsRice))
        perhapsRice <- perhapsRice[getthis]
    }
	pRice <- raster(paste(perhapsPath,perhapsRice[1],sep="/"))
    
	pat <- paste(tileNumber, "_evi.*cleaned.grd", sep="")
	files <- list.files(prevYearPath, pattern=pat)
	files <- paste(prevYearPath, files, sep="/")
	st <- grep("249",files)
	
	files2 <- list.files(curYearPath, pattern=pat)
	files2 <- paste(curYearPath, files2, sep="/")
	
	allfiles <- c(files[st:length(files)], files2)
	
	#l  <- k-10
	# k <- l-45
	# stck <- stack( allfiles[k:l] )
	
	stck <- stack(allfiles)	
    riceRast2 <- numeric(0)
    
	for( r in 1:nrow(pRice) ){
		cat("Row:", r,"\n")
		flush.console()
		
		vals <- t(getValues(stck, r))
		
		pVec <- getValues(pRice, r)
		rice <- rep(0,length(pVec))
		
		# get perhaps rice
		pvec1 <- which(pVec==1)
		if (length(pvec1)==0){
            riceRast2 <- c(riceRast2,rice)
            next          
        }
        
        #Flag pixels as detected with water but not rice
        rice[pvec1]<- 2            
		# find flooding in EVIs
		fld <- vals[,pvec1]<=floodT
		nfld <- colSums(as.matrix(fld), na.rm=TRUE)
		 
		veg <- vals[,pvec1]>=riceT
		nveg <- colSums(as.matrix(veg),na.rm=TRUE)
        
        # remove pixels with no flooding detected and no EVI > 0.4
		pvec2 <- pvec1[nfld>0 & nveg>0]
		# if no pixel fits the criteria go to next row
        if (length(pvec2)==0){
            riceRast2 <- c(riceRast2,rice)
            next          
        }
        
        vals <- as.matrix(vals[,pvec2])        
		
		for(i in 1:ncol(vals)){
            for(j in 61:8){
                if((!is.na(vals[j,i])) & (vals[j,i]<upperT & vals[j,i]>riceT)){
                    img7 <- vals[(j-1):(j-7),i]
                    chk <- img7<=floodT
                    if (sum(chk,na.rm=TRUE)>0){
                        rice[pvec2[i]] <- 1
                    }
                }
		    }
        }            
        riceRast2 <- c(riceRast2,rice)
	}
	
	riceRast <- raster(pRice)	
	riceRast <- setValues(riceRast, riceRast2)
    
    #plot(pRice)
    #x11()
	#plot(riceRast)
	
	fnameRast <- paste(outPath, "/reallyRice_", tileNumber, "_", substr(perhapsRice, 20,23), ".tif", sep="")
	proj <- CRS(projection(pRice))
    gtop <- GridTopology(c(xmin(pRice)+(xres(pRice)/2),ymin(pRice)+(yres(pRice)/2)),c(xres(pRice),yres(pRice)),c(ncol(pRice),nrow(pRice)))
    band1 <- riceRast2
    band1[is.na(band1)] <- -15    
    band1 <- as.data.frame(band1)
    rnew <- SpatialGridDataFrame(gtop, band1, proj4string=proj)
    if (file.exists(fnameRast)) file.remove(bfname)
    rnew <- writeGDAL(rnew,fnameRast, options=c("COMPRESS=LZW", "TFW=YES"), type = "Int16")
    rm(rnew)            
	# writeRaster(riceRast, filename=fnameRast, format="GTiff", datatype= "INT1S", overwrite=T)

}
