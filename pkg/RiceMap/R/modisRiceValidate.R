# Authors: Jorrel Khalil Aunario, Angelo Carlo Pachec and Robert J. Hijmans 
# International Rice Research Institute
# Date :  December 2009
# Version 0,1
# Licence GPL v3

modisRiceValidate <- function(perhapsPath, eviPath, outPath, tileNumber, year, valscale=NULL){
    require(rgdal)

	# thresholds:
	upperT <- 0.75   #considered as the highest EVI value of rice; adjustable
	riceT <- 0.40    #considered as half of the maximum rice EVI; adjustable
	floodT <- 0.25   #considered as maximum value for flooded; adjustable

	#curYearPath = paste(curYearPath, "/", sep="")
	#prevYearPath = paste(prevYearPath, "/", sep="")

	if (!file.exists(outPath)) dir.create(outPath, recursive = TRUE)

	cat("Verifying using evi: ", perhapsPath, "\n", sep="")
	flush.console()
	
    filext <- formatExt(informat)
    pat <- paste("perhapsrice", tileNumber, year, filext, sep=".*")
	perhapsRice <- list.files(perhapsPath, pattern=pat)
	#if (length(perhapsRice)>1){
	#    getthis <- c(grep(".grd", perhapsRice), grep(".tif", perhapsRice))
    #   perhapsRice <- perhapsRice[getthis]
    #}
	pRice <- raster(paste(perhapsPath,perhapsRice[1],sep="/"))
    
	files <- list.files(eviPath, pattern=paste(year-1,tileNumber, "evi.cleaned",filext, sep=".*"))
	files <- paste(eviPath, files, sep="/")
	st <- grep("249",files)
	
	files2 <- list.files(eviPath, pattern=paste(year,tileNumber, "evi.cleaned",filext, sep=".*"))
	files2 <- paste(eviPath, files2, sep="/")
	
	#l  <- k-10
	# k <- l-45
	# stck <- stack( allfiles[k:l] )
	
	stck <- stack(c(files[st:length(files)], files2))	
    riceRast2 <- numeric(0)
    
	for( r in 1:nrow(pRice) ){
		cat("Row:", r,"\n")
		flush.console()
		
		vals <- t(getValues(stck, r))
		vals[vals==-9999] <- NA
		if(!is.null(valscale)){
            vals <- vals/valscale
        }                    
                    
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
		    for(j in nlayers(stck):8){
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
	
	riceRast <- setValues(pRice, riceRast2)
    riceRast[is.na(riceRast)] <- -15    # ??? why?

	fnameRast <- paste(outPath, "/reallyRice_", tileNumber, "_", substr(perhapsRice, 20,23), ".tif", sep="")
	riceRast <- writeRaster(riceRast, filename=fnameRast, datatye='INT2S', options=c("COMPRESS=LZW", "TFW=YES"))
}

