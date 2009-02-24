# Author: Alice Laborte
# International Rice Research Institute
#Date: February 2009

#Read metadata text file
readMetadata <- function(filename) {
	meta <- readIniFile(filename)
	meta <- meta[,-1]
	meta[,2] <- gsub('\"', "", meta[,2])
	return(meta)
}

#Read the Landsat calibration paramter file (CPF) via ftp
readLandsatCPF <- function(filename) {
	pars 		<- readMetadata(filename)
	spacecraft_id   <- pars[pars[,1]=="SPACECRAFT_ID",2]
	sensor_id        <-  pars[pars[,1]=="SENSOR_ID",2]
	
	if (spacecraft_id %in% c("Landsat1", "Landsat2", "Landsat3", "Landsat4", "Landsat5", "Landsat7") & sensor_id %in% c("TM", "ETM+")) {
		cpf_file_name	<- pars[pars[,1]=="CPF_FILE_NAME",2]
		if (sensor_id == "ETM+")   
			ftpdir <- "ftp://edclpdsftp.cr.usgs.gov/pub/data/CPF/ETM/"
		else  
			ftpdir <- "ftp://edclpdsftp.cr.usgs.gov/pub/data/CPF/TM/" 
	
		n <- nchar(cpf_file_name)
		cpf_filename <- paste(substr(cpf_file_name, 1, n-3), ".", sep="")
		cpf_filename <- paste(cpf_filename, substr(cpf_file_name,n-1,n), sep ="")
		download.file(paste(ftpdir, cpf_filename, sep=""), cpf_filename)
		cpf <- readMetadata(cpf_filename)
	}
	else 
		cpf  <- NA
	return(cpf)
}

#parse Landsat metadata file and extract needed parameters
landsat <- function(filename) {

	pars <- readMetadata(filename)
	
	spacecraft_id   		<- pars[pars[,1]=="SPACECRAFT_ID",2]
	sensor_id        	    <-  pars[pars[,1]=="SENSOR_ID",2]
	acquisition_date	    <-  pars[pars[,1]=="ACQUISITION_DATE",2]
	product_creation_date	<-  pars[pars[,1]=="PRODUCT_CREATION_TIME",2]
	doy                 	<- as.integer(format(acquisition_date,"%j"))
	sun_elevation 			<- as.numeric(pars[pars[,1]=="SUN_ELEVATION",2])
	sun_azimuth 			<- as.numeric(pars[pars[,1]=="SUN_AZIMUTH",2])
	bandcomb       			<- pars[pars[,1]=="BAND_COMBINATION",2]
	cpf_file_name			<- pars[pars[,1]=="CPF_FILE_NAME",2]
	n_bands              	<- as.integer(nchar(bandcomb))
	wrs_path				<- as.integer(pars[pars[,1]=="WRS_PATH",2])
	starting_row			<- as.integer(pars[pars[,1]=="STARTING_ROW",2])
	ending_row			<- as.integer(pars[pars[,1]=="ENDING_ROW",2])
	
	bands    <- rep(NA,n_bands)
	for (i in 1:n_bands) {
		bands[i] <- paste("BAND", substr(bandcomb,i,i), sep="")
	}
	if (sensor_id %in% c("ETM+", "etm+")) {                        
		bands[6] <- "BAND61"
		bands[7] <- "BAND62"
	}
	
	lmax      <- rep(NA, n_bands)
	lmin       <- rep(NA, n_bands)
	qcalmax <- rep(NA, n_bands)
	qcalmin  <- rep(NA, n_bands)
	band_filenames  <- rep(NA, n_bands)
	
	names(lmax) 	  <- bands
	names(lmin) 	  <- bands
	names(qcalmax) <- bands
	names(qcalmin)  <- bands
	names(gain)  	   <- bands
	names(bias) 	   <- bands
	names(band_filenames)  <- bands
	
	
	
	for (i in 1:n_bands) {
		lmax[i]      <- as.numeric(pars[pars[,1]==paste("LMAX_", bands[i], sep=""),2])
		lmin[i]       <- as.numeric(pars[pars[,1]==paste("LMIN_", bands[i], sep=""),2])
		qcalmax[i] <- as.numeric(pars[pars[,1]==paste("QCALMAX_", bands[i], sep=""),2])
		qcalmin[i]  <- as.numeric(pars[pars[,1]==paste("QCALMIN_", bands[i], sep=""),2])
		band_filenames[i]  <- pars[pars[,1]==paste( bands[i], "_FILE_NAME", sep=""),2]
	}
	
	
	if (sensor_id == "ETM+") {
		img <- new("LandsatETMp")

		img@spacecraft_id   		= spacecraft_id,
		sensor_id        	        =  sensor_id,
		product_creation_date	=  product_creation_date,
		acquisition_date	        =  acquisition_date,
		doy                 			= doy,
		sun_elevation 			= sun_elevation,
		sun_azimuth 			= sun_azimuth,
		band_combination		= bandcomb,
		cpf_file_name			= cpf_file_name,
		nbands              		= n_bands,
		bands 				= bands,
		band_filenames 		= band_filenames,
		wrs_path				= wrs_path,
		starting_row			= starting_row,
		ending_row			= ending_row,
		lmax	     				= lmax,
		lmin          			= lmin,
		qcalmax				= qcalmax,
		qcalmin				= qcalmin,
		gain					= gain,
		bias					= bias

		bands <- stack( selected_bandfiles )

	#cpf <- readLandsatCPF(filename)
	#acquisition_time <- cpf[cpf[,1]=="Turn_Around_Time",2]
	
	return (Landsat)
 }
 
 
 