
readMetadata <- function(filename) {
	meta <- readIniFile(filename)
	meta <- meta[,2:3]
	meta[,2] <- gsub('\"', "", meta[,2])
	return(meta)
}
