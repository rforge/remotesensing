# snow index


ndsi <- function (green, nir) {
	result <- (green - nir)/(green + nir)
	#NAvalue(result) 	<-Inf
	return(result)
}
