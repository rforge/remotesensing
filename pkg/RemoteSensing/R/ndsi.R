# snow index


ndsi <- function (green, nir) {
    result <- (green - nir)/(green + nir)
	result[(is.infinite(result))] <- NA
	return(result)
}
