# snow index


ndsi <- function (green, nir) {
	result <- (green - nir)/(green + nir)
	result[is.infinite(result)] <- NA
    result[result < -1] <- -1
    result[result > 1] <- 1
	return(result)
}
