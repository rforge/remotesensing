# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3

forest <- function(ndvi){
	return(ndvi>=0.7)
}

# TODO: verify if > or <
shrub <- function(lswi){
	return(lswi<0.1)
}
