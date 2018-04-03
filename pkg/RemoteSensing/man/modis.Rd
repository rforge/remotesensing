\name{modis}

\alias{modisFiles}


\title{ modis files }

\description{
Get a table with parsed Modis files
}

\usage{
modisFiles(sep, modisinfo, file.ext, ...)
}


\arguments{
  \item{sep}{ symbol separating information to be extracted from the filenames. default is period (".")}
  \item{modisinfo}{ a vector of chacter strings }
  \item{file.ext}{ File extension name of file for filtering(default: "tif")}
  \item{...}{ arguments from dir/list.files }
}
  
\value{
data.frame
}

\author{ Robert Hijmans, Sonia Asilo, Jorrel Khalil Aunario }

\keyword{ spatial, modis }

