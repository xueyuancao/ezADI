\name{ezADI}
\alias{ezADI}
\title{
Area Deprivation Index (ADI) Query
}
\description{
This function impletements the query of USA census geocoder to obtain FIPS of addresses in USA, then merge with ADI data by FIPS.
}
\usage{
ezADI(inputFile = NULL, inputDat = NULL, adiFile = "US_ADI_Example.csv", outputFile = NULL, batchSize = 10)
}
\arguments{
  \item{inputFile}{
a character string of data file contains minimum columns of \emph{ID} and \emph{Address} or \emph{ID}, \emph{street}, \emph{city}, \emph{state} and \emph{zipcode}. It supports 3 file formats: \sQuote{.txt}, \sQuote{.csv}, and \sQuote{.xlsx}. Default value is NULL. 
}
  \item{inputDat}{
a data frame contains minimum columns of \emph{ID} and \emph{Address} or \emph{ID}, \emph{street}, \emph{city}, \emph{state} and \emph{zipcode}. Default value is NULL.
}
  \item{adiFile}{
an ADI file in the same format as in the example file: US_ADI_Example.csv. It could be downloaed from \url{https://www.neighborhoodatlas.medicine.wisc.edu/}.
}
  \item{outputFile}{
a character string of \sQuote{.xlsx} file to which the queried ADI data will write.
}
  \item{batchSize}{
batch query size, default is 1000.
}
}
\details{
This function performs query of USA census geocoder on Addresses in USA via geocode implemented in the \emph{tidygeocoder} package. The queried results are used to contruct FIPS which is used as the key to merge the address data with ADI data provided in the package. Currently, two ADI data are provided: \emph{adi2015} and \emph{adi2020}.  
}
\value{
  \item{adiRes}{a dataframe contains the geocoder query result and merged ADI data}
  \item{matched}{number of mathed addresses}
  \item{no_match}{number of not mathed addresses}
  \item{problem_address}{addresses that cannot be matched}
  \item{nadi}{number of addresses with available ADI data}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Xueyuan Cao <xcao12@uthsc.edu>
}
\examples{
data(exmpDat)
test<- ezADI(inputFile = NULL, inputDat = exmpDat, adiFile = "US_ADI_Example.csv", outputFile = NULL, batchSize = 100)
}

