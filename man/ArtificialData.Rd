\name{ArtificialData}
\alias{ArtificialData}
\title{create artificial data for testing}
\description{This function allows quick generation of a test data set which can be used with the majority of the Join functions}
\usage{ArtificialData(fakeDataDir = "~/fakeData2/", joinKey = letters[1:20], 
    numFiles = 4, N = rep(15, numFiles), SORT = 1, GZIP = 0, 
    sep = c(" ", ",", "\\t", "|")[1], prefix = "file", suffix = ".txt", 
    daten = month.abb, NCOL = rep(3, numFiles), chunkSize = 1000, 
    verbose = 0)}
\arguments{
  \item{fakeDataDir}{directory to put the data}
  \item{joinKey}{set of join keys to choose from (has to be longer than N) - this column will be the key for join}
  \item{numFiles}{number of files to split the data across}
  \item{N}{number of rows in each file created, e.g. N = c(15,20,10,30)}
  \item{SORT}{should the join key be sorted?}
  \item{GZIP}{should the data files created by gzipped?}
  \item{sep}{column delimiter; default white space}
  \item{prefix}{file name prefix}
  \item{suffix}{file name suffix}
  \item{daten}{data to sample from}
  \item{NCOL}{number of data columns per file}
  \item{chunkSize}{write that many lines to the file at once}
  \item{verbose}{level of verbosity}
}

\value{invisibly return data and file names}

\author{"Markus Loecher, Berlin School of Economics and Law (BSEL)" <markus.loecher@gmail.com>}




\examples{
ArtificialData("fakeData2",verbose=1)
ArtificialData("fakeData2",joinKey = 1:2000, N = rep(1500,4) ,verbose=0)

ret = ArtificialData(fakeDataDir="/tmp/fakeData")
ret = ArtificialData(fakeDataDir="./fakeData", joinKey=letters[1:10], numFiles = 6, N = rep(5,6))
ret = ArtificialData(SORT = 1, GZIP = 1)

ret = ArtificialData(fakeDataDir="fakeData", joinKey = 0:9, N = rep(6, 4), verbose=1)
#on allegro:
ret = ArtificialData(fakeDataDir="./fakeData", joinKey=letters, numFiles = 10, 
                     N = rep(18,10), NCOL=rep(5,10))
}
