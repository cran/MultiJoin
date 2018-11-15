\name{CountColumns}
\alias{CountColumns}
\title{count columns of files}
\description{small helper function that attempts to count how many columns there are in a file}
\usage{CountColumns(files = c("ftr1.txt", "ftr2.txt"), sep = c(" ", 
    ",", "\\t", "|")[1], mycat = c("", "gunzip -cf ", "cat ")[1], 
    filterStr = "", verbose = 0, ...)}
\arguments{
  \item{files}{which files to inspect}
  \item{sep}{column delimiter; default white space}
  \item{mycat}{effective cat command, if empty do NOT use FIFos}
  \item{filterStr}{various inline filters that act locally and do not need an input file,}
  \item{verbose}{level of verbosity}
  \item{\dots}{further arguments to myjoin such as missingValue or extraARGS}
}

\value{returns number of columns of each file}

\author{"Markus Loecher, Berlin School of Economics and Law (BSEL)" <markus.loecher@gmail.com>}




\examples{
if (0){
  ret = ArtificialData(fakeDataDir="fakeData2", joinKey = 0:9, N = rep(6, 4), verbose=1)
  CountColumns(paste0("fakeData2/file",1:4,".txt"))
  #gzipped data:
  ret = ArtificialData(fakeDataDir="fakeData2", joinKey = 0:9, N = rep(6, 4), GZIP=1, verbose=1)
  CountColumns(paste0("fakeData2/file",1:4,".txt.gz"),mycat ="gunzip -cf ")
  #gzipped and selected columns:  
  ret = ArtificialData(fakeDataDir="fakeData2", joinKey = 0:9, N = rep(6, 4), GZIP=1, verbose=1)
  CountColumns(paste0("fakeData2/file",1:4,".txt.gz"),mycat ="gunzip -cf ", 
               filterStr=" | cut -f1,3 -d\" \" ")
}
}
