\name{MakeFIFOs}
\alias{MakeFIFOs}
\title{creates named Unix pipes, which gzipped files can be streamed to for e.g. further joins}
\description{Additional filters can be implemented based upon the input arguments.
This string is typically used in between pipes. }
\usage{MakeFIFOs(file = "file1.txt.gz", FIFO = "/tmp/fifo1", path = ".", 
    filterStr = " | cut -f2,3 -d\\" \\" --complement", mycat = "gunzip -cf ", 
    verbose = 2)}
\arguments{
  \item{file}{Name of the file that contains the data to uncompress and filter on}
  \item{FIFO}{Name of the FIFO to create}
  \item{path}{Directory to find the files in}
  \item{filterStr}{various inline filters that act locally and do not need an input file,}
  \item{mycat}{effective cat command}
  \item{verbose}{level of verbosity}
}

\value{filter string}

\author{"Markus Loecher, Berlin School of Economics and Law (BSEL)" <markus.loecher@gmail.com>}




\examples{
MakeFIFOs(verbose=2)
MakeFIFOs(filterStr=" | awk '$2 > 100 && $3 > 5' | 
          cut -f2,3 -d\" \" --complement | head -n 10000 | sort -k1,1")
}
