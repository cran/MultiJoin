\name{FullJoinPairs}
\alias{FullJoinPairs}
\title{create command to fully join lines of two files on a common field}
\description{Calls the Unix utilitiy join to join lines of two files on a common field
The -a option is set for both files such that also unpairable lines are printed.}
\usage{FullJoinPairs(f1, f2, j1 = 1, j2 = 1, o1 = 2:4, o2 = 2:4, missingValue = "NA", 
    sep = c(" ", ",", "\\t", "|")[1], extraARGS = "")}
\arguments{
  \item{f1}{filename of first file}
  \item{f2}{filename of second file}
  \item{j1}{join on this FIELD of file 1}
  \item{j2}{join on this FIELD of file 2}
  \item{o1}{obey this FORMAT while constructing output line from file 1 (NCOL1 would be the number of columns of file 1)}
  \item{o2}{obey this FORMAT while constructing output line from file 2 (NCOL2 would be the number of columns of file 2)}
  \item{missingValue}{replace missing input fields with missingValue}
  \item{sep}{column delimiter; default white space}
  \item{extraARGS}{extra (optional) arguments to be passed to the join function (such as --check-order or --header or --ignore-case)}
}
\details{Each output line is constructed according to the FORMAT in the -o option. Each element in FIELD-LIST is either the single
character 0 or has the form M.N where the file number, M, is 1 or 2 and N is a positive field number.
A field specification of 0 denotes the join field.  In most
cases, the functionality of the 0 field spec may be reproduced
using the explicit M.N that corresponds to the join field.
However, when printing unpairable lines (using either of the -a
or -v options), there is no way to specify the join field using
M.N in FIELD-LIST if there are unpairable lines in both files.  To
give join that functionality, POSIX invented the 0 field
specification notation.

The elements in FIELD-LIST are separated by commas or blanks.
Blank separators typically need to be quoted for the shell.  For
example, the commands join -o 1.2,2.2 and join -o 1.2 2.2
are equivalent.}
\value{returns command}

\author{"Markus Loecher, Berlin School of Economics and Law (BSEL)" <markus.loecher@gmail.com>}
\note{Important:  FILE1  and  FILE2 must be sorted on the join fields. If you are unsure, pass the --check-order flag
Note, comparisons honor the rules specified by LC_COLLATE.}



\examples{
if (0){
  ret = ArtificialData(fakeDataDir=tempdir(), numFiles=2,NCOL = rep(4,2))
  FullJoinPairs(ret$fnames[[1]][1], ret$fnames[[2]][1], o1=2:4, o2 = 2:4)
}
}
