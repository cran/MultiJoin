LeftJoinPairs <- structure(function#create command to left join lines of two files on a common field with no further options
### Calls the Unix utilitiy join to join lines of two files on a common field. No unpairable lines are printed
(f1,   ##<< filename of first file
 f2,   ##<< filename of second file
 j1=1, ##<<  join on this FIELD of file 1
 j2=1, ##<<  join on this FIELD of file 2
# o1=2:NCOL1, ##<<  obey this FORMAT while constructing output line from file 1 (NCOL1 would be the number of columns of file 1)
# o2=2:NCOL2, ##<<  obey this FORMAT while constructing output line from file 2 (NCOL2 would be the number of columns of file 2)
 missingValue = "NA", ##<< replace missing input fields with missingValue
 sep=c(" ", ",", "\t", "|")[1], ##<< column delimiter; default white space
 extraARGS = "" ##<< extra (optional) arguments to be passed to the join function (such as --check-order or --header or --ignore-case)
){
  ##note<<Important:  FILE1  and  FILE2 must be sorted on the join fields. If you are unsure, pass the --check-order flag
  ##Note, comparisons honor the rules specified by LC_COLLATE.
  ##details<< Each output line is constructed according to the FORMAT in the -o option. Each element in FIELD-LIST is either the single
  ## character 0 or has the form M.N where the file number, M, is 1 or 2 and N is a positive field number.
  ##  A field specification of 0 denotes the join field.  In most
  ##    cases, the functionality of the 0 field spec may be reproduced
  ##   using the explicit M.N that corresponds to the join field.
  ##   However, when printing unpairable lines (using either of the -a
  ##   or -v options), there is no way to specify the join field using
  ##   M.N in FIELD-LIST if there are unpairable lines in both files.  To
  ##   give join that functionality, POSIX invented the 0 field
  ##   specification notation.
  ##
  ##   The elements in FIELD-LIST are separated by commas or blanks.
  ##   Blank separators typically need to be quoted for the shell.  For
  ##   example, the commands join -o 1.2,2.2 and join -o 1.2 2.2
  ##   are equivalent.
  
  if (sep==" ") {
    myjoin = "join "
#   } else if (sep=="\t"){
#     myjoin = paste0("join -t $'\t'")
  } else {
    myjoin = paste0("join -t '",sep,"'")
  }
  cmd <- paste0(myjoin, extraARGS," -1 ",j1, " -2 ",j2, " ", f1, " ", f2);
  
  return(cmd);
  ### Unix command to be executed
}, ex = function(){
  LeftJoinPairs("f1.txt","f2.txt")
  
  #tab delimiter:
  ret = ArtificialData(fakeDataDir="/tmp/fakeData2",sep = "\t")
  cmd = LeftJoinPairs("/tmp/fakeData2/file1.txt","/tmp/fakeData2/file2.txt",sep = "\t")
 # cat(cmd, file = "/tmp/tmp.sh")
#  system("bash /tmp/tmp.sh")
  
})
