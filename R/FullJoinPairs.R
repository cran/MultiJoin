FullJoinPairs <- structure(function#   create command to fully join lines of two files on a common field
### Calls the Unix utilitiy join to join lines of two files on a common field
### The -a option is set for both files such that also unpairable lines are printed.
  ( f1,   ##<< filename of first file
    f2,   ##<< filename of second file
    j1=1, ##<<  join on this FIELD of file 1
    j2=1, ##<<  join on this FIELD of file 2
    o1=2:4, ##<<  obey this FORMAT while constructing output line from file 1 (NCOL1 would be the number of columns of file 1)
    o2=2:4, ##<<  obey this FORMAT while constructing output line from file 2 (NCOL2 would be the number of columns of file 2)
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
    
    if (missing(o1) | is.null(o1) | !is.numeric(o1)) {
      #NCOL1 = as.numeric(system(paste("head -n 1 ", f1, " | wc -w "), intern= TRUE))
      NCOL1=CountColumns(f1, sep,mycat="cat ")
      o1=2:NCOL1
    }
    if (missing(o2) | is.null(o2) | !is.numeric(o2)) {
      #NCOL2 = as.numeric(system(paste("head -n 1 ", f2, " | wc -w "), intern= TRUE))
      NCOL2=CountColumns(f2, sep,mycat="cat ")
      o2=2:NCOL2
    }
    
    o1 = paste("1.", o1, sep="", collapse = " ")
    o2 = paste("2.", o2, sep="", collapse = " ")
    #cmd <- paste("join -a 1 -a 2 -o 0 ",o1," ",o2," -e ", dQuote(missingValue)," -1 ",j1, " -2 ",j2, " ", f1, " ", f2 ,sep="");
    if (sep==" ") {
      myjoin = "join "
      #   } else if (sep=="\t"){
      #     myjoin = paste0("join -t $'\t'")
    } else {
      myjoin = paste0("join -t '",sep,"'")
    }
    cmd <- paste0(myjoin, extraARGS," -a 1 -a 2 -o \"0 ",o1," ",o2,"\" -e ",missingValue," -1 ",j1, " -2 ",j2, " ", f1, " ", f2);
      
    return(cmd);
    ### returns command
  }, ex = function(){
    ret = ArtificialData(fakeDataDir=tempdir(), numFiles=2,NCOL = rep(4,2))
    FullJoinPairs(ret$fnames[[1]][1], ret$fnames[[2]][1], o1=2:4, o2 = 2:4)
  })
