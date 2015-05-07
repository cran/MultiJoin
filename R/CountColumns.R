CountColumns <- structure(function# count columns of files
### small helper function that attempts to count how many columns there are in a file
( files = c("ftr1.txt","ftr2.txt"), ##<< which files to inspect
  sep=c(" ", ",", "\t", "|")[1], ##<< column delimiter; default white space
  mycat = c("","gunzip -cf ", "cat ")[1], ##<< effective cat command, if empty do NOT use FIFos 
  filterStr = "",##<<various inline filters that act locally and do not need an input file,   
  verbose = 0, ##<< level of verbosity
  ... ##<< further arguments to myjoin such as missingValue or extraARGS
){
  #print(missing(sep))
  #return()
  N = length(files);
  NCOLS = rep(NA,N)
  if (mycat == "") mycat = "cat"
  
  if (verbose>2) browser() 
  for (i in 1:N) {
    cmd = paste(mycat, files[i] , filterStr)
    con=pipe(cmd)
    x = read.table(con, sep=sep, nrows=1)
    NCOLS[i] = ncol(x)
    #close(con)
    if (verbose) cat(files[i], "has" ,NCOLS[i], "columns.\n")
  }
  
  return(NCOLS)  
  
  ### returns number of columns of each file
}, ex = function(){
  
  ret = ArtificialData(fakeDataDir="fakeData2", joinKey = 0:9, N = rep(6, 4), verbose=1)
  CountColumns(paste0("fakeData2/file",1:4,".txt"))
  #gzipped data:
  ret = ArtificialData(fakeDataDir="fakeData2", joinKey = 0:9, N = rep(6, 4), GZIP=1, verbose=1)
  CountColumns(paste0("fakeData2/file",1:4,".txt.gz"),mycat ="gunzip -cf ")
  #gzipped and selected columns:  
  ret = ArtificialData(fakeDataDir="fakeData2", joinKey = 0:9, N = rep(6, 4), GZIP=1, verbose=1)
  CountColumns(paste0("fakeData2/file",1:4,".txt.gz"),mycat ="gunzip -cf ", 
               filterStr=" | cut -f1,3 -d\" \" ")
})
