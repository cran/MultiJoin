FullJoin <- structure(function# create command to fully join multiple (more than 2) files
### Iteratively calls the function FullJoinPairs() to join lines of two files on a common field
( files = c("ftr1.txt","ftr2.txt"), ##<< which files to join
  prefix = " time ", ##<< any convenience prefix command to be passed to the beginning of the Unix command to be executed
  suffix = " > joined.txt", ##<< any convenience suffix command to be passed to the end of the Unix command to be executed
  myjoin = FullJoinPairs, ##<< the particular Join function from the package to use
  NumFields = rep(2, length(files)), ##<< this includes the userid column
  sep=c(" ", ",", "\t", "|")[1], ##<< column delimiter; default white space
  mycat = c("","gunzip -cf ", "cat ")[1], ##<< effective cat command, if empty do NOT use FIFos 
  filterStr = "",##<<various inline filters that act locally and do not need an input file,
  ReturnData = FALSE, ##<< should the result of the join command be read into R and returned as a dataframe?
  verbose = 2, ##<< level of verbosity
  ... ##<< further arguments to myjoin such as missingValue or extraARGS
){
  N = length(files);
  stopifnot(N >1);
  if (missing(NumFields)){
    NumFields=CountColumns(files, sep,mycat,filterStr,verbose)
  }
  stopifnot(N == length(NumFields));
  #use FIFOs ?
  FIFOs = files
  FIFOcmds = list()
  if (nchar(mycat)!=0){
    for (i in 1:N) {
      FIFOs[i] = paste0("/tmp/fifo",i)
      ret=MakeFIFOs(files[i],FIFO = FIFOs[i],
                path = ".", 
                filterStr=filterStr,mycat = mycat, verbose=verbose)
     if (verbose) print(ret)
     files[i] = FIFOs[i]
     FIFOcmds[[i]] = ret
    }
  }
  
  NF = cumsum(NumFields-1)+1;
  FullCmd <- myjoin(files[1],files[2], o1 = 2:NumFields[1], o2 = 2:NumFields[2], sep=sep, ...);
  if (N > 2) { 
    for (i in 3:N) FullCmd <- c(FullCmd, myjoin("-",files[i],o1 = 2:NF[i-1], o2 = 2:NumFields[i], sep=sep, ...));
    FullCmd <- paste(FullCmd, collapse = " | ");
  }
  FullCmd <- paste(prefix, FullCmd, suffix, collapse = " ");
  if (verbose < 2){#do not execute if level of verbosity is higher than 1
    if (ReturnData) {
      con = pipe(FullCmd, "r")
      x = utils::read.table(con, sep = sep, stringsAsFactors = FALSE)
      close(con)
    } else {
      system(FullCmd)
      #for (cmd in FullCmd)system(cmd)
    }
    if (verbose) {
      cat("just executed ", FullCmd, "\n")
    }
    if (nchar(mycat)!=0){
      for (i in 1:N) 
       if (file.exists(FIFOs[i])) try(file.remove(FIFOs[i]));
    }
  }
  if (ReturnData) return(x) else return(c(unlist(FIFOcmds),FullCmd));
  ### returns command only
}, ex = function(){
  if (0){
    #no FIFOs:
    FullJoin(NumFields = rep(4,2))
    FullJoin(paste0("ftr",1:4,".txt"), NumFields = rep(4, 4), suffix = " | gzip > joined.txt.gz")
    FullJoin(paste0("ftr",1:4,".txt"), NumFields = rep(3, 4),missingValue="0", suffix = "")
    #with FIFOs:
    FullJoin(paste0("ftr",1:4,".txt"), mycat = "cat ", NumFields = rep(3, 4),missingValue="0", 
             suffix = "", verbose=2)
    FullJoin(paste0("ftr",1:3,".txt.gz"), mycat = "gunzip -cf ", filterStr = " | cut -f1,3", 
             NumFields = rep(2, 3), verbose=2)
    #selected columns only:
    FullJoin(paste0("ftr",1:3,".txt"), mycat = "cat ", filterStr = "cut -f1,3",  
             NumFields = rep(2, 3),missingValue="0", suffix = "", verbose=2)
    
    ret = ArtificialData(fakeDataDir="./fakeData2", joinKey=letters, numFiles = 10, 
                         N = rep(18,10), NCOL=rep(5,10))
    FullJoin(paste0("./fakeData2/file",1:10,".txt"),missingValue="0", suffix = "", verbose=2)
    
    # let's try FIFOs:
    #small:
    cmd = FullJoin(paste0("file",1:2,".txt"), mycat = "cat ", NumFields = rep(5, 2),
                   missingValue="0", suffix = " > joined.txt", verbose=2)
    
    cmd = FullJoin(paste0("file",1:3,".txt"), mycat = "cat ", NumFields = rep(5, 3),
                   missingValue="0", suffix = " > joined.txt", verbose=2)
    
    # and now gzipped files:
    ret = ArtificialData(fakeDataDir="./fakeData", joinKey=letters, numFiles = 10,GZIP =1, 
                         N = rep(18,10), NCOL=rep(5,10))
    cmd = FullJoin(paste0("./fakeData/file",1:10,".txt.gz"), mycat = "gunzip -c ",  
                   NumFields = rep(3, 10),missingValue="NA",
                   filterStr = " | cut -f1,2,3",
                   suffix = "  > joined.txt", verbose=2)
    
    x = FullJoin(paste0("./fakeData/file",1:10,".txt.gz"), mycat = "gunzip -c ",  
                 NumFields = rep(3, 10),missingValue="NA",
                 filterStr = " | cut -f1,2,3",ReturnData=TRUE,
                 suffix = "", verbose=0)
  }
  #let us try a laarge example
  
  #uids = sort(paste0(sample(LETTERS,10^7,replace=TRUE), sample(10^8,10^7)))
  #uids = paste0(LETTERS, (10^7):(10^8))
  #tmp=expand.grid(LETTERS,LETTERS,LETTERS,0:9,0:9);str(tmp)
  #uids=apply(expand.grid(LETTERS[1:3],LETTERS[1:3],0:2,0:3),1,paste0,collapse="")
  #uids=apply(expand.grid(LETTERS,LETTERS,LETTERS,0:9,0:9),1,paste0,collapse="")
  if (0) {
    uids = scan("uids.txt",what="")
    Nfiles=100          
    ret = ArtificialData(fakeDataDir="./fakeData", joinKey=uids, 
                   numFiles = Nfiles, GZIP =1, N = rep(10^5,Nfiles), NCOL=rep(10,Nfiles))
    cmd = FullJoin(paste0("fakeData/file",1:10,".txt.gz"), mycat = "gunzip -c ",  
                   NumFields = rep(3, 10),missingValue="NA",
                   filterStr = " | cut -f1,2,3",
                   suffix = " | gzip > ./fakeData/joined.txt.gz", verbose=2)
    system("rm /tmp/fifo*")
    for (go in cmd) system(go)
    
    x = FullJoin(paste0("./fakeData/file",1:10,".txt.gz"), mycat = "gunzip -c ",  
                 NumFields = rep(3, 10),missingValue="NA",
                 filterStr = " | cut -f1,2,3",ReturnData=TRUE,
                 suffix = "", prefix="", verbose=0)
  }
  
  })
