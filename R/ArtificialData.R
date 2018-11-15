ArtificialData <- structure(function#create artificial data for testing
###This function allows quick generation of a test data set which can be used with the majority of the Join functions
  ( fakeDataDir = "~/fakeData2/", ##<< directory to put the data
    joinKey = letters[1:20], ##<< set of join keys to choose from (has to be longer than N) - this column will be the key for join
    numFiles = 4, ##<< number of files to split the data across
    N = rep(15,numFiles),  ##<< number of rows in each file created, e.g. N = c(15,20,10,30)
    SORT = 1, ##<< should the join key be sorted?
    GZIP = 0, ##<< should the data files created by gzipped?
    sep = c(" ", ",", "\t", "|")[1], ##<< column delimiter; default white space
    prefix = "file", ##<< file name prefix
    suffix = ".txt", ##<< file name suffix
    daten = month.abb, ##<< data to sample from
    NCOL = rep(3,numFiles), ##<< number of data columns per file
    chunkSize = 1000, ##<< write that many lines to the file at once
    verbose = 0 ##<< level of verbosity
  ){
    
    y <- list();
    # Make sure the fakeData directory exists, if not create it.
    if (!is.null(fakeDataDir)) {
      if (!file.exists(fakeDataDir)) try(dir.create(fakeDataDir));
    }
    # Create the file names
    fnames = paste(file.path(fakeDataDir, prefix) , 1:numFiles, suffix, sep="")
    if (verbose) print(fnames)
    colCount = 1;       
    for (i in 1:numFiles) {
      #k=sample(joinKey,N[i])
      #if (SORT) k=sort(k)
      #data <- cbind.data.frame(key=k, x1= sample(100,N[i]) , x2= sample(100,N[i]), x3= sample(100,N[i]));
      #data <- cbind.data.frame(key=k, x= matrix(sample(daten,N[i], replace=TRUE),ncol=NCOL[i]));
      #data <- cbind.data.frame(key=joinKey,x= matrix(paste0("f",i,"-",outer(1:length(joinKey),1:NCOL[i],paste0)),ncol=NCOL[i]))
      #data <- cbind.data.frame(key=joinKey,x = matrix(,length(joinKey),ncol=NCOL[i]))
      
      #do not allocate memory for the whole matrix, that is exactly what we are trying to change!
      #data <- matrix(, length(joinKey), ncol= NCOL[i])
      #Instead allocate chunkSize rows at once:
      
      jKSeq=1:length(joinKey)
      jK = sort(sample(jKSeq,N[i]))
      NN = min(c(length(jK),chunkSize))
      data <- matrix(NA, NN, ncol= NCOL[i]+1)
      
      # Create double loop to insert 'fi-rowcol' type content 
      k=0
      if (verbose>2) browser()
      while (k < length(jK)){
        NN = min(c(length(jK)-k,chunkSize))
        if (verbose>1) cat("working on rows", k+1,":",k+NN, "\n")
        data[1:NN,1] = joinKey[jK[(k+1):(k+NN)]]
        for (l in 1:NN){
#           column by column is inefficient:
#           for (j in 1:NCOL[i]){ 
#             tmp <- paste0("f",i,collapse="")
#             data[l,j+1] <- paste(tmp,jKSeq[jK[k+l]],j,sep="-")
#           }
          fi = paste0("f",i)
          data[l,2:(NCOL[i]+1)] <- paste(fi,jKSeq[jK[k+l]],1:NCOL[i],sep="-")
        }
        if (k==0) append=FALSE else append=TRUE
        utils::write.table(data,file = fnames[i],  quote = F, sep=sep,append=append,row.names=F, col.names = F);
        k=k+NN
      }
      
      #cat(c("key", paste("Col", colCount:(colCount+2), sep="")), file = paste(fakeDataDir, "/Header", i, sep =""),sep="\t");
      #colCount = colCount + 3;
    
      #write.table(data[sort(sample(1:length(joinKey),N[i])),],file = fnames[i], row.names=F, col.names = F, quote = F, sep=sep, eol = "\n");
     
      if (GZIP) {
        system(paste("gzip ", fnames[i]));
      }
      y[[i]] <- data;
if (verbose) cat("just created file ", fnames[i], "\n")
    }
    if (verbose == 1) {
      if (GZIP) {
        cat("Printed gzip files:", paste(fnames));
      } else {
        cat("Printed files:", paste(fnames));
      }
    }
    invisible(list(data=y, fnames = fnames));
    ### invisibly return data and file names
}, ex = function(){
  if (0){
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
  })


