MakeFIFOs = structure(function# creates named Unix pipes, which gzipped files can be streamed to for e.g. further joins
### Additional filters can be implemented based upon the input arguments.
### This string is typically used in between pipes. 
(
	file="file1.txt.gz", ##<<Name of the file that contains the data to uncompress and filter on
	FIFO = "/tmp/fifo1", ##<<Name of the FIFO to create
  path = ".", ##<<Directory to find the files in
	filterStr=" | cut -f2,3 -d\" \" --complement", ##<<various inline filters that act locally and do not need an input file,
	mycat = "gunzip -cf ", ##<< effective cat command
  verbose = 2 ##<< level of verbosity
) {

	## implement the filter:
	file = file.path(path, file);
  cmd1 = paste("mkfifo ",  FIFO)
  if (verbose) print(cmd1)
  cmd2 = paste(mycat, file , filterStr, " > ", FIFO, " &")
	if (verbose < 2){#do not execute if level of verbosity is higher than 1
	  if (file.exists(FIFO)) try(file.remove(FIFO));
	  system(cmd1);
	  system(cmd2)
	  if (verbose) {
      cat("just executed ", cmd1, "\n")
	  }
	} #else return(cmd2)
	 #myPipeStr = paste(pipeStr, " | join ", FIFO, " - ", filterStr)
	#browser();
	return(c(cmd1,cmd2))
### filter string
}, ex = function(){
  if (0){
    MakeFIFOs(verbose=2)
    MakeFIFOs(filterStr=" | awk '$2 > 100 && $3 > 5' | 
            cut -f2,3 -d\" \" --complement | head -n 10000 | sort -k1,1")
  }
})
