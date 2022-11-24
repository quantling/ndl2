## Tests Correctness of multiple threads and also tests performance boost.
##
##
require(ndl2)
#Copy files
dir.create("/tmp/ndl2Testing")
file.copy("./tinyCorpus.gz","/tmp/ndl2Testing/tinyCorpus.gz",overwrite=TRUE)
file.copy("./eng.alpha.txt","/tmp/ndl2Testing/eng.alpha.txt",overwrite=TRUE)
file.copy("./eng.dict.txt","/tmp/ndl2Testing/eng.dict.txt",overwrite=TRUE)

## Set working Directory
setwd("/tmp/ndl2Testing")
## Uncompress Test Corpus
system("gunzip -f tinyCorpus.gz")

#Get max number of threads.
MaxThreads = Sys.getenv("OMP_NUM_THREADS")

#Preprocess corpus using max number of threads.
ndlPreprocessCorpus("tinyCorpus","eng.alpha.txt","eng.dict.txt",numThreads=MaxThreads,overwrite=TRUE,verbose=TRUE,maxNGramSize = 1)
base=system.time(wm1 <- learnWeightsCompact("tinyCorpus",numThreads="1",verbose=TRUE))
base = as.numeric(base[3])

## Pick 4 sizes to test.
MaxThreadsN = as.numeric(MaxThreads)
testThreads = c(MaxThreadsN, MaxThreadsN -1, floor(MaxThreadsN*0.75), floor(MaxThreadsN/2), floor(MaxThreadsN*0.25))
## remove non positive integers
testThreads = testThreads[testThreads>0]
# Remove any duplicates
testThreads = unique(testThreads)

for (NTHREADS in testThreads) {
    newtime=system.time(wm <- learnWeightsCompact("tinyCorpus",numThreads=as.character(NTHREADS), verbose=FALSE))
    result = all.equal(wm1, wm)
    cat(paste(c("Test passed for ", NTHREADS, " threads : ", result,"\n"),collapse=""))
    cat(paste(c("Speedup compared to running with one thread:",
                round(
                    base/as.numeric(newtime[3])
                    ,2),
                " times faster.\n",col="")))
}
    


