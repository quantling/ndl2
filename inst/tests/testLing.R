#Getting ready.

#require(ndl)
require(ndl2)

dat = read.table("tiny.test.data.no.punct.tsv",h=TRUE,comm="",quote="",as.is=TRUE, encoding="UTF-8",fileEncoding="UTF-8")

dat.lc = read.table("tiny.test.lc.txt",h=TRUE,comm="",quote="",as.is=TRUE, encoding="UTF-8",fileEncoding="UTF-8")

dat$Frequency=1
dat.lc$Frequency=1

dat.rw = RescorlaWagnerWeights(dat,random=FALSE,nruns=1)
dat.irw = learnWeights(dat,numThreads=1,normalizeCase = FALSE, asMatrix=TRUE)
dat.rw = dat.rw[rownames(dat.irw),]
dat.rw = dat.rw[,colnames(dat.irw)]

dat.lc.rw = RescorlaWagnerWeights(dat.lc,random=FALSE,nruns=1)
dat.lc.irw = learnWeights(dat.lc,numThreads=1,v=TRUE, asMatrix=TRUE)
dat.lc.rw = dat.lc.rw[rownames(dat.lc.irw),]
dat.lc.rw = dat.lc.rw[,colnames(dat.lc.irw)]

class(dat.irw) <- "matrix"
class(dat.lc.irw) <- "matrix"
all.equal(dat.rw, dat.irw, check.attributes=F, check.names=T)
all.equal(dat.lc.rw, dat.lc.irw, check.attributes=F, check.names=T)
