context("Learning Weights")


################################################################################
### Helper Functions

deleteWriteMatrixFiles <- function(filePath) {
  unlink(paste0(filePath, c("", ".cues", ".outcomes", ".info")))
}

################################################################################
### Setup

# Comparison Rescorla-Wagner R-function takes quite a while, therefore choose:
dataSource <- 'toy'; # options: toy (lexample), tiny (> 3000 events)
learner <- NULL # set-up below

dat <- NA
if (dataSource == 'toy') { 
  data(lexample) 
  dat <- lexample[sample(rep(1:nrow(lexample), lexample$Frequency)),]
  dat$Frequency <- 1
  dat$Cues <- orthoCoding(dat$Word, grams=2)
} else if (dataSource == 'tiny') {
  dat <- read.table("tiny.tabular.tsv", header=TRUE, comment="", quote="",
                    as.is=TRUE, encoding="UTF-8", fileEncoding="UTF-8")
} else {
  stop(paste0("Invalid data source for testing learning selected: ", dataSource))
}


################################################################################
### Unit tests

test_that("learnWeights produces same results as RescorlaWagnerWeights", {
  learner <<- learnWeights(dat, normalizeCase=FALSE)
  wmRW <- RescorlaWagnerWeights(dat, random=FALSE, nruns=1)
  expect_that(nrow(wmRW), equals(length(learner$getCues())))
  expect_that(ncol(wmRW), equals(length(learner$getOutcomes())))
  # RescorlaWagnerWeights has a different cue/outcome order -> change it
  wmRW <- wmRW[learner$getCues(),]
  wmRW <- wmRW[, learner$getOutcomes()]
  expect_that(learner$getWeights(), equals(wmRW, check.attributes=F, check.names=T))
})

test_that("writing and reading binary weight matrix works", {
  if (is.null(learner))
    skip("learnWeights not working (learner was not created)")
  filePath <- tempfile("unit_test-write_binary-")
  learner$writeWeightMatrix(filePath, binary=T)
  expect_that(readWeightMatrix(filePath, binary=T), equals(learner$getWeights()))
  deleteWriteMatrixFiles(filePath)
})

test_that("writing and reading text weight matrix works", {
  if (is.null(learner))
    skip("learnWeights not working (learner was not created)")
  filePath <- tempfile("unit_test-write_text-")
  learner$writeWeightMatrix(filePath, binary=F)
  # Don't use identical here, as saving matrices as text leads to loss in precision
  expect_that(readWeightMatrix(filePath, binary=F), equals(learner$getWeights()))
  deleteWriteMatrixFiles(filePath)
})

test_that("using digits with writing text weight matrix works", {
  if (is.null(learner))
    skip("learnWeights not working (learner was not created)")
  filePath <- tempfile("unit_test-write_text-")
  learner$writeWeightMatrix(filePath, binary=F, digits=3)
  # Don't use identical here, as saving matrices as text leads to loss in precision
  wmRead <- readWeightMatrix(filePath, binary=F)
  expect_that(wmRead, equals(signif(learner$getWeights(), 3)))
  expect_that(wmRead, not(equals(learner$getWeights())))
  deleteWriteMatrixFiles(filePath)
})



################################################################################
### Cleanup

# datTabularPp$deleteFiles()
