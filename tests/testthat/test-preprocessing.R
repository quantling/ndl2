context("Preprocessing Tabular Data")

################################################################################
### Helper Functions

makeDataFrame <- function(cue_vec, outcome_vec, freq=1) {
	data.frame(Cues=cue_vec, Outcomes=outcome_vec, Frequency=freq,
		stringsAsFactors=F)
}

readTabularFile <- function(path, header=TRUE) {
	read.table(path, header=header, comment.char="",
		quote="", as.is=TRUE, encoding="UTF-8", fileEncoding="UTF-8")
}

preprocessedDfr <- function(dfr, normalizeCase=F) {
	outputName <- ndlPreprocessDataframe(dfr, outputDir=tempfile("ndl2UnitTests-"),
		normalizeCase=normalizeCase, returnPath=TRUE)
	return(ndlViewEvents(basename(outputName), 0, 10^8, dirname(outputName)))
}

preprocessedTabular <- function(path, normalizeCase=F, additionalArgs=list()) {
	params <- list("sourceFile"=path, "normalizeCase"=normalizeCase,
		"outputDir"=tempfile("ndl2UnitTests-"), returnPath=TRUE)
	outputName <- do.call(ndlPreprocessTabular, c(params, additionalArgs))
	return(ndlViewEvents(basename(outputName), 0, 10^8, dirname(outputName)))
}

expectThatIsEqualApartFromWithinEventOrder <- function(orig, out) {
	expect_that(orig$Frequency, equals(out$Frequency))
	
  # Splits cue/outcome string for each event and sorts the cues/outcomes.
	# (Just within the events - the order of cues/outcomes between events is kept)
	sortWithin <- function(xs) { lapply(strsplit(xs, "_"), sort) }
	expect_that(sortWithin(orig$Cues), equals(sortWithin(out$Cues)))
	expect_that(sortWithin(orig$Outcomes), equals(sortWithin(out$Outcomes)))
}


################################################################################
### Unit tests

test_that("ndlPreprocessDataFrame works for 1 event with 1 cue, 1 outcome", {
	dfr <- makeDataFrame("cue1", "out1", 1)
	expectThatIsEqualApartFromWithinEventOrder(dfr, preprocessedDfr(dfr))
})

test_that("ndlPreprocessDataFrame works for 1 event with 3 cues, 1 outcome", {
	df1 <- makeDataFrame("cue1_cue2_cue3", "out1", 1)
	expectThatIsEqualApartFromWithinEventOrder(df1, preprocessedDfr(df1))

	df2 <- makeDataFrame("cue3_xxx_cue1", "out1", 1)
	expectThatIsEqualApartFromWithinEventOrder(df2, preprocessedDfr(df2))
})

test_that("ndlPreprocessDataFrame works for 1 event with 1 cue, 3 outcomes", {
	df1 <- makeDataFrame("cue1", "out1_out2_out3", 1)
	expectThatIsEqualApartFromWithinEventOrder(df1, preprocessedDfr(df1))

	df2 <- makeDataFrame("cue1", "out3_out1_yyy", 1)
	expectThatIsEqualApartFromWithinEventOrder(df2, preprocessedDfr(df2))
})

test_that("ndlPreprocessDataFrame simple example data (lexample)", {
	data(lexample)
	lexample$Cues <- orthoCoding(lexample$Word, grams=2)
	expectThatIsEqualApartFromWithinEventOrder(lexample,
		preprocessedDfr(lexample))
})

test_that("ndlPreprocessDataFrame complex example data (tiny.tabular)", {
	dat <- readTabularFile("tiny.tabular.tsv")
	expectThatIsEqualApartFromWithinEventOrder(dat, preprocessedDfr(dat))
})

test_that("ndlPreprocessDataFrame and ...Tabular give the same output", {
	path <- "tiny.tabular.tsv"
	expect_that(preprocessedDfr(readTabularFile(path), normalizeCase=F),
		equals(preprocessedTabular(path), normalizeCase=F))
})

test_that("ndlPreprocessTabular lowerCase parameter works", {
	expect_that(preprocessedTabular("tiny.tabular.tsv", normalizeCase=T),
		equals(preprocessedTabular("tiny.tabular.lowercase.tsv", normalizeCase=F)))
})

test_that("ndlPreprocessTabular maxNumberCues parameter works", {
	maxNumCues = 1000
	expect_that(datLimited <- preprocessedTabular("tiny.tabular.tsv",
		  additionalArgs = list("maxNumberCues"=maxNumCues, verbose=F)),
    prints_text(paste("Dropping low frequency cues.",
                      "Original size of Cues.*3511",
                      paste0("New set size for Cues.*", maxNumCues),
                      "Read 3653 lines and kept [1-3].*% of the events.",
                      sep="\\n")))
	# Without restriction on number of cues (and outcomes) there are 3511 cues.
	numCues <- length(unique(unlist(strsplit(datLimited$Cues, "_"))))
	# Events in which all of the cues get removed (due to cue number limit)
	# get removed completely.
	# In the preprocessor version first tested the cue count was 781, but it
	# could change slightly if the order of outcomes (with same freq) changes.
	expect_that(numCues >= maxNumCues*3/5, is_true())
	expect_that(numCues <= maxNumCues, is_true())
})


test_that("ndlPreprocessTabular maxNumberOutcomes parameter works", {
	maxNumOutcomes = 500
	expect_that(datLimited <- preprocessedTabular("tiny.tabular.tsv",
		  additionalArgs = list("maxNumberOutcomes"=maxNumOutcomes, verbose=F)),
    prints_text(paste("Dropping low frequency outcomes.",
                      "Original size of Outcomes.*1763",
                      paste0("New set size for Outcomes.*", maxNumOutcomes),
                      "Read 3653 lines and kept 9.*% of the events.",
                      sep="\\n")))
	# Without restriction on number of outcomes (and cues) it has 1763 outcomes.
	numOutcomes <- length(unique(unlist(strsplit(datLimited$Outcomes, "_"))))
	expect_that(numOutcomes, equals(maxNumOutcomes))
})
