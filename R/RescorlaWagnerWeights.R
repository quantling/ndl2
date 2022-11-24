RescorlaWagnerWeights = function(
  cuesOutcomes, 
  nruns=1, 
  random = TRUE,
  randomOrder = NA,
  alpha=0.1, lambda=1, beta1=0.1, beta2=0.1) {

  cues =  unique(unlist(strsplit(as.character(cuesOutcomes$Cues), "_")))
  outcomes = unique(unlist(strsplit(as.character(cuesOutcomes$Outcomes), "_")))

  weights = matrix(0, nrow=length(cues), ncol=length(outcomes))
  rownames(weights) = cues
  colnames(weights) = outcomes

  res = vector(mode="numeric")
  # ultimately, res will have nruns * sum(cuesOutcomes$Frequency) elements

  dfr = data.frame(
    cuevector = rep(cuesOutcomes$Cues, cuesOutcomes$Frequency),
    outcomevector = rep(cuesOutcomes$Outcomes, cuesOutcomes$Frequency),
    stringsAsFactors=FALSE
  )

  theOrder = 1:nrow(dfr)
  for (run in 1:nruns) {
    if (random) {
      if (is.na(randomOrder[1])) {
        theOrder = sample(1:nrow(dfr))
      } else {
        theOrder = randomOrder
      }
    }
    for (i in 1:nrow(dfr)) {
      row = theOrder[i] #The current index based on the (random) order
      currentCues = unique(unlist(strsplit(dfr$cuevector[row],"_")))
      currentCuesInd = which(cues %in% currentCues)
      currentOutcomes = unique(unlist(strsplit(dfr$outcomevector[row],"_")))

      for (j in 1:length(outcomes)) {
      	Vtotal = sum(weights[currentCuesInd,j])
      	if (outcomes[j] %in% currentOutcomes) {
        	Lambda = lambda
      	} else {
        	Lambda = 0
      	}
      	#for (j in 1:length(weightvec)) {  
      	#  if (names(weightvec)[j] %in% currentCues) {
      	#    weightvec[j] = weightvec[j] + alpha*beta1*(Lambda-Vtotal)
      	#  }
      	#}
      	weights[currentCuesInd,j] = weights[currentCuesInd,j]+alpha*beta1*(Lambda-Vtotal)
      }
    }
  }

  return(weights)
}
