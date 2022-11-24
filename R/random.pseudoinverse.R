#' Calculate an approximation of the pseudoinverse of a matrix.
#' 
#' An internal function that uses an approximation of the SVD using the first k
#' singular values of A to calculate the pseudo-inverse. Only used when the
#' cue-cue matrix contains more than 20,000 cues.
#' 
#' This idea was proposed by Gunnar Martinsson Associate Professor and Director
#' of Graduate Studies Department of Applied Mathematics, University of
#' Colorado at Boulder http://amath.colorado.edu/faculty/martinss/ And with
#' ideas from: Yoel Shkolnisky and his Out-of-Core SVD code:
#' https://sites.google.com/site/yoelshkolnisky/software
#' 
#' @param m A matrix.
#' @param k If k = 0, the default, k will be set to the size of 3/4 of the
#' singular values. If not, the k-rank approximation will be calculated.
#' @param verbose Display diagnostic messages or not.
#' @return The approximate pseudoinverse of the input matrix
#' @note No temporary files are used.
#' @section Acknowledgements: Thanks to Gunnar for his help with this!
#' @author Cyrus Shaoul
#' @seealso \link{estimateWeights}, \link{estimateWeightsCompact},
#' @references
#' 
#' "Finding structure with randomness: Probabilistic algorithms for
#' constructing approximate matrix decompositions" Nathan Halko, Per-Gunnar
#' Martinsson, Joel A. Tropp http://arxiv.org/abs/0909.4061
#' @keywords classif
#' @examples
#' 
#' #None (internal function)
#' 
random.pseudoinverse = function (m, verbose=F, k = 0)
{
# computes an approximation of the SVD using the first k singular values of A
  stoch_svd = function(A, k, p = 200, verbose=F) {
                                        # default p=200, may need a larger value here
                                        # to use the the fast.svd function from the corpcor library
                                        #  require(corpcor)
    
                                        # get the dimensions of the matrix
    n = dim(A)[1]
    m = dim(A)[2]
    
                                        # Make a random projection of A    
    if (verbose) message("Making Random Projection")
    flush.console()
    Y <- (A %*% matrix(rnorm((k+p) * m,-1,1), ncol=k+p))
                                        # the left part of the decomposition for A (approximately)
    if (verbose) message("Calculating QR decompostion of Random Projection")
    Q = qr.Q(qr(Y))
                                        # taking that off gives us something small to decompose
    if (verbose) message("Multiply transposed QR with Orig.")
    B = t(Q) %*% A
                                        # decomposing B gives us singular values and right vectors for A
    if (verbose) message("Doing SVD of subset.")
                                        #  s = fast.svd(B)
    s = svd(B)
                                        # Calculate U, Q time U of subset
    if (verbose) message("Get U from Q times U of subset.")
    U = Q %*% s$u
                                        # and then we can put it all together for a complete result
    if (verbose) message("Stoch SVD is complete.")
    flush.console()
    return (list(u=U, v=s$v, d=s$d))
  }
  
  if (k<1) {
                                        #Default: K is the top 3/4 of the singular values.
    k = floor((dim(m)[1]) * 0.75)
    if (verbose) message(c("k = ",k))
  }
  if (k>dim(m)[1]) {
    stop("k must be less that the size of the matrix")
  }
  if (verbose) message ("Starting reduced rank SVD approximation calc.")
  msvd = stoch_svd(m,k,verbose)
  if (length(msvd$d) == 0)
                                        #No singular values, so return zero matrix
    {
      return(array(0, dim(m)[2:1]))
    }
  else
    {
      if (verbose) message ("Done calculating pseudoinverse.")
      return(
        msvd$v %*% (1/msvd$d * t(msvd$u))
        )
    }    
}
