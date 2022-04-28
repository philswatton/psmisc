bamscale <- function(x, resp = NULL, type=1, left, right,
                     starts=NULL, chains=1, adapt=1000,
                     samples=1000, thin=1, verbose=T) {

  # Step 1: Validate and prepare input

  # Step 2: Starting values

  # Step 3: Pass to rjags function
  bam <- rjags::jags.model("bam.jags",
                           data = list('z' = X, 'q' = J, 'N' = N),
                           inits = list(zhatstar = am$stimuli + rnorm(length(am$stimuli), 0, 1)),
                           n.chains = chains,
                           n.adapt = iter)

  # Step 4: Sample from posterior distributions
  rjags::coda.samples(bam, c("zhat", "a", "b"))

  # Step 5: Return output

}
