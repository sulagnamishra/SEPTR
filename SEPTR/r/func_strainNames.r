strainNames <- function(x) {
  n <- c(D="Donor", R="Recipient", T="Transconjugant")
  ifelse(x %in% names(n), n[x], "<undefined>")
}

