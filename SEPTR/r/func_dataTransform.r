# Create suitable cross-table from data-base table
dataTransform <- function(x) {

  times <- sort(unique(x$time))  #
  strains <- sort(unique(x$strain))

  # transform into temporary cross table (time points appear as separate columns)
  x$item <- paste(x$strain, x$time, sep=".")
  x <- x[,!names(x) %in% c("strain","time")]
  x <- reshape2::dcast(x, ... ~ item, value.var="conc")

  # initialize transconjugatnst to zero
  x$T.0 <- 0

  # Transform into final cross table suitable for comparison with simulation
  #   experiment, flask, from, to, D.ini, R.ini, T.ini, D.end, R.end, T.end
  intervals <- t(combn(times,2))
  colnames(intervals) <- c("from", "to")
  intervals <- intervals[intervals[,"from"] < intervals[,"to"],]
  for (i in 1:nrow(intervals)) {
    # first part (basic info and time interval)
    part1 <- x[,!grepl(pattern="[a-zA-Z][.][0-9]", x=names(x))]
    part1$from <- intervals[i,"from"]
    part1$to <- intervals[i,"to"]
    # second part (initial values)
    part2 <- x[,paste(strains,intervals[i,"from"],sep=".")]
    names(part2) <- paste0(strains,".ini")
    # third part (values at end of interval)
    part3 <- x[,paste(strains,intervals[i,"to"],sep=".")]
    names(part3) <- paste0(strains,".end")
    # append
    if (i == 1)
      y <- cbind(part1, part2, part3)
    else
      y <- rbind(y, cbind(part1, part2, part3))
  }

  # Delete incomplete rows (those containing NA in any column)
  y <- y[apply(y,1,function(x){!any(is.na(x))}),]

  # Delete rows where .end < .ini
  n <- nrow(y)
  for (s in strains) {
    y <- y[y[paste0(s,".end")] >= y[paste0(s,".ini")], ]
  }
  if (nrow(y) < n)
    print(paste0("deleted ",n-nrow(y)," records with negative growth"))

  return(y)
}

