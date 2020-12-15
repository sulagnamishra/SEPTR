# Read data, rename items, check contents, aggregate over replicates
dataPreprocess <- function(file, sep) {

  d <- read.table(file=file, sep=sep, header=TRUE, stringsAsFactors=FALSE)

  # drop unnecessary columns
  d$date <- NULL
  d$volume <- NULL
  d$I.quarter.CFU <- NULL
  d$II.quarter.CFU <- NULL
  d$segment <- NULL

  # rename columns
  # (a) fundamental columns
  names(d)[names(d) == "plate"] <- "strain"
  names(d)[names(d) == "timepoint"] <- "time"
  names(d)[names(d) == "CFUperML"] <- "conc"
  # (b) columns defining replicates (flasks, plates, dilutions)
  names(d)[names(d) == "Flask"] <- "flask"
  names(d)[names(d) == "ReplicatePlateout"] <- "plate"
  names(d)[names(d) == "dilution"] <- "dilution"
  # (c) columns defining experimental conditions
  names(d)[names(d) == "sample"] <- "phase"
  names(d)[names(d) == "DesiredInitialRatio"] <- "D2R_desired"
  names(d)[names(d) == "temperature"] <- "temp"

  # recode values
  # (a) strain names
  d$strain[d$strain == "nal100"] <- "R"
  d$strain[d$strain == "Tet"] <- "D"
  d$strain[d$strain == "str/tet/nal"] <- "T"
  # (b) growth phases
  d$phase[d$phase == "overnight"] <- -1  # early phase
  d$phase[d$phase == "0.3"] <- -1  # early phase
  d$phase[d$phase == "0.5"] <- 0   # intermediate
  d$phase[d$phase == "1.0"] <- 1   # late phase
  d$phase <- as.numeric(d$phase)

  # check for duplicate records
  primaryKeyCols <- c("strain","time","conc","flask","plate","dilution",
    "phase","D2R_desired","temp")
  dups <- which(duplicated(d[,primaryKeyCols]))
  if (length(dups) > 0) {
    warning(length(dups)," duplicate/ambiguous records found")
    for (k in 1:length(dups)) {
      for (i in 1:nrow(d)) {
         if ((i != dups[k]) && identical(unlist(d[i,primaryKeyCols]), unlist(d[dups[k],primaryKeyCols])))
           warning(paste("record",dups[k],"(row",dups[k]+1,") conflicts with record",i,"(row",i+1,")"))
      }
    }
    warning("deleted those records for now")
    d <- d[-dups,]
  }

  # aggregate over plate replicates and dilutions (but not flask replicates)
  d <- aggregate(x=data.frame(conc=d[,"conc"]),
    by=d[, !names(d) %in% c("conc", "plate", "dilution")], FUN=mean)
  d$conc <- round(d$conc)

  return(d)
}

