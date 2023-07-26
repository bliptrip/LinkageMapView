# edit file
# make sure all positions in the file are numeric

editlgdf  <- function (df) {
    # make sure all positions are numeric
    if (!is.numeric(df[,"position"]))
    { 
      for (i in 1:nrow(df)) {
      message(c("Some positions are not numeric - in linkage group: ",df[i,"group"]))
      if (!is.numeric(df[i,"position"])) {
        message(c("I see ",  df[i,"position"] , " in row " , i))

      }
    }
      stop("Some positions are not numeric") }
}
