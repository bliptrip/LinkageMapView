# read passed dataframe

readlgdf <-
  function(df,
           mapthese)
  {
    if (ncol(df) < 3) {
      for (i in 1:ncol(df)) {
        stop("Less than 3 columns found on input data frame - see help for mapthis parameter")
      }

    }

    # make group a character field

    df$group <- as.character(df$group)
    df$locus <- as.character(df$locus)

    if (!missing(mapthese)) {
      if (!all(mapthese %in% df$group)) {
        stop ("chrnames to map not found in input data frame")
      }
    }

    return (df)
  }
