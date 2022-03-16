

list_to_df <- function(listfordf){
  if(!is.list(listfordf)) stop("it should be a list")
  n <- length(listfordf)
  message("lenghth",n)
  strcols <- names(unlist(listfordf[[1]]))
  message("/////////",strcols)
  strrows <- names(listfordf)
  message(strrows)
  if(is.null(strrows))strrows <- 1:n
  if(n>1){
    lapply(2:n, function(i){
      strcols <<- unique(c(strcols,names(unlist(listfordf[[i]]))))
      message("======",strcols)
    })
  }

  df <- data.frame(matrix(0,n,NROW(strcols)))
  colnames(df) <- strcols
  rownames(df) <- strrows

  for(i in 1:n){
    x <- unlist(listfordf[[i]])
    for(j in 1:NCOL(df)){
      if(is.na(x[strcols[j]])){
        df[i,j] <- NA
      } else{
        df[i,j] <- x[strcols[j]]
      }
    }
  }

  return(df)
}

