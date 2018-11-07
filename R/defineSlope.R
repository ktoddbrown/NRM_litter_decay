defineSlope <- function(x, debug=FALSE){
  ans <- NA
  
  if(debug){
    print(summary(x))
  }
  
  
  #Wetting
  flag <- c(NA, x[-length(x)]) < x & x < c(x[-1], NA)
  flag[is.na(flag)] <- FALSE
  ans[flag] <- 'increasing'
  
  #drying
  flag <- c(NA, x[-length(x)]) > x & x > c(x[-1], NA)
  flag[is.na(flag)] <- FALSE
  ans[flag] <- 'decreasing'
  
  #peak
  flag <- c(NA, x[-length(x)]) <= x & x >= c(x[-1], NA)
  flag[is.na(flag)] <- FALSE
  ans[flag] <- 'peak'
  
  #valley
  flag <- c(NA, x[-length(x)]) >= x & x <= c(x[-1], NA)
  flag[is.na(flag)] <- FALSE
  ans[flag] <- 'valley'
  
  #TBD
  flag <- c(NA, x[-length(x)]) == x &  x == c(x[-1], NA)
  flag[is.na(flag)] <- FALSE
  ans[flag] <- NA
  
  if(is.finite(x[1]) & x[1] == min(x, na.rm=TRUE)){
    ans[1] <- 'valley'
  }
  
  for(rowII in 2:(length(ans)-1)){
    if(is.na(ans[rowII])){ #Deal with TBD
      if(grepl('increasing', ans[rowII - 1])){
        ans[rowII] <- 'peak'
      }else if(grepl('decreasing', ans[rowII - 1])){
        ans[rowII] <- 'valley'
      }else{
        ans[rowII] <-  ans[rowII - 1]
      }
    }
  }
  
  ans[is.na(x)] <- NA
  return(ans)
}
