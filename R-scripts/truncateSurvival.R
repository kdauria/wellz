# truncate survival curves when they level off. The first derivative
# is the input


# find the point where a survival function levels off and truncate the data
# at that point
truncateSurvival = function(y) {
  # find the first negative number
  if( any(y < 0) ) {
    # the curve decreases at some point
    first.neg = which(y<0)[1]
    new.y = y[first.neg:length(y)]
    if( any(new.y > 0) ) {  
      next.positive = which(new.y>0)[1]
      out = y[1:next.positive]
    } else {
      out = y
    }
  } else {
    # the curve is monotonically increasing
    out = y
  }
  
  # return a y vector with the same length of x
  padded.out = c( out, rep(NA,length(y)-length(out) ) )
  return(padded.out)
  
}

