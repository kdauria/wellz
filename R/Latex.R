#' Print plate layout in LaTeX
#' 
#' Print out the layout of a playout as a LaTeX table
#' 
#' @import reshape2
#' @import stringr
#' @import xtable
#' @param x a \code{well} object
#' @param ID a \code{character}
#' @param concs show concentrations of wells
#' @param totals show compound of type "total"
#' @param ... passed to \code{print.xtable}
#' @export
latex_layout = function(x, ID="last", concs=TRUE, totals=FALSE, ... ) {
  
  # set up a data frame. One column for each attribute (e.g. concs or totals)
  out = get_coords(x)
  if( concs ) out$concs = group(x, "concentration", ID=ID)
  if( totals ) out$totals = group(x, "compound", type="total")
  
  # The string to put in each well...
  out$string = paste2(out$concs, out$totals, sep=" + ")
  
  # A matrix that will be displayed
  mat = acast( out, row~col, value.var="string" )
  
  # table options
  rowcaption = paste(unique(filename(x)))
  align = c("r","|",rep(c("c","|"),ncol(mat)) )
  hline = 0:(nrow(mat)-1)
  
  # Make the xtable object
  xres = xtable( mat, align=align )
  
  # hack to add one row that acts as a caption and one that adds column names
  rowcaption = paste(unique(filename(x)))
  first.row = str_c( " \\multicolumn{1}{c}{} &",
                     paste0("\\multicolumn{1}{c}{",colnames(mat),"}", collapse=" & "), "\\\\")
  last.row = str_c( str_c( "\\cline{2-",ncol(xres)+1,"}" ), 
                    " \\multicolumn{1}{c}{} & \\multicolumn{",ncol(xres),"}{c}{",rowcaption,"}" )
  atr = list(pos=list(0,nrow(xres)),command=c(first.row,last.row))
  
  # Make the actual latex code
  latex.code = print(xres, include.rownames=TRUE, include.colnames=FALSE,
                     hline.after=hline, add.to.row=atr, print.results=FALSE, ...)
  
  # Modify the latex code for partial horizontal lines
  part.hline = paste("\\\\cline{2-",ncol(xres)+1,"}",sep="")
  latex.code2 = str_replace_all(latex.code, "\\\\hline", part.hline)
  latex.code2
}

#' rows and columns of wells
#' 
#' Returns a data frame with the rows and column of
#' each well
#' 
#' @import stringr
#' @param x a \code{well} or \code{wellList} object
get_coords = function(x,...) UseMethod("get_coords",x)
#' @export
get_coords.character = function(x) {
  row = str_extract(x,"[A-Za-z]+")
  col = as.numeric(str_extract(x,"[0-9]+"))
  return( data.frame(row=row,col=col,stringsAsFactors=FALSE) )
}
#' @export
get_coords.well = function(x) get_coords(code(x))
#' @export
get_coords.wellList = function(x) ldply(x,get_coords)

