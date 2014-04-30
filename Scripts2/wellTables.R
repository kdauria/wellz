# Custom latex.table.by function
latex.table.by = function (df, num.by.vars = 1, hline=NULL, ...) 
{
  require(xtable)
  if (!is.numeric(num.by.vars) | length(num.by.vars) != 1) {
    stop("num.by.vars must be a number")
  }
  by.vars = 1:num.by.vars
  numcols = length(colnames(df))
  df.original = df
  clines = rep(num.by.vars + 1, length(df[[1]]))
  for (b in rev(by.vars)) {
    groups = rep("", length(df[[b]]))
    for (by.vars.index in 1:b) {
      groups = paste(groups, df.original[[by.vars.index]], 
                     sep = "")
    }
    df[[b]] <- as.character(df[[b]])
    rle.lengths <- rle(groups)$lengths
    first <- !duplicated(groups)
    df[[b]][!first] <- ""
    df[[b]][first] <- paste("\\multirow{", rle.lengths, "}{*}{", 
                            df[[b]][first], "}")
    clines[first] = b
  }
  
  # hack to have partial horizontal lines
  if( !is.null(hline) ) {
    df[hline,1] <- paste("\\cline{", clines[hline]+1, "-", numcols, "}", df[[1]][hline], 
                     sep = "")
  }
  # hack to make the hlines not go up to the top row
  for( i in 1:ncol(df) ) {
    df[1,i] = str_c( "\\multicolumn{1}{c}{",df[1,i],"}")
  }
  
  xt = xtable(df, ... )
}

# A function to make a table of several wells according to 
# the coordinates of the wells
well_table = function(x, ID, concs=TRUE, totals=FALSE, ... ) {
  
  # set up a data frame. One column for each attribute (e.g. concs or totals)
  out = get_coords(x)
  if( concs ) {
    conc.strings = compound_string(x,ID=ID,type="start")
    out = cbind(out,conc.strings,stringsAsFactors=FALSE)
  }
  if( totals ) {
    total.strings = compound_string(x,type="total")
    out = cbind(out,total.strings,stringsAsFactors=FALSE)
  }
  strings = out[,3:ncol(out),drop=FALSE]
  
  # set up the matrix for the output
  n.col = ncol(strings)
  allrows = sort( unique(out$row) )
  allcols = sort( unique(out$col) )
  mat = matrix(NA,nrow=length(allrows)*n.col,ncol=length(allcols))
  rownames(mat) = paste0( rep(allrows,each=n.col), 1:n.col )
  colnames(mat) = allcols
  
  # fill in the matrix
  out = out[order(out$col,out$row), ]
  for( i in 1:n.col ) {
    string = strings[[i]]
    for( j in 1:nrow(out) ) {
      mat[ paste0(out$row[j],i), as.character(out$col[j]) ] = string[j]
    }
  }
  fmat = data.frame( r=rep(allrows,each=n.col), mat, stringsAsFactors=FALSE )
  fmat = rbind(c("",allcols),fmat) # make the top row the column name

  # table options
  rowcaption = paste( unique( getfiles(x) ) )
  align = c("c","r","|",rep(c("c","|"),length(allcols)) )
  
  # Decide where the horizontal lines go
  if( n.col==1 ) {
    hline = 1:length(allrows) + 1
  } else {
    hline = (1:length(allrows))*n.col
  }
  
  # Make the table
  xt = latex.table.by(fmat,align=align,hline=hline)
  attributes(xt) = c(attributes(xt),rowcaption=rowcaption)
  return(xt)
}

print_well_table = function( xt, ... ) {  
  # hack to add partial horizontal line at the bottom of the table
  cmd1 = str_c( "\\cline{2-",ncol(xt),"}" )
  
  # hack to add one row that acts as a caption
  rowcaption = attributes(xt)$rowcaption
  cmd = str_c(cmd1, " \\multicolumn{1}{c}{} & \\multicolumn{",ncol(xt)-1,"}{c}{",rowcaption,"}" )
  
  # The string to add to the last row of the table
  atr = list(pos=list(nrow(xt)),command=cmd)

  print( xt, include.rownames=FALSE, include.colnames=FALSE, 
         sanitize.text.function=force, hline.after=NULL, add.to.row=atr, ... )
}