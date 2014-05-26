# Read the data using the fread function from data.table
# and attache the data to the well objects
add_data = function(x,...) UseMethod("add_data",x)
add_data.wellList = function( wells, data.dir, parse_fun ) {

  allfiles = filename(wells)
  allcodes = code(wells)
  codes = split( allcodes, allfiles )
  files = names(codes)
  file.paths = file.path(data.dir,names(codes))
  r = roster(wells)
  
  for( i in seq_along(file.paths)) {
    
    codes.i = codes[[i]]
    message(paste("Parsing",files[i]))
    dat = parse_fun(file.paths[i])
    
    for( j in seq_along(codes.i) ) {
      if( !codes.i[j] %in% colnames(dat) ) {
        warning(paste("Well",codes.i[j],"not in file",files[i]))
        next
      }
      well.ij = which( files[i] == r$file & codes.i[j] == r$location )
      wells[[well.ij]]$data = dat[ , c("i","t",codes.i[j]), with=FALSE]
      setnames(wells[[well.ij]]$data, c("i","t","value"))
    }
  }
  wells
}

parse_rtca = function(filepath) {
  dat = fread(filepath)
  dat[,"Time-Interval":=NULL,with=FALSE]
  dat[,i:=1:nrow(dat)]
  cn = colnames(dat)
  for( ii in 2:(ncol(dat)-1) )
    cn[ii] = paste0( substr(cn[ii],4,5) )
  cn[1] = "t"
  setnames(dat,cn)
  #class(dat) = "data.frame"
  dat
}



