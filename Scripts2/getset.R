################################################################################
#                          Accessor functions                                  #
################################################################################

# get the row from a timeline given the well and action ID
getrows.by.ID = function( x, ID ) UseMethod("getrows.by.ID",x)
getrows.by.ID.wellList = function( x, ID ) do.call( rbind, Map(getrows.by.ID,x,ID) )
getrows.by.ID.Well = function( x, ID ) {
  if( ID == "final" ) {
    row = x$timeline[nrow(x$timeline),]
  } else if ( ID %in% x$timeline$ID ) {
    row = x$timeline[x$timeline$ID==ID,]
  } else {
    a = x$timeline[NULL,]
    a[1,] = NA
    row = a
  }
  return(row)
}

# quick functions to get information from wells
getfiles = function(x) UseMethod("getfiles",x)
getfiles.wellList = function( x ) vapply(x, "[[", "", "file")
getfiles.Well = function(well) well$file
getfiles.list = function(x) if(length(x)>1) vapply(x, "[[", "", "file") else x$file

# the location of each well in a wellList
getlocations = function(x) UseMethod("getlocations",x)
getlocations.wellList = function( wells ) vapply(wells, "[[", "", "location")
getlocations.Well = function(well) well$location


# function to separate the columns and rows from well names
get_coords = function(x,...) UseMethod("get_coords",x)
get_coords.character = function(x) {
  row = str_extract(x,"[A-Za-z]+")
  col = as.numeric(str_extract(x,"[0-9]+"))
  return( data.frame(row=row,col=col,stringsAsFactors=FALSE) )
}
get_coords.Well = function(x) {
  loc = getlocations(x)
  get_coords(loc)
}
get_coords.wellList = function(x) ldply(x,get_coords)

# get the ID's of actions in wellList. 
# This function is really only useful if ID="final"
getIDs = function(x, ID) UseMethod("getIDs",x )
getIDs.wellList = function(x, ID="final") getrows.by.ID(x,ID)$ID

# get solutions from wellList correspond from the action ID
getsolutions = function(x, ID) UseMethod("getsolutions",x)
getsolutions.wellList = function(x, ID="final" ) lapply(x,getsolutions,ID=ID)
getsolutions.Well = function(x,ID="final") getrows.by.ID(x,ID)$status[[1]]

# get all the volumes from a list of solutions
getvolumes = function(x,...) UseMethod("getvolumes",x)
getvolumes.list = function(x) vapply(x,"[[",1,"volume") # for a list of solutions
getvolumes.wellList = function(x,ID="final") vapply(x,getvolumes,1,ID=ID)
getvolumes.Well = function(x,ID="final") getsolutions(x,ID)$volume
getvolumes.Solution = function(x) x$volume

# get all the compounds from a list of solutions or wells
getcompounds = function(x,...) UseMethod("getcompounds",x)
getcompounds.list = function(x,type="all") lapply(x,getcompounds,type)
getcompounds.Solution = function(x,type="all") {
  if(type=="all") {
    out = x$compounds
  } else {
    out = x$compounds[ x$compounds$type==type, ]
  }
  return(out)
}
getcompounds.default = function(x,...) {
  if(is.na(x)) {
    return( data.frame(name=NA_character_,conc=NA_real_,type=NA_character_) )
  } else {
    stop("Can't get compound")
  }
}
getcompounds.Well = function(x,type="all",ID="final") {
  soln = getsolutions(x,ID)
  getcompounds(soln,type)
}
getcompounds.wellList = function(x,type="all",ID="final") {
  lapply(x,getcompounds,type,ID)
}

# Output the contents of compound(s) in a single string
compound_string = function(x,...) UseMethod("compound_string",x)
compound_string.data.frame = function(x,collapse=", ") {
  paste(x$name,x$conc,sep="-",collapse=collapse)
}
compound_string.Solution = function(x,type="all",...) {
  compound_string( getcompounds(x,type), ... )
}
compound_string.Well = function(x,type="all",ID="final",...) {
  compound_string( getcompounds(x,type=type,ID=ID), ... )
}
compound_string.wellList = function(x,type="all",ID="final",...) {
  comps = getcompounds(x,type=type,ID=ID)
  vapply(comps,compound_string,"",...)
}

# get all of the compound names from one or more solutions
compound_names = function(x,...) UseMethod("compound_names",x)
compound_names.Solution = function(x, type="start") {
  getcompounds(x,type=type)$name
}
compound_names.Well = function(x, type="start", ID="final") {
  getcompounds(x,type=type,ID=ID)$name
}
compound_names.wellList = function(x, type="start", ID="final" ) {
  names = lapply(x,compound_names,type=type,ID=ID)
  return( unique(unlist(names)) )
}

# get all the solvents from a list of solutions
getsolvents = function(x) UseMethod("getsolvents",x)
getsolvents.default = function(x) lapply(x,"[[","solvent")

# get the data matrix
datamat = function(x,...) UseMethod("datamat",x)
datamat.Well = function(x) x$data
datamat.wellList = function(x) lapply(x,datamat)
"datamat<-" = function(x,value) { x$data = value; x }

# get all the data matrices from each well
welldata = function(x) UseMethod("welldata",x)
welldata.wellList = function(x) lapply(x,welldata)
welldata.Well = function(x) x$data$values
well.data.default = function(x) return(x)
"welldata<-" = function(x, value) { x$data$values=value; x }

# get the time data from a well
timesdata = function(x) UseMethod("timesdata",x) 
timesdata.wellList = function(x) lapply(x,timesdata)
timesdata.Well = function(x) x$data$time
timesdata.default = function(x) x$time
"timesdata<-" = function(x,value) { x$data$time = value; x}

# get the spline from a well
getSpline = function(x) UseMethod("getSpline",x)
getSpline.Well = function(x) {
  return(x$spline)
}
getSpline.default = function(x) {
  return(identity)
}

# get the sweep numbers from the data matrices
sweepdata = function(x,...) UseMethod("sweepdata",x) 
sweepdata.Well = function(x) x$data$sweep
sweepdata.wellList = function(x) lapply(x,sweepdata)

