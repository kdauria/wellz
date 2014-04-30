################################################################################
#                            print Methods                                     #
################################################################################

print.RTCAaction =  function(object) {
  cat("type: \"",object$ID,"\", ",
      "sweep ", object$t1," & ", object$t2, 
      sep="")
  #if(object$plateRemoved) cat(", plate out")
  #if(object$wellDisturbed) cat(", well touched")
  cat("\n")
  if(object$rmVol!=0) {
    cat("removed: ",object$rmVol,"uL\n",sep="")
  }
  if(object$solution$volume!=0) {
    soln = capture.output(summary(object$solution))
    cat("added: ",object$solution$volume, "uL ", soln ,sep="")
  }
}

print.Solution = function(object, indent=0) {
  
  # print volume
  cat(object$volume,"uL ")
  
  # print solvent
  if(nrow(object$compounds)>0) cat("\nSolvent: ")
  if( nrow(object$solvent)==1 ) {
    cat(rownames(object$solvent)[1])
  } else {
    for( i in 1:nrow(object$solvent) ) {
      cat(rownames(object$solvent)[i]," ",object$solvent[i,"pctg"],"%",sep="")
      if(i!=nrow(object$solvent)) cat(", ")
    }
  }
  cat("\n")
  
  # print off compounds
  if( nrow(object$compounds)>0 ) {
    for( i in 1:nrow(object$compounds) ) {
      ro = object$compounds[i,]
      cat( ro$name, " ", sep="" )
      if( !is.na(ro$type) & ro$type=="final" ) {
        cat( ro$conc, "ng/ml in final solution." )
      } else if ( !is.na(ro$type) & ro$type=="start" ) {
        cat( ro$conc, "ng/ml." )
      } else if ( !is.na(ro$type) & ro$type=="total" ) {
        cat( ro$conc, "total." )
      } 
      cat("\n") 
    }
  }
  
}

print.Well = function( object ) {
  
  t = timesdata(object)
  cat0("Well: ",object$file,", ",object$location,"\n")
  cat0( format(nrow(object$data),big.mark=","), " data points over ")
  cat0( round(diff(range(t)),1), " hours\n\n" )
  
  print(object$timeline)
}

print.wellList = function( object, ID="final" ) {
  # get all the information
  files = getfiles(object)
  locs = getlocations(object)
  points = format( vapply( welldata(object), length, 1 ), big.mark="," )
  duration = round( vapply( timesdata(object), function(x) diff(range(x)), 1 ), 1 )
  
  totals = compound_string(object,ID=ID,type="total")
  others = compound_string(object,ID=ID,type="start")
  
  # put it all in a data frame
  out = data.frame(file=files,points,
                   hours=duration,well=locs,totals=totals,
                   ID.soln=others,
                   stringsAsFactors=FALSE)
  
  # remove repeats of rows and replace with a dash
  skipcount = rep(0,ncol(out))
  if(nrow(out)>1) {
    for( i in nrow(out):2 ) {
      out[ i, out[i,] == out[i-1,] ] = "-"
    }
  }
  print(out)
}

# summarize the status over a timeline
status = function(x) UseMethod("status",x)
status.RTCAactionTimeline = function( x ) {
  
  y = x[,1:4]
  colnames(y) = c("ID","sweep","uL","status")
  solns = vapply(x$status, function(x) capture.output(summary(x)),"")
  vols = vapply(x$status, function(x) x$volume,1)
  y$uL = vols
  y$status = solns
  y
  
}

################################################################################
#                               as.* methods                                   #
################################################################################

as.data.frame.RTCAaction = function(object) {
  atomic = vapply(object,is.atomic,TRUE)  
  df = as.data.frame( object[atomic], stringsAsFactors=FALSE )
  df$solution = I(list(object$solution))
  return(df)
}

################################################################################
#                             Summary Methods                                  #
################################################################################

summary.Solution = function(object, showtype=TRUE) {
  
  # figure out what concentration to show
  full.string = ""
  if(nrow(object$compounds)>0) {
    for( i in 1:nrow(object$compounds) ) {
      row = object$compounds[i,]
      number = row$conc
      if( is.na(row$type) ) {
        unit = ""
        type= ""
      } else if( row$type == "start") {
        unit = "ng/ml"
        type = ""
      } else if( row$type == "final") {
        unit = "ng/ml"
        type = " final"
      } else if ( row$type == "total") {
        unit = ""
        type = "total"
      } else {
        unit = ""
        type = ""
      }
      conc = str_c(unit,ifelse(showtype,type,""))
      conc.string = str_c(row$name," ",round(number,3)," ",conc,", ")
      full.string = str_c(full.string,conc.string)
    }
    summary.string = str_trim(str_sub( full.string, start=1, end=-3 ))
    cat(summary.string)
  } else {
    if( nrow(object$solvent)==1 ) {
      cat(rownames(object$solvent)[1])
    } else {
      for( i in 1:nrow(object$solvent) ) {
        cat(rownames(object$solvent)[i]," ",object$solvent[i,"pctg"],"%",sep="")
        if(i!=nrow(object$solvent)) cat(", ")
      }
    }
  }
}

summary.RTCAaction = function(object) {
  df = as.data.frame(object)
  df$solution = summary(object$solution)
  df$`uL+` = object$solution$volume
  df = df[,c("actionType","sweepBefore","volumeRemoved","uL+","solution")]
  names(df) = c("type","sweep","uL-","uL+","solution")
  return(df)
}

summary.RTCAactionList = function( object ) {
  
  cat("\"RTCAactionList\" object\n-----------------------\n")
  
  cat("This \"data.frame\" describes ",nrow(object)," RTCA action",
      ifelse(nrow(object)>1,"s",""), ".\n",sep="")
  cat("The \"solution\" column contains one \"Solution\" object for each action\n\n")
  print(head(as.data.frame(object), n=6))
  if(nrow(object)>6) {
    left = nrow(object)-6
    cat("---- ",left," row",ifelse(nrow(object)>1,"s","")," not shown ----",sep="")
  }
}


################################################################################
#                          Custom operators                                    #
################################################################################

# shortcut functions for adding or subtracting solutions
`+.Solution` = function(s1,s2) changeSolutions(s1, s2, `+`)
`-.Solution` = function(s1,s2) changeSolutions(s1, s2, `-`)
sum.Solution = function(sList) Reduce(`+.Solution`,sList)

# general function to add or subtract solvents from two solutions
changeSolvents = function( s1, s2, fun=`+` ) {
  
  # set up new solvent matrix
  v1 = s1$volume
  v2 = s2$volume
  vf = fun(v1,v2)
  sv1 = as.data.frame(s1$solvent)
  sv2 = as.data.frame(s2$solvent)
  sv.names = union( rownames(sv1), rownames(sv2) )
  svf = matrix(NA,nrow=length(sv.names))
  rownames(svf) = sv.names
  colnames(svf) = colnames(sv1)
  
  # calculate new solvent percentages one by one
  for( nm in sv.names ) {
    c1 = ifelse(is.na(sv1[nm,]),0,sv1[nm,])
    c2 = ifelse(is.na(sv2[nm,]),0,sv2[nm,])
    cf = fun(c1*v1,c2*v2)/vf
    svf[nm,] = cf
  }
  
  svf2 = svf[svf[,1]!=0,,drop=FALSE]
  return(svf2)
}

# general function to add or subtract two solutions
changeSolutions = function( s1, s2, fun=`+` ) {
  
  # variable names for easier coding
  sf = s1
  sf$volume = fun(s1$volume, s2$volume)
  c1 = s1$compounds
  c2 = s2$compounds
  
  # calculate solvent volume percentages
  sf$solvent = changeSolvents(s1, s2, fun)
  
  # get all of the compounds (ID'd by name). Set up final compound df
  c.names = union(c1$name, c2$name)
  c3 = as.data.frame(matrix(NA,ncol=ncol(c1),nrow=length(c.names)))
  names(c3) = names(c1)
  c3$name = c.names
  
  # now go through each compound individually
  compoundFun = if(identical(fun,`+`)) addCompound else subtractCompound
  for( nm in c.names ) {
    c3[ c3$name==nm, ] = compoundFun(s1,s2,nm)
  }
  sf$compounds = c3
  sf$compounds$name = c.names
  sf$compounds = sf$compounds[ sf$compounds$conc != 0, ]
  return(sf)
  
}

# support function for changeSolutions. Adds one compound in two solutions.
addCompound = function( s1, s2, name ) {
  
  # organize arguments just for easier coding
  v1 = s1$volume
  v2 = s2$volume
  comp1 = s1$compounds[s1$compounds$name==name,]
  comp2 = s2$compounds[s2$compounds$name==name,]
  c1 = comp1$conc
  c2 = comp2$conc
  t1 = comp1$type
  t2 = comp2$type
  vf = v1+v2
  
  # change format of concentration type if the length is 0
  if(is.bs(t1)) t1=""
  if(is.bs(t2)) t2=""
  if(t1=="" & t2=="") stop("The concentration type is not defined")
  
  # decide if the addition can be done
  {
    if( t1 == "start" ) {
      
      if( t2 == "start" ) {
        cf = (c1*v1 + c2*v2)/vf
        tf = "start"
      } else if ( t2=="total" ) {
        stop("This requires unit conversion -- saving for later versions")
      } else if ( is.bs(t2) ) {
        cf = c1*v1/vf
        tf = "start"
      } else if( t2=="final" ) {
        warning("Adding concentrations of type start and type final")
        cf = c2
        tf = "start"
      }     
    } else if ( t1 == "total" ) {
      
      if( t2 == "start" ) {
        stop("This requires unit conversion -- saving for later versions")
      } else if ( t2=="total" ) {
        cf = c1 + c2
        tf = "total"
      } else if ( is.bs(t2) ) {
        cf = c1
        tf = "total"
      } else if( t2=="final" ) {
        stop("This requires unit conversion -- saving for later versions")
      }       
    } else if ( is.bs(t1) ) {
      
      if( t2 == "start" ) {
        cf = v2*c2/vf
        tf = "start"
      } else if ( t2=="total" ) {
        cf = c2
        tf = "total"
      } else if ( is.bs(t2) ) {
        stop("The named compound is in neither solution")
      } else if( t2=="final" ) {
        cf = c2
        tf = "start"
      }       
    } else if ( t1=="final" ) {
      
      if( t2 == "start" ) {
        warning("Adding concentrations of type final and type start")
        cf = c1
        tf = "start"
      } else if ( t2=="total" ) {
        stop("This requires unit conversion -- saving for later versions")
      } else if ( is.bs(t2) ) {
        cf = c1
        tf = "start"
      } else if( t2=="final" ) {
        stop("Not allowed to add concentrations of type final and type final")
      } 
      
    } 
  }
  
  compf = data.frame(name=name,conc=0,conc.type="")
  compf$conc.type = tf
  compf$conc = cf
  
  return(compf)
}

# support function for changeSolutions. Subtracts one compound in two solutions.
subtractCompound = function( s1, s2, name ) {
  # organize arguments just for easier coding
  v1 = s1$volume
  v2 = s2$volume
  comp1 = s1$compounds[s1$compounds$name==name,]
  comp2 = s2$compounds[s2$compounds$name==name,]
  c1 = comp1$conc
  c2 = comp2$conc
  t1 = comp1$type
  t2 = comp2$type
  vf = v1-v2
  
  # change format of concentration type if the length is 0
  if(is.bs(t1)) t1=""
  if(is.bs(t2)) t2=""
  if(t1=="" & t2=="") stop("The concentration type is not defined")
  
  # decide if the subtraction can be done
  {
    if( t1 == "start" ) {
      
      if( t2 == "start" ) {
        cf = (c1*v1 - c2*v2)/vf
        tf = "start"
      } else if ( t2=="total" ) {
        stop("This requires unit conversion -- saving for later versions")
      } else if ( is.bs(t2) ) {
        cf = c1*v1/vf
        tf = "start"
      } else if( t2=="final" ) {
        stop("Not allowed to subtract concentration of type final from type start.")
      }     
    } else if ( t1 == "total" ) {
      
      if( t2 == "start" ) {
        stop("This requires unit conversion -- saving for later versions")
      } else if ( t2=="total" ) {
        cf = c1 - c2
        tf = "total"
      } else if ( is.bs(t2) ) {
        cf = c1
        tf = "total"
      } else if( t2=="final" ) {
        stop("Not allowed to subtract concentration of type final from type total")
      }       
    } else if ( is.bs(t1) ) {
      stop("The named compound is not in the minuend")
    } else if ( t1=="final" ) {
      stop("Not allowed to subtract any concentration from a concentration of 
             type final")      
    }
  }
  
  compf = data.frame(name=name,conc=0,conc.type="")
  compf$conc.type = tf
  compf$conc = cf
  
  return(compf)
}

# Get the total mass of each compound in a solution
compound_mass = function(x,...) UseMethod("compound_mass",x)
compound_mass.Solution = function(x, compounds=NULL) {
  
  comps = getcompounds(x)
  comps = comps[ comps[["type"]] != "total", ]
  vol = getvolumes(x)
  mass = comps[["conc"]] * vol
  names(mass) = comps[["name"]]
  
  if(!is.null(compounds)) {
    nc = length(compounds)
    cm = matrix(NA,ncol=nc)
    colnames(cm) = compounds
    cm[1,] = mass[compounds]
    mass = data.frame(cm)
    rownames(mass) = NULL
  } 
  return( mass )
}
compound_mass.Well = function(x,compounds=NULL, ID="final") {
  soln = getsolutions(x,ID)
  mass = compound_mass(soln,compounds)
  return(mass)
}
compound_mass.wellList = function(x, compounds=NULL, ID="final") {
  mass = lapply(x, compound_mass, ID=ID, compounds=compounds)
  if(!is.null(compounds)) mass = rbindlist(mass)
  return(mass)
}

# Get the total mass of all compounds in all wells. Output as a data frame/table
compound_masses = function(x,...) UseMethod("compound_masses",x)
compound_masses.wellList = function(x, type="start", ID="final" ) {
  comps = compound_names(x,type,ID)
  comp.dt = compound_mass(x,compounds=comps,ID)
  return(comp.dt)
}

# Get a string per well summarizing the mass of compounds in that well
compound_masses_string = function(x,...) UseMethod("compound_masses_string")
compound_masses_string.wellList = function(x, type="start", ID="final" ) {
  dt = compound_masses(x,type=type,ID=ID)
  comp.names = colnames(dt)
  strings = apply(dt,1,function(x) paste(comp.names,x,sep=":",collapse="-") )
  return(strings)
}

################################################################################
#                              Subset Methods                                  #
################################################################################

`[.wellList` = function( x, i ) {
  class(x) = "list"
  y = x[i]
  class(y) = c("wellList","list")
  y
}

c.wellList = function(x,...) {
  class(x) = "list"
  out = c(x,...)
  class(out) = c("wellList","list")
  return(out)
}

