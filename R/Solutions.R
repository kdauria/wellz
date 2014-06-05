# S3 generics for "Solution" objects
`+.Solution` = function(s1,s2, rmVol=0) {
  if( rmVol > 0 ) s1$volume = s1$volume - rmVol
  structure( list( solvent=change_solvent( s1, s2, sum), 
             compounds=add_compounds( s1, s2 ), 
             volume=s1$volume+s2$volume),
             class=c("Solution","list"))
}
`-.Solution` = function(s1, s2, rmVol=0 ) {
  if( rmVol > 0 ) s2$volume = s2$volume - rmVol
  
  if( identical(s1,s2) ) {
    s1$solvent = s1$solvent[NULL,]
    s1$compounds = s1$compounds[NULL,]
    s1$volume = 0
    return(s1)
  } else if( s1$volume==s2$volume ) {
    stop("Subtracting different solutions with same volume")
  } else {
    structure( list( solvent=change_solvent( s1, s2, subtract), 
               compounds=subtract_compounds( s1, s2 ) , 
               volume=s1$volume-s2$volume),
               class=c("Solution","list"))
  }
}

# wrapper for adding/subtracting compounds in solution one by one (using ddply, not loops)
add_compounds = function(s1, s2) {
  vols = c(s1$volume,s2$volume)
  nms = unique(c(s1$compounds$name, s2$compounds$name))
  out = data.frame(name=nms,conc=0,type="start",stringsAsFactors=FALSE)
  for( i in nms ) {
    r1 = s1$compounds[ s1$compounds$name==i, ]
    r2 = s2$compounds[ s2$compounds$name==i, ]
    conc = c( ifelse(length(r1$conc),r1$conc,0) , 
              ifelse(length(r2$conc),r2$conc,0) )
    type = c(r1$type, r2$type)
    
    # which solutions have the compound. E.g. id=[1,2] if both  do
    # or id=1 if only the first solution does
    id = (1:2)[as.logical( c(length(r1$conc),length(r2$conc)))]
  
    if( length(type)==2 && sum(type=="total")==1 )
      stop("Either all or none of the 'type's for each compound must be 'total'")
    if( sum(type=="final")==2 )
      stop("Both compounds cannot be of type 'final'")
    
    if( "final" %in% type ) {
      cf = conc[ type=="final" & conc!=0 ]
      type[1] = "start"
    } else {
      cf = switch(type[1],
                  start = sum( vols[id] * conc[id] ) / sum(vols) ,
                  total = sum(conc) )
    }
    out[ out$name==i, "conc" ] = cf
    out[ out$name==i, "type" ] = type[1]
  }
  out
}
subtract_compounds = function( s1, s2 ) {
  if( "final" %in% c(s1$compounds$type,s2$compounds$type) )
    stop("Cannot subtract solutions if one of the compounds is of type 'final'")
  if( s1$volume < s2$volume )
    stop("Second solution has more volume than the first")
  
  allcomps = rbind_compounds(s1,s2)
  na.omit( ddply(allcomps,.(name),subtract_compound_df,c(s1$volume,s2$volume)) )
}


# adding/subtracting compounds from different solutions
subtract_compound_df = function(x,vols) {
  if( length(x)==2 && sum(x=="total")==1 )
    stop("Either all or none of the 'type's for each compound must be 'total'")
  
  if( "total" %in% x$type ) {
    if( subtract( x$conc ) < 0 ) stop("More units in second than first solution")
    cf = subtract( x$conc )
  } else if( "start" %in% x$type ) {
    if( any( subtract( (vols * x$conc) < 0 ) ) ) stop("More mass in second solution than the first")
    cf = subtract( vols[x$i] * x$conc[x$i] ) / subtract(vols)
  }
  
  out = data.frame(conc=cf,type=x$type[1],stringsAsFactors=FALSE)
  out[out$conc!=0,]
}

# Adding/subtracting solvents
change_solvent = function(s1,s2,fun) {
  nms = unique( c(s1$solvent$name, s2$solvent$name) )
  out = data.frame(name=nms,perc=0,stringsAsFactors=FALSE)
  for( i in nms ) {
    out[out$name==i,"perc"] = 
      max(0,s1$solvent$perc[ s1$solvent$name %in% i ]*s1$volume) +
      max(0,s2$solvent$perc[ s2$solvent$name %in% i ]*s2$volume)
  }
  out$perc = 100*out$perc/sum(out$perc)
  out
}

# Helper functions
subtract = function(x) Reduce(`-`, x)
rbind_compounds = function(s1,s2) {
  rbind.data.frame( data.frame(s1$compounds,i=integer(nrow(s1$compounds))+1), 
                    data.frame(s2$compounds,i=integer(nrow(s2$compounds))+2) )
}

