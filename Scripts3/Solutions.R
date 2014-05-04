# S3 generics for "Solution" objects
`+.Solution` = function(s1,s2) {
  structure( list( solvent=change_solvent( s1, s2, sum), 
             compounds=add_compounds( s1, s2 ), 
             volume=s1$volume+s2$volume),
             class=c("Solution","list"))
}
`-.Solution` = function(s1, s2) {
  structure( list( solvent=change_solvent( s1, s2, subtract), 
             compounds=subtract_compounds( s1, s2 ) , 
             volume=s1$volume-s2$volume),
             class=c("Solution","list"))
}

# wrapper for adding/subtracting compounds in solution one by one (using ddply, not loops)
add_compounds = function( s1, s2 ) {
  allcomps = rbind_compounds(s1,s2)
  ddply(allcomps,.(name),add_compound_df,c(s1$volume,s2$volume))
}
subtract_compounds = function( s1, s2 ) {
  if( "final" %in% c(s1$compounds$type,s2$compounds$type) )
    stop("Cannot subtract solutions if one of the compounds is of type 'final'")
  if( s1$volume <= s2$volume )
    stop("Second solution has more volume than the first")
  allcomps = rbind_compounds(s1,s2)
  ddply(allcomps,.(name),subtract_compound_df,c(s1$volume,s2$volume))
}

# adding/subtracting compounds from different solutions
add_compound_df = function(x,vols) {
  
  if( length(x$type)==2 && sum(x$type=="total")==1 )
    stop("Either all or none of the 'type's for each compound must be 'total'")
  if( sum(x$type=="final")==2 )
    stop("Both compounds cannot be of type 'final'")
  if( "final" %in% x$type ) {
    cf = x$conc[ x$type=="final" ]
    x$type[1] = "start"
  } else {
    cf = switch(x$type[1],
                start = sum( vols[x$i] * x$conc[x$i] ) / sum(vols) ,
                total = sum(x$conc) )
  }
  data.frame(conc=cf,type=x$type[1],stringsAsFactors=FALSE)
}
subtract_compound_df = function(x,vols) {
  if( length(x)==2 && sum(x=="total")==1 )
    stop("Either all or none of the 'type's for each compound must be 'total'")
  
  if( "total" %in% x$type ) {
    if( subtract( x$conc ) <=0 ) stop("More units in second than first solution")
    cf = subtract( x$conc )
  } else if( "start" %in% x$type ) {
    if( any( subtract( (vols * x$conc)<=0 ) ) ) stop("More mass in second solution than the first")
    cf = subtract( vols[x$i] * x$conc[x$i] ) / subtract(vols)
  }
  data.frame(conc=cf,type=x$type[1],stringsAsFactors=FALSE)
}

# Adding/subtracting solvents
change_solvent = function(s1,s2,fun) {
  solvents = rbind(data.frame(s1$solvent,vol=s1$volume), 
                   data.frame(s2$solvent,vol=s2$volume))
  solvents = ddply(solvents, .(name), 
                   function(x) data.frame(perc=fun(x$perc*x$vol) ) )
  solvents$perc = 100 * solvents$perc / fun(solvents$perc)
  solvents
}

# Helper functions
subtract = function(x) Reduce(`-`, x)
rbind_compounds = function(s1,s2) {
  rbind( data.frame(s1$compounds,i=integer(nrow(s1$compounds))+1), 
         data.frame(s2$compounds,i=integer(nrow(s2$compounds))+2) )
}

