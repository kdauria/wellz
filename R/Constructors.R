#' @title Construct a Solution object
#' 
#' @description
#' Construct a \code{Solution} object from many possible inputs
#' 
#' @details
#' A \code{Solution} object is a list with three elements:
#' the \code{volume}, a data frame in the \code{compounds}
#' slot, and a data frame in the \code{solvents} slot. 
#' The compounds data frame has three columns: \code{name} (a string), 
#' \code{conc} (a numeric), and \code{type} (a string either
#' \code{"type"}, \code{"start"}, or \code{"final"}). Each row
#' represents one compound in the solution. The solvents data 
#' frame has two columns: \code{name} (a string) and \code{perc} 
#' (a numeric between >0 and <=100 ).
#' 
#' The volume is set to \code{NA} unless it is specified.
#' 
#' If a compound data frame or solvent data frame is available, the quickest way 
#' to make the solution is to give it as the \code{compound.df} or \code{solvent.df}
#' argument, respectively.
#' 
#' If a compound data frame isn't available, then one will be
#' constructed using the \code{compound.names}, \code{concentrations},
#' and \code{compound.types} arguments. If a solvent data frame isn't
#' available, one will be constructed with the \code{solvent.names} and
#' \code{solvent.percs} argument. All of the arguments used to construct
#' the data frames must be the same length or \code{NULL}. If they are \code{NULL},
#' then the corresponding column will be filled with \code{NA} values.
#' If no arguments are provided for a data frame, then a data frame
#' with the correct columns but zero rows will be returned.
#' 
#' @param volume
#' @param compound.df
#' @param compound.names
#' @param concentrations
#' @param compound.types
#' @param solvent.df
#' @param solvent.names
#' @param solvent.percs
#'
#' @examples
#' # empty Solution object
#' Solution()
#' 
#' # partially filled out Solution object
#' Solution( volume=100, compound.names=c("A"), concentrations=100)
#' 
#' # Error: two concentrations defined for one compound
#' Solution( volume=100, compound.names=c("A"), concentrations=c(10,100))
#' 
#' # A completely filled out solution object
#' Solution( volume=100, compound.names=c("A","B"), concentrations=c(10,100),
#' compound.types=c("start","total"), solvent.names="water",
#' solvent.percs=100)
Solution = function( volume=NA, compound.df=NULL,
                     compound.names=NULL, concentrations=NULL,
                     compound.types=NULL, 
                     solvent.df=NULL,
                     solvent.names=NULL,solvent.percs=NULL) {
  
  soln = list()
  soln$volume = volume
  
  # Make compounds data.frame
  if(!is.null(compound.df)) {
    soln$compounds = compound.df
  } else if( !is.null(compound.names) ||
               !is.null(concentrations) ||
               !is.null(compound.types) ) {
    
    compounds.list = list(name=compound.names, conc=concentrations, type=compound.types)
    ncomps = sapply( compounds.list, length)
    if( length(unique(setdiff(ncomps,0))) > 1 ) {
      stop("compound.names, concentrations, and compound.types arguments must
             either be NULL or the same length")
    }
    
    # Set unspecified columns of the compound data.frame to NA
    nrows = max(ncomps)
    compounds.list = lapply(compounds.list, function(x) {
      if(length(x)==0) {
        return(rep(NA,nrows))
      } else {
        return(x)
      }
    })
    soln$compounds = as.data.frame(compounds.list)
  } else {
    soln$compounds = data.frame(name=character(),conc=numeric(),type=character())
  }
  
  # Make solvents data.frame
  if(!is.null(solvent.df)) {
    soln$solvent = solvent.df
  } else if( !is.null(solvent.names) ||
               !is.null(solvent.percs) ) {
    
    solvent.list = list(name=solvent.names, perc=solvent.percs)
    nsolvents = sapply( solvent.list, length)
    if( length(unique(setdiff(nsolvents,0))) > 1 ) {
      stop("solvent.names and solvent.percs arguments must
             either be NULL or the same length")
    }
    
    # Set unspecified columns of the compound data.frame to NA
    nrows = max(nsolvents)
    solvent.list = lapply(solvent.list, function(x) {
      if(length(x)==0) {
        return(rep(NA,nrows))
      } else {
        return(x)
      }
    })
    soln$solvent = as.data.frame(solvent.list)
  } else {
    soln$solvent = data.frame(name=character(),perc=numeric())
  }
  
  class(soln) = c("Solution","list")
  return(soln)
}

#' Construct an action object
#' 
#' Construct an action object
#' 
#' An action object has one \code{Solution} object in the 
#' \code{solution} slot and three other slots: \code{ID},
#' \code{i}, and \code{rmVol}. \code{ID} is just that, an
#' \code{ID} (a string) for this action. Since actions will need to be referred
#' to by name in many other functions, this can be very useful.
#' \code{i} (a numeric) is the index indicating before which data point this action
#' occurred. It is not the time at which the action occurred.
#' \code{rmVol} (a numeric) is how much volume was removed from
#' the well before the solution from this action was added.
#' 
#' If a solution object is already available, the action
#' can be easily created by giving it in the \code{solution} argument.
#' If it isn't already created, one will be created with
#' the \code{Solution} constructor and the \code{...} arguments.
#' 
#' @param ID a string
#' @param i a numeric
#' @param rmVol a numeric
#' @param solution a \code{Solution} object
#' @param ... passed to \code{Solution}
Action = function( ID=NA, i=NA, rmVol=0, 
                   solution=NULL, ... ) {
  
  action = list( ID=ID, i=i, rmVol=rmVol )
  if(!is.null(solution)) {
    stopifnot( "Solution" %in% class(solution) )
    action$solution=solution
  } else {
    action$solution = Solution(...)
  }
  class(action) = c("action","list")
  return(action)
}




