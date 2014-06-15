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
    if( !equal_length_elements(compounds.list) ) {
      stop("compound.names, concentrations, and compound.types arguments must
             either be NULL or the same length")
    }
    soln$compounds = as.data.frame_with_nulls(compounds.list)
  } else {
    soln$compounds = data.frame(name=character(),conc=numeric(),type=character())
  }
  
  # Make solvents data.frame
  if(!is.null(solvent.df)) {
    soln$solvent = solvent.df
  } else if( !is.null(solvent.names) ||
               !is.null(solvent.percs) ) {
    
    solvent.list = list(name=solvent.names, perc=solvent.percs)
    if( !equal_length_elements(solvent.list) ) {
      stop("solvent.names and solvent.percs arguments must
             either be NULL or the same length")
    }
    soln$solvent = as.data.frame_with_nulls(solvent.list)
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
#' If \code{ID}, \code{i}, or \code{rmVol} are NULL in the function call,
#' they are changed to the defaults in the function definition.
#' 
#' @param ID a string
#' @param i a numeric
#' @param rmVol a numeric
#' @param solution a \code{Solution} object
#' @param ... passed to \code{Solution}
Action = function( ID=NA, i=NA, rmVol=0, 
                   solution=NULL, ... ) {
  
  # Change null arguments to the default above
  if(is.null(ID)) ID=NA
  if(is.null(i)) i=NA
  if(is.null(rmVol)) rmVol=0
  
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

#' Construct an actionList object
#' 
#' An actionList is simply a list with \code{names} equal
#' to the ID of each \code{action} object.
#' 
#' The most straightforward way to construct an actionList is
#' to supply a list of action objects to the \code{actions} argument.
#' Alternatively, several vectors can be supplied to describe multiple
#' actions. The \code{ID}, \code{rmVol}, and \code{i} arguments
#' must be vectors that are the same length \emph{n} that describe
#' \emph{n} actions. One or more of them can be \code{NULL}, but then
#' an empty action will be created for the actionList. A list
#' of \emph{n} \code{Solution} objects can aso be supplied to the
#' \code{solutions} argument. If no Solution object are provided,
#' empty object are constructed and added to the action objects.
#' 
#' If no ID's are provided, the the actions are given numbers
#' as their ID's (numbers change with \code{as.character}). If any
#' of the actions have NA as their ID, these actions are named
#' similarly.
#' 
#' @examples
#' # Empty actionList
#' ActionList()
#' 
#' # Making an action list from actions
#' actions = list(Action(ID="a"), Action())
#' ActionList(actions)
#' 
#' # Making an action list with other arguments
#' ActionList(ID="a")
#' ActionList(i=33)
#' ActionList(ID="a",i=1,rmVol=30,solutions=list(Solution(volume=90)))
ActionList = function( actions=NULL, 
                       ID=NULL, rmVol=NULL, i=NULL, solutions=NULL ) {
  
  if(!is.null(actions)) {
    if( !all(sapply(actions, function(x) "action" %in% class(x) )) ) {
      stop("The actions argument must be a list of action objects")
    }
  } else {
    params = list(ID=ID, rmVol=rmVol, i=i, solutions=solutions)
    if(!equal_length_elements(params)) {
      stop("ID, rmVol, i, and solutions arguments must all be the same
            length or NULL")
    }
    params = equal_length_list(params)
    
    actions = list()
    n.actions = max( 1, length(params[[1]])) # at least one empty action
    
    # Create empty solutions if no solutions provided
    if(is.null(solutions)) params$solutions = rep(list(Solution()),n.actions)
    
    # Create numeric IDs if no IDs provided
    if(is.null(ID)) params$ID = as.character(1:n.actions)
    
    for( ii in 1:n.actions) {
      actions[[ii]] = with(params, Action(i=i[ii], 
                                          ID=ID[ii], 
                                          solution=solutions[[ii]],
                                          rmVol=rmVol[ii]))
    }
  }
  
  class(actions) = c("actionList","list")
  
  # Change any actions that have an ID of NA to a number
  n.actions = length(actions)
  action.ids = ID(actions)
  na.ids = is.na( action.ids )
  if( any(na.ids) ) {
    allowable.ids = setdiff( as.character(1:(n.actions*2)), action.ids )
    ID(actions[na.ids]) = allowable.ids[1:sum(na.ids)]
  }
  
  names(actions) = ID(actions)
  return(actions)
}

#' Constructor for a well object
#' 
#' A well object consists of the information to uniquely identify
#' a well, an \code{actionList} object, the data, and any data
#' splines or smoothers
#' 
#' The \code{file} and \code{location} arguments can be set simply
#' as strings.
#' 
#' A straightforward way to create a well object is with an already
#' available actionList object. If one is not available, then the approach
#' here is to assume that the object will have only one action (and so
#' one solution). Therefore, the ... arguments will be passed to 
#' the \code{Solution} constructor. A one-action actionList will
#' be created, where the action will contain the solution generated
#' from the ... arguments and the \code{Solution} constructor.
#' 
#' Any more complicated well objects will require the more complex
#' Solution, action, and actionList objects to be made beforehand with their
#' associated constructors.
#' 
#' @param file
#' @param location
#' @param actionList
#' @param ...
#' 
#' @examples
#' # A blank well object
#' Well()
#' 
#' 

aa = ActionList(ID=c("a","b"),i=1:2,rmVol=c(0,30),solutions=list(Solution(volume=90),Solution()))

Well = function( file=NA, location=NA, 
                 actionList=NULL, ...) {
  
  well = list( file=file, code=location )
  
  if( !is.null(actionList) ) {
    well$actions = actionList
  } else {
    soln = Solution(...)
    action = Action(solution=soln)
    actionList = ActionList(actions=list(action))
    well$actions = actionList
  }
  class(well) = c("well","list")
  return(well)
}

#' Constructor for a wellList object
#' 
#' A \code{wellList} object is just a \code{list} of well objects.
#' The only purpose of having the \code{wellList} class is so that
#' generic functions that can be applied to \code{wellList} objects.
#' It also provides a nice structure so that any further
#' restrictions (e.g., well locations must be unique) can be
#' ensured.
#' 
#' 




###### Helper functions

# A function to check that all elements in a list are the same
# length. However, some can be NULL
equal_length_elements = function(x) {
  element.lengths = sapply( x, length)
  if( length(unique(setdiff(element.lengths,0))) > 1 ) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Convert a list with equal length elements or NULL elements
# to truly a list with equal length elements by changing the NULL
# elements to several NA values
equal_length_list = function(x) {
  stopifnot(equal_length_elements(x))
  n.elements = max(sapply(x,length))
  if(n.elements==0) return(x) # all are NULL
  x = lapply(x, function(y) {
    if(length(y)==0) {
      return(rep(NA,n.elements))
    } else {
      return(y)
    }
  })
  x
}

# Convert a list with equal length elements or NULL
# elements to a data.frame. NULL elements are made columns
# with all NA values
as.data.frame_with_nulls = function(x) {
  as.data.frame(equal_length_list(x))
}














