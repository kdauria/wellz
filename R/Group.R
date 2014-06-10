#' @title Group wells by characteristics
#' 
#' @description
#' Return a vector that groups wells by characteristics
#' of solution composition and well location
#' 
#' @details
#' When \code{by} equals \code{"location"} or \code{"file"}, a character
#' vector is returned; a numeric vector is returned for \code{"volume"}.
#' 
#' For \code{by="compound"}, the names of the compounds are concatenated
#' so that each well is summarized by one string. By default, the solution
#' of the well's last \code{action} is used. Actions of different names
#' can be used instead by setting the \code{ID} argument. Only certain
#' compounds can be returned by setting the \code{type} argument (see below
#' for details).
#' 
#' There are two types of compound concentration types: "start" which
#' means that this is a typical way that concentration is reported,
#' and "total" which means that the total amount of the compound
#' is tracked and not altered by changing volume (think pebbles
#' at the bottom of a glass whose volume is changing).
#' The default is to output only compounds and concentrations of type "start", although
#' "all" or just the "total" concentrations can also be returned.
#' 
#' For \code{by="concentrations"}, the numeric concentrations can only be given if there
#' is only one compound in all wells. Otherwise
#' it would be unclear to which compound the number refers.
#' If there is more than one compound in all of the wells,
#' then the compound of interest can be specified with the "compound" argument.
#' If there are more than 2 compounds and "compound" isn't specified,
#' then the name and concentration of the compounds are concatenated
#' into a string of the form "compound1-conc, compound2-conc".
#' 
#' \code{by="solvent"} is similar to \code{by="compound"}, except that
#' solvents are returned. \code{by="solvent.percentages"} is similar to
#' \code{by="concentration"} except that the \code{"type"} argument
#' does not apply because solvents are only of one type.
#' 
#' @param x a \code{wellList} object
#' @param by the characteristic to group by (either \code{location}, \code{file}, \code{volume},
#'           \code{compound}, \code{concentration}, \code{solvent}, or \code{solvent.percentages})
#' @param ID choose which \code{action} object to look for in all wells
#' @param type what type of compound concentrations to report (one of \code{start}, 
#'          \code{total}, or \code{all})
#' @param compound the name of the compound to look at
#' @param solvent the name of the solvent to look at


group = function(x,...) UseMethod("group",x)
group.wellList = function(x, by="location", ID="last", 
                          type="start", compound=NULL, solvent=NULL ) {
  
  switch( by,
          location = code(x),
          file == filename(x),
          volume = volume(x),
          compound = compound_names(x, unique=FALSE, type=type, ID=ID ),
          concentration = concentration(x, type=type, ID=ID, compound=compound ),
          solvent = solvent_names( x, ID=ID, unique=FALSE ),
          solvent.percentages = solvent_percentages( x, ID=ID, solvent=solvent) )
}











