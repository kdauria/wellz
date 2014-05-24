# Often, I want a vector describing a certain
# aspect of all wells. For instance, I may want to 
# have a character vector of the location for every 
# well in a well list. For another instance, I may
# want to have the concentration of a compound in a
# numeric vector.

# Below is a list of the current characterstics that
# can be put into a vector. A further description
# of each is provided after the list.
# 1. file
# 2. location
# 3. concentration
# 4. concentration
# 5. concentration.start
# 6. concentration.total
# 7. concentration.all
# 8. compound
# 9. compound.start
# 10. compound.total
# 11. compound.all
# 12. solvent
# 13. solvent.percentages
# 14. volume


# These two are self-explanatory
# file
# location

# For volume, each well has different volumes at different
# points. Therefore, and ID must be input to indicate
# at which point the volume is requested. If it is not
# given, then the volume of the final solution is given.
# volume

# The numeric values can only be given if there
# is only one compound in all wells. Otherwise
# it would be unclear to which compound the number refers.
# If there is more than one compound in all of the wells,
# then the compound of interest can be specified with the "name" argument.
# If there are more than 2 compounds and "name" isn't specified,
# then the name and concentration of the compounds are concatenated
# into a string. If a well has more than one compound, the 
# "compound-conc" strings will be concatenated with a comma delimiter

# concentration
# + type: "start", "all", "total"
# + name: NULL or character

# There are two types of concentrations: "start" which
# means that this is a typical way that concentration is reported,
# and "total" which means that the total amount of the compound
# is tracked and not altered by changing volume (think pebbles
# at the bottom of a glass whose volume is changing)
# The default is output only concentrations of type "start", although
# all or just the total concentrations can also be included.
# Wells with more than one compound have the compounds
# concatenated with a comma delimiter. If the "name" is given,
# then the output is a logical indicating if the compound is in the well

# compound
# + type: "start", "all", "total"
# + name: NULL or character

# if multiple solvents, their names are concatenated
# like with concentrations, numeric values are only given
# if there is one solvent for all wells. Otherwise,
# percentages are concatenated with names

# solvent
# solvent.percentages

group = function(x,...) UseMethod("group",x)
group.wellList = function(x, by="location", type="start", ID="last", compound=NULL ) {
  
  if( by == "location" ) {
    out = code(x)
  } else if( by == "file" ) {
    out = filename(x)
  } else if( by == "volume") {
    out = volume(x)
  } else if( by == "compound" && is.null(compound) ) {
    out = compound_names(x, unique=FALSE, type=type, ID=ID )
  } else if( by=="compound" && !is.null(compound) ) {
    comps = compound_names(x, unique=FALSE, collapse=FALSE, type=type, ID=ID)
    out = sapply(comps, function(x) if(length(x)==1 && is.na(x)) NA else compound %in% x )
  } else if( by =="concentration" ) {
    out = concentration(x, type=type, ID=ID, compound=compound)
  }
  return(out)
}











