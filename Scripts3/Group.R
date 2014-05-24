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
# 2. locaton
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


# These two are self-explanatory
# file
# location

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
# concentration.start
# concentration.total
# concentration.all

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
# compound.start
# compound.total
# compound.all


# if multiple solvents, their names are concatenated
# like with concentrations, numeric values are only given
# if there is one solvent for all wells. Otherwise,
# percentages are concatenated with names

# solvent
# solvent.percentages













