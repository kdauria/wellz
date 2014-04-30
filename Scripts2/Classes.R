################ Solution ####################

# A class to simplify how solutions are stored
# In the future, may be a nice class to make special operations where
# solutions can be added together into one solution or subtracted
# to figure out what solution to make to get to a defined final concentration

################ RTCAaction ###################
# A class to keep track of actions performed on wells in an RTCA experiment


################ RTCAactionList ###############

# A class to combine multiple actions. Mainly made so that the 
# generic "show" function can be specialized for a list of RTCAactions.
# Also serves as a nice parent class to a timeline class.


############## RTCAactionTimeline ############

# A class to keep track of all the actions. This orders the actions
# from an RTCAactionList and validates that the actions are possible.
# In addition to the solutionLIst ina an RTCAaction list, it has a slot which is
# a list of solutions that keeps track of the well as volumes are added. 
# In other words, this extra solution slot gives a full description of a well's
# contents at every sweep

#################### Well #####################

# A class with all the data for one well
# contains: RTCAactionTimeline, file name for data, welll location, and data

#################### WellList ##################

# A class to hold many wells
# made first for custom print function that quickly
# explains all of the wells
# also useful for applying actions to groups of wells (e.g., transformation)