
# Possible aesthetics

# for line plot
# color
# linetype
# size (linewidth)
# alpha

# for point plot
# color
# fill
# alpha
# shape
# size



mplot = function( x, ..., ID="last", type="start", compound=NULL, solvent=NULL ) {
  
  poss.aethetics = c("color","linetype","size","alpha","shape","fill")
  
  args = list( color="concentration", size=1:7 )
  if( !all(names(args) %in% poss.aethetics) )
    stop( paste("plot parameters must be one of", paste(poss.aethetics,collapse=" ") ) )
  
  # change the single character inputs to vectors
  
  
}


x = select(wells,"TcdA",filename="HCT8.txt", ID="toxinAdd")

# melt_wellList_params(x, "volume", "file", "concentration", 
#                      manual.params=list(aa=1:7,bb=2:8), ID="toxinAdd")







