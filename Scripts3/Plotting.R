
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

# Note error here is the gray values that are because of NA values
# Should throw an error here and remove them from the plot
# That's not really possible actually. It should be done at the select stage
x = select(wells,file="HCT8.txt")[1:4]
x = select(wells,file="HCT8.txt", ID="toxinAdd")


ggplot(x, color="concentration") + geom_point() + geom_line()

ggplot.wellList = function( x, ... ) {
  
  maes = well_aes(x, ...)
  maes$group = quote(interaction(file,location))
  dat = melt_wellList_params(x, ...)
  ggplot(dat, maes)
  
}

