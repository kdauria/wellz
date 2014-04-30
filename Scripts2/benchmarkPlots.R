source("./Scripts2/mf.R")
environment(mf) = asNamespace("lattice")


out = plot(subset, color="by.well", linetype="by.concentrations", 
           xvar="time",manip=TRUE, showpoints=FALSE, spline=FALSE, nbw=1)


trls = lattice::trellis.last.object()
legend = lattice:::evaluate.legend(trls$legend)

############## With customized plot.trellis function
b = NULL
for( i in 1:20 ) {
  a = profr( mf(trls,legend.object=legend,save.object=FALSE), interval=0.01 )[1,"time"]
  b = c(b,a)
}
mean(b) # 0.61 seconds

######### with default trellis function
b = NULL
for( i in 1:20 ) {
  a = profr( print(trls,save.object=FALSE), interval=0.01 )[1,"time"]
  b = c(b,a)
}
mean(b) # 0.95 seconds

######### with ggplot2
b = NULL
for( i in 1:20 ) {
  a = profr( print(p), interval=0.01 )[1,"time"]
  b = c(b,a)
}
mean(b) # 1.88 seconds


# What about outside of Rstudio?
# 0.56
# 0.79
# 1.64

# With manipulate
# ~ 2.2 seconds with lattice. It takes an extra ~1.5 seconds for the manipulate overhead.
# ~ 2 seconds with the ggplot. Apparently ggplot pairs really well with manipulate.
# I'm not using base graphics because it's difficult to make nice plots.

# The real problem is that "manipulate" is not all that fast
# However, it probably isn't manipulate's fault.
# Another problem is that the function
# runs quickly, but then it takes a little while for the computer
# to update the plot. This can be seen by plotting outside of
# Rstudio. I can see the lines and dots rapidly being plotted, one by one.
# Therefore the bottleneck seems to be a combination of grid, base R,
# and the hardware on the computer. I can't see lines being rapidly
# updated in Rstudio, so it seems like Rstudio writes the output
# to a temporary file/memory and then spits it all out at once.

# In practice, the lattice functions (which I optimized for speed)
# take about ~2 seconds to get updated on the screen even though
# the code only takes 0.6 seconds. Oddly, ggplot2 also takes about
# only a little longer to run (~2.5-3 seconds) even though the code 
# takes more than a second longer to run.
# Perhaps ggplot sets up better grobs that are drawn more quickly?
# Whatever the case, the difference isn't huge.

# I could try using Shiny, but I don't think that
# doesn't provide the point and click I'm looking for. 
# I think the best option would be to
# use rCharts and make a nice "control panel" with javaScript. It would also
# be nice that it's accessible online. But,... that is alot of
# work and to be saved for later if done at all
