library(bigvis)
library(ggplot2)
library(grid)
library(memoise)
set.seed(1013)

tweak <- list(
  theme(
    text = element_text(size = 18),
    plot.margin = unit(c(0.5, 0.25, 0, 0.25), "lines")),
  xlab(NULL)
)

m4 <- function(x, sigma = 1) {
  24 * sqrt(x * (1 - x)) * sin(2 * pi * 1.05 / (x + 0.05)) +
    rnorm(length(x), sd = sigma)
}

x <- sort(runif(1e3, 0, 1))
y <- m4(x)
xsum <- condense(bin(x, 1/1e3/5), z = y, drop = TRUE)

# create false bins becasue the best_h function must have a condensed object
x = timesdata(well)
y = welldata(well)
xsum <- condense(bin(x,min(diff(x))/5), z = y, drop = TRUE)

# break up the condensed bins into additional bins manually
npieces <- function(m) floor(m / (10 * log(m)))
width <- function(x) diff(range(x)) / npieces(length(x))


# Variable smooth --------------------------------------------------------------

nbins = npieces(nrow(xsum))
vec = seq(min(x),max(x),length.out=nbins+1)
bins = findInterval(xsum$x,vec,all.inside=TRUE)
pieces <- split(xsum, bins)
mid <- vapply(pieces,function(x) mean(x$x),1)
              
best_h2 <- memoise(best_h)
foo = function(x) {
  hh = max(as.numeric(diff(x$x)))
  best_h(x,var=".mean",h_init=hh)
}
hs <- lapply(pieces, foo )
failed <- vapply(hs, function(x) attr(x, "conv") != 0, logical(1))

widths <- data.frame(mid = mid, h = unlist(hs))
widths$h[failed] <- NA
widths$smoothed <- predict(loess(h ~ mid, data = widths, na.action = na.exclude))


ggplot(aes(mid, h), data = widths) + 
  geom_line(aes(col = "raw")) + 
  geom_line(aes(y = smoothed, col = "smoothed")) + 
  scale_colour_brewer(palette = "Set1") + 
  labs(colour = NULL) + 
  theme(legend.position = c(0, 1), legend.justification = c("left", "top")) +
  tweak


# Results --------------------------------------------------------------

hf <- approxfun(widths$mid, widths$smoothed, rule = 2)
smooth_one <- function(i) {
  row <- xsum[i, , drop = FALSE]
  smooth(xsum, h = max(0.000000001,hf(row$x)), grid = as.matrix(row[1]), var = ".mean")
}

xvar <- do.call("rbind", lapply(seq_len(nrow(xsum)), smooth_one))
autoplot(xvar) + tweak + geom_point(data=data.frame(x,y),aes(x=x,y=y),alpha=0.3)


