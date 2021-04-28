##################### 
# This script could be used to plot the ancestral states of stochastic character mapping HiSSE. Adapted from RevBayes Manual
##################### 

library(plotrix)
library(phytools)

character_file = "output/stochastic-mapping/marginal_character.tree"

sim2 = read.simmap(file=character_file, format="phylip")
#ladderize.simmap(sim2,right=TRUE)->sim2

####################
# Define colors for 4 character states
####################

# There are 4 states in our HiSSE analysis (0=1A, 1=2A, 2=1b, 3=2B), including the observed characters (1= terrestrial, 2= aquatic) and the hidden states (A, B). Therefore, in this case, terrestrial is represented by 0, 2, aquatic by 1, 3.
colors = vector()
for (i in 1:length( sim2$maps ) ) { 
    colors = c(colors, names(sim2$maps[[i]]) )
}
colors = sort(as.numeric(unique(colors)))
colors
cols = setNames( rainbow(length(colors), start=0.0, end=0.9), colors)
cols

## change colors to assing the same color to hidden states A and B of each character state 1 and 2.
cols[[1]] <- "darkgoldenrod3"
cols[[2]] <- "deepskyblue2"
cols[[3]] <- "darkgoldenrod3"
cols[[4]] <- "deepskyblue2"


####################
# plot ancestral states of stochastic character mapping HiSSE
####################

library(phytools)
library(plotrix)

pdf("RevBayes_StochCharMap_HiSSE.pdf", paper="special", height =16, width=11)
plotSimmap(sim2, cols, direction="rightwards",fsize=0.001, lwd=1, split.vertical=TRUE, type="fan")
dev.off()
# add legend
leg = names(cols)
leg
add.simmap.legend(leg, colors=cols, cex=0.3, x=0.8, y=0.8, fsize=0.8)
A = 0
B = 350
C = 700
D = 1150

####################
# plot posteriors ancestral states of stochastic character mapping HiSSE
####################

posterior_file = "output/stochastic-mapping/marginal_posterior.tree"
sim_p = read.simmap(file=posterior_file, format="phylip")

# Define colours for posterior probability 
colors = vector()
for (i in 1:length( sim_p$maps ) ) { 
  colors = c(colors, names(sim_p$maps[[i]]) )
}
colors = sort(as.numeric(unique(colors)))

# We can use different two colour choices to plot the posterior tree as a "heatmap". For posteriors, this works better.
cols = setNames( heat.colors(length(colors), rev=TRUE), colors)

# Or using a basic palette with red, yellow, blue, etc.
# cols = setNames( rainbow(length(colors), start=0.0, end=0.9, rev=TRUE), colors)
# fsize is font size for tipe labels, lwd = line width for plotting, ftype = b (bold), i (italics)
# pts: whether to plot filled circles at each tree vertex, as well as transition points between mapped states: default is false.

plotSimmap(sim_p, cols, fsize=1.0, lwd=2.0, split.vertical=TRUE, ftype="bi", pts=FALSE)

# Add legend
# To identify which colour corresponde to which value of the posterior probability
leg = names(cols)
leg
add.simmap.legend(leg, colors=cols, cex=0.2, x=0.2, y=0.2, fsize=0.3)

# A message appears in console: "Click where you want to draw legend". Click and draw in RQuartz window to get the legend plotted.

# Save image using Save ----- RPlot



