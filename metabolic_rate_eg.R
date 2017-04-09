source("metabolic_rate_functions.R")

files<-list.files(pattern=".txt")

mrates<-readMR(files)

alldata<-extractMR(mrates, names=names(mrates))

check<-extractsteadylength(alldata)

# calculates the amount of oxygen (in %) consumed over the time period
oxloss<-calcoxygenloss(alldata, n = 15)

# Mass (in g) needs to be supplied
# add a column for mass
massdat<-read.csv("mrate_mass.csv")

oxloss$mass<-sapply(oxloss$names, function(x){massdat$mass[which(massdat$id==sub(".*_(.*)", "\\1", x))]})

# calculates the metabolic rate and mass-specific metabolic rate. 
# assumes a 20ml vial; this can be changed by supplying an n argument.
# note that a negative metabolic rate implies an increase in oxygen consumption.
metrate<-calcmetabolicrate(oxloss)
