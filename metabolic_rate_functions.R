# Read in metabolic rate data files
# Return a character vector of file names
readMR<-function(files){
  cnames<-c("date","time","time_s","d1","O2","d2","d3","d4","Tcomp","d5","d6","d7","P",
             "RH","T","d8","d9","raw","d10","d11","d12","signal","d13","d14","d15",
             "ambLight","d16","d17","d18","d19","mainbkgd","d19","d20","d21","mainphi",
             "d22","d23","d24","refampl","d25","d26","d27","refbkgnd","d28","d29",
             "d30","refphi","d31","d32","d33")
  mrlist<-list()
  for (i in 1:length(files)){
    d1<-read.table(files[i], sep="\t", skip=14, col.names=cnames, fill=TRUE)
    d1<-subset(d1, select=-c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,
                             d20,d21,d22,d23,d24,d25,d26,d27,d28,d29,d30,d31,d32,d33))
    d1$date<-as.Date(d1$date, format="%m/%d/%Y")
    d1$hr<-as.numeric(sapply(strsplit(as.character(d1$time), ":"),"[",1))
    d1$min<-as.numeric(sapply(strsplit(as.character(d1$time), ":"),"[",2))
    d1$sec<-as.numeric(sapply(strsplit(as.character(d1$time), ":"),"[",3))
    mrlist[[i]]<-d1
  }
  
  names(mrlist)<-sub("(.+_.+)_.*","\\1", files)
  
  for (i in 1:length(files)){
    mrlist[[i]]$info<-rep(names(mrlist)[i], nrow(mrlist[[i]]))
  }
  
  return(mrlist)
}

# Find the difference between the maximum and minimum values of a vector
maxmin<-function(x){
  result<-max(x) - min(x)
  return(result)
}

# Subset the data for the period over which metabolic rate will be measured.
# This function checks for a period (the length of which is user-defined) when temperature
# fluctuates less than a defined limit, and extracts the start and stop indices for this interval. 
# This function takes as inputs:
# 1. data - a list of data frames, generated using the readMR function
# 2. names - a character vector with the names of the elements from data to be processed
#   *defaults to names = names(data), processing all elements from data
# 3. interval - the time interval (in seconds) that the data was recorded in
#   *defaults to 1 (second)
# 4. width - the time interval over which you want metabolic rate to be calculated (in seconds)
#   *defaults to 2700 (45 minutes); note that if interval != 1, the desired time interval
#    should be divided by interval (e.g. if interval = 2 then width = 1350 will give a 45-minute width)
# 5. by - controls the step interval for moving through the data (see ?rollapply for more information)
# 6. limit - defines the allowable temperature fluctuation
# It returns a list of lists containing the following values:
# values - a subset of the input data
# steady - "yes" if temperatures were within the defined limit for the defined time period; otherwise "no
# lengthsec - the length (in seconds) covered by the data subset. 

extractMR<-function(data, names=names(data), interval=1, width=2700, by=1, limit=0.1){
  mrlist<-rep(list(list("values"=list(), "steady"=vector())), length(names))
  steady<-vector()
  for (i in 1:length(names)){
    extract<-names[[i]]
    dat<-data[[as.character(extract)]]
    test<-rollapply(dat$T, width=width, by=by, FUN=maxmin, align="left")
    startstop<-issteady(test, width, limit)
    mrlist[[i]]$values<-dat[startstop[1]:startstop[2],]
    mrlist[[i]]$steady<-ifelse(min(test)>0.1, "no", "yes")
    mrlist[[i]]$lengthsec<-(startstop[2]-startstop[1])*2
  }
  names(mrlist)<-names
  return(mrlist)
}

# Checks if the temperature fluctuation in a time interval is <= the defined limit.
# If it is, returns the indices for the time interval with the smallest temperature fluctuation
# Otherwise, returns the indices for the longest time interval where temperature fluctions
# are within the defined limits.
issteady<-function(fluct, width, limit){
    if (min(fluct) <= limit){
        a<-min(which(fluct==min(fluct)))
        b<-a + width
        startstop<-c(a, b)
    } else {
        tempmat<-lengthwithinlimit(dat)
        index<-which(tempmat[,3]==max(tempmat[,3]))
        startstop<-c(tempmat[index,1],tempmat[index,2])
    }
    return(startstop)
}

# Finds the longest period where the temperature fluctuation is <=0.1
lengthwithinlimit<-function(dat){
  tempmat<-matrix(nrow=nrow(dat), ncol=3,
                  dimnames=list(rownames(dat),c("start","end","length")))
  start<-1
  while(start<(nrow(dat)-1)){
    end<-start+1
    while(dat$T[start] - dat$T[end] <= limit & end < nrow(dat)){
      end = end + 1
    }
  tempmat[start,]<-c(start, end, (end-start))
  start=start+1
  }
  return(na.omit(tempmat))
}

# Extracts the steady and length values from the object returned by extractMR
# Returns a dataframe containing the file name, steady, and lengthsec information
extractsteadylength<-function(datalist){
  steady<-sapply(datalist, function(x) x$steady)
  length<-sapply(datalist, function(x) x$lengthsec)
  output<-data.frame("name"= names(length),"steady"=steady, "length"=length)
  return(output)
}

# Takes the difference between the first and last oxygen values
# Returns a dataframe with the file name, amount (in %) of oxygen lost,
# and the length of time over which oxygen loss was calculated
calcoxygenloss<-function(data, n = 15){
    diffvec<-vector()
    for (i in 1:length(data)){
        x<-data[[i]][[1]]
        initial<-startaverage(x, n)
        final<-endaverage(x, n)
        diffvec[i]<-initial-final
    }
    names<-names(data)
    lengthsec<-sapply(data, function(x){x$lengthsec})
    return(data.frame(names, diffvec, lengthsec))
}

# Calculates the mean of the first n oxygen values
startaverage<-function(data, n){
  initial<-mean(data$O2[1:n])
  return(initial)
}

# Calculates the mean of the last n oxygen values
endaverage<-function(data, n){
  final<-mean(data$O2[(length(data)-n):length(data)])
  return(final)
}

# Takes oxygen loss data returned by calcoxygenloss.
# Returns a dataframe containing metabolic rate (mlO2/hour) 
# and mass-specific metabolic rate (mlO2/hour/g).
calcmetabolicrate<-function(oxygenloss, container=20){
  ratehour<-with(oxygenloss, ((container-mass)*diffvec)*(1/(lengthsec/3600)))
  ratehourmass<-with(oxygenloss, ratehour/mass)
  names<-oxygenloss$names
  return(data.frame(names, ratehour, ratehourmass))
}