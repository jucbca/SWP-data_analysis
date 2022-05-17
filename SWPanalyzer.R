library (ggplot2)
library (tidyr)
library(pracma)

home = getwd()
list.files()
# move desired genotype file to folder and it will be loaded
ALLtraces = read.csv(list.files()[grep("csv", list.files())])

traceSummary <- NULL
Date = c()
Stimulus = c()
individual = c()
Leaf = c()
peakAmplitude = c()
HalfMaxTimes = c()
halfDecay = c()
depolDuration = c()
# filter to get to one single plant
for (datei in unique(ALLtraces$Date)){ # pick one date
  subSet0 = dplyr::filter(ALLtraces, Date == datei)
  for(stimi in unique(subSet0$Stim)) { # pick one stimulus form
    subSet1 = dplyr::filter(subSet0, Stim == stimi) 
    for(IDi in unique(subSet1$id)) { # pick one id
      subSet2 = dplyr::filter(subSet1, id == IDi) 
      for (leafi in unique(subSet2$leaf)){ # pick a leaf
        trace = na.omit(dplyr::filter(subSet2, leaf == leafi)) 
        # smooth with Savitzky-Golay filter necessary for 1Hz sampling? probably not
        #Vtrace = sgolayfilt(c(trace$V) - trace$V[1] , p = 1, n = 9)
        
        # stimulus time
        stimFrame =  min(which(is.na(ALLtraces$V)))
        Vtrace = trace$V 
        # set reference time 0 when stimulus is applied
        trace$Seconds = trace$Seconds - stimFrame + 1
        # average base line
        bl = mean(Vtrace[ 1:stimFrame ]) 
        
        # find peak. It's extracellular recording, so max goes to negative
        
        maximum = min( Vtrace[ stimFrame:length(Vtrace) ])
        maxFrame = which(Vtrace == maximum) 
        # average 1 second before and after peak
        meanMax = mean(Vtrace[(maxFrame-1):(maxFrame+1)] ) 
        # find amplitude between average of max and bl's
        halfMax =  (meanMax - bl) / 2
        
        # to find half max frame, substract from halfMax from the trace and pick the closest to 0.
        stimTrace = Vtrace[ stimFrame :  maxFrame ] - bl # focus on the chunk between stim and max
        HalfMaxtime = min (
          which (  abs((stimTrace)-halfMax ) == min( abs((stimTrace)-halfMax))  )  ## esto esta raro
        ) 

        # get half time to decay
        decayTrace = Vtrace[ maxFrame : length(Vtrace) ] - bl
        
        decay = min (
          which (  abs( (decayTrace)-halfMax ) <= 0.05 )#min( abs( (decayTrace)-halfMax )) ) 
          ) + maxFrame - stimFrame
        
        depDur = decay-HalfMaxtime
        
        
        #plot
        plot(x = trace$Seconds, y = Vtrace, type = "l" ) +
          abline(v = HalfMaxtime, col="red") +
          abline(v = decay, col="red", lty = 2) +
          abline(h = bl, col = "blue") +
          abline(h = meanMax, col = "blue") +
          abline(v = 0, col="black")

        print(paste(datei,stimi,IDi,leafi,maximum,HalfMaxtime,decay,depDur, sep = "-"))
        saveVal <-  readline(prompt="Enter [s] to save, [n] to discard:")
        
        if (saveVal == "s"){ ### decide if keep or not!!!
          Date = append(Date, datei)
          Stimulus = append( Stimulus, stimi)
          individual = append(individual, IDi)
          Leaf = append(Leaf, leafi)
          peakAmplitude = append(peakAmplitude, maximum)
          HalfMaxTimes = append(HalfMaxTimes, HalfMaxtime)
          halfDecay = append(halfDecay, decay)
          depolDuration = append(depolDuration, depDur)
        }
      }
    }
  }
}

traceSummary = data.frame( Date, Leaf, Stimulus, individual, peakAmplitude, HalfMaxTimes, halfDecay, depolDuration )
for(i in 5:ncol(traceSummary)){
  traceSummary[which(traceSummary[,i] == Inf),i] = NaN
}

head(traceSummary)
savename = list.files()[grep("csv", list.files())]
savename <- savename %>% substr(  1,(nchar(savename)-4)  )
write.csv(traceSummary, paste( dirname(home),"/",savename,"Summary.csv",sep = "" ), row.names = FALSE )


# proper inflection point finder https://stats.stackexchange.com/questions/76959/finding-inflection-points-in-r-from-smoothed-data




