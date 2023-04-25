######## DESCRIPTION ############
######## 
### Analysis of traces. Determination of Peak amplitude and duration ###
# Input: A ">GENOTYPE<.csv" file from "loadandplot.R". Put the file of ONE genotype in the home folder.
# Output: a ">GENOTYPE<Summary.csv" file. With the amplitude, raising time, decay time.
###
####### BIASES #######
# traces are filtered with a Savitzky-Golay method which reduces the amplitude (by around 5mV) of traces with a very sharp spike in the peak depolarization. If there is a phenotype at very sharp depol, we are probably missing it.


####   0.       Run all this every time you start the program
#
##
### 
#####
library (ggplot2)
library (tidyr)
library(pracma)
library(signal)
library(dplyr)

# Function for parameter finding
findSWPvalues <- function(trace, plant){
  # correct units
  trace = trace*100

  ### Set reference time 0 when stimulus is applied: either at the first F1 value or or at the first maximum value.
  if(length( which( is.na(trace)) ) > 0 ){
    stimFrames =   which( is.na(trace))
    if( length( which( is.na(trace))) > 2 ) {
      stimFrames =  which( is.na(trace))[c(1,3)]
    }
    # Pick the first NaN (or F1 in DayTime column) for plant 1, and the 3rd NaN for plant 2
    if( plant == 1){
      stimFrame = stimFrames[1] ### Determines the stimulus frame by finding the first NaN. Which is when the pedal is pressed.
    }
    if( plant == 2){
      stimFrame =   stimFrames[length(stimFrames)]  ### Determines the -later- stimulus frame for plant 2
    }
    
  } else {
    stimFrames =  c(90,95)
    # Pick the first NaN (or F1 in DayTime column) for plant 1, and the 3rd NaN for plant 2
    if( plant == 1){
      stimFrame = stimFrames[1] ### Determines the stimulus frame by finding the first NaN. Which is when the pedal is pressed.
    }
    if( plant == 2){
      stimFrame =   stimFrames[2]  ### Determines the -later- stimulus frame for plant 2
    }
  }
  
  
  
  
  ### Filter signal
  trace[which(trace < -150)] = trace[1] # Make extreme values equal baseline. It's when the electrode looses contact.
  # smooth with Savitzky-Golay filter. 
  # n is the amount of point befor/after the nth point to be averaged
  # p is the filter order. Dont know what that is.
   trace = sgolayfilt(na.omit(trace) , p = 1, n = 3)
  # trace = na.omit(trace) ## if not filtering, just omit NaNs
  # Check trace (for debugging)
  plot(trace, type = "l")+
    abline(v=stimFrame) 
  


  ### Find baseline
  bl = mean(trace[ 1:(stimFrame-2)]) # average from 0 to before stimFrame
  
  
  
  ### Find peak depolarization
  maxDepol = min(   na.omit( trace[ stimFrame:length(trace) ] )   ) # find peak depol. It's extracellular recording, so depol goes to negative
  
  depolFrame = which(trace == maxDepol)                # Find frame at maximum  
  
  
  
  ### Find peak of first hyperpolarization between stimulus and depol peak
  maxHyperpol = max(  na.omit(trace[ (stimFrame-10) : which(trace==maxDepol) ])   )
  hyperpolFrame =  which(trace == maxHyperpol)       
  # maxDepol = maxDepol - abs(maxHyperpol) # Calculate max depol from the peak of hyperpol rather than from the initial baseline
  
  
  ### Find Half maximum to depolarization peak     
  halfDepol =  (maxDepol - bl) / 2 # Find halfmax Voltage
    # to find half max frame, substract the halfMax V from the trace and pick the closest to 0.
    # focus on the chunk between stim and max
  depolTrace = trace[ (stimFrame+1) : depolFrame  ] 
  HalfDepolframe = min (
    which (  abs((depolTrace)-halfDepol ) == min( abs((depolTrace)-halfDepol))  )  ## Find the time at Halfdepol
  ) #+ stimFrame 
  
  
  
  ### Find max repolarization
    # focus on repolarization part of the trace
  repolTrace = trace[(depolFrame+1):length(trace)]
  maxRepol = max(repolTrace)
  maxRepolframe = which(repolTrace == maxRepol)+depolFrame
  
  
  
  ### Find second baseline i.e. after repolarization
  bl2 = mean(trace[   (length(trace)-60): length(trace)   ])
  
  
  
  ### Find hyperpolarization amplitude in the recovery phase
  maxHyperpol2 = maxRepol-bl2
  
  
  
  ### Find repolarization half time
  halfRepol = bl2 + (maxDepol - bl2)/2# the voltage value
  halfRepolframe = which( abs(repolTrace-halfRepol) == min (abs(repolTrace-halfRepol))  )   + depolFrame - stimFrame
  

  
  ### Depolarization time. Time from halfmax raise and decay        
  depDur = halfRepolframe - HalfDepolframe
  
  
  #return all these
  SWPvalues = data.frame(
  bl,
  maxHyperpol,
  HalfDepolframe,
  maxDepol,
  halfRepolframe,
  maxRepol,
  maxHyperpol2,
  bl2,
  depDur,
  stimFrame
  )
  return( list(trace, SWPvalues))
  
}

trimTrace <-function(trace, second){
  trimFrame = which(trace$Seconds == second)
  trace = trace[1:trimFrame,]
  plot(trace$V, type="l")
  return(trace)
}

AmIDone <- function(){
  if( nrow(  dplyr::filter(ALLtraces, Plot=="No" ))>0  ){
    print("Nope, you are still missing the following plants: ")
    pendingTraces = dplyr::filter(ALLtraces, Plot=="No" )
    for(i in unique(pendingTraces$Date) ){
      missingDates = i
      ids = unique( pendingTraces$id[which(pendingTraces$Date==i)] )
      print( paste(i,ids,sep = "-")  )
    }
  } else {
    print("Yes, you're done!!!!!!")
    print(paste("Please remember to move all the files related to ",savename,
                  " out of this folder", sep = ""))
    print(getwd())
  }
}
###############################################################################################

####   1.       Move just one >genotype.csv< file to folder and run the following block
#
##
### 
#####
home = getwd()
list.files()
#read.csv( paste( dirname(home),"/","ALLtraces.csv",sep = "" ) )
ALLtraces = read.csv(list.files()[grep("csv", list.files())]) 
ALLtraces$Plot = "No"
# determine which traces I already checked and skip them from.
savename = list.files()[grep("csv", list.files())] # name of current genotype
savename <- savename %>% substr(  1,(nchar(savename)-4)  )
if( length(grep(paste(savename, "Summary.csv", sep = ""),list.files(dirname(home)))) != 0){
  prevSummary = read.csv(paste(dirname(home),"/",savename, "Summary.csv", sep = "")) # Load existing summary
  # Make list with date_id in the summary
  doneRows = unique(unite(prevSummary,id,1,4)[1])$id
  # determine new ids
  newRows = unique(unite(ALLtraces, id, 6,8)[6])$id
  newRows = setdiff(newRows, doneRows)
  # Keep only new data in ALLtraces
   # gives true/false vector with all with T for those that are new
  newTraces = ALLtraces[unite(ALLtraces, id, 6,8)$id %in% newRows,]
  print(unique(unite(newTraces,id,6,8)$id))
  newTraces = NULL
  ALLtraces = newTraces
} else {
  prevSummary = NULL
}
# Empty summary variables
traceSummary <- NULL
#####
####
###
##
#



###############################################################################################



####   2.       Move just one >genotype.csv< file to folder and run the following block
#
##
### 
#####
               ### Am I done with this file? check all the traces to be done
                        AmIDone()
               ### 
### Run from here down if the >genotype<.csv file was not finished.
Date = c()
Stim = c()
id = c()
leaf = c()
#### filter to get to one single plant
subSet0 = dplyr::filter(ALLtraces,Plot == "No")
for (datei in unique(subSet0$Date)){ # pick one date
  subSet1 = dplyr::filter(subSet0, Date == datei)
  # create folder if it doesn't exist
  if( dir.exists(paste( savename,"-",datei,sep = ""))==FALSE ){
    dir.create(paste( savename,"-",datei,sep = ""))
  }
   for(stimi in unique(subSet1$Stim) ) { # pick one stimulus form
    subSet2 = dplyr::filter(subSet1, Stim == stimi) 
    for(IDi in unique(subSet2$id) ) { # pick one id
      subSet3 = dplyr::filter(subSet2, id == IDi) 
      for (leafi in unique(subSet3$leaf) ){ # pick a leaf
        trace = dplyr::filter(subSet3, leaf == leafi)
        
### Check trace to decide if needs trimming of final part
        plot(trace$V, type="l")
        # Trim or not
        answer <- readline (prompt="Keep the trace until which second?  ENTER continue w/o trim | -n- for skipping trace  ")
        if(nchar(answer)>0){
          if(answer == "n"){
            print("Trace not saved")
            #### Erase the trace is not saved!
            traceIndeces = which(ALLtraces$Date == datei & 
                                   ALLtraces$Stim==stimi &
                                   ALLtraces$id == IDi &
                                   ALLtraces$leaf == leafi )
            ALLtraces = ALLtraces[-traceIndeces,]
            write.csv(ALLtraces, paste(savename,".csv",sep = ""), row.names = FALSE)
            traceIndeces = c()
            
            break # this should go to the next leaf
            
          } else {
            trace = trimTrace(trace, answer)
          }
          
        } 
    
        
### FUNCTION TO CALCUATE PARAMETERS         
        SWPanalysis = findSWPvalues(trace$V, trace$plant[1])
        trace = na.omit(trace)
        # Save trace and values independently
        trace$V = SWPanalysis[[1]]
        SWPvalues = SWPanalysis[[2]]
        trace$Seconds = trace$Seconds - SWPvalues$stimFrame  # Make time at stimulus frame = 0

        

### PLOT!        
        plotTitle = paste(IDi,leafi, sep = ".")
        
          plot(x = trace$Seconds, y = trace$V, type = "l", main = plotTitle ) +
          abline(h = SWPvalues$bl, col="black", lty = 2) +
          abline(v = SWPvalues$HalfDepolframe, col="red") +
          abline(v = SWPvalues$halfRepolframe, col="green") +
          abline(h = SWPvalues$maxDepol, col = "red") +
          abline(h = SWPvalues$maxHyperpol, col = "blue") +
          abline(h = SWPvalues$maxRepol, col = "blue") +
          abline(h = SWPvalues$bl2, col="green", lty = 2) +
          abline(v = 0, col="black", lty = 2)

         
          print(paste(datei,IDi,stimi,leafi,sep = "-"))
          print(SWPvalues)
        
        
#### Save or not
        if(SWPvalues$maxDepol > -10){
          answer <- readline (prompt="NO RESPONSE! save as failed trace? [y];[n]    ")
          if(answer == "y"){
            SWPvalues$depDur = 0
            plotTitle = paste("FAIL",plotTitle, sep = "-")
          }
        } else {
          answer <- readline (prompt="Press [y] to save; [n] to skip    ")
        }
        
        while ( is.null(answer)==FALSE ){ #answer != "y" || answer != "n"
          traceIndeces = which(ALLtraces$Date == datei & 
                                 ALLtraces$Stim==stimi &
                                 ALLtraces$id == IDi &
                                 ALLtraces$leaf == leafi )
          if (answer == "y") {
            # put in dataframe
            Date = datei
            Stim = stimi
            id = IDi
            leaf = leafi
            
            # save data
            traceSummary = rbind(traceSummary, cbind(Date,Stim,id,leaf,SWPvalues) )
            
            # save plot
            pdf(file = paste( savename,"-",datei,"/", plotTitle, ".pdf" , sep = "") )
            plot(x = trace$Seconds, y = trace$V, type = "l", main = plotTitle) +
              abline(h = SWPvalues$bl, col="black", lty = 2) +
              abline(v = SWPvalues$HalfDepolframe, col="red") +
              abline(v = SWPvalues$halfRepolframe, col="green") +
              abline(h = SWPvalues$maxDepol, col = "red") +
              abline(h = SWPvalues$maxHyperpol, col = "blue") +
              abline(h = SWPvalues$maxRepol, col = "blue") +
              abline(h = SWPvalues$bl2, col="green", lty = 2) +
              abline(v = 0, col="black", lty = 2) 
            dev.off()
            
            # update database of traces. label the plotted as plotted.
            # Set plot status to YES
            ALLtraces$Plot[traceIndeces] = "Yes"
            write.csv(ALLtraces, paste(savename,".csv",sep = ""), row.names = FALSE)
          
            break
          }
          else if (answer == "n") {
            print("Trace not saved")
            #### borrar del archivo si no se guarda!
            ALLtraces = ALLtraces[-traceIndeces,]
            write.csv(ALLtraces, paste(savename,".csv",sep = ""), row.names = FALSE)
            traceIndeces = c()

            break
          }
          else {
            print("Not valid input")
            answer <- readline (prompt="Press [y] to save; [n] to skip")
            
          }
        } 
      }
    }
  }
}
#####
####
###
##
#
# Sure you are done? 
AmIDone()



###############################################################################################


####   3.    Continue here to save summary data in a file. 
#
##
### 
#####
# put all individual vectors in a dataframe
traceSummary = rbind(prevSummary, traceSummary)

## get rid of infinite values
for(i in 5:ncol(traceSummary)){
  traceSummary[which(traceSummary[,i] == Inf),i] = NaN
}
# check that it looks fine and save
head(traceSummary)
print(savename)
write.csv(traceSummary, paste( savename,"Summary.csv",sep = "" ), row.names = FALSE )
#####
####
###
##
#
## You are done! Make sure to move all the file related to this genotype out of this home folder.

###############################################################################################

####   Troubleshooting tools
#
##
### 
#####
# Down sample to 1Hz if the traces were saved in 10Hz
nrow(ALLtraces)
newIndeces = sort( 
  c( which(ALLtraces$Seconds%%1==0), which(is.na(ALLtraces$Seconds)==TRUE) ), 
  decreasing = FALSE)
ALLtraces = ALLtraces[newIndeces,]
  #save it
write.csv(ALLtraces, paste(savename,".csv",sep = ""), row.names = FALSE)


