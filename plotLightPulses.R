library (ggplot2)
library (tidyr)
library(pracma)

# Functions
#...
home = getwd()
list.files()

x = transient$iSlope2
w = 110
## Function to find changes in slope direction
slopeInvertions <- function(x, w){
  x[which(is.na(x)==TRUE)] = mean(na.omit(x[50:length(x)])) # make NaNs into mean baseline
  #x = savgol(x, fl = 5)
  invIndeces = c()
  direction = 0
  previousDirection = 0
  for(i in c(2:length(x)) ){
    if(abs(x[i]) > (mean(na.omit(x[w:length(x)])) + 5*sd(na.omit(x[w:length(x)]))) ){ # only take into account the values that are 5 sd out of the noise.. why 5? dunno, that's how it worked.
      previousDirection = direction
      direction = x[i]-x[i-1]
      #print(direction)
      if(previousDirection*direction < 0){
        invIndeces = append(invIndeces, i-1)
        #print(i)
      }
    }
  }
   plot(x, type = "l")+
     abline(v=invIndeces)
  return(invIndeces)
} # Inputs a vector. outputs the indeces at which the signal changes direction. ONLY at values above 5 sd above the mean to avoid noise. The problem is that some changes occur within the noise range.


# My theme
#####
My_Theme =  theme(
  axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
  axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0 , vjust = 0.5, face = "plain"),
  axis.title.x = element_text(color = "black", size = 30, angle = 0, hjust = 0.5, vjust = 0, face = "plain"), #element_blank()
  axis.title.y = element_text(color = "black", size = 30, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
  plot.title = element_text(color = "black", size = 35, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),#element_blank()
  legend.text = element_text(size = 20),
  legend.title = element_text(hjust = 0, size = 30),
  panel.grid.major.y = element_line(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(), #element_line()
  panel.grid.minor.x = element_blank()) 
######

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

## Loop through all the .csv traces in the folder, calculate :
# 4 moments: 

TRaisingSlope= NULL
TAmplitude= NULL
TDecaySlope1= NULL
TDecaySlope2= NULL

Trace = NULL 
Pulse = NULL

for (t in list.files()[grep(".csv", list.files())]){
  trace = read.csv(t)
  name = substr(t,1,6)
  # Find point-to-point slope
  iSlope = c()
  stimIndex = which(is.na(transient$V))
  for (i in stimIndex:(nrow(trace)-1)){ # find slope of the whole trace
    iSlope = append(iSlope, (trace$V[i]-trace$V[i+1])/(trace$Time[i]-trace$Time[i+1]) )
  }
  iSlope = append(rep(0,stimIndex), iSlope)
  trace = cbind(trace[,2:3], iSlope)
  # Find point-to-point second derivative
  iSlope2 = c()
  for (i in stimIndex:(nrow(trace)-1)){ # find slope of the whole trace
    iSlope2 = append(iSlope2, (trace$iSlope[i]-trace$iSlope[i+1])/(trace$Time[i]-trace$Time[i+1]) )
  }
  iSlope2 = append(rep(0,stimIndex), iSlope2)
  trace = cbind(trace, iSlope2)
  
  # quantification of pulses. Two pulses per trace, 1 s and 10 s.
  for (p in c(1,2)){
    firstPls = which(is.na(trace$V))[p]
    
    #bl 
    baseline = mean(trace$V[(firstPls-20):firstPls-1])
    # Trace stim -> 20s
    if(p==1){
      W = 50
    } else if(p==2){
      W = 120
    }
    transient = trace[(firstPls-20):(firstPls+W),]
    
    # head(transient)
    ###### raising slope: first peak of first derivative
    moment1 = which(transient$iSlope == min(na.omit(transient$iSlope)) ) #slopeInvertions(transient$iSlope,W)[1]
    raisingSlope = transient$iSlope[moment1]
    
    # Get amplitude
    moment2 =  which(transient$V == min(na.omit(transient$V)) )# max V value averaged with last 9 points
      #slopeInvertions(transient$iSlope2 ,W)[3] # maximum amplitude: V at the 3 peak of the second derivative
    amplitude = transient$V[moment2] - baseline
    
    # Find start of decay: Slope at max point of first derivative
    moment3 = which(transient$iSlope == max(na.omit(transient$iSlope)) )
    decaySlope1 = transient$iSlope[moment3]
    
    # Get second decay slope: the slope at first minimum after the max of slope 2 (assuming it will always be when the light is off and decay is fastest)
    maxIndex = which(transient$iSlope2 == max(na.omit(transient$iSlope2[moment2:nrow(transient)])) )
    moment4 = which(transient$iSlope2 == min(na.omit(transient$iSlope2[maxIndex:nrow(transient)] )) )   
      #slopeInvertions(transient$iSlope2,W)[5]
    decaySlope2 = transient$iSlope[moment4]

      
      
    
    
    
    
    
    
    
    
    
    
    ############## STILL SHOULD ANALYZE THE SLOPES2 AVOIDING SOME CHUNCK OF THE FIRST PART.
      
    
    
    
    
    
    
    
    
    
    
    
  
    # plot trace and slope
    tracePlot <- ggplot(transient, aes(`Time`,`V` )) + #gather(transient, "Variable","Y",  2:4) `Y`, color = `Variable`
      geom_line(size = 1) +
      ylab ("V") + 
      xlab ("Time (s)") + 
      ggtitle(name) +
      geom_vline(xintercept = transient$Time[c(moment1, moment2, moment3, moment4)]  ) +
      My_Theme
    print(tracePlot)
    
    
    
    
    
    readline (prompt="Press key to continue  ")

    TRaisingSlope = append(TRaisingSlope, raisingSlope)
    TAmplitude = append(TAmplitude, amplitude)
    TDecaySlope1 = append(TDecaySlope1, decaySlope1)
    TDecaySlope2 = append(TDecaySlope2, decaySlope2)
    
    Trace = append(Trace, name)
    Pulse = append(Pulse, p)
    Pulse[which(Pulse == 2)]=10
  
    ggsave(paste(name,"-",p,".pdf",sep = ""), tracePlot)
  }
  #write.csv(trace, paste(name,".csv",sep = ""), row.names = FALSE)
}




###############!!!!! IT'S NOT WORKING FOR LONGER PULSES!!


stats =   as.data.frame(cbind(Trace, Pulse, TRaisingSlope,TAmplitude , TDecaySlope1, TDecaySlope2))

stats = separate(stats, "Trace", c("Diameter", "Location"),sep = "-") 
stats$Power = as.numeric(substr(stats$Location,2,2))
stats$Location = substr(stats$Location,1,1)
stats$TRaisingSlope = as.numeric(stats$TRaisingSlope)
stats$TAmplitude = as.numeric(stats$TAmplitude)
stats$TDecaySlope1 = as.numeric(stats$TDecaySlope1)
stats$TDecaySlope2 = as.numeric(stats$TDecaySlope2)
write.csv(stats, paste("pulseStats.csv",sep = ""), row.names = FALSE)


name = "Amplitude"
tracePlot <- stats %>% dplyr::filter(Pulse == 1 ) %>% 
  ggplot( aes(`Location`, `TAmplitude`, size = `Diameter`,  color = `Power`)) + #shape = `Pulse`,
  geom_point() +
  ylab ("voltage (V)") + 
  #xlab ("Time (s)") + 
  ggtitle(name) +
  geom_vline(xintercept = trace$Time[which(is.na(trace$Time))-1], color = "blue", linetype = "dotted", size = 0.8) +
  My_Theme
print(tracePlot)

ggsave(paste(name,".pdf",sep = ""), tracePlot)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#



##### extra editions #####
# cut over thresholds
tst = trace  #tst = read.csv("Col0-ACR1.csv" )
tst$V[which(tst$V < -0.8)]=-0.8
tst$V[which(tst$V > 0)]=0
trace = tst
# equal times
tst$X[which(tst$leaf == "L13")] = tst$X[which(tst$leaf == "L08")] 

# for loop backbone
for (t in list.files()[grep(".txt", list.files())] ){
  name = substr(t,1,6)
  readline (prompt="Press key to continue  ")
  
}






# amplitude

# decay time
plot(transient$Time, transient$V)

tracePlot <- ggplot(gather(trace, "Variable","Y",  2:4), aes(`Time`, `Y`, color = `Variable`)) +
  geom_line(size = 1) +
  ylab ("V") + 
  xlab ("Time (s)") + 
  ggtitle(name) +
  geom_vline(xintercept = transient$Time[c(inflectionPoint, inflectionPointOff, lastInflection,firstSlopeEnd)],  ) +
  My_Theme
print(tracePlot)
