library (ggplot2)
library (tidyr)
# Functions
DataExtraction <- function(data, date){ 
  
  output <- list()
  time1 = data$Time
  ToD = data$TimeOfDay
  #determine which have info
  PlantNumber = 1
  if ( length(grep("L08",names(data))) > 1){
    PlantNumber = 2
  }
  # Extract info one plant at a time.
  for( n in 1:PlantNumber){
    leaf1 = paste("L08.", n, sep = "")
    leaf2 = paste("L13.",n, sep = "")
    plant1 = data[, grep(leaf1, names(data)): grep(leaf2, names(data))] 
    # extract info from L08 col (Stim,genotype)
    # from cols 2,3 which has 08
    info1 = names(plant1)[   grep("08", names(plant1))   ]
    # extract from that all necessary info
    stim1 = strsplit(info1,"_")[[1]][2] # separate by "_". save part with the stim. e.g. "L08.1CG"
    stim1 <- substr(stim1,6,nchar(stim1))
    genotype1 = strsplit(info1,"_")[[1]][3] # separate by "_". save part with the stim. e.g. "L08.1CG"
    # rename (Time, L08(V), L13)(V)) and tidy, and convert to numeric corresponding cols.
    plant1 = cbind(time1,plant1)
    names(plant1)[1] <- "Seconds"
    names(plant1)[   grep("08", names(plant1))   ] <- "L08"
    names(plant1)[   grep("13", names(plant1))   ] <- "L13"
    plant1 = gather(plant1, "leaf", "V", 2:3)
    plant1$Seconds = as.numeric(plant1$Seconds)
    plant1$`V` = as.numeric(plant1$`V`)
    plant1$Genotype = genotype1
    plant1$Stim = stim1
    plant1$Date = date
    plant1$DayTime = ToD
    plant1 = list(plant1)
    output <- append(output,plant1) 
    names(output[n]) <-  paste("plant",n,sep = "")
  }
  
  return(output)
}###  LabScribe into data frame. PlantNumber= 1 or 2 per trace (Should make it 4, for each electrode)
home = getwd()









## Load file names
list.files()
# Loop through all the files and go into the folders
files = c()
for (f in list.files()){
  setwd(home)
  
  # Check if it's a folder with content
  if(length(list.files(f)) != 0){ 
    files = list.files(f)[  grep(".txt",  list.files(f))  ]
  } 
  else if(length(list.files(f)) == 0) next # skip to next file if doesnt have content
  
  ################ Loop through all files in folder
  for (i in files){
    setwd(paste(home,f,sep = "/")) # go into folder with the files to extract
    traceList = DataExtraction( read.delim(i), f )# load .txt file returns list with all plants recorded in file.
    
    # Save traces to files separated by phenotype -in another folder-
    for (p in 1:length(traceList)){ # loop through plants in list of traces
      setwd(dirname(home)) # go to dir parental to the project where databases are saved
      
      # if genotype file of plant "p" exists, open it
      if ( length( grep(traceList[[p]]$Genotype[1], list.files( ), fixed = TRUE)) == 1 ){ # looks for EXACT match of genotype. i.e., Col0 =! Col0.1
        genotypeData = read.csv( list.files(dirname(home))[grep(traceList[[p]]$Genotype[1], list.files(dirname(home)), fixed = TRUE )] ) 
        # create new, consecutive id number for plants recorded the same date.
        if(length(grep(f, genotypeData$Date)) > 0 ){ # if genotype exists and also date
          traceList[[p]]$id = as.numeric(genotypeData %>% dplyr::filter(Date == f) %>% dplyr::summarise(max(id)) + 1 ) 
        }
        else if(length(grep(f, genotypeData$Date)) == 0){# if the genotype exists but not the date
          traceList[[p]]$id = 1
        } 
        genotypeData = rbind(genotypeData, traceList[[p]]) # append plant "p" to genotype database
      }
      
      # else, no genotype file yet
      else if ( length( grep(traceList[[p]]$Genotype[1], list.files(dirname(home)), fixed = TRUE )) == 0 ){  # looks for EXACT match of genotype. i.e., Col0 =! Col0.1
        traceList[[p]]$id = 1
        genotypeData = traceList[[p]] # create new file
      }
      
      # save i    
      write.csv(genotypeData, paste( dirname(home),"/",traceList[[p]]$Genotype[1],".csv",sep = "" ), row.names = FALSE )
      print(paste( traceList[[p]]$Genotype[1],traceList[[p]]$Date[1], traceList[[p]]$DayTime[1], sep = " - ") )
    } # finish saving all traces "p" in file "i"
  } # finish opening all files in folder "f"
  
  setwd(home)
}








# 
# # If the file needs to be modified:
# # < pertinent modifications >
# names(data1)[5] <- "E4_L08.2CG_almt5.1"
# write.csv(data1, "alm9-2_5-1-edited.csv", row.names = FALSE)
# # Save
# data1 = read.csv("alm9-2_5-1-edited.csv")










# find derivative



# Plot
#dplyr::filter(PlotCompare, leaf == "L13" ) %>% 
plant1 %>% ggplot( aes(x = `Seconds`, y = `V`, color = `leaf`) ) +
    geom_line()








# PlotCompare = rbind(plant1, plant1_1Hz)
# dplyr::filter(PlotCompare, leaf == "L13" ) %>% ggplot( aes(x = `Time(s)`, y = `V`, color = `SR`) ) +
#   geom_line()
# ggsave("1Hz.vs.10Hz.pdf")

