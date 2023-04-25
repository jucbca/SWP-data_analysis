
#### INSTRUCTIONS


### INPUT
# Put into the home folder(where this script is running) all the folders that contain the data files to be analyzed. Folder must be named as the date of the experiment YYMMDD.
# The script will take all the .txt files and ignore all the others
# Data about the experiment is extracted from the leaf 8 column title. So columns of the original .txt file NEED to be named in the following structure for the code to manipulate it:
# "Ez_Ly.xPG_w" where: 
#                   z is the electrode number (1 to 4)
#                   y is the leaf on which that electrode is (08 or 13)
#                   x is the plant (1 or 2)
#                   PG stands for Plier and Gears as the stimulation device. GL as green light
#                   w is the GENOTYPE (text of any length, just avoid "_") it has to be consistent for the system to compile them all together.
# example "E1_L08.1PG_Col0" is the electrode one on leaf 8 of plant 1, stimulated with PG, Col0
# if there is only one plant in the experiment, name the unused columns as "Ez_Leer"


### OUTPUT
# The script will create a ">GENOTYPE<.csv" file for every genotype inputed in the .txt files, or use a created one if existing. All the traces will be sorted by genotype in those files. If there is one genotype written in different ways, two separate files will be created. 
  # e.j. "Col0.csv" and "col0.csv" will be created if the uppercase is not consistent in the raw data. 
# That genotype-separated data will be automatically saved in the parental folder of the one containing this project.
# All the data is pooled in an "ALLtraces.csv" file that can be saved as backup for that batch of experiments.



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
    plant1 = data[, grep(leaf1, names(data)): grep(leaf2, names(data))] # pick the columns w/ the data
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
    plant1$plant = n
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
setwd(home)
list.files()
# Loop through all the date-named folder containing the raw data. Saves and prints all the traces found.
files = c()
for (f in list.files()){
  setwd(home)
  
  
  
  ################ Check if it's a folder with content
  if(length(list.files(f)) != 0){ 
    files = list.files(f)[  grep(".txt",  list.files(f))  ]
  } 
  else if(length(list.files(f)) == 0) next # skip to next file if doesn't have content
  
  
  
  ################ Loop through all files in folder
  for (i in files){
    setwd(paste(home,f,sep = "/")) # go into folder with the files to extract
    traceList = DataExtraction( read.delim(i), f )# load .txt file returns list with all plants recorded in file.
    
    # Save traces to files. Sort by genotype -in another folder-
    for (p in 1:length(traceList)){ # loop through plants in list of traces
      setwd(dirname(home)) # go to dir parental to the project where databases are saved
      
      # if genotype file of plant "p" exists, open it
      findFile = paste(traceList[[p]]$Genotype[1], ".csv", sep = "")
      if ( length( grep( findFile , list.files( ), fixed = TRUE)) == 1 ){ # looks for EXACT match of genotype.csv i.e., Col0 =! Col0.1
        genotypeData = read.csv( list.files(dirname(home))[ grep(findFile, list.files(dirname(home)), fixed = TRUE )] )  
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
      if ( length( grep( paste(traceList[[p]]$Genotype[1], ".csv", sep = ""), list.files( ), fixed = TRUE)) == 0 ){  # looks for EXACT match of genotype. i.e., Col0 =! Col0.1
        traceList[[p]]$id = 1
        genotypeData = traceList[[p]] # create new file
      }
      
      # save i    
      write.csv(genotypeData, paste( dirname(home),"/",traceList[[p]]$Genotype[1],".csv",sep = "" ), row.names = FALSE )
      print(paste( f, i, traceList[[p]]$Genotype[1], traceList[[p]]$DayTime[1], sep = " - ") )
    } # finish saving all traces "p" in file "i"
  } # finish opening all files in folder "f"
  
  setwd(home)
}


### You are done here! go to the "SWPanalyzer.R" script to analyse each genotype trace.








# plot one trace at a time if wanna play around
list.files()

trace = DataExtraction( read.delim( list.files()[  grep(".txt",  list.files()) ]   ), "220907")[[1]] # it is for two leaves simultaneous. 


