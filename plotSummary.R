library(tidyr)

list.files()
### Filtering and plotting the summarized data of signal peaks ###


# Open all the "Summary" files in the folder.
SummaryData = NULL
for( i in grep("Summary", list.files())){
  data = read.csv( list.files()[i] )
  data$Genotype = substr(list.files()[i], 1, nchar(list.files()[i])-11 )
  SummaryData = rbind(SummaryData, data )
}
head(SummaryData)

### Filter the data by following criteria
filter1 = which(names(SummaryData) =="Genotype")
filter2 = which(names(SummaryData) =="Leaf")
unique(SummaryData[,filter1])
unique(SummaryData[,filter2])
filter3 = which(names(SummaryData) =="Stimulus")
unique(SummaryData[,filter3])

subGroup = SummaryData %>% dplyr::filter(SummaryData[,filter2] == "L13" & 
                                  SummaryData[,filter1] == "Col0") #& 
                                  #SummaryData[,filter1] == "")
head(subGroup)



My_Theme =  theme(
  axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
  axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0 , vjust = 0.5, face = "plain"),
  axis.title.x = element_text(color = "black", size = 30, angle = 0, hjust = 0.5, vjust = 0, face = "plain"), #element_blank()
  axis.title.y = element_text(color = "black", size = 30, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
  plot.title = element_blank(),#element_text(color = "black", size = 35, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
  legend.text = element_text(size = 20),
  legend.title = element_text(hjust = 0.1, size = 20),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(), #element_line()
  panel.grid.minor.x = element_blank()) 





tracePlot <- ggplot(subGroup, aes(`Genotype`, `depolDuration`, color = `Stimulus`)) +
  geom_violin() +
  geom_jitter(shape=16, position=position_dodge(1)) +
  ylab ("DepolDuration") + 
  #xlab ("Time (s)") + 
  #labs(color = "Leaf") +
  #ggtitle("Uninjected Oocyte. Voltage Clamp. Vh = -60mV") +
  My_Theme
print(tracePlot)

ggsave( "StimuliResponses.png" )

#write.csv(A12, "A12.csv", row.names = FALSE)

# cut over thresholds
tst = read.csv("Col0-ACR1.csv" )
tst$V[which(tst$V < -0.3)]=-0.3
tst$V[which(tst$V > 0.3)]=0.3

# equal times
tst$X[which(tst$leaf == "L13")] = tst$X[which(tst$leaf == "L08")] 

tracePlot <- ggplot(tst, aes(`X`, `V`, color = `leaf`)) +
  geom_line(size = 1) +
  ylab ("V") + 
  xlab ("Time (s)") + 
  #labs(color = "Leaf") +
  #ggtitle("Uninjected Oocyte. Voltage Clamp. Vh = -60mV") +
  My_Theme
print(tracePlot)
