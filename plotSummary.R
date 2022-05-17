library(tidyr)

list.files()

# Open all the "Summary" files in the folder.
SummaryData = NULL
for( i in grep("Summary", list.files())){
  data = read.csv( list.files()[i] )
  data$Genotype = substr(list.files()[i], 1, nchar(list.files()[i])-11 )
  SummaryData = rbind(SummaryData, data )
}
head(SummaryData)

filter1 = which(names(SummaryData) =="Genotype")
filter2 = which(names(SummaryData) =="Leaf")
unique(SummaryData[,filter1])
unique(SummaryData[,filter2])



subGroup = SummaryData %>% dplyr::filter(SummaryData[,filter2] == "L13")# & 
                                  #data[,filter2] == "CG") #& 
                                  #data[,filter3] == "")
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





tracePlot <- ggplot(subGroup, aes(`Genotype`, `depolDuration`, color = `Leaf`)) +
  geom_violin() +
  geom_jitter(shape=16, position=position_dodge(1)) +
  ylab ("mV") + 
  #xlab ("Time (s)") + 
  #labs(color = "Leaf") +
  #ggtitle("Uninjected Oocyte. Voltage Clamp. Vh = -60mV") +
  My_Theme
print(tracePlot)

ggsave( "GenotypesL13_depolDuration.png" )

write.csv(A12, "A12.csv", row.names = FALSE)

