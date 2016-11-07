library(ggplot2)
library(reshape2)
library(multcompView)
library(plyr)
library(tools)

files <- list.files(path="/Users/Peter/Programming/R/FW_plot_and_anova/data/2016-10_1-8/di", pattern="*.csv", full.names=T, recursive=FALSE)
for (filepath in files){
  #filepath='/Users/Peter/Programming/R/FW_plot_and_anova/data/2016-10_1-8/di/cuc_21_7_2wpi.csv'
  filename<-basename(filepath)
  filename_split<-unlist(strsplit(file_path_sans_ext(filename), "_"))
  
  graphcolor="gray50"
  if (filename_split[1] == "cuc") {plant="cucumber"; graphcolor="olivedrab4"
  } else if (filename_split[1] == "mel") {plant="melon"; graphcolor="goldenrod"
  } else if (filename_split[1] == "wm") {plant="watermelon"; graphcolor="darkslategray4"
  } else if (filename_split[1] == "tom") {plant="tomato"; graphcolor="red"
  }
  
  title=paste("Disease indexes of",plant,"plants")
  subtitle=paste(filename_split[2]," C; 10^", filename_split[3], " sp/ml; ", filename_split[4], sep="")

  #in the txt file, no '-' characters may be present.
  mydata=read.csv(filepath, header=FALSE)
  mydata$V1 <- gsub('-', '_', mydata$V1)
  data = dcast(melt(mydata, id.vars = "V1"), variable ~ V1)
  data=data[,-1]
  
  stats<-data.frame(
    treatment=colnames(data),
    DI0=apply(data,2, function(x) length(which(x==0))),
    DI1=apply(data,2, function(x) length(which(x==1))),
    DI2=apply(data,2, function(x) length(which(x==2))),
    DI3=apply(data,2, function(x) length(which(x==3))),
    DI4=apply(data,2, function(x) length(which(x==4)))
  )

  melted_stats <- melt(stats)
  di_colors <- c("DI0" = "#dbefd9","DI1" = "#f3f0ba","DI2" = "#e58735", "DI3" = "#bd0913", "DI4" = "gray10")
  
  ggplot(melted_stats, aes(x=treatment, y=value, fill=variable)) +
    geom_bar(stat = "identity",colour="black", size=.5, width=.8) +
    scale_fill_manual(values=di_colors, name="") +
    ylab("number of plants") +
    xlab(NULL)+
    ggtitle(bquote(atop(.(title), atop(.(subtitle), "")))) +
    scale_y_continuous(limits=c(0,8), expand=c(0, 0), breaks = seq(0,8,1)) +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = rel(1.8), colour = "black")) +
    theme(axis.text.y = element_text(size = rel(1.8), colour = "black")) +
    theme(axis.title.x = element_text(angle=0, size = rel(1.8), colour = "black")) +
    theme(axis.title.y = element_text(size = rel(1.8), colour = "black", vjust=2)) +
    theme(plot.title=element_text(size = rel(2), face = "bold", colour = "black", vjust = -1)) +
    theme(axis.ticks.x=element_blank())+
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="gray60", size=0.5)) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.background = element_rect(fill="gray96")) +
    theme(legend.text = element_text(size=rel(1.5))) +
    theme(legend.position="top") +
    theme(plot.margin = unit(c(0.5,0.5,0.5,1.5), "cm"))
  ggsave(paste(file_path_sans_ext(filepath),".png",sep=""), width=30, height=30, units="cm",dpi=300)
}