library(ggplot2)
library(reshape2)
library(multcompView)
library(plyr)
library(tools)
library(gridExtra)
library(cowplot)

csvpath='/Users/Peter/Programming/R/FW_plot_and_anova/data/2016-10_1-8/fw/'
files <- list.files(path=csvpath, pattern="*.csv", full.names=T, recursive=FALSE)

gg_list <- list()
outfilename <- csvpath
for (filepath in files){
  #filepath='/Users/Peter/Programming/R/FW_plot_and_anova/data/2016-10_1-8/mel_21_7_2wpi.csv'
  filename<-basename(filepath)
  filename_split<-unlist(strsplit(file_path_sans_ext(filename), "_"))
  #tobeplotted <- c(tobeplotted, c=file_path_sans_ext(filename))
  graphcolor="gray50"
  
  if (filename_split[1] == "cuc") {plant="cucumber"; graphcolor="olivedrab4"
  } else if (filename_split[1] == "mel") {plant="melon"; graphcolor="goldenrod"
  } else if (filename_split[1] == "wm") {plant="watermelon"; graphcolor="darkslategray4"
  } else if (filename_split[1] == "tom") {plant="tomato"; graphcolor="red"
  }
  
  outfilename <- paste(outfilename,plant,sep="_")
  fw_title=paste("Average fresh weight of",plant,"plants")
  fw_subtitle=paste(filename_split[2]," C; 10^", filename_split[3], " sp/ml; ", filename_split[4], sep="")
  print(fw_title)
  
  #in the txt file, no '-' characters may be present.
  mydata=read.csv(filepath, header=FALSE)
  mydata$V1 <- gsub('-', '_', mydata$V1)
  data = dcast(melt(mydata, id.vars = "V1"), variable ~ V1)
  data=data[,-1]
  
  stats<-data.frame(
    treatment=colnames(data),
    mean=apply(data, 2, mean, na.rm = TRUE),
    n=apply(data,2, function(x) length(which(!is.na(x)))),
    sd=apply(data,2,sd, na.rm=TRUE)
  )
  stats$sem=stats$sd/sqrt(stats$n)
  stats$cumulativelen=stats$mean+stats$sem
  longest_element_w_sembar=max(stats$cumulativelen)
  
  y_major_break_labels=1
  if (longest_element_w_sembar>10){y_major_break_labels=5; y_minor_break_labels=1
  } else if (longest_element_w_sembar>4){y_major_break_labels=1; y_minor_break_labels=.5
  } else{y_major_break_labels=1; y_minor_break_labels=.5
  }
  
  melteddata <- melt(data, na.rm = FALSE, value.name="freshweight", variable.name="treatment")
  aov_out<-aov(freshweight ~ treatment, data=melteddata)
  thsd <- TukeyHSD(aov_out, ordered = FALSE, conf.level = 0.95)
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- thsd$treatment[,4]
  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels$Letters)
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels$Letters,stringsAsFactors = FALSE)
  plot.levels.by_treatment <- plot.levels[order(plot.levels[,1], plot.levels[,2]), ]
  plot.levels.by_treatment$sem=stats$sem
  plot.levels.by_significance <- plot.levels.by_treatment[order(plot.levels[,2], plot.levels[,1]), ]                         
  
  gg_list[[plant]] <- ggplot(stats, aes(x = treatment, y = mean)) +
    geom_bar(position=position_dodge(), stat="identity", fill=graphcolor, colour="black", size=1, width=.8) + 
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,  size=.8, position=position_dodge(.9), colour="black") +
    ylab("Fresh weight (g)") +
    xlab(NULL)+
    scale_y_continuous(limits=c(0,max(stats$cumulativelen)+.2*max(stats$cumulativelen)), expand=c(0, 0), breaks = seq(0,longest_element_w_sembar+.2*longest_element_w_sembar, y_major_break_labels), minor_breaks = seq(0,longest_element_w_sembar+.2*longest_element_w_sembar, y_minor_break_labels)) +
    ggtitle(bquote(atop(.(fw_title), atop(.(fw_subtitle), "")))) + 
    #add significance labels:
    geom_text(aes(label=plot.levels.by_treatment[,2]), y=stats$cumulativelen+.05*longest_element_w_sembar,  size=rel(6)) +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = rel(1.8), colour = "black")) +
    theme(axis.text.y = element_text(size = rel(1.8), colour = "black")) +
    theme(axis.title.x = element_text(angle=0, size = rel(1.8), colour = "black")) +
    theme(axis.title.y = element_text(size = rel(1.8), colour = "black", vjust=2)) +
    theme(plot.title=element_text(size = rel(2), face = "bold", colour = "black", vjust = -1)) +
    theme(axis.ticks.x=element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="gray60", size=0.5)) +
    theme(panel.grid.minor.y = element_line(colour="gray87", size=0.5)) +
    theme(panel.background = element_rect(fill="gray96")) +
    theme(plot.margin = unit(c(0.5,0.5,0.5,1.5), "cm"))
  ggsave(paste(file_path_sans_ext(filepath),".png",sep=""), width=30, height=30, units="cm",dpi=300)
}

outfilename <- paste(outfilename,"png",sep=".")

notitlestheme <- theme(axis.title.y = element_blank(), axis.text.x=element_blank(), plot.title=element_blank())
gg_list[[1]] <- gg_list[[1]] + notitlestheme
gg_list[[2]] <- gg_list[[2]] + notitlestheme
gg_list[[3]] <- gg_list[[3]] + notitlestheme

ggdraw() +
  draw_plot(gg_list[[1]], 0, 0, .33, 1) +
  draw_plot(gg_list[[2]], .333, 0, .33, 1) +
  draw_plot(gg_list[[3]], .666, 0, .33, 1) +
  draw_plot_label(c("A", "B", "C"), c(0, .333, .666), c(1, 1, 1), size = 15)
ggsave(outfilename, width=60, height=20, units="cm",dpi=300)
