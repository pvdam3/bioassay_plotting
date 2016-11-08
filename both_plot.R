library(ggplot2)
library(reshape2)
library(multcompView)
library(plyr)
library(tools)
library(gridExtra)
library(cowplot)
library(gdata)
library(dplyr)

# should contain fw/ di/ and (not obliged) labels.csv
maindirectory<-"/Users/Peter/Programming/R/FW_plot_and_anova/data/2016-10-HCT/"
fwcsvpath=paste(maindirectory, "/fw/", sep="")
fwfiles <- list.files(path=fwcsvpath, pattern="*.csv", full.names=T, recursive=FALSE)
dicsvpath=paste(maindirectory, "/di/", sep="")
difiles <- list.files(path=dicsvpath, pattern="*.csv", full.names=T, recursive=FALSE)

plots <- list()
plot.levels.multiplot <- list()
outfilename <- maindirectory

for (filepath in fwfiles){
  #filepath='/Users/Peter/Programming/R/FW_plot_and_anova/data/2016-10_1-8/mel_21_7_2wpi.csv'
  filename<-basename(filepath)
  filename_split<-unlist(strsplit(file_path_sans_ext(filename), "_"))
  graphcolor="gray50"
  
  if (filename_split[1] == "cuc") {plant="cucumber"; graphcolor="olivedrab4"
  } else if (filename_split[1] == "mel") {plant="melon"; graphcolor="goldenrod"
  } else if (filename_split[1] == "wm") {plant="watermelon"; graphcolor="darkslategray4"
  } else if (filename_split[1] == "tom") {plant="tomato"; graphcolor="red"
  }
  
  outfilename <- paste(outfilename,plant,sep="_fw")
  fw_title=paste("Average fresh weight of",plant,"plants")
  fw_subtitle=paste(filename_split[2]," C; 10^", filename_split[3], " sp/ml; ", filename_split[4], sep="")
  print(fw_title)
  
  #in the txt file, no '-' characters may be present.
  mydata=read.csv(filepath, header=FALSE)
  mydata$V1 <- gsub('-', '_', mydata$V1)
  #mydata$order <- seq(from=1, to=length(mydata$V1))
  melteddata <- melt(mydata, na.rm = FALSE, value.name="freshweight", variable.name="replicate", id.vars = "V1")
  #make sure the original order is retained..
  melteddata$V1 <- factor(melteddata$V1, as.character(unique(mydata$V1))) 
  data=dcast(melteddata, replicate ~ V1)
  data=data[,-1]
  
  stats<-data.frame(
    treatment=colnames(data),
    mean=apply(data, 2, mean, na.rm = TRUE),
    n=apply(data,2, function(x) length(which(!is.na(x)))),
    sd=apply(data,2,sd, na.rm=TRUE)
  )
  stats$sem=stats$sd/sqrt(stats$n)
  stats$cumulativelen=stats$mean+stats$sem
  #retain plotting order of input:
  stats$treatment <- factor(stats$treatment, levels=stats$treatment)
  #data$treatment <- factor(data$treatment)
  longest_element_w_sembar=max(stats$cumulativelen)
  
  y_major_break_labels=2; y_minor_break_labels=.5
  #if (longest_element_w_sembar>10){y_major_break_labels=5; y_minor_break_labels=1
  #} else if (longest_element_w_sembar>4){y_major_break_labels=1; y_minor_break_labels=.5
  #} else{y_major_break_labels=1; y_minor_break_labels=.5
  #}
  
  #meltedfwdata <- melt(data, na.rm = FALSE, value.name="freshweight", variable.name="treatment", id.vars=colnames(data))
  #meltedfwdata$treatment <- factor(meltedfwdata$treatment, as.character(unique(mydata$V1)))
  melteddata$V1 <- factor(melteddata$V1)
  aov_out<-aov(data=melteddata, freshweight ~ V1)
  thsd <- TukeyHSD(aov_out, ordered = FALSE, conf.level = 0.95)
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- thsd$V1[,4]
  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels$Letters)
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels$Letters,stringsAsFactors = FALSE)
  #retain order of labels:
  plot.levels$plot.labels <- reorder.factor(plot.levels$plot.labels, new.order=stats$treatment)
  plot.levels <- plot.levels %>% arrange(plot.labels)
  
  plot.levels$sem <- stats$sem                
  plot.levels.multiplot[[paste(plant,"fw",sep = "")]] <- plot.levels
  plot.levels.multiplot <- plot.levels.multiplot
  currentlistofypositions = (stats$cumulativelen+.05*longest_element_w_sembar)
  plot.levels.multiplot[[paste(plant,"fw",sep ="")]]$ypos= currentlistofypositions
  
  plots[[paste(plant,"fw",sep = "")]] <- ggplot(stats, aes(x = treatment, y = mean)) +
    geom_bar(position=position_dodge(), stat="identity", fill=graphcolor, colour="black", size=1, width=.8) + 
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,  size=.8, position=position_dodge(.9), colour="black") +
    ylab("Fresh weight (g)") +
    xlab(NULL)+
    scale_y_continuous(limits=c(0,max(stats$cumulativelen)+.1*max(stats$cumulativelen)), expand=c(0, 0), breaks = seq(0,longest_element_w_sembar+.2*longest_element_w_sembar, y_major_break_labels), minor_breaks = seq(0,longest_element_w_sembar+.2*longest_element_w_sembar, y_minor_break_labels)) +
    ggtitle(bquote(atop(.(fw_title), atop(.(fw_subtitle), "")))) + 
    #add significance labels:
    #geom_text(aes(label=plot.levels[,2]), y=stats$cumulativelen+.05*longest_element_w_sembar,  size=rel(8)) +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = rel(1.8), colour = "black")) +
    theme(axis.text.y = element_text(size = rel(1.8), colour = "black")) +
    theme(axis.title.x = element_text(angle=0, size = rel(1.8), colour = "black")) +
    theme(axis.title.y = element_text(size = rel(1.8), colour = "black", vjust=2)) +
    theme(plot.title=element_text(size = rel(2), face = "bold", colour = "black", vjust = -1)) +
    theme(axis.ticks.x=element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.major.y = element_line(color="gray70", size=0.5)) +
    theme(panel.grid.minor.y = element_line(colour="gray87", size=0.5)) +
    theme(panel.background = element_rect(fill="gray96")) +
    theme(plot.margin = unit(c(0.5,0.5,0.5,1.5), "cm")) +
    background_grid(major = "y", minor = "y")
  ggsave(paste(file_path_sans_ext(filepath),".png",sep=""), width=30, height=30, units="cm",dpi=300)
}

for (filepath in difiles){
  #filepath='/Users/Peter/Programming/R/FW_plot_and_anova/data/2016-10_1-8/di/cuc_21_7_2wpi.csv'
  filename<-basename(filepath)
  filename_split<-unlist(strsplit(file_path_sans_ext(filename), "_"))
  
  graphcolor="gray50"
  if (filename_split[1] == "cuc") {plant="cucumber"; graphcolor="olivedrab4"
  } else if (filename_split[1] == "mel") {plant="melon"; graphcolor="goldenrod"
  } else if (filename_split[1] == "wm") {plant="watermelon"; graphcolor="darkslategray4"
  } else if (filename_split[1] == "tom") {plant="tomato"; graphcolor="red"
  }
  
  outfilename <- paste(outfilename,plant,sep="_di")
  title=paste("Disease indexes of",plant,"plants")
  subtitle=paste(filename_split[2]," C; 10^", filename_split[3], " sp/ml; ", filename_split[4], sep="")
  print(title)
  
  #in the txt file, no '-' characters may be present.
  mydidata=read.csv(filepath, header=FALSE)
  mydidata$V1 <- gsub('-', '_', mydidata$V1)
  melteddidata <- melt(mydidata, id.vars = "V1")
  #make sure the original order is retained..
  melteddidata$V1 <- factor(melteddidata$V1, as.character(unique(mydidata$V1))) 
  didata = dcast(melteddidata, variable ~ V1)
  didata=didata[,-1]

  
  
  distats<-data.frame(
    treatment=colnames(didata),
    DI0=apply(didata,2, function(x) length(which(x==0))),
    DI1=apply(didata,2, function(x) length(which(x==1))),
    DI2=apply(didata,2, function(x) length(which(x==2))),
    DI3=apply(didata,2, function(x) length(which(x==3))),
    DI4=apply(didata,2, function(x) length(which(x==4)))
  )
  
  #melteddidata$V1 <- factor(melteddidata$V1, as.character(unique(mydidata$V1))) 
  melted_distats <- melt(distats)
  melted_distats$treatment <- factor(melted_distats$treatment, as.character(unique(mydidata$V1)))
  di_colors <- c("DI0" = "#dbefd9","DI1" = "#f3f0ba","DI2" = "#e58735", "DI3" = "#bd0913", "DI4" = "gray10")
  
  plots[[paste(plant,"di",sep = "")]] <- ggplot(melted_distats, aes(x=treatment, y=value, fill=variable)) +
    geom_bar(stat = "identity",colour="black", size=1, width=.8) +
    scale_fill_manual(values=di_colors, name="", labels=c("DI0   ","DI1   ","DI2   ","DI3   ","DI4   ")) +
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
    theme(panel.grid.major.y = element_line(colour="gray70", size=0.5)) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.background = element_rect(fill="gray96")) +
    theme(legend.text = element_text(size=rel(1.5))) +
    theme(legend.key.size = unit(1, "cm")) +
    theme(legend.position="top") +
    theme(plot.margin = unit(c(0.5,0.5,0.5,1.5), "cm"))
    background_grid(major = "y", minor = "none")
  ggsave(paste(file_path_sans_ext(filepath),".png",sep=""), width=30, height=30, units="cm",dpi=300)
}

outfilename <- paste(outfilename,"png",sep=".")

notitlestheme <- theme(axis.title.y = element_blank(), axis.text.x=element_blank())+
  theme(plot.title=element_blank(), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  theme(legend.text=element_blank(), legend.position="none")
 


plots[[1]] <- plots[[1]] + notitlestheme + background_grid(major = "xy", minor = "none")+geom_text(aes(label=plot.levels.multiplot[[1]][[2]]), y=plot.levels.multiplot[[1]][[4]],size=rel(8))
plots[[2]] <- plots[[2]] + notitlestheme + background_grid(major = "xy", minor = "none")+geom_text(aes(label=plot.levels.multiplot[[2]][[2]]), y=plot.levels.multiplot[[2]][[4]],size=rel(8))
plots[[3]] <- plots[[3]] + notitlestheme + background_grid(major = "xy", minor = "none")+geom_text(aes(label=plot.levels.multiplot[[3]][[2]]), y=plot.levels.multiplot[[3]][[4]],size=rel(8))
plots[[4]] <- plots[[4]] + notitlestheme + background_grid(major = "xy", minor = "none")
plots[[5]] <- plots[[5]] + notitlestheme + background_grid(major = "xy", minor = "none")
plots[[6]] <- plots[[6]] + notitlestheme + background_grid(major = "xy", minor = "none")

labels <- ggplot(melted_distats, aes(x=treatment, fill=variable)) +
  xlab(NULL)+
  theme(axis.text.x = element_text(angle=45, hjust=1,vjust=.8, size = rel(1.8), colour = "black"))+
  theme(axis.line.x=element_blank(), axis.ticks.x=element_blank())

if (file.exists(paste(maindirectory,"labels.csv",sep=""))){
  customlabels <- read.csv(paste(maindirectory,"labels.csv",sep=""), header=FALSE)
  #labels <- labels + scale_x_discrete(labels=c("A", "B", "C", "D","E","A", "B", "C", "D","E"))
  labels <- labels + scale_x_discrete(labels=as.character(t(customlabels[1,])))
}

dilegend <- get_legend(plots[[4]] + theme(legend.position="bottom", legend.text=element_text(size=rel(1.8))))

png(filename=outfilename, width=60, height=45, units="cm",res=300)
sixplots <-plot_grid(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],labels, labels, labels, align="v", nrow=3,ncol=3, rel_heights = c(1, 1, .15))
plot_grid(sixplots, dilegend, nrow=2,ncol=1, rel_heights = c(1, .2), scale = 0.95)
dev.off()
