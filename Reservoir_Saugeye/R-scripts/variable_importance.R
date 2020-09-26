# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/Reservoir_Saugeye")

# Load data
dat=read.csv("csv/variable_importance.csv")

data1<-dat[ which(dat$analysis=="growth"), ]
data2<-dat[ which(dat$analysis =="survival"), ]

#Load packages
library(ggplot2)
library(cowplot)
library(ggpubr)
library(grid)
library(gridExtra)
library(extrafont)
loadfonts(device = "win")


# Importance plot for growth
growth_plot <- ggplot(data1, aes(x=var, y=imp )) + 
  geom_bar(stat="identity", width = 0.5, fill = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05)) +
  scale_x_discrete(labels = c("SD", "SA", "SAUG", "CRAP", "BASS", "S_LEN", "S_DEN", "S_DOY")) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA),
      axis.title.y=element_blank(), 
      axis.title.x=element_blank(),
      axis.text=element_text(size=12, colour = "black", family = "Times New Roman"),
      legend.position="none",
      plot.margin = unit(c(0.15,0.2,0.15,0.8), "cm")) #top, right, bottom left)
   
  
recruitment_plot <- ggplot(data2, aes(x=var, y=imp )) + 
  geom_bar(stat="identity", width = 0.5, fill = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05)) +
  scale_x_discrete(labels = c("SD", "SA", "SAUG", "CRAP", "BASS", "S_LEN", "S_DEN", "S_DOY")) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        axis.text=element_text(size=12, colour = "black", family = "Times New Roman"),
        legend.position="none",
        plot.margin = unit(c(0.15,0.2,0.15,0.8), "cm")) #top, right, bottom left
                        
                        

# Combine plots
png(filename = paste("Figure3.png", sep = ""), 
    width = 6.5, 
    height = 3, 
    units = "in", 
    res = 600, 
    restoreConsole = TRUE)

importance_plots <- plot_grid(growth_plot, 
                            recruitment_plot,
                            nrow=1,
                            labels = "AUTO",
                            label_size = 14,
                            label_fontfamily = "Times New Roman")

x.grob <- textGrob("Relative Variable Importance", 
                   gp=gpar(fontsize=14, fontfamily = "Times New Roman"))

importance_plots <- grid.arrange(arrangeGrob(importance_plots, bottom = x.grob))

importance_plots
                                 
dev.off()