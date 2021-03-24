# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/Reservoir_Saugeye")

# Load necessary packages
library(performance)
library(pracma)
library(car)
library(arm)
library(ggplot2)
library(extrafont)
library(cowplot)
library(ggstance)
library(grid)
loadfonts(device = "win")

#######################################################################
# Estimate plot for age-0 recruitment
#######################################################################

# Load data
dat=read.csv("csv/estimate_plot.csv")

data2<-dat[ which(dat$analysis == "recruitment"), ]

# Create estimate plot
rec_estimate_plot <- ggplot(data2, aes(mean, variable)) + 
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin=low, xmax=high), 
                  show.legend = FALSE) + 
  geom_point(aes(color = symbol)) + 
  scale_color_manual(values=c("black","grey")) +
  scale_y_discrete(labels = c("SPTEMP", "SD", "SAUG", "CRAP", "BASS", "S_DEN", "S_DOY")) +
  xlim(-1.5, 1.5) +
  theme(legend.position = "none", 
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        axis.text=element_text(size=12, colour = "black", family = "Times New Roman"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0.15,0.2,0.15,0.8), "cm"))

#######################################################################
# Estimate plot for age-0 growth
#######################################################################

# Load data
dat=read.csv("csv/estimate_plot2.csv")

data1<-dat[ which(dat$analysis =="growth"), ]

# Create estimate plot
gro_estimate_plot <- ggplot(data1, aes(mean, variable)) + 
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin=low, xmax=high),
                  show.legend = FALSE) + 
  geom_pointrangeh(aes(xmin=low, xmax=high), 
                  show.legend = TRUE) + 
  geom_point(aes(color = symbol)) + 
  scale_color_manual(values=c("black","grey")) +
  scale_y_discrete(labels = c("SUTEMP", "SD", "SAUG", "S_DEN", "S_DOY")) +
  xlim(-.15, .15) +
  theme(legend.title = element_blank(), 
        legend.key = element_blank(),
        legend.text = element_text(size = 10.2, family = "Times New Roman"),
        legend.text.align = 0,
        legend.key.width = unit(0.8,"line"),
        legend.key.height = unit(0.6,"line"),
        legend.direction = "vertical",
        legend.spacing.x = unit(0, 'cm'),
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(0.01, 0.01, 0.01, 0.01, "cm"),
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        axis.text=element_text(size=12, colour = "black", family = "Times New Roman"),
        legend.position = c(0.545, 0.93),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0.15,0.2,0.15,0.8), "cm"))
                

# Combine plots
estimate_plots <- plot_grid(gro_estimate_plot, 
                            rec_estimate_plot,
                            nrow=1,
                            labels = "AUTO",
                            label_size = 12,
                            label_fontfamily = "Times New Roman")

x.grob <- textGrob("Standardized Coefficient +/- 95% CI", 
                   gp=gpar(fontsize=12, fontfamily = "Times New Roman"))

estimate_plots <- grid.arrange(arrangeGrob(estimate_plots, bottom = x.grob))

# Save as tiff
ggsave(
  filename = "Figure 3.tiff",
  plot = estimate_plots,
  height = 3,
  width = 6.5,
  units = "in",
  dpi = 600,
  compression = "lzw+p")
