# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/Reservoir_Saugeye")

# Load data
dat=read.csv("csv/Data_complete4.csv")

# Load packages
library(ggplot2)
library(extrafont)
library(cowplot)
loadfonts(device = "win")


# Response variable
age0_cpue = dat$age0_cpue

# Fixed effects 
lbass_cpue = dat$lbass_cpue
lcrap_cpue = dat$lcrap_cpue
lsaug_cpue = dat$lsaug_cpue
sto_dens_ha = dat$sto_dens_ha
sto_length_mm = dat$sto_length_mm
sto_doy = dat$sto_doy
secchi = dat$secchi
size_ha = dat$size_ha

# Possible random effects 
location = as.factor(dat$location)
year = as.factor(dat$year)


############################ Box Plot Age0_cpue ####################################
age0_cpue_plot <- ggplot(dat, aes(x=location, y=age0_cpue)) + 
                  geom_boxplot() +
                  ylab("Saugeye Recruitment") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 12,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0,0,0.15,0.75), "cm")) #top, right, bottom left
age0_cpue_plot

############################ Box Plot Large bass CPUE ####################################
lbass_cpue_plot <- ggplot(dat, aes(x=location, y=lbass_cpue)) + 
                  geom_boxplot() +
                  ylab("BASS") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 12,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0.15,0.75), "cm")) #top, right, bottom left
lbass_cpue_plot

############################ Box Plot Large crappie CPUE ####################################
lcrap_cpue_plot <- ggplot(dat, aes(x=location, y=lcrap_cpue)) + 
                  geom_boxplot() +
                  ylab("CRAP") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 12,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0.15,0.75), "cm")) #top, right, bottom left
lcrap_cpue_plot

############################ Box Plot Large adult saugeye CPUE ####################################
lsaug_cpue_plot <- ggplot(dat, aes(x=location, y=lsaug_cpue)) + 
                  geom_boxplot() +
                  ylab("SAUG") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman"),
                        axis.text.x = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman",
                                                   angle = 45, 
                                                   hjust = 1),
                        axis.title.y = element_text(colour = "black",
                                                    size = 12,
                                                    family = "Times New Roman"),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0,0.75), "cm")) #top, right, bottom left
lsaug_cpue_plot

############################ Box Plot Stocking Density ####################################
sto_dens_ha_plot <- ggplot(dat, aes(x=location, y=sto_dens_ha)) + 
                  geom_boxplot() +
                  ylab("S_DEN") +
                  ylim(NA, 900) +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 12,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0.15,0.75), "cm")) #top, right, bottom left
sto_dens_ha_plot

############################ Box Plot Length at stocking ####################################
sto_length_mm_plot <- ggplot(dat, aes(x=location, y=sto_length_mm)) + 
                  geom_boxplot() +
                  ylab("S_LEN") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 12,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0.15,0.75), "cm")) #top, right, bottom left
sto_length_mm_plot

############################ Box Plot Stocking day of year ####################################
sto_doy_plot <- ggplot(dat, aes(x=location, y=sto_doy)) + 
                  geom_boxplot() +
                  ylab("S_DOY") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 12,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0,0,0.15,0.75), "cm")) #top, right, bottom left
sto_doy_plot

############################ Box Plot Secchi ####################################
secchi_plot <- ggplot(dat, aes(x=location, y=secchi)) + 
                  geom_boxplot() +
                  ylab("SD") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 12,
                                                    family = "Times New Roman"),
                        axis.text.x = element_text(colour = "black", 
                                                   size = 12,
                                                   family = "Times New Roman",
                                                   angle = 45, 
                                                   hjust = 1),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0,0.75), "cm")) #top, right, bottom left
                      
secchi_plot

##################### Combine Plots ################################################
png(filename = paste("Figure2.png", sep = ""),
     width = 6.5, height = 7.25, units = "in", res = 600, restoreConsole = TRUE)

box_plots <- plot_grid(age0_cpue_plot, 
                         sto_doy_plot,
                         sto_dens_ha_plot, 
                         sto_length_mm_plot, 
                         lbass_cpue_plot,
                         lcrap_cpue_plot, 
                         lsaug_cpue_plot, 
                         secchi_plot,
                         nrow = 4,
                         align = "v",
                         labels = "AUTO",
                         label_size = 14,
                         label_fontfamily = "Times New Roman",
                         rel_heights = c(1,1,1,1.43))



box_plots

dev.off()


