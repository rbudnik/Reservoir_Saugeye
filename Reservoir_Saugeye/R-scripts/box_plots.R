# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/SAE_Retrospective/NAJFM manuscript 2")

# Load data
dat=read.csv("csv/Data_complete8.csv")

# Load packages
library(ggplot2)
library(extrafont)
library(cowplot)
loadfonts(device = "win")


# Response variable
age0_cpue = dat$age0_cpue
growth = dat$growth

# Fixed effects 
lbass_cpue = dat$lbass_cpue
lcrap_cpue = dat$lcrap_cpue
lsaug_cpue = dat$lsaug_cpue
sto_dens_ha = dat$sto_dens_ha
sto_doy = dat$sto_doy
secchi = dat$secchi
size_ha = dat$size_ha
AvgSpWTemp = dat$AvgSpWTemp
AvgSuWTemp = dat$AvgSuWTemp


############################ Box Plot Age0_cpue ####################################
growth_plot <- ggplot(dat, aes(x=location, y=growth)) + 
                  geom_boxplot() +
                  ylab("Saugeye Growth") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0,0,0.15,0.75), "cm")) #top, right, bottom left
growth_plot


############################ Box Plot Age0_cpue ####################################
age0_cpue_plot <- ggplot(dat, aes(x=location, y=age0_cpue)) + 
                  geom_boxplot() +
                  ylab("Saugeye Recruitment") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
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
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
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
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
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
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0.15,0.75), "cm")) #top, right, bottom left
lsaug_cpue_plot

############################ Box Plot Stocking Density ####################################
sto_dens_ha_plot <- ggplot(dat, aes(x=location, y=sto_dens_ha)) + 
                  geom_boxplot() +
                  ylab("S_DEN") +
                  ylim(NA, 900) +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0.15,0.75), "cm")) #top, right, bottom left
sto_dens_ha_plot

############################ Box Plot Stocking day of year ####################################
sto_doy_plot <- ggplot(dat, aes(x=location, y=sto_doy)) + 
                  geom_boxplot() +
                  ylab("S_DOY") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
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
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
                                                    family = "Times New Roman"),
                        axis.text.x = element_blank(),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0.15,0.75), "cm")) #top, right, bottom left
secchi_plot

############################ Box Plot AvgSpTemp ####################################
AvgSpWTemp_plot <- ggplot(dat, aes(x=location, y=AvgSpWTemp)) + 
                  geom_boxplot() +
                  ylab("SPTEMP") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
                                                    family = "Times New Roman"),
                        axis.text.x = element_text(colour = "black", 
                                                   size = 10,
                                                   family = "Times New Roman",
                                                   angle = 55, 
                                                   hjust = 1),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0,0.75), "cm")) #top, right, bottom left
                      
AvgSpWTemp_plot

############################ Box Plot AvgSuTemp ####################################
AvgSuWTemp_plot <- ggplot(dat, aes(x=location, y=AvgSuWTemp)) + 
                  geom_boxplot() +
                  ylab("SUTEMP") +
                  theme(axis.text.y = element_text(colour = "black", 
                                                   size = 10,
                                                   family = "Times New Roman"),
                        axis.title.y = element_text(colour = "black",
                                                    size = 10,
                                                    family = "Times New Roman"),
                        axis.text.x = element_text(colour = "black", 
                                                   size = 10,
                                                   family = "Times New Roman",
                                                   angle = 55, 
                                                   hjust = 1),
                        axis.title.x=element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", 
                                                    fill = NA, 
                                                    size = 1.2),
                        plot.margin = unit(c(0.15,0,0,0.75), "cm")) #top, right, bottom left
                      
AvgSuWTemp_plot

##################### Combine Plots ################################################
box_plots <- plot_grid(growth_plot, 
                       age0_cpue_plot, 
                       sto_doy_plot, 
                       sto_dens_ha_plot, 
                       lbass_cpue_plot, 
                       lcrap_cpue_plot,
                       lsaug_cpue_plot,
                       secchi_plot,
                       AvgSpWTemp_plot,
                       AvgSuWTemp_plot,
                       nrow = 5, 
                       align = "v", 
                       labels = "AUTO", 
                       label_size = 12, 
                       label_fontfamily = "Times New Roman", 
                       rel_heights = c(1,1,1,1,1.43))

# Save as tiff
ggsave(
  filename = "Figures/Figure 2_ver2.tiff",
  plot = box_plots,
  height = 7.25,
  width = 6.53,
  units = "in",
  dpi = 600,
  compression = "lzw+p")