library(tidyverse)


# Load datasets

NGRIP <- read_csv("./analysis/data/raw_data/svensson_NGRIP.csv")

MD95.2043.forest <- read_csv("./analysis/data/raw_data/MD95-2043_forest.csv")
MD95.2043.SST <- read_csv("./analysis/data/raw_data/MD95-2043_SST.csv")

SU81.18.forest <- read_csv("./analysis/data/raw_data/SU81-18_forest.csv")

MD95.2042.forest <- read_csv("./analysis/data/raw_data/MD95-2042_forest.csv")
MD95.2042.SST <- read_csv("./analysis/data/raw_data/MD95-2042_SST_2.csv")



# NGRIP plot

ngrip.plot <- ggplot(NGRIP,aes(Age, d18O))+geom_line(color="Black", size = 0.5)+
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_reverse(lim = c(35,12.5), breaks=seq(0, 35, 5)) +
  annotate(geom = "rect", xmin = 38.2, xmax = 39.9, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 36.58, xmax = 35.48, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 33.74, xmax = 34.74, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 32.5, xmax = 33.36, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 30.84, xmax = 32.04, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 28.9, xmax = 30.6, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 27.78, xmax = 28.6, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 23.34, xmax = 27.54, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 23.020, xmax = 23.22, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 14.692, xmax = 22.9, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 12.5, xmax = 12.896, ymin = -36, ymax = -48,
           alpha = .2) +
  annotate(geom = "rect", xmin = 19, xmax = 26.5, ymin = -35, ymax = -31,
           alpha = .9, fill = "red") +
  annotate(geom = "text", x = c(39, 36, 34.2, 32.9, 31.5, 29.8, 28.2,
         25.5, 23.1, 19, 12.7), y = -38, label = c("9", "8", "7", "6", "5.2", "5.1", "4", "3", "2.2", "2.1", "1")) +
  annotate(geom = "text", x = c(22.8), y = -33.1, label = c("LGM"), fontface = "bold", color = "white") +
  ylim(NA, -30)



# MD95-2043 plots


MD95.2043.SST.plot <- ggplot(MD95.2043.SST,aes(Age, `SST`))+geom_line(color="Blue", size = 1)+
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_reverse(lim = c(35,12.5), breaks=seq(0, 35, 5)) +
#  annotate(geom="text", x=39, y=18, label="MD95-2043",
#           color="Black", size = 3, fontface ="bold") +
  scale_y_continuous(position = "right") +
  ylab('SST (ºC)') +
  annotate(geom = "rect", xmin = 38.9, xmax = 39.9, ymin = 15, ymax = 15.5,
           fill = "black") +
  annotate(geom = "rect", xmin = 29, xmax = 31, ymin = 15, ymax = 15.5,
           fill = "black") +
  annotate(geom = "rect", xmin = 23.2, xmax = 24.5, ymin = 15, ymax = 15.5,
           fill = "black") +
  annotate(geom = "rect", xmin = 16, xmax = 16.6, ymin = 15, ymax = 15.5,
           fill = "black")+
  annotate(geom = "text", x = c(39.4, 30, 23.8, 16.3), y = 16.5, label = c("HE4", "HE3", "HE2", "HE1"))


MD95.2043.forest.plot <- ggplot(MD95.2043.forest,aes(CalAge, `Pollen forest (%)`))+geom_area(stat = "identity", fill = "dark green", color = "black")+
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_reverse(lim = c(40, 12.5), breaks=seq(0, 35, 5)) +
#  annotate(geom="text", x=39, y=40, label="MD95-2043",
#           color="Black", size = 3, fontface ="bold") +
  ylab(expression(atop("Temperate", paste("forest (%)")))) +
  xlab("Age (ka)")


# MD95-2042 + SU81-18 plots

MD95.2042.SST.plot <- ggplot(MD95.2042.SST,aes(Age, `SST`))+geom_line(color="Blue", size = 1)+
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_reverse(lim = c(35,12.5), breaks=seq(0, 35, 5)) +
  #  annotate(geom="text", x=39, y=18, label="MD95-2043",
  #           color="Black", size = 3, fontface ="bold") +
  scale_y_continuous(position = "right", limits = c(NA, 26)) +
  ylab('SST (ºC)') +
  annotate(geom = "rect", xmin = 38.9, xmax = 39.9, ymin = 22, ymax = 23,
           fill = "black") +
  annotate(geom = "rect", xmin = 29.9, xmax = 31, ymin = 22, ymax = 23,
           fill = "black") +
  annotate(geom = "rect", xmin = 24, xmax = 25, ymin = 22, ymax = 23,
           fill = "black") +
  annotate(geom = "rect", xmin = 16, xmax = 17, ymin = 22, ymax = 23,
           fill = "black")+
  annotate(geom = "text", x = c(39.4, 30.4, 24.5, 16.5), y = 25, label = c("HE4", "HE3", "HE2", "HE1"))



# Join tables

atlantic_forest <- rbind(MD95.2042.forest, SU81.18.forest)


atlantic.forest.plot <- ggplot(atlantic_forest,aes(CalAge, `Pollen forest (%)`))+geom_area(stat = "identity", fill = "dark green", color = "black")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_reverse(lim = c(35, 12.5), breaks=seq(0, 35, 5)) +
  #  annotate(geom="text", x=39, y=40, label="MD95-2043",
  #           color="Black", size = 3, fontface ="bold") +
  ylab(expression(atop("Temperate", paste("forest (%)")))) +
  xlab("Age (cal ka)")



# Combined plot


cowplot::plot_grid(ngrip.plot, MD95.2043.SST.plot, MD95.2043.forest.plot,MD95.2042.SST.plot, atlantic.forest.plot, align = "v", ncol = 1, labels = c('A', 'B', 'C', 'D', 'E'))

