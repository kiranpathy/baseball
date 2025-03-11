#Aaron Judge R Script

setwd("~/Desktop/Baseball Coding/Aaron Judge Playoffs 24")

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(ggpubr)
library(paletteer)

judge_post24 <- read_csv("judge_post24.csv")

heat_colors_interpolated <- colorRampPalette(paletteer_d("RColorBrewer::RdBu", 
                                                         n = 9,
                                                         direction = -1))(16)

left <- -8.5 / 12
right <- 8.5 / 12
bottom <- 18.29 / 12
top <- 44.08 / 12
width <- (right - left) / 3
height <- (top - bottom) / 3

#I am multiplying the plate_x by -1 to make it from pitcher's perspective
judge_post24 <- judge_post24 %>%
  mutate(plate_x = plate_x * -1)

#fastball, total
judge_ff <- judge_post24

freq_ff <- judge_ff %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_ff$n)

freq_ff$usage_percent <- round((freq_ff$n / total)*100, digits = 0)
freq_ff <- subset(freq_ff, select = -c(n))

judge_ff <- merge(judge_ff, freq_ff)

judge_ff <- judge_ff %>%
  filter(pitch_type == "FF")

ff_total <- judge_ff %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Fastball"), paste(judge_ff$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#slider, total
judge_sl <- judge_post24

freq_sl <- judge_sl %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_sl$n)

freq_sl$usage_percent <- round((freq_sl$n / total)*100, digits = 0)
freq_sl <- subset(freq_sl, select = -c(n))

judge_sl <- merge(judge_sl, freq_sl)

judge_sl <- judge_sl %>%
  filter(pitch_type == "SL")

sl_total <- judge_sl %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Slider"), paste(judge_sl$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#sinker, total
judge_si <- judge_post24

freq_si <- judge_si %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_si$n)

freq_si$usage_percent <- round((freq_si$n / total)*100, digits = 0)
freq_si <- subset(freq_si, select = -c(n))

judge_si <- merge(judge_si, freq_si)

judge_si <- judge_si %>%
  filter(pitch_type == "SI")

si_total <- judge_si %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Sinker"), paste(judge_si$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#curveball (incl knucklecurve), total
judge_cu <- judge_post24

freq_cu <- judge_cu %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_cu$n)

freq_cu$usage_percent <- round((freq_cu$n / total)*100, digits = 0)
freq_cu <- subset(freq_cu, select = -c(n))

judge_cu <- merge(judge_cu, freq_cu)

judge_cu <- judge_cu %>%
  filter(pitch_type == "CU" | pitch_type == "KC")

cu_total <- judge_cu %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Curveball"), paste(judge_cu$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#cutter, total
judge_fc <- judge_post24

freq_fc <- judge_fc %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_fc$n)

freq_fc$usage_percent <- round((freq_fc$n / total)*100, digits = 0)
freq_fc <- subset(freq_fc, select = -c(n))

judge_fc <- merge(judge_fc, freq_fc)

judge_fc <- judge_fc %>%
  filter(pitch_type == "FC")

fc_total <- judge_fc %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Cutter"), paste(judge_fc$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#changeup, total
judge_ch <- judge_post24

freq_ch <- judge_ch %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_ch$n)

freq_ch$usage_percent <- round((freq_ch$n / total)*100, digits = 0)
freq_ch <- subset(freq_ch, select = -c(n))

judge_ch <- merge(judge_ch, freq_ch)

judge_ch <- judge_ch %>%
  filter(pitch_type == "CH")

ch_total <- judge_ch %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Changeup"), paste(judge_ch$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#sweeper, total
judge_st <- judge_post24

freq_st <- judge_st %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_st$n)

freq_st$usage_percent <- round((freq_st$n / total)*100, digits = 0)
freq_st <- subset(freq_st, select = -c(n))

judge_st <- merge(judge_st, freq_st)

judge_st <- judge_st %>%
  filter(pitch_type == "ST")

st_total <- judge_st %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Sweeper"), paste(judge_st$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#splitter, total
judge_fs <- judge_post24

freq_fs <- judge_fs %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_fs$n)

freq_fs$usage_percent <- round((freq_fs$n / total)*100, digits = 0)
freq_fs <- subset(freq_fs, select = -c(n))

judge_fs <- merge(judge_fs, freq_fs)

judge_fs <- judge_fs %>%
  filter(pitch_type == "FS")

fs_total <- judge_fs %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Splitter"), paste(judge_fs$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#slurve, total
judge_sv <- judge_post24

freq_sv <- judge_sv %>%
  group_by(pitch_type) %>%
  tally()

total <- sum(freq_sv$n)

freq_sv$usage_percent <- round((freq_sv$n / total)*100, digits = 0)
freq_sv <- subset(freq_sv, select = -c(n))

judge_sv <- merge(judge_sv, freq_sv)

judge_sv <- judge_sv %>%
  filter(pitch_type == "SV")

sv_total <- judge_sv %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density2d_filled() +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-3,3) + ylim(0, 5) + 
  ggtitle(paste("Slurve"), paste(judge_sv$usage_percent,"%")) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))

#arrange totals
total_plot <- ggarrange(ff_total,
          sl_total,
          si_total,
          cu_total,
          fc_total,
          ch_total,
          st_total,
          fs_total,
          sv_total, nrow = 3, ncol = 3) %>%
  annotate_figure(plot, top = text_grob("Aaron Judge Playoffs 2024 (All Pitches)", 
                                        color = "red", face = "italic", size = 14))
total_plot

#strikezone plot for pitches he got hits on
judge_posthits <- judge_post24 %>%
  filter(events == "double" | events == "home_run" | events == "single")

hits_plot <- judge_posthits %>%
  ggplot(mapping = aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = pitch_type), size = 3) +
  scale_color_manual(values = c(FC = "red",
                                FF = "blue",
                                SI = "yellow",
                                SL = "green",
                                ST = "orange")) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge Playoffs 2024 (Hits)")

hits_plot

#start pitcher data - need to use separate dataset to get pitcher names (razzball)
razzball <- read_csv("razzball.csv")

razzball <- razzball %>%
  select(Name, MLBAMID)

pitcher_stats <- inner_join(judge_post24, razzball, by=c("pitcher...8" = "MLBAMID"))

pitcher_overall <- pitcher_stats %>%
  group_by(Name) %>%
  mutate(total_pitches = n(),
         whiffs = sum(description == "swinging_strike"),
         whiff_percentage = (whiffs / total_pitches) * 100,
         mean_ev = mean(launch_speed, na.rm = TRUE),
         strikeouts = sum(events == "strikeout", na.rm = TRUE),
         hits = sum(events == "double" | events == "home_run" | events == "single", na.rm = TRUE),
         batting_avg = hits / sum(events == "field_out" | events == "force_out" | 
                                    events == "grounded_into_double_play" | 
                                    events == "strikeout", na.rm = TRUE
                                   )) %>%
  select(Name, total_pitches, whiffs, whiff_percentage, mean_ev, strikeouts, hits, batting_avg) %>%
  distinct(Name, .keep_all = TRUE)

pitcher_bypitch <- pitcher_stats %>%
  group_by(Name, pitch_type) %>%
  mutate(total_pitches = n(),
         whiffs = sum(description == "swinging_strike"),
         whiff_percentage = (whiffs / total_pitches) * 100,
         mean_ev = mean(launch_speed, na.rm = TRUE),
         strikeouts = sum(events == "strikeout", na.rm = TRUE),
         hits = sum(events == "double" | events == "home_run" | events == "single", na.rm = TRUE),
         batting_avg = hits / sum(events == "field_out" | events == "force_out" | 
                                    events == "grounded_into_double_play" | 
                                    events == "strikeout", na.rm = TRUE)) %>%
  select(Name, pitch_type, total_pitches, whiffs, whiff_percentage, mean_ev, strikeouts, hits, batting_avg) %>%
  distinct(pitch_type, .keep_all = TRUE)

moto <- pitcher_stats %>%
  filter(Name == "Yoshinobu Yamamoto") %>%
  mutate(launch_speed = ifelse(is.na(launch_speed), 0, launch_speed),
         whiff = ifelse(description == "swinging_strike", "Yes", "No"))

moto_plot_ev <- moto %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
               geom_point(aes(color = launch_speed, shape = pitch_type), size = 3) +
               scale_color_continuous() +
               geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
               geom_segment(x = left, y = top, xend = right, yend = top) +
               geom_segment(x = left, y = bottom, xend = left, yend = top) +
               geom_segment(x = right, y = bottom, xend = right, yend = top) +
               geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
               geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
               geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
               geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
               geom_segment(x = left, y = 0, xend = right, yend = 0) +
               geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
               geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
               geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
               geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
               xlim(-2.5, 2.5) + ylim(-.5, 5) +
               theme(panel.background = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.title = element_text(hjust = 0.5)) +
               labs(title = "Aaron Judge vs. Yoshinobu Yamamoto (EV), Playoffs 2024",
                    x = "",
                    y = "")

moto_plot_whiff <- moto %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = whiff, shape = pitch_type), size = 3) +
  scale_color_discrete() +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge vs. Yoshinobu Yamamoto (Whiff), Playoffs 2024",
       x = "",
       y = "")

moto_plots <- ggarrange(moto_plot_ev,
                         moto_plot_whiff,
                         nrow = 2, ncol = 1)
moto_plots

wacha <- pitcher_stats %>%
  filter(Name == "Michael Wacha") %>%
  mutate(launch_speed = ifelse(is.na(launch_speed), 0, launch_speed),
         whiff = ifelse(description == "swinging_strike", "Yes", "No"))

wacha_plot_ev <- wacha %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = launch_speed, shape = pitch_type), size = 3) +
  scale_color_continuous() +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge vs. Michael Wacha (EV), Playoffs 2024",
       x = "",
       y = "")

wacha_plot_whiff <- wacha %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = whiff, shape = pitch_type), size = 3) +
  scale_color_discrete() +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge vs. Michael Wacha (Whiff), Playoffs 2024",
       x = "",
       y = "")

wacha_plots <- ggarrange(wacha_plot_ev,
                         wacha_plot_whiff,
                         nrow = 2, ncol = 1)
wacha_plots

bibee <- pitcher_stats %>%
  filter(Name == "Tanner Bibee") %>%
  mutate(launch_speed = ifelse(is.na(launch_speed), 0, launch_speed),
         whiff = ifelse(description == "swinging_strike", "Yes", "No"))

bibee_plot_ev <- bibee %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = launch_speed, shape = pitch_type), size = 3) +
  scale_color_continuous() +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge vs. Tanner Bibee (EV), Playoffs 2024",
       x = "",
       y = "")

bibee_plot_whiff <- bibee %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = whiff, shape = pitch_type), size = 3) +
  scale_color_discrete() +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge vs. Tanner Bibee (Whiff), Playoffs 2024",
       x = "",
       y = "")

bibee_plots <- ggarrange(bibee_plot_ev,
                         bibee_plot_whiff,
                         nrow = 2, ncol = 1)
bibee_plots

zerpa <- pitcher_stats %>%
  filter(Name == "Angel Zerpa") %>%
  mutate(launch_speed = ifelse(is.na(launch_speed), 0, launch_speed),
         whiff = ifelse(description == "swinging_strike", "Yes", "No"))

zerpa_plot_ev <- zerpa %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = launch_speed, shape = pitch_type), size = 3) +
  scale_color_continuous() +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge vs. Angel Zerpa (EV), Playoffs 2024",
       x = "",
       y = "")

zerpa_plot_whiff <- zerpa %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = whiff, shape = pitch_type), size = 3) +
  scale_color_discrete() +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge vs. Angel Zerpa (Whiff), Playoffs 2024",
       x = "",
       y = "")

zerpa_plots <- ggarrange(zerpa_plot_ev,
                         zerpa_plot_whiff,
                         nrow = 2, ncol = 1)
zerpa_plots

judge_hrs <- read_csv("judge_hrs.csv")

judge_hrs <- judge_hrs %>%
  mutate(pitch_type = ifelse(pitch_type == "KC", "CU", pitch_type))

judge_hrs_plot <- judge_hrs %>%
  ggplot(mapping = aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = pitch_type), size = 3) +
  geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
  geom_segment(x = left, y = top, xend = right, yend = top) +
  geom_segment(x = left, y = bottom, xend = left, yend = top) +
  geom_segment(x = right, y = bottom, xend = right, yend = top) +
  geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
  geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
  geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
  geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
  geom_segment(x = left, y = 0, xend = right, yend = 0) +
  geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
  geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
  geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
  xlim(-2.5, 2.5) + ylim(-.5, 5) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Aaron Judge Regular Season 2024 (HRs)")

judge_hrs_plot


zerpa_plots <- ggarrange(zerpa_plot_ev,
                         zerpa_plot_whiff,
                         nrow = 2, ncol = 1)
zerpa_plots