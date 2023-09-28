# Exploratory check of subscale correlations
library(ggplot2)
library(corrplot)
library(tidyverse)

setwd("C:/Users/farka/OneDrive/Desktop/Bence_PhD_project/open/FF/data")
data_sbscl <- read.csv("data_full_2.csv")

data_sbscl$act <- rowMeans(data_sbscl[,c("act_3y","act_5y")])
data_sbscl$act <- (data_sbscl$act - mean(data_sbscl$act,na.rm = T)) / sd(data_sbscl$act,na.rm = T)

data_sbscl$toys <- rowMeans(data_sbscl[,c("toys_3y","toys_5y")])
data_sbscl$toys <- (data_sbscl$toys - mean(data_sbscl$toys,na.rm = T)) / sd(data_sbscl$toys,na.rm = T)

data_sbscl$interact <- rowMeans(data_sbscl[,c("interact_3y","interact_5y")])
data_sbscl$interact <- (data_sbscl$interact - mean(data_sbscl$interact,na.rm = T)) / sd(data_sbscl$interact,na.rm = T)

data_sbscl$deprivation <- data_sbscl$act + data_sbscl$toys + data_sbscl$interact

data_sbscl$phys_agg <- rowMeans(data_sbscl[,c("phys_agg_3y","phys_agg_5y")])
data_sbscl$phys_agg <- (data_sbscl$phys_agg - mean(data_sbscl$phys_agg,na.rm = T)) / sd(data_sbscl$phys_agg,na.rm = T)

data_sbscl$psych_agg <- rowMeans(data_sbscl[,c("psych_agg_3y","psych_agg_5y")])
data_sbscl$psych_agg <- (data_sbscl$psych_agg - mean(data_sbscl$psych_agg,na.rm = T)) / sd(data_sbscl$psych_agg,na.rm = T)

data_sbscl$violence <- rowMeans(data_sbscl[,c("violence_3y","violence_5y")])
data_sbscl$violence <- (data_sbscl$violence - mean(data_sbscl$violence,na.rm = T)) / sd(data_sbscl$violence,na.rm = T)

data_sbscl$threat <- data_sbscl$phys_agg + data_sbscl$psych_agg + data_sbscl$violence

data_sbscl$arrang_3y <- as.numeric(data_sbscl$arrang_3y)
data_sbscl$arrang_5y <- as.numeric(data_sbscl$arrang_5y)
data_sbscl$arrang <- rowMeans(data_sbscl[,c("arrang_3y","arrang_5y")])
data_sbscl$arrang <- (data_sbscl$arrang - mean(data_sbscl$arrang,na.rm = T)) / sd(data_sbscl$arrang,na.rm = T)

data_sbscl$bedtime <- rowMeans(data_sbscl[,c("bedtime_3y","bedtime_5y")])
data_sbscl$bedtime <- (data_sbscl$bedtime - mean(data_sbscl$bedtime,na.rm = T)) / sd(data_sbscl$bedtime,na.rm = T)

data_sbscl$bed_routine <- rowMeans(data_sbscl[,c("bed_routine_3y","bed_routine_5y")])
data_sbscl$bed_routine <- (data_sbscl$bed_routine - mean(data_sbscl$bed_routine,na.rm = T)) / sd(data_sbscl$bed_routine,na.rm = T)

data_sbscl$mom_depress <- rowMeans(data_sbscl[,c("mom_depress_3y","mom_depress_5y")])
data_sbscl$mom_depress <- (data_sbscl$mom_depress - mean(data_sbscl$mom_depress,na.rm = T)) / sd(data_sbscl$mom_depress,na.rm = T)

data_sbscl$unpredictability <- data_sbscl$arrang + data_sbscl$bedtime + data_sbscl$bed_routine + data_sbscl$mom_depress

data_sbscl$sep_3y <- as.numeric(data_sbscl$sep_3y)
data_sbscl$sep_5y <- as.numeric(data_sbscl$sep_5y)
data_sbscl$sep <- rowMeans(data_sbscl[,c("sep_3y","sep_5y")])
data_sbscl$sep <- (data_sbscl$sep - mean(data_sbscl$sep,na.rm = T)) / sd(data_sbscl$sep,na.rm = T)

data_sbscl$move_3y <- as.numeric(data_sbscl$move_3y)
data_sbscl$move_5y <- as.numeric(data_sbscl$move_5y)
data_sbscl$move <- rowMeans(data_sbscl[,c("move_3y","move_5y")])
data_sbscl$move <- (data_sbscl$move - mean(data_sbscl$move,na.rm = T)) / sd(data_sbscl$move,na.rm = T)

data_sbscl$jobs_3y <- as.numeric(data_sbscl$jobs_3y)
data_sbscl$jobs_5y <- as.numeric(data_sbscl$jobs_5y)
data_sbscl$jobs <- rowMeans(data_sbscl[,c("jobs_3y","jobs_5y")])
data_sbscl$jobs <- (data_sbscl$jobs - mean(data_sbscl$jobs,na.rm = T)) / sd(data_sbscl$jobs,na.rm = T)

data_sbscl$mom_depress_ch <- rowMeans(data_sbscl[,c("mom_depress_ch_3y","mom_depress_ch_5y")])
data_sbscl$mom_depress_ch <- (data_sbscl$mom_depress_ch - mean(data_sbscl$mom_depress_ch,na.rm = T)) / sd(data_sbscl$mom_depress_ch,na.rm = T)

data_sbscl$volatility <- data_sbscl$sep + data_sbscl$move + data_sbscl$jobs + data_sbscl$mom_depress_ch

cormat <- cor(data_sbscl[,c(41:49,53,57,62,67)],use = "pairwise.complete.obs")
corrplot.mixed(cormat)

