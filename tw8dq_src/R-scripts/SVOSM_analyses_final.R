################################################################################

# Assessing the Test-Retest Reliability of the Social Value Orientation Slider 
# Measure (SVOSM)
# Author: Carlos de Matos Fernandes
# Affiliation: Department of Sociology, University of Groningen, the Netherlands
# Email: c.a.de.matos.fernandes@rug.nl

################################################################################

# This R-script guides you through our analyses of the test-retest reliability
# of the social value orientation slider measure using data from 6 monthly 
# waves. The data was collected by the LISS panel: https://www.lissdata.nl/  

# This script is by no means written for the purpose to provide a maximally 
# efficient code. 

################################################################################

################################################################################
####
####  Prepare the data
####
################################################################################ 

# set working directory where the data and other files are stored
setwd("/working-directory")

# check working directory
getwd()

# load the following libraries
# if not installed please first use install.packages("package")
library(tidyverse)
library(dplyr)
library(sjmisc)
library(reshape2)
library(Hmisc)
library(irr)
library(haven)
library(kSamples)
library(plyr)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(forcats)
library(ggalluvial)
library(Cairo)
library(lme4)
library(mitml)
library(jtools)
library(NISTunits)
library(mitml)
library(ltm)
library(lmerTest)
library(patchwork)

################################################################################
####
####  Load data
####
################################################################################

# load the data
# some additional variables are constructed along the way of running this script
load(file = "SVOSM_data.RData")
names(long_svo_multi)

################################################################################
####
####  transitivity check
####
################################################################################

# load transitivity check functions
# put this r-script in the file in which you store the data (your wd)
# before loading this variable, please adjust the variable names accordingly
# in the row_ranking check_row_transitivity functions. 
source('SVOSM_transitivity_check.R')

# run transitivity check and store TRUE/FALSE variable per row
long_svo_multi$trans <- apply(long_svo_multi, 1, check_row_transitivity)

# count and proportion false/true in the sample
summary(long_svo_multi$trans)
table(long_svo_multi$trans)/sum(table(long_svo_multi$trans))

# count transitive respondents
aggregate(long_svo_multi$trans,list(long_svo_multi$wave_cat),FUN=sum,na.rm =T)

# mean level of transitive respondents per wave
aggregate(long_svo_multi$trans,list(long_svo_multi$wave_cat),FUN=mean,na.rm =T)

################################################################################
####
####  Vector length check, adopted from Bakker and Dijkstra (2021)
####  doi: 10.1177/01902725211008938
####
################################################################################

# vector length
# remove 50 from angle
mean_self <- long_svo_multi$SVO_mean_first_six_Items_Self - 50
mean_other <- long_svo_multi$SVO_mean_first_six_Items_Other - 50

# function to calculate vector lengths
calculate_vlength = function(mean_self, mean_other) {
  length = sqrt(mean_self^2+mean_other^2)
  return(length)}

# create variable with only NA values
long_svo_multi$vlengths = rep(NA, times = 2970) 

# calculate vector length per row
for (i in 1:2970) {long_svo_multi$vlengths[i]=calculate_vlength(mean_self[i], 
                                                                mean_other[i])}

# apply criterion of 35
long_svo_multi$trans_vlenght<-ifelse(long_svo_multi$vlengths > 35,
            ifelse(long_svo_multi$vlengths != "NA", TRUE, FALSE),FALSE)

# implement 37.5 and 40 (insted of 35) to check the consequences of a stricter 
# vector length criterion

# exploration of vector length criterion
summary(long_svo_multi$trans_vlenght)
table(long_svo_multi$trans_vlenght)/sum(table(long_svo_multi$trans_vlenght))

# compare transitivity check and vector length criterion
table(long_svo_multi$trans_vlenght,long_svo_multi$trans)
long_svo_multi %>% group_by(trans) %>% frq(trans_vlenght)

# count of intransitive and too short vector length cases
long_svo_multi%>%group_by(wave_cat)%>%frq(trans & trans_vlenght)

################################################################################
####
####  criterion to select applicable cases
####
################################################################################

# select only rows with transitive and good vector length response profiles
# as well as respondents with NA values
# this criterion used to select applicable cases for data analyses and figures
data_criterion <- (long_svo_multi$trans == "TRUE" & 
                   long_svo_multi$trans_vlenght == "TRUE" | 
                   is.na(long_svo_multi$trans) | 
                   is.na(long_svo_multi$trans_vlenght))

################################################################################
####
####  Inspection of the data: Frequencies, percentages, means, sd, 
####  regression, multilevel model, correlations
####
################################################################################

# count and proportion of SVO per wave
long_svo_multi%>%filter(data_criterion)%>%group_by(wave_cat)%>%frq(SVO_type_cat)
long_svo_multi%>%filter(data_criterion)%>%group_by(wave_cat)%>%frq(SVO_dicho)

################################################################################

# multilevel analysis to estimate explanatory power of SVO categories on SVO 
# angle (reported in the paper), but we control for the nested data structure 
# that we have.
a<-lmer(SVO_angle~SVO_type_cat+(1|id),(long_svo_multi%>%filter(data_criterion)))
summ(a)
b<-lmer(SVO_angle~SVO_dicho+(1|id),(long_svo_multi%>%filter(data_criterion)))
summ(b)

# inspecting multiple R-squared measures for multilevel models
multilevelR2(a)
multilevelR2(b)

################################################################################

# mean and sd svo angles per wave
long_svo_multi %>% filter(data_criterion) %>% group_by(wave_cat) %>%
  summarise_at(vars(SVO_angle), funs(mean), na.rm = TRUE)
long_svo_multi %>% filter(data_criterion) %>% group_by(wave_cat) %>%
  summarise_at(vars(SVO_angle), funs(sd), na.rm = TRUE)

# mean and sd total
long_svo_multi %>% filter(data_criterion) %>% 
  summarise_at(vars(SVO_angle), mean,na.rm = TRUE)
long_svo_multi %>% filter(data_criterion) %>% 
  summarise_at(vars(SVO_angle), funs(sd), na.rm = TRUE)

# mean and sd total of items
long_svo_multi %>% filter(data_criterion) %>% group_by(wave_cat) %>%
  summarise_at(c(3:16), mean,na.rm = TRUE)
long_svo_multi %>% filter(data_criterion) %>% group_by(wave_cat) %>%
  summarise_at(c(3:16), funs(sd),na.rm = TRUE)
long_svo_multi %>% filter(data_criterion) %>% 
  summarise_at(c(15:16), mean,na.rm = TRUE)
long_svo_multi %>% filter(data_criterion) %>% 
  summarise_at(c(15:16), funs(sd),na.rm = TRUE)

################################################################################

# prepare data for correlations SVO angles
data_wide <- dcast(long_svo_multi %>% filter(data_criterion), id ~  wave_cat, 
          value.var = "SVO_angle")
data_correlation <- data_wide[, c(2:7)] 

# tables for correlation SVO angles across waves
# table 2
rcorr(as.matrix(data_correlation))

################################################################################

# mean SVO stability per wave
# distribution of change in SVO categories
long_svo_multi %>% filter(data_criterion) %>%
  summarise_at(vars(change_cat), mean, na.rm = TRUE)

aggregate(change_cat ~ wave_cat, data = (long_svo_multi %>%  
                                       filter(data_criterion)), mean,na.rm =T)

################################################################################

################################################################################
####
####  Multilevel model to inspect influence of prior SVO score on current SVO
####
################################################################################

# studying prior continuous svo on prospective svo score
# table 3
data_mod <- (long_svo_multi %>% filter(data_criterion)) %>%                             
  group_by(id) %>% 
  dplyr::mutate(prior_SVO = lag(SVO_angle, n = 1, default = NA)) 

model_mod<-lmer(SVO_angle ~ factor(wave)  + prior_SVO +
                (1|id), data_mod)
summary(model_mod)
summ(model_mod)

################################################################################
####
####  ICC, AD-test, Fisher, Kolmogorov-Smirnov
####
################################################################################

# Prepare data for intraclass correlation coefficient, SVO angles
adjust <-dplyr::select(long_svo_multi,id,SVO_angle,wave_cat,trans,trans_vlenght)
data_input <- na.omit(adjust) 
data_input$SVO_angle <- round(data_input$SVO_angle, digits = 2) 
data_input <- data_input %>% filter(trans == "TRUE" & trans_vlenght == "TRUE")
data_wide_2 <- dcast(data_input, id ~ wave_cat, value.var="SVO_angle") 
rownames(data_wide_2) <- data_wide_2$id 
data_wide_2 = dplyr::select(data_wide_2, -id)
data_wide_2 <- na.omit(data_wide_2)
# ICC SVO angles
icc(data_wide_2, model = "twoway", type = "agreement", unit = "single")

################################################################################

# Anderson-Darling K-smaples test
# prepare data for AD test
long_svo_multi_1 = long_svo_multi %>% filter(data_criterion)
data_wave_1 <- long_svo_multi_1[which(long_svo_multi_1$wave==1), ]
data_wave_2 <- long_svo_multi_1[which(long_svo_multi_1$wave==2), ]
data_wave_3 <- long_svo_multi_1[which(long_svo_multi_1$wave==3), ]
data_wave_4 <- long_svo_multi_1[which(long_svo_multi_1$wave==4), ]
data_wave_5 <- long_svo_multi_1[which(long_svo_multi_1$wave==5), ]
data_wave_6 <- long_svo_multi_1[which(long_svo_multi_1$wave==6), ]

# compute the AD test for SVO degrees
ad.test(data_wave_1$SVO_angle,data_wave_2$SVO_angle,data_wave_3$SVO_angle,
        data_wave_4$SVO_angle,data_wave_5$SVO_angle,data_wave_6$SVO_angle)

# similar as for item scoring
# self and other payoff distribution is similar
ad.test(data_wave_1$SVO_Item_01_Other,data_wave_2$SVO_Item_01_Other,
        data_wave_3$SVO_Item_01_Other,data_wave_4$SVO_Item_01_Other,
        data_wave_5$SVO_Item_01_Other,data_wave_6$SVO_Item_01_Other)
ad.test(data_wave_1$SVO_Item_02_Other,data_wave_2$SVO_Item_02_Other,
        data_wave_3$SVO_Item_02_Other,data_wave_4$SVO_Item_02_Other,
        data_wave_5$SVO_Item_02_Other,data_wave_6$SVO_Item_02_Other)
ad.test(data_wave_1$SVO_Item_03_Other,data_wave_2$SVO_Item_03_Other,
        data_wave_3$SVO_Item_03_Other,data_wave_4$SVO_Item_03_Other,
        data_wave_5$SVO_Item_03_Other,data_wave_6$SVO_Item_03_Other)
ad.test(data_wave_1$SVO_Item_04_Other,data_wave_2$SVO_Item_04_Other,
        data_wave_3$SVO_Item_04_Other,data_wave_4$SVO_Item_04_Other,
        data_wave_5$SVO_Item_04_Other,data_wave_6$SVO_Item_04_Other)
ad.test(data_wave_1$SVO_Item_05_Other,data_wave_2$SVO_Item_05_Other,
        data_wave_3$SVO_Item_05_Other,data_wave_4$SVO_Item_05_Other,
        data_wave_5$SVO_Item_05_Other,data_wave_6$SVO_Item_05_Other)
ad.test(data_wave_1$SVO_Item_06_Other,data_wave_2$SVO_Item_06_Other,
        data_wave_3$SVO_Item_06_Other,data_wave_4$SVO_Item_06_Other,
        data_wave_5$SVO_Item_06_Other,data_wave_6$SVO_Item_06_Other)

# Kolmogorov-Smirnov test & Fishers-exact test
# Comparing distribution from wave 6 and 1 for dropouts
# The following is needed because we need to explicate who dropped out 
# and who stayed. This is not possible with different sizes per data frame 
# (at least not to my knowledge).
data_wave_1_adj <- long_svo_multi[ which(long_svo_multi$wave==1), ]
data_wave_2_adj <- long_svo_multi[ which(long_svo_multi$wave==2), ]
data_wave_3_adj <- long_svo_multi[ which(long_svo_multi$wave==3), ]
data_wave_4_adj <- long_svo_multi[ which(long_svo_multi$wave==4), ]
data_wave_5_adj <- long_svo_multi[ which(long_svo_multi$wave==5), ]
data_wave_6_adj <- long_svo_multi[ which(long_svo_multi$wave==6), ]

###########Wave 6
dropouts <- data_wave_1_adj[is.na(data_wave_6_adj$SVO_angle), ]
dropouts <- dropouts %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                                | is.na(trans) | is.na(trans_vlenght))
stayers <- na.omit(data_wave_6_adj)
stayers <- stayers %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                              | is.na(trans) | is.na(trans_vlenght)) 
# Kolmogorov-Smirnov k-samples test for SVO angles
ks.test(dropouts$SVO_angle, stayers$SVO_angle)

# Fishers-exact test for SVO types
fisher.test(table(dropouts$SVO_dicho_num),table( stayers$SVO_dicho_num))
###########

############Wave 5
dropouts <- data_wave_1_adj[is.na(data_wave_5_adj$SVO_angle), ]
dropouts <- dropouts %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                                | is.na(trans) | is.na(trans_vlenght))
stayers <- na.omit(data_wave_5_adj)
stayers <- stayers %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                              | is.na(trans) | is.na(trans_vlenght)) 

# Kolmogorov-Smirnov k-samples test for SVO angles
ks.test(dropouts$SVO_angle, stayers$SVO_angle)

# Fishers-exact test for SVO types
fisher.test(table(dropouts$SVO_dicho_num),table( stayers$SVO_dicho_num))
###########

############Wave 4
dropouts <- data_wave_1_adj[is.na(data_wave_4_adj$SVO_angle), ]
dropouts <- dropouts %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                                | is.na(trans) | is.na(trans_vlenght))
stayers <- na.omit(data_wave_4_adj)
stayers <- stayers %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                              | is.na(trans) | is.na(trans_vlenght)) 

# Kolmogorov-Smirnov k-samples test for SVO angles
ks.test(dropouts$SVO_angle, stayers$SVO_angle)

# Fishers-exact test for SVO types
fisher.test(table(dropouts$SVO_dicho_num),table( stayers$SVO_dicho_num))
###########

############Wave 3
dropouts <- data_wave_1_adj[is.na(data_wave_3_adj$SVO_angle), ]
dropouts <- dropouts %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                                | is.na(trans) | is.na(trans_vlenght))
stayers <- na.omit(data_wave_3_adj)
stayers <- stayers %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                              | is.na(trans) | is.na(trans_vlenght)) 

# Kolmogorov-Smirnov k-samples test for SVO angles
ks.test(dropouts$SVO_angle, stayers$SVO_angle)

# Fishers-exact test for SVO types
fisher.test(table(dropouts$SVO_dicho_num),table( stayers$SVO_dicho_num))
###########

############Wave 2
dropouts <- data_wave_1_adj[is.na(data_wave_2_adj$SVO_angle), ]
dropouts <- dropouts %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                                | is.na(trans) | is.na(trans_vlenght))
stayers <- na.omit(data_wave_2_adj)
stayers <- stayers %>% filter(trans == "TRUE" & trans_vlenght == "TRUE" 
                              | is.na(trans) | is.na(trans_vlenght)) 

# Kolmogorov-Smirnov k-samples test for SVO angles
ks.test(dropouts$SVO_angle, stayers$SVO_angle)

# Fishers-exact test for SVO types
fisher.test(table(dropouts$SVO_dicho_num),table( stayers$SVO_dicho_num))
###########

################################################################################

# We need only the respondents who participated in all waves to calculate 
# Cohens Kappa
adjust_3 = dplyr::select(long_svo_multi_1, id,SVO_dicho,wave)
data_input_3 <- na.omit(adjust_3) 
data_wide_4 <- dcast(data_input_3, id ~ wave, value.var="SVO_dicho") 
data_wide_4 <- na.omit(data_wide_4)
long_data_wide_4 <- melt(data = data_wide_4, id.vars = c("id"), 
                         variable.name = "wave",value.name = "SVO_dicho")

adjust_3 = dplyr::select(long_svo_multi_1, id,SVO_dicho,wave)
data_wide_4 <- dcast(adjust_3, id ~ wave, value.var="SVO_dicho") 
long_data_wide_4 <- melt(data = data_wide_4, id.vars = c("id"), 
                         variable.name = "wave",value.name = "SVO_dicho")

data_wave_1 <- long_data_wide_4[which(long_data_wide_4$wave==1), ]
data_wave_2 <- long_data_wide_4[which(long_data_wide_4$wave==2), ]
data_wave_3 <- long_data_wide_4[which(long_data_wide_4$wave==3), ]
data_wave_4 <- long_data_wide_4[which(long_data_wide_4$wave==4), ]
data_wave_5 <- long_data_wide_4[which(long_data_wide_4$wave==5), ]
data_wave_6 <- long_data_wide_4[which(long_data_wide_4$wave==6), ]

# differences per wave
w1_to_w2<-cbind(data_wave_1$SVO_dicho,data_wave_2$SVO_dicho)
w2_to_w3<-cbind(data_wave_2$SVO_dicho,data_wave_3$SVO_dicho)
w3_to_w4<-cbind(data_wave_3$SVO_dicho,data_wave_4$SVO_dicho)
w4_to_w5<-cbind(data_wave_4$SVO_dicho,data_wave_5$SVO_dicho)
w5_to_w6<-cbind(data_wave_5$SVO_dicho,data_wave_6$SVO_dicho)

# calculating Cohens Kappa
kappa2(w1_to_w2)
kappa2(w2_to_w3)
kappa2(w3_to_w4)
kappa2(w4_to_w5)
kappa2(w5_to_w6)

# calculating Fleiss Kappa across all waves at once
data_wide_5 = dplyr::select(data_wide_4, -id)
kappam.fleiss(data_wide_5)


################################################################################
####
####  Multilevel analysis of near-boundary scoring, Appendix A
####
################################################################################

# calculate distance to proself-prosocial boundary
long_svo_multi$abs_near_boundary <- round(abs(long_svo_multi$SVO_angle - 22.45),
                                          digits = 2)

# reverse the variable
long_svo_multi <- long_svo_multi %>% mutate(reversed_abs_near_boundary = 
          (max(long_svo_multi$abs_near_boundary, na.rm=T)) - abs_near_boundary)

# multilevel analysis
model1<-glmer(change_cat ~ wave_cat + reversed_abs_near_boundary + SVO_dicho + 
                (1|id), family="binomial", (long_svo_multi %>%  
                                              filter(data_criterion)))

summary(model1)
multilevelR2(model1)

# brief visualization of the relation between changing in SVO and boundary
# scoring per wave, OSF figure
custom.col <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot((long_svo_multi %>%  filter((data_criterion), !is.na(change_cat))),
       aes(x=factor(change_cat),y=reversed_abs_near_boundary,fill=wave_cat)) + 
  geom_violin(trim=FALSE) + 
  scale_fill_manual(values = custom.col) +
  scale_color_manual(values = custom.col) +
  geom_boxplot(aes(col = wave_cat), width = 0.3, fill = "white", position = 
                 position_dodge(0.9)) +
  labs(x="Changing in SVO", y = "Proximity to the boundary") + 
  theme_classic(base_size=16) +
  scale_y_continuous(limits = c(0, 50),breaks=seq(0,40,10)) + 
  annotate("rect", xmin = 1.49, xmax = 1.54, ymin = 0, ymax = 50,
           alpha = .3,fill = "black") +
  theme(legend.title = element_blank())

################################################################################
####
####  Prepare data on dropping out, included in Figure 2, and later on in the 
####  in the analyses
####
################################################################################

# first step, make lagged variables of SVO angle and SVO dichotomous
long_svo_multi <- long_svo_multi %>%  group_by(id) %>% 
  dplyr::mutate(prior_SVO = lag(SVO_angle, n = 1, default = NA)) 

long_svo_multi <- long_svo_multi %>%  group_by(id) %>% 
  dplyr::mutate(prior_dicho = lag(SVO_dicho, n = 1, default = NA)) 

# lagged variables of distance to 45 boundary in SVO
long_svo_multi$distance_45<-round(abs(45 - long_svo_multi$prior_SVO),digits = 2)

# select waves 2 to 6
data_adjusted <- long_svo_multi %>% filter( wave ==2 | wave == 3 | wave == 4 | 
                                              wave == 5 | wave == 6)

# alter a variable to label dropouts as 1 and stayers as 0 for ML analysis and
# this variable as dependent variable
data_adjusted$stay_dropout <- data_adjusted$change_cat
data_adjusted$stay_dropout <- ifelse(data_adjusted$stay_dropout == 1,0,
                                     ifelse(data_adjusted$stay_dropout == 0,0,1))
data_adjusted$stay_dropout <- replace_na(data_adjusted$stay_dropout,value = 1)


data_adjusted_1 <- data_adjusted %>% filter(trans == "TRUE"  &  
                                            trans_vlenght == "TRUE" |
                                            is.na(trans) | is.na(trans_vlenght))

# for dropping out due to changing in categories
data_adjusted_2 <- data_adjusted_1 %>% filter(wave == 1 & stay_dropout == 0 | 
                                                wave == 2 )
data_adjusted_3 <- data_adjusted_1 %>% filter(wave == 2 & stay_dropout == 0 | 
                                                wave == 3 )
data_adjusted_4 <- data_adjusted_1 %>% filter(wave == 3 & stay_dropout == 0 | 
                                                wave == 4 )
data_adjusted_5 <- data_adjusted_1 %>% filter(wave == 4 & stay_dropout == 0 | 
                                                wave == 5 )
data_adjusted_6 <- data_adjusted_1 %>% filter(wave == 5 & stay_dropout == 0 | 
                                                wave == 6 )

################################################################################
####
####  FIGURES
####
################################################################################

# Figure 1
# SVO angle per wave
# calculate mean per wave to include in figure
mean_angle_wave <- ddply((long_svo_multi %>%  filter(data_criterion)), 
            "wave_cat", summarise, grp.mean=mean(na.omit(SVO_angle)))

fig1 <- long_svo_multi %>%   filter(data_criterion) %>%
  ggplot()+ aes(x=SVO_angle) +
  geom_histogram(binwidth = 2, position="identity", alpha=0.5,na.rm = TRUE,
                 colour="black", fill="grey") +
  geom_vline(data=mean_angle_wave, aes(xintercept=grp.mean),linetype="dashed") +
  #scale_color_manual(values=c("white", "white")) +
  #scale_fill_manual(values=c("snow3", "grey40")) +
  labs(x="SVO score", y = "Count") + 
  labs(title='') +
  facet_wrap(~wave_cat, ncol = 2) +
  scale_y_continuous(limits = c(0, 80),breaks=seq(10,70,20)) +
  scale_x_continuous(limits = c(-20, 65),breaks=seq(-20,60,10)) +
  theme_classic(base_size=16) +
  theme(legend.title = element_blank())
fig1

################################################################################

# Figure 2
# visualization of SVO test-retest reliability
data_wave_2 <- data_mod[which(data_mod$wave==2), ]
data_wave_3 <- data_mod[which(data_mod$wave==3), ]
data_wave_4 <- data_mod[which(data_mod$wave==4), ]
data_wave_5 <- data_mod[which(data_mod$wave==5), ]
data_wave_6 <- data_mod[which(data_mod$wave==6), ]

# data of dropouts
test  = dplyr::select(data_adjusted_1, id,prior_SVO,stay_dropout)
test <- test %>% filter(stay_dropout == 1) 
test_1  = dplyr::select(data_adjusted_2, id,prior_SVO,stay_dropout)
test_1 <- test_1 %>% filter(stay_dropout == 1) 
test_2  = dplyr::select(data_adjusted_3, id,prior_SVO,stay_dropout)
test_2 <- test_2 %>% filter(stay_dropout == 1) 
test_3  = dplyr::select(data_adjusted_4, id,prior_SVO,stay_dropout)
test_3 <- test_3 %>% filter(stay_dropout == 1) 
test_4  = dplyr::select(data_adjusted_5, id,wave,prior_SVO,stay_dropout)
test_4 <- test_4 %>% filter(stay_dropout == 1) 
test_5  = dplyr::select(data_adjusted_6, id,wave,prior_SVO,stay_dropout)
test_5 <- test_5 %>% filter(stay_dropout == 1) 

# all combined
wave_all <- ggplot(data = data_mod) +
  aes(x=prior_SVO) +
  aes(y=SVO_angle) +
  geom_point(alpha = 1,size=2,shape=21,color = "black",fill = "white") +
  geom_smooth(method = "lm", size = 1, alpha = 0.25, se = T, color = 'blue') +
  labs(x=expression(paste("SVO score at ",italic("t"), " - 1"))) +
  labs(y=expression(paste("SVO score at ",italic("t")))) +
  theme_classic(base_size=12) + 
  theme(plot.title = element_text(size = 16)) +
  theme(legend.title = element_blank()) +
  geom_abline(intercept = 0.2, slope = 1, size = 1, color = 'black') +
  labs(title = "a") +
  geom_rug(data=test, aes(x = prior_SVO), inherit.aes = F,
           color = "red",alpha = 1) +
  scale_x_continuous(limits = c(-20, 65),breaks=seq(-15,60,15)) + 
  scale_y_continuous(limits = c(-20, 65),breaks=seq(-15,60,15))
wave_all

wave1_and2 <- ggplot(data = data_wave_2) +
  aes(x=prior_SVO) +
  aes(y=SVO_angle) +
  geom_point(alpha = 1,size=2,shape=21,color = "black",fill = "white") +
  geom_smooth(method = "lm", size = 1, alpha = 0.25, se = T, color = 'blue') +
  labs(x = "SVO score at wave 1")+
  labs(y = "SVO score at wave 2")+
  theme_classic(base_size=12) + 
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  geom_abline(intercept = 0.2, slope = 1, size = 1, color = 'black') +
  labs(title='b') +
  geom_rug(data=test_1, aes(x = prior_SVO), inherit.aes = F,
           color = "red",alpha = 1) +
  scale_x_continuous(limits = c(-20, 65),breaks=seq(-15,60,15)) + 
  scale_y_continuous(limits = c(-20, 65),breaks=seq(-15,60,15))
wave1_and2

wave2_and3 <- ggplot(data = data_wave_3) +
  aes(x=prior_SVO) +
  aes(y=SVO_angle) +
  geom_point(alpha = 1,size=2,shape=21,color = "black",fill = "white") +
  geom_smooth(method = "lm", size = 1, alpha = 0.25, se = T, color = 'blue') +
  labs(x = "SVO score at wave 2")+
  labs(y = "SVO score at wave 3")+
  theme_classic(base_size=12) + 
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  geom_abline(intercept = 0.2, slope = 1, size = 1, color = 'black') +
  labs(title='c') +
  geom_rug(data=test_2, aes(x = prior_SVO), inherit.aes = F,
           color = "red",alpha = 1) +
  scale_x_continuous(limits = c(-20, 65),breaks=seq(-15,60,15)) + 
  scale_y_continuous(limits = c(-20, 65),breaks=seq(-15,60,15))
wave2_and3

wave3_and4 <- ggplot(data = data_wave_4) +
  aes(x=prior_SVO) +
  aes(y=SVO_angle) +
  geom_point(alpha = 1,size=2,shape=21,color = "black",fill = "white") +
  geom_smooth(method = "lm", size = 1, alpha = 0.25, se = T, color = 'blue') +
  labs(x = "SVO score at wave 3")+
  labs(y = "SVO score at wave 4")+
  theme_classic(base_size=12) + 
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  geom_abline(intercept = 0.2, slope = 1, size = 1, color = 'black') +
  labs(title='d') +
  geom_rug(data=test_3, aes(x = prior_SVO), inherit.aes = F,
           color = "red",alpha = 1) +
  scale_x_continuous(limits = c(-20, 65),breaks=seq(-15,60,15)) + 
  scale_y_continuous(limits = c(-20, 65),breaks=seq(-15,60,15))
wave3_and4

wave4_and5 <- ggplot(data = data_wave_5) +
  aes(x=prior_SVO) +
  aes(y=SVO_angle) +
  geom_point(alpha = 1,size=2,shape=21,color = "black",fill = "white") +
  geom_smooth(method = "lm", size = 1, alpha = 0.25, se = T, color = 'blue') +
  labs(x = "SVO score at wave 4")+
  labs(y = "SVO score at wave 5")+
  theme_classic(base_size=12) + 
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  geom_abline(intercept = 0.2, slope = 1, size = 1, color = 'black') +
  labs(title='e') +
  geom_rug(data=test_4, aes(x = prior_SVO), inherit.aes = F,
           color = "red",alpha = 1) +
  scale_x_continuous(limits = c(-20, 65),breaks=seq(-15,60,15)) + 
  scale_y_continuous(limits = c(-20, 65),breaks=seq(-15,60,15))
wave4_and5

wave5_and6 <- ggplot(data = data_wave_6) +
  aes(x=prior_SVO) +
  aes(y=SVO_angle) +
  geom_point(alpha = 1,size=2,shape=21,color = "black",fill = "white") +
  geom_smooth(method = "lm", size = 1, alpha = 0.25, se = T, color = 'blue') +
  labs(x = "SVO score at wave 5")+
  labs(y = "SVO score at wave 6")+
  theme_classic(base_size=12) + 
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(size = 16)) +
  geom_abline(intercept = 0.2, slope = 1, size = 1, color = 'black') +
  labs(title='f') +
  geom_rug(data=test_5, aes(x = prior_SVO), inherit.aes = F,
           color = "red",alpha = 1) +
  scale_x_continuous(limits = c(-20, 65),breaks=seq(-15,60,15)) + 
  scale_y_continuous(limits = c(-20, 65),breaks=seq(-15,60,15))
wave5_and6

# combining figures
# Fig. 2 in the paper
wave_combined <- wave_all+wave1_and2+wave2_and3+wave3_and4+ wave4_and5+ 
  wave5_and6 + plot_layout(ncol = 2)
wave_combined

################################################################################

# Figure 3
# visualizing changes in SVO types via alluvial plot

# extra to include intranstive ties
long_svo_multi$t1 <- ifelse(data_criterion == FALSE,
                            "intransitive and too short vector length",
                            long_svo_multi$SVO_dicho)

long_svo_multi$t2 <- fct_explicit_na(long_svo_multi$t1, na_level = "NA")

long_svo_multi$ordered_dicho <- factor(long_svo_multi$t2, levels = 
      c("intransitive and too short vector length","NA", "proself","prosocial"))

# ordered_dicho
ggplot((long_svo_multi %>% filter(data_criterion)),
       aes(x = wave, stratum = ordered_dicho, alluvium = id,
           fill = ordered_dicho)) +
  geom_lode(na.rm=T) + 
  geom_flow(curve_type = "cubic") +
  geom_stratum(alpha = 1,na.rm=T) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("white", "snow3", "grey40"))+ 
  labs(x = 'Wave')+
  labs(y = 'Count respondents')+
  theme_classic(base_size=16) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0.8, 6.2),breaks=seq(1,6,1)) + 
  scale_y_continuous(limits = c(0, 500),breaks=seq(0,500,100))

# export to eps for latex. needed to ensure that different shades in between 
# waves are included in the figure
setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(type='cairo'))
options(device='x11')

# before runnning this, set plot view correct
# 6.58 x 4.91 in image (Figure 3 in paper)
ggsave("fig3.eps", device=cairo_ps)

################################################################################

# figure 4
# distribution of types with NA
# Need to include NA in this way to reorder the variable in such a way that 
# NA appear as the first category.
long_svo_multi$ordered_dicho <- fct_explicit_na(long_svo_multi$SVO_dicho,
                                              na_level = "NA")
long_svo_multi$ordered_dicho <- factor(long_svo_multi$ordered_dicho, 
                                     levels = c("NA", "proself","prosocial"))

fig4a<-ggplot((long_svo_multi %>% filter(data_criterion)), 
  aes(x=wave, fill=ordered_dicho)) + 
  geom_bar(position="fill",color="black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=c("black", "black")) +
  scale_fill_manual(values=c("white","snow3", "grey40")) + 
  labs(x="Wave", y = "") + 
  scale_x_continuous(limits = c(0.5, 6.5),breaks=seq(1,6,1)) +
  theme_classic(base_size=16) + 
  labs(title='a') +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "bottom")
fig4a

# distribution of types without NA
fig4b<- filter((long_svo_multi %>% filter(data_criterion)), 
          !is.na(SVO_dicho)) %>% 
  ggplot() +
  aes(x = wave, fill = SVO_dicho) +
  geom_bar( position="fill", color="black") +
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values=c("black", "black")) +
  scale_fill_manual(values=c("snow3", "grey40")) + 
  labs(title='b') +
  labs(x="Wave", y = "") + 
  scale_x_continuous(limits = c(0.5, 6.5),breaks=seq(1,6,1)) +
  theme_classic(base_size=16) +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "bottom")
fig4b

# grid arrange to create figure with distribution of angles and SVO categories
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

bp <- theme(legend.position="none")
# extract legend to create 1 overlapping legend
legend <- get_legend(fig4a) # this one is needed for NA inclusions

# Fig. 4 in the paper
grid.arrange(fig4a + bp,fig4b + bp, legend, ncol = 2, layout_matrix = 
             cbind(c(1,3), c(2,3)), heights = c(3,0.5))

################################################################################
####
#### Assessing determinants of dropping out
####
################################################################################

# does dropping out depend on SVO scoring?

# select data per wave to accomodate missing values on current SVO scores due to
# dropping out. The models rely on prior SVO score
data_adjusted_svo_2 <- data_adjusted_1 %>% filter(wave == 2 & 
                                                    prior_dicho != "NA")
data_adjusted_svo_3 <- data_adjusted_1 %>% filter(wave == 3 & 
                                                    prior_dicho != "NA")
data_adjusted_svo_4 <- data_adjusted_1 %>% filter(wave == 4 & 
                                                    prior_dicho != "NA")
data_adjusted_svo_5 <- data_adjusted_1 %>% filter(wave == 5 & 
                                                    prior_dicho != "NA")
data_adjusted_svo_6 <- data_adjusted_1 %>% filter(wave == 6 & 
                                                    prior_dicho != "NA")

# in what follows: we explore dichotomous SVO categories, SVO continuous scores.
# and distance to score 45 as explanatory variable for dropping out
# wave 2
model_dicho<-glm(stay_dropout ~ prior_dicho, family="binomial", 
                   (data_adjusted_svo_2))
summary(model_dicho)

model_degree<-glm(stay_dropout ~ prior_SVO, family="binomial", 
                    (data_adjusted_svo_2))
summary(model_degree)

model_45<-glm(stay_dropout~distance_45,family="binomial",(data_adjusted_svo_2))
summary(model_45)

# wave 3
model_dicho<-glm(stay_dropout ~ prior_dicho, family="binomial", 
                   (data_adjusted_svo_3))
summary(model_dicho)

model_degree<-glm(stay_dropout ~ prior_SVO, family="binomial", 
                    (data_adjusted_svo_3))
summary(model_degree)

model_45<-glm(stay_dropout~distance_45,family="binomial",(data_adjusted_svo_3))
summary(model_45)

###
# wave 4
model_dicho<-glm(stay_dropout ~ prior_dicho, family="binomial", 
                   (data_adjusted_svo_4))
summary(model_dicho)

model_degree<-glm(stay_dropout ~ prior_SVO, family="binomial", 
                    (data_adjusted_svo_4))
summary(model_degree)

model_45<-glm(stay_dropout~distance_45,family="binomial",(data_adjusted_svo_4))
summary(model_45)

# Inspection of dropouts and SVO in wave 4
data_adjusted_svo_4 %>% group_by(stay_dropout) %>%
  summarise_at(vars(prior_SVO), mean , na.rm = TRUE)
data_adjusted_svo_4 %>% group_by(stay_dropout) %>%
  summarise_at(vars(prior_SVO), sd , na.rm = TRUE)
table(data_adjusted_svo_4$stay_dropout)
prop.table(table(data_adjusted_svo_4$stay_dropout))
dropouts <- data_adjusted_svo_4 %>% filter(stay_dropout == 1)
stayers <- data_adjusted_svo_4 %>% filter(stay_dropout == 0)
# Kolmogorov-Smirnov k-samples test for prior SVO angles
ks.test(dropouts$prior_SVO, stayers$prior_SVO)

###
# wave 5
model_dicho<-glm(stay_dropout ~ prior_dicho, family="binomial", 
                   (data_adjusted_svo_5))
summary(model_dicho)

model_degree<-glm(stay_dropout ~ prior_SVO, family="binomial", 
                    (data_adjusted_svo_5))
summary(model_degree)

model_45<-glm(stay_dropout~distance_45,family="binomial",(data_adjusted_svo_5))
summary(model_45)

# wave 6
model_dicho<-glm(stay_dropout ~ prior_dicho, family="binomial", 
                   (data_adjusted_svo_6))
summary(model_dicho)

model_degree<-glm(stay_dropout ~ prior_SVO, family="binomial", 
                    (data_adjusted_svo_6))
summary(model_degree)

model_45<-glm(stay_dropout~distance_45,family="binomial",(data_adjusted_svo_6))
summary(model_45)

################################################################################

# in what follows:
# select only those cases per wave that are needed. We delete change_cat cases
# from respondents who dropped out because they would be deleted list-wise.
# only include change_cat variable from the two waves prior to dropping out (or)
# staying in the sample.

# does dropping out in wave 3 depend on changes in SVO from wave 1 to 2
w_staydrop_3 <- dcast(data_adjusted_3, id ~ wave, value.var="stay_dropout") 
w_staydrop_3 = dplyr::select(w_staydrop_3, id,3)
w_change_2 <- dcast(data_adjusted_3, id ~ wave, value.var="change_cat") 
w_change_2 = dplyr::select(w_change_2, id,2)
long_w_change_2 <- melt(data = w_change_2, id.vars = c("id"), 
                         variable.name = "wave",value.name = "change_cat_wave2")
long_w_staydrop_3 <- melt(data = w_staydrop_3, id.vars = c("id"), 
                         variable.name = "wave",value.name = 
                           "stay_dropout_wave3")
long_w_change_2$stay_dropout_wave3 <-long_w_staydrop_3$stay_dropout_wave3
model2<-glm(stay_dropout_wave3 ~ factor(change_cat_wave2), family="binomial", 
            (long_w_change_2))
summary(model2)

# does dropping out in wave 4 depend on changes in SVO from wave 2 to 3
w_staydrop_4 <- dcast(data_adjusted_4, id ~ wave, value.var="stay_dropout") 
w_staydrop_4 = dplyr::select(w_staydrop_4, id,3)
w_change_3 <- dcast(data_adjusted_4, id ~ wave, value.var="change_cat") 
w_change_3 = dplyr::select(w_change_3, id,2)
long_w_change_3 <- melt(data = w_change_3, id.vars = c("id"), 
                        variable.name = "wave",value.name = "change_cat_wave3")
long_w_staydrop_4 <- melt(data = w_staydrop_4, id.vars = c("id"), 
                          variable.name = "wave",value.name = 
                            "stay_dropout_wave4")
long_w_change_3$stay_dropout_wave4 <-long_w_staydrop_4$stay_dropout_wave4
model3<-glm(stay_dropout_wave4 ~ factor(change_cat_wave3), family="binomial", 
            (long_w_change_3))
summary(model3)

# does dropping out in wave 5 depend on changes in SVO from wave 3 to 4
w_staydrop_5 <- dcast(data_adjusted_5, id ~ wave, value.var="stay_dropout") 
w_staydrop_5 = dplyr::select(w_staydrop_5, id,3)
w_change_4 <- dcast(data_adjusted_5, id ~ wave, value.var="change_cat") 
w_change_4 = dplyr::select(w_change_4, id,2)
long_w_change_4 <- melt(data = w_change_4, id.vars = c("id"), 
                        variable.name = "wave",value.name = "change_cat_wave4")
long_w_staydrop_5 <- melt(data = w_staydrop_5, id.vars = c("id"), 
                          variable.name = "wave",value.name = 
                            "stay_dropout_wave5")
long_w_change_4$stay_dropout_wave5 <-long_w_staydrop_5$stay_dropout_wave5
model4<-glm(stay_dropout_wave5 ~ factor(change_cat_wave4), family="binomial", 
            (long_w_change_4))
summary(model4)

# does dropping out in wave 6 depend on changes in SVO from wave 4 to 5
w_staydrop_6 <- dcast(data_adjusted_6, id ~ wave, value.var="stay_dropout") 
w_staydrop_6 = dplyr::select(w_staydrop_6, id,3)
w_change_5 <- dcast(data_adjusted_6, id ~ wave, value.var="change_cat") 
w_change_5 = dplyr::select(w_change_5, id,2)
long_w_change_5 <- melt(data = w_change_5, id.vars = c("id"), 
                        variable.name = "wave",value.name = "change_cat_wave5")
long_w_staydrop_6 <- melt(data = w_staydrop_6, id.vars = c("id"), 
                          variable.name = "wave",value.name = 
                            "stay_dropout_wave6")
long_w_change_5$stay_dropout_wave6 <-long_w_staydrop_6$stay_dropout_wave6
model5<-glm(stay_dropout_wave6 ~ factor(change_cat_wave5), family="binomial", 
            (long_w_change_5))
summary(model5)

################################################################################
####
#### Assessing minor longitudinal differences in SVO angles
####
################################################################################

# select applicable cases and variables
adjust <-dplyr::select(long_svo_multi,id,SVO_angle,wave_cat,trans,trans_vlenght)
data_input <- na.omit(adjust) 
data_input$SVO_angle <- round(data_input$SVO_angle, digits = 2) 
data_input <- data_input %>% filter(trans == "TRUE" & trans_vlenght == "TRUE")
data_wide_2 <- dcast(data_input, id ~ wave_cat, value.var="SVO_angle") 
data_wide_2 <- na.omit(data_wide_2)

# compare SVO scores wave per wave
data_wide_2$diff_w1 <- 0
data_wide_2$diff_w1_w2 <- round(data_wide_2$`wave 2` - 
                                  data_wide_2$`wave 1`,digits = 2)
data_wide_2$diff_w2_w3 <- round(data_wide_2$`wave 3` - 
                                  data_wide_2$`wave 2`,digits = 2)
data_wide_2$diff_w3_w4 <- round(data_wide_2$`wave 4` - 
                                  data_wide_2$`wave 3`,digits = 2)
data_wide_2$diff_w4_w5 <- round(data_wide_2$`wave 5` - 
                                  data_wide_2$`wave 4`,digits = 2)
data_wide_2$diff_w5_w6 <- round(data_wide_2$`wave 6` - 
                                  data_wide_2$`wave 5`,digits = 2)

# select variables
data_wide_4 <- data_wide_2
data_wide_5 <- data_wide_4[, c(1:7)] 
data_wide_6 <- data_wide_4[, c(1,8:13)] 

# from wide to long format for visualization
long_SVO_2 <-melt(data = data_wide_5, id.vars = c("id"), variable.name = "wave",
                 value.name = "SVO_angle")
long_SVO_3 <-melt(data = data_wide_6, id.vars = c("id"), variable.name = "wave",
                   value.name = "diff_SVO_angle")

################################################################################
# Figures in OSF figure folder
# flow of SVO angles over time
ggplot(long_SVO_2, aes(x = wave, y = round(SVO_angle),colour = id,group = id)) +
  geom_line(size=0.1) + 
  labs(title='Distribution of SVO degrees across waves') +
  scale_colour_grey(start = 0.5, end = 0.1) +
  labs(x="", y = "SVO degree scores") + 
  scale_y_continuous(limits = c(-20, 65),breaks=seq(-20,60,20)) +
  theme_classic(base_size=14) +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none")

# comparing changes in angle wave per wave
ggplot(long_SVO_3, aes(x = wave, y = round(diff_SVO_angle), colour = id, 
                       group = id)) +
  geom_line(size=0.1) +   
  scale_colour_grey(start = 0.5, end = 0.1) +
  labs(x="", y = "Difference in SVO degree") +
  labs(title='Distribution of differences in SVO degrees across waves') +
  scale_y_continuous(limits = c(-50, 60),breaks=seq(-50,50,10)) +
  theme_classic(base_size=14) +
  theme(legend.title = element_blank()) + 
  scale_x_discrete(labels=c("diff_w1" = "wave 1",
                            "diff_w1_w2" = "wave 2",
                            "diff_w2_w3" = "wave 3",
                            "diff_w3_w4" = "wave 4",
                            "diff_w4_w5" = "wave 5",
                            "diff_w5_w6" = "wave 6")) +
  theme(legend.position = "none")

################################################################################
# comparing differences in SVO with percentages
# first set differences as absolute values
long_SVO_3$abs_diff_SVO_angle <- abs(long_SVO_3$diff_SVO_angle)


# total mean and sd
mean(long_SVO_3$abs_diff_SVO_angle)
sd(long_SVO_3$abs_diff_SVO_angle)

# mean and sd per wave
long_SVO_3  %>% group_by(wave) %>%
  summarise_at(vars(long_SVO_3$abs_diff_SVO_angle), funs(mean), na.rm = TRUE)
long_SVO_3 %>% group_by(wave) %>%
  summarise_at(vars(long_SVO_3$abs_diff_SVO_angle), funs(sd), na.rm = TRUE)

# dividing sample below and above a certain cutoff point
# cutoff = 1
long_SVO_3$size_diff<- ifelse(long_SVO_3$abs_diff_SVO_angle < (1),"lower", "higher")
table(long_SVO_3$size_diff)
prop.table(table(long_SVO_3$size_diff))

# cutoff = 3
long_SVO_3$size_diff <- ifelse(long_SVO_3$abs_diff_SVO_angle < (3),
                               "lower", "higher")
table(long_SVO_3$size_diff)
prop.table(table(long_SVO_3$size_diff))

# cutoff = SD
long_SVO_3$size_diff <- ifelse(long_SVO_3$abs_diff_SVO_angle < 
                                 (1*sd(long_SVO_3$abs_diff_SVO_angle)),
                   "lower", "higher")
table(long_SVO_3$size_diff)
prop.table(table(long_SVO_3$size_diff))

# cutoff = 2 times SD
long_SVO_3$size_diff <- ifelse(long_SVO_3$abs_diff_SVO_angle < 
                                 (2*sd(long_SVO_3$abs_diff_SVO_angle)),
                               "lower", "higher")
table(long_SVO_3$size_diff)
prop.table(table(long_SVO_3$size_diff))

# cutoff = 3 times SD (not reported in paper)
long_SVO_3$size_diff <- ifelse(long_SVO_3$abs_diff_SVO_angle < 
                                 (3*sd(long_SVO_3$abs_diff_SVO_angle)),
                               "lower", "higher")
table(long_SVO_3$size_diff)
prop.table(table(long_SVO_3$size_diff))

################################################################################
# cumulative change of difference in SVO angles wave per wave
data_wide_6$change_wave_1 <- data_wide_6$diff_w1
data_wide_6$change_wave_1_2 <- data_wide_6$diff_w1_w2
data_wide_6$change_wave_1_2_3 <- data_wide_6$change_wave_1_2 + 
  data_wide_6$diff_w2_w3
data_wide_6$change_wave_1_2_3_4 <- data_wide_6$change_wave_1_2_3 +
  data_wide_6$diff_w3_w4
data_wide_6$change_wave_1_2_3_4_5 <- data_wide_6$change_wave_1_2_3_4 + 
  data_wide_6$diff_w4_w5
data_wide_6$change_wave_1_2_3_4_5_6 <- data_wide_6$change_wave_1_2_3_4_5 + 
  data_wide_6$diff_w5_w6
data_wide_7 <- data_wide_6[, c(1,8:13)] 
long_SVO_4 <- melt(data = data_wide_7, id.vars = c("id"), 
                   variable.name = "wave_compare",
                   value.name = "cumulative_change_in_SVO")

# visualizing cumulative change
# Figure in OSF folder
ggplot(long_SVO_4, aes(x = wave_compare, y = round(cumulative_change_in_SVO), 
                       colour = id, group = id)) +
  geom_line(size=0.1) +   
  labs(title='Cumulative change in difference between SVO degrees') +
  scale_colour_grey(start = 0.5, end = 0.1) +
  labs(x="", y = "Cumulative differences in SVO degree") + 
  scale_y_continuous(limits = c(-50, 50),breaks=seq(-50,50,10)) +
  theme_classic(base_size=14) +
  theme(legend.title = element_blank()) + 
  scale_x_discrete(labels=c("change_wave_1" = "wave 1",
                            "change_wave_1_2" = "wave 1-2",
                            "change_wave_1_2_3" = "wave 1-3",
                            "change_wave_1_2_3_4" = "wave 1-4",
                            "change_wave_1_2_3_4_5" = "wave 1-5",
                            "change_wave_1_2_3_4_5_6" = "wave 1-6")) +
  theme(legend.position = "none")

################################################################################

################################################################################
####
#### end of r-script
####
################################################################################