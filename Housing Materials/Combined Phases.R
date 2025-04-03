setwd("C:/Users/mrthy/Desktop/Uni boiiiiis/PhD/Papers to Submit/Bystander Carton Experiment/Analysis")

##### Libraries #####
library(tidyverse)
library(Matrix)
library(lme4)
library(emmeans)
library(effects)
library(car)
library(ggpubr)
library(cowplot)
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions

#### Reading in raw data ####
phase1 <- read.csv("Phase_1_Raw_Data.csv") #pulling in raw data from phase 1
phase1F <- read.csv("Phase1FemaleData.csv")
phase1M <- read.csv("Phase1MaleData.csv")
phase2 <- read.csv("P2RDateSimplified.csv") #data which was sorted to have date and treatment separated
phase2F <- read.csv("Phase2FemaleData.csv")
phase2M <- read.csv("Phase2MaleData.csv")


##### population data #####

#### Managing data, separating reps and treatments ####
Phase1Data <- phase1 %>% 
  separate(Group, c("tmp1", "Date", "Treatment")) %>% ##This separates group (replicates) into treatment and date (where date is replicates and treatment is experimental grouping)
  mutate(Date = as.factor(Date), Sex = factor(x = Sex, levels = c("F", "M"))) %>% #comparing to females
  mutate(Treatment = factor(x = Treatment, levels = c("Sham", "IR"))) #comparing to sham

Phase2Data <- phase2 %>% 
  mutate(Treatment = factor(Treatment,levels = c("sham", "bystander", "new"))) %>%
  mutate(Sex = factor(Sex, levels = c("F", "M"))) 

#### controlling for rep effects ####
MatTime1 <- lmer(Time ~  Treatment + Sex+ (1 | Date), data = Phase1Data)
MatMass1 <- lmer(Weight ~  Treatment + Sex+ (1 | Date), data = Phase1Data)
MatRate1 <- lmer(GR ~  Treatment + Sex+ (1 | Date), data = Phase1Data)

MatTime2 <- lmer(Time ~  Treatment + Sex+ (1 | Date), data = Phase2Data)
MatMass2 <- lmer(Weight ~  Treatment + Sex+ (1 | Date), data = Phase2Data)
MatRate2 <- lmer(Growth.Rate ~  Treatment + Sex+ (1 | Date), data = Phase2Data)

#### Confidence intervals ####
confint.merMod(MatTime1)
confint.merMod(MatMass1)
confint.merMod(MatRate1)
confint.merMod(MatTime2)
confint.merMod(MatMass2)
confint.merMod(MatRate2)


#### plots ####
time1 <- sjPlot::plot_model(MatTime1, show.values = T, show.p = T, title = "Phase 1 Time to Maturation (day)",
                            vline.color = "#061423",
  axis.labels = 
  c("Males Compared to Females", "0.5 Gy Compared to Sham"
  )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

mass1 <- sjPlot::plot_model(MatMass1, show.values = T, show.p = T, title = "Phase 1 Mass at Maturation (mg)",
                            vline.color = "#061423",
  axis.labels = 
  c("Males Compared to Females", "0.5 Gy Compared to Sham"
  )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rate1 <- sjPlot::plot_model(MatRate1, show.values = T, show.p = T, title = "Phase 1 Average Growth Rate (mg/day)", 
                            vline.color = "#061423",
  axis.labels = 
  c("Males Compared to Females", "0.5 Gy Compared to Sham"
  )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)")

ggarrange(time1, mass1, rate1, ncol = 1,labels = "auto")

time2 <- sjPlot::plot_model(MatTime2, show.values = T, show.p = T, title = "Phase 2 Time to Maturation (day)",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Males Compared to Females", "0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                              )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

mass2 <- sjPlot::plot_model(MatMass2, show.values = T, show.p = T, title = "Phase 2 Mass at Maturation (mg)",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Males Compared to Females", "0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                              )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rate2 <- sjPlot::plot_model(MatRate2, show.values = T, show.p = T, title = "Phase 2 Average Growth Rate (mg/day)",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Males Compared to Females", "0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                              )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(time2, mass2, rate2, ncol = 1,labels = "auto")


#### estimated means ####
emmeans(MatTime1, specs = "Treatment") |> pwpm()
emmeans(MatMass1, specs = "Treatment") |> pwpm()
emmeans(MatRate1, specs = "Treatment") |> pwpm()

emmeans(MatTime2, specs = "Treatment") |> pwpm()
emmeans(MatMass2, specs = "Treatment") |> pwpm()
emmeans(MatRate2, specs = "Treatment") |> pwpm()




#####Sex Specific Data#####

#### Data managing ####

Phase1FData <- phase1F %>% 
  separate(Group, c("tmp1", "Date", "Treatment")) %>% ##This separates group (replicates) into treatment and date (where date is replicates and treatment is experimental grouping)
  mutate(Treatment = factor(x = Treatment, levels = c("Sham", "IR"))) #comparing to sham

Phase1MData <- phase1M %>% 
  separate(Group, c("tmp1", "Date", "Treatment")) %>% ##This separates group (replicates) into treatment and date (where date is replicates and treatment is experimental grouping)
  mutate(Treatment = factor(x = Treatment, levels = c("Sham", "IR"))) #comparing to sham

Phase2FData <- phase2F %>% 
  mutate(Treatment = factor(Treatment,levels = c("sham", "bystander", "new")))

Phase2MData <- phase2M %>% 
  mutate(Treatment = factor(Treatment,levels = c("sham", "bystander", "new")))


# By sex
#### Controlling for reps ####

#Phase 1 Male and Female
MatTime1F <- lmer(Time ~  Treatment +  (1 | Date), data = Phase1FData)
MatMass1F <- lmer(Weight ~  Treatment +  (1 | Date), data = Phase1FData)
MatRate1F <- lmer(GR ~  Treatment +  (1 | Date), data = Phase1FData)

MatTime1M <- lmer(Time ~  Treatment +  (1 | Date), data = Phase1MData)
MatMass1M <- lmer(Weight ~  Treatment +  (1 | Date), data = Phase1MData)
MatRate1M <- lmer(GR ~  Treatment +  (1 | Date), data = Phase1MData)

#Phase 2 Male and Female

MatTime2F <- lmer(Time ~  Treatment +  (1 | Date), data = Phase2FData)
MatMass2F <- lmer(Weight ~  Treatment +  (1 | Date), data = Phase2FData)
MatRate2F <- lmer(GR ~  Treatment +  (1 | Date), data = Phase2FData)

MatTime2M <- lmer(Time ~  Treatment +  (1 | Date), data = Phase2MData)
MatMass2M <- lmer(Weight ~  Treatment +  (1 | Date), data = Phase2MData)
MatRate2M <- lmer(GR ~  Treatment +  (1 | Date), data = Phase2MData)

#### Confidence interval ####

confint.merMod(MatTime1F)
confint.merMod(MatMass1F)
confint.merMod(MatRate1F)
emmeans(MatTime1F, specs = "Treatment") |> pwpm()
emmeans(MatMass1F, specs = "Treatment") |> pwpm()
emmeans(MatRate1F, specs = "Treatment") |> pwpm()

confint.merMod(MatTime1M)
confint.merMod(MatMass1M)
confint.merMod(MatRate1M)
emmeans(MatTime1M, specs = "Treatment") |> pwpm()
emmeans(MatMass1M, specs = "Treatment") |> pwpm()
emmeans(MatRate1M, specs = "Treatment") |> pwpm()

confint.merMod(MatTime2F)
confint.merMod(MatMass2F)
confint.merMod(MatRate2F)
emmeans(MatTime2F, specs = "Treatment") |> pwpm()
emmeans(MatMass2F, specs = "Treatment") |> pwpm()
emmeans(MatRate2F, specs = "Treatment") |> pwpm()

confint.merMod(MatTime2M)
confint.merMod(MatMass2M)
confint.merMod(MatRate2M)
emmeans(MatTime2M, specs = "Treatment") |> pwpm()
emmeans(MatMass2M, specs = "Treatment") |> pwpm()
emmeans(MatRate2M, specs = "Treatment") |> pwpm()

#### Plots ####
#P1F
time1F <- sjPlot::plot_model(MatTime1F, show.values = T, show.p = T, title = "Phase 1 Female Time to Maturation (day)",
                             vline.color = "#061423",
                             axis.labels = c("0.5 Gy Compared to Sham"),
                             axis.lim = c(-1, 1)) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

mass1F <- sjPlot::plot_model(MatMass1F, show.values = T, show.p = T, title = "Phase 1 Female Mass at Maturation (mg)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham"
                               )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rate1F <- sjPlot::plot_model(MatRate1F, show.values = T, show.p = T, title = "Phase 1 Female Average Growth Rate (mg/day)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham"
                               )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(time1F, mass1F, rate1F, ncol = 1,labels = "auto")

#P1M
time1M <- sjPlot::plot_model(MatTime1M, show.values = T, show.p = T, title = "Phase 1 Male Time to Maturation (day)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

mass1M <- sjPlot::plot_model(MatMass1M, show.values = T, show.p = T, title = "Phase 1 Male Mass at Maturation (mg)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rate1M <- sjPlot::plot_model(MatRate1M, show.values = T, show.p = T, title = "Phase 1 Male Average Growth Rate (mg/day)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(time1M, mass1M, rate1M, ncol = 1,labels = "auto")
ggarrange(time1F, time1M, mass1F, mass1M, rate1F, rate1M, ncol = 2, nrow= 3,labels = "auto")

#P2F
time2F <- sjPlot::plot_model(MatTime2F, show.values = T, show.p = T, title = "Phase 2 Female Time to Maturation (day)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

mass2F <- sjPlot::plot_model(MatMass2F, show.values = T, show.p = T, title = "Phase 2 Female Mass at Maturation (mg)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rate2F <- sjPlot::plot_model(MatRate2F, show.values = T, show.p = T, title = "Phase 2 Female Average Growth Rate (mg/day)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(time2F, mass2F, rate2F, ncol = 1,labels = "auto")

#P2M
time2M <- sjPlot::plot_model(MatTime2M, show.values = T, show.p = T, title = "Phase 2 Male Time to Maturation (day)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

mass2M <- sjPlot::plot_model(MatMass2M, show.values = T, show.p = T, title = "Phase 2 Male Mass at Maturation (mg)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rate2M <- sjPlot::plot_model(MatRate2M, show.values = T, show.p = T, title = "Phase 2 Male Average Growth Rate (mg/day)",
                             vline.color = "#061423",
                             axis.labels = 
                               c("0.5 Gy Compared to Sham", "Bystander Compared to Sham"
                               ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(time2M, mass2M, rate2M, ncol = 1,labels = "auto")

ggarrange(time2, time2F, time2M, ncol = 1, labels = "auto")
ggarrange(mass2, mass2F, mass2M, ncol = 1, labels = "auto")
ggarrange(rate2, rate2F, rate2M, ncol = 1, labels = "auto")


######comparing to new instead of sham#####

#### data management ####
NPhase2Data <- phase2 %>% 
  mutate(Treatment = factor(Treatment,levels = c("new", "bystander", "sham"))) %>%
  mutate(Sex = factor(Sex, levels = c("F", "M"))) 

NPhase2FData <- phase2F %>% 
  mutate(Treatment = factor(Treatment,levels = c("new", "bystander", "sham")))

NPhase2MData <- phase2M %>% 
  mutate(Treatment = factor(Treatment,levels = c("new", "bystander", "sham")))

NMatTime2 <- lmer(Time ~  Treatment + Sex+ (1 | Date), data = NPhase2Data)
NMatMass2 <- lmer(Weight ~  Treatment + Sex+ (1 | Date), data = NPhase2Data)
NMatRate2 <- lmer(Growth.Rate ~  Treatment + Sex+ (1 | Date), data = NPhase2Data)

NMatTime2F <- lmer(Time ~  Treatment +  (1 | Date), data = NPhase2FData)
NMatMass2F <- lmer(Weight ~  Treatment +  (1 | Date), data = NPhase2FData)
NMatRate2F <- lmer(GR ~  Treatment +  (1 | Date), data = NPhase2FData)

NMatTime2M <- lmer(Time ~  Treatment +  (1 | Date), data = NPhase2MData)
NMatMass2M <- lmer(Weight ~  Treatment +  (1 | Date), data = NPhase2MData)
NMatRate2M <- lmer(GR ~  Treatment +  (1 | Date), data = NPhase2MData)

#### confidence interv ####
confint.merMod(NMatTime2)
confint.merMod(NMatMass2)
confint.merMod(NMatRate2)

emmeans(NMatTime2, specs = "Treatment") |> pwpm()
emmeans(NMatMass2, specs = "Treatment") |> pwpm()
emmeans(NMatRate2, specs = "Treatment") |> pwpm()

confint.merMod(NMatTime2F)
confint.merMod(NMatMass2F)
confint.merMod(NMatRate2F)

emmeans(NMatTime2F, specs = "Treatment") |> pwpm()
emmeans(NMatMass2F, specs = "Treatment") |> pwpm()
emmeans(NMatRate2F, specs = "Treatment") |> pwpm()

confint.merMod(NMatTime2M)
confint.merMod(NMatMass2M)
confint.merMod(NMatRate2M)

emmeans(NMatTime2M, specs = "Treatment") |> pwpm()
emmeans(NMatMass2M, specs = "Treatment") |> pwpm()
emmeans(NMatRate2M, specs = "Treatment") |> pwpm()
