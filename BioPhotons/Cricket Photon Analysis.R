setwd("C:/Users/mrthy/Desktop/Uni boiiiiis/PhD/Papers to Submit/Bystander Photon Experiment/Raw data")
###### Libraries ######
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


##### getting data #####
Phodata <- read.csv("Maturation Data.csv")
FilmPhodata <- read.csv("Maturation Data Film.csv")
BysData <- read.csv("Maturation Data BysFilm.csv")
PhoM <- read.csv("PhoMale.csv")
PhoF <- read.csv("PhoFemale.csv")
FilmPhoM <- read.csv("PhoMaleFilm.csv")
FilmPhoF <- read.csv("PhoFemaleFilm.csv")
BysPhoM <- read.csv("PhoMaleBys.csv")
BysPhoF <- read.csv("PhoFemaleBys.csv")

######population level######

##### controlling effects of replication #####
#looked at effect of treatment and sex on maturation date
#controlled for replication
MatTime <- lmer(time ~  treatment + sex + (1 | replicate), data = Phodata)
MatMass <- lmer(mass ~  treatment + sex + (1 | replicate), data = Phodata)
MatRate <- lmer(rate ~  treatment + sex + (1 | replicate), data = Phodata)

#comparing to the sham film as control instead of sham no film
FilmMatTime <- lmer(time ~  treatment + sex + (1 | replicate), data = FilmPhodata)
FilmMatMass <- lmer(mass ~  treatment + sex + (1 | replicate), data = FilmPhodata)
FilmMatRate <- lmer(rate ~  treatment + sex + (1 | replicate), data = FilmPhodata)

#compared to bystander with film as control
BysMatTime <- lmer(time ~  treatment + sex + (1 | replicate), data = BysData)
BysMatMass <- lmer(mass ~  treatment + sex + (1 | replicate), data = BysData)
BysMatRate <- lmer(rate ~  treatment + sex + (1 | replicate), data = BysData)


#### Confidence intervals ####
confint.merMod(MatTime)
confint.merMod(MatMass)
confint.merMod(MatRate)

confint.merMod(FilmMatTime)
confint.merMod(FilmMatMass)
confint.merMod(FilmMatRate)

confint.merMod(BysMatTime)
confint.merMod(BysMatMass)
confint.merMod(BysMatRate)

####Plot####

time <- sjPlot::plot_model(MatTime, show.values = T, show.p = T, title = "Time to Maturation (day) Compared to Sham - Filter",
                           vline.color = "#061423",
                           axis.labels = 
                             c("Males compared to Females", "Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                             ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

mass <- sjPlot::plot_model(MatMass, show.values = T, show.p = T, title = "Mass at Maturation (mg) Compared to Sham - Filter",
                           vline.color = "#061423",
                           axis.labels = 
                             c("Males compared to Females", "Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                             ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rate <- sjPlot::plot_model(MatRate, show.values = T, show.p = T, title = "Average Growth Rate (mg/day) Compared to Sham - Filter",
                           vline.color = "#061423",
                           axis.labels = 
                             c("Males compared to Females", "Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                             ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(time, mass, rate, ncol = 1,labels = "auto")
#

#### Estimated means####
emmeans(MatTime, specs = "treatment") |> pwpm()
emmeans(MatMass, specs = "treatment") |> pwpm()
emmeans(MatRate, specs = "treatment") |> pwpm()

emmeans(FilmMatTime, specs = "treatment") |> pwpm()
emmeans(FilmMatMass, specs = "treatment") |> pwpm()
emmeans(FilmMatRate, specs = "treatment") |> pwpm()

emmeans(BysMatTime, specs = "treatment") |> pwpm()
emmeans(BysMatMass, specs = "treatment") |> pwpm()
emmeans(BysMatRate, specs = "treatment") |> pwpm()
##### by sex #####

#### rep control ####
MatTimeF <- lmer(time ~  treatment + (1 | replicate), data = PhoF)
MatMassF <- lmer(mass ~  treatment + (1 | replicate), data = PhoF)
MatRateF <- lmer(rate ~  treatment + (1 | replicate), data = PhoF)

MatTimeM <- lmer(time ~  treatment + (1 | replicate), data = PhoM)
MatMassM <- lmer(mass ~  treatment + (1 | replicate), data = PhoM)
MatRateM <- lmer(rate ~  treatment + (1 | replicate), data = PhoM)
#Comparing to film instead of no film as the control
FMatTimeF <- lmer(time ~  treatment + (1 | replicate), data = FilmPhoF)
FMatMassF <- lmer(mass ~  treatment + (1 | replicate), data = FilmPhoF)
FMatRateF <- lmer(rate ~  treatment + (1 | replicate), data = FilmPhoF)

FMatTimeM <- lmer(time ~  treatment + (1 | replicate), data = FilmPhoM)
FMatMassM <- lmer(mass ~  treatment + (1 | replicate), data = FilmPhoM)
FMatRateM <- lmer(rate ~  treatment + (1 | replicate), data = FilmPhoM)
#comaring to bys instead
BMatTimeF <- lmer(time ~  treatment + (1 | replicate), data = BysPhoF)
BMatMassF <- lmer(mass ~  treatment + (1 | replicate), data = BysPhoF)
BMatRateF <- lmer(rate ~  treatment + (1 | replicate), data = BysPhoF)

BMatTimeM <- lmer(time ~  treatment + (1 | replicate), data = BysPhoM)
BMatMassM <- lmer(mass ~  treatment + (1 | replicate), data = BysPhoM)
BMatRateM <- lmer(rate ~  treatment + (1 | replicate), data = BysPhoM)
#### confit ####
confint.merMod(MatTimeF)
emmeans(MatTimeF, specs = "treatment") |> pwpm()
confint.merMod(FMatTimeF)
emmeans(FMatTimeF, specs = "treatment") |> pwpm()
confint.merMod(BMatTimeF)
emmeans(MatTimeF, specs = "treatment") |> pwpm()

confint.merMod(MatMassF)
emmeans(MatMassF, specs = "treatment") |> pwpm()
confint.merMod(FMatMassF)
emmeans(FMatMassF, specs = "treatment") |> pwpm()
confint.merMod(BMatMassF)
emmeans(MatTimeF, specs = "treatment") |> pwpm()

confint.merMod(MatRateF)
emmeans(MatRateF, specs = "treatment") |> pwpm()
confint.merMod(FMatRateF)
emmeans(FMatRateF, specs = "treatment") |> pwpm()
confint.merMod(BMatRateF)
emmeans(MatTimeF, specs = "treatment") |> pwpm()

confint.merMod(MatTimeM)
emmeans(MatTimeM, specs = "treatment") |> pwpm()
confint.merMod(FMatTimeM)
emmeans(FMatTimeM, specs = "treatment") |> pwpm()
confint.merMod(BMatTimeM)
emmeans(MatTimeF, specs = "treatment") |> pwpm()

confint.merMod(MatMassM)
emmeans(MatMassM, specs = "treatment") |> pwpm()
confint.merMod(FMatMassM)
emmeans(FMatMassM, specs = "treatment") |> pwpm()
confint.merMod(BMatMassM)
emmeans(MatTimeF, specs = "treatment") |> pwpm()

confint.merMod(MatRateM)
emmeans(MatRateM, specs = "treatment") |> pwpm()
confint.merMod(FMatRateM)
emmeans(FMatRateM, specs = "treatment") |> pwpm()
confint.merMod(BMatRateM)
emmeans(MatTimeF, specs = "treatment") |> pwpm()







#### plots ####
timeF <- sjPlot::plot_model(MatTimeF, show.values = T, show.p = T, title = "Female Time to Maturation (day) Compared to Sham - Filter",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

massF <- sjPlot::plot_model(MatMassF, show.values = T, show.p = T, title = "Female Mass at Maturation (mg) Compared to Sham - Filter",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rateF <- sjPlot::plot_model(MatRateF, show.values = T, show.p = T, title = "Female Average Growth Rate (mg/day) Compared to Sham - Filter",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(timeF, massF, rateF, ncol = 1,labels = "auto")



timeM <- sjPlot::plot_model(MatTimeM, show.values = T, show.p = T, title = "Male Time to Maturation (day) Compared to Sham - Filter",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

massM <- sjPlot::plot_model(MatMassM, show.values = T, show.p = T, title = "Male Mass at Maturation (mg) Compared to Sham - Filter",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rateM <- sjPlot::plot_model(MatRateM, show.values = T, show.p = T, title = "Male Average Growth Rate (mg/day) Compared to Sham - Filter",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Sham + Filter", "Bystander - Filter", "Bystander + Filter"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(timeM, massM, rateM, ncol = 1,labels = "auto")

ggarrange(time, timeF, timeM, ncol = 1, labels = "auto", align = c("hv"))
ggarrange(mass, massF, massM, ncol = 1, labels = "auto", align = c("hv"))
ggarrange(rate, rateF, rateM, ncol = 1, labels = "auto", align = c("hv"))


#compared to film sham

FtimeF <- sjPlot::plot_model(FMatTimeF, show.values = T, show.p = T, title = "Female Time to Maturation (day)"
)+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

FmassF <- sjPlot::plot_model(FMatMassF, show.values = T, show.p = T, title = "Female Mass at Maturation (mg)"
)+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

FrateF <- sjPlot::plot_model(FMatRateF, show.values = T, show.p = T, title = "Female Average Growth Rate (mg/day)",
)+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(FtimeF, FmassF, FrateF, ncol = 1,labels = "auto")

FtimeM <- sjPlot::plot_model(FMatTimeM, show.values = T, show.p = T, title = "Male Time to Maturation (day)"
)+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

FmassM <- sjPlot::plot_model(FMatMassM, show.values = T, show.p = T, title = "Male Mass at Maturation (mg)"
)+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

FrateM <- sjPlot::plot_model(FMatRateM, show.values = T, show.p = T, title = "Male Average Growth Rate (mg/day)",
)+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(FtimeM, FmassM, FrateM, ncol = 1,labels = "auto")
