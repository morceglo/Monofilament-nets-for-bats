library(readxl)
Datos <- read_excel("C:/Users/Gloriana/Desktop/Proyecto redes/Análisis/Nets.xlsx")
View(Datos)


#Verify assumptions

dat = lm(Minins_em ~ Net, Datos)

plot(fitted(dat),residuals(dat))^2

hist(residuals(dat))

qqnorm(residuals(dat))


library(ggpubr)

ggqqplot(Datos$Nind_sr)


#Graph of number of individuals by habitat and net type
boxplot(Nind_sr ~ Net*Habitat,
        col=c("white","lightgray"),Datos)

boxplot(Nind_sr ~ Net,
        col=c("white","lightgray"),Datos)

boxplot(Nind_sr ~ Habitat,
        col=c("white","lightgray"),Datos)

boxplot(Nind_sr ~ Block,
        col=c("white","lightgray"),Datos)

boxplot(Nind_sr ~ Day,
        col=c("white","lightgray"),Datos)



library ("lme4")

#Creating null, no interaction (half), and full mixed model to observe the differences in number of individuals per net type and habitat (see page 13 of Winter)


efficiency.null = lmer(Nind_sr ~ Habitat + (1|Day) + (1|Block), data=Datos, REML=FALSE)

efficiency.half = lmer(Nind_sr ~ Net + Habitat + (1|Day) + (1|Block), data=Datos, REML=FALSE)

efficiency.full = lmer(Nind_sr ~ Net*Habitat + (1|Day) + (1|Block), data=Datos, REML=FALSE)




#Testing the models, one with the effect of net*habitat and one without the effect of net

anova (efficiency.null, efficiency.full)


#Testing the models, one with the effect of net + habitat and one without the effect of net

anova (efficiency.null, efficiency.half)


#Testing the models, one with the interaction effect of net*habitat and one without its effect

anova (efficiency.half, efficiency.full)


#Generating coefficients for the best model (no interaction)

efficiency.half


#Violin plot with number of individuals by net type and habitat

library (ggplot2)
library(viridis)
library(tidyverse)
library(ggthemes)
library(dplyr)
library(forcats)
library(hrbrthemes)


q <- ggplot(Datos, aes(x = Net, y = Nind_em)) +
  facet_wrap( ~ Habitat ) + 
  geom_violin (aes(fill = factor(Net)), scale = "area") +
  stat_summary (fun.data = "mean_sdl",  fun.args = list(mult = 1), geom = "pointrange", color = "gray20", size = 0.8) + theme_classic () +
  
  labs( x = "Type of net", y = "Number of bats") + theme(legend.position = "none") +
  scale_fill_viridis(discrete=T, name="", alpha = 0.5)

q

ggsave("Figure1.png", width = 5, height = 4)


#Calculate species accumulation curves using iNext (https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12613)

library(devtools)
library("iNEXT")

MF <- c(26,23,21,13,7,6,5,4,3,3,2,2,2,2,1,1,1,1,1,1)
R <- c(27,17,10,9,8,3,3,3,1,1,1)

Species <- list(Monofilament = MF, Regular = R)

iNEXT(Species, q=0, datatype="abundance")

m <- c(1, 5, 20, 50, 75, 100, 150)

iNEXT(Species, q=0, datatype="abundance", size=m)

out <- iNEXT(Species, q=c(0, 1, 2), datatype="abundance", endpoint=250) 

# Sample-size-based R/E curves, separating plots by "site" 
ggiNEXT(out, type=1, facet.var="site") + theme_bw(base_size=18)

# Sample-size-based R/E curves, separating plots by "order"
ggiNEXT(out, type=1, facet.var="order") + theme_bw(base_size=18)
ggsave("Figure2.png", width = 10, height = 4)

# Sample completeness curve
ggiNEXT(out, type=2) + theme_bw(base_size=18)

# Coverage-based R/E sampling curves
ggiNEXT(out, type=3, facet.var="order") + theme_bw(base_size=18)
ggsave("Figure3.png", width = 10, height = 4)
