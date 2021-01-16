# install.packages("lmerTest", "tidyverse", "gridExtra", "ggsignif", "FSA", "MASS", "ggpubr", "dplyr")

library(lmerTest); library(tidyverse); library(ggplot2); library(gridExtra); library(ggsignif); library(FSA); library(lme4);
library(FactoMineR); library(psych); library(GPArotation); library(lattice); library(MASS); library(gridExtra); library(vcd);
library(qpcR)

Tsimane_study1 <- read.csv("Rosenfieldetal_Study1.csv")

# change some variables from factors to numbers

cols.num <- c(5:12)

Tsimane_study1[cols.num] <- sapply(sapply(Tsimane_study1[cols.num],as.character),as.numeric); rm(cols.num)

# make male, female, prestige, dominance, feminized, and masculinized dataframes with vertical data

Tsimane_study1.vert <- Tsimane_study1 %>%
  tidyr::gather(key = condition, value = num_chosen, -ID, -sex, -age, -village) %>%
  tidyr::separate(condition, into = c("char", "manip"), sep="_")

Tsimane_study1_women <- Tsimane_study1.vert %>%
  filter (is.na(num_chosen) == F) %>%
  filter (sex == 0) %>% 
  dplyr::select (ID, char, manip, num_chosen)

Tsimane_study1_men <- Tsimane_study1.vert %>%
  filter (is.na(num_chosen) == F) %>%
  filter (sex == 1) %>%  
  dplyr::select (ID, char, manip, num_chosen)

# run repeated measures ANOVAs to see if characteristic or maniplation have an effect on number chosen

anova(lmer(num_chosen ~ char*manip + (1|ID), data=Tsimane_study1_women))

##### now assuming Poisson distribution FOR ROBUSTNESS CHECK

aov_women_pois <- glmer(num_chosen ~ char*manip + (1|ID), family = poisson, data=Tsimane_study1_women)
summary(aov_women_pois)
anova(aov_women_pois)

##### results do not differ

## women's responses do not differ whether they are indicating short- or long-term attractiveness or depending on type of manipulation

anova(lmer(num_chosen ~ char*manip + (1|ID), data=Tsimane_study1_men))

##### now assuming Poisson distribution FOR ROBUSTNESS CHECK

aov_men_pois <- glmer(num_chosen ~ char*manip + (1|ID), family = poisson, data=Tsimane_study1_men)
summary(aov_men_pois)
anova(aov_men_pois)

aov_men_nb <- glmer.nb(num_chosen ~ char*manip + (1|ID), data=Tsimane_study1_men)
summary(aov_men_nb)
anova(aov_men_nb)

##### results do not differ

## men's responses do not differ depending on type of manipulation but DO differ whether they are rating dominance or prestige

par(mfrow=c(2,4))
hist(Tsimane_study1$att_fem)
hist(Tsimane_study1$att_mas)
hist(Tsimane_study1$mar_fem)
hist(Tsimane_study1$mar_mas)
hist(Tsimane_study1$dom_fem)
hist(Tsimane_study1$dom_mas)
hist(Tsimane_study1$pre_fem)
hist(Tsimane_study1$pre_mas)

Summarize(num_chosen ~ manip + sex, data = Tsimane_study1.vert)
Summarize(num_chosen ~ char + sex, data = Tsimane_study1.vert)

1.181286/78^0.5
1.175847/78^0.5
1.252902/78^0.5
1.100737/78^0.5

1.397385/90^0.5
1.357873/90^0.5
1.254455/90^0.5
1.428662/90^0.5

# graph of men's and women's number of masculiune stimuli chosen under 4 conditions

Tsimane_study1_men_plot.data <- Tsimane_study1 %>%
  tidyr::gather(key = condition, value = num_chosen, -ID, -sex, -age, -village) %>%
  filter (sex == 1) %>%  
  filter (is.na(num_chosen) == F) %>%
  dplyr::select (ID, condition, num_chosen)

Tsimane_study1_women_plot.data <- Tsimane_study1 %>%
  tidyr::gather(key = condition, value = num_chosen, -ID, -sex, -age, -village) %>%
  filter (sex == 0) %>%  
  filter (is.na(num_chosen) == F) %>%
  dplyr::select (ID, condition, num_chosen)

men_conditions <- ggplot(Tsimane_study1_men_plot.data, aes(condition, num_chosen)) +
  theme(axis.title.y = element_text(hjust = 5)) +
  ylim(-0.2, 5.7) +
  labs(title = "Numbers of more masculine stimuli chosen, by condition\n",
       subtitle = "Men", x="", y="") +
  scale_x_discrete(breaks=c("dom_fem", "dom_mas", "pre_fem", "pre_mas"),
                   labels=c("Dominance,\nfeminized", "Dominance,\nmasculinized",
                            "Prestige,\nfeminized", "Prestige,\nmasculinized")) +
  geom_point() +
  geom_violin(aes(fill = condition)) +
  stat_summary(fun.y=mean, geom="point", size=5, color="white") +
  #  geom_crossbar(stat="summary", fun.y=median, fun.ymax=median, fun.ymin=median, fatten=2, width=.5) +
  geom_line(data = data.frame(x = c(0,4.5), y = c(2,2)),
            aes(x = x, y = y), colour = "red", linetype="dashed", size = 1.3) +
  #  geom_boxplot(width=0.15) +
  geom_jitter(height = 0.1, width = .15, color = 'black') +
  scale_fill_brewer(palette="Pastel1") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.3, 0.3, 0, 0.3, "cm"),
        legend.position="none")  +
  geom_signif(y_position=c(4.45), xmin=c(1.5), xmax=c(3.5),
              annotation=c("p < 0.001"), vjust = -0.7, hjust = 0.5, tip_length = 0.035) +
  geom_signif(y_position=c(4.3, 4.3), xmin=c(1, 3), xmax=c(2, 4),
              annotation=c("", ""), tip_length = 0.03)
#  geom_signif(y_position=c(-0.6), xmin=c(1.5), xmax=c(3.5),
#            annotation=c("ns"), vjust = 6, hjust = 2.75, tip_length = c(-0.025, -.073)) +
#  geom_signif(y_position=c(-0.5, -0.3), xmin=c(1, 2), xmax=c(3, 4),
#              annotation=c("", ""), tip_length = -0.03)

women_conditions <- ggplot(Tsimane_study1_women_plot.data, aes(condition, num_chosen)) +
  theme(axis.title.y = element_text(hjust = 5)) +
  ylim(-0.2, 5.7) +
  labs(subtitle = "Women", x="", y="") +
  scale_x_discrete(breaks=c("att_fem", "att_mas", "mar_fem", "mar_mas"),
                   labels=c("Attractiveness,\nfeminized", "Attractiveness,\nmasculinized",
                            "Marraige,\nfeminized", "Marraige,\nmasculinized")) +
  geom_point() +
  geom_violin(aes(fill = condition)) +
  stat_summary(fun.y=mean, geom="point", size=5, color="white") +
  #  geom_crossbar(stat="summary", fun.y=median, fun.ymax=median, fun.ymin=median, fatten=2, width=.5) +
  geom_line(data = data.frame(x = c(0,4.5), y = c(2,2)),
            aes(x = x, y = y), colour = "red", linetype="dashed", size = 1.3) +
  #  geom_boxplot(width=0.15) +
  geom_jitter(height = 0.1, width = .15, color = 'black') +
  scale_fill_brewer(palette="Pastel1") +
  theme_classic() +
  theme(plot.margin = margin(0.2, 0.3, 0.3, 0.3, "cm"), legend.position="none") +
  geom_signif(y_position=c(4.45), xmin=c(1.5), xmax=c(3.5),
              annotation=c("ns"), vjust = -0.4, hjust = 0.3, tip_length = 0.035) +
  geom_signif(y_position=c(4.3, 4.3), xmin=c(1, 3), xmax=c(2, 4),
              annotation=c("", ""), tip_length = 0.03)
#  geom_signif(y_position=c(-0.6), xmin=c(1.5), xmax=c(3.5),
#            annotation=c("ns"), vjust = 6, hjust = 2.75, tip_length = c(-0.025, -.073)) +
#  geom_signif(y_position=c(-0.5, -0.3), xmin=c(1, 2), xmax=c(3, 4),
#              annotation=c("", ""), tip_length = -0.03)

grid.arrange(men_conditions, women_conditions, nrow=2, left = "Condition")

######################################

# make women's mean number chosen, men's mean for dominance, and men's mean for prestige dataframes

Tsimane_study1_women_avg <- Tsimane_study1_women %>%
  group_by (ID) %>%
  summarize(avg_chosen = mean(num_chosen))

Tsimane_study1_men_pre_avg <- Tsimane_study1_men %>%
  filter (is.na(num_chosen) == F) %>%
  filter (char == "pre") %>%
  group_by (ID, char) %>%
  summarize(avg_chosen = mean(num_chosen))

Tsimane_study1_men_dom_avg <- Tsimane_study1_men %>%
  filter (is.na(num_chosen) == F) %>%
  filter (char == "dom") %>%
  group_by (ID, char) %>%
  summarize(avg_chosen = mean(num_chosen))

# t-tests for did they select more masculine than by chance

t.test(Tsimane_study1_women_avg$avg_chosen, mu=2, paired=FALSE, no.action=na.omit)

t.test(Tsimane_study1_men_pre_avg$avg_chosen, mu=2, paired=FALSE, no.action=na.omit)

t.test(Tsimane_study1_men_dom_avg$avg_chosen, mu=2, paired=FALSE, no.action=na.omit)

Summarize(Tsimane_study1_women_avg$avg_chosen)
Summarize(Tsimane_study1_men_pre_avg$avg_chosen)
Summarize(Tsimane_study1_men_dom_avg$avg_chosen)

# SEMs

0.6857303/39^0.5
1.306259/45^0.5
1.115773/45^0.5

# t-tests as above but with different data treatments

## all conditions seperately
t.test(subset(Tsimane_study1, sex = 0, select = c("att_fem")), mu=2, paired=FALSE, no.action=na.omit)
t.test(subset(Tsimane_study1, sex = 0, select = c("att_mas")), mu=2, paired=FALSE, no.action=na.omit)
t.test(subset(Tsimane_study1, sex = 0, select = c("mar_fem")), mu=2, paired=FALSE, no.action=na.omit)
t.test(subset(Tsimane_study1, sex = 0, select = c("mar_mas")), mu=2, paired=FALSE, no.action=na.omit)

t.test(subset(Tsimane_study1, sex = 1, select = c("dom_fem")), mu=2, paired=FALSE, no.action=na.omit)
t.test(subset(Tsimane_study1, sex = 1, select = c("dom_mas")), mu=2, paired=FALSE, no.action=na.omit)
t.test(subset(Tsimane_study1, sex = 1, select = c("pre_fem")), mu=2, paired=FALSE, no.action=na.omit)
t.test(subset(Tsimane_study1, sex = 1, select = c("pre_mas")), mu=2, paired=FALSE, no.action=na.omit)

## women with body and marraige attractiveness seperate

Tsimane_study1_women_att_avg <- Tsimane_study1_women %>%
  filter (is.na(num_chosen) == F) %>%
  filter (char == "att") %>%
  group_by (ID, char) %>%
  summarize(avg_chosen = mean(num_chosen))

Tsimane_study1_women_mar_avg <- Tsimane_study1_women %>%
  filter (is.na(num_chosen) == F) %>%
  filter (char == "mar") %>%
  group_by (ID, char) %>%
  summarize(avg_chosen = mean(num_chosen))

t.test(Tsimane_study1_women_att_avg$avg_chosen, mu=2, paired=FALSE, no.action=na.omit)
t.test(Tsimane_study1_women_mar_avg$avg_chosen, mu=2, paired=FALSE, no.action=na.omit)

# Figure 2

Tsimane_study1_women_avg_dummy <- rbind(Tsimane_study1_women_avg[,2], NA, NA, NA, NA, NA, NA)

figure_2.data <- cbind(c(1:45), Tsimane_study1_women_avg_dummy[,1], Tsimane_study1_men_dom_avg[,3],
                       Tsimane_study1_men_pre_avg[,3])

colnames(figure_2.data) <-  c("ID", "women", "men_dom", "men_pre")
figure_2.data$ID <- as.factor(figure_2.data$ID)

figure_2.data.vert <- figure_2.data %>%
  tidyr::gather(key = group, value = avg_chosen, -ID)

ggplot(figure_2.data.vert, aes(group, avg_chosen)) +
  theme(axis.title.y = element_text(hjust = 5)) +
  labs(x="Group", y="Average number chosen") +
  scale_x_discrete(breaks=c("women", "men_dom", "men_pre"),
                   labels=c("Women", "Men,\ndominance", "Men,\nprestige")) +
  geom_point() +
  geom_violin(aes(fill = group)) +
  stat_summary(fun.y = mean, geom="point", color = "white", size = 5) +
  #  geom_crossbar(stat="summary", fun.y=median, fun.ymax=median, fun.ymin=median, fatten=2, width=.5) +
  geom_line(data = data.frame(x = c(0,4.5), y = c(2,2)),
            aes(x = x, y = y), colour = "red", linetype="dashed", size = 1.3) +
  #  geom_boxplot(width=0.15) +
  geom_jitter(height = 0.05, width = .15, color = 'black') +
  scale_fill_brewer(palette="Pastel1") +
  theme_classic() +
  theme(plot.margin = margin(0.2, 0.3, 0.3, 0.3, "cm"), legend.position="none")

##### non-parametric 1-sample FOR ROBUSTNESS CHECK

wilcox.test(Tsimane_study1_women_avg$avg_chosen, mu = 2, alternative = "two.sided")

wilcox.test(Tsimane_study1_men_pre_avg$avg_chosen, mu = 2, alternative = "two.sided")

wilcox.test(Tsimane_study1_men_dom_avg$avg_chosen, mu = 2, alternative = "two.sided")

####

##### make men's overall mean number chosen FOR ROBUSTNESS CHECK

Tsimane_study1_men_avg <- Tsimane_study1_men %>%
  group_by (ID) %>%
  summarize(avg_chosen = mean(num_chosen))

t.test(Tsimane_study1_men_avg$avg_chosen, mu=2, paired=FALSE, no.action=na.omit)

wilcox.test(Tsimane_study1_men_avg$avg_chosen, mu = 2, alternative = "two.sided")

#####

# did subjects choose manipulated or unmanipulated voice at a higher rate than chance?

Tsimane_study1_men_manip <- Tsimane_study1_men %>%
  mutate(manip_chosen = 
           case_when(
           manip == "fem" ~ 4 - num_chosen,
           manip == "mas" ~ num_chosen
         )) %>%
  group_by(ID) %>%
  summarize(avg_manip = mean(manip_chosen))

t.test(Tsimane_study1_men_manip$avg_manip, mu=2, paired=FALSE, no.action=na.omit)

wilcox.test(Tsimane_study1_men_manip$avg_manip, mu=2, paired=FALSE, alternative = "two.sided")

Summarize(Tsimane_study1_men_manip$avg_manip); 0.372932/45^0.5

Tsimane_study1_women_manip <- Tsimane_study1_women %>%
  mutate(manip_chosen = 
           case_when(
             manip == "fem" ~ 4 - num_chosen,
             manip == "mas" ~ num_chosen
           )) %>%
  group_by(ID) %>%
  summarize(avg_manip = mean(manip_chosen))

t.test(Tsimane_study1_women_manip$avg_manip, mu=2, paired=FALSE, no.action=na.omit)

wilcox.test(Tsimane_study1_women_manip$avg_manip, mu=2, paired=FALSE, alternative = "two.sided")

Summarize(Tsimane_study1_women_manip$avg_manip); 0.4511382/39^0.5

mean(residuals(lm(log_kids~ meanF0 + Size + Wealth + poly(age,2), data=Tsimane_study2, na.action = na.omit))) - sd(residuals(lm(log_kids~ meanF0 + Size + Wealth + poly(age,2), data=Tsimane_study2, na.action = na.omit)))*3

mean(residuals(lm(log_kids~ meanF0 + Size + Wealth + poly(age,2), data=Tsimane_study2, na.action = na.omit))) + sd(residuals(lm(log_kids~ meanF0 + Size + Wealth + poly(age,2), data=Tsimane_study2, na.action = na.omit)))*3

sort(residuals(lm(log_kids~ meanF0 + Size + Wealth + poly(age,2), data=Tsimane_study2, na.action = na.omit)))

citation("lmerTest")
citation("tidyverse")
citation("ggplot2");
citation("ggsignif") # brackets
citation("FSA") # summarize function
citation("ggpubr") # 1 sample wilcox
citation("MASS") # glm.nb
citation("vcd") # goodfit
citation("FactoMineR")
citation("GPArotation")
citation("gridExtra")
citation("lattice")
citation("lme4")
citation("qpcR")
citation("psych")
citation("QuantPsyc")




