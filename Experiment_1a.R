library(ggplot2)
library(gridExtra)
library(reshape)
library(plyr)
library(lme4)
require(optimx)
source(system.file("utils", "allFit.R", package = "lme4"))

# import child and adult picture-matching data
kids <- read.csv("manner-kids_extra.csv")
adults <- read.csv("manner-adultsk.csv")
# import child information 
info <- read.csv("questionnaire_trog_bpvs2.csv")

# merge files 

kids_d <- merge(kids, info, by.x = "Participant")

# make sure columns are the same for both 

colnames(adults)[1] <- "Participant"
colnames(adults)[12] <- "MMResponse"
colnames(adults)[13] <- "Response2"
colnames(adults)[3] <- "ItemNo"
colnames(adults)[11] <- "Item"
colnames(kids_d)[11] <- "Mresponse"
colnames(adults)[c(6,7)] <- c("Type2", "Type")
colnames(kids_d)[c(20,21)] <- c("Education1", "Education2")
colnames(adults)[5] <- "OrderNo"
colnames(kids_d)[26] <- "BPVS_standard"
adults$COB<- NA
adults$learning.language<- NA
adults$Car<- NA
adults$holiday<- NA
adults$computer<- NA
adults$bedroom<- NA
adults$Education1<- NA
adults$Education2<- NA
adults$SES<- NA
adults$Other_langs<- NA
adults$Trog<- NA
adults$BPVS_raw<- NA
adults$BPVS_standard <- NA
adults$Age <- NA
adults$MMResponse <- NULL

adults$agegroup <- "adult"
kids_d$agegroup <- "child"
adults$Participant <- as.factor(adults$Participant)
adults$Mresponse <- as.factor(adults$Mresponse)

d <- rbind(kids_d, adults)


d$ItemNo <- as.factor(d$ItemNo)

# check merge by comparing means from kids and adult datasets and separately 

kids_means_condition <- aggregate(Response2~Type, data = kids_d, FUN = mean)
adults_means_condition <- aggregate(Response2~Type, data=adults, FUN = mean)
means_condition <- aggregate(Response2 ~ Type + agegroup, data = d, FUN = mean)
means_condition
kids_means_condition
adults_means_condition
# all the same 

# monolinguals only 
kids_bi <- read.csv("questionnaire_bilingual.csv")
colnames(kids_bi)[1] <- "Participant"

d_mono <- d[!(d$Participant=="IG10" | d$Participant=="IG16" | d$Participant=="IG20" | d$Participant=="IG22" | d$Participant=="IG28"),]
d_monos <- d_mono[!(d_mono$Participant=="2" | d_mono$Participant=="6" | d_mono$Participant=="9" | d_mono$Participant=="21"),]


means_condition_monos <- aggregate(Response2 ~ Type + agegroup, data = d_monos, FUN = mean)
means_condition_monos


# collapse type 1 and 2 (PCI vs GCI)

type_converter <- function(x) if(x %in% c("M1", "M2")) "M1" else if (x%in% "M3") "M2" else if (x %in% c("I1","I2")) "I1" else "I2"

d_monos$Type3 <- sapply(d_monos$Type, type_converter)

type_converter2 <- function(x) if(x %in% c("M1", "I1")) "P" else "G"

d_monos$Type4 <- sapply(d_monos$Type3, type_converter2)

#plot results
plot_GCI_monos <- ggplot(data = d_monos, aes(Type4, Response2, fill = Type2))
plot_GCI_monos + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + facet_grid(~agegroup) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2, position = position_dodge(width = .9)) +
  ggtitle("Manner picture selection by type and age") + 
  theme(plot.title = element_text(size = 10)) +  xlab("Type") + ylim(0:1) +
  scale_fill_discrete(name = "Condition") +
  ylab("Proportion M picture selection") + ylim(0:1) + ggsave("GCI_PCI_plot_monos.png")


plot_condition2_monos <- ggplot(data = d_monos, aes(Type2, Response2, fill = agegroup))
plot_condition2_monos + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2, position = position_dodge(width = .9)) +
  ggtitle("Manner picture selection by type and age") + xlab("Condition") +
  theme(plot.title = element_text(size = 10)) + 
  ylab("Proportion M picture selection") + ylim(0:1) + ggsave("M_selection_Cond_age_mono.png") 

# mono items
plot_item_monos <- ggplot(data = d_monos, aes(ItemNo, Response2, fill = Type4))
plot_item_age_monos <- plot_item_monos + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  ylab("Proportion M picture selection") + ylim(0:1) + facet_wrap(Type2~agegroup) + 
  theme(plot.title = element_text(size = 10)) + ggtitle("Proportion M picture selection by item, Condition and agegroup") + 
  scale_fill_discrete(name="Type") +
  ggsave("item_responses_age_monos.png")

plot_item_age_monos


Rate_M_SelectionmonosW <- arrangeGrob(grobs = list(plot_item_age_monos, table_key), ncol = 2)
ggsave(filename = "Rate_M_SelectionmonosW.png", Rate_M_SelectionmonosW, height = 7, width = 13)


# participants
plot_subj_monos <- ggplot(data = d_monos, aes(Type, Response2, fill=agegroup))
plot_subj_monos + stat_summary(fun.y = mean, geom = "bar") +  ylim(0:1)+
  facet_wrap(~Participant) + ggsave("Participants_subtype.png")

participant_ms <- aggregate(Response2 ~ Participant + Type, data = d_monos, FUN = mean)
write.csv(participant_ms, "Participant_Ms.csv")


Fl_Ceiling <- function (x) if(x == 1|x==0) "true" else "false"
participant_ms$fl_ceil <- sapply(participant_ms$Response2, Fl_Ceiling)

Passer <- function (x) if(x==1) "true" else "false"
participant_ms$Pass <- sapply(participant_ms$Response2, Passer)

###########################

#ANALYSIS#

# dummy coding for simple effect - to compare M with I 
#set baseline as child, M, PCI

d_monos_dum <- d_monos
contrasts(d_monos_dum$Type) <- contr.treatment(6)
contrasts(d_monos_dum$Type2) <- contr.treatment(2)
d_monos_dum$Type2 <- factor(d_monos_dum$Type2, levels = c("M","I"))
contrasts(d_monos_dum$Type3) <- contr.treatment(4)
contrasts(d_monos_dum$Type4) <- contr.treatment(2)
d_monos_dum$Type4 <- factor(d_monos_dum$Type4, levels = c("P", "G"))
d_monos_dum$agegroup <- as.factor(d_monos_dum$agegroup)
d_monos_dum$agegroup <- factor(d_monos_dum$agegroup, levels = c("child","adult"))

contrasts(d_monos_dum$agegroup) <- contr.treatment(2)


#age and condition 
mr_subj_dum <- glmer(Response2 ~ Type2 * agegroup + (1+ Type2|Participant), family = "binomial", optimizer = "bobyqa", control = glmerControl(optCtrl = list(maxfun = 150000)),data = d_monos_dum)
mr_item_dum <- glmer(Response2 ~ Type2 * agegroup + (1+ Type2|ItemNo), family = "binomial", optimizer = "bobyqa", control = glmerControl(optCtrl = list(maxfun = 150000)),data = d_monos_dum)


manner_exp1_dum_MI <- capture.output(summary(mr_subj_dum))
write(manner_exp1_dum_MI, "manner_exp1_dum_MI.txt")

summary(mr_item_dum)

mr_item_dum_PCI <- glmer(Response2 ~ Type2 * Type4 * agegroup + (1+ Type2|ItemNo), family = "binomial", optimizer = "bobyqa", control = glmerControl(optCtrl = list(maxfun = 150000)),data = d_monos_dum)
manner_exp1_dum <- capture.output(summary(mr_item_dum_PCI))
write(manner_exp1_dum, "manner_exp1_dum.txt")
