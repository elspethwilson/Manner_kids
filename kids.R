library(ggplot2)
library(gridExtra)
library(gtools)
library(lme4)
require(optimx)
source(system.file("utils", "allFit.R", package = "lme4"))

# import kids data

kids <- read.csv("manner_final_V2_kids2.csv")

names(kids)[names(kids)=="ï..id"] <- "id"


kids$Type <- relevel(kids$Type, "M")
kids$Agegroup <- "child"
kids$Agegroup <- as.factor(kids$Agegroup)

# create adult and kids dataframe 
final_V2_all_mono$Agegroup <- "adult"
final_V2_all_mono$Agegroup <- as.factor(final_V2_all_mono$Agegroup)
final_V2_all_mono$Type <- as.factor(final_V2_all_mono$Type)
final_V2_all_mono$Type <- relevel(final_V2_all_mono, "M")

all <-  smartbind(kids, final_V2_all_mono)

# graph data

kids_condition <- aggregate(M_selection ~ Type, data = kids, FUN = mean)
kids_condition


kids_items <- aggregate(M_selection ~ Type + Item_no, data = kids, FUN = mean)
kids_items 
write.csv(kids_items, "kids_items.csv")

# by item
kids_item_plot <- ggplot(kids, aes(Item_no, M_selection, fill = Type))
kids_items_plot_grid <- kids_item_plot + geom_bar(position = "dodge", stat= "summary", fun.y = "mean") + 
  ylim(0:1) + xlab("Item") + ylab ("Rate M picture selection") +
  ggtitle("Rate of M picture selection") + xlim("1", "2", "3", "4", "5", "6", "7", "8") + 
  ggsave("kids_items.png")


kids_items_plot_grid2 <- arrangeGrob(kids_items_plot_grid, final_V2_just_table_key)
ggsave(filename = "kids_item_table.png", kids_items_plot_grid2, width = 7, height = 12)

# by condition only 
kids_condition <- ggplot(kids, aes(Type, M_selection))
kids_condition + geom_bar(stat = "summary", fun.y = "mean") + 
  ylim(0:1) + ylab("Rate of selection of M picture") + xlab("Condition - utterance") + 
  ggtitle("Rate of selection of M picture across condition") +
  ggsave("kids_condition.png", width = 4, height = 4)


# graphs individual responses 

kids_id <- ggplot(kids, aes(id, M_selection, fill = Type))
kids_id + geom_bar(stat = "summary", fun.y = "mean", position = "dodge") + 
  ggtitle("Selection of M picture by participant and condition") + ylab ("Rate of M picture selection")

kids_id_table <- aggregate(M_selection ~ id + Type, kids, sum)

kids_hist <- ggplot(subset(kids_id_table, kids_id_table$Type=="M"), aes(M_selection))
kids_hist + geom_histogram(bins = 5) + ggtitle("Histogram of selection of M picture for M utterance") + 
  xlab("Number of M pictures selected/4") + ggsave("kids_hist.png")

kids_hist_I <- ggplot(subset(kids_id_table, kids_id_table$Type=="I"), aes(M_selection))
kids_hist_I + geom_histogram(bins = 5) + ggtitle("Histogram of selection of M picture for I utterance") +
  xlab("Number of I pictures selected/4")+ ggsave("Hist_kids.png")


# adults and kids 

all_plot <- ggplot(all, aes(Type, M_selection, fill=Agegroup))
all_plot + geom_bar(stat = "summary", fun.y = "mean", position = "dodge")+ 
  ylim(0:1) + ylab("Rate of selection of M picture") + xlab("Condition - utterance") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(width = .9), width = .2) + 
  ggsave("all.png", width = 4, height = 4)

# add in GCI vs PCI 

# collapse type 1 and 2 (PCI vs GCI)

type_converter_PCI <- function(x) if(x %in% c("3", "6", "8")) "G" else "P"

all$Type_CI <- sapply(all$Item_no, type_converter_PCI)

all_plot_GCI <- ggplot(all, aes(Type_CI, M_selection, fill=Type))
all_plot_GCI + geom_bar(stat = "summary", fun.y = "mean", position = "dodge")+ 
  ylim(0:1) + ylab("Rate of selection of M picture") + xlab("Condition - utterance") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(width = .9), width = .2) +
  facet_wrap(~Agegroup) + ggsave("all_PCI.png", width = 4, height = 4)

# by item 


all_item <- ggplot(all, aes(Item_no, M_selection, fill = Type_CI))
all_item_plot <-  all_item + geom_bar(position = "dodge", stat= "summary", fun.y = "mean") + 
  ylim(0:1) + xlab("Item") + ylab ("Rate M picture selection") +
  ggtitle("Rate of M picture selection") + facet_wrap(Type ~ Agegroup) + scale_fill_brewer(palette = "Set2", name = "Type") +
  xlim("1", "2", "3", "4", "5", "6", "7", "8") 

all_item_plot

all_item_plot_key <- arrangeGrob(all_item_plot, final_V2_just_table_key)
ggsave(filename = "all_item_table.png", all_item_plot_key, width = 7, height = 12)


# means by type and agegroup

aggregate(M_selection ~ Type*Type_CI + Agegroup, all, mean)
aggregate(M_selection ~ Type + Agegroup, all, mean)


# analysis 
# set contrasts as dummy

all$Agegroup <- as.factor(all$Agegroup)
all$Agegroup <- relevel(all$Agegroup, "child")
all$Type <- as.factor(all$Type)
all$Type <- relevel(all$Type, "M")
all$Type_CI <- as.factor(all$Type_CI)
all$Type_CI <- relevel(all$Type_CI, "P")
all$Item_no_dum <- as.factor(all$Item_no)


by_item <- glmer(M_selection ~ Type * Type_CI * Agegroup + (1+ Type|Item_no_dum), family = "binomial", optimizer = "bobyqa", control = glmerControl(optCtrl = list(maxfun = 150000)),data = all)
by_item_all <- allFit(by_item)

by_item_anlaysis <- capture.output(summary(by_item_all$bobyqa))
write(by_item_anlaysis, "by_item_analysis.txt")
