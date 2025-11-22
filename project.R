## ----lib, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------
library(mosaic)
library(tidyverse)
library(bayesrules)
library(janitor) 
library(rstan)  
library(bayesplot) 
library(broom.mixed) 
library(rstanarm) 
library(tidybayes)
library(e1071)


## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------
whitewine <- read.csv("winequality-white.csv", sep = ";") %>%
  mutate(Type = "W")

redwine <- read.csv("winequality-red.csv", sep = ";") %>%
  mutate(Type = "R")

wine_correct <- rbind(whitewine, redwine)

glimpse(wine_correct)

#From [UC Irvine](https://archive.ics.uci.edu/dataset/186/wine+quality).

#write_csv(wine_correct, file = "~/MCS-354/Project/wine_correct.csv")

#qcolor_rule <- scale_color_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))



## ----make_group, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------

wine_correct <- wine_correct %>%
  mutate(qgroup = case_when(quality %in% c(3, 4, 5) ~ "Low",
                           quality %in% c(6) ~ "Medium",
                           quality %in% c(7, 8, 9) ~ "High"),
         qgroup = factor(qgroup, levels = c("Low", "Medium", "High")))

wine_correct %>%
  ggplot(aes(x=quality, fill = qgroup))+
  geom_bar()+
  scale_fill_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"),
                    labels = c("Low" = "Low (3-5)", "Medium"="Medium (6)", "High"="High (7-9)"))+
  labs(x="Quality Score", fill="Quality Group")

wine_correct %>%
  count(qgroup)


## ----bargraph, echo=FALSE, include=FALSE-------------------------------------------------------------------------------------------------------------------------
wine_correct %>%
  ggplot(aes(x=quality, color = Type))+
  geom_bar()+
  labs(x="Quality Rating")+
  facet_grid(. ~ Type, labeller = labeller(Type = 
    c("R" = "Red Wines (n = 1599)",
      "W" = "White Wines (n = 4898)")))+
  scale_color_manual(values = c("R" = "#ad336d", "W" = "#f9c58d"),
                     labels=c("Red", "White"))+
  theme_bw()


## ----eda, echo=FALSE, warning=FALSE, include=TRUE, fig.height=2--------------------------------------------------------------------------------------------------
wine_correct %>%
  ggplot(aes(x=quality, y=alcohol, color = Type))+
  geom_point()+
  scale_color_manual(values = c("R" = "#ad336d", "W" = "#f9c58d"),
                     labels=c("Red", "White"))+
  facet_grid(. ~ Type, labeller = labeller(Type = 
    c("R" = "Red Wines (n = 1599)",
      "W" = "White Wines (n = 4898)")))+
  theme_bw()+
  geom_smooth(method = "lm")

wine_correct %>%
  ggplot(aes(x=alcohol))+
  geom_histogram()+
  facet_grid(. ~ qgroup, labeller = labeller(Type = 
    c("R" = "Red Wines (n = 1599)",
      "W" = "White Wines (n = 4898)")))+
  labs(x="ABV")+
  theme_bw()

wine_correct %>%
  ggplot(aes(x=volatile.acidity))+
  geom_histogram()+
  facet_grid(. ~ qgroup, labeller = labeller(Type = 
    c("R" = "Red Wines (n = 1599)",
      "W" = "White Wines (n = 4898)")))+
  labs(x="Volatile Acidity")+
  theme_bw()


## ----following_the_book_1, include=FALSE-------------------------------------------------------------------------------------------------------------------------
set.seed(5)

ten_head_df_low <- sample_n(wine_correct, 10)
# This was primarily here for testing that it worked.
#  filter(qgroup == "Low") %>%

linevalue <- ten_head_df_low %>%
  head(1) %>%
  .$alcohol

dioxvalue <- ten_head_df_low %>%
  head(1) %>%
  .$volatile.acidity

qualityvalue <- ten_head_df_low %>%
  head(1) %>%
  .$quality

qgroupvalue <- ten_head_df_low %>%
  head(1) %>%
  .$qgroup

typevalue <- ten_head_df_low %>%
  head(1) %>%
  .$Type


## ----the_book_as_well, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
#Actual distributions of alcohol level grouped by quality
ggplot(wine_correct, aes(x = alcohol, fill = qgroup)) + 
  geom_density(alpha = 0.7)+
  geom_vline(xintercept = linevalue, linetype = "dashed")+
  labs(fill = "Quality Group"
       , title = paste("Actual density of",ten_head_df_low$qgroup[1],
                       "group, alcohol level =", linevalue),
       x="ABV")+
  scale_fill_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))


## ----wine_stats, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------
# Calculate sample mean and sd for each quality group..
wine_correct %>% 
  group_by(qgroup) %>% 
  summarize(mean = mean(alcohol),
            median = median(alcohol),
            sd = sd(alcohol))


## ----normal-plot, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#..then tune a normal distribution to each group to estimate their points
ggplot(wine_correct, aes(x = alcohol, color = qgroup)) + 
  stat_function(fun = dnorm, args = list(mean = 9.87, sd = 0.84), aes(color = "Low")) +
  stat_function(fun = dnorm, args = list(mean = 10.6, sd = 1.13), aes(color = "Medium")) +
  stat_function(fun = dnorm, args = list(mean = 11.4, sd = 1.22), aes(color = "High")) + 
  geom_vline(xintercept = linevalue, linetype = "dashed")+
  scale_color_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))+
  labs(color = "Quality Group", x="ABV", 
       title=paste("Normal priors for",ten_head_df_low$qgroup[1], "group, alcohol level =", linevalue))



## ----bayes_rule_single_1, include=FALSE--------------------------------------------------------------------------------------------------------------------------

# L(y = Low | ABV = line)
l_low <- dnorm(linevalue, mean = 9.87, sd = 0.842)
#p of Low in total df, f(y)
f_low <- (wine_correct %>%
  filter(qgroup == "Low") %>%
  nrow()) / nrow(wine_correct)

# L(y = Med | ABV = line)
l_med <- dnorm(linevalue, mean = 10.6, sd = 1.13)
#p of Med in total df, f(y)
f_med <- (wine_correct %>%
  filter(qgroup == "Medium") %>%
  nrow()) / nrow(wine_correct)

# L(y = High | ABV = line)
l_hi <- dnorm(linevalue, mean = 11.4, sd = 1.22)
#p of Low in total df, f(y)
f_hi <- (wine_correct %>%
  filter(qgroup == "High") %>%
  nrow()) / nrow(wine_correct)

nc_single <- (f_low * l_low) + (f_med * l_med) + (f_hi * l_hi)

###BAYES RULE###
p_low <- (f_low * l_low)/nc_single
p_med <- (f_med * l_med)/nc_single
p_hi <- (f_hi * l_hi)/nc_single



## ----bayes_rule_single_2-----------------------------------------------------------------------------------------------------------------------------------------
p_low #Probability this wine is in the Low quality group
p_med #Probability this wine is in the Medium quality group
p_hi #Probability this wine is in the High quality group

#..and do they add up to 1?
p_low + p_med + p_hi

#Yes, they do. Compare with the predictions below...


## ----firstmodel, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------
first <- naiveBayes(qgroup ~ alcohol, data = wine_correct)

first_wine = data.frame(alcohol = linevalue)

predict(first, newdata = first_wine, type = "raw")


## ----vol_grph, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------
#Actual distributions of sulfur dioxide level grouped by quality
ggplot(wine_correct, aes(x = volatile.acidity, fill = qgroup)) + 
  geom_density(alpha = 0.7)+
  scale_fill_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))+
  labs(title = "Density of Volatile Acidity", x="Volatile Acidity", fill = "Quality Group")+
  geom_vline(xintercept = dioxvalue, linetype = "dashed")


## ----volmodel, echo=F--------------------------------------------------------------------------------------------------------------------------------------------
volmodel <- naiveBayes(qgroup ~ volatile.acidity, data = wine_correct)

one_wine = data.frame(volatile.acidity = dioxvalue)

predict(volmodel, newdata = one_wine, type = "raw")


## ----actual_picked_observation, echo=FALSE-----------------------------------------------------------------------------------------------------------------------

#Actual distributions of alcohol level grouped by quality
ggplot(wine_correct, aes(x = alcohol, y= volatile.acidity, color = qgroup)) + 
  geom_point()+
  geom_vline(xintercept = linevalue, linetype = "dashed")+
  geom_hline(yintercept = dioxvalue, linetype = "dashed")+
  scale_color_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))+
  labs(title = "ABV and Volatile Acidity Relationship",
       x="ABV", y="Volatile Acidity", color="Quality Group")
  


## ----bayes_oclock, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
twopredmodel <- naiveBayes(qgroup ~ alcohol + volatile.acidity, data = wine_correct)

one_wine = data.frame(alcohol = linevalue, volatile.acidity = dioxvalue)

predict(twopredmodel, newdata = one_wine, type = "raw")


## ----abv_and_type_1, echo=FALSE, fig.height=3--------------------------------------------------------------------------------------------------------------------
#Actual relationship b/w alcohol and quality, colored by wine type
#We don't really need this graph
ggplot(wine_correct, aes(x = alcohol, y=qgroup, color=Type))+
  geom_point()+
  geom_jitter()+
  geom_hline(yintercept = qualityvalue, linetype = "dashed")+
  geom_vline(xintercept = linevalue, linetype = "dashed")+
  scale_color_manual(values = c("R" = "#ad336d", "W" = "#f9c58d"),
                     labels = c("R" = "Red", "W" = "White"))+
  labs(color = "Type of Wine",
       title = paste(ten_head_df_low$qgroup[1], "group,",
                     ten_head_df_low$Type[1], "type wine, ABV of", linevalue),
       x="ABV")




## ----abv_and_type_2, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------
#Actual relationship b/w alcohol and wine type, colored by quality group
ggplot(wine_correct, aes(x = alcohol, y=Type, color=qgroup))+
  geom_point()+
  geom_jitter()+
  geom_vline(xintercept = linevalue, linetype = "dashed")+
  scale_color_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))+
  labs(color = "Quality Group",
       title = paste(ten_head_df_low$qgroup[1], "group,",
                     ten_head_df_low$Type[1], "type wine,",
                     ten_head_df_low$alcohol[1], "ABV"),
       x="ABV")

#Just need to filter this out.
wine_106 <- wine_correct %>%
  filter(alcohol == 10.6)

ggplot(wine_106, aes(x = Type, fill=qgroup))+
  geom_bar(position="fill")+
  scale_fill_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))+
  labs(fill = "Quality Group",
       title = "Proportion of all 10.6% ABV wines",
       x="Type")+
  
  #This text I did have to cobble together from the internet a little bit.
  geom_text(aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),
                                                                   after_stat(x),
                                                                   sum)
                                        [after_stat(x)], accuracy = 1),
      group = qgroup),
    stat = "count",
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 6
  )


## ----abv_and_type_3, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------
#Kind of halfway distribution of wine type over the three groups
ggplot(wine_correct, aes(x = Type, fill=qgroup))+
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))+
  labs(fill = "Quality Group",
       title = paste(ten_head_df_low$qgroup[1], "group,",
                     ten_head_df_low$Type[1], "type wine"))+
  
  #This text I did have to cobble together from the internet a little bit.
  geom_text(aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),
                                                                   after_stat(x),
                                                                   sum)
                                        [after_stat(x)], accuracy = 1), 
#I still don't really understand what the end of that line does, but
#if I remove it the text breaks.
      group = qgroup),
    stat = "count",
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 6
  )


## ----type_by_itself, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------
typesinglemodel <- naiveBayes(qgroup ~ Type, data = wine_correct)

one_wine = data.frame(Type = typevalue)

predict(typesinglemodel, newdata = one_wine, type = "raw")


## ----second_attempt, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------
second_attempt_model <- naiveBayes(qgroup ~ alcohol + Type, data = wine_correct)

one_wine = data.frame(alcohol = linevalue, Type = typevalue)

predict(second_attempt_model, newdata = one_wine, type = "raw")


## ----bayes_three, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------
ggplot(wine_correct, aes(x = alcohol, y = volatile.acidity, color = qgroup)) +
  geom_point() +
  facet_wrap(. ~ Type) +
  labs(title = "ABV vs. Volatile Acidity by Quality Group and Wine Type",
    x = "ABV (Alcohol by Volume)",
    y = "Volatile Acidity")+
  theme_bw()+
  scale_color_manual(values = c("Low" = "#DD1C1A", "Medium"="#1BDAAA", "High"="#086788"))+
  geom_vline(xintercept = linevalue, linetype = "dashed")+
  geom_hline(yintercept = dioxvalue, linetype = "dashed")

#---------
threepredmodel <- naiveBayes(qgroup ~ alcohol + volatile.acidity + Type, data = wine_correct)

one_wine = data.frame(alcohol = linevalue, volatile.acidity = dioxvalue, Type = typevalue)

predict(threepredmodel, newdata = one_wine, type = "raw")


## ----diox_grph, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------
ggplot(wine_correct, aes(x = volatile.acidity, fill = Type)) + 
  geom_density(alpha = 0.7)+
  scale_fill_manual(values = c("R" = "#ad336d", "W" = "#f9c58d"),
                     labels = c("R" = "Red", "W" = "White"))+
  labs(title = paste(ten_head_df_low$Type[1], "wine type,",
                     ten_head_df_low$volatile.acidity[1], "volatile acidity"),
       x="Volatile Acidity", fill = "Wine Type")+
  geom_vline(xintercept = dioxvalue, linetype = "dashed")


## ----reverse_many_model, echo=FALSE------------------------------------------------------------------------------------------------------------------------------
reverse_many_model <- naiveBayes(Type ~ alcohol + qgroup + volatile.acidity, data = wine_correct)

one_wine = data.frame(alcohol = linevalue, qgroup = qgroupvalue, volatile.acidity = dioxvalue)

predict(reverse_many_model, newdata = one_wine, type = "raw")


## ----reverse_actual_picked_observation---------------------------------------------------------------------------------------------------------------------------
ggplot(wine_correct, aes(x = alcohol, y= volatile.acidity, color = Type)) + 
  geom_point()+
  geom_vline(xintercept = linevalue, linetype = "dashed")+
  geom_hline(yintercept = dioxvalue, linetype = "dashed")+
  scale_color_manual(values = c("R" = "#ad336d", "W" = "#f9c58d"),
                     labels = c("R" = "Red", "W" = "White"))+
  labs(title = "ABV and Volatile Acidity Relationship",
       x="ABV", y="Volatile Acidity", color="Wine Type")


## ----reverse_model, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------
reverse_model_1 <- naiveBayes(Type ~ alcohol + volatile.acidity, data = wine_correct)

one_wine = data.frame(alcohol = linevalue, volatile.acidity = dioxvalue)

predict(reverse_model_1, newdata = one_wine, type = "raw")


## Just for fun.
reverse_model_2 <- naiveBayes(Type ~ alcohol + qgroup, data = wine_correct)

one_wine = data.frame(alcohol = linevalue, qgroup = qgroupvalue)

predict(reverse_model_2, newdata = one_wine, type = "raw")


## ----crossvalidation, include=FALSE------------------------------------------------------------------------------------------------------------------------------
firstcv <- naive_classification_summary_cv(model = first,
                                             data = wine_correct, y = "qgroup", k = 10)

volcv <- naive_classification_summary_cv(model = volmodel,
                                             data = wine_correct, y = "qgroup", k = 10)

twocv <- naive_classification_summary_cv(model = twopredmodel,
                                             data = wine_correct, y = "qgroup", k = 10)

typecv_1 <- naive_classification_summary_cv(model = typesinglemodel,
                                             data = wine_correct, y = "qgroup", k = 10)

secondcv <- naive_classification_summary_cv(model = second_attempt_model,
                                             data = wine_correct, y = "qgroup", k = 10)

threecv <- naive_classification_summary_cv(model = threepredmodel,
                                             data = wine_correct, y = "qgroup", k = 10)

bigrev_cv <- naive_classification_summary_cv(model = reverse_many_model,
                                             data = wine_correct, y = "Type", k = 10)

rev_cv <- naive_classification_summary_cv(model = reverse_model_1,
                                             data = wine_correct, y = "Type", k = 10)

rev_c2v <- naive_classification_summary_cv(model = reverse_model_2,
                                             data = wine_correct, y = "Type", k = 10)


## ----confmat_2var, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
twocv$cv


## ----confmat_3var, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
threecv$cv


## ----abv_grph, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------
ggplot(wine_correct, aes(x = alcohol, fill = Type)) + 
  geom_density(alpha = 0.7)+
  scale_fill_manual(values = c("R" = "#ad336d", "W" = "#f9c58d"),
                     labels = c("R" = "Red", "W" = "White"))+
  labs(title = paste(ten_head_df_low$Type[1], "wine type,",
                     ten_head_df_low$alcohol[1], "ABV"),
       x="ABV", fill = "Wine Type")+
  geom_vline(xintercept = linevalue, linetype = "dashed")

