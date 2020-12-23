#### Preamble ####
# Purpose: The purpose of this script is to clean up the raw data set to better
# fit the analysis we intend to conduct. Also included in the script are graphs 
# that visualize the data.
# Authors: Jasmine Carlos
# Date: 22 December 2020
# Contact: jasmine.carlos@mail.utoronto.ca
# Pre-requisites: Download the raw dataset provided by Globe and Mail which is
# included in the inputs folder.

#### Workspace setup ####
library(haven)
library(tidyverse)
library(readr)
library(dplyr)

## Read in the raw data
raw_data <- read.csv("C:/Users/owner/STA304 Admin/bias-behind-bars-replication/inputs/The_Globe_and_Mail_CSC_OMS_2012-2018_20201022235635.csv")

#### Clean the data ####

# Group the various races into the following categories:black, indigenous, white
# and other

race_grouped_data <- raw_data %>%
  mutate(
    RACE_GROUPED = case_when(
      RACE == "Black" ~ "Black",
      RACE == "White" ~ "White",
      RACE == "Metis" ~ "Indigenous",
      RACE == "Inuit" ~ "Indigenous",
      RACE == "North American" ~ "Indigenous",
      RACE == "S. E. Asian" ~ "Other",
      RACE == "Arab/West Asian" ~ "Other",
      RACE == "Chinese" ~ "Other",
      RACE == "Asiatic" ~ "Other",
      RACE == "Unknown" ~ "Other",
      RACE == "Other" ~ "Other",
      RACE == "Korean" ~ "Other",
      RACE == "Filipino" ~ "Other",
      RACE == "Latin American" ~ "Other",
      RACE == "South Asian" ~ "Other",
      RACE == "East Indian" ~ "Other",
      RACE == "Hispanic" ~ "Other",
      RACE == "Japanese" ~ "Other",
      RACE == "Multirac/Ethnic" ~ "Other",
      RACE == "Caribbean" ~ "Other",
      RACE == "Unable Specify" ~ "Other",
      RACE == "Euro.-Southern" ~ "Other",
      RACE == "Asi-E/Southeast" ~ "Other",
      RACE == "Arab" ~ "Other",
      RACE == "Euro.-Eastern" ~ "Other",
      RACE == "Oceania" ~ "Other",
      RACE == "Asian-South" ~ "Other",
      RACE == "Euro.-Western" ~ "Other",
      RACE == "Sub-Sahara Afri" ~ "Other",
      RACE == "Asian-West" ~ "Other",
      RACE == "European French" ~ "Other",
      RACE == "British Isles" ~ "Other",
      RACE == "Euro.-Northern" ~ "Other",
    ) 
  ) %>%
  filter(!RACE_GROUPED == 'NA') %>%
  filter(!OFFENDER.SECURITY.LEVEL == 'NA') %>%
  filter(!OFFENDER.SECURITY.LEVEL == '') %>%
  filter(!REINTEGRATION.POTENTIAL == 'NA')%>%
  filter(!REINTEGRATION.POTENTIAL == '')



#### Make the model ####

# Model that looked at how race affects an inmate's security level (high vs 
# medium or low score) for females

reduc_mod1F_dat <-
  race_grouped_data %>%
  select(RACE_GROUPED,
         GENDER,
         OFFENDER.SECURITY.LEVEL)

filt_mod1F_dat <-
  reduc_mod1F_dat %>%
  filter(!RACE_GROUPED == 'NA') %>%
  filter(!GENDER == 'NA') %>%
  filter(!OFFENDER.SECURITY.LEVEL == 'NA') %>%
  filter(GENDER == "FEMALE") %>%
  mutate(
    OFFENDER.SECURITY.GROUP = case_when(
      OFFENDER.SECURITY.LEVEL == "MINIMUM" ~ "Not Max",
      OFFENDER.SECURITY.LEVEL == "MEDIUM" ~ "Not Max",
      OFFENDER.SECURITY.LEVEL == "MAXIMUM" ~ "Max") 
  ) %>%
  filter(!OFFENDER.SECURITY.GROUP == 'NA') %>%
  mutate(
    OFFENDER.SECURITY.BINARY = case_when(
      OFFENDER.SECURITY.GROUP == "Not Max" ~ 0,
      OFFENDER.SECURITY.GROUP == "Max" ~ 1,)
  ) %>%
  filter(!OFFENDER.SECURITY.BINARY == 'NA') 

model1F <- glm(OFFENDER.SECURITY.BINARY ~  as.factor(RACE_GROUPED),
               data = filt_mod1F_dat, 
               family = binomial)

summary(model1F)

# Model that looked at how race affects an inmate's security level (high vs 
# medium or low score) for males

reduc_mod1M_dat <-
  race_grouped_data %>%
  select(RACE_GROUPED,
         GENDER,
         OFFENDER.SECURITY.LEVEL)

filt_mod1M_dat <-
  reduc_mod1M_dat %>%
  filter(!RACE_GROUPED == 'NA') %>%
  filter(!GENDER == 'NA') %>%
  filter(!OFFENDER.SECURITY.LEVEL == 'NA') %>%
  filter(GENDER == "MALE") %>%
  mutate(
    OFFENDER.SECURITY.GROUP = case_when(
      OFFENDER.SECURITY.LEVEL == "MINIMUM" ~ "Not Max",
      OFFENDER.SECURITY.LEVEL == "MEDIUM" ~ "Not Max",
      OFFENDER.SECURITY.LEVEL == "MAXIMUM" ~ "Max") 
  ) %>%
  filter(!OFFENDER.SECURITY.GROUP == 'NA') %>%
  mutate(
    OFFENDER.SECURITY.BINARY = case_when(
      OFFENDER.SECURITY.GROUP == "Not Max" ~ 0,
      OFFENDER.SECURITY.GROUP == "Max" ~ 1,)
  ) %>%
  filter(!OFFENDER.SECURITY.BINARY == 'NA') 

model1M <- glm(OFFENDER.SECURITY.BINARY ~  as.factor(RACE_GROUPED),
               data = filt_mod1M_dat, 
               family = binomial)

summary(model1M)

# Model that looked at how race affects the reintegration score (low vs 
# medium or high) for females

reduc_mod2F_dat <-
  race_grouped_data %>%
  select(RACE_GROUPED,
         GENDER,
         REINTEGRATION.POTENTIAL)

filt_mod2F_dat <-
  reduc_mod2F_dat %>%
  filter(!RACE_GROUPED == 'NA') %>%
  filter(!GENDER == 'NA') %>%
  filter(!REINTEGRATION.POTENTIAL == 'NA') %>%
  filter(GENDER == "FEMALE") %>%
  mutate(
    REINTEGRATION.POTENTIAL.GROUP = case_when(
      REINTEGRATION.POTENTIAL == "LOW" ~ "Low",
      REINTEGRATION.POTENTIAL == "MEDIUM" ~ "Not Low",
      REINTEGRATION.POTENTIAL == "HIGH" ~ "Not Low") 
  ) %>%
  filter(!REINTEGRATION.POTENTIAL.GROUP == 'NA') %>%
  mutate(
    REINTEGRATION.POTENTIAL.BINARY = case_when(
      REINTEGRATION.POTENTIAL.GROUP == "Not Low" ~ 0,
      REINTEGRATION.POTENTIAL.GROUP == "Low" ~ 1,)
  ) %>%
  filter(!REINTEGRATION.POTENTIAL.BINARY == 'NA') 

model2F <- glm(REINTEGRATION.POTENTIAL.BINARY ~  as.factor(RACE_GROUPED),
               data = filt_mod2F_dat, 
               family = binomial)

summary(model2F)


# Model that looked at how race affects the reintegration score (low vs 
# medium or high) for males

reduc_mod2M_dat <-
  race_grouped_data %>%
  select(RACE_GROUPED,
         GENDER,
         REINTEGRATION.POTENTIAL)

filt_mod2M_dat <-
  reduc_mod2M_dat %>%
  filter(!RACE_GROUPED == 'NA') %>%
  filter(!GENDER == 'NA') %>%
  filter(!REINTEGRATION.POTENTIAL == 'NA') %>%
  filter(GENDER == "MALE") %>%
  mutate(
    REINTEGRATION.POTENTIAL.GROUP = case_when(
      REINTEGRATION.POTENTIAL == "LOW" ~ "Low",
      REINTEGRATION.POTENTIAL == "MEDIUM" ~ "Not Low",
      REINTEGRATION.POTENTIAL == "HIGH" ~ "Not Low") 
  ) %>%
  filter(!REINTEGRATION.POTENTIAL.GROUP == 'NA') %>%
  mutate(
    REINTEGRATION.POTENTIAL.BINARY = case_when(
      REINTEGRATION.POTENTIAL.GROUP == "Not Low" ~ 0,
      REINTEGRATION.POTENTIAL.GROUP == "Low" ~ 1,)
  ) %>%
  filter(!REINTEGRATION.POTENTIAL.BINARY == 'NA') 

model2M <- glm(REINTEGRATION.POTENTIAL.BINARY ~  as.factor(RACE_GROUPED),
               data = filt_mod2M_dat, 
               family = binomial)

summary(model2M)

#### Make graphs ####

# Distribution of race

race_graph <- race_grouped_data %>%
  ggplot( aes(x = RACE_GROUPED)) + 
  geom_bar() +
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  geom_text(stat="count", aes(label=sprintf("%.01f %%",..count../sum(..count..)*100)))+
  labs(
    title = "Figure 1: Distribution of Races in Prison",
    x = "Race Groups",
    y = "Number of Prisoners",
    caption = "Source: Globe and Mail")

race_graph

# Race Compared to Offender Security Level

offender_race_graph <-
  race_grouped_data %>%
  ggplot(aes(x = OFFENDER.SECURITY.LEVEL, fill= RACE_GROUPED)) +
  geom_bar(stat="count") +
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  labs(
    title = "Figure 2: Distribution of Race Groups over Offender Security Level",
    x = "Offender Security Level",
    y = "Number of Prisoners",
    fill = "Race Group",
    caption = "Source: Globe and Mail)")

offender_race_graph

# White distribution of Offender Security Level

offender_race_graph_white <-
  race_grouped_data %>%
  filter(RACE_GROUPED == "White") %>%
  ggplot(aes(x = OFFENDER.SECURITY.LEVEL, fill= RACE_GROUPED)) +
  geom_bar(stat="count") +
  geom_text(stat="count", aes(label=sprintf("%.01f %%",..count../sum(..count..)*100)))+
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  labs(
    title = "Figure 3: Distribution of White Offenders over Offender Security Level",
    x = "Offender Security Level",
    y = "Number of Prisoners",
    fill = "White Offenders",
    caption = "Source: Globe and Mail)")

offender_race_graph_white

# Black distribution of Offender Security Level

offender_race_graph_black <-
  race_grouped_data %>%
  filter(RACE_GROUPED == "Black") %>%
  ggplot(aes(x = OFFENDER.SECURITY.LEVEL, fill= RACE_GROUPED)) +
  geom_bar(stat="count") +
  geom_text(stat="count", aes(label=sprintf("%.01f %%",..count../sum(..count..)*100)))+
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  labs(
    title = "Figure 4: Distribution of Black Offenders over Offender Security Level",
    x = "Offender Security Level",
    y = "Number of Prisoners",
    fill = "Black Offenders",
    caption = "Source: Globe and Mail)")

offender_race_graph_black

# Indigenous distribution of Offender Security Level

offender_race_graph_ind <-
  race_grouped_data %>%
  filter(RACE_GROUPED == "Indigenous") %>%
  ggplot(aes(x = OFFENDER.SECURITY.LEVEL, fill= RACE_GROUPED)) +
  geom_bar(stat="count") +
  geom_text(stat="count", aes(label=sprintf("%.01f %%",..count../sum(..count..)*100)))+
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  labs(
    title = "Figure 5: Distribution of Indigenous Offenders over Offender Security Level",
    x = "Offender Security Level",
    y = "Number of Prisoners",
    fill = "Indigenous Offenders",
    caption = "Source: Globe and Mail)")

offender_race_graph_ind

# Race Compared to Reintegration Score

reintegration_race_graph <-
  race_grouped_data %>%
  ggplot(aes(x = REINTEGRATION.POTENTIAL, fill= RACE_GROUPED)) +
  geom_bar(stat="count") +
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  labs(
    title = "Figure 6: Distribution of Race Groups over Reintegration Score",
    x = "Reintegration Score",
    y = "Number of Prisoners",
    fill = "Race Group",
    caption = "Source: Globe and Mail)")

reintegration_race_graph

# White Distribution of Reintegration Score

reintegration_race_graph_white <-
  race_grouped_data %>%
  filter(RACE_GROUPED == "White") %>%
  ggplot(aes(x = REINTEGRATION.POTENTIAL, fill= RACE_GROUPED)) +
  geom_bar(stat="count") +
  geom_text(stat="count", aes(label=sprintf("%.01f %%",..count../sum(..count..)*100)))+
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  labs(
    title = "Figure 7: Distribution of White Offenders over Reintegration Score",
    x = "Reintegration Score",
    y = "Number of Prisoners",
    fill = "White Offenders",
    caption = "Source: Globe and Mail)")

reintegration_race_graph_white

# Black Distribution of Reintegration Score

reintegration_race_graph_black <-
  race_grouped_data %>%
  filter(RACE_GROUPED == "Black") %>%
  ggplot(aes(x = REINTEGRATION.POTENTIAL, fill= RACE_GROUPED)) +
  geom_bar(stat="count") +
  geom_text(stat="count", aes(label=sprintf("%.01f %%",..count../sum(..count..)*100)))+
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  labs(
    title = "Figure 8: Distribution of Black Offenders over Reintegration Score",
    x = "Reintegration Score",
    y = "Number of Prisoners",
    fill = "Black Offenders",
    caption = "Source: Globe and Mail)")

reintegration_race_graph_black

# Indigenous Distribution of Reintegration Score

reintegration_race_graph_ind <-
  race_grouped_data %>%
  filter(RACE_GROUPED == "Indigenous") %>%
  ggplot(aes(x = REINTEGRATION.POTENTIAL, fill= RACE_GROUPED)) +
  geom_bar(stat="count") +
  geom_text(stat="count", aes(label=sprintf("%.01f %%",..count../sum(..count..)*100)))+
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  labs(
    title = "Figure 9: Distribution of Indigenous Offenders over Reintegration Score",
    x = "Reintegration Score",
    y = "Number of Prisoners",
    fill = "Indigenous Offenders",
    caption = "Source: Globe and Mail)")

reintegration_race_graph_ind


