library(tidyverse)
library(lubridate)
library(scales)
library(janitor)
library(ggthemes)
library(MetBrewer)
library(showtext)
library(patchwork)

patient_risk_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

df <- 
  patient_risk_profiles |> 
  pivot_longer(cols = c(2:20),
               names_to = "age",
               values_to = "value")

df <- df |> 
  pivot_longer(cols = c(2:3),
               names_to = "sex",
               values_to = "value_1")

df |> 
  filter(value_1 == 1) |> 
  filter(value == 1) |> 
  group_by(age,sex) |> 
  summarise(avg = mean(`predicted risk of Parkinson's disease, inpatient or with 2nd diagnosis`)) |> 
  mutate(avg = avg * 100)

p1 <- df |> 
  filter(value_1 == 1) |> 
  filter(value == 1) |> 
  group_by(age,sex) |> 
  summarise(avg = mean(`predicted risk of Dementia`)) |> 
  #mutate(avg = avg * 100) |> 
  mutate(age =  gsub('age group:', '', age)) |> 
  mutate(sex =  gsub('Sex =', '', sex)) |> 
  ggplot(aes(age, avg, fill = sex)) + 
  geom_col(color = "black") + 
  facet_wrap(~sex) + 
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("pink","lightblue")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(strip.text = element_blank()) +
  theme(plot.title = element_text(size = 30)) + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size=15)) +
  labs(title = "Dementia")

p2 <- df |> 
  filter(value_1 == 1) |> 
  filter(value == 1) |> 
  group_by(age,sex) |> 
  summarise(avg = mean(`predicted risk of Migraine`)) |> 
  #mutate(avg = avg * 100) |> 
  mutate(age =  gsub('age group:', '', age)) |> 
  mutate(sex =  gsub('Sex =', '', sex)) |> 
  ggplot(aes(age, avg, fill = sex)) + 
  geom_col(color = "#363636") + 
  facet_wrap(~sex) + 
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("pink","lightblue")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 30)) +
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size=15)) +
  theme(strip.text = element_blank()) + 
  labs(title = "Migrain")

p3 <- df |> 
  filter(value_1 == 1) |> 
  filter(value == 1) |> 
  group_by(age,sex) |> 
  summarise(avg = mean(`predicted risk of Muscle weakness or injury`)) |> 
  #mutate(avg = avg * 100) |> 
  mutate(age =  gsub('age group:', '', age)) |> 
  mutate(sex =  gsub('Sex =', '', sex)) |> 
  ggplot(aes(age, avg, fill = sex)) + 
  geom_col(color = "#363636") + 
  facet_wrap(~sex) + 
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("pink","lightblue")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 30)) + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size=15)) +
  theme(strip.text = element_blank()) + 
  labs(title = "Muscle weakness or injury")

p4 <- df |> 
  filter(value_1 == 1) |> 
  filter(value == 1) |> 
  group_by(age,sex) |> 
  summarise(avg = mean(`predicted risk of Sudden Vision Loss, with no eye pathology causes`)) |> 
  #mutate(avg = avg * 100) |> 
  mutate(age =  gsub('age group:', '', age)) |> 
  mutate(sex =  gsub('Sex =', '', sex)) |> 
  ggplot(aes(age, avg, fill = sex)) + 
  geom_col(color = "#363636") + 
  facet_wrap(~sex) + 
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("pink","lightblue")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 30)) + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size=15)) +
  theme(strip.text = element_blank()) + 
  labs(title = "Sudden Vision Loss")

p5 <- df |> 
  filter(value_1 == 1) |> 
  filter(value == 1) |> 
  group_by(age,sex) |> 
  summarise(avg = mean(`predicted risk of  Treatment resistant depression (TRD)`)) |> 
  #mutate(avg = avg * 100) |> 
  mutate(age =  gsub('age group:', '', age)) |> 
  mutate(sex =  gsub('Sex =', '', sex)) |> 
  ggplot(aes(age, avg, fill = sex)) + 
  geom_col(color = "#363636") + 
  facet_wrap(~sex) + 
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("pink","lightblue")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 30)) + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(strip.text = element_blank()) + 
  labs(title = "Treatment resistant depression")

p6 <- df |> 
  filter(value_1 == 1) |> 
  filter(value == 1) |> 
  group_by(age,sex) |> 
  summarise(avg = mean(`predicted risk of Parkinson's disease, inpatient or with 2nd diagnosis`)) |> 
  #mutate(avg = avg * 100) |> 
  mutate(age =  gsub('age group:', '', age)) |> 
  mutate(sex =  gsub('Sex =', '', sex)) |> 
  ggplot(aes(age, avg, fill = sex)) + 
  geom_col(color = "#363636") + 
  facet_wrap(~sex) + 
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("pink","lightblue")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 30)) + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(strip.text = element_blank()) + 
  labs(title = "Parkinson's disease")

P = (p1 + p2)/(p3 + p4)/(p5 + p6)
P

# Added the title, subtitle in Figma
