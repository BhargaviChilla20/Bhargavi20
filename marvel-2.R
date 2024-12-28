rm(list = ls())

install.packages("readxl")

library(readxl)

getwd()

setwd("C:/Users/user/Documents")

marvel <- read_excel("comic_characters.xlsx")

head(marvel)

head(marvel, 2)

library(ggplot2)

ggplot(marvel, aes(x = Alignment, y = Universe, color = Alignment)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0.2), size = 3, alpha = 0.6) +  
  labs(
    title = "Scatterplot: Alignment vs Universe",
    x = "Character Alignment",
    y = "Universe"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Good" = "blue", "Bad" = "red", "Neutral" = "green", "Non-dual" = "purple"))  


library(dplyr)

df_proportions <- marvel %>%
  group_by(Universe, Alignment) %>%
  summarise(Count = n(), .groups = 'drop') %>%  
  group_by(Universe) %>%  
  mutate(Proportion = Count / sum(Count))  

head(df_proportions)

ggplot(df_proportions, aes(x = Universe, y = Proportion, fill = Alignment)) +
  geom_bar(stat = "identity") +  
  labs(
    title = "Proportion of Alignments within Marvel and DC Universes",
    x = "Universe",
    y = "Proportion of Characters"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Good" = "green", "Bad" = "red", "Neutral" = "gray"))



ggplot(marvel, aes(x = Appearances)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(marvel$Appearances, na.rm = TRUE), 
                                         sd = sd(marvel$Appearances, na.rm = TRUE)), color = "red", size = 1) +  
  facet_wrap(~ Universe) +  
  labs(
    title = "Histogram of Appearances by Universe with Normal Curve",
    x = "Appearances",
    y = "Density"
  ) +
  theme_minimal()

ggplot(marvel, aes(x = Appearances, y = Universe, fill = Universe)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Boxplot: Character Appearances by Alignment",
    x = "Number of Appearances",
    y = "Universe"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Good" = "blue", "Bad" = "red", "Neutral" = "green"))  


