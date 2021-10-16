#################
##  Analysis  ##
################

library(tidyverse)
library(scales)
library(lubridate)

df <- readRDS("C:\\Users\\isaac\\Desktop\\eBird\\Data\\Long Island Jan 2010 - Aug 2021.rds")

speciesByAppearance <- df %>%
  mutate(totalChecklists = n_distinct(checklistID)) %>%
  pivot_longer(species, values_to = "species") %>%
  group_by(species, lifer) %>%
  summarize(appearances = n(),
            prop = n() / mean(totalChecklists),
            .groups = "drop") %>%
  arrange(-appearances)

speciesByAppearanceByCounty <- df %>%
  mutate(totalChecklists = n_distinct(checklistID)) %>%
  pivot_longer(species, values_to = "species") %>%
  group_by(species, lifer, county) %>%
  summarize(appearances = n(),
            prop = n() / mean(totalChecklists),
            .groups = "drop_last") %>%
  mutate(propTotal = sum(prop)) %>%
  ungroup() %>%
  arrange(-propTotal)

speciesByAppearanceBySeason <- df %>%
  drop_na(date) %>%
  mutate(season = case_when(month(date) %in% c(12, 1, 2) ~ "Winter",
                            month(date) %in% c(3, 4, 5) ~ "Spring",
                            month(date) %in% c(6, 7, 8) ~ "Summer",
                            month(date) %in% c(9, 10, 11) ~ "Fall")) %>%
  group_by(season) %>%
  mutate(totalChecklistsBySeason = n_distinct(checklistID)) %>%
  pivot_longer(species, values_to = "species") %>%
  group_by(species, lifer, season) %>%
  summarize(appearances = n(),
            prop = n() / mean(totalChecklistsBySeason),
            .groups = "drop")

speciesByAppearanceByDate <- df %>%
  drop_na(date) %>%
  mutate(day = yday(date)) %>%
  group_by(day) %>%
  mutate(totalChecklistsByDay = n_distinct(checklistID)) %>%
  group_by(species, day) %>%
  summarize(prop = n() / mean(totalChecklistsByDay),
            .groups = "drop_last") %>%
  mutate(prop_lag1 = lag(prop, 1),
         prop_lag2 = lag(prop, 2),
         prop_lag3 = lag(prop, 3),
         prop_lead1 = lead(prop, 1),
         prop_lead2 = lead(prop, 2),
         prop_lead3 = lead(prop, 3)) %>%
  rowwise() %>%
  mutate(prop_running7 = mean(c(prop, prop_lag1, prop_lag2, prop_lag3, prop_lead1, prop_lead2, prop_lead3), na.rm = T)) %>%
  group_by(species) %>%
  mutate(diff = max(prop_running7) - min(prop_running7)) %>%
  ungroup() %>%
  filter(diff > .45)

speciesByAppearanceByCounty %>%
  mutate(county = recode(county, 
                         "Suffolk" = "Suffolk County",
                         "Nassau" = "Nassau County")) %>%
  slice_head(n = 100) %>%
  mutate(species = fct_reorder(species, prop)) %>%
  ggplot() +
    geom_bar(aes(species, prop, fill = county), stat = "identity") +
    scale_y_continuous(name = "Checklist Appearances",
                       position = "right",
                       label = label_percent(accuracy = 1),
                       expand = c(0, 0, 0, 0),
                       limits = c(0, .375),
                       breaks = seq(0, .35, .05)) +
    scale_x_discrete(name = NULL) +
    scale_fill_manual(name = NULL,
                      values = c("#F9B650", "#009DAE"),
                      guide = guide_legend(reverse = T)) +
    ggtitle("The Most Commonly Identified Birds on Long Island",
            subtitle = "2010 - Present") +
    coord_flip() +
    theme(legend.position = "top",
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5, face = "italic"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey"),
          axis.ticks.x = element_blank())

ggsave("C:\\Users\\isaac\\Desktop\\eBird\\Output\\Birds on LI.png",
       width = 6, height = 8, units = "in")

speciesByAppearance %>%
  slice_head(n = 100) %>%
  mutate(species = fct_reorder(species, prop)) %>%
  ggplot() +
    geom_bar(aes(species, prop, fill = lifer), stat = "identity") +
    scale_y_continuous(name = "Checklist Appearances",
                       position = "right",
                       label = label_percent(accuracy = 1),
                       expand = c(0, 0, 0, 0),
                       limits = c(0, .375),
                       breaks = seq(0, .35, .05)) +
    scale_x_discrete(name = NULL) +
    scale_fill_manual(name = NULL,
                      values = c("#F9B650", "#009DAE"),
                      labels = c("Not yet!", "Lifer"),
                      guide = guide_legend(reverse = T)) +
    ggtitle("The Most Commonly Identified Birds on Long Island") +
    coord_flip() +
    theme(legend.position = "top",
          axis.text = element_text(size = 5),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey"),
          axis.ticks.x = element_blank())

ggsave("C:\\Users\\isaac\\Desktop\\eBird\\Output\\Birds on LI by Lifelist.png",
       width = 6, height = 12, units = "in")

speciesByAppearanceBySeason %>%
  group_by(season) %>%
  arrange(-prop) %>%
  slice_head(n = 50) %>%
  mutate(species = species %>%
           paste0(., ".", season) %>%
           fct_reorder(., prop)) %>%
  ggplot() +
    geom_bar(aes(species, prop, fill = lifer), stat = "identity") +
    facet_wrap(~ season, nrow = 2, scales = "free") +
    scale_y_continuous(name = "Checklist Appearances",
                       position = "right",
                       label = label_percent(accuracy = 1),
                       expand = c(0, 0, 0, 0),
                       limits = c(0, .5),
                       breaks = seq(0, .5, .05)) +
    scale_x_discrete(name = NULL,
                     labels = function(x) gsub("\\..*", "", x)) +
    scale_fill_manual(name = NULL,
                      values = c("#F9B650", "#009DAE"),
                      labels = c("Not yet!", "Lifer"),
                      guide = guide_legend(reverse = T)) +
    ggtitle("The Most Commonly Identified Birds on Long Island") +
    coord_flip() +
    theme(legend.position = "top",
          axis.text = element_text(size = 5),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey"),
          axis.ticks.x = element_blank())

ggsave("C:\\Users\\isaac\\Desktop\\eBird\\Output\\Birds on LI by Lifelist by Season.png",
       width = 10, height = 10, units = "in")

speciesByAppearanceByDate %>%
  ggplot() +
    geom_density(aes(day, prop_running7, color = species, fill = species), 
                 stat = "identity",
                 alpha = .1) +
    scale_y_continuous(name = "Checklist Appearances",
                       label = label_percent(accuracy = 1)) +
    scale_x_discrete(name = "Day",
                     labels = function(x) gsub("\\..*", "", x)) +
    theme(legend.position = "top",
          axis.text = element_text(size = 5),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey"),
          axis.ticks.x = element_blank())

ggsave("C:\\Users\\isaac\\Desktop\\eBird\\Output\\Birds on LI by Date.png",
       width = 10, height = 10, units = "in")

speciesByAppearance %>%
  filter(lifer == T) %>%
  slice_tail(n = 25) %>%
  mutate(species = fct_reorder(species, prop)) %>%
  ggplot() +
    geom_bar(aes(species, prop, fill = lifer), stat = "identity") +
    scale_y_continuous(name = "Checklist Appearances",
                       position = "right",
                       label = label_percent(accuracy = 1),
                       expand = c(0, 0, 0, 0),
                       limits = c(0, .375),
                       breaks = seq(0, .35, .05)) +
    scale_x_discrete(name = NULL) +
    scale_fill_manual(name = NULL,
                      values = c("#009DAE"),
                      labels = c("Lifer"),
                      guide = guide_legend(reverse = T)) +
    ggtitle("The Rarest LI Birds on my Life List") +
    coord_flip() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey"),
          axis.ticks.x = element_blank())

ggsave("C:\\Users\\isaac\\Desktop\\eBird\\Output\\Rarest Birds on Lifelist.png",
       width = 6, height = 8, units = "in")

speciesByAppearance %>%
  filter(lifer == F) %>%
  slice_head(n = 25) %>%
  mutate(species = fct_reorder(species, prop)) %>%
  ggplot() +
    geom_bar(aes(species, prop, fill = lifer), stat = "identity") +
    scale_y_continuous(name = "Checklist Appearances",
                       position = "right",
                       label = label_percent(accuracy = 1),
                       expand = c(0, 0, 0, 0),
                       limits = c(0, .375),
                       breaks = seq(0, .35, .05)) +
    scale_x_discrete(name = NULL) +
    scale_fill_manual(name = NULL,
                      values = c("#F9B650"),
                      labels = c("Not yet!"),
                      guide = guide_legend(reverse = T)) +
    ggtitle("The Most Common LI Birds Not on my Life List") +
    coord_flip() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey"),
          axis.ticks.x = element_blank())

ggsave("C:\\Users\\isaac\\Desktop\\eBird\\Output\\Most Common Birds Not on Lifelist.png",
       width = 6, height = 8, units = "in")
