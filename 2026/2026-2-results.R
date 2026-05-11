rm(list=ls())

data <- read.csv("Results/2026/Cleaned_Summary.csv")

## convert to correct variable types
data <- data %>% mutate(across(
  c("score", "talent", "culture", "alumni", "fundraising", "relevant"),
  ~ as.numeric(.x)
))
data <- data %>% mutate(across(
  c("institution", "index_type", "n_consulted", "time"),
  ~ as.factor(.x)
))


################################################
## Plot overall scores
################################################
plotdata <- data
levels(plotdata$index_type) <- c("Core Index", "Full Index")

plotdata <- plotdata %>% 
  ungroup() %>%   # As a precaution / handle in a separate .grouped_df method
  arrange(index_type, score) %>%   # arrange by facet variables and continuous values
  mutate(.r = row_number()) # Add a row number variable

## x axis = institutions, facets = score type (core / full), y axis = score
ggplot(plotdata,
       aes(x = reorder_within(institution, score, index_type), y = score, group = index_type, color = index_type)) +
  geom_point(size = 2, position=position_dodge(width = 0.5)) +
  geom_text(aes(label = sprintf("%.2f", score)), vjust = -1.5, size = 3) +
  facet_grid(~ index_type, scale="free_x", space="free_x") +
  labs(y = "Score", x = "Institution", title = "2026 Inclusion Index") +
  theme_bw() +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0,10)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette="Dark2") +
  theme(panel.spacing.x = unit(1, "lines")) +
  theme(panel.spacing.y = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.5))
ggsave("Figures/scores_2026.png", width = 7, height = 5, dpi = 600)


################################################
## Plot scores by theme
################################################
plotdata <- data

plotdata <- plotdata %>% pivot_longer(
  cols = c(talent, culture, alumni, fundraising),
  names_to = "theme"
)

levels(plotdata$index_type) <- c("Core Index", "Full Index")
plotdata$theme <- factor(plotdata$theme)
levels(plotdata$theme) <- c("Alumni", "Culture", "Fundraising", "Talent")

plotdata <- plotdata %>% 
  ungroup() %>%   # As a precaution / handle in a separate .grouped_df method
  arrange(index_type, value) %>%   # arrange by facet variables and continuous values
  mutate(.r = row_number()) # Add a row number variable

## x axis = themes, facets = index type (core / full), y axis = value
ggplot(plotdata,
       aes(x = theme, y = value, fill = index_type, color = index_type)) +
  #geom_boxplot(aes(color = type), outlier.shape = NA) +
  geom_dotplot(aes(), stroke = NA, alpha = 0.6, binaxis = 'y', stackdir = 'center', method = "histodot", binwidth = 0.2) +
  #geom_point(size = 2, position=position_dodge(width = 0.5)) +
  #geom_text(aes(label = sprintf("%.2f", value)), vjust = -1, size = 2) +
  facet_grid(~ index_type, scale="free_x") +
  stat_summary(geom = "errorbar", fun.min = median, fun = median, fun.max = median, width = 0.8, alpha = 1) +
  labs(y = "Score", x = "Theme", fill = "Index Type", title = "2026 Inclusion Index") +
  theme_bw() +
  scale_y_continuous(limits = c(0,10)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(panel.spacing.x = unit(1, "lines")) +
  theme(panel.spacing.y = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold"))
ggsave("Figures/scores_by_theme_2026.png", width = 8, height = 5, dpi = 600)


################################################
## Plot changes in score over time
################################################

data2025 <- read.csv("Results/2025/Cleaned_Summary.csv")
data2025$year <- rep(2025)
data2025 <- data2025 %>% select(c(institution, index_type, score, year))
data2026 <- read.csv("Results/2026/Cleaned_Summary.csv")
data2026$year <- rep(2026)
data2026 <- data2026 %>% select(c(institution, index_type, score, year))

plotdata <- rbind(data2025, data2026)
rm(data2025, data2026)
plotdata <- plotdata %>% arrange(institution, year, index_type)
plotdata$year <- as.factor(plotdata$year)

## keep cases where more than one observation is available
plotdata <- plotdata %>% group_by(institution) %>% filter(n() > 1)
## additionally, delete the following cases so that we have one of either core or full index for both years per institution: Warwick Full 2025+2026, Nottingham Core 2025
plotdata <- plotdata %>% filter_out(institution == "Warwick" & index_type == "Full")
plotdata <- plotdata %>% filter_out(institution == "Nottingham" & year == 2025 & index_type == "Core")

## create ordering column for magnitude of difference
plotdata <- plotdata %>% group_by(institution) %>% mutate(diff = diff(score)) %>% ungroup()
plotdata <- plotdata %>% mutate(change = ifelse(diff >= 0, "Increase", "Decrease"))
## create ordering column for average score across years
plotdata <- plotdata %>% group_by(institution) %>% mutate(avg = mean(score)) %>% ungroup()

ggplot(plotdata,
       aes(x = score, y = fct_reorder(institution, diff))) +
  geom_line(aes(colour = change), linewidth = 1) +
  geom_point(aes(fill = year), shape = 21, colour = "transparent", size = 4) +
  #facet_grid(~ index_type, scale="free_x") +
  scale_x_continuous(limits = c(1,9)) +
  labs(x = "Score", y = "Institution", fill = "Year", colour = "", title = "2026 Inclusion Index") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(type = "qual", direction = -1, palette = "Paired") +
  scale_colour_manual(values = c("indianred3", "green4")) +
  guides(colour = "none") +
  theme(panel.spacing.x = unit(1, "lines")) +
  theme(panel.spacing.y = unit(1.5, "lines")) +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.text.y = element_text(face="bold"))
ggsave("Figures/score_change_2026.png", width = 8, height = 6, dpi = 600)


rm(plotdata)