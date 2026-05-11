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
  scale_y_continuous(limits = c(0,10)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette="Dark2") +
  theme(panel.spacing.x = unit(1, "lines")) +
  theme(panel.spacing.y = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(axis.text.x = element_blank())
ggsave("Figures/Anonymised/scores_2026_anon.png", width = 7, height = 5, dpi = 600)
rm(plotdata)

################################################
## Export plots for each institution separately
################################################

## Create a generic function to produce separate plots for each institution, only retaining the relevant axis label

single_export <- function (data, inst) {
  plotdata <- data
  plot <- ggplot(plotdata,
         aes(x = reorder_within(institution, score, index_type), y = score, group = index_type, color = index_type)) +
    geom_point(size = 2, position=position_dodge(width = 0.5)) +
    geom_text(aes(label = sprintf("%.2f", score)), vjust = -1.5, size = 3) +
    facet_grid(~ index_type, scale="free_x", space="free_x") +
    labs(y = "Score", x = "Institution", title = "2026 Inclusion Index") +
    theme_bw() +
    scale_x_reordered(labels = ~if_else(grepl(inst, .x), gsub("\\_.*", "", .x), "")) +
    scale_y_continuous(limits = c(0,10)) +
    theme(legend.position = "none") +
    scale_color_brewer(palette="Dark2") +
    theme(panel.spacing.x = unit(1, "lines")) +
    theme(panel.spacing.y = unit(1.5, "lines")) +
    theme(axis.title.y = element_text(face="bold")) +
    theme(axis.text.x = element_text(face="bold"))
  plot
  
  ggsave(paste0("Figures/Anonymised/Individual/", inst, ".png"), plot, width = 7, height = 5, dpi = 600)
}

## Now run this function for each subset and outcome
instlist <- levels(data$institution)
for (inst in instlist) {
  single_export(data, inst)
}
rm(inst, instlist, single_export)


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

## anonymised plot of score change
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
  theme(axis.text.y = element_blank())
ggsave("Figures/Anonymised/score_change_2026.png", width = 8, height = 6, dpi = 600)


## Create a generic function to produce separate plots for each institution, only retaining the relevant axis label
single_export_change <- function (data, inst) {
  ggplot(plotdata,
         aes(x = score, y = fct_reorder(institution, diff))) +
    geom_line(aes(colour = change), linewidth = 1) +
    geom_point(aes(fill = year), shape = 21, colour = "transparent", size = 4) +
    #facet_grid(~ index_type, scale="free_x") +
    scale_x_continuous(limits = c(1,9)) +
    scale_y_reordered(labels = ~if_else(grepl(inst, .x), gsub("\\_.*", "", .x), "")) +
    labs(x = "Score", y = "Institution", fill = "Year", colour = "") +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_fill_brewer(type = "qual", direction = -1, palette = "Paired") +
    scale_colour_manual(values = c("indianred4", "green4")) +
    guides(colour = "none") +
    theme(panel.spacing.x = unit(1, "lines")) +
    theme(panel.spacing.y = unit(1.5, "lines")) +
    theme(axis.title.x = element_text(face="bold")) +
    theme(axis.text.y = element_text(face="bold"))
  
  ggsave(paste0("Figures/Anonymised/Individual/", inst, "_Change.png"), width = 8, height = 6, dpi = 600)
}

## Now run this function for each subset and outcome
instlist <- levels(as.factor(plotdata$institution))
for (inst in instlist) {
  single_export_change(data, inst)
}
rm(inst, instlist, single_export_change)

rm(plotdata)