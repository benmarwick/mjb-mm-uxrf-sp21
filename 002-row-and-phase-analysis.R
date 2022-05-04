source(here::here("001-import-the-data.R"))

# split here to to archaeological samples and termite samples (further below)

# add row and phase values

block_sample_locations_to_join <- 
  block_sample_locations %>% 
  mutate(mm_number = str_match(`Sample ID2`, "MM-\\d")) %>% 
  mutate(mm_number = tolower(mm_number)) %>% 
  mutate(mm_number = str_remove(mm_number, "-")) 

all_files_wt_df_matrix_wide_norm_mm <- 
  all_files_wt_df_matrix_wide_norm %>% 
  filter(str_detect(rownames(.), "mm" )) %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_match(rowname, "mm\\d")) %>% 
  left_join(block_sample_locations_to_join,
            by = c('rowname' = 'mm_number' ))

# check it looks as we expect
# all_files_wt_df_matrix_wide_norm_mm %>% 
#   select(rowname, "Sample ID2", Row, Phase) 

library(ggbeeswarm)

# change the element name in here to make plots 
# showing how each element varies by phase

# by Phase (depth)
all_files_wt_df_matrix_wide_norm_mm_long <- 
  all_files_wt_df_matrix_wide_norm_mm %>% 
  select(-rowname,
         -`Sample ID`,
         -`Sample ID2`,
         -Row,
         -Note) %>% 
  pivot_longer(-Phase,
               names_to = "element",
               values_to = "concentration")

ggplot(all_files_wt_df_matrix_wide_norm_mm_long) +
  aes(Phase,
      concentration,
      group = Phase) +
  geom_boxplot() +
  facet_wrap(~ element, 
             scales = "free_y") +
  geom_quasirandom(alpha = 0.3,
                   size = 3) +
  scale_y_log10() +
  theme_bw()

# by row (front-back of the rockshelter)
all_files_wt_df_matrix_wide_norm_mm_long_row <- 
  all_files_wt_df_matrix_wide_norm_mm %>% 
  select(-rowname,
         -`Sample ID`,
         -`Sample ID2`,
         -Phase,
         -Note) %>% 
  pivot_longer(-Row,
               names_to = "element",
               values_to = "concentration") %>% 
  mutate(Row = as.factor(Row)) 


ggplot(all_files_wt_df_matrix_wide_norm_mm_long_row) +
  aes(Row, 
      concentration,
      group = Row) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.3,
                   size = 3) +
  theme_bw() +
  facet_wrap(~ element, 
             scales = "free_y") 

#-----------------------------------------------------------------
# termite means for each element

tm_element_average_conc <- 
  all_files_wt_df_matrix_wide_norm %>% 
  rownames_to_column() %>% 
  filter(str_detect(rowname, "tm")) %>% 
  select(-rowname) %>% 
  pivot_longer(everything(),
               names_to = "element",
               values_to = "concentration") %>%
  arrange(element) %>% 
  group_by(element) %>% 
  summarise(average_concentration = mean(concentration),
            sd_concentration = sd(concentration)) 

# combine with archaeological concs

# phases with termite ranges
ggplot() +
  geom_hline(data = tm_element_average_conc,
             aes(yintercept = average_concentration),
             colour = "red") +
  geom_rect(aes(xmin = -Inf, 
                xmax = Inf, 
                ymin = average_concentration - sd_concentration, 
                ymax = average_concentration + sd_concentration),
            fill = "red",
            alpha = 0.2,
            data = tm_element_average_conc,
            inherit.aes = FALSE) +
  geom_boxplot(data = all_files_wt_df_matrix_wide_norm_mm_long, 
               aes(Phase,
                   concentration,
                   group = Phase),
               outlier.shape = NA) +
  geom_quasirandom(data = all_files_wt_df_matrix_wide_norm_mm_long, 
                   aes(Phase,
                       concentration,
                       group = Phase),
                   alpha = 0.3,
                   size = 3) +
  scale_y_log10() +
  facet_wrap(~ element, 
             scales = "free_y") +
  theme_bw() 

ggsave(here::here("figures/phases-with-termite-ranges.png"),
       w = 10,
       h = 7,
       dpi = 600)

# rows with termite ranges 
ggplot(all_files_wt_df_matrix_wide_norm_mm_long_row) +
  aes(Row, 
      concentration,
      group = Row) +
  geom_hline(data = tm_element_average_conc,
             aes(yintercept = average_concentration),
             colour = "red") +
  geom_rect(aes(xmin = -Inf, 
                xmax = Inf, 
                ymin = average_concentration - sd_concentration, 
                ymax = average_concentration + sd_concentration),
            fill = "red",
            alpha = 0.2,
            data = tm_element_average_conc,
            inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA) +
  geom_quasirandom(alpha = 0.3,
                   size = 3) +
  theme_bw() +
  facet_wrap(~ element, 
             scales = "free_y") 

