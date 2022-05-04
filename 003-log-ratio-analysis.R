
# import and prepare the data
source(here::here("001-import-the-data.R"))

# explore LRA for matrix points -----------------------------------------------
# compute Log Ratio Analysis
all_files_wt_df_matrix_wide_norm_lra <- 
  LRA(all_files_wt_df_matrix_wide_norm)

# plot matrix points -----------------------------------------------
# read in the code to make the plot, and visualise the LRA
source(here::here("999-gglra-function.R"))
gglra(all_files_wt_df_matrix_wide_norm_lra) +
  theme(plot.background = element_rect(fill = "white", 
                                       colour = "white"))

ggsave(here::here("figures/all_files_wt_df_matrix_wide_norm_lra.png"),
       w = 8,
       h = 7,
       dpi = 600)

## plot by phases and rows ------------------------------------------
all_files_wt_df_matrix_wide_norm_phases_lra <- 
  all_files_wt_df_matrix_wide_norm %>% 
  filter(str_detect(rownames(.), "mm")) %>% 
  LRA()

source(here::here("002-row-and-phase-analysis.R"))

# from 002-row-and-phase-analysis.R we have the 
# normalised elements with row and phase values
all_files_wt_df_matrix_wide_norm_mm_phase_and_row <- 
all_files_wt_df_matrix_wide_norm_mm %>% 
  # create rowname
  mutate(phase_rowname = paste0(rowname, 
                                "_row", Row, 
                                "_phase", Phase, 
                                "_", row_number())) %>% 
  column_to_rownames("phase_rowname") %>% 
  select(-rowname,
         -`Sample ID`,
         -`Sample ID2`,
         -Row,
         -Note,
         -Phase) 

all_files_wt_df_matrix_wide_norm_mm_phase_and_row_lra <-  
  LRA(all_files_wt_df_matrix_wide_norm_mm_phase_and_row)

# plot by phases
gglra_phases(all_files_wt_df_matrix_wide_norm_mm_phase_and_row_lra)

# plot by rows
gglra_rows(all_files_wt_df_matrix_wide_norm_mm_phase_and_row_lra)












# ----------------- stop here --------------------------------

















# Look at feature points ---------------------------------------

# filter data to keep only feature points
all_files_wt_df_feature <- 
  all_files_wt_df %>% 
  filter(
    str_detect(sample, 
               str_c(block_sample_point_details_feature$sample_id, collapse="|"))
  )

# check to see what we have
all_files_wt_df_feature %>% 
  mutate(sample = str_remove(sample, "-rt|-table|-pt.*")) %>% 
  group_by(sample) %>% 
  tally() 

# select only a sub-set of elements for feature points
all_files_wt_df_feature_wide <- 
  all_files_wt_df_feature  %>% 
  pivot_wider(names_from = "Element",
              values_from = `[wt.%]` ) %>% 
  replace(is.na(.), 0)  %>% 
  replace(. == 0, 0.0001) %>% 
  select(-sample) %>% 
  select(our_elements)

# normalise the feature point measurements so all elements sum to 100 in each sample
all_files_wt_df_feature_wide_norm <- data.frame(normalize.rows(all_files_wt_df_feature_wide) * 100)
names(all_files_wt_df_feature_wide_norm) <- names(all_files_wt_df_feature_wide)
rownames(all_files_wt_df_feature_wide_norm) <- 
  str_remove(unique(all_files_wt_df_feature$sample), "mjb15-")

# explore LRA for feature points -----------------------------------------------
# compute Log Ratio Analysis
all_files_wt_df_feature_wide_norm_lra <- LRA(all_files_wt_df_feature_wide_norm)


# plot matrix points -----------------------------------------------
# read in the code to make the plot, and visualise the LRA
source(here::here("999-gglra-function.R"))
gglra(all_files_wt_df_feature_wide_norm_lra)



# Look at edge points ---------------------------------------

# filter data to keep only edge points
all_files_wt_df_edge <- 
  all_files_wt_df %>% 
  filter(
    str_detect(sample, 
               str_c(block_sample_point_details_edge$sample_id, collapse="|"))
  )

# check to see what we have
all_files_wt_df_edge %>% 
  mutate(sample = str_remove(sample, "-rt|-table|-pt.*")) %>% 
  group_by(sample) %>% 
  tally() 

# select only a sub-set of elements for edge points
all_files_wt_df_edge_wide <- 
  all_files_wt_df_edge  %>% 
  pivot_wider(names_from = "Element",
              values_from = `[wt.%]` ) %>% 
  replace(is.na(.), 0)  %>% 
  replace(. == 0, 0.0001) %>% 
  select(-sample) %>% 
  select(our_elements)

# normalise the edge point measurements so all elements sum to 100 in each sample
all_files_wt_df_edge_wide_norm <- data.frame(normalize.rows(all_files_wt_df_edge_wide) * 100)
names(all_files_wt_df_edge_wide_norm) <- names(all_files_wt_df_edge_wide)
rownames(all_files_wt_df_edge_wide_norm) <- 
  str_remove(unique(all_files_wt_df_edge$sample), "mjb15-")

# explore LRA for edge points -----------------------------------------------
# compute Log Ratio Analysis
all_files_wt_df_edge_wide_norm_lra <- LRA(all_files_wt_df_edge_wide_norm)


# plot matrix points -----------------------------------------------
# read in the code to make the plot, and visualise the LRA
source(here::here("999-gglra-function.R"))
gglra(all_files_wt_df_edge_wide_norm_lra)


