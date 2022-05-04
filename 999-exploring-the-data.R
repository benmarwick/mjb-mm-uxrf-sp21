
#-----------------------------------------------------------------
library(tidyverse)
library(readxl)

# MJB15-MM-5
mm5_files <- list.files(pattern = "mm5",
                        recursive = TRUE,
                        full.names = TRUE) %>% 
  str_subset ("xls")

# mm5 point 1 is ground matrix
mm5_pt_001 <- 
mm5_files %>% 
  str_subset ("pt-001 v") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum"))

# mm5 point 4 is a large grain
mm5_pt_004 <- 
  mm5_files %>% 
  str_subset ("pt-004") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(Net, "Sum"))

# mm5 point 5 is a large grain
mm5_pt_005 <- 
  mm5_files %>% 
  str_subset ("pt-004") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(Net, "Sum"))

# mm5 point 6 is a large grain
mm5_pt_006 <- 
  mm5_files %>% 
  str_subset ("pt-006") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(Net, "Sum"))

ggplot(mm5_pt_001) +
  aes(Element,
      `[wt.%]`) +
  geom_col() +
  coord_flip()  +
  theme_minimal()

ggplot(mm5_pt_004) +
  aes(Element,
      `[wt.%]`) +
  geom_col() +
  coord_flip()  +
  theme_minimal()

ggplot(mm5_pt_006) +
  aes(Element,
      `[wt.%]`) +
  geom_col() +
  coord_flip()  +
  theme_minimal()
  
  
# 
mm2_files <- list.files(pattern = "mm2",
                        recursive = TRUE,
                        full.names = TRUE) %>% 
  str_subset ("xls")


## compare vacuum and no vacuum

# mm5 point 1 is ground matrix
mm5_pt_001_nv <- 
  mm5_files %>% 
  str_subset ("pt-001 nv") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

mm5_pt_001_wv <- 
  mm5_files %>% 
  str_subset ("pt-001 v") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

# join 
list(nv = mm5_pt_001_nv,
     wv = mm5_pt_001_wv) %>% 
  bind_rows( .id = "sample") %>% 
  ggplot() +
  aes(Element,
      `[wt.%]`,
      fill = sample) +
  geom_col(position = "dodge") +
  coord_flip()  +
  theme_minimal()  +
  ylim(0, 1) +
  ggtitle("mm5_pt_001")

# compare pt-002 vac no vac
mm5_pt_002_nv <- 
  mm5_files %>% 
  str_subset ("pt-002 NV") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

mm5_pt_002_wv <- 
  mm5_files %>% 
  str_subset ("pt-001 V") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

# join 
list(nv = mm5_pt_002_nv,
     wv = mm5_pt_002_wv) %>% 
  bind_rows( .id = "sample") %>% 
  ggplot() +
  aes(Element,
      `[wt.%]`,
      fill = sample) +
  geom_col(position = "dodge") +
  coord_flip()  +
  theme_minimal() +
  ggtitle("mm5_pt_002") +
  ylim(0, 1)

# compare pt-003 vac no vac
mm5_pt_003_nv <- 
  mm5_files %>% 
  str_subset ("pt-003 NV") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

mm5_pt_003_wv <- 
  mm5_files %>% 
  str_subset ("pt-003 v") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

# join 
list(nv = mm5_pt_003_nv,
     wv = mm5_pt_003_wv) %>% 
  bind_rows( .id = "sample") %>% 
  ggplot() +
  aes(Element,
      `[wt.%]`,
      fill = sample) +
  geom_col(position = "dodge") +
  coord_flip()  +
  theme_minimal() +
  ggtitle("mm5_pt_003") +
  ylim(0, 1)

# compare pt-007 vac no vac
# this is the only one with exact same set of elements
mm5_pt_007_nv <- 
  mm5_files %>% 
  str_subset ("pt-007 nv") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

mm5_pt_007_wv <- 
  mm5_files %>% 
  str_subset ("pt-007 v") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

# join 
list(nv = mm5_pt_007_nv,
     wv = mm5_pt_007_wv) %>% 
  bind_rows( .id = "sample") %>% 
  ggplot() +
  aes(Element,
      `[wt.%]`,
      fill = sample) +
  geom_col(position = "dodge") +
  coord_flip()  +
  theme_minimal() +
  ggtitle("mm5_pt_007") +
  ylim(0, 0.1)


# on TM1-1, a termite slide

# MJB15-MM-5
tm1_1_files <- list.files(pattern = "tm1-1",
                        recursive = TRUE,
                        full.names = TRUE) %>% 
  str_subset ("xls")

# pt 2 looks like a resin void

tm1_1_pt_002 <- 
  tm1_1_files %>% 
  str_subset ("pt02_spectable") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

# pt 3 looks like a feature

tm1_1_pt_003 <- 
  tm1_1_files %>% 
  str_subset ("pt03_spectable") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

# pt 4 looks like sediment matrix

tm1_1_pt_004 <- 
  tm1_1_files %>% 
  str_subset ("pt04_spectable") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

# join 
list(pt_002 = tm1_1_pt_002,
     pt_003 = tm1_1_pt_003,
     pt_004 = tm1_1_pt_004) %>% 
  bind_rows( .id = "sample") %>% 
  # drop elements where all are zero
  ggplot() +
  aes(Element,
      `[norm. wt.%]`,
      fill = sample) +
  geom_col(position = "dodge") +
  coord_flip()  +
  theme_minimal() +
  ggtitle("tm1_1 pt 2 (resin) & pt 4 (matrix)") 

# look into multiple elements vs. fewer

tm_1_5_files <- list.files(pattern = "tm-1-5",
                          recursive = TRUE,
                          full.names = TRUE) %>% 
  str_subset ("xls")

tm1_5_pt_005 <- 
  tm1_1_files %>% 
  str_subset ("tm-1-5") %>% 
  read_excel(skip = 7) %>% 
  filter(!str_detect(series, "Sum")) 

# join 
list(pt_002 = tm1_1_pt_002,
     pt_003 = tm1_1_pt_003,
     pt_004 = tm1_1_pt_004) %>% 
  bind_rows( .id = "sample") %>% 
  # drop elements where all are zero
  ggplot() +
  aes(Element,
      `[norm. wt.%]`,
      fill = sample) +
  geom_col(position = "dodge") +
  coord_flip()  +
  theme_minimal() +
  ggtitle("tm1_1 pt 2 (resin) & pt 4 (matrix)") 


# explore LRA -----------------------------------------------

all_files <- list.files(pattern = "mjb15|tm",
                        recursive = TRUE,
                        full.names = TRUE) %>% 
  str_subset ("xls")

# name the data with the sample names
names(all_files) <- str_remove(basename(all_files), ".xls")

# standardise the sample names 
names(all_files) <- 
  names(all_files) %>% 
  str_replace_all(" |_", "-") %>% 
  str_replace_all("-1-pt0", "-1-pt-00") %>% 
  str_replace_all("pt0", "pt-0") %>% 
  ifelse(startsWith(., "mjb15-"), ., str_c("mjb15-", .) )

all_files_xls <- 
  map(all_files, ~read_excel(.x, skip = 7)) 

# drop empty or malformed data tables, check for presence of 'series' column

# identify bad tables
all_files_xls_format_ok_idx <- 
  map_lgl(all_files_xls, ~ifelse("series" %in% names(.x), TRUE, FALSE))

# drop bad tables
all_files_xls_format_ok <- 
all_files_xls[all_files_xls_format_ok_idx]

# select only the element and wt cols
all_files_xls_format_ok_wt <- 
map(all_files_xls_format_ok, ~.x %>% 
      filter(!str_detect(series, "Sum")) %>% 
      select(Element, `[wt.%]` )) 

# convert from list of tables into one data frame
all_files_wt_df <- 
  bind_rows(all_files_xls_format_ok_wt, .id = "sample") 

# only look at points that sample the matrix 
library(googlesheets4)
block_sample_point_details <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1nbMGQI0_8iOh6vr6sQhBWCsX78NupUqxGeVyhCsFdas/",
             sheet = "block sample points")

block_sample_point_details_matrix <- 
block_sample_point_details %>% 
  filter(Comment == "matrix") %>% 
  mutate(Block = tolower(Block),
         Point = sprintf("%03d", Point)) %>% 
  mutate(sample_id = str_c(Block, "-pt-", Point)) %>% 
  mutate(sample_id = str_replace(sample_id, "mm-5", "mm5")) %>% 
  mutate(sample_id = str_replace(sample_id, "mm-2", "mm2"))

# filter data to keep only matrix points
all_files_wt_df_matrix <- 
  all_files_wt_df %>% 
  filter(
    str_detect(sample, 
               str_c(block_sample_point_details_matrix$sample_id, collapse="|"))
  )
  

# select only a sub-set of elements
all_files_wt_df_wide <- 
  all_files_wt_df_matrix %>% 
  pivot_wider(names_from = "Element",
              values_from = `[wt.%]` ) %>% 
  replace(is.na(.), 0)  %>% 
  replace(. == 0, 0.0001) %>% 
  select(-sample) %>% 
  select(
    # our set of elements
    Aluminium  ,  
    # Arsenic  ,  
    # Barium   , 
    # Bismuth  ,  
    # Bromine  ,  
    Calcium  , 
    # Chromium,  
    # Cobalt    , 
    Copper    , 
    # Gallium   , 
    # Germanium , 
    # Gold      , 
    # Iridium   , 
    Iron,      
    Magnesium,  Manganese , 
    # Mercury   , 
    Nickel    , Osmium    , 
    # Palladium , 
    Phosphorus,
    Platinum ,  Potassium , 
    # Rhenium   , 
    # Rhodium   , 
    # Rubidium  , 
    # Ruthenium , 
    # Scandium,  
    # Selenium , 
    Silicon   , 
    # Silver    , 
    Strontium , 
    # Sulfur    ,
    Tantalum  , Tin,       
    Titanium ,  Tungsten  , Vanadium  , Yttrium   , 
    # Zinc      , 
    Zirconium 
         )

library(vegetarian)
all_files_wt_df_wide_norm <- data.frame(normalize.rows(all_files_wt_df_wide) * 100)
names(all_files_wt_df_wide_norm) <- names(all_files_wt_df_wide)
rownames(all_files_wt_df_wide_norm) <- unique(all_files_wt_df_matrix$sample)

library(easyCODA)
all_files_wt_df_wide_norm_lra <- LRA(all_files_wt_df_wide_norm)
gglra(all_files_wt_df_wide_norm_lra)

##-------------------------------------------------------------------------
# Sampled TM1-5 slide, points 1, 2, 3, 4, 5, 6, 7, 8 with full set of elements
# Sampled point 1 twice: once with 5 elements: Al, Mg, P, K, Cu (named tm-1-5-pt-001 5e), and once with full set (named tm-1-5-pt-001). 

all_files_wt_df_tm_1_5 <- 
all_files_wt_df %>% 
  filter(str_detect(sample, "tm-1-5-pt-001"))

ggplot(all_files_wt_df_tm_1_5) +
  aes(Element,
      `[wt.%]`) +
  geom_col() +
  facet_wrap( ~ sample, 
              ncol = 1,
              scales = "free_y")

ggplot(all_files_wt_df_tm_1_5) +
  aes(Element,
      `[wt.%]`,
      fill = sample) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# what if we select only the elements in common, then normalise? # Will the 
# two samples appear the same?




