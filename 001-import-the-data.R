

library(tidyverse) # you may need to install.packages("tidyverse") in the console
library(readxl)
library(vegetarian) # https://github.com/cran/vegetarian, not on CRAN
# to install remotes::install_github("cran/vegetarian")
library(easyCODA)
library(googlesheets4)

# change to your own UW email
gs4_auth("bmarwick@uw.edu") # opens a browser window, you need to check the box and 
# click "ok" and wait for the message that says 'close this window'

# Get our google sheet of information about the location of each point
block_sample_point_details <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1nbMGQI0_8iOh6vr6sQhBWCsX78NupUqxGeVyhCsFdas/",
             sheet = "block sample points") 

block_sample_locations <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1nbMGQI0_8iOh6vr6sQhBWCsX78NupUqxGeVyhCsFdas/",
             sheet = "block sample locations in excavation")

# to get or update the data files:
# 1. Go to https://drive.google.com/drive/u/1/folders/1KaYNY0ybUQbUAwgcCd1G-A9FXe4-uiA6
# 2. Download to your computer this folder: "Data files from XRF instrument"
# 3. Unzip and place the unzipped folder in a folder named 'data' 
# in the same directory as this file

# get all the file names from our data folder
all_files <- list.files(path = here::here("data"),
                        pattern = "mjb15|mjb-15|tm|MJB15|TM",
                        recursive = TRUE,
                        full.names = TRUE) %>% 
  str_subset ("xls")

# name the data with the sample names
names(all_files) <- tolower(str_remove(basename(all_files), ".xls"))

# how many do we have?
length(all_files) # 250

# most have the individual points labelled like
# pt-000 or pt00 or p000
# so we can take the filename and paste the point number on

# standardise the sample names 
names(all_files) <- 
  names(all_files) %>% 
  str_replace_all(" |_", "-") %>% 
  str_replace_all("pt1",     "pt-001-") %>% 
  str_replace_all("pt01-",   "pt-001-") %>% 
  str_replace_all("pt02-",   "pt-002-") %>% 
  str_replace_all("pt03-",   "pt-003-") %>% 
  str_replace_all("pt04-",   "pt-004-") %>% 
  str_replace_all("pt05-",   "pt-005-") %>% 
  str_replace_all("pt06-",   "pt-006-") %>% 
  str_replace_all("pt07-",   "pt-007-") %>% 
  str_replace_all("pt08-",   "pt-008-") %>% 
  str_replace_all("pt09-",   "pt-009-") %>% 
  str_replace_all("pt10-",   "pt-010-") %>% 
  str_replace_all("pt11-",   "pt-011-") %>% 
  str_replace_all("pt12-",   "pt-012-") %>% 
  str_replace_all("pt-01-",  "pt-001-") %>% 
  str_replace_all("pt-02-",  "pt-002-") %>% 
  str_replace_all("pt-03-",  "pt-003-") %>% 
  str_replace_all("pt-04-",  "pt-004-") %>% 
  str_replace_all("pt-05-",  "pt-005-") %>% 
  str_replace_all("pt-06-",  "pt-006-") %>% 
  str_replace_all("pt-07-",  "pt-007-") %>% 
  str_replace_all("pt-08-",  "pt-008-") %>% 
  str_replace_all("pt-09-",  "pt-009-") %>% 
  str_replace_all("p001-",  "pt-001-") %>% 
  str_replace_all("p002-",  "pt-002-") %>% 
  str_replace_all("p003-",  "pt-003-") %>% 
  str_replace_all("p004-",  "pt-004-") %>% 
  str_replace_all("p005-",  "pt-005-") %>% 
  str_replace_all("p006-",  "pt-006-") %>% 
  str_replace_all("p007-",  "pt-007-") %>% 
  str_replace_all("p008-",  "pt-008-") %>% 
  str_replace_all("p009-",  "pt-009-") %>% 
  str_replace_all("p010-",  "pt-010-") %>% 
  str_replace_all("p011-",  "pt-011-") %>% 
  str_replace_all("p012-",  "pt-012-")

# this gets the nice block IDs
block_ids <- map_chr(all_files, ~strsplit(.x, '/')[[1]][8])
# this get the analysis point numbers
pt_nums <- str_extract(all_files, "pt-[[0-9]]{3}")
# this puts them together
checking <- tibble(block_ids, 
                   pt_nums) %>% 
  mutate(new_file_name = paste0(block_ids, "-", pt_nums, ".xlsx"))
  
dir.create("data/renamed-files/")
file.rename(all_files, 
            paste0("data/renamed-files/", checking$new_file_name))

# up to here

all_files_xls <- 
  map(all_files, ~read_excel(.x, skip = 7)) 
# View(all_files_xls)

all_files_xls_tbl <- 
  tibble(clean_names = names(all_files),
         actual_names = all_files)

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

# check to see what we have, how many & which blocks do we have data from?
names(all_files_xls_format_ok_wt) %>% 
  str_remove(., "-rt|-table|-pt.*") %>% 
  unique() %>% 
  enframe() %>% 
  arrange(value) # 17 rows -> 35 rows


# convert from list of tables into one data frame
all_files_wt_df <- 
  bind_rows(all_files_xls_format_ok_wt, 
            .id = "sample") 

# only look at points that sample the different point types that we have 
block_sample_point_details_matrix <- 
  block_sample_point_details %>% 
  filter(Comment == "matrix") %>% 
  mutate(Block = tolower(Block),
         Point = sprintf("%03d", Point)) %>% 
  mutate(sample_id = str_c(Block, "-pt-", Point)) %>% 
  mutate(sample_id = str_replace(sample_id, "mm-", "mm")) %>% 
  mutate(sample_id = str_replace(sample_id, "mjb-15", "mjb15")) %>% 
  mutate(sample_id = str_replace_all(sample_id, "5wt-\\d{3}-", "")) 


# Matrix points ------------------------------------------------------
# filter data to keep only matrix points
all_files_wt_df_matrix <- 
  all_files_wt_df %>% 
  filter(
    str_detect(sample, 
               str_c(block_sample_point_details_matrix$sample_id, 
                     collapse="|"))
  )

idx <- str_detect(all_files_wt_df$sample,
                  str_c(block_sample_point_details_matrix$sample_id, 
                        collapse="|"))

# all_files_wt_df[idx,]

# check to see what we have
all_files_wt_df_matrix %>% 
  mutate(sample = str_remove(sample, "-rt|-table|-pt.*")) %>% 
  group_by(sample) %>% 
  tally() # 16 -> 11

# our set of elements

our_elements <- c(
  # our set of elements, many are highly correlated, so
  # we comment-out elements to exclude them from the LRA
  # "Aluminium"  ,  
  # Arsenic  ,  
  # Barium   , 
  # Bismuth  ,  
  # Bromine  ,  
  "Calcium"  , 
  # Chromium,  
  # Cobalt    , 
  "Copper"    , 
  # Gallium   , 
  # Germanium , 
  # Gold      , 
  # Iridium   , 
  "Iron",      
  "Magnesium",  
  # "Manganese" , 
  # Mercury   , 
  "Nickel"    , 
  # Osmium    , 
  # Palladium , 
  # "Phosphorus",
  # Platinum ,  
  "Potassium" , 
  # Rhenium   , 
  # Rhodium   , 
  # Rubidium  , 
  # Ruthenium , 
  # Scandium,  
  "Selenium" , 
  "Silicon"   , 
  # Silver    , 
  # "Strontium" , 
  # Sulfur    ,
  # Tantalum  , 
  "Tin",       
  "Titanium" ,  
  # Tungsten  , 
  # Vanadium  , 
  "Yttrium"   , 
  "Zinc"      , 
  "Zirconium")

# select only a sub-set of elements
all_files_wt_df_matrix_wide <- 
  all_files_wt_df_matrix %>% 
  pivot_wider(names_from = "Element",
              values_from = `[wt.%]` ) %>% 
  replace(is.na(.), 0)  %>% 
  replace(. == 0, 0.0001) %>% 
  select(-sample) %>% 
  select(our_elements)

# normalise the measurements so all elements sum to 100 in each sample
all_files_wt_df_matrix_wide_norm <- 
  data.frame(normalize.rows(all_files_wt_df_matrix_wide) * 100)

names(all_files_wt_df_matrix_wide_norm) <- 
  names(all_files_wt_df_matrix_wide)

rownames(all_files_wt_df_matrix_wide_norm) <- 
  str_remove(unique(all_files_wt_df_matrix$sample), "mjb15-")

