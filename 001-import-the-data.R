

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
length(all_files) # 346

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
pt_nums <- str_extract(names(all_files), "pt-[[0-9]]{3}")

# this puts them together
checking <- tibble(block_ids, 
                   pt_nums) %>% 
  mutate(new_file_name = paste0(block_ids, "-", pt_nums, ".xls"))
  
# delete this later
dir.create("data/renamed-files/")

file.rename(all_files, 
            paste0("data/renamed-files/", 
                   checking$new_file_name))

all_renamed_files <- 
  list.files("data/renamed-files/",
             full.names = TRUE)

# delete everything in the "Data files from XRF instrument"
# folder to avoid confusion
unlink("data/Data files from XRF instrument",
       recursive = TRUE)

all_files_rennamed <- 
list.files("data/renamed-files/",
           full.names = TRUE)

all_files_xls <- 
<<<<<<< HEAD
  map(all_renamed_files, ~read_excel(.x, skip = 7)) 
# View(all_files_xls)

# drop empty or malformed data tables, check for presence of 'series' column
=======
  map(all_files_rennamed, ~read_excel(.x, skip = 7)) 

names(all_files_xls) <- tolower(str_remove(basename(all_files_rennamed), ".xls"))

# drop empty or malformed data tables, 
# check for presence of 'series' column
# this indicates that summary table that 
# we want. Some files have counts, this
# is too raw for us. 
>>>>>>> ab381787716256e01f19d253a9b74b847e573751

names(all_files_xls)

# identify tables that have the element weights, they have a column
# called "series" we want that. 
# (don't want the spectra files, which lack the 'series' column)
all_files_xls_format_ok_idx <- 
  map_lgl(all_files_xls, 
          ~ifelse("series" %in% names(.x), TRUE, FALSE))

<<<<<<< HEAD
# what are those samples that lack the table of data
all_renamed_files[!all_files_xls_format_ok_idx]

# drop bad tables
=======
# drop data frames that are not the summary tables
# that we want
>>>>>>> ab381787716256e01f19d253a9b74b847e573751
all_files_xls_format_ok <- 
  all_files_xls[all_files_xls_format_ok_idx]

# how many points does this leave us?
length(all_files_xls_format_ok) # 201

# select only the element and wt cols
all_files_xls_format_ok_wt <- 
  map(all_files_xls_format_ok, ~.x %>% 
        filter(!str_detect(series, "Sum")) %>% 
        select(Element, `[wt.%]` )) 

# check to see what we have, how many & 
# which blocks do we have data from?
names(all_files_xls_format_ok_wt) %>% 
  str_remove_all(., "-rt|-table|-p.{3,5}") %>% 
  unique() %>% 
  enframe() %>% 
<<<<<<< HEAD
  arrange(value) # 17 rows -> 35 rows

=======
  arrange(value) # 19 rows
>>>>>>> ab381787716256e01f19d253a9b74b847e573751

# convert from list of tables into one data frame
all_files_wt_df <- 
  bind_rows(all_files_xls_format_ok_wt, 
            .id = "sample") 

# only look at points that sample the different point types that we have 
block_sample_point_details_matrix <- 
  block_sample_point_details %>% 
  filter(Comment == "matrix") %>% 
  mutate(Block = tolower(Block2),
         Point = sprintf("%03d", Point)) %>% 
  mutate(sample_id = tolower(str_c(Block2, "-pt-", Point))) %>% 
  drop_na(sample_id)


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
all_files_wt_df_matrix_blocks <- 
all_files_wt_df_matrix %>% 
  mutate(sample = str_remove(sample, "-pt.*")) %>% 
  group_by(sample) %>% 
  tally() # 16 -> 11 -> 9 -> 12

all_files_wt_df_matrix_blocks

# our set of elements

our_elements <- c(
  # our set of elements, many are highly correlated, so
  # we comment-out elements to exclude them from the LRA
  #"Aluminium"  ,  
  "Arsenic"  ,  
  "Barium"   , 
  "Bromine"  ,  
  "Calcium"  , 
  "Chromium",  
  "Cobalt"    , 
  "Copper"    , 
  "Gallium"   , 
  "Germanium" , 
  "Iridium"   , 
  "Iron",      
  "Magnesium",  
  "Manganese" , 
  "Nickel"    , 
  "Palladium" , 
  #"Phosphorus",
  "Platinum" ,  
  "Potassium" , 
  "Rhodium"   , 
  "Rubidium"  , 
  "Scandium",  
  "Selenium" , 
  "Silicon"   , 
  "Silver"    ,  # very correlated 
  "Strontium" ,  # very correlated 
  #"Sulfur"    ,  # very correlated 
  "Tin",       
  "Titanium" ,  
  "Tungsten"  , 
  "Vanadium"  , 
  "Yttrium"   , 
  "Zinc"      , 
  "Zirconium"
 )

# select only a sub-set of elements
all_files_wt_df_matrix_wide <- 
  all_files_wt_df_matrix %>% 
  pivot_wider(names_from = "Element",
              values_from = `[wt.%]` ) %>% 
  replace(is.na(.), 0)  %>% 
  replace(. == 0, 0.0001) %>% 
  dplyr::select(-sample) %>% 
  dplyr::select(our_elements)

# normalise the measurements so all elements sum to 100 in each sample
all_files_wt_df_matrix_wide_norm <- 
  data.frame(normalize.rows(all_files_wt_df_matrix_wide) * 100)

names(all_files_wt_df_matrix_wide_norm) <- 
  names(all_files_wt_df_matrix_wide)

rownames(all_files_wt_df_matrix_wide_norm) <- 
  str_remove(unique(all_files_wt_df_matrix$sample), "mjb15-")

# explore just a few samples
all_files_wt_df_matrix_wide_norm_subset <- 
  all_files_wt_df_matrix_wide_norm %>% 
  filter(
    str_detect(rownames(.), 
                    str_c(
                    c(
                    # we need to manually add the block ID
                    # here to make it show up on the plot
                    "mm-05-9ya-013",
                    "mm-06-9ya-014",
                    "mm-07-9ya-015",
                    "mm-08-5wt-001",
                    "mm-08-9ya-016",
                    "mm-09-5wt-002",
                    "mm-10-5wt-003",
                    "mm-17-6dg-004",
                    "mm-22-5wt-006",
                    "tm-02-00-9ya-001",
                    "tm-01-01-9ya-003",
                    "tm-01-05-9ya-007"
                    ),
                    collapse = "|"))
  ) %>% 
  filter(!rownames(.) %in% c(
    # filter out highly outlying points
    "tm-02-00-9ya-001-pt-004-pt-004.xls",
    "tm-02-00-9ya-001-pt-003-pt-003.xls"
                              ))



