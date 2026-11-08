library(dplyr)
library(ggplot2)
library(rlang)
library(purrr)
library(stringr)
library(tibble)

## certain questions are removed from dfs to protect anonymity

# read farmer data
farm = read.csv("farmer_data_github.csv")

# summarize
length(unique(farm$farm_id)) # 16 farms
nrow(farm) # 24
table(farm$sex) # 5 women, 19 men
table(farm$education) # 1 degree; 23 primary

####### build summary table function ######
summarize_question = function(data, col, id = farm_id,
                              declined_label = "Declined to answer",
                              declined_vals = c("", "unk")) {
  col_quo <- enquo(col)
  id_quo  <- enquo(id)
  tmp <- data %>%
    mutate(
      .resp_raw = as.character(!!col_quo),
      .declined = is.na(!!col_quo) | .resp_raw %in% declined_vals,
      .resp     = if_else(.declined, declined_label, .resp_raw)
    )
  
  # Denominators (from raw data; correct for per-question respondents)
  total_rows_all         <- nrow(tmp)
  total_rows_non_declined <- sum(!tmp$.declined)
  
  total_farms_all <- tmp %>% distinct(!!id_quo) %>% nrow()
  total_farms_respondents <- tmp %>%
    filter(!.declined) %>%
    distinct(!!id_quo) %>% nrow()
  
  tab <- tmp %>%
    group_by(!!id_quo, .resp) %>%
    summarise(n_rows = n(), .groups = "drop") %>%
    group_by(.resp) %>%
    summarise(
      n_farms     = n_distinct(!!id_quo),
      n_responses = sum(n_rows),
      .groups     = "drop"
    ) %>%
    mutate(
      pct_rows_of_respondents = if_else(
        .resp != declined_label & total_rows_non_declined > 0,
        round(100 * n_responses / total_rows_non_declined, 1),
        NA_real_
      ),
      pct_rows_of_interviewed = round(100 * n_responses / total_rows_all, 1),
      pct_farms_of_respondents = if_else(
        .resp != declined_label & total_farms_respondents > 0,
        round(100 * n_farms / total_farms_respondents, 1),
        NA_real_
      ),
      pct_farms_of_interviewed = round(100 * n_farms / total_farms_all, 1)
    ) %>%
    arrange(desc(n_responses)) %>%
    # rename response column back to the original name
    rename(!!as_name(col_quo) := .resp) %>%
    # enforce your requested column order
    select(
      !!col_quo,
      n_responses,
      pct_rows_of_respondents,
      pct_rows_of_interviewed,
      n_farms,
      pct_farms_of_respondents,
      pct_farms_of_interviewed
    )
  
  tab
}

########### weekly sale ########### 
table_weekly_sale = summarize_question(farm, weekly_sale)

########### live/dead ########### 
table_live_dead = summarize_question(farm, live_dead)

########### cost ########### 
# cost- some gave specifics, some gave a range. we'll choose a midpoint for the range and then group by bins <150; 150-200 (exclusive); >200
# midpoint for 150-200 = 175l 250_300 = 275
farm$cost_kg = ifelse(farm$cost_kg == "150_200", 175, farm$cost_kg)
farm$cost_kg = ifelse(farm$cost_kg == "250_300", 275, farm$cost_kg)
farm$cost_kg = as.numeric(farm$cost_kg)
farm$cost_bin = ifelse(farm$cost_kg < 150, "<150", 
                       ifelse(farm$cost_kg >= 150 & farm$cost_kg < 200, "150-200",
                              ifelse(farm$cost_kg > 200, "> 200", NA)))

table_cost_bin = summarize_question(farm, cost_bin)

########### free-roam ########### 
farm$free_roam = ifelse(farm$enclosed_pen == "<25", "75-100", farm$free_roam) # merge information from the flip-side question
table_free_roam = summarize_question(farm, free_roam)

# if above 25%, put 1
farm$free_roam_2 = ifelse(farm$free_roam == "75-100" | farm$free_roam == "25_50" | farm$free_roam == "50-75", 1, 
                          ifelse(farm$free_roam == "<25", 0, NA))

table(farm$free_roam_2, farm$farm_id, useNA = "always")
########### access to human waste ########### 
# combine information from roam_defecate for the 1 'maybe' response (answer to do they roam where people defecate is yes)
farm$waste_access = ifelse(farm$roam_defecate == "y_sometimes", "y", farm$waste_access) # merge information from the flip-side question
# summarize
table_waste_access = summarize_question(farm, waste_access)

########### toilet ########### 
# bin toilet info. dig gole, outside, pit latrine = open_defecation. toilet is toilet
farm$toilet_bin = ifelse(farm$toilet == "dig holes" | farm$toilet == "outside" | farm$toilet == "pit latrine", "open", farm$toilet)

table_toilet = summarize_question(farm, toilet_bin)

###### open defecation within 5 meters of pigs
# of all non-absent responses
table(farm$toilet_meters)
(17/23)*100
# of just the ones with open defecation
close = farm[farm$toilet_bin == "open",]
table(close$toilet_meters)
(17/18)*100
length(unique(close$farm_id)) # 13

########### client base ########### 

# add in additional information from next column for the one person who said they sold to chinese market but put just 'companies' in sell to column
farm$sold_to = ifelse(farm$local_export == "chinese_market_addis", "chinese", farm$sold_to)

tmp = farm %>%
  mutate(sold_to_std = str_squish(str_to_lower(sold_to)))
total_rows_all        = nrow(tmp)                                                     # all rows
total_rows_non_blank  = tmp %>% filter(!is.na(sold_to_std), sold_to_std != "") %>% nrow()

total_farms_all       = tmp %>% distinct(farm_id) %>% nrow()                          # all farms
total_farms_respond   = tmp %>%
  filter(!is.na(sold_to_std), sold_to_std != "") %>%
  distinct(farm_id) %>% nrow()

# summarize, non-exclusive (responses can appear in multiple categories)
table_sold_to = tmp %>%
  mutate(
    is_blank   = is.na(sold_to_std) | sold_to_std == "",
    is_chinese = str_detect(sold_to_std, "chinese"),
    is_broker  = str_detect(sold_to_std, "middle\\s*men|broker"),
    is_hotel   = str_detect(sold_to_std, "hotel|restaurant"),
    is_other   = !is_blank & !(is_chinese | is_broker | is_hotel)
  ) %>%
  pivot_longer(
    c(is_chinese, is_broker, is_hotel, is_blank, is_other),
    names_to = "category", values_to = "hit"
  ) %>%
  filter(hit) %>%
  mutate(
    category = dplyr::case_match(
      category,
      "is_chinese" ~ "Chinese",
      "is_broker"  ~ "Middle men / brokers",
      "is_hotel"   ~ "Hotels / restaurants",
      "is_blank"   ~ "Blank / missing",
      "is_other"   ~ "Other",
      .default = category
    )
  ) %>%
  group_by(category) %>%
  summarise(
    n_farms    = n_distinct(farm_id),
    total_rows = n(),
    .groups    = "drop"
  ) %>%
  mutate(
    # Row-based percentages (non-exclusive; categories can sum >100%)
    pct_rows_of_respondents = if_else(
      category != "Blank / missing" & total_rows_non_blank > 0,
      round(100 * total_rows / total_rows_non_blank, 1),
      NA_real_
    ),
    pct_rows_of_interviewed = round(100 * total_rows / total_rows_all, 1),
    
    # Farm-based percentages (non-exclusive; categories can sum >100%)
    pct_farms_of_respondents = if_else(
      category != "Blank / missing" & total_farms_respond > 0,
      round(100 * n_farms / total_farms_respond, 1),
      NA_real_
    ),
    pct_farms_of_interviewed = round(100 * n_farms / total_farms_all, 1)
  ) %>%
  arrange(desc(total_rows))

#################### slaughter #################### 
slaughter = farm[farm$live_dead == "both" | farm$live_dead == "farm_slaughter",]

#################### cleaning schedge #################### 
table_clean = summarize_question(farm, clean_sched)

############################ ############################ ############################ 
#################################### ABATTOIR DATA#################################### 
############################ ############################ ############################ 

abattoir = read.csv("abattoir_data_github.csv")

# summarize
length(unique(abattoir$loc_id)) # 4 abattoirs
nrow(abattoir) # 12 respondents
table(abattoir$sex) # 1 women, 11 men
table(abattoir$education) # 7 degree; 2 primary; 3 secondary

####### photo question
abattoir$photo_c = ifelse(grepl("\\bC\\b", abattoir$photo_q, ignore.case = FALSE), 1, 0)
table(abattoir$photo_c, abattoir$loc_id)

### client base
# create ethnicity column from client column
abattoir = abattoir %>%
  mutate(
    ethnicity = if_else(
      str_detect(str_to_lower(clients), "chinese"),
      "chinese",
      "unspecified"))

### create business column from client column
abattoir = abattoir %>%
  mutate(
    business = if_else(
      str_detect(str_to_lower(clients), "hotel|restaurant"),
      "hotels/restaurants",
      ifelse(clients == "unk", "unk",
             "other")))

table(abattoir$business, useNA = "always")
table(abattoir$ethnicity, useNA = "always") 

#### parts inspected
parts_summary = abattoir %>%
  mutate(row_id = row_number()) %>%
  separate_rows(parts_inspec, sep = ",\\s*") %>%
  mutate(parts_inspec = str_trim(str_to_lower(parts_inspec))) %>%
  distinct(row_id, parts_inspec) %>%
  count(parts_inspec, name = "n_rows") %>%
  mutate(
    total_rows_all = nrow(abattoir),
    total_rows_non_na = sum(!is.na(abattoir$parts_inspec)),
    pct_all = round(100 * n_rows / total_rows_all, 1),
    pct_non_na = round(100 * n_rows / total_rows_non_na, 1)
  ) %>%
  arrange(desc(n_rows))

# abattoir perspective on parts inspected
loc_summary = abattoir %>%
  separate_rows(parts_inspec, sep = ",\\s*") %>%
  mutate(parts_inspec = str_trim(str_to_lower(parts_inspec))) %>%
  filter(!is.na(parts_inspec) & parts_inspec != "") %>%
  distinct(loc_id, parts_inspec) %>%    # each loc_id counts only once per part
  count(parts_inspec, name = "n_loc_id") %>%   # number of unique loc_id per part
  arrange(desc(n_loc_id))

# summarize number of rows and percentages per response
summary_from_11 = abattoir %>%
  mutate(across(11:ncol(.), ~ {
    x <- as.character(.)
    x[str_to_lower(x) %in% c("", "unk", "unknown")] <- NA
    x
  })) %>%
  pivot_longer(
    cols = 11:ncol(.),
    names_to = "variable",
    values_to = "response"
  ) %>%
  group_by(variable) %>%
  mutate(
    total_rows_all    = n(),                    
    total_rows_non_na = sum(!is.na(response))   
  ) %>%
  mutate(response = if_else(is.na(response), "missing", response)) %>%
  group_by(variable, response, total_rows_all, total_rows_non_na, .add = TRUE) %>%
  summarise(
    n_rows   = n(),  
    n_loc_id = n_distinct(loc_id),  
    .groups  = "drop_last"
  ) %>%
  mutate(
    pct_non_na = if_else(
      response != "missing" & total_rows_non_na > 0,
      round(100 * n_rows / total_rows_non_na, 1),
      NA_real_
    ),
    pct_all = round(100 * n_rows / total_rows_all, 1)
  ) %>%
  ungroup() %>%
  select(
    variable, response,
    n_rows, total_rows_non_na, pct_non_na,
    total_rows_all, pct_all,
    n_loc_id
  ) %>%
  arrange(variable, desc(n_rows))

