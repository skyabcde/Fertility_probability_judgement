library(tidyverse)
library(tidyr)
library(haven) 
library(dplyr)
library(data.table) 
library(here)
library(data.table)
library(ggplot2)
library(ggridges)
theme_set(theme_bw())

#----------------------------------------------------------------------------------------------------
# Fertililty patients dataset.
fertility_patient = here::here(".git/Dataset/FP_n281.sav")
fp_all = haven::read_sav(fertility_patient)

colnames(fp_all) %>%
  grepl(pattern = '^task_b_') %>%
  which() -> task_b_columns
colnames(fp_all) %>%
  grepl(pattern = '^task_b_display') %>%
  which() -> task_b_display_columns

fp_taskb <- fp_all %>%
  select(all_of(task_b_columns), -all_of(task_b_display_columns)) %>%
  dplyr::mutate(id = row_number())

fp_taskb_display <- fp_all %>%
  select(all_of(task_b_display_columns)) %>%
  dplyr::mutate(id = row_number())

str_remove(fp_taskb$task_b_unlikely,"%") -> fp_taskb$task_b_unlikely
str_remove(fp_taskb$task_b_almost_certainly,"%") -> fp_taskb$task_b_almost_certainly
str_remove(fp_taskb$task_b_probably,"%") -> fp_taskb$task_b_probably
str_remove(fp_taskb$task_b_probable,"%") -> fp_taskb$task_b_probable
str_remove(fp_taskb$task_b_very_good_chance,"%") -> fp_taskb$task_b_very_good_chance
str_remove(fp_taskb$task_b_probably_not,"%") -> fp_taskb$task_b_probably_not
str_remove(fp_taskb$task_b_about_even,"%") -> fp_taskb$task_b_about_even
str_remove(fp_taskb$task_b_we_doubt,"%") -> fp_taskb$task_b_we_doubt
str_remove(fp_taskb$task_b_almost_no_chance,"%") -> fp_taskb$task_b_almost_no_chance
str_remove(fp_taskb$task_b_better_than_even,"%") -> fp_taskb$task_b_better_than_even
str_remove(fp_taskb$task_b_improbable,"%") -> fp_taskb$task_b_improbable
str_remove(fp_taskb$task_b_chances_are_slight,"%") -> fp_taskb$task_b_chances_are_slight
str_remove(fp_taskb$task_b_little_chance,"%") -> fp_taskb$task_b_little_chance
str_remove(fp_taskb$task_b_highly_unlikely,"%") -> fp_taskb$task_b_highly_unlikely
str_remove(fp_taskb$task_b_we_believe,"%") -> fp_taskb$task_b_we_believe
str_remove(fp_taskb$task_b_likely,"%") -> fp_taskb$task_b_likely
str_remove(fp_taskb$task_b_highly_likely,"%") -> fp_taskb$task_b_highly_likely
str_remove_all(fp_taskb$task_b_almost_certainly,'\\+') -> fp_taskb$task_b_almost_certainly

# Save the cleansed task B data from patients. 
write.csv(fp_taskb, ".git/Dataset/fp_taskb.csv", row.names=FALSE)

sapply(fp_taskb, class) 
fp_taskb <- as.data.frame(apply(fp_taskb, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

pivot_longer(fp_taskb, -id, names_to="term", values_to = "ratings") %>%
  mutate(ratings = as.numeric(ratings), 
         term = substring(term, 8,100)) %>%
  filter(!is.na(ratings)) -> fp_taskb_long

fp_taskb %>% 
  select(-id) %>% 
  colMeans(na.rm = TRUE) %>% 
  sort() %>% 
  names() %>%
  substring(8,100) -> term_order_fp

# Ploting 
fp_taskb_long %>%
  mutate(term= factor(term, levels=term_order_fp, ordered = TRUE)) %>%
  ggplot(aes(x = ratings, y = term, height = stat(density))) +
  geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = FALSE) +
  labs(title = 'Ratings of terms in patients') 

# Pairwise correlation test between participants in Task B.
expand.grid(id1=fp_taskb$id, id2=fp_taskb$id) %>%
  mutate(cor = purrr::map2_dbl(id1, id2, function(id1,id2){
    id1_rating=unlist(fp_taskb[fp_taskb$id == id1,])
    id2_rating=unlist(fp_taskb[fp_taskb$id == id2,])
    cor(id1_rating, id2_rating, method = 'k', use = 'pairwise')
  })) %>%
  filter(id1 != id2) -> cor_taskb_fp

cor_taskb_fp %>%
  filter(cor!=0)%>%
  group_by(id1) %>% 
  summarise(mean_cor_b = mean(cor)) %>% 
  arrange(mean_cor_b) %>%
  mutate(rank_cor_b = row_number()) -> cor_ave_taskb_fp

# Plotting
cor_taskb %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = cor_mean_taskb_ranked$id1, ordered = TRUE),
    id2 = factor(id2, levels = cor_mean_taskb_ranked$id1, ordered = TRUE)
  ) -> cor_taskb1_fp

cor_taskb1_fp %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "x", expand = c(0,0)) +
  scale_y_continuous(name = "y", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Task B Rating Pairwise Ordinal Correlations within patients"))

# Descriptive analysis
fp_taskb <- fp_taskb[sapply(fp_taskb, is.numeric)]  
fp_taskb_sd <- apply(fp_taskb, 2, sd, na.rm=TRUE)
fp_taskb_mean <- apply(fp_taskb, 2, mean, na.rm=TRUE)
fp_taskb_median <- apply(fp_taskb, 2, median, na.rm=TRUE)

fp_taskb_display <- fp_taskb_display[sapply(fp_taskb_display, is.numeric)]  
fp_taskb_display_sd <- apply(fp_taskb_display, 2, sd, na.rm=TRUE)
fp_taskb_display_mean <- apply(fp_taskb_display, 2, mean, na.rm=TRUE)
fp_taskb_display_median <- apply(fp_taskb_display, 2, median, na.rm=TRUE)

#----------------------------------------------------------------------------------------------------
# Health care professional dataset.
fertility_doctors = here::here(".git/Dataset/HCP_n263.sav")
hcp_all = haven::read_sav(fertility_doctors)

colnames(hcp_all) %>%
  grepl(pattern = '^task_b_') %>%
  which() -> task_b_columns
colnames(hcp_all) %>%
  grepl(pattern = '^task_b_display') %>%
  which() -> task_b_display_columns

hcp_taskb <- hcp_all %>%
  select(all_of(task_b_columns), -all_of(task_b_display_columns)) %>%
  dplyr::mutate(id = row_number())

hcp_taskb_display <- hcp_all %>%
  select(all_of(task_b_display_columns)) %>%
  dplyr::mutate(id = row_number())

str_remove(hcp_taskb$task_b_unlikely,"%") -> hcp_taskb$task_b_unlikely
str_remove(hcp_taskb$task_b_almost_certainly,"%") -> hcp_taskb$task_b_almost_certainly
str_remove(hcp_taskb$task_b_probably,"%") -> hcp_taskb$task_b_probably
str_remove(hcp_taskb$task_b_probable,"%") -> hcp_taskb$task_b_probable
str_remove(hcp_taskb$task_b_very_good_chance,"%") -> hcp_taskb$task_b_very_good_chance
str_remove(hcp_taskb$task_b_probably_not,"%") -> hcp_taskb$task_b_probably_not
str_remove(hcp_taskb$task_b_about_even,"%") -> hcp_taskb$task_b_about_even
str_remove(hcp_taskb$task_b_we_doubt,"%") -> hcp_taskb$task_b_we_doubt
str_remove(hcp_taskb$task_b_almost_no_chance,"%") -> hcp_taskb$task_b_almost_no_chance
str_remove(hcp_taskb$task_b_better_than_even,"%") -> hcp_taskb$task_b_better_than_even
str_remove(hcp_taskb$task_b_improbable,"%") -> hcp_taskb$task_b_improbable
str_remove(hcp_taskb$task_b_chances_are_slight,"%") -> hcp_taskb$task_b_chances_are_slight
str_remove(hcp_taskb$task_b_little_chance,"%") -> hcp_taskb$task_b_little_chance
str_remove(hcp_taskb$task_b_highly_unlikely,"%") -> hcp_taskb$task_b_highly_unlikely
str_remove(hcp_taskb$task_b_we_believe,"%") -> hcp_taskb$task_b_we_believe
str_remove(hcp_taskb$task_b_likely,"%") -> hcp_taskb$task_b_likely
str_remove(hcp_taskb$task_b_highly_likely,"%") -> hcp_taskb$task_b_highly_likely
str_remove_all(hcp_taskb$task_b_almost_certainly,'\\+') -> hcp_taskb$task_b_almost_certainly

# Save the cleansed task B data from patients. 
write.csv(hcp_taskb, ".git/Dataset/hcp_taskb.csv", row.names=FALSE)

sapply(hcp_taskb, class) 
hcp_taskb <- as.data.frame(apply(hcp_taskb, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

pivot_longer(hcp_taskb, -id, names_to="term", values_to = "ratings") %>%
  mutate(ratings = as.numeric(ratings), 
         term = substring(term, 8,100)) %>%
  filter(!is.na(ratings)) -> hcp_taskb_long

hcp_taskb %>% 
  select(-id) %>% 
  colMeans(na.rm = TRUE) %>% 
  sort() %>% 
  names() %>%
  substring(8,100) -> term_order_hcp

# Ploting 
hcp_taskb_long %>%
  mutate(term= factor(term, levels=term_order_hcp, ordered = TRUE)) %>%
  ggplot(aes(x = ratings, y = term, height = stat(density))) +
  geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = FALSE) +
  labs(title = 'Ratings of terms in doctors') 

# Pairwise correlation test between participants in Task B.
expand.grid(id1=hcp_taskb$id, id2=hcp_taskb$id) %>%
  mutate(cor = purrr::map2_dbl(id1, id2, function(id1,id2){
    id1_rating=unlist(hcp_taskb[hcp_taskb$id == id1,])
    id2_rating=unlist(hcp_taskb[hcp_taskb$id == id2,])
    cor(id1_rating, id2_rating, method = 'k', use = 'pairwise')
  })) %>%
  filter(id1 != id2) -> cor_taskb_hcp

cor_taskb_hcp %>%
  filter(cor!=0)%>%
  group_by(id1) %>% 
  summarise(mean_cor_b = mean(cor)) %>% 
  arrange(mean_cor_b) %>%
  mutate(rank_cor_b = row_number()) -> cor_ave_taskb_hcp

# Plotting
cor_taskb_hcp %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = cor_ave_taskb_hcp$id1, ordered = TRUE),
    id2 = factor(id2, levels = cor_ave_taskb_hcp$id1, ordered = TRUE)
  ) -> cor_taskb1_hcp

cor_taskb1_hcp %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "x", expand = c(0,0)) +
  scale_y_continuous(name = "y", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Task B Rating Pairwise Ordinal Correlations within doctors"))

# Descriptive analysis.
hcp_taskb <- hcp_taskb[sapply(hcp_taskb, is.numeric)]  
hcp_taskb_sd <- apply(hcp_taskb, 2, sd, na.rm=TRUE)
hcp_taskb_mean <- apply(hcp_taskb, 2, mean, na.rm=TRUE)
hcp_taskb_median <- apply(hcp_taskb, 2, median, na.rm=TRUE)

hcp_taskb_display <- hcp_taskb_display[sapply(hcp_taskb_display, is.numeric)]  
hcp_taskb_display_sd <- apply(hcp_taskb_display, 2, sd, na.rm=TRUE)
hcp_taskb_display_mean <- apply(hcp_taskb_display, 2, mean, na.rm=TRUE)
hcp_taskb_display_median <- apply(hcp_taskb_display, 2, median, na.rm=TRUE)

