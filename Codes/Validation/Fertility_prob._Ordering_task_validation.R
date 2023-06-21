#lOAD libraries (unsure about what each library do, will specify)
library(tidyverse)
library(tidyr)
library(haven) 
library(dplyr)
library(gdata) #remane.vars
library(data.table) #sprsplit & table 
library(here)
library(renv)
library(data.table) 
library(ggplot2)

theme_set(theme_bw())
theme_set(theme_classic())


# Load Fertility Patients (FP) dataset.
fertility_patient = here::here("Fertility_probability_judgement/Dataset/FP_n281.sav")
fp_all = haven::read_sav(fertility_patient)

# Load Fertility Health Care Professional (HCP) dataset.
fertility_doctors = here::here("Fertility_probability_judgement/Dataset/HCP_n263.sav")
hcp_all = haven::read_sav(fertility_doctors)

# Organize datasets.
colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_fp
fp <- fp_all %>%
  select(task_a_columns_fp, item_string)  %>%
  mutate(id = row_number())

colnames(hcp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_hcp
hcp <- hcp_all %>%
  select(task_a_columns_hcp, item_string)  %>%
  mutate(id = row_number())

# Pairwise correlation between individual participant given their ordered ranking, FP. 
colnames(fp) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_cleanfp

expand.grid(id1 = fp$id, id2 = fp$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp[fp$id == id1, task_a_columns_cleanfp])
      id2_responses = unlist(fp[fp$id == id2, task_a_columns_cleanfp])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> task_a_pairwise_fp

task_a_pairwise_fp %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> task_a_avr_cor_fp

# Pairwise ordinal correlations graph, FP. 
task_a_pairwise_fp %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = task_a_avr_cor_fp$id1, ordered = TRUE),
    id2 = factor(id2, levels = task_a_avr_cor_fp$id1, ordered = TRUE)
    ) -> task_a_pairwise_fp_ordered

task_a_pairwise_fp_ordered %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations between patients"))

# Pairwise correlation between individual participant given their ordered ranking, HCP. 
colnames(hcp) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_cleanhcp

expand.grid(id1 = hcp$id, id2 = hcp$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp[hcp$id == id1, task_a_columns_cleanhcp])
      id2_responses = unlist(hcp[hcp$id == id2, task_a_columns_cleanhcp])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> task_a_pairwise_hcp

task_a_pairwise_hcp %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> task_a_avr_cor_hcp

# Pairwise ordinal correlations graph, HCP. 
task_a_pairwise_hcp %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = task_a_avr_cor_hcp$id1, ordered = TRUE),
    id2 = factor(id2, levels = task_a_avr_cor_hcp$id1, ordered = TRUE)
  ) -> task_a_pairwise_hcp_ordered

task_a_pairwise_hcp_ordered %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations between doctors"))

# The correlation between participant's ordering and original item strings ordering, FP.
data.frame(id1 = fp$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(fp[fp$id == id1, task_a_columns_cleanfp]) 
      if(all(is.na(a))) return(NA)
      id1_responses = a %>% 
        sort() %>%
        names() %>%
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all(
          pattern = '_', replacement = ' '
        )
      item_string_order = strsplit(fp[fp$id == id1,]$item_string,split = ';') [[1]]
      cor(1:16, match(table = id1_responses, item_string_order), method = 'k')
    })) -> task_a_ori_cor_fp 

# Correlation between average and original graph, FP. 
task_a_avr_cor_fp %>% 
  left_join(task_a_ori_cor_fp, by="id1") -> task_a_avr_ori_fp 
task_a_avr_ori_fp %>% ggplot(aes(mean_cor, item_string_cor)) +
  geom_point()
# BACKGROUND, x/y axis name, SORTING NEEDED. 

# The correlation between participant's ordering and original item strings ordering, HCP.
data.frame(id1 = hcp$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(hcp[hcp$id == id1, task_a_columns_cleanhcp]) 
      if(all(is.na(a))) return(NA)
      id1_responses = a %>% 
        sort() %>%
        names() %>%
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all(
          pattern = '_', replacement = ' '
        )
      item_string_order = strsplit(hcp[hcp$id == id1,]$item_string,split = ';') [[1]]
      cor(1:16, match(table = id1_responses, item_string_order), method = 'k')
    })) -> task_a_ori_cor_hcp 

# Correlation between average and original graph, HCP. 
task_a_avr_cor_hcp %>% 
  left_join(task_a_ori_cor_hcp, by="id1") -> task_a_avr_ori_hcp
task_a_avr_ori_hcp %>% ggplot(aes(mean_cor, item_string_cor)) +
  geom_point() 
# BACKGROUND, x/y axis name, SORTING NEEDED. 

# The correlation between each participant's ordering and the centroid participant's ordering, FP.
expand.grid(id1 = fp$id, id2 = 14) %>%
  mutate(
    centroid_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp[fp$id == id1, task_a_columns_cleanfp])
      id2_responses = unlist(fp[fp$id == id2, task_a_columns_cleanfp])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(centroid_cor) 
  ) -> task_a_centroid_cor_fp

# Correlation between average and centroid graph, FP. 
task_a_centroid_cor_fp %>% 
  left_join(task_a_ori_cor_fp, by="id1") -> task_a_avr_centroid_fp
task_a_avr_centroid_fp %>% ggplot(aes(centroid_cor, item_string_cor)) +
  geom_point()

# The correlation between each participant's ordering and the centroid participant's ordering, HCP.
expand.grid(id1 = hcp$id, id2 = 33) %>% # Create a data frame that contains all combinations of IDs
  mutate(
    centroid_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp[hcp$id == id1, task_a_columns_cleanhcp])
      id2_responses = unlist(hcp[hcp$id == id2, task_a_columns_cleanhcp])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(centroid_cor) 
  ) -> task_a_centroid_cor_hcp

# Correlation between average and centroid graph, HCP. 
task_a_centroid_cor_hcp %>% 
  left_join(task_a_ori_cor_hcp, by="id1") -> task_a_avr_centroid_hcp
task_a_avr_centroid_hcp %>% ggplot(aes(centroid_cor, item_string_cor)) +
  geom_point()

# So far, we have seen three kinds of graphs (pairwise correlation, correlation 
# between each participant's ordering and their original item string, correlation 
# between distances from centroid and from original item string) and all graphs 
# visually illustrated the clustering problem within both patients and doctors' dataset.
# Next step, we will utilize the Maximum Likelihood estimation to prove the clustering
# phenomenon mathematically. 

# Maximum likelihood estimation, FP.
task_a_centroid_cor_fp %>% 
  filter(centroid_cor != max(centroid_cor)) %>% 
  pull(centroid_cor)  -> task_a_centroid_cor_fp_data

task_a_mle_fun_fp <- function(par, y) {
  n=15
  sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
  probs= exp(c(par[3:4],0))
  density = (probs[1]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[1]), shape2 = exp(par[2])) +
    # Reversers PDF (red).
    (probs[2]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[2]), shape2= exp(par[1])) +
    # Constant value givers PDF (blue).
    (probs[3]/sum(probs))* dnorm(y,0, sd)
    # Randomizes PDF (green).
  -sum(log(density))
} 

task_a_mle_est_fp <- optim(par=c(-1,5,-1,-2), fn=task_a_mle_fun_fp, y=task_a_centroid_cor_fp_data)
beta_par_fp <- exp(task_a_mle_est_fp$par[1:2]) 
prob_par_fp <- exp(task_a_mle_est_fp$par[3:4]) 

# Distributions graph for three clusters. 
curve(prob_par_fp[2]/sum(prob_par_fp,1)*0.5*dbeta((x+1)/2, shape1 = beta_par_fp[2], shape2 = beta_par_fp[1]),from=-1, to=1, col="blue")
curve(prob_par_fp[1]/sum(prob_par_fp,1)*0.5*dbeta((x+1)/2, shape1 = beta_par_fp[1], shape2 = beta_par_fp[2]),from=-1, to=1, col="red",add=TRUE)
n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
curve(1/sum(prob_par_fp,1)*dnorm(x, 0, sd),from=-1, to=1, add = TRUE, col="green")

# Data exclusion. 
# The value for each participant in each distribution . 
task_a_centroid_cor_fp %>%
  mutate(curve_constant = 0.5*dbeta((task_a_centroid_cor_fp$centroid_cor+1)/2, shape1 = beta_par_fp[2], shape2 = beta_par_fp[1]),
         curve_reverser = 0.5*dbeta((task_a_centroid_cor_fp$centroid_cor+1)/2, shape1 = beta_par_fp[1], shape2 = beta_par_fp[2]),
         curve_middle = (dnorm(task_a_centroid_cor_fp$centroid_cor, 0, sd))) %>%
  # filter the centroid participant out because it is not represented properly in this data set, and we will put it in later. 
  filter(centroid_cor != max(centroid_cor))  %>%
  select(-id2) %>%
  arrange(desc(curve_constant))-> task_a_density_value_fp

# The ration between different distributions, the bigger the ration is, the more confident we are that
# one participant is in one cluster. 
task_a_density_value_fp  %>%
  rowwise() %>%
  mutate(ratio_constant = curve_constant/curve_middle,
         ratio_randomizer1 = curve_middle/curve_constant, 
         ratio_randomizer2 = curve_middle/curve_reverser, 
         ratio_reverser = curve_reverser/curve_middle,
         ratio = NA) -> task_a_density_value_fp
# From 1-162, curve_constant > curve_middle.
task_a_density_value_fp$ratio[1:162] <- task_a_density_value_fp$ratio_constant[1:162]
# From 163-180, curve_middle > curve_constant.
task_a_density_value_fp$ratio[163:180] <- task_a_density_value_fp$ratio_randomizer1[163:180]
# From 181-192, curve_middle > curve_reverser.
task_a_density_value_fp$ratio[181:192] <- task_a_density_value_fp$ratio_randomizer2[181:192]
# From 193-202, curve_reverser > curve_middle.
task_a_density_value_fp$ratio[193:202] <- task_a_density_value_fp$ratio_reverser[193:202]
task_a_density_value_fp %>%
  select(-ratio_constant, -ratio_randomizer1, -ratio_randomizer2, -ratio_reverser) ->
  task_a_ratio_fp

task_a_ratio_fp %>%
  arrange(desc(ratio)) %>%
  # We filter any ration values under 10, meaning we don't have enough confidence which cluster 
  # they should be put in.
  filter(ratio>10) -> task_a_ratio_fp

# We are interested who are constant value givers and reversers. 
task_a_ratio_fp %>%
  filter(curve_middle< curve_constant) -> task_a_constant_fp
task_a_constant_fp$id1-> constant_id_fp

task_a_ratio_fp %>%
  filter(curve_middle< curve_reverser) -> task_a_reverser_fp
task_a_reverser_fp$id1 -> reverser_id_fp

fp[c(constant_id_fp),] -> constant_fp
fp[c(reverser_id_fp),] -> reverser_fp
fp[c(fp$id==14),] -> centroid_fp

# Then we flipped reversers' answers.
max <- 16
mim <- 1
(reverser_fp[1:17] - max/2) * (-1) -> reverser_fp_cal
reverser_fp_cal + max/2 +mim -> reverser_fp_flipped

# Organized and combined clean data set, FP. 
reverser_fp_flipped %>%
  add_column(id = reverser_fp$id) %>%
  add_column(item_string = reverser_fp$item_string) -> reverser_fp_flipped

constant_fp %>% 
  add_row(reverser_fp_flipped) %>%
  add_row(centroid_fp)-> fp_taska_cleansed

# Save the cleansed Ordering data sets from FP. 
write.csv(fp_taska_cleansed, "Fertility_probability_judgement/Dataset/fp_taska_cleansed.csv", row.names=FALSE)


# Next, we will use the same method for the HCP sample. Detailed will not be included. 
# Maximum likelihood estimation, HCP.
task_a_centroid_cor_hcp %>% 
  filter(centroid_cor != max(centroid_cor)) %>%
  pull(centroid_cor)  -> task_a_centroid_cor_hcp_data

task_a_mle_fun_hcp <- function(par, y) {
  n=15
  sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
  probs= exp(c(par[3:4],0))
  density = (probs[1]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[1]), shape2 = exp(par[2])) +
    (probs[2]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[2]), shape2 = exp(par[1])) +
    (probs[3]/sum(probs))* dnorm(y,0, sd)
  -sum(log(density))
} 

task_a_mle_est_hcp <- optim(par=c(-1,5,-1,-2), fn=task_a_mle_fun_hcp, y=task_a_centroid_cor_hcp_data)
beta_par_hcp <- exp(task_a_mle_est_hcp$par[1:2]) 
prob_par_hcp <- exp(task_a_mle_est_hcp$par[3:4]) 

curve(prob_par_hcp[2]/sum(prob_par_hcp,1)*0.5*dbeta((x+1)/2, shape1 = beta_par_hcp[2], shape2 = beta_par_hcp[1]),from=-1, to=1, col="blue")
curve(prob_par_hcp[1]/sum(prob_par_hcp,1)*0.5*dbeta((x+1)/2, shape1 = beta_par_hcp[1], shape2 = beta_par_hcp[2]),from=-1, to=1, col="red",add=TRUE)
n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
curve(1/sum(prob_par_hcp,1)*dnorm(x, 0, sd),from=-1, to=1, add = TRUE, col="green")

# Data exclusion
task_a_centroid_cor_hcp %>%
  mutate(curve_constant = 0.5*dbeta((task_a_centroid_cor_hcp$centroid_cor+1)/2, shape1 = beta_par_hcp[2], shape2 = beta_par_hcp[1]),
         curve_reverser = 0.5*dbeta((task_a_centroid_cor_hcp$centroid_cor+1)/2, shape1 = beta_par_hcp[1], shape2 = beta_par_hcp[2]),
         curve_middle = (dnorm(task_a_centroid_cor_hcp$centroid_cor, 0, sd))) %>%
  filter(centroid_cor != max(centroid_cor)) %>%
  select(-id2) %>%
  arrange(desc(curve_constant))-> task_a_density_value_hcp

task_a_density_value_hcp  %>%
  rowwise() %>%
  mutate(ratio_constant = curve_constant/curve_middle,
         ratio_randomizer1 = curve_middle/curve_constant, 
         ratio_randomizer2 = curve_middle/curve_reverser, 
         ratio_reverser = curve_reverser/curve_middle,
         ratio = NA) -> task_a_density_value_hcp

task_a_density_value_hcp$ratio[1:130] <- task_a_density_value_hcp$ratio1_130[1:130]
task_a_density_value_hcp$ratio[131:145] <- task_a_density_value_hcp$ratio131_145[131:145]
task_a_density_value_hcp$ratio[146:148] <- task_a_density_value_hcp$ratio146_148[146:148]
task_a_density_value_hcp$ratio[149:152] <- task_a_density_value_hcp$ratio149_152[149:152]
task_a_density_value_hcp %>%
  select(-ratio_constant, -ratio_randomizer1, -ratio_randomizer2, -ratio_reverser) ->
  task_a_ratio_hcp


# We are interested who are constant value givers and reversers. 
task_a_ratio_fp %>%
  filter(curve_middle< curve_constant) -> task_a_constant_fp
task_a_constant_fp$id1-> constant_id_fp

task_a_ratio_hcp %>%
  filter(curve_middle< curve_constant) -> task_a_constant_hcp
task_a_constant_hcp$id1-> constant_id_hcp

task_a_ratio_hcp %>%
  filter(curve_middle< curve_reverser) -> task_a_reverser_hcp
task_a_reverser_hcp$id1 -> reverser_id_hcp

hcp[c(reverser_id_hcp),] -> reverser_hcp
hcp[c(constant_id_hcp),] -> constant_hcp
hcp[c(hcp$id==33),] -> centroid_hcp

max <- 16
mim <- 1
(reverser_hcp[1:17] - max/2) * (-1) -> reverser_hcp_cal
reverser_hcp_cal + max/2 +mim -> reverser_hcp_flipped

reverser_hcp_flipped %>%
  add_column(id = reverser_hcp$id) %>%
  add_column(item_string = reverser_hcp$item_string)-> reverser_hcp_flipped

constant_hcp %>% 
  add_row(reverser_hcp_flipped) %>%
  add_row(centroid_hcp) -> hcp_taska_cleansed

# Save the cleansed Ordering data sets from HCP. 
write.csv(hcp_taska_cleansed, "Fertility_probability_judgement/Dataset/hcp_taska_cleansed.csv", row.names=FALSE)



# Verification. 
fertility_patient = here::here(".git/Dataset/fp_taska.csv")
fp_taska <- read.csv(fertility_patient)

colnames(fp_taska) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
expand.grid(id1 = fp_taska$id, id2 = fp_taska$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_taska[fp_taska$id == id1, task_a_columns])
      id2_responses = unlist(fp_taska[fp_taska$id == id2, task_a_columns])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> cor_all_taska

cor_all_taska %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> cor_ave_taska 

cor_all_taska %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = cor_ave_taska$id1, ordered = TRUE),
    id2 = factor(id2, levels = cor_ave_taska$id1, ordered = TRUE)
  ) -> cor_all_taska

cor_all_taska %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations between patients"))

# Descriptive analysis.
fp <- fp[sapply(fp, is.numeric)]  
fp_a_before_sd <- apply(fp, 2, sd, na.rm=TRUE)
fp_a_before_mean <- apply(fp, 2, mean, na.rm=TRUE)
fp_a_before_median <- apply(fp, 2, median, na.rm=TRUE)

fp_taska  <- fp_taska[sapply(fp_taska, is.numeric)] 
fp_a_after_sd <- apply(fp_taska, 2, sd, na.rm=TRUE)
fp_a_after_mean <- apply(fp_taska, 2, mean, na.rm=TRUE)
fp_a_after_median <- apply(fp_taska, 2, median, na.rm=TRUE)

#----------------------------------------------------------------------------------------------------




# Save the cleansed task A data from doctors. 
write.csv(hcp_taska, ".git/Dataset/hcp_taska.csv")

fertility_doctors = here::here(".git/Dataset/hcp_taska.csv")
hcp_taska <- read.csv(fertility_doctors)

colnames(hcp_taska) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
expand.grid(id1 = hcp_taska$id, id2 = hcp_taska$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp_taska[hcp_taska$id == id1, task_a_columns])
      id2_responses = unlist(hcp_taska[hcp_taska$id == id2, task_a_columns])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> cor_all_taska

cor_all_taska %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> cor_ave_taska 

cor_all_taska %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = cor_ave_taska$id1, ordered = TRUE),
    id2 = factor(id2, levels = cor_ave_taska$id1, ordered = TRUE)
  ) -> cor_all_taska

cor_all_taska %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations between doctors"))

# Descriptive analysis.
hcp <- hcp[sapply(hcp, is.numeric)]  
hcp_a_before_sd <- apply(hcp, 2, sd, na.rm=TRUE)
hcp_a_before_mean <- apply(hcp, 2, mean, na.rm=TRUE)
hcp_a_before_median <- apply(hcp, 2, median, na.rm=TRUE)

hcp_taska  <- hcp_taska[sapply(hcp_taska, is.numeric)] 
hcp_a_after_sd <- apply(hcp_taska, 2, sd, na.rm=TRUE)
hcp_a_after_mean <- apply(hcp_taska, 2, mean, na.rm=TRUE)
hcp_a_after_median <- apply(hcp, 2, median, na.rm=TRUE)
