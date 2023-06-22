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
library(ggridges) # PlackettLuce
library(PlackettLuce) # PlackettLuce


theme_set(theme_bw())
theme_set(theme_classic())

# In this coding file, we only verified the data cleaning through fertility patients
# data set since the fertility professionals will have the exactly same verification process.
# So far, this coding is all served for a poster I made for 2023 MathPsych conference.
# In this case, it will include 3 graphs (pairwise correlation, distribution graph)
# and scatterplot between item corr. and centroid corr.) presenting the clustering problem 
# in the patient's data set. Then for the comparison/ verification, there are the 
# comparison graphs between the results from Plackett Luce model; the results from
# rating ..?
# 


# Load raw data sets.
fertility_patient = here::here("Fertility_probability_judgement/Dataset/FP_n281.sav")
fp_all = haven::read_sav(fertility_patient)

# Load clean data sets. 
fertility_patient_cleanta = here::here("Fertility_probability_judgement/Dataset/fp_taska.csv")
fp_ta = read.csv(fertility_patient_cleanta)
fertility_patient_cleantb = here::here("Fertility_probability_judgement/Dataset/fp_taskb.csv")
fp_tb = read.csv(fertility_patient_cleantb)

# First, presentation of latent clustering. 
# The correlation between participant's ordering and original item strings ordering, FP.
data.frame(id1 = fp_all$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(fp_all[fp_all$id == id1, task_a_columns_all]) 
      if(all(is.na(a))) return(NA)
      id1_responses = a %>% 
        sort() %>%
        names() %>%
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all(
          pattern = '_', replacement = ' '
        )
      item_string_order = strsplit(fp_all[fp_all$id == id1,]$item_string,split = ';') [[1]]
      cor(1:16, match(table = id1_responses, item_string_order), method = 'k')
    })) -> task_a_ori_cor_all

expand.grid(id1 = fp_all$id, id2 = 14) %>%
  mutate(
    centroid_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_all[fp_all$id == id1, task_a_columns_all])
      id2_responses = unlist(fp_all[fp_all$id == id2, task_a_columns_all])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(centroid_cor) 
  ) -> task_a_centroid_cor_all

cor_best_taska_fp %>% 
  left_join(cors_item_taska_fp, by="id1") -> joined_cor_best_item_taska_fp
joined_cor_best_item_taska_fp %>% ggplot(aes(best_cor_fp, item_string_cor_fp)) +
  geom_point() +
  theme_minimal()+
  labs(title = 'Scatterplot of the patients sample', x = 'Correlation w/ centroid', y = 'Correlation w/ random start') 


# Distributions graph.
task_a_centroid_cor_all %>% 
  filter(centroid_cor != max(centroid_cor)) %>% 
  pull(centroid_cor)  -> task_a_centroid_cor_all
# <Figure 4>
hist(task_a_centroid_cor_all,freq = FALSE,breaks = 20)  

task_a_mle_fun_all <- function(par, y) {
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

# Distribution graph with clean data set. 
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

# Second, we want to see the pairwise correlation graph for both raw and clean data, FP.
# Raw Ordering data.
colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_allraw
fp_all <- fp_all %>%
  select(all_of(task_a_columns_allraw), item_string)  %>%
  mutate(id = row_number())

colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_all

expand.grid(id1 = fp_all$id, id2 = fp_all$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_all[fp_all$id == id1, task_a_columns_all])
      id2_responses = unlist(fp_all[fp_all$id == id2, task_a_columns_all])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> task_a_pairwise_all

task_a_pairwise_all %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> task_a_avr_cor_all

task_a_pairwise_all %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = task_a_avr_cor_all$id1, ordered = TRUE),
    id2 = factor(id2, levels = task_a_avr_cor_all$id1, ordered = TRUE)
  ) -> task_a_pairwise_all_ordered

task_a_pairwise_all_ordered %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations BEFORE latent categorisation"))

# Clean Ordering data.
colnames(fp_ta) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_ta

expand.grid(id1 = fp_ta$id, id2 = fp_ta$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_ta[fp_ta$id == id1, task_a_columns_ta])
      id2_responses = unlist(fp_ta[fp_ta$id == id2, task_a_columns_ta])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> task_a_pairwise_ta

task_a_pairwise_ta %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> task_a_avr_cor_ta

task_a_pairwise_ta %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = task_a_avr_cor_ta$id1, ordered = TRUE),
    id2 = factor(id2, levels = task_a_avr_cor_ta$id1, ordered = TRUE)
  ) -> task_a_pairwise_ta_ordered

task_a_pairwise_ta_ordered %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations AFTER"))

# Third, we would like to see the differences between raw and clean data from 
# Plackett Luce model (it will give you the ordering of the terms in a quantity
# scale.)

# Plackett_Luce model analysis OLD data sets
colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_fp
fp_all1 <- fp_all %>%
  select(task_a_columns_fp, item_string)  %>%
  mutate(id = row_number())

# Task A inferential analysis.
col_order_fp <- c("task_a_almost_certainly", "task_a_highly_likely",   
                  "task_a_very_good_chance",  "task_a_likely", 
                  "task_a_probable", "task_a_probably",
                  "task_a_we_believe", "task_a_better_than_even",
                  "task_a_about_even", "task_a_chances_are_slight",
                  "task_a_we_doubt", "task_a_little_chance",
                  "task_a_probably_not", "task_a_unlikely",
                  "task_a_improbable",  "task_a_highly_unlikely",
                  "task_a_almost_no_chance")

fp_ranked <- fp_all1[, col_order_fp] %>% 
  mutate(id = row_number())
fp_long <- pivot_longer(fp_ranked, cols = c(1:17), names_to ="term", values_to = "ranking") 

fp_pl_format <- pivot_wider(fp_long, names_from = "ranking", values_from = "term") %>%
  select(-id)
order <- c("1","2","3","4","5","6","7","8","9","10","11","12",
           "13","14","15","16","NA")
fp_pl_format <- fp_pl_format[, order] 

fp_pl_format %>%
  select(-"NA") ->fp_pl_format

fp_pl_ranked <- as.rankings(fp_pl_format, input = "orderings")

pmodel_fp <- PlackettLuce(fp_pl_ranked, npseudo = 0) 
qv_fp <- qvcalc(pmodel_fp)
qv_fp$qvframe <- qv_fp$qvframe[order(coef(pmodel_fp)),]

# <Figure 8>: Rankings of terms given by Plackettluce model (patients).
fp_all1 <- as.data.frame(apply(fp_all1, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)
fp_all1 %>%
  select(-id) %>% 
  colMeans(na.rm = TRUE) %>% 
  sort() %>% 
  names() %>%
  substring(8,100) %>%
  str_replace("_", " ")-> term_order_fp
plot(qv_fp,  
     main = "Rankings of terms given by Plackettluce (patients)",
     xaxt="n", xlim = c(1, 17), ylim = c(-6,6))
axis(1, at = seq_len(17), labels = term_order_fp, las = 2, cex.axis = 0.6)
coefs_pl_fp <- round(coef(pmodel_fp), 2) %>% sort()



# Plackett_Luce model analysis CLEAN data sets
# Task A inferential analysis.
col_order_fp <- c("task_a_almost_certainly", "task_a_highly_likely",   
                  "task_a_very_good_chance",  "task_a_likely", 
                  "task_a_probable", "task_a_probably",
                  "task_a_we_believe", "task_a_better_than_even",
                  "task_a_about_even", "task_a_chances_are_slight",
                  "task_a_we_doubt", "task_a_little_chance",
                  "task_a_probably_not", "task_a_unlikely",
                  "task_a_improbable",  "task_a_highly_unlikely",
                  "task_a_almost_no_chance")

fp_ranked <- fp_ta[, col_order_fp] %>% 
  mutate(id = row_number())
fp_long <- pivot_longer(fp_ranked, cols = c(1:17), names_to ="term", values_to = "ranking") 

fp_pl_format <- pivot_wider(fp_long, names_from = "ranking", values_from = "term") %>%
  select(-id)
order <- c("1","2","3","4","5","6","7","8","9","10","11","12",
           "13","14","15","16","NA")
fp_pl_format <- fp_pl_format[, order] 

fp_pl_format %>%
  select(-"NA") ->fp_pl_format

fp_pl_ranked <- as.rankings(fp_pl_format, input = "orderings")

pmodel_fp <- PlackettLuce(fp_pl_ranked, npseudo = 0) 
qv_fp <- qvcalc(pmodel_fp)
qv_fp$qvframe <- qv_fp$qvframe[order(coef(pmodel_fp)),]

# <Figure 8>: Rankings of terms given by Plackettluce model (patients).
fp_ta <- as.data.frame(apply(fp_ta, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)
fp_ta %>%
  select(-id, -item_string) %>% 
  colMeans(na.rm = TRUE) %>% 
  sort() %>% 
  names() %>%
  substring(8,100) %>%
  str_replace("_", " ")-> term_order_fp
plot(qv_fp,  
     main = "Rankings of terms given by Plackettluce (patients)",
     xaxt="n", xlim = c(1, 17), ylim = c(-6,6))
axis(1, at = seq_len(17), labels = term_order_fp, las = 2, cex.axis = 0.6)
coefs_pl_fp <- round(coef(pmodel_fp), 2) %>% sort()

















# Old data set analysis. 
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

