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
# Scatterplot
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

# <Figure>: 
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
