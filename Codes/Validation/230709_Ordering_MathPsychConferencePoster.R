#lOAD libraries (unsure about what each library do, will specify)
library(ggrepel) # plackett-luce graph
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
fp_allraw = haven::read_sav(fertility_patient)
colnames(fp_allraw) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_allraw
fp_all <- fp_allraw %>%
  select(all_of(task_a_columns_allraw), item_string)  %>%
  mutate(id = row_number())
colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_all


# Load clean data sets. 
fertility_patient_cleanta = here::here("Fertility_probability_judgement/Dataset/fp_taska.csv")
fp_ta = read.csv(fertility_patient_cleanta)
colnames(fp_ta) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_ta

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

expand.grid(id1 = fp_ta$id, id2 = 14) %>%
  mutate(
    centroid_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_ta[fp_ta$id == id1, task_a_columns_ta])
      id2_responses = unlist(fp_ta[fp_ta$id == id2, task_a_columns_ta])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(centroid_cor) 
  ) -> task_a_centroid_cor_ta


# Distributions graph (RAW).
task_a_centroid_cor_all %>% 
  filter(centroid_cor != max(centroid_cor)) %>% 
  pull(centroid_cor)  -> task_a_centroid_cor_all_pulled

hist(task_a_centroid_cor_all_pulled,freq = FALSE,breaks = 20, xlim = c(-1,1), ylim = c(0,8))  

task_a_mle_fun_all <- function(par, y) {
  n=15
  sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
  probs= exp(c(par[3:4],0))
  density = (probs[1]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[1]), shape2 = exp(par[2])) +
    (probs[2]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[2]), shape2 = exp(par[1])) +
    (probs[3]/sum(probs))* dnorm(y,0, sd)
  -sum(log(density))
} 

task_a_mle_est_all <- optim(par=c(-1,5,-1,-2), fn=task_a_mle_fun_all, y=task_a_centroid_cor_all_pulled)
beta_par_all <- exp(task_a_mle_est_all$par[1:2]) 
prob_par_all <- exp(task_a_mle_est_all$par[3:4]) 

curve(prob_par_all[2]/sum(prob_par_all,1)*0.5*dbeta((x+1)/2, shape1 = beta_par_all[2], shape2 = beta_par_all[1]),from=-1, to=1, col="blue",add=TRUE)
curve(prob_par_all[1]/sum(prob_par_all,1)*0.5*dbeta((x+1)/2, shape1 = beta_par_all[1], shape2 = beta_par_all[2]),from=-1, to=1, col="green",add=TRUE)
n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
curve(1/sum(prob_par_all,1)*dnorm(x, 0, sd),from=-1, to=1, add = TRUE, col="red")

# Distributions graph (CLEAN).
#task_a_centroid_cor_ta %>% 
#  filter(centroid_cor != max(centroid_cor)) %>% 
#  pull(centroid_cor)  -> task_a_centroid_cor_ta

#hist(task_a_centroid_cor_ta,freq = FALSE,breaks = 20, xlim = c(-1,1), ylim = c(0,8))  


#task_a_mle_fun_ta <- function(par, y) {
#  n=15
#  sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
# probs= exp(c(par[3:4],0))
#  density = (probs[1]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[1]), shape2 = exp(par[2])) +
#    (probs[2]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[2]), shape2 = exp(par[1])) +
#    (probs[3]/sum(probs))* dnorm(y,0, sd)
#  -sum(log(density))
} 

#task_a_mle_est_ta <- optim(par=c(-1,5,-1,-2), fn=task_a_mle_fun_ta, y=task_a_centroid_cor_ta)
#beta_par_ta <- exp(task_a_mle_est_all$par[1:2]) 
#prob_par_ta <- exp(task_a_mle_est_all$par[3:4]) 

#curve(prob_par_ta[2]/sum(prob_par_ta,1)*0.5*dbeta((x+1)/2, shape1 = beta_par_ta[2], shape2 = beta_par_ta[1]),from=-1, to=1, col="blue",add=TRUE)
#curve(prob_par_ta[1]/sum(prob_par_ta,1)*0.5*dbeta((x+1)/2, shape1 = beta_par_ta[1], shape2 = beta_par_ta[2]),from=-1, to=1, col="red",add=TRUE)
#n=15
#sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
#curve(1/sum(prob_par_ta,1)*dnorm(x, 0, sd),from=-1, to=1, add = TRUE, col="green")

### codes below add color to the scatterplot
# Finished the data clustering based on participants' cluster.  
task_a_centroid_cor_all %>% 
  mutate(curve_constant = 0.5*dbeta((task_a_centroid_cor_all$centroid_cor+1)/2, shape1 = beta_par_all[2], shape2 = beta_par_all[1]),
         curve_reverser = 0.5*dbeta((task_a_centroid_cor_all$centroid_cor+1)/2, shape1 = beta_par_all[1], shape2 = beta_par_all[2]),
         curve_middle = (dnorm(task_a_centroid_cor_all$centroid_cor, 0, sd))) %>%
  # filter the centroid participant out because it is not represented properly in this data set, and we will put it in later. 
  filter(centroid_cor != max(centroid_cor))  %>%
  select(-id2) %>%
  arrange(desc(curve_constant))-> task_a_density_value_all

# The ration between different distributions, the bigger the ration is, the more confident we are that
# one participant is in one cluster. 
task_a_density_value_all  %>%
  rowwise() %>%
  mutate(ratio_constant = curve_constant/curve_middle,
         ratio_randomizer1 = curve_middle/curve_constant, 
         ratio_randomizer2 = curve_middle/curve_reverser, 
         ratio_reverser = curve_reverser/curve_middle,
         ratio = NA) -> task_a_density_value_all
# From 1-162, curve_constant > curve_middle.
task_a_density_value_all$ratio[1:162] <- task_a_density_value_all$ratio_constant[1:162]
# From 163-180, curve_middle > curve_constant.
task_a_density_value_all$ratio[163:180] <- task_a_density_value_all$ratio_randomizer1[163:180]
# From 181-192, curve_middle > curve_reverser.
task_a_density_value_all$ratio[181:192] <- task_a_density_value_all$ratio_randomizer2[181:192]
# From 193-202, curve_reverser > curve_middle.
task_a_density_value_all$ratio[193:202] <- task_a_density_value_all$ratio_reverser[193:202]
task_a_density_value_all %>%
  select(-ratio_constant, -ratio_randomizer1, -ratio_randomizer2, -ratio_reverser) ->
  task_a_ratio_all

task_a_ratio_all %>%
  arrange(desc(ratio)) %>%
  # We filter any ration values under 10, meaning we don't have enough confidence which cluster 
  # they should be put in.
  filter(ratio>10) -> task_a_ratio_included

# We are interested who are constant value givers and reversers. 
task_a_ratio_included %>%
  filter(curve_middle< curve_constant) -> task_a_constant_all
task_a_constant_all$id1-> constant_id_all

task_a_ratio_included %>%
  filter(curve_middle< curve_reverser) -> task_a_reverser_1
task_a_reverser_1$id1 -> reverser_id1
task_a_ratio_included %>%
  filter(curve_reverser> curve_constant) -> task_a_reverser_2
task_a_reverser_2$id1 -> reverser_id2

task_a_ratio_all %>%
  arrange(desc(ratio)) %>%
  filter(ratio <10)  %>%
  filter(ratio >1) -> randomizer_1
randomizer_1$id1 -> randomizer_id1

task_a_ratio_included %>%
  filter(curve_middle> curve_constant) %>%
  filter(curve_middle> curve_reverser)-> randomizer_2
randomizer_2$id1 -> randomizer_id2

fp_all[c(constant_id_all),] -> constant
fp_all[c(reverser_id1),] -> reverser_1
fp_all[c(reverser_id2),] -> reverser_2
fp_all[c(fp_all$id==14),] -> centroid_all

fp_allraw %>% mutate(id=row_number()) -> fp_allraw
fp_allraw[c(randomizer_id1),] -> randomizer_1
fp_allraw[c(randomizer_id2),] -> randomizer_2
fp_allraw[c(reverser_id1),] -> reverser
fp_allraw[c(reverser_id2),] -> reverser

randomizer_1 %>% add_row(randomizer_2)-> randomizer
reverser_1 %>% add_row(reverser_2)-> reverser

randomizer$id -> randomizer_id
reverser$id -> reverser_id
constant$id -> constant_id

task_a_centroid_cor_all %>% 
  left_join(task_a_ori_cor_all, by="id1") -> task_a_cor_centroid_ori_all 

task_a_cor_centroid_ori_all %>% 
  mutate(Clustering = case_when(
    id1 %in% c(randomizer_id) ~ "1Randomizers",
    id1 %in% c(reverser_id) ~ "2Reversers",
    TRUE ~ "3Constant value giver")) -> a

dev.off()
# <Figure 3> Scatterplot b/w best and item ranking cor (patients). 
a %>% ggplot(aes(centroid_cor, item_string_cor, col= Clustering)) +
  geom_point() +
  theme_minimal()+
  labs(title = 'Scatterplot of raw FP sample', x = 'Corr. w/ centroid', y = 'Corr. w/ random start') 



# Second, we want to see the pairwise correlation graph for both raw and clean data, FP.
# Raw Ordering data.
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
  scale_x_continuous(name = "Number of participants less 'typical' (sorted)", expand = c(0,0)) +
  scale_y_continuous(name = "Number of participants less 'typical' (sorted)", expand = c(0,0)) +
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
  scale_x_continuous(name = "Number of participants less 'typical' (sorted)", expand = c(0,0)) +
  scale_y_continuous(name = "Number of participants less 'typical' (sorted)", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation", begin = 0.6, end = 1) + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations AFTER"))

# Third, we would like to see the differences between raw and clean data from 
# Plackett Luce model (it will give you the ordering of the terms in a quantity
# scale.)

# Plackett_Luce model analysis OLD data sets
col_order <- c("task_a_almost_certainly", "task_a_highly_likely",   
                  "task_a_very_good_chance",  "task_a_likely", 
                  "task_a_probable", "task_a_probably",
                  "task_a_we_believe", "task_a_better_than_even",
                  "task_a_about_even", "task_a_chances_are_slight",
                  "task_a_we_doubt", "task_a_little_chance",
                  "task_a_probably_not", "task_a_unlikely",
                  "task_a_improbable",  "task_a_highly_unlikely",
                  "task_a_almost_no_chance")

fp_ranked <- fp_all[, col_order] %>% 
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
qv_fp_all <- qvcalc(pmodel_fp)
qv_fp_all$qvframe <- qv_fp_all$qvframe[order(coef(pmodel_fp)),]

# <Figure>: 
fp_all <- as.data.frame(apply(fp_all, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)
fp_all %>%
  select(-id) %>% 
  colMeans(na.rm = TRUE) %>% 
  sort() %>% 
  names() %>%
  substring(8,100) %>%
  str_replace("_", " ")-> term_order_fp
plot(qv_fp,  
     main = "Terms ordering by Plackett-Luce model (BEFORE)",
     xaxt="n", xlim = c(1, 17), ylim = c(-6,6))
axis(1, at = seq_len(17), labels = term_order_fp, las = 2, cex.axis = 0.6)

coefs_pl_fp <- round(coef(pmodel_fp), 2) %>% sort()



# Plackett_Luce model analysis CLEAN data sets
fp_ranked <- fp_ta[, col_order] %>% 
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
qv_fp_clean <- qvcalc(pmodel_fp)
qv_fp_clean$qvframe <- qv_fp_clean$qvframe[order(coef(pmodel_fp)),]

# <Figure 8>: Rankings of terms given by Plackettluce model (patients).
fp_ta <- as.data.frame(apply(fp_ta, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)
fp_ta %>%
  select(-id, -item_string) %>% 
  colMeans(na.rm = TRUE) %>% 
  sort() %>% 
  names() %>%
  substring(8,100) %>%
  str_replace_all("_", " ")-> term_order_fp
plot(qv_fp,  
     main = "Terms ordering by Plackett-Luce model (AFTER)",
     xaxt="n", xlim = c(1, 17), ylim = c(-6,6))
axis(1, at = seq_len(17), labels = term_order_fp, las = 2, cex.axis = 0.6)
coefs_pl_fp <- round(coef(pmodel_fp), 2) %>% sort()


##

mtcars
clean_es <- qv_fp_clean$qvframe[1]
clean_er <- qv_fp_clean$qvframe[2]

all_es <- qv_fp_all$qvframe[1]
all_er <- qv_fp_all$qvframe[2]

placket <- data.frame(clean_es, clean_er, all_es, all_er)
dev.off()
p <- ggplot(placket, aes(estimate.1, estimate, label = term_order_fp, color= term_order_fp, vjust=0.6)) +
  geom_point() +
  theme_minimal() +  
  labs(x = 'Plackett-Luce estimate BEFORE', y = 'Plackett-Luce estimate AFTER') +
  geom_text_repel() + 
  geom_abline(intercept = 0, slope = 1)+
  xlim(c(-6,6))+
  ylim(c(-6,6))
p

