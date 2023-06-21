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


# Load Fertility patients dataset.
fertility_patient = here::here("Fertility_probability_judgement/Dataset/FP_n281.sav")
fp_all = haven::read_sav(fertility_patient)

# Load Fertility doctors dataset.



colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_1
fp <- fp_all %>%
  select(task_a_columns_1, item_string)  %>%
  mutate(id = row_number())

# The pairwise correlation of the rankings given from one participant and from all other participants.
colnames(fp) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
expand.grid(id1 = fp$id, id2 = fp$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp[fp$id == id1, task_a_columns])
      id2_responses = unlist(fp[fp$id == id2, task_a_columns])
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

# Plotting over the pairwise correlations. 
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

# The correlation between the rankings given from each participant and their original item strings in the survey. 
data.frame(id1 = fp$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(fp[fp$id == id1, task_a_columns]) 
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
    })) -> cors_item_taska 

# Plot the relation between the mean correlation and item correlation. 
cor_ave_taska %>% 
  left_join(cors_item_taska, by="id1") -> joined_cor_ave_item_taska
joined_cor_ave_item_taska %>% ggplot(aes(mean_cor, item_string_cor)) +
  geom_point() 

# Correlation between the ranking given from the best participant and from all other participants. 
expand.grid(id1 = fp$id, id2 = 14) %>%
  mutate(
    best_cor = purrr::map2_dbl(id2, id1, function(id2, id1){
      id1_responses = unlist(fp[fp$id == id2, task_a_columns])
      id2_responses = unlist(fp[fp$id == id1, task_a_columns])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(best_cor) 
  ) -> cor_best_taska

#Plot the relation between the mean correlation and item correlation. 
cor_best_taska %>% 
  left_join(cors_item_taska, by="id1") -> joined_cor_best_item_taska
joined_cor_best_item_taska %>% ggplot(aes(best_cor, item_string_cor)) +
  geom_point()

# Maximum likelihood estimation for task A ranking data from patients.
cor_best_taska %>% 
  filter(best_cor != max(best_cor)) %>% 
  pull(best_cor)  -> cor_best_taska_df

model_esti_taska <- function(par, y) {
  n=15
  sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
  probs= exp(c(par[3:4],0))
  density = (probs[1]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[1]), shape2 = exp(par[2])) +
    (probs[2]/sum(probs))*0.5*dbeta((y+1)/2, shape2 = exp(par[1]), shape1 = exp(par[2])) +
    (probs[3]/sum(probs))* dnorm(y,0, sd)
  -sum(log(density))
} 

parameter_esti_taska <- optim(par=c(-1,5,-1,-2), fn=model_esti_taska, y=cor_best_taska_df)
parameter_esti_taska 
shape_par <- exp(parameter_esti_taska$par[1:2]) 
shape_par
pi_par <- exp(parameter_esti_taska$par[3:4]) 
pi_par

# Plot the curves of each distribution. 
curve(0.8045172*0.5*dbeta((x+1)/2, shape2 = shape_par[1], shape1 = shape_par[2]),from=-1, to=1, col="blue",add=TRUE)
curve(0.04964689*0.5*dbeta((x+1)/2, shape1 = shape_par[1], shape2 = shape_par[2]),from=-1, to=1, col="red",add=TRUE)
n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
curve(0.1458359*dnorm(x, 0, sd),from=-1, to=1, add = TRUE, col="green")

n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
cor_best_taska %>%
  mutate(curve_most = 0.5*dbeta((cor_best_taska$best_cor+1)/2, shape2 = 3.10, shape1 = 27.23),
         curve_r = 0.5*dbeta((cor_best_taska$best_cor+1)/2, shape1 = 3.10, shape2 = 27.23),
         curve_middle = (dnorm(cor_best_taska$best_cor, 0, sd))) %>%
  filter(best_cor != max(best_cor)) -> cor_curve_height_taska

cor_curve_height_taska %>%
  select(-id2) %>%
  arrange(desc(cor_curve_height_taska$curve_most))-> cor_h_only_taska

cor_h_only_taska  %>%
  rowwise() %>%
  mutate(ratio1_162 = curve_most/curve_middle,
         ratio163_180 = curve_middle/curve_most, 
         ratio181_192 = curve_middle/curve_r, 
         ratio193_202 = curve_r/curve_middle,
         ratio = NA) -> cor_h_only_taska
cor_h_only_taska$ratio[1:162] <- cor_h_only_taska$ratio1_162[1:162]
cor_h_only_taska$ratio[163:180] <- cor_h_only_taska$ratio163_180[163:180]
cor_h_only_taska$ratio[181:192] <- cor_h_only_taska$ratio181_192[181:192]
cor_h_only_taska$ratio[193:202] <- cor_h_only_taska$ratio193_202[193:202]
cor_h_only_taska %>%
  select(-ratio1_162, -ratio163_180, -ratio181_192, -ratio193_202) ->
  cor_ratio

cor_ratio %>%
  arrange(desc(ratio)) %>%
  filter(ratio>10)  -> cor_ratio

cor_ratio %>%
  filter(curve_middle< curve_most) -> cor_ratio_most
cor_ratio_most$id1-> most_id

cor_ratio %>%
  filter(curve_middle< curve_r) -> cor_ratio_re
cor_ratio_re$id1 -> reverser_id

fp[c(reverser_id),] -> fp_reverser
fp[c(most_id),] -> fp_most
fp[c(fp$id==14),] -> fp_best_14

max <- 16
mim <- 1
(fp_reverser[1:17] - max/2) * (-1) -> new
new + max/2 +mim -> fp_reverser_flipped

fp_reverser_flipped %>%
  add_column(id = fp_reverser$id) %>%
  add_column(item_string = fp_reverser$item_string) -> fp_reverser_flipped

fp_most %>% 
  add_row(fp_reverser_flipped) %>%
  add_row(fp_best_14)-> fp_taska

# Save the cleansed task A data from patients. 
write.csv(fp_taska, ".git/Dataset/fp_taska.csv", row.names=FALSE)

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
# Health care professional dataset.
fertility_doctors = here::here(".git/Dataset/HCP_n263.sav")
hcp_all = haven::read_sav(fertility_doctors)

colnames(hcp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_1
hcp <- hcp_all %>%
  select(task_a_columns_1, item_string)  %>%
  mutate(id = row_number())

# The pairwise correlation of the rankings given from one participant and from all other participants.
colnames(hcp) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
expand.grid(id1 = hcp$id, id2 = hcp$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp[hcp$id == id1, task_a_columns])
      id2_responses = unlist(hcp[hcp$id == id2, task_a_columns])
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

# Plotting over the pairwise correlations. 
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

# The correlation between the rankings given from each participant and their original item strings in the survey. 
data.frame(id1 = hcp$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(hcp[hcp$id == id1, task_a_columns]) 
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
    })) -> cors_item_taska 

# Plot the relation between the mean correlation and item correlation. 
cor_ave_taska %>% 
  left_join(cors_item_taska, by="id1") -> joined_cor_ave_item_taska
joined_cor_ave_item_taska %>% ggplot(aes(mean_cor, item_string_cor)) +
  geom_point() 

# Correlation between the ranking given from the best participant and from all other participants.  
expand.grid(id1 = hcp$id, id2 = 33) %>% # Create a data frame that contains all combinations of IDs
  mutate(
    best_cor = purrr::map2_dbl(id2, id1, function(id2, id1){
      id1_responses = unlist(hcp[hcp$id == id2, task_a_columns])
      id2_responses = unlist(hcp[hcp$id == id1, task_a_columns])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(best_cor) 
  ) -> cor_best_taska

# Plot the relation between the best correlation and item correlation. 
cor_best_taska %>% 
  left_join(cors_item_taska, by="id1") -> joined_cor_best_item_taska
joined_cor_best_item_taska %>% ggplot(aes(best_cor, item_string_cor)) +
  geom_point()

# Maximum likelihood estimation for task A ranking data from doctors.
cor_best_taska %>% 
  filter(best_cor != max(best_cor)) %>%
  pull(best_cor)  -> cor_best_taska_df

model_esti_taska <- function(par, y) {
  n=15
  sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
  probs= exp(c(par[3:4],0))
  density = (probs[1]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[1]), shape2 = exp(par[2])) +
    (probs[2]/sum(probs))*0.5*dbeta((y+1)/2, shape2 = exp(par[1]), shape1 = exp(par[2])) +
    (probs[3]/sum(probs))* dnorm(y,0, sd)
  -sum(log(density))
} 

parameter_esti_taska <- optim(par=c(-1,5,-1,-2), fn=model_esti_taska, y=cor_best_taska_df)
parameter_esti_taska 
shape_par <- exp(parameter_esti_taska$par[1:2])
shape_par
pi_par <- exp(parameter_esti_taska$par[3:4]) 
pi_par

# Plot the curves of each distribution. 
curve(0.8749822*0.5*dbeta((x+1)/2, shape2 = shape_par[1], shape1 = shape_par[2]),from=-1, to=1, col="blue",add=TRUE)
curve(0.02637576*0.5*dbeta((x+1)/2, shape1 = shape_par[1], shape2 = shape_par[2]),from=-1, to=1, col="red",add=TRUE)
n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
curve(0.09864208*dnorm(x, 0, sd),from=-1, to=1, add = TRUE, col="green")

n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
cor_best_taska %>%
  mutate(curve_most = 0.5*dbeta((cor_best_taska$best_cor+1)/2, shape2 = 3.55, shape1 = 23.64),
         curve_r = 0.5*dbeta((cor_best_taska$best_cor+1)/2, shape1 = 3.55, shape2 = 23.64),
         curve_middle = (dnorm(cor_best_taska$best_cor, 0, sd))) %>%
  filter(best_cor != max(best_cor)) -> cor_curve_height_taska

cor_curve_height_taska %>%
  select(-id2) %>%
  arrange(desc(cor_curve_height_taska$curve_most))-> cor_h_only_taska

cor_h_only_taska  %>%
  rowwise() %>%
  mutate(ratio1_130 = curve_most/curve_middle,
         ratio131_145 = curve_middle/curve_most, 
         ratio146_148 = curve_middle/curve_r, 
         ratio149_152 = curve_r/curve_middle,
         ratio = NA) -> cor_h_only_taska
cor_h_only_taska$ratio[1:130] <- cor_h_only_taska$ratio1_130[1:130]
cor_h_only_taska$ratio[131:145] <- cor_h_only_taska$ratio131_145[131:145]
cor_h_only_taska$ratio[146:148] <- cor_h_only_taska$ratio146_148[146:148]
cor_h_only_taska$ratio[149:152] <- cor_h_only_taska$ratio149_152[149:152]
cor_h_only_taska %>%
  select(-ratio1_130, -ratio131_145, -ratio146_148, -ratio149_152) ->
  cor_ratio

cor_ratio %>%
  arrange(desc(ratio)) %>%
  filter(ratio>10)  -> cor_ratio

cor_ratio %>%
  filter(curve_middle< curve_most) -> cor_ratio_most
cor_ratio_most$id1-> most_id

cor_ratio %>%
  filter(curve_middle< curve_r) -> cor_ratio_re
cor_ratio_re$id1 -> reverser_id

hcp[c(reverser_id),] -> hcp_reverser
hcp[c(most_id),] -> hcp_most
hcp[c(hcp$id==33),] -> hcp_best_33

max <- 16
mim <- 1
(hcp_reverser[1:17] - max/2) * (-1) -> new
new + max/2 +mim -> hcp_reverser_flipped

hcp_reverser_flipped %>%
  add_column(id = hcp_reverser$id) %>%
  add_column(item_string = hcp_reverser$item_string)-> hcp_reverser_flipped

hcp_most %>% 
  add_row(hcp_reverser_flipped) %>%
  add_row(hcp_best_33) -> hcp_taska

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
