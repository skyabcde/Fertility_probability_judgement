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
#library(MASS)
# first set seed, check with different seeds later. 
set.seed(10)


theme_set(theme_bw())
theme_set(theme_classic())
here::here()

# Load dataset.
fertility_patient = here::here("Fertility_probability_judgement/Dataset/fp_taska.csv")
fp = read_csv(fertility_patient)
fertility_doctors = here::here("Fertility_probability_judgement/Dataset/hcp_taska.csv")
hcp = read_csv(fertility_doctors)
hcp %>% select(-1) -> hcp

# Convert data to long format and eliminate unnecessary term names. 
fp_long <- pivot_longer(fp, cols = c(1:17), names_to ="term", values_to = "ordering") %>%
  select(-item_string) 

fp_long <- fp_long %>%
  mutate(term = unlist(fp_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_a_') %>%
           stringr::str_replace_all(
             pattern = '_', replacement = ' ')) %>%
  mutate(subject = "fp")

################################################################################
hcp_long <- pivot_longer(hcp, cols = c(1:17), names_to ="term", values_to = "ordering") %>%
  select(-item_string) 
hcp_long <- hcp_long %>% 
  mutate(term = unlist(hcp_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_a_') %>%
           stringr::str_replace_all(
             pattern = '_', replacement = ' ')) %>%
  mutate(subject = "hcp")
################################################################################

bind_rows(fp_long, hcp_long) %>%
  mutate( 
    id = paste0(subject,"_",id)
  ) -> all_long

#all_long %>%
#  group_by(id) %>%
#  summarise(subject = first(subject)) %>%
#  mutate(subject = sample(subject)) -> permute0

#all_long %>%
#  mutate(
#    subject = permute0$subject[match(x = id, permute0$id)]
 #   ) -> permute_all_long

# Calculate the frequency of the orderings of each term from fp and hcp. 
all_long %>%
  mutate(ordering = factor(ordering, levels = 1:16, ordered = TRUE)) %>%
  group_by(subject, term, ordering, .drop = FALSE) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(!is.na(ordering)) -> all_order_n

################################################################################

all_order_n %>%
  split(.$term) %>%
  purrr::map_df(function(df){
    df %>%
      select(-term) %>%
      tidyr::pivot_wider(names_from = subject, values_from = n) %>%
      select(-ordering) %>%
      as.matrix() %>%
      MASS::loglm( ~ 1 + 2, data = .) -> x0
    data.frame(term = df$term[1], X2 = x0$lrt, df = x0$df) 
  }) %>%
  mutate(p.value = pchisq(X2, df, lower.tail = FALSE)) -> X2_real

sum(X2_real$X2)

permute_results_10k <- replicate (10000, simplify = FALSE,
           {all_long %>%
             group_by(id) %>%
             summarise(subject = first(subject)) %>%
             mutate(subject = sample(subject)) -> permute0
           
           all_long %>%
             mutate(
               subject = permute0$subject[match(x = id, permute0$id)]
             ) -> permute_all_long
           
           permute_all_long %>%
             mutate(ordering = factor(ordering, levels = 1:16, ordered = TRUE)) %>%
             group_by(subject, term, ordering, .drop = FALSE) %>%
             summarise(n = n(), .groups = 'drop') %>%
             ungroup() %>%
             filter(!is.na(ordering)) ->permute_order_n
           

           permute_order_n %>%
             split(.$term) %>%
             purrr::map_df(function(df){
               df %>%
                 select(-term) %>%
                 tidyr::pivot_wider(names_from = subject, values_from = n) %>%
                 select(-ordering) %>%
                 as.matrix() %>%
                 MASS::loglm( ~ 1 + 2, data = .) -> x0
               data.frame(term = df$term[1], X2 = x0$lrt, df = x0$df) 
             }) %>%
             mutate(p.value = pchisq(X2, df, lower.tail = FALSE))
           }
           )
save(permute_results_10k, file = "permute_10k.rda") 
load()

################################################################################
permute_results_10k %>%
  purrr::map_dbl(function(df){
    sum(df$X2)
  }) -> X2_permute


quantile(X2_permute,.025)
#146.2995 
quantile(X2_permute,.975)
#220.7496 
sum(X2_real$X2)
#243.1567
t.test(X2_permute, mu = sum(X2_real$X2))
#t = -323.77, df = 9999, p-value < 2.2e-16

hist(X2_permute, xlim = c(100, 300), 
     xlab = "summed Chi-squared scores for 10,000 ramdonmized permutation")
abline(v=sum(X2_real$X2),
       col="dodgerblue3",
       lty=2,
       lwd=2)
abline(v = c(quantile(X2_permute,.025), quantile(X2_permute,.975)), col = "blue", lty = 2, lwd = 2)

getwd()




# almost certainly
#fp_ac <- fp_order_n %>% filter(term == "almost certainly")
#hcp_ac <- hcp_order_n %>% filter(term == "almost certainly")
#all_ac <- merge(data.frame(hcp_ac, row.names=NULL), data.frame(fp_ac, row.names=NULL), 
#      by = 0, all = TRUE)[-1] 
#t_ac <- chisq.test(all_ac$count.x, all_ac$count.y, simulate.p.value = TRUE)


