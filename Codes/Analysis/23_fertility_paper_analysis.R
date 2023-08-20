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
fertility_patient_taska = here::here("Fertility_probability_judgement/Dataset/fp_taska.csv")
fp = read_csv(fertility_patient_taska)
fertility_doctors_taska = here::here("Fertility_probability_judgement/Dataset/hcp_taska.csv")
hcp = read_csv(fertility_doctors_taska)
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

hcp_long <- pivot_longer(hcp, cols = c(1:17), names_to ="term", values_to = "ordering") %>%
  select(-item_string) 
hcp_long <- hcp_long %>% 
  mutate(term = unlist(hcp_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_a_') %>%
           stringr::str_replace_all(
             pattern = '_', replacement = ' ')) %>%
  mutate(subject = "hcp")

################################################################################
################################################################################

bind_rows(fp_long, hcp_long) %>% # there will be identical numbers for the id for both
  # data sets. 
  mutate( 
    id = paste0(subject,"_",id) # unique id number for participants in each data sets. 
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
  mutate(ordering = factor(ordering, levels = 1:16, ordered = TRUE)) %>% # give a ordering
  # to each term. 
  group_by(subject, term, ordering, .drop = FALSE) %>%
  summarise(n = n()) %>% # count the every term for how many orderings appear. 
  ungroup() %>% # drop the last thing that you group but still group by the first thing
  filter(!is.na(ordering)) -> all_order_n

################################################################################

all_order_n %>%
  
  split(.$term) %>% # . refers to the last thing you pass in; "split" means you are 
  # going to split the whole data sets ". (all_order_n)" by vector "$ term". So you get
  # a list of data frame that you split by term. 
  
  purrr::map_df(function(df){ # We want to do the same thing to every one of the term. 
    # purrr:: map () let you call a function on every element of the list. _df is what
    # comes out from the map function. Now I name the df from the map_df function "df"
    # and I give a function to every "df", which is inside the {} braket.  
  
    df %>%
    select(-term) %>%
    tidyr::pivot_wider(names_from = subject, values_from = n) %>%
    select(-ordering) %>%
    as.matrix() %>%
    MASS::loglm( ~ 1 + 2, data = .) -> x0
    
  data.frame(term = df$term[1], X2 = x0$lrt, df = x0$df) # Lastly, create a data frame
    # to put all the things that I want. 
  }) %>%
  mutate(p.value = pchisq(X2, df, lower.tail = FALSE))  -> X2_real# compute the p_value

sum(X2_real$X2)

df %>%
  select(-term) %>% # we don't want the terms here right now. 
  tidyr::pivot_wider(names_from = subject, values_from = n) %>% # we need to arrange 
  # it so that subjects are in the each column. 
  select(-ordering) %>% # we don't need ordering in chi squared test as well.
  as.matrix() %>% # the format that chi squared test can digest is matrix, so we make
  # the list into as.matrix()
  MASS::loglm( ~ 1 + 2, data = .) -> x0 # however, chi squared test can't digest variale
  # equals 0. Therefore, we use likelihood ratio contingency test. data = .
  # library(mass) has a function called select so when we load the whole library, 
  # R will use the new library to run function select(). Therefore, when we only need 
  # one function in a library, we write MASS::function(). unload()
  # str() of the object, we can look inside the results. 

################################################################################
# Permutation test. 
# Need to keep the subject number in the original data frame because how the permuation
# test would do it relabelize. 

permute_results_10k <- replicate (10000, simplify = FALSE,
           { # function in the replicate function. 
             all_long %>%
             group_by(id) %>% # group_by is an embedded function, it tells you nothing from
               # the output.
             summarise(subject = first(subject))  %>% # after the group_by, we just need one 
               # of the subjects out of the same participant. The first element of the first 
               # subject vector. Just need one thing to sample. 
             mutate(subject = sample(subject)) -> permute0
               # randomly sample the subject to each subject, now the participant from fp data
               # sets doesn't necessary be sampled as the fp participant. 

           
           all_long %>%
             mutate(
               subject = permute0$subject[match(x = id, permute0$id)] # match the sampled id 
               # number in last data frame "permute0" with the "all_long" id. New random ordering
               # for random fp/hcp samples.
               ) -> permute_all_long
           
           permute_all_long %>%
             mutate(ordering = factor(ordering, levels = 1:16, ordered = TRUE)) %>%
             group_by(subject, term, ordering, .drop = FALSE) %>%
             summarise(n = n(), .groups = 'drop') %>% # .groups = 'drop' get rid of the warning.
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
#  set.seed(10)
base::load("Fertility_probability_judgement/permute_10k.rda")

permute_results_10k %>%
  purrr::map_dbl(function(df){ # df is just a small holder name, it doesn't mean anything. 
    # it holds the dbl from map_dbl() from "permute_results_10k".
    sum(df$X2) # calculate the X2 from holder df. 
  }) -> X2_permute

################################################################################
################################################################################

quantile(X2_permute,.025)
#147.3924  
quantile(X2_permute,.975)
#220.3735 
sum(X2_real$X2)
#243.1567
t.test(X2_permute, mu = sum(X2_real$X2)) # Is this correct?  
# t = -328.58, df = 9999, p-value < 2.2e-16

hist(X2_permute, xlim = c(100, 300), 
     xlab = "summed Chi-squared scores for 10,000 ramdonmized permutation")
abline(v=sum(X2_real$X2),
       col="dodgerblue3",
       lty=2,
       lwd=2)
abline(v = c(quantile(X2_permute,.025), quantile(X2_permute,.975)), col = "blue", lty = 2, lwd = 2)

getwd()


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# Load dataset.
fertility_patient_taskb = here::here("Fertility_probability_judgement/Dataset/fp_taskb.csv")
fpb = read_csv(fertility_patient_taskb)
fertility_doctors_taskb = here::here("Fertility_probability_judgement/Dataset/hcp_taskb.csv")
hcpb = read_csv(fertility_doctors_taskb)

sapply(fpb, class) 
fpb <- as.data.frame(apply(fpb, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

sapply(hcpb, class) 
hcpb <- as.data.frame(apply(hcpb, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

# Clear subjects with no answers.
fpb <- 
  fpb %>% 
  filter(!is.na(task_b_unlikely) & !is.na(task_b_almost_certainly))

hcpb <- 
  hcpb %>% 
  filter(!is.na(task_b_unlikely) & !is.na(task_b_almost_certainly))

# Convert data to long format and eliminate unnecessary term names. 
fpb_long <- pivot_longer(fpb, -id, names_to="term", values_to = "number") 

fpb_long <- fpb_long %>%
  mutate(term = unlist(fpb_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_b_') %>%
           stringr::str_replace_all(
             pattern = '_', replacement = ' ')) %>%
  mutate(subject = "fp")

hcpb_long <- pivot_longer(hcpb, -id, names_to="term", values_to = "number") 

hcpb_long <- hcpb_long %>%
  mutate(term = unlist(hcpb_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_b_') %>%
           stringr::str_replace_all(
             pattern = '_', replacement = ' ')) %>%
  mutate(subject = "hcp")


################################################################################

bind_rows(fpb_long, hcpb_long) %>% 
  mutate( 
    id = paste0(subject,"_",id) 
  ) -> allb_long

###
# TRYOUT
allb_long %>% filter(term == "almost certainly")  -> df
df %>% tidyr::pivot_wider(names_from = subject, values_from = number) %>%
  select(-id, -term) %>%
  as.data.frame() -> df1
  
ks.test(df1$fp, df1$hcp, data = df1)-> x0
###

allb_long %>%
  
  split(.$term) %>% 
  
  purrr::map_df(function(df){   
    
    df %>%
      tidyr::pivot_wider(names_from = subject, values_from = number) %>%
      select(-id, -term) %>%
      as.data.frame() %>%
      ks.test(.$fp, .$hcp, data = .) -> x0
    
    data.frame(term = df$term[1], D = x0$statistic, pvalue = x0$p.value) 
  }) -> number_real

sum(number_real$D)

################################################################################
# Permutation test 
permute_results_10k <- replicate (10000, simplify = FALSE,
                                  { # function in the replicate function. 
                                    allb_long %>%
                                      group_by(id) %>% 
                                      summarise(subject = first(subject))  %>% 
                                      mutate(subject = sample(subject)) -> permuteb

                                    allb_long %>%
                                      mutate(
                                        subject = permuteb$subject[match(x = id, permuteb$id)] 
                                      ) -> permute_allb_long
                                    
                                    permute_allb_long %>%
                                      split(.$term) %>%
                                      
                                      purrr::map_df(function(df){   
                                        df %>%
                                          tidyr::pivot_wider(names_from = subject, values_from = number) %>%
                                          select(-id, -term) %>%
                                          as.data.frame() %>%
                                          ks.test(.$fp, .$hcp, data = .) -> x0

                                         data.frame(term = df$term[1], D = x0$statistic, pvalue = x0$p.value) }) 
                                    } ) -> permuteb_10k

save(permuteb_10k, file = "permute_number_10k.rda") 
#  set.seed(10)
base::load("Fertility_probability_judgement/permute_10k.rda")

permuteb_10k %>%
  purrr::map_dbl(function(df){ 
    sum(df$D)  
  }) -> permuteb_stats


################################################################################


################################################################################
################################################################################

# almost certainly
#fp_ac <- fp_order_n %>% filter(term == "almost certainly")
#hcp_ac <- hcp_order_n %>% filter(term == "almost certainly")
#all_ac <- merge(data.frame(hcp_ac, row.names=NULL), data.frame(fp_ac, row.names=NULL), 
#      by = 0, all = TRUE)[-1] 
#t_ac <- chisq.test(all_ac$count.x, all_ac$count.y, simulate.p.value = TRUE)


