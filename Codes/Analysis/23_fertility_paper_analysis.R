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
here::here()

# Load dataset.
fertility_patient = here::here("Fertility_probability_judgement/Dataset/fp_taska.csv")
fp = read_csv(fertility_patient)
fertility_doctors = here::here("Fertility_probability_judgement/Dataset/hcp_taska.csv")
hcp = read_csv(fertility_doctors)
hcp %>% select(-1) -> hcp

# Convert data to long format and eliminate unnecessary term names. 
fp_long <- pivot_longer(fp, cols = c(1:17), names_to ="term", values_to = "ordering") %>%
  select(-item_string, -id) 

fp_long <- fp_long %>%
  mutate(term = unlist(fp_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_a_') %>%
           stringr::str_replace_all(
             pattern = '_', replacement = ' ')) %>%
  mutate(subject = "fp")

################################################################################
hcp_long <- pivot_longer(hcp, cols = c(1:17), names_to ="term", values_to = "ordering") %>%
  select(-item_string, -id) 
hcp_long <- hcp_long %>% 
  mutate(term = unlist(hcp_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_a_') %>%
           stringr::str_replace_all(
             pattern = '_', replacement = ' ')) %>%
  mutate(subject = "hcp")
################################################################################

# Calculate the frequency of the orderings of each term from fp and hcp. 
fp_long %>%
  group_by(term, ordering) %>%
  summarise(count = table(ordering)) %>%
  mutate(subject = "fp") -> fp_order_n

################################################################################
hcp_long %>%
  group_by(term, ordering) %>%
  summarise(count = table(ordering)) %>%
  mutate(subject = "hcp") -> hcp_order_n
################################################################################

# I am not sure about how to calculate the chi-squared test within a long format 
# data set, which includes all terms and both subjects. see fp_order_n + 
# hcp_order_n. So here I manually divide each term and calculate the test. 

# Another question: do I need to add orderings for it to range from 1-16 because 
# there are some terms don't have certain orderings. 

# about even 
fp_ae <- fp_order_n %>% filter(term == "about even")
hcp_ae <- hcp_order_n %>% filter(term == "about even")
all_ae <- merge(data.frame(hcp_ae, row.names=NULL), data.frame(fp_ae, row.names=NULL), 
      by = 0, all = TRUE)[-1]  
t_ae <- chisq.test(all_ae$count.x, all_ae$count.y, simulate.p.value = TRUE)

# almost certainly
fp_ac <- fp_order_n %>% filter(term == "almost certainly")
hcp_ac <- hcp_order_n %>% filter(term == "almost certainly")
all_ac <- merge(data.frame(hcp_ac, row.names=NULL), data.frame(fp_ac, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_ac <- chisq.test(all_ac$count.x, all_ac$count.y, simulate.p.value = TRUE)

# almost no chance
fp_anc <- fp_order_n %>% filter(term == "almost no chance")
hcp_anc <- hcp_order_n %>% filter(term == "almost no chance")
all_anc <- merge(data.frame(hcp_anc, row.names=NULL), data.frame(fp_anc, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_anc <- chisq.test(all_anc$count.x, all_anc$count.y, simulate.p.value = TRUE)

# better than even
fp_bte <- fp_order_n %>% filter(term == "better than even")
hcp_bte <- hcp_order_n %>% filter(term == "better than even")
all_bte <- merge(data.frame(hcp_bte, row.names=NULL), data.frame(fp_bte, row.names=NULL), 
      by = 0, all = TRUE)[-1]  
t_bte <- chisq.test(all_bte$count.x, all_bte$count.y, simulate.p.value = TRUE)

# chances are slight
fp_cas <- fp_order_n %>% filter(term == "chances are slight")
hcp_cas <- hcp_order_n %>% filter(term == "chances are slight")
all_cas <- merge(data.frame(hcp_cas, row.names=NULL), data.frame(fp_cas, row.names=NULL), 
      by = 0, all = TRUE)[-1]  
t_cas <- chisq.test(all_cas$count.x, all_cas$count.y, simulate.p.value = TRUE)

# highly likely
fp_hlil <- fp_order_n %>% filter(term == "highly likely")
hcp_hlil <- hcp_order_n %>% filter(term == "highly likely")
all_hlil <- merge(data.frame(hcp_hlil, row.names=NULL), data.frame(fp_hlil, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_hlil <- chisq.test(all_hlil$count.x, all_hlil$count.y, simulate.p.value = TRUE)

# highly unlikely
fp_hunl <- fp_order_n %>% filter(term == "highly unlikely")
hcp_hunl <- hcp_order_n %>% filter(term == "highly unlikely")
all_hunl <- merge(data.frame(hcp_hunl, row.names=NULL), data.frame(fp_hunl, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_hunl <- chisq.test(all_hunl$count.x, all_hunl$count.y, simulate.p.value = TRUE)

# improbable
fp_improe <- fp_order_n %>% filter(term == "improbable")
hcp_improe <- hcp_order_n %>% filter(term == "improbable")
all_improe <- merge(data.frame(hcp_improe, row.names=NULL), data.frame(fp_improe, row.names=NULL), 
      by = 0, all = TRUE)[-1]  
t_improe <- chisq.test(all_improe$count.x, all_improe$count.y, simulate.p.value = TRUE)

# likely
fp_lil <- fp_order_n %>% filter(term == "likely")
hcp_lil <- hcp_order_n %>% filter(term == "likely")
all_lil <- merge(data.frame(hcp_lil, row.names=NULL), data.frame(fp_lil, row.names=NULL), 
      by = 0, all = TRUE)[-1]  
t_lil <- chisq.test(all_lil$count.x, all_lil$count.y, simulate.p.value = TRUE)

# little chance
fp_lc <- fp_order_n %>% filter(term == "little chance")
hcp_lc <- hcp_order_n %>% filter(term == "little chance")
all_lc <- merge(data.frame(hcp_lc, row.names=NULL), data.frame(fp_lc, row.names=NULL), 
      by = 0, all = TRUE)[-1]  
t_lc <- chisq.test(all_lc$count.x, all_lc$count.y, simulate.p.value = TRUE)

# probable
fp_proe <- fp_order_n %>% filter(term == "probable")
hcp_proe <- hcp_order_n %>% filter(term == "probable")
all_proe <- merge(data.frame(hcp_proe, row.names=NULL), data.frame(fp_proe, row.names=NULL), 
      by = 0, all = TRUE)[-1]
t_proe <- chisq.test(all_proe$count.x, all_proe$count.y, simulate.p.value = TRUE)

# probably
fp_proy <- fp_order_n %>% filter(term == "probably")
hcp_proy <- hcp_order_n %>% filter(term == "probably")
all_proy <- merge(data.frame(hcp_proy, row.names=NULL), data.frame(fp_proy, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_proy <- chisq.test(all_proy$count.x, all_proy$count.y, simulate.p.value = TRUE)

# probably not
fp_proyn <- fp_order_n %>% filter(term == "probably not")
hcp_proyn <- hcp_order_n %>% filter(term == "probably not")
all_proyn <- merge(data.frame(hcp_proyn, row.names=NULL), data.frame(fp_proyn, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_proyn <- chisq.test(all_proyn$count.x, all_proyn$count.y, simulate.p.value = TRUE)

# unlikely
fp_unli <- fp_order_n %>% filter(term == "unlikely")
hcp_unli <- hcp_order_n %>% filter(term == "unlikely")
all_unli <- merge(data.frame(hcp_unli, row.names=NULL), data.frame(fp_unli, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_unli <- chisq.test(all_unli$count.x, all_unli$count.y, simulate.p.value = TRUE)

# very good chance
fp_vgc <- fp_order_n %>% filter(term == "very good chance")
hcp_vgc <- hcp_order_n %>% filter(term == "very good chance")
all_vgc <- merge(data.frame(hcp_vgc, row.names=NULL), data.frame(fp_vgc, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_vgc <- chisq.test(all_vgc$count.x, all_vgc$count.y, simulate.p.value = TRUE)

# we believe
fp_wb <- fp_order_n %>% filter(term == "we believe")
hcp_wb <- hcp_order_n %>% filter(term == "we believe")
all_wb <- merge(data.frame(hcp_wb, row.names=NULL), data.frame(fp_wb, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_wb <- chisq.test(all_wb$count.x, all_wb$count.y, simulate.p.value = TRUE)

# we doubt
fp_wd <- fp_order_n %>% filter(term == "we doubt")
hcp_wd <- hcp_order_n %>% filter(term == "we doubt")
all_wd <- merge(data.frame(hcp_wd, row.names=NULL), data.frame(fp_wd, row.names=NULL), 
      by = 0, all = TRUE)[-1] 
t_wd <-chisq.test(all_wd$count.x, all_wd$count.y, simulate.p.value = TRUE)


# Chi-squared results between the orderings given from patients and doctors for each term.
categories <- unique(hcp_order_n$term) 

# numberOfCategories <- length(categories)
chi_estimates <- tibble(term = c(categories), 
       x_squared = c(t_ae$statistic, t_ac$statistic, t_anc$statistic, t_bte$statistic, 
                     t_cas$statistic, t_hlil$statistic, t_hunl$statistic, t_improe$statistic, 
                     t_lil$statistic, t_lc$statistic, t_proe$statistic, t_proy$statistic, 
                     t_proyn$statistic, t_unli$statistic, t_vgc$statistic, t_wd$statistic, t_wb$statistic), 
       p_value = c(t_ae$p.value, t_ac$p.value, t_anc$p.value, t_bte$p.value, t_cas$p.value, 
                   t_hlil$p.value, t_hunl$p.value, t_improe$p.value, t_lil$p.value, t_lc$p.value, 
                   t_proe$p.value, t_proy$p.value, t_proyn$p.value, t_unli$p.value, t_vgc$p.value, 
                   t_wd$p.value, t_wb$p.value))

# test statistics in a big data set without specifying. 
fp_order_n$count[fp_order_n$term=="we doubt"]
hcp_order_n$count[hcp_order_n$term=="we doubt"]
test.stat1 <-chisq.test(fp_order_n$count[fp_order_n$term=="we doubt"], 
                        hcp_order_n$count[hcp_order_n$term=="we doubt"],
                        simulate.p.value = TRUE)

# permutation test. 

# optional: set a seed; it allows us to get the exact same set of random data each 
# time we run the code.
set.seed(10)

# the number of observation to sample. in my case, should be 16. 
n <- length(1:16)

# the number of permutation sample to take
p <- 10

# the variable we will resample from; the number of ordering to each term.  
variable <- c(0:150)

# initialize a matrix to store the permutation data
orsample <- matrix(0, nrow = n, ncol = p)

# the problem i cant use the exact data example is because i need to have 2 columns
# to run the test statistics and the example only needs one. 

# next, get the permutation samples, using a loop.
for (i in 1:p) {
  orsample[,i] <- sample(variable, size =n, replace=FALSE)
}

orsample[, 1:10]

# calculate the test statistics for the permutation sample
# initialize verctors to store all of the test stats:
per.test1 <- replicate(n=p, sample(0, size=16, replace=TRUE))
per.test2 <- rep(0,n)

# loop through, and calculate the test stats
for (i in 1:p) {
   for (j in 1:p) {
     per.test[i] <- chisq.test(orsample[, i], 
                                orsample[, j],
                                simulate.p.value = TRUE)
#   }
 
}

per.test
# It doesn't work; no idea why. 




as.data.frame(orsample)
d <- as.data.frame(cbind(c(rep(0, 16)), c(rep(0, 16))))
fp <- d$V2
hcp <- d$V1

#Difference in means
original <- diff(tapply(outcome, treatment, mean))
mean(outcome[treatment==1])-mean(outcome[treatment==0])
test.stat1 <-chisq.test(fp,hcp)

#Permutation test
permutation.test <- function(treatment, outcome, n){
  distribution=c()
  result=0
  for(i in 1:n){
    distribution[i]=diff(by(outcome, sample(treatment, length(treatment), FALSE), mean))
  }
  result=sum(abs(distribution) >= abs(original))/(n)
  return(list(result, distribution))
}

test1 <- permutation.test(treatment, outcome, 10000)
hist(test1[[2]], breaks=50, col='grey', main="Permutation Distribution", las=1, xlab='')
abline(v=original, lwd=3, col="red")

test1[[1]]




fp_long %>% add_row(hcp_long) -> all_long
all_long$term = factor(all_long$term,
                         levels=unique(all_long$term))
all_long$subject = factor(all_long$subject ,
                       levels=unique(all_long$subject))
fp_order_n$count <- as.numeric(fp_order_n$count)
hcp_order_n$count <- as.numeric(hcp_order_n$count)
all_long %>%
  group_by(term, subject, ordering) %>%
  summarise(count = table(ordering)) -> all_order_n


class(fp_order_n$count)
#install.packages("coin")
#install.packages("lmPerm")
#install.packages("FSA")
library(coin)
#library(FSA)


class(all_long$term)








####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

# pivot_wider; bind_rows; merge; add_rows; left_join
d <- pivot_wider(fp_ae, names_from ="ordering", values_from = "count") 
d1 <- pivot_wider(hcp_ae, names_from ="ordering", values_from = "count") 
d %>% bind_rows(d1, before=NULL) -> d2_wide

as.data.frame(fp_order_n) -> df
merge(data.frame(fp_order_n, row.names=NULL), data.frame(hcp_order_n, row.names=NULL), 
      by = 0, all = TRUE)[-1] -> all_order_n

df <- as_tibble(fp_order_n)
df %>% filter(term == "task_a_about_even") %>%
  as.numeric(count)-> df1

df2 <- data.frame(ordering = c(1:7), count=0)  
as.table(df2)
class(df2$count)
as.double(df$count) 
as.double(df1$count) 

as.data.frame(df2) -> df2
df3 = right_join(df1, df2, by = c("ordering", "count"))


total <- rbind.data.frame(df1,df2, na.rm=TRUE)

df <- fp_order_n %>% filter(all_of(fp_order_n$term == "task_a_about_even"))


test1 <- 
  chisq.test(table(unlikely$Freq.x, unlikely$Freq.y), simulate.p.value = TRUE)

length(fp_long$ordering)
fp_long.apply(pd.Series.value_counts)


as.factor(fp_long$ordering)

len(np. unique(my_array))

  summarise(count = sum(ordering == "1"))
  fp_long %>% as.numeric(ordering) ->fp_long
  

all_long %>%
  group_by(term) %>%
  group_by(subject) %>%
  summarise(count = sum(ordering == "1"))

data.frame(unlikely_fp =fp$task_a_unlikely) -> a
data.frame(unlikely_hcp =hcp$task_a_unlikely) -> b
merge(data.frame(a, row.names=NULL), data.frame(b, row.names=NULL), 
      by = 0, all = TRUE)[-1] -> unlikely 

as.data.frame(x = table(unlikely['unlikely_fp'])) ->aa
as.data.frame(x = table(unlikely['unlikely_hcp'])) -> bb
aa <- as.integer(aa$unlikely_fp)
class(aa$unlikely_fp)
aaa <- tibble(unlikely_fp = 1:7, Freq = 0)

aa %>% add_row(tibble(unlikely_fp = 1:7, Freq = 0))
as.integer(aa$unlikely_fp)
class(aa$unlikely_fp)
aa %>% add_row(unlikely_fp = 1:7, Freq = 0)

merge(data.frame(aa, row.names=NULL), data.frame(bb, row.names=NULL), 
      by = 0, all = TRUE)[-1] -> unlikely 

as.factor(unlikely$unlikely_fp)
as.factor(unlikely$Freq.x) 
as.factor(unlikely$unlikely_hcp)
as.factor(unlikely$Freq.y)          
          

n_fp_unlikely = table(unlikely['unlikely_fp'])
n_hcp_unlikely = table(unlikely['unlikely_hcp'])
aa %>% add_row(df)

df <- data.frame(unlikely_fp = 1:7, Freq= 0) %>% 
  mutate(across(cols_to_change, as.double))

df <- tibble(x = 1:3, y = 0)
df %>% add_row(x = 4:7, y = 0)

test1 <- chisq.test(table(unlikely$Freq.x, unlikely$Freq.y), simulate.p.value = TRUE)
test2 <- chisq.test(unlikely$Freq.x, unlikely$Freq.y, simulate.p.value = TRUE)
test1$p.value


###
table(unlikely['unlikely_fp'])

hcp %>% select(-1, item_string, id) -> hcp
fp %>% select(-item_string, -id) -> fp

fp_long <- pivot_longer(fp, cols = c(1:17), names_to ="term", values_to = "ordering") 
hcp_long <- pivot_longer(hcp, cols = c(1:17), names_to ="term", values_to = "ordering") 
c <- fp_long %>% inner_join(hcp_long)
combine()
a <- fp %>% select("task_a_unlikely")

a <- c("12", "13", "14")
length(fp$task_a_unlikely == a)

fp <- as.data.frame(x= fp)
is.data.frame(fp)
df1 <- fp[["task_a_unlikely"]].copy()

new = fp[['id']].copy()

new = fp.filter("task_a_unlikely", axis=1)

print(df2)


colnames(fp) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_cleanfp

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

data.frame(unlikely_fp =fp$task_a_unlikely) -> a
data.frame(unlikely_hcp =hcp$task_a_unlikely) -> b
merge(data.frame(a, row.names=NULL), data.frame(b, row.names=NULL), 
      by = 0, all = TRUE)[-1] -> unlikely 

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
