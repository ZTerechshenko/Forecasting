rm(list=ls())
# Loading libraries
library(here)
library(reshape2)
library(rPython)                                                                  
library(dplyr)
library(countrycode)
#Loading Phil Schrodt's text_to_Cameo script and loading the data
python.load('text_to_CAMEO.py')
files = list.files(pattern='reduced.ICEWS.events.*.txt')
icews.data = do.call('rbind', lapply(files, function(x) read.table(x, header=FALSE,
                                                                   sep='\t')))                             
# international crisis #
#create dataset, which includes year and month
df1 = icews.data %>%
  filter(V4=='GOV' | V4=='MIL') %>% #actors are only government and military
  filter(V7=='GOV' | V7=='MIL') %>%
  filter(V2 != V5) %>% # exclude within-country observations
  filter(V2 != "---") %>%
  filter(V5 != "---") %>%
  filter(V3 != 0 & V6 != 0) %>% 
  filter(V3 != 997 & V6 != 997) %>% 
  mutate(ccode = V3) %>% #cown
  mutate(year = substr(V1, 1, 4)) %>% # create year variable
  mutate(month = substr(V1, 6, 7)) %>% # month variable
  filter(year >=2001 & year <=2014) %>%
  mutate(vcp = ifelse(V10==1, 1, 0)) %>% # verbal cooperation
  mutate(mcp = ifelse(V10==2, 1, 0)) %>% # material cooperation
  mutate(vcf = ifelse(V10==3, 1, 0)) %>% # verbal conflict
  mutate(mcf = ifelse(V10==4, 1, 0)) %>% # material conflict
  select(ccode, year, month, V2, V3, V5, V6, V8, vcp, mcp, vcf, mcf)

df2 = df1%>%
  mutate(ccode = V6)

df = rbind(df1, df2)
# Aggregate by country-month
new_df = df %>%
  select(ccode, year, month, vcp, mcp, vcf, mcf) %>%
  melt(id.vars = c('ccode','year', 'month')) %>%
  dcast(ccode+year+month~variable, fun.aggregate=sum) 

names(new_df)<-c('ccode', 'year', 'month','vcp', 'mcp', 'vcf', 'mcf')                                

write.csv(new_df, "icw_international.csv")

# Create a dataset for rebellions

df3 = icews.data %>%
  filter(V4=='GOV' | V4=='MIL' | V4=="REB") %>% #actors are only government and military and rebels
  filter(V7=='GOV' | V7=='MIL' | V7=="REB") %>%
  filter(V4 != V7) %>%
  filter(V2 == V5) %>%  # within-country observations
  mutate(year = substr(V1, 1, 4)) %>% # create year variable
  mutate(month = substr(V1, 6, 7)) %>% # month variable
  filter(year >=2001 & year <=2014) %>%
  mutate(vcp = ifelse(V10==1, 1, 0)) %>% # verbal cooperation
  mutate(mcp = ifelse(V10==2, 1, 0)) %>% # material cooperation
  mutate(vcf = ifelse(V10==3, 1, 0)) %>% # verbal conflict
  mutate(mcf = ifelse(V10==4, 1, 0)) %>% # material conflict
  mutate(ccode = V3) %>%
  select(ccode, year, month, vcp, mcp, vcf, mcf)

# Aggregate by country-month
new_df3 = df3 %>%
  melt(id.vars = c('ccode','year', 'month')) %>%
  dcast(ccode+year+month~variable, fun.aggregate=sum) 

write.csv(new_df3, "icw_domestic.csv")


  


