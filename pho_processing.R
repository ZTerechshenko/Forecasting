rm(list=ls())
library(here)
library(reshape2)
library(dplyr)
library(corrplot)
library(countrycode)

source1<-read.csv("/Users/zhanna.terechshenko/MA/DATA/Phoenix/ClineCenterHistoricalPhoenixEventData/PhoenixFBIS_1995-2004.csv")
source2<-read.csv("/Users/zhanna.terechshenko/MA/DATA/Phoenix/ClineCenterHistoricalPhoenixEventData/PhoenixNYT_1945-2005.csv")
source3<-read.csv("/Users/zhanna.terechshenko/MA/DATA/Phoenix/ClineCenterHistoricalPhoenixEventData/PhoenixSWB_1979-2015.csv")

sources <-rbind(source1, source2, source3)


#international
phoenix.data1 = sources %>%
  filter(source_root != target_root) %>%
  filter(source_agent=="GOV" | source_agent=="MIL") %>%
  filter(source_root!="") %>%
  filter(target_agent=="GOV" | target_agent=='MIL') %>%
  filter(target_root!="") %>%
  filter(is.na(year)==F) %>%
  filter(year >=2001 & year <=2014) %>%
  filter(source_root!="PSE" & source_root!="HKG" &
           source_root!="NGO" & source_root!="IGO" & source_root!="MNC" & 
           source_root!="BMU" & source_root!="ABW" & source_root!="AIA" &
           source_root!="COK" & source_root!="CYM") %>%
  filter(target_root!="PSE" & target_root!="HKG" &
           target_root!="NGO" & target_root!="IGO" & target_root!="MNC"  &
           target_root!="BMU" & target_root!="ABW" & target_root!="AIA" &
           target_root!="COK" & target_root!="CYM") %>%
  mutate(cow1 = countrycode(source_root, 'iso3c', 'cown')) %>%
  mutate(cow1 = ifelse(source_root=='SRB', '345', cow1)) %>%
  mutate(cow1 = ifelse(source_root=='TMP', '860', cow1)) %>%
  mutate(cow1 = ifelse(source_root=='SUN', '365', cow1)) %>%
  mutate(cow1 = ifelse(source_root=='KSV', '347', cow1)) %>%
  mutate(cow2 = countrycode(target_root, 'iso3c', 'cown')) %>%
  mutate(cow2 = ifelse(target_root=='SRB', '345', cow2)) %>%
  mutate(cow2 = ifelse(target_root=='TMP', '860', cow2)) %>%
  mutate(cow2 = ifelse(target_root=='SUN', '365', cow2)) %>%
  mutate(cow2 = ifelse(target_root=='KSV', '347', cow2)) %>%
  mutate(ccode = cow1) %>%
  mutate(vcp = ifelse(quad_class==1, 1, 0)) %>% # verbal cooperation
  mutate(mcp = ifelse(quad_class==2, 1, 0)) %>% # material cooperation
  mutate(vcf = ifelse(quad_class==3, 1, 0)) %>% # verbal conflict
  mutate(mcf = ifelse(quad_class==4, 1, 0)) %>% # material conflict
  select(ccode, year, month, cow1, cow2, vcp, mcp, vcf, mcf)

phoenix.data2 = phoenix.data1 %>%
  mutate(ccode = cow2)

pho = rbind(phoenix.data1, phoenix.data2)

pho.data = pho %>%
  select(ccode, year, month, vcp, mcp, vcf, mcf) %>%
  melt(id.vars = c('ccode','year', 'month')) %>%
  dcast(ccode+year+month~variable, fun.aggregate=sum) 

names(pho.data)<-c('ccode', 'year', 'month','vcp', 'mcp', 'vcf', 'mcf')                                

write.csv(pho.data, "pho_international.csv")


#domestic
phoenix.data3 = sources %>%
  filter(source_root == target_root) %>%
  filter(source_agent=="GOV" | source_agent=="MIL" | source_agent=="REB") %>%
  filter(source_root!="") %>%
  filter(target_agent=="GOV" | target_agent=='MIL' | target_agent=="REB") %>%
  filter(target_root!="") %>%
  filter(is.na(year)==F) %>%
  filter(year >=2001 & year <=2014) %>%
  filter(source_root!="PSE" & source_root!="HKG" &
           source_root!="NGO" & source_root!="IGO" & source_root!="MNC" & 
           source_root!="BMU" & source_root!="ABW" & source_root!="AIA" &
           source_root!="COK" & source_root!="CYM") %>%
  filter(target_root!="PSE" & target_root!="HKG" &
           target_root!="NGO" & target_root!="IGO" & target_root!="MNC"  &
           target_root!="BMU" & target_root!="ABW" & target_root!="AIA" &
           target_root!="COK" & target_root!="CYM") %>%
  mutate(cow1 = countrycode(source_root, 'iso3c', 'cown')) %>%
  mutate(cow1 = ifelse(source_root=='SRB', '345', cow1)) %>%
  mutate(cow1 = ifelse(source_root=='TMP', '860', cow1)) %>%
  mutate(cow1 = ifelse(source_root=='SUN', '365', cow1)) %>%
  mutate(cow1 = ifelse(source_root=='KSV', '347', cow1)) %>%
  mutate(cow2 = countrycode(target_root, 'iso3c', 'cown')) %>%
  mutate(cow2 = ifelse(target_root=='SRB', '345', cow2)) %>%
  mutate(cow2 = ifelse(target_root=='TMP', '860', cow2)) %>%
  mutate(cow2 = ifelse(target_root=='SUN', '365', cow2)) %>%
  mutate(cow2 = ifelse(target_root=='KSV', '347', cow2)) %>%
  mutate(ccode = cow1) %>%
  mutate(vcp = ifelse(quad_class==1, 1, 0)) %>% # verbal cooperation
  mutate(mcp = ifelse(quad_class==2, 1, 0)) %>% # material cooperation
  mutate(vcf = ifelse(quad_class==3, 1, 0)) %>% # verbal conflict
  mutate(mcf = ifelse(quad_class==4, 1, 0)) %>% # material conflict
  select(ccode, year, month, vcp, mcp, vcf, mcf)

pho.data3 = phoenix.data3 %>%
  melt(id.vars = c('ccode','year', 'month')) %>%
  dcast(ccode+year+month~variable, fun.aggregate=sum) 

write.csv(pho.data3, "pho_domestic.csv")
