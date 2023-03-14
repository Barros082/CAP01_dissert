library(tidyverse)
library(readxl)
library(geobr)

base_lucas <- read_excel("base_geral_1.xlsx")

str(base_lucas)
base_lucas %>% 
  filter(abbrev_state=="PE") %>% 
  select(code_muni, code_short, code_state, 
         name_muni, abbrev_state, area_mun, 
         area_caat, nvc_2010, nvcPerc_10, 
         `desmat_2010 a 2011`, tx.desmat.perc_10, 
         idh_10, idhE_10, idhL_10, idhR_10, renda_10, 
         meioSal_10, quartSal_10)->blucas_2010_PE

str(blucas_2010_PE)


library(FactoMineR)
pca=PCA(blucas_2010_PE[-1:-7], graph=TRUE)
#DIM1=social factors
#DIM2=environmental factors


pca$var
# "confirmando" a interpretação das dimensões

pca$ind
data.frame(pca$ind$contrib) %>% 
  select(Dim.1, Dim.2) %>% 
  rename("social_fact"=Dim.1, "env_fact"=Dim.2) %>% 
  mutate(code_muni = blucas_2010_PE$code_muni)-> contrib_pca


all_mun_pe <- read_municipality(code_muni="PE", year=2010)

left_join(blucas_2010_PE, all_mun_pe) %>% 
  full_join(contrib_pca) %>% 
  select(code_muni, code_short, code_state, 
         name_muni, geom, abbrev_state, area_mun, 
         area_caat, social_fact, env_fact)->data_full

str(data_full)
summary(data_full)
# social_factor - 0,806452 (média) e 0.281958 de mediana
# env_fact -  0,806452 (média) e 0.539474 de mediana

#####
#fig to help 
data_full %>% 
  ggplot(aes(social_fact))+
  geom_density()+
  xlim(0,5)


data_full %>% 
  ggplot()+
  geom_sf(aes(geometry=geom, fill=social_fact, col=social_fact))

data_full %>% 
  ggplot()+
  geom_sf(aes(geometry=geom, fill=env_fact, col=env_fact))


data_full %>%
  mutate(cat_social_fact=case_when(social_fact < 0.806452 ~ "hard", 
                                   social_fact > 0.806452 ~ "soft")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geom, fill=cat_social_fact, col=cat_social_fact), alpha=0.5)


data_full %>%
  mutate(cat_env_fact=case_when(env_fact < 0.806452 ~ "hard", 
                                env_fact > 0.806452 ~ "soft")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geom, fill=cat_env_fact, col=cat_env_fact), alpha=0.5)



# ______
library(raster)
library(sf)

data_full %>% 
  mutate(cat_social_fact=case_when(social_fact < 0.806452 ~ "hard", 
                                   social_fact > 0.806452 ~ "soft")) %>%
  mutate(cat_env_fact=case_when(env_fact < 0.806452 ~ "hard", 
                                env_fact > 0.806452 ~ "soft")) ->data_full


data_full$geom

teste<-st_geometrycollection(data_full$geom, dim = "XY")

ggplot(data = data_full)+
  geom_sf(aes(geometry=teste, fill=cat_social_fact, col=cat_social_fact), alpha=0.5)


#####


malhaPE_buffer<-read_sf(dsn=".", layer="malhaPE_buffer")
str(malhaPE_buffer)
#fid é uma coluna gerada pela intersection between buffer5km and the correction of Pe_malha
summary(malhaPE_buffer)
#com_area_l, ml_area_lo, nd_b_area_, tlpc_area_, tlpl_area_, rast are only have NA values

malhaPE_buffer %>% 
  select(-com_area_l, -ml_area_lo, -nd_b_area_, -tlpc_area_, -tlpl_area_, -rast) %>% 
  filter(tre_cvr<0.2) %>% #arbitrary cut-off about forest
  #str()
  #colnames()
  group_by(cd_mun, id_buff, gid, ownership_, sub_class, Ardty_z, lnd_s_c) %>% 
  summarise(area_total=sum(area)) %>% 
  spread(value = area_total )
  # não sei oq fazer aqui
#  summarise_at(vars(area_origi, area, ag_area_lo,  aru_area_l, carpo_area, 
#                    carpr_area,  nd_i_area_,  ql_area_lo, sigef_area, ti_h_area_,
#                    ti_n_area_, trans_area, ucpi_area_,  ucus_area_, urb_area_l), .funs = sum)
  
malhaPE_buffer %>% 
  select(-com_area_l, -ml_area_lo, -nd_b_area_, -tlpc_area_, -tlpl_area_, -rast) %>% 
  filter(tre_cvr<0.2) %>% #arbitrary cut-off about forest
  #str()
  #colnames()
  group_by(cd_mun, id_buff, gid, ownership_, sub_class, Ardty_z, lnd_s_c) %>% 
  mutate(area_buff=(3.14*5000)*(3.14*5000), #buffer area
         cd_mun=as.factor(cd_mun), 
         id_buff=as.factor(id_buff), 
         #gid=as.factor(gid), 
         ownership_=as.factor(ownership_), 
         sub_class=as.factor(sub_class), 
         Ardty_z=as.factor(Ardty_z), 
         lnd_s_c=as.factor(lnd_s_c))->already_malhaPE_buff

already_malhaPE_buff$gid<-as.factor(already_malhaPE_buff$gid)

already_malhaPE_buff %>%
  mutate(prop_fre=levels(gid))
