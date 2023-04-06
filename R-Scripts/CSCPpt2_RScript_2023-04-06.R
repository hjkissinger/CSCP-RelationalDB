library(tidyverse)

ch_df <- cscpopendata

colnames(ch_df)

###Selecting tables

#Select for Company table
company_tbl <- ch_df %>% 
  select('CompanyId', 'CompanyName')

company_tbl_v2 <- company_tbl %>% 
  distinct(CompanyId, .keep_all = T) %>% 
  arrange(CompanyId)

#Select for Chemical table
chemical_tbl <- ch_df %>% 
  select('CasId', 'CasNumber', 'ChemicalName')

chemical_tbl_v2 <- chemical_tbl %>% 
  distinct(CasId, .keep_all = T) %>% 
  arrange(CasId)

as.numeric(chemical_tbl_v2$CasNumber)
glimpse(chemical_tbl_v2)

#Select for Primary Category table
primarycategory_tbl <- ch_df %>% 
  select('PrimaryCategoryId', 'PrimaryCategory')

primarycategory_tbl_v2 <- primarycategory_tbl %>% 
  distinct(PrimaryCategoryId, .keep_all = T) %>% 
  arrange(PrimaryCategoryId)

#Select for Subcategory table
subcategory_tbl <- ch_df %>% 
  select('SubCategoryId', 'SubCategory')

subcategory_tbl_v2 <- subcategory_tbl %>% 
  distinct(SubCategoryId, .keep_all = T) %>% 
  arrange(SubCategoryId)

#Create a ch_df with CSFId = null removed
ch_df_v2 <- ch_df

ch_df_v2$CSFId[which(is.na(ch_df_v2$CSFId) == T)]<-0
ch_df_v2 <- ch_df_v2[!(ch_df_v2$CSFId == 0),]

#ch_df_v2 CHFId left over = Variant table

#Select for Variant table
variant_tbl <- ch_df_v2 %>% 
  select('CSFId', 'CSF', 'SubCategoryId')

variant_obs <- 80662

#Select for VarChemId table
varchemid_tbl <- ch_df_v2 %>% 
  select('ChemicalId', 'CasId', 'CSFId', 'ChemicalCreatedAt', 'ChemicalUpdatedAt', 'ChemicalDateRemoved')

varchemid_tbl_v2 <- varchemid_tbl

#Select for null CSFId in ch_df to create SubCatChemId table
#Converting null to T and then subset T
ch_df_v3 <- ch_df
ch_df_v3$CSFId[which(is.na(ch_df_v3$CSFId) == T)]<-'T'
ch_df_v4 <- ch_df_v3[which(ch_df_v3$CSFId=='T'),]

#Select for SubCatChemId table
subcatchemid_tbl <- ch_df_v4 %>% 
  select('ChemicalId', 'SubCategoryId', 'CasId', 'ChemicalCreatedAt', 'ChemicalUpdatedAt', 'ChemicalDateRemoved')

subcatchem_obs <- 33972

subcatchemid_tbl_v2 <- subcatchemid_tbl

# Add variant and subcatchemid obs to verify accurate table number for products in original dataset

total_obs <- variant_obs + subcatchem_obs

###Cleaning data

#Find and replace CasIds of VarChemId table with newly assigned standardized Ids

#Aloe vera, non-colorized whole leaf extract
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 1178]<-1108
varchemid_tbl_v2[grep(1178, varchemid_tbl_v2$CasId), ]

#Carbon black (airborne, unbound particles of respirable size)
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 1104]<-104
varchemid_tbl_v2[grep(1104, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 726]<-104
varchemid_tbl_v2[grep(726, varchemid_tbl_v2$CasId), ]

#Coal Tar
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 888]<-731
varchemid_tbl_v2[grep(888, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 895]<-731
varchemid_tbl_v2[grep(895, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 898]<-731
varchemid_tbl_v2[grep(898, varchemid_tbl_v2$CasId), ]

#Cocamide diethanol amine (DEA)
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 969]<-162
varchemid_tbl_v2[grep(969, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 1007]<-162
varchemid_tbl_v2[grep(1007, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 986]<-162
varchemid_tbl_v2[grep(986, varchemid_tbl_v2$CasId), ]

#Coffee extract
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 890]<-732
varchemid_tbl_v2[grep(890, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 908]<-732
varchemid_tbl_v2[grep(908, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 909]<-732
varchemid_tbl_v2[grep(909, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 911]<-732
varchemid_tbl_v2[grep(911, varchemid_tbl_v2$CasId), ]

#Ethanol in alcoholic beverages
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 1242]<-964
varchemid_tbl_v2[grep(1242, varchemid_tbl_v2$CasId), ]

#Lauramide DEA
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 1048]<-1029
varchemid_tbl_v2[grep(1048, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 987]<-1029
varchemid_tbl_v2[grep(987, varchemid_tbl_v2$CasId), ]

#Paraformaldehyde
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 1069]<-1068
varchemid_tbl_v2[grep(1069, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 1075]<-1068
varchemid_tbl_v2[grep(1075, varchemid_tbl_v2$CasId), ]

#Retinyl palmitate
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 933]<-773
varchemid_tbl_v2[grep(933, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 938]<-773
varchemid_tbl_v2[grep(938, varchemid_tbl_v2$CasId), ]

#Talc
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 894]<-780
varchemid_tbl_v2[grep(894, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 953]<-780
varchemid_tbl_v2[grep(953, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 955]<-780
varchemid_tbl_v2[grep(955, varchemid_tbl_v2$CasId), ]

varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 943]<-780
varchemid_tbl_v2[grep(943, varchemid_tbl_v2$CasId), ]

#Titanium dioxide
varchemid_tbl_v2$CasId[varchemid_tbl_v2$CasId == 1032]<- 656
varchemid_tbl_v2[grep(1032, varchemid_tbl_v2$CasId), ]

#Add 'Removed' binary column
varchemid_tbl_v3 <- varchemid_tbl_v2 %>% 
  mutate(Removed = varchemid_tbl_v2$ChemicalDateRemoved)

varchemid_tbl_v3$Removed[which(is.na(varchemid_tbl_v3$Removed) == F)]<-'1'
varchemid_tbl_v3$Removed[which(is.na(varchemid_tbl_v3$Removed) == T)]<-'0'

#Find and replace CasIds of subcatchemid table with newly assigned standardized Ids

#Aloe vera, non-colorized whole leaf extract
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 1178]<-1108
subcatchemid_tbl_v2[grep(1178, subcatchemid_tbl_v2$CasId), ]

#Carbon black (airborne, unbound particles of respirable size)
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 1104]<-104
subcatchemid_tbl_v2[grep(1104, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 726]<-104
subcatchemid_tbl_v2[grep(726, subcatchemid_tbl_v2$CasId), ]

#Coal Tar
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 888]<-731
subcatchemid_tbl_v2[grep(888, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 895]<-731
subcatchemid_tbl_v2[grep(895, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 898]<-731
subcatchemid_tbl_v2[grep(898, subcatchemid_tbl_v2$CasId), ]

#Cocamide diethanol amine (DEA)
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 969]<-162
subcatchemid_tbl_v2[grep(969, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 1007]<-162
subcatchemid_tbl_v2[grep(1007, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 986]<-162
subcatchemid_tbl_v2[grep(986, subcatchemid_tbl_v2$CasId), ]

#Coffee extract
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 890]<-732
subcatchemid_tbl_v2[grep(890, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 908]<-732
subcatchemid_tbl_v2[grep(908, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 909]<-732
subcatchemid_tbl_v2[grep(909, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 911]<-732
subcatchemid_tbl_v2[grep(911, subcatchemid_tbl_v2$CasId), ]

#Ethanol in alcoholic beverages
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 1242]<-964
subcatchemid_tbl_v2[grep(1242, subcatchemid_tbl_v2$CasId), ]

#Lauramide DEA
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 1048]<-1029
subcatchemid_tbl_v2[grep(1048, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 987]<-1029
subcatchemid_tbl_v2[grep(987, subcatchemid_tbl_v2$CasId), ]

#Paraformaldehyde
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 1069]<-1068
subcatchemid_tbl_v2[grep(1069, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 1075]<-1068
subcatchemid_tbl_v2[grep(1075, subcatchemid_tbl_v2$CasId), ]

#Retinyl palmitate
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 933]<-773
subcatchemid_tbl_v2[grep(933, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 938]<-773
subcatchemid_tbl_v2[grep(938, subcatchemid_tbl_v2$CasId), ]

#Talc
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 894]<-780
subcatchemid_tbl_v2[grep(894, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 953]<-780
subcatchemid_tbl_v2[grep(953, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 955]<-780
subcatchemid_tbl_v2[grep(955, subcatchemid_tbl_v2$CasId), ]

subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 943]<-780
subcatchemid_tbl_v2[grep(943, subcatchemid_tbl_v2$CasId), ]

#Titanium dioxide
subcatchemid_tbl_v2$CasId[subcatchemid_tbl_v2$CasId == 1032]<- 656
subcatchemid_tbl_v2[grep(1032, subcatchemid_tbl_v2$CasId), ]

#Add 'Removed' binary column
subcatchemid_tbl_v3 <- subcatchemid_tbl_v2 %>% 
  mutate(Removed = subcatchemid_tbl_v2$ChemicalDateRemoved)

subcatchemid_tbl_v3$Removed[which(is.na(subcatchemid_tbl_v3$Removed) == F)]<-'1'
subcatchemid_tbl_v3$Removed[which(is.na(subcatchemid_tbl_v3$Removed) == T)]<-'0'

#Select for Product table
product_tbl <- ch_df %>% 
  select('CDPHId', 'ProductName', 'BrandName', 'CompanyId', 'PrimaryCategoryId', 'SubCategoryId', 'ChemicalCount', 'InitialDateReported', 'MostRecentDateReported', 'DiscontinuedDate')

product_tbl_v2 <- product_tbl %>% 
  distinct(CDPHId, .keep_all = T) %>% 
  arrange(CDPHId)

#Find and replace CompanyIds of product table with newly assigned standardized Ids

#NYX Los Angeles
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 896]<- 30
product_tbl_v2[grep(896, product_tbl_v2$CompanyId), ]

#The Procter & Gamble Company
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 159]<- 86
product_tbl_v2[grep(159, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 899]<- 86
product_tbl_v2[grep(899, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1036]<- 86
product_tbl_v2[grep(1036, product_tbl_v2$CompanyId), ]

#Regis Corporation
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 116]<- 89
product_tbl_v2[grep(116, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 165]<- 89
product_tbl_v2[grep(165, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 757]<- 89
product_tbl_v2[grep(757, product_tbl_v2$CompanyId), ]

#Added Extras LLC
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 157]<- 118
product_tbl_v2[grep(157, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 716]<- 118
product_tbl_v2[grep(716, product_tbl_v2$CompanyId), ]

#Giovanni Cosmetics, Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1321]<- 150
product_tbl_v2[grep(1321, product_tbl_v2$CompanyId), ]

#Combe Incorporated
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1254]<- 172
product_tbl_v2[grep(1254, product_tbl_v2$CompanyId), ]

#Unite Eurotherapy
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 867]<- 222
product_tbl_v2[grep(867, product_tbl_v2$CompanyId), ]

#MAKE UP FOR EVER
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1231]<- 345
product_tbl_v2[grep(1231, product_tbl_v2$CompanyId), ]

#Crabtree & Evelyn, Ltd.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 976]<- 357
product_tbl_v2[grep(976, product_tbl_v2$CompanyId), ]

#American International Industries
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1148]<- 372
product_tbl_v2[grep(1148, product_tbl_v2$CompanyId), ]

#ORLY INTERNATIONAL
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 926]<- 396
product_tbl_v2[grep(926, product_tbl_v2$CompanyId), ]

#Iredale Mineral Cosmetics
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 510]<- 427
product_tbl_v2[grep(510, product_tbl_v2$CompanyId), ]

#Walmart
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 827]<- 534
product_tbl_v2[grep(827, product_tbl_v2$CompanyId), ]

#Pharmaceutical Specialties, Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 840]<- 583
product_tbl_v2[grep(840, product_tbl_v2$CompanyId), ]

#The Kroger Co.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 811]<- 697
product_tbl_v2[grep(811, product_tbl_v2$CompanyId), ]

#I Love Cosmetics Limited
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 950]<- 773
product_tbl_v2[grep(950, product_tbl_v2$CompanyId), ]

#Alessandro International GmbH
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 920]<- 813
product_tbl_v2[grep(950, product_tbl_v2$CompanyId), ]

#Enchante Accessories, Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1009]<- 819
product_tbl_v2[grep(1009, product_tbl_v2$CompanyId), ]

#Too Faced Cosmetics
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1103]<- 943
product_tbl_v2[grep(1103, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1213]<- 943
product_tbl_v2[grep(1213, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1242]<- 943
product_tbl_v2[grep(1242, product_tbl_v2$CompanyId), ]

#Perfect Angel Cosmetics & Health Co., Limited
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1164]<- 1044
product_tbl_v2[grep(1164, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1187]<- 1044
product_tbl_v2[grep(1187, product_tbl_v2$CompanyId), ]

#Herbswork CC
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1342]<- 1136
product_tbl_v2[grep(1342, product_tbl_v2$CompanyId), ]

#Townley Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1212]<- 1140
product_tbl_v2[grep(1212, product_tbl_v2$CompanyId), ]

#HUDA BEAUTY
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1207]<- 1204
product_tbl_v2[grep(1207, product_tbl_v2$CompanyId), ]

#Alberto Culver USA, Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 783]<- 204
product_tbl_v2[grep(783, product_tbl_v2$CompanyId), ]

#American Consumer Products, LLC
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 844]<- 825
product_tbl_v2[grep(844, product_tbl_v2$CompanyId), ]

#Apollo Health and Beauty Care Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1178]<- 475
product_tbl_v2[grep(1178, product_tbl_v2$CompanyId), ]

#Arcadia Beauty Labs, LLC
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1093]<- 1086
product_tbl_v2[grep(1093, product_tbl_v2$CompanyId), ]

#Athena Cosmetics, Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 826]<- 190
product_tbl_v2[grep(826, product_tbl_v2$CompanyId), ]

#Cover FX Skincare Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1189]<- 542
product_tbl_v2[grep(1189, product_tbl_v2$CompanyId), ]

#Fresh, Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1215]<- 570
product_tbl_v2[grep(1215, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 843]<- 570
product_tbl_v2[grep(843, product_tbl_v2$CompanyId), ]

#Interparfums Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 984]<- 558
product_tbl_v2[grep(984, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1356]<- 558
product_tbl_v2[grep(1356, product_tbl_v2$CompanyId), ]

#Laboratories Dr N.G. Payot
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 939]<- 655
product_tbl_v2[grep(939, product_tbl_v2$CompanyId), ]

#KMC EXIM CORPORATION/DASHING DIVA FRANCHISE CORP
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 424]<- 310
product_tbl_v2[grep(424, product_tbl_v2$CompanyId), ]

#LVMH FRAGRANCE BRANDS
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 376]<- 373
product_tbl_v2[grep(376, product_tbl_v2$CompanyId), ]

#Nail Alliance, LLC
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1035]<- 1033
product_tbl_v2[grep(1035, product_tbl_v2$CompanyId), ]

#Shiseido Americas Corporation
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1011]<- 127
product_tbl_v2[grep(1011, product_tbl_v2$CompanyId), ]

product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 351]<- 127
product_tbl_v2[grep(351, product_tbl_v2$CompanyId), ]

#Stila Styles LLC
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1332]<- 291
product_tbl_v2[grep(1332, product_tbl_v2$CompanyId), ]

#Vi-Jon, Inc.
product_tbl_v2$CompanyId[product_tbl_v2$CompanyId == 1092]<- 615
product_tbl_v2[grep(1092, product_tbl_v2$CompanyId), ]

#Find and replace SubCategoryIds with newly assigned standardized Ids

#Hair Conditioners (leave in)
product_tbl_v2$SubCategoryId[product_tbl_v2$SubCategoryId == 33]<- 20
product_tbl_v2[grep(33, product_tbl_v2$SubCategoryId), ]

#Hair Conditioners (rinse out
product_tbl_v2$SubCategoryId[product_tbl_v2$SubCategoryId == 34]<- 21
product_tbl_v2[grep(34, product_tbl_v2$SubCategoryId), ]

#Hair Shampoos (making a cosmetic claim)
product_tbl_v2$SubCategoryId[product_tbl_v2$SubCategoryId == 38]<- 25
product_tbl_v2[grep(38, product_tbl_v2$SubCategoryId), ]

#Add Discontinued binary column
product_tbl_v3 <- product_tbl_v2 %>% 
  mutate(Discontinued = product_tbl_v2$DiscontinuedDate)

product_tbl_v3$Discontinued[which(is.na(product_tbl_v3$Discontinued) == F)]<-'1'
product_tbl_v3$Discontinued[which(is.na(product_tbl_v3$Discontinued) == T)]<-'0'

#Export tables

#Company Table
write.csv(company_tbl_v2, file = 'company_tbl_v2.csv')

#Chemical Table
write.csv(chemical_tbl_v2, file = 'chemical_tbl_v2.csv')

#Primary Category Table
write.csv(primarycategory_tbl_v2, file = 'primarycategory_tbl_v2.csv')

#SubCategory Table
write.csv(subcategory_tbl_v2, file = 'subcategory_tbl_v2.csv')

#Variant Table
write.csv(variant_tbl, file = 'variant_tbl.csv')

#VarChemId Table (bridge)
write.csv(varchemid_tbl_v3, file = 'varchemid_tbl_v3.csv')

#SubCatChemId Table (bridge)
write.csv(subcatchemid_tbl_v3, file = 'subcatchemid_tbl_v3.csv')

#Product Table
write.csv(product_tbl_v3, file = 'product_tbl_v3.csv')

