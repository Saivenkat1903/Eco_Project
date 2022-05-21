Species_below <- read_excel("Eco_Project\\Processed_data\\Species_Freq_Below.xlsx")
Species_above <- read_excel("Eco_Project\\Processed_data\\Species_Freq_Above.xlsx")
Species_below_family <- read_excel("Eco_Project\\Processed_data\\Family_Freq_Below.xlsx")
Species_above_family <-read_excel("Eco_Project\\Processed_data\\Family_Freq_Above.xlsx")


similar <- semi_join(Species_below,Species_above, by="Species")
in_below_not_above <- anti_join(Species_below,Species_above, by="Species")
in_above_not_below <- anti_join(Species_above,Species_below, by="Species")

similar_family <- semi_join(Species_above_family,Species_below_family, by="Family")
in_below_not_above_family <- anti_join(Species_below_family,Species_above_family, by="Family")
in_above_not_below_family <- anti_join(Species_above_family,Species_below_family, by="Family")

Jaccard <- nrow(similar)/(nrow(Species_above)+nrow(Species_below)-nrow(similar))

sorenson_CS <- (2*nrow(similar))/(nrow(Species_above)+nrow(Species_below))

  
Jaccard_family <- nrow(similar_family)/(nrow(Species_above_family)+nrow(Species_below_family)-nrow(similar_family))

sorenson_CS_family <- (2*nrow(similar_family))/(nrow(Species_above_family)+nrow(Species_below_family))
  

print(Jaccard)
print(sorenson_CS)

print(Jaccard_family)
print(sorenson_CS_family)

write.xlsx(in_below_not_above_family,"Eco_Project\\Processed_data\\Family_below_not_above.xlsx")


