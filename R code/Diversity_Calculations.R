#Data upload
Eco_14m <- read.csv("Eco_Project\\Original_data\\Eco_Data_14m.csv")


#Diversity for 14m
Species_Freq <-data.frame(table(Eco_14m$taxon_family_name))

Total <- nrow(Eco_14m)

for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 884m
Eco_884m <-read_excel("Eco_Project\\Original_data\\Eco_Data_884m.xlsx")


#Diversity for 884m
Family_Freq <- data.frame(table(Eco_884m$Family))
Total <- sum(Eco_884m$`No of observations`)

for (k in seq(1,nrow(Family_Freq))){
  Family_Freq[k,3]=Family_Freq[k,2]/Total
  Family_Freq[k,4]=log(Family_Freq[k,3])
  Family_Freq[k,5]=Family_Freq[k,3]*Family_Freq[k,4]
  Family_Freq[k,6]=Family_Freq[k,3]**2
}

Shanon_total <- sum(Family_Freq[,5])
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Family_Freq[,6])
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 611m
Eco_Madhu <-read_excel("Eco_Project\\Original_data\\Eco_Data_611m.xlsx")

#Diversity for 611m
Species_Freq <-data.frame(table(Eco_611m$taxon_family_name))

Total <- nrow(Eco_611m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 498m
Eco_498m <- read_excel("Eco_Project\\Original_data\\Eco_Data_498m.xlsx")

#Diversity for 498m
Species_Freq <-data.frame(table(Eco_498m$taxon_family_name))

Total <- nrow(Eco_498m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 457m
Eco_457m <- read_excel("Eco_Project\\Original_data\\Eco_Data_457m.xlsx")

#Diversity for 457m
Species_Freq <-data.frame(table(Eco_457m$Family))

Total <- nrow(Eco_457m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 240m
Eco_240m<-read_excel("Eco_Project\\Original_data\\Eco_Data_240m.xlsx")

#Diversity for 240m
Species_Freq <-data.frame(table(Eco_240m$taxon_family_name))

Total <- nrow(Eco_240m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 131m
Eco_131m <- read_excel("Eco_Project\\Original_data\\Eco_Data_131m.xlsx")

#Diversity for 131m
Family_Freq <-data.frame(table(Eco_131m$taxon_family_name))

Total <- nrow(Eco_131m)


for (k in seq(1,nrow(Eco_131m))){
  Eco_131m[k,4]=Eco_131m[k,3]/Total
  Eco_131m[k,5]=log(Eco_131m[k,4])
  Eco_131m[k,6]=Eco_131m[k,5]*Eco_131m[k,4]
  Eco_131m[k,7]=Eco_131m[k,4]**2
}

for (k in seq(1,nrow(Family_Freq))){
  Family_Freq[k,3]=Family_Freq[k,2]/Total
  Family_Freq[k,4]=log(Family_Freq[k,3])
  Family_Freq[k,5]=Family_Freq[k,3]*Family_Freq[k,4]
  Family_Freq[k,6]=Family_Freq[k,3]**2
}

print(Eco_131m)

Shanon_total <- sum(Family_Freq[,5])
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Family_Freq[,6])
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 52m
Eco_52m <-read_excel("Eco_Project\\Original_data\\Eco_Data_52m.xlsx")

#Diversity for 52m
Species_Freq <-data.frame(table(Eco_Sabdhayani$taxon_family_name))

Total <- nrow(Eco_52m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 38m
Eco_38m <-read_excel("Eco_Project\\Original_data\\Eco_Data_38m.xlsx")

#Diversity for 38m
Species_Freq <-data.frame(table(Eco_38m$taxon_family_name))

Total <- nrow(Eco_38m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 37m
Eco_37m <- read_excel("Eco_Project\\Original_data\\Eco_Data_37m.xlsx")

#Diversity for 37m
Species_Freq <-data.frame(table(Eco_37m$taxon_family_name))

Total <- nrow(Eco_37m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 30m
Eco_30m <- read_excel("Eco_Project\\Original_data\\Eco_Data_30m.xlsx")

#Diversity for 30m
Species_Freq <-data.frame(table(Eco_30m$taxon_family_name))

Total <- nrow(Eco_30m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 21m
Eco_21m <- read_excel("Eco_Project\\Original_data\\Eco_Data_21m.xlsx")

#Diversity for 21m
Species_Freq <-data.frame(table(Eco_21m$taxon_family_name))

Total <- nrow(Eco_21m)


for (k in seq(1,nrow(Species_Freq))){
  Species_Freq[k,3]=Species_Freq[k,2]/Total
  Species_Freq[k,4]=log(Species_Freq[k,3])
  Species_Freq[k,5]=Species_Freq[k,3]*Species_Freq[k,4]
  Species_Freq[k,6]=Species_Freq[k,3]**2
}

print(Species_Freq)

Shanon_total <- sum(Species_Freq$V5)
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Species_Freq$V6)
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for 16m
Eco_16m <-read_excel("Eco_Project\\Original_data\\Eco_Data_16m.xlsx")

#Diversity for 16m

Family_Freq <- data.frame(table(Eco_16m$taxon_family_name))

Total <- sum(Family_Freq$Freq)

for (k in seq(1,nrow(Family_Freq))){
  Family_Freq[k,3]=Family_Freq[k,2]/Total
  Family_Freq[k,4]=log(Family_Freq[k,3])
  Family_Freq[k,5]=Family_Freq[k,3]*Family_Freq[k,4]
  Family_Freq[k,6]=Family_Freq[k,3]**2
}

for (k in seq(1,nrow(Eco_16m))){
  Eco_16m[k,6]=Eco_16m[k,4]/Total
  Eco_16m[k,7]=log(Eco_16m[k,6])
  Eco_16m[k,8]=Eco_16m[k,6]*Eco_16m[k,7]
  Eco_16m[k,9]=Eco_16m[k,6]**2
}

print(Eco_16m)

Shanon_total <- sum(Family_Freq[,5])
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Family_Freq[,6])
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())


#Data upload for 16m1
Eco_16m1 <- read_excel("Eco_Project\\Original_data\\Eco_Data_16m1.xlsx")

#Diversity for Shashank

Family_Freq <- data.frame(table(Eco_16m1$taxon_family_name))

Total <- sum(Family_Freq$Freq)

for (k in seq(1,nrow(Family_Freq))){
  Family_Freq[k,3]=Family_Freq[k,2]/Total
  Family_Freq[k,4]=log(Family_Freq[k,3])
  Family_Freq[k,5]=Family_Freq[k,3]*Family_Freq[k,4]
  Family_Freq[k,6]=Family_Freq[k,3]**2
}

for (k in seq(1,nrow(Eco_16m1))){
  Eco_16m1[k,6]=Eco_16m1[k,4]/Total
  Eco_16m1[k,7]=log(Eco_16m1[k,6])
  Eco_16m1[k,8]=Eco_16m1[k,6]*Eco_16m1[k,7]
  Eco_16m1[k,9]=Eco_16m1[k,6]**2
}


Shanon_total <- sum(Family_Freq[,5])
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Family_Freq[,6])
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())


#subgroup below
Family_Freq_below <- read_excel("Eco_Project\\Processed_data\\Master_data_family.xlsx")

Total_below <-sum(Family_Freq_below$Freq)

for (k in seq(1,nrow(Family_Freq_below))){
  Family_Freq_below[k,4]=Family_Freq_below[k,3]/Total_below
  Family_Freq_below[k,5]=log(Family_Freq_below[k,4])
  Family_Freq_below[k,6]=Family_Freq_below[k,5]*Family_Freq_below[k,4]
  Family_Freq_below[k,7]=Family_Freq_below[k,4]**2
}

write.xlsx(Family_Freq_below,"Eco_Project\\Processed_data\\\\Family_Freq_Below.xlsx")

#subgroup above
Total_family_above <- read_excel("Eco_Project\\Processed_data\\Total_Observations_Above (Species).xlsx")

Family_Freq_above <-data.frame(table(Total_family_above$Species))

Total_above <- nrow(Total_family_above)

for (k in seq(1,nrow(Family_Freq_above))){
  Family_Freq_above[k,3]=Family_Freq_above[k,2]/Total_above
  Family_Freq_above[k,4]=log(Family_Freq_above[k,3])
  Family_Freq_above[k,5]=Family_Freq_above[k,3]*Family_Freq_above[k,4]
  Family_Freq_above[k,6]=Family_Freq_above[k,3]**2
}

write.xlsx(Family_Freq_above,"Eco_Project\\Processed_data\\Species_Freq_Above.xlsx")


