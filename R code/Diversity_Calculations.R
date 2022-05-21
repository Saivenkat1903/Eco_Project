#Data upload
Eco_Sai <- read.csv("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Sai.csv")


#Diversity for Sai
Species_Freq <-data.frame(table(Eco_Sai$taxon_family_name))

Total <- nrow(Eco_Sai)

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

#Data upload for Meenakshi
Eco_Meen <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Meenakshi.xlsx")


#Diversity for Meenakshi
Family_Freq <- data.frame(table(Eco_Meen_Clean$Family))
Total <- sum(Eco_Meen$`No of observations`)

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

#Data upload for Madhu
Eco_Madhu <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Madhu.xlsx")

#Diversity for Madhu
Species_Freq <-data.frame(table(Eco_Madhu$taxon_family_name))

Total <- nrow(Eco_Madhu)


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

#Data upload for Revanth
Eco_Revanth <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Revanth.xlsx")

#Diversity for Revanth
Species_Freq <-data.frame(table(Eco_Revanth$taxon_family_name))

Total <- nrow(Eco_Revanth)


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

#Data upload for Manasa
Eco_Manasa <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Manasa.xlsx")

#Diversity for Manasa
Species_Freq <-data.frame(table(Eco_Manasa$Family))

Total <- nrow(Eco_Manasa)


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

#Data upload for Srivani
Eco_Srivani <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Srivani.xlsx")

#Diversity for Srivani
Species_Freq <-data.frame(table(Eco_Srivani$taxon_family_name))

Total <- nrow(Eco_Srivani)


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

#Data upload for Rajagopal
Eco_Raja <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Raja.xlsx")

#Diversity for Rajagopal
Family_Freq <-data.frame(table(Eco_Raja$taxon_family_name))

Total <- nrow(Eco_Raja)


for (k in seq(1,nrow(Eco_Raja_Clean))){
  Eco_Raja[k,4]=Eco_Raja[k,3]/Total
  Eco_Raja[k,5]=log(Eco_Raja[k,4])
  Eco_Raja[k,6]=Eco_Raja[k,5]*Eco_Raja[k,4]
  Eco_Raja[k,7]=Eco_Raja[k,4]**2
}

for (k in seq(1,nrow(Family_Freq))){
  Family_Freq[k,3]=Family_Freq[k,2]/Total
  Family_Freq[k,4]=log(Family_Freq[k,3])
  Family_Freq[k,5]=Family_Freq[k,3]*Family_Freq[k,4]
  Family_Freq[k,6]=Family_Freq[k,3]**2
}

print(Eco_Raja)

Shanon_total <- sum(Family_Freq[,5])
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Family_Freq[,6])
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())

#Data upload for Sabdhayani
Eco_Sabdhayani <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Sabdhayani.xlsx")

#Diversity for Sabdhayani
Species_Freq <-data.frame(table(Eco_Sabdhayani$taxon_family_name))

Total <- nrow(Eco_Sabdhayani)


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

#Data upload for Rushdra
Eco_Rushdra <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Rushdra.xlsx")

#Diversity for Rushdra
Species_Freq <-data.frame(table(Eco_Rushdra$taxon_family_name))

Total <- nrow(Eco_Rushdra)


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

#Data upload for Bhavana
Eco_Bhavana <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Bhavana.xlsx")

#Diversity for Bhavana
Species_Freq <-data.frame(table(Eco_Bhavana$taxon_family_name))

Total <- nrow(Eco_Bhavana)


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

#Data upload for Adhina
Eco_Adhina <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Adhina.xlsx")

#Diversity for Adhina
Species_Freq <-data.frame(table(Eco_Adhina$taxon_family_name))

Total <- nrow(Eco_Adhina)


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

#Data upload for RC Vishnu
Eco_RC <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_RC.xlsx")

#Diversity for RC Vishnu
Species_Freq <-data.frame(table(Eco_RC$taxon_family_name))

Total <- nrow(Eco_RC)


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

#Data upload for Mukesh
Eco_Mukesh <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Mukesh.xlsx")

#Diversity for Mukesh

Family_Freq <- data.frame(table(Eco_Mukesh_Clean$taxon_family_name))

Total <- sum(Family_Freq$Freq)

for (k in seq(1,nrow(Family_Freq))){
  Family_Freq[k,3]=Family_Freq[k,2]/Total
  Family_Freq[k,4]=log(Family_Freq[k,3])
  Family_Freq[k,5]=Family_Freq[k,3]*Family_Freq[k,4]
  Family_Freq[k,6]=Family_Freq[k,3]**2
}

for (k in seq(1,nrow(Eco_Mukesh))){
  Eco_Mukesh[k,6]=Eco_Mukesh[k,4]/Total
  Eco_Mukesh[k,7]=log(Eco_Mukesh[k,6])
  Eco_Mukesh[k,8]=Eco_Mukesh[k,6]*Eco_Mukesh[k,7]
  Eco_Mukesh[k,9]=Eco_Mukesh[k,6]**2
}

print(Eco_Mukesh)

Shanon_total <- sum(Family_Freq[,5])
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Family_Freq[,6])
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())


#Data upload for Shashank
Eco_Shashank <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Shashank.xlsx")

#Diversity for Shashank

Family_Freq <- data.frame(table(Eco_Shashank_Clean$taxon_family_name))

Total <- sum(Family_Freq$Freq)

for (k in seq(1,nrow(Family_Freq))){
  Family_Freq[k,3]=Family_Freq[k,2]/Total
  Family_Freq[k,4]=log(Family_Freq[k,3])
  Family_Freq[k,5]=Family_Freq[k,3]*Family_Freq[k,4]
  Family_Freq[k,6]=Family_Freq[k,3]**2
}

for (k in seq(1,nrow(Eco_Mukesh))){
  Eco_Mukesh[k,6]=Eco_Mukesh[k,4]/Total
  Eco_Mukesh[k,7]=log(Eco_Mukesh[k,6])
  Eco_Mukesh[k,8]=Eco_Mukesh[k,6]*Eco_Mukesh[k,7]
  Eco_Mukesh[k,9]=Eco_Mukesh[k,6]**2
}


Shanon_total <- sum(Family_Freq[,5])
Shanon_Diversity <- Shanon_total*(-1)

Simpson_Total <- sum(Family_Freq[,6])
Simpson_Diversity <- 1-Simpson_Total

print(Shanon_Diversity)
print(Simpson_Diversity)
rm(list=ls())


#subgroup stuff
Family_Freq_below <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Master_data_family.xlsx")

Total_below <-sum(Family_Freq_below$Freq)

for (k in seq(1,nrow(Family_Freq_below))){
  Family_Freq_below[k,4]=Family_Freq_below[k,3]/Total_below
  Family_Freq_below[k,5]=log(Family_Freq_below[k,4])
  Family_Freq_below[k,6]=Family_Freq_below[k,5]*Family_Freq_below[k,4]
  Family_Freq_below[k,7]=Family_Freq_below[k,4]**2
}

write.xlsx(Family_Freq_below,"D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Family_Freq_Below.xlsx")

#more subgroup stuff
Total_family_above <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Total_Observations_Above (Species).xlsx")

Family_Freq_above <-data.frame(table(Total_family_above$Species))

Total_above <- nrow(Total_family_above)

for (k in seq(1,nrow(Family_Freq_above))){
  Family_Freq_above[k,3]=Family_Freq_above[k,2]/Total_above
  Family_Freq_above[k,4]=log(Family_Freq_above[k,3])
  Family_Freq_above[k,5]=Family_Freq_above[k,3]*Family_Freq_above[k,4]
  Family_Freq_above[k,6]=Family_Freq_above[k,3]**2
}

write.xlsx(Family_Freq_above,"D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Species_Freq_Above.xlsx")


