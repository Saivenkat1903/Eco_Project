#subgroup calculations

#below 100m data upload
Eco_Sai <- read.csv("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Sai.csv")
Eco_Rushdra <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Rushdra.xlsx")
Eco_Mukesh <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Mukesh.xlsx")
Eco_Sabdhayani <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Sabdhayani.xlsx")
Eco_Bhavana <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Bhavana.xlsx")
Eco_Adhina <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Adhina.xlsx")
Eco_RC <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_RC.xlsx")
Eco_Shashank <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Shashank.xlsx")


Master_data_family <-data.frame(matrix(nrow=0,ncol=1))
colnames(Master_data_family) <- c("Family")

for (i in Eco_Sai$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_Rushdra$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_Mukesh_Clean$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_Shashank_Clean$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_Sabdhayani$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_Bhavana$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_Adhina$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_RC$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

write.xlsx(Master_data_family,"D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Total_Observations_Below(Families).xlsx")



#Families accumulation below 100m
useful_below_family <- round_any(length(Master_data_family$Family), 10, f = ceiling)
x_axis_below_family <- seq(10,useful_below_family,10)
template_vector_below_family <- character(length(x_axis_below_family))


for (i in seq(1,length(template_vector_below_family))) {
  if (x_axis_below_family[i]<length(Master_data_family$Family)){
    y <-Master_data_family$Family[1:x_axis_below_family[i]]
    z <- length(unique(y))
    template_vector_below_family[i]=z
    rm(z,y,i)
  }
  else{
    y <-Master_data_family$Family
    z <- length(unique(y))
    template_vector_below_family[length(template_vector_below_family)]=z
    rm(z,y,i)
  }
}

No_of_species_below_family<-as.integer(template_vector_below_family)

Chart_below_family <- data.frame(x_axis_below_family,No_of_species_below_family)
Chart_below_family[nrow(Chart_below_family),1]=length(Master_data_family$Family)

ggplot(data=Chart_below_family,mapping=aes(x=x_axis_below_family,y=No_of_species_below_family))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of species")+ggtitle("Families accumulation curve for below 100m")



#Above 100m data upload
Eco_Srivani <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Srivani.xlsx")
Eco_Manasa <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Manasa.xlsx")
Eco_Raja <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Raja.xlsx")
Eco_Revanth <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Revanth.xlsx")
Eco_Meen <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Meenakshi.xlsx")
Eco_Madhu <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Madhu.xlsx")


Master_data_above_family <-data.frame(matrix(nrow=0,ncol=1))
colnames(Master_data_above_family) <- c("Family")

for (i in Eco_Srivani$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_Manasa$Family){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_Raja$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_Revanth$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_Meen$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_Madhu$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

write.xlsx(Master_data_above_family,"D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Total_Observations_Above(Families).xlsx")


#Species accumulation above 100m
useful_above_family <- round_any(length(Master_data_above_family$Family), 10, f = ceiling)
x_axis_above_family <- seq(10,useful_above_family,10)
template_vector_above_family <- character(length(x_axis_above_family))


for (i in seq(1,length(template_vector_above_family))) {
  if (x_axis_above_family[i]<length(Master_data_above_family$Family)){
    y <-Master_data_above_family$Family[1:x_axis_above_family[i]]
    z <- length(unique(y))
    template_vector_above_family[i]=z
    rm(z,y,i)
  }
  else{
    y <-Master_data_above_family$Family
    z <- length(unique(y))
    template_vector_above_family[length(template_vector_above_family)]=z
    rm(z,y,i)
  }
}

No_of_species_above_family<-as.integer(template_vector_above_family)

Chart_above_family <- data.frame(x_axis_above_family,No_of_species_above_family)
Chart_above_family[nrow(Chart_above_family),1]=length(Master_data_above_family$Family)

ggplot(data=Chart_above_family,mapping=aes(x=x_axis_above_family,y=No_of_species_above_family))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of species")+ggtitle("Families accumulation curve for above 100m")


















