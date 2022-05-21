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


Master_data <-data.frame(matrix(nrow=0,ncol=1))
colnames(Master_data) <- c("Species")

for (i in Eco_Sai$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_Rushdra$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_Mukesh_Clean$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_Shashank_Clean$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_Sabdhayani$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_Bhavana$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_Adhina$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_RC$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

write.xlsx(Master_data,"D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Total_Observations_Below.xlsx")


#Species accumulation below 100m
useful_below <- round_any(length(Master_data$Species), 10, f = ceiling)
x_axis_below <- seq(10,useful_below,10)
template_vector_below <- character(length(x_axis_below))


for (i in seq(1,length(template_vector_below))) {
  if (x_axis_below[i]<length(Master_data$Species)){
    y <-Master_data$Species[1:x_axis_below[i]]
    z <- length(unique(y))
    template_vector_below[i]=z
    rm(z,y,i)
  }
  else{
    y <-Master_data$Species
    z <- length(unique(y))
    template_vector_below[length(template_vector_below)]=z
    rm(z,y,i)
  }
}

No_of_species_below<-as.integer(template_vector_below)

Chart_below <- data.frame(x_axis_below,No_of_species_below)
Chart_below[nrow(Chart_below),1]=length(Master_data$Species)

ggplot(data=Chart_below,mapping=aes(x=x_axis_below,y=No_of_species_below))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of species")+ggtitle("Species accumulation curve for below 100m")



#Above 100m data upload
Eco_Srivani <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Srivani.xlsx")
Eco_Manasa <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Manasa.xlsx")
Eco_Raja <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Raja.xlsx")
Eco_Revanth <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Revanth.xlsx")
Eco_Meen <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Meenakshi.xlsx")
Eco_Madhu <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Madhu.xlsx")


Master_data_above <-data.frame(matrix(nrow=0,ncol=1))
colnames(Master_data_above) <- c("Species")

for (i in Eco_Srivani$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_Manasa$`Species name`){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_Raja$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_Revanth$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_Meen$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_Madhu$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

write.xlsx(Master_data_above,"D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Total_Observations_Above.xlsx")


#Species accumulation above 100m
useful_above <- round_any(length(Master_data_above$Species), 10, f = ceiling)
x_axis_above <- seq(10,useful_above,10)
template_vector_above <- character(length(x_axis_above))


for (i in seq(1,length(template_vector_above))) {
  if (x_axis_above[i]<length(Master_data_above$Species)){
    y <-Master_data_above$Species[1:x_axis_above[i]]
    z <- length(unique(y))
    template_vector_above[i]=z
    rm(z,y,i)
  }
  else{
    y <-Master_data_above$Species
    z <- length(unique(y))
    template_vector_above[length(template_vector_above)]=z
    rm(z,y,i)
  }
}

No_of_species_above<-as.integer(template_vector_above)

Chart_above <- data.frame(x_axis_above,No_of_species_above)
Chart_above[nrow(Chart_above),1]=length(Master_data_above$Species)

ggplot(data=Chart_above,mapping=aes(x=x_axis_above,y=No_of_species_above))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of species")+ggtitle("Species accumulation curve for above 100m")


#for all data
data1 <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Total_Observations_Below(Families).xlsx")
data2 <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Total_Observations_Above(Families).xlsx")

Main <- rbind(data1,data2)
set.seed(42)

rm(list=ls())

rows <- sample(nrow(Main))

Main <-Main[rows,]

useful_above <- round_any(length(Main$Family), 10, f = ceiling)
x_axis_above <- seq(10,useful_above,10)
template_vector_above <- character(length(x_axis_above))


for (i in seq(1,length(template_vector_above))) {
  if (x_axis_above[i]<length(Main$Family)){
    y <-Main$Family[1:x_axis_above[i]]
    z <- length(unique(y))
    template_vector_above[i]=z
    rm(z,y,i)
  }
  else{
    y <-Main$Family
    z <- length(unique(y))
    template_vector_above[length(template_vector_above)]=z
    rm(z,y,i)
  }
}

No_of_species_above<-as.integer(template_vector_above)

Chart_above <- data.frame(x_axis_above,No_of_species_above)
Chart_above[nrow(Chart_above),1]=length(Main$Family)


ggplot(data=Chart_above,mapping=aes(x=x_axis_above,y=No_of_species_above))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve for all data")

write.xlsx(Chart_above,"D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\All_data_Family_Accumulation.xlsx")



























