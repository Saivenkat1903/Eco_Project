#subgroup calculations

#below 100m data upload
Eco_14m <- read.csv("Eco_Project\\Original_data\\Eco_Data_14m.csv")
Eco_38m <-read_excel("Eco_Project\\Original_data\\Eco_Data_38m.xlsx")
Eco_16m <-read_excel("Eco_Project\\Original_data\\Eco_Data_16m.xlsx")
Eco_52m <-read_excel("Eco_Project\\Original_data\\Eco_Data_52m.xlsx")
Eco_37m <- read_excel("Eco_Project\\Original_data\\Eco_Data_37m.xlsx")
Eco_30m <- read_excel("Eco_Project\\Original_data\\Eco_Data_30m.xlsx")
Eco_21m <- read_excel("Eco_Project\\Original_data\\Eco_Data_21m.xlsx")
Eco_16m1 <- read_excel("Eco_Project\\Original_data\\Eco_Data_16m1.xlsx")


Master_data <-data.frame(matrix(nrow=0,ncol=1))
colnames(Master_data) <- c("Species")

for (i in Eco_14m$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_38m$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_16m$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_16m1$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_52m$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_37m$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_30m$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

for (i in Eco_21m$scientific_name){
  Master_data <- rbind(Master_data,i)
  rm(i)
}

write.xlsx(Master_data,"Eco_Project\\Processed_data\\Total_Observations_Below.xlsx")


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
Eco_240m <-read_excel("Eco_Project\\Original_data\\Eco_Data_240m.xlsx")
Eco_457m <- read_excel("Eco_Project\\Original_data\\Eco_Data_457m.xlsx")
Eco_131m <- read_excel("Eco_Project\\Original_data\\Eco_Data_131m.xlsx")
Eco_498m <- read_excel("Eco_Project\\Original_data\\Eco_Data_498m.xlsx")
Eco_884m <-read_excel("Eco_Project\\Original_data\\Eco_Data_884m.xlsx")
Eco_611m <-read_excel("Eco_Project\\Original_data\\Eco_Data_611m.xlsx")


Master_data_above <-data.frame(matrix(nrow=0,ncol=1))
colnames(Master_data_above) <- c("Species")

for (i in Eco_240m$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_457m$Species name`){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_131m$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_498m$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_884m$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

for (i in Eco_611m$scientific_name){
  Master_data_above <- rbind(Master_data_above,i)
  rm(i)
}

write.xlsx(Master_data_above,"Eco_Project\\Processed_data\\Total_Observations_Above.xlsx")


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
data1 <- read_excel("Eco_Project\\Processed_data\\Total_Observations_Below(Families).xlsx")
data2 <- read_excel("Eco_Project\\Processed_data\\Total_Observations_Above(Families).xlsx")

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

write.xlsx(Chart_above,"Eco_Project\\Processed_data\\All_data_Family_Accumulation.xlsx")



























