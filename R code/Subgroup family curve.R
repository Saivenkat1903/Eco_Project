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


Master_data_family <-data.frame(matrix(nrow=0,ncol=1))
colnames(Master_data_family) <- c("Family")

for (i in Eco_14m$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_38m$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_16m_Clean$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_16m1_Clean$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_52m$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_37m$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_30m$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

for (i in Eco_21m$taxon_family_name){
  Master_data_family <- rbind(Master_data_family,i)
  rm(i)
}

write.xlsx(Master_data_family,"Eco_Project\\Processed_data\\Total_Observations_Below(Families).xlsx")



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
Eco_240m <-read_excel("Eco_Project\\Original_data\\Eco_Data_240m.xlsx")
Eco_457m <- read_excel("Eco_Project\\Original_data\\Eco_Data_457m.xlsx")
Eco_131m <- read_excel("Eco_Project\\Original_data\\Eco_Data_131m.xlsx")
Eco_498m <- read_excel("Eco_Project\\Original_data\\Eco_Data_498m.xlsx")
Eco_884m <-read_excel("Eco_Project\\Original_data\\Eco_Data_884m.xlsx")
Eco_611m <-read_excel("Eco_Project\\Original_data\\Eco_Data_611m.xlsx")


Master_data_above_family <-data.frame(matrix(nrow=0,ncol=1))
colnames(Master_data_above_family) <- c("Family")

for (i in Eco_240m$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_457m$Family){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_131m$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_498m$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_884m$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

for (i in Eco_611m$taxon_family_name){
  Master_data_above_family <- rbind(Master_data_above_family,i)
  rm(i)
}

write.xlsx(Master_data_above_family,"Eco_Project\\Processed_data\\Total_Observations_Above(Families).xlsx")


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


write.xlsx(Master_data_above_family,"Eco_Project\\Processed_data\\Total_Observations_Above(Species).xlsx")















