#Eco data analysis


#Entering the data
Eco_Sai <- read.csv("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Sai.csv")
Eco_Madhu <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Madhu.xlsx")
Eco_Meen <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Meenakshi.xlsx")
Eco_Rushdra <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Rushdra.xlsx")
Eco_Mukesh <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Mukesh.xlsx")
Eco_Sabdhayani <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Sabdhayani.xlsx")
Eco_Srivani <-read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Srivani.xlsx")
Eco_Bhavana <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Bhavana.xlsx")
Eco_Revanth <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Revanth.xlsx")
Eco_Manasa <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Manasa.xlsx")
Eco_Raja <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Raja.xlsx")
Eco_Adhina <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Adhina.xlsx")
Eco_RC <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_RC.xlsx")
Eco_Shashank <- read_excel("D:\\Saivenkat\\College shizzle dizzle\\Semester 3\\BIO211\\Eco_Data_Shashank.xlsx")



#Getting Species names For Sai
species_column_Sai <- Eco_Sai$taxon_family_name
useful_Sai <- round_any(length(species_column_Sai), 10, f = ceiling)
x_axis_Sai <- seq(10,useful_Sai,10)
template_vector_Sai <- character(length(x_axis_Sai))

for (i in seq(1,length(template_vector_Sai))) {
  if (x_axis_Sai[i]<length(species_column_Sai)){
    y <-species_column_Sai[1:x_axis_Sai[i]]
    z <- length(unique(y))
    template_vector_Sai[i]=z
    rm(z,y,i)
  }
  else{
    y <-species_column_Sai
    z <- length(unique(y))
    template_vector_Sai[length(template_vector_Sai)]=z
    rm(z,y,i)
  }
}

No_of_species_Sai<-as.integer(template_vector_Sai)

Chart <- data.frame(x_axis_Sai,No_of_species_Sai)
Chart[nrow(Chart),1]=length(species_column_Sai)

ggplot(data=Chart,mapping=aes(x=x_axis_Sai,y=No_of_species_Sai))+geom_point()+xlim(0,170)+ylim(0,80)+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Family accumulation curve at 14m")


#Getting Species names For Madhu
species_column_Madhu <- Eco_Madhu$scientific_name
useful_Madhu <- round_any(length(species_column_Madhu), 10, f = ceiling)
x_axis_Madhu <- seq(10,useful_Madhu,10)
template_vector_Madhu <- character(length(x_axis_Madhu))


for (i in seq(1,length(template_vector_Madhu))) {
  if (x_axis_Madhu[i]<length(species_column_Madhu)){
    y <-species_column_Madhu[1:x_axis_Madhu[i]]
    z <- length(unique(y))
    template_vector_Madhu[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Madhu
    z <- length(unique(y))
    template_vector_Madhu[length(template_vector_Madhu)]=z
    rm(y,i,z)
  }
}

No_of_species_Madhu<-as.integer(template_vector_Madhu)

Chart_Madhu <- data.frame(x_axis_Madhu,No_of_species_Madhu)
Chart_Madhu[nrow(Chart_Madhu),1]=length(species_column_Madhu)

ggplot(data=Chart_Madhu,mapping=aes(x=x_axis_Madhu,y=No_of_species_Madhu))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Family accumulation curve at 611m")


#Getting Species for Meenakshi
species_column_Meen <- Eco_Meen$scientific_name
useful_Meen <- round_any(length(species_column_Meen), 10, f = ceiling)
x_axis_Meen <- seq(10,useful_Meen,10)
template_vector_Meen <- character(length(x_axis_Meen))


for (i in seq(1,length(template_vector_Meen))) {
  if (x_axis_Meen[i]<length(species_column_Meen)){
    y <-species_column_Meen[1:x_axis_Meen[i]]
    z <- length(unique(y))
    template_vector_Meen[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Meen
    z <- length(unique(y))
    template_vector_Meen[length(template_vector_Meen)]=z
    rm(y,i,z)
  }
}

No_of_species_Meen<-as.integer(template_vector_Meen)

Chart_Meen <- data.frame(x_axis_Meen,No_of_species_Meen)
Chart_Meen[nrow(Chart_Meen),1]=length(species_column_Meen)

ggplot(data=Chart_Meen,mapping=aes(x=x_axis_Meen,y=No_of_species_Meen))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Families accumulation curve at 884m")


#Getting Species for Srivani
species_column_Srivani <- Eco_Srivani$scientific_name
useful_Srivani <- round_any(length(species_column_Srivani), 10, f = ceiling)
x_axis_Srivani <- seq(10,useful_Srivani,10)
template_vector_Srivani <- character(length(x_axis_Srivani))


for (i in seq(1,length(template_vector_Srivani))) {
  if (x_axis_Srivani[i]<length(species_column_Srivani)){
    y <-species_column_Srivani[1:x_axis_Srivani[i]]
    z <- length(unique(y))
    template_vector_Srivani[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Srivani
    z <- length(unique(y))
    template_vector_Srivani[length(template_vector_Srivani)]=z
    rm(y,i,z)
  }
}

No_of_species_Srivani<-as.integer(template_vector_Srivani)

Chart_Srivani <- data.frame(x_axis_Srivani,No_of_species_Srivani)
Chart_Srivani[nrow(Chart_Srivani),1]=length(species_column_Srivani)

ggplot(data=Chart_Srivani,mapping=aes(x=x_axis_Srivani,y=No_of_species_Srivani))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of species")+ggtitle("Species accumulation curve at 240m")


#Getting Species for Bhavana
species_column_Bhavana <- Eco_Bhavana$scientific_name
useful_Bhavana <- round_any(length(species_column_Bhavana), 10, f = ceiling)
x_axis_Bhavana <- seq(10,useful_Bhavana,10)
template_vector_Bhavana <- character(length(x_axis_Bhavana))


for (i in seq(1,length(template_vector_Bhavana))) {
  if (x_axis_Bhavana[i]<length(species_column_Bhavana)){
    y <-species_column_Bhavana[1:x_axis_Bhavana[i]]
    z <- length(unique(y))
    template_vector_Bhavana[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Bhavana
    z <- length(unique(y))
    template_vector_Bhavana[length(template_vector_Bhavana)]=z
    rm(y,i,z)
  }
}

No_of_species_Bhavana<-as.integer(template_vector_Bhavana)

Chart_Bhavana <- data.frame(x_axis_Bhavana,No_of_species_Bhavana)

Chart_Bhavana[nrow(Chart_Bhavana),1]=length(species_column_Bhavana)

ggplot(data=Chart_Bhavana,mapping=aes(x=x_axis_Bhavana,y=No_of_species_Bhavana))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Families accumulation curve at 37m")

#Getting Species for Shashank
species_column_Shashank <- Eco_Shashank_Clean$X.Cenostigma.eriostachys.
useful_Shashank <- round_any(length(species_column_Shashank), 10, f = ceiling)
x_axis_Shashank <- seq(10,useful_Shashank,10)
template_vector_Shashank <- character(length(x_axis_Shashank))


for (i in seq(1,length(template_vector_Shashank))) {
  if (x_axis_Shashank[i]<length(species_column_Shashank)){
    y <-species_column_Shashank[1:x_axis_Shashank[i]]
    z <- length(unique(y))
    template_vector_Shashank[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Shashank
    z <- length(unique(y))
    template_vector_Shashank[length(template_vector_Shashank)]=z
    rm(y,i,z)
  }
}

No_of_species_Shashank<-as.integer(template_vector_Shashank)

Chart_Shashank <- data.frame(x_axis_Shashank,No_of_species_Shashank)
Chart_Shashank[nrow(Chart_Shashank),1]=length(species_column_Shashank)

ggplot(data=Chart_Shashank,mapping=aes(x=x_axis_Shashank,y=No_of_species_Shashank))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 16m")

#Getting Species for Sabdhayani
species_column_Sabdhayani <- Eco_Sabdhayani$scientific_name
useful_Sabdhayani <- round_any(length(species_column_Sabdhayani), 10, f = ceiling)
x_axis_Sabdhayani <- seq(10,useful_Sabdhayani,10)
template_vector_Sabdhayani <- character(length(x_axis_Sabdhayani))


for (i in seq(1,length(template_vector_Sabdhayani))) {
  if (x_axis_Sabdhayani[i]<length(species_column_Sabdhayani)){
    y <-species_column_Sabdhayani[1:x_axis_Sabdhayani[i]]
    z <- length(unique(y))
    template_vector_Sabdhayani[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Sabdhayani
    z <- length(unique(y))
    template_vector_Sabdhayani[length(template_vector_Sabdhayani)]=z
    rm(y,i,z)
  }
}

No_of_species_Sabdhayani<-as.integer(template_vector_Sabdhayani)

Chart_Sabdhayani <- data.frame(x_axis_Sabdhayani,No_of_species_Sabdhayani)

Chart_Sabdhayani[nrow(Chart_Sabdhayani),1]=length(species_column_Sabdhayani)

ggplot(data=Chart_Sabdhayani,mapping=aes(x=x_axis_Sabdhayani,y=No_of_species_Sabdhayani))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 52m")


#Getting Species for Revanth
species_column_Revanth <- Eco_Revanth$scientific_name
useful_Revanth <- round_any(length(species_column_Revanth), 10, f = ceiling)
x_axis_Revanth <- seq(10,useful_Revanth,10)
template_vector_Revanth <- character(length(x_axis_Revanth))


for (i in seq(1,length(template_vector_Revanth))) {
  if (x_axis_Revanth[i]<length(species_column_Revanth)){
    y <-species_column_Revanth[1:x_axis_Revanth[i]]
    z <- length(unique(y))
    template_vector_Revanth[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Revanth
    z <- length(unique(y))
    template_vector_Revanth[length(template_vector_Revanth)]=z
    rm(y,i,z)
  }
}

No_of_species_Revanth<-as.integer(template_vector_Revanth)

Chart_Revanth <- data.frame(x_axis_Revanth,No_of_species_Revanth)

Chart_Revanth[nrow(Chart_Revanth),1]=length(species_column_Revanth)

ggplot(data=Chart_Revanth,mapping=aes(x=x_axis_Revanth,y=No_of_species_Revanth))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of familiies")+ggtitle("Family accumulation curve at 498m")



#Getting Species for Manasa
species_column_Manasa <- Eco_Manasa$`Species name`
useful_Manasa <- round_any(length(species_column_Manasa), 10, f = ceiling)
x_axis_Manasa <- seq(10,useful_Manasa,10)
template_vector_Manasa <- character(length(x_axis_Manasa))


for (i in seq(1,length(template_vector_Manasa))) {
  if (x_axis_Manasa[i]<length(species_column_Manasa)){
    y <-species_column_Manasa[1:x_axis_Manasa[i]]
    z <- length(unique(y))
    template_vector_Manasa[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Manasa
    z <- length(unique(y))
    template_vector_Manasa[length(template_vector_Manasa)]=z
    rm(y,i,z)
  }
}

No_of_species_Manasa<-as.integer(template_vector_Manasa)

Chart_Manasa <- data.frame(x_axis_Manasa,No_of_species_Manasa)

Chart_Manasa[nrow(Chart_Manasa),1]=length(species_column_Manasa)

ggplot(data=Chart_Manasa,mapping=aes(x=x_axis_Manasa,y=No_of_species_Manasa))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 457m")


#Getting Species for Raja
species_column_Raja <- Eco_Raja$taxon_family_name
useful_Raja <- round_any(length(species_column_Raja), 10, f = ceiling)
x_axis_Raja <- seq(10,useful_Raja,10)
template_vector_Raja <- character(length(x_axis_Raja))


for (i in seq(1,length(template_vector_Raja))) {
  if (x_axis_Raja[i]<length(species_column_Raja)){
    y <-species_column_Raja[1:x_axis_Raja[i]]
    z <- length(unique(y))
    template_vector_Raja[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Raja
    z <- length(unique(y))
    template_vector_Raja[length(template_vector_Raja)]=z
    rm(y,i,z)
  }
}

No_of_species_Raja<-as.integer(template_vector_Raja)

Chart_Raja <- data.frame(x_axis_Raja,No_of_species_Raja)

Chart_Raja[nrow(Chart_Raja),1]=length(species_column_Raja)

ggplot(data=Chart_Raja,mapping=aes(x=x_axis_Raja,y=No_of_species_Raja))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Family accumulation curve at 131m")


#Getting Species for Rushdra
species_column_Rushdra <- Eco_Rushdra$scientific_name
useful_Rushdra <- round_any(length(species_column_Rushdra), 10, f = ceiling)
x_axis_Rushdra <- seq(10,useful_Rushdra,10)
template_vector_Rushdra <- character(length(x_axis_Rushdra))


for (i in seq(1,length(template_vector_Rushdra))) {
  if (x_axis_Rushdra[i]<length(species_column_Rushdra)){
    y <-species_column_Rushdra[1:x_axis_Rushdra[i]]
    z <- length(unique(y))
    template_vector_Rushdra[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Rushdra
    z <- length(unique(y))
    template_vector_Rushdra[length(template_vector_Rushdra)]=z
    rm(y,i,z)
  }
}

No_of_species_Rushdra<-as.integer(template_vector_Rushdra)

Chart_Rushdra <- data.frame(x_axis_Rushdra,No_of_species_Rushdra)

Chart_Rushdra[nrow(Chart_Rushdra),1]=length(species_column_Rushdra)

ggplot(data=Chart_Rushdra,mapping=aes(x=x_axis_Rushdra,y=No_of_species_Rushdra))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 38m")


#Getting Species for Adhina
species_column_Adhina <- Eco_Adhina$scientific_name
useful_Adhina <- round_any(length(species_column_Adhina), 10, f = ceiling)
x_axis_Adhina <- seq(10,useful_Adhina,10)
template_vector_Adhina <- character(length(x_axis_Adhina))


for (i in seq(1,length(template_vector_Adhina))) {
  if (x_axis_Adhina[i]<length(species_column_Adhina)){
    y <-species_column_Adhina[1:x_axis_Adhina[i]]
    z <- length(unique(y))
    template_vector_Adhina[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Adhina
    z <- length(unique(y))
    template_vector_Adhina[length(template_vector_Adhina)]=z
    rm(y,i,z)
  }
}

No_of_species_Adhina<-as.integer(template_vector_Adhina)

Chart_Adhina <- data.frame(x_axis_Adhina,No_of_species_Adhina)

Chart_Adhina[nrow(Chart_Adhina),1]=length(species_column_Adhina)

ggplot(data=Chart_Adhina,mapping=aes(x=x_axis_Adhina,y=No_of_species_Adhina))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 30m")


#Getting Species for RC Vishnu
species_column_RC <- Eco_RC$scientific_name
useful_RC <- round_any(length(species_column_RC), 10, f = ceiling)
x_axis_RC <- seq(10,useful_RC,10)
template_vector_RC <- character(length(x_axis_RC))


for (i in seq(1,length(template_vector_RC))) {
  if (x_axis_RC[i]<length(species_column_RC)){
    y <-species_column_RC[1:x_axis_RC[i]]
    z <- length(unique(y))
    template_vector_RC[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_RC
    z <- length(unique(y))
    template_vector_RC[length(template_vector_RC)]=z
    rm(y,i,z)
  }
}

No_of_species_RC<-as.integer(template_vector_RC)

Chart_RC <- data.frame(x_axis_RC,No_of_species_RC)

Chart_RC[nrow(Chart_RC),1]=length(species_column_RC)

ggplot(data=Chart_RC,mapping=aes(x=x_axis_RC,y=No_of_species_RC))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 21m")


#Getting Species for Mukesh
species_column_Mukesh <- Eco_Mukesh_Clean$X.Calotropis.gigantea.
useful_Mukesh <- round_any(length(species_column_Mukesh), 10, f = ceiling)
x_axis_Mukesh <- seq(10,useful_Mukesh,10)
template_vector_Mukesh <- character(length(x_axis_Mukesh))


for (i in seq(1,length(template_vector_Mukesh))) {
  if (x_axis_Mukesh[i]<length(species_column_Mukesh)){
    y <-species_column_Mukesh[1:x_axis_Mukesh[i]]
    z <- length(unique(y))
    template_vector_Mukesh[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_Mukesh
    z <- length(unique(y))
    template_vector_Mukesh[length(template_vector_Mukesh)]=z
    rm(y,i,z)
  }
}

No_of_species_Mukesh<-as.integer(template_vector_Mukesh)

Chart_Mukesh <- data.frame(x_axis_Mukesh,No_of_species_Mukesh)

Chart_Mukesh[nrow(Chart_Mukesh),1]=length(species_column_Mukesh)

ggplot(data=Chart_Mukesh,mapping=aes(x=x_axis_Mukesh,y=No_of_species_Mukesh))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 16m")



