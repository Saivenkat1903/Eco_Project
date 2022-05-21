#Eco data analysis


#Entering the data
Eco_14m <- read.csv("Eco_Project\\Original_data\\Eco_Data_14m.csv")
Eco_611m <-read_excel("Eco_Project\\Original_data\\Eco_Data_611m.xlsx")
Eco_884m <-read_excel("Eco_Project\\Original_data\\Eco_Data_884m.xlsx")
Eco_38m <-read_excel("Eco_Project\\Original_data\\Eco_Data_38m.xlsx")
Eco_16m <-read_excel("Eco_Project\\Original_data\\Eco_Data_16m.xlsx")
Eco_52m <-read_excel("Eco_Project\\Original_data\\Eco_Data_52m.xlsx")
Eco_240m <-read_excel("Eco_Project\\Original_data\\Eco_Data_240m.xlsx")
Eco_37m <- read_excel("Eco_Project\\Original_data\\Eco_Data_37m.xlsx")
Eco_498m <- read_excel("Eco_Project\\Original_data\\Eco_Data_498m.xlsx")
Eco_457m <- read_excel("Eco_Project\\Original_data\\Eco_Data_457m.xlsx")
Eco_131m <- read_excel("Eco_Project\\Original_data\\Eco_Data_131m.xlsx")
Eco_30m <- read_excel("Eco_Project\\Original_data\\Eco_Data_30m.xlsx")
Eco_21m <- read_excel("Eco_Project\\Original_data\\Eco_Data_21m.xlsx")
Eco_16m1 <- read_excel("Eco_Project\\Original_data\\Eco_Data_16m1.xlsx")



#Getting Species names For 14m
species_column_14m <- Eco_14m$taxon_family_name
useful_14m <- round_any(length(species_column_14m), 10, f = ceiling)
x_axis_14m <- seq(10,useful_14m,10)
template_vector_14m <- character(length(x_axis_14m))

for (i in seq(1,length(template_vector_14m))) {
  if (x_axis_14m[i]<length(species_column_14m)){
    y <-species_column_14m[1:x_axis_14m[i]]
    z <- length(unique(y))
    template_vector_14m[i]=z
    rm(z,y,i)
  }
  else{
    y <-species_column_14m
    z <- length(unique(y))
    template_vector_14m[length(template_vector_14m)]=z
    rm(z,y,i)
  }
}

No_of_species_14m<-as.integer(template_vector_14m)

Chart <- data.frame(x_axis_14m,No_of_species_14m)
Chart[nrow(Chart),1]=length(species_column_14m)

ggplot(data=Chart,mapping=aes(x=x_axis_14m,y=No_of_species_14m))+geom_point()+xlim(0,170)+ylim(0,80)+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Family accumulation curve at 14m")


#Getting Species names For 611m
species_column_611m <- Eco_611m$scientific_name
useful_611m <- round_any(length(species_column_611m), 10, f = ceiling)
x_axis_611m <- seq(10,useful_611m,10)
template_vector_611m <- character(length(x_axis_611m))


for (i in seq(1,length(template_vector_611m))) {
  if (x_axis_611m[i]<length(species_column_611m)){
    y <-species_column_611m[1:x_axis_611m[i]]
    z <- length(unique(y))
    template_vector_611m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_611m
    z <- length(unique(y))
    template_vector_611m[length(template_vector_611m)]=z
    rm(y,i,z)
  }
}

No_of_species_611m<-as.integer(template_vector_611m)

Chart_611m <- data.frame(x_axis_Madhu,No_of_species_611m)
Chart_611m[nrow(Chart_611m),1]=length(species_column_611m)

ggplot(data=Chart_611m,mapping=aes(x=x_axis_611m,y=No_of_species_611m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Family accumulation curve at 611m")


#Getting Species for 884m
species_column_884m <- Eco_884m$scientific_name
useful_884m <- round_any(length(species_column_884m), 10, f = ceiling)
x_axis_884m <- seq(10,useful_884m,10)
template_vector_884m <- character(length(x_axis_884m))


for (i in seq(1,length(template_vector_884m))) {
  if (x_axis_884m[i]<length(species_column_884m)){
    y <-species_column_884m[1:x_axis_884m[i]]
    z <- length(unique(y))
    template_vector_884m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_884m
    z <- length(unique(y))
    template_vector_884m[length(template_vector_884m)]=z
    rm(y,i,z)
  }
}

No_of_species_884m<-as.integer(template_vector_884m)

Chart_884m <- data.frame(x_axis_884m,No_of_species_884m)
Chart_884m[nrow(Chart_884m),1]=length(species_column_884m)

ggplot(data=Chart_884m,mapping=aes(x=x_axis_884m,y=No_of_species_884m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Families accumulation curve at 884m")


#Getting Species for 240m
species_column_240m <- Eco_240m$scientific_name
useful_240m <- round_any(length(species_column_240m), 10, f = ceiling)
x_axis_240m <- seq(10,useful_240m,10)
template_vector_240m <- character(length(x_axis_240m))


for (i in seq(1,length(template_vector_240m))) {
  if (x_axis_240m[i]<length(species_column_240m)){
    y <-species_column_240m[1:x_axis_240m[i]]
    z <- length(unique(y))
    template_vector_240m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_240m
    z <- length(unique(y))
    template_vector_240m[length(template_vector_240m)]=z
    rm(y,i,z)
  }
}

No_of_species_240m<-as.integer(template_vector_240m)

Chart_240m <- data.frame(x_axis_240m,No_of_species_240m)
Chart_240m[nrow(Chart_240m),1]=length(species_column_240m)

ggplot(data=Chart_240m,mapping=aes(x=x_axis_240m,y=No_of_species_240m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of species")+ggtitle("Species accumulation curve at 240m")


#Getting Species for 37m
species_column_37m <- Eco_37m$scientific_name
useful_37m <- round_any(length(species_column_37m), 10, f = ceiling)
x_axis_37m <- seq(10,useful_37m,10)
template_vector_37m <- character(length(x_axis_37m))


for (i in seq(1,length(template_vector_37m))) {
  if (x_axis_37m[i]<length(species_column_37m)){
    y <-species_column_37m[1:x_axis_37m[i]]
    z <- length(unique(y))
    template_vector_37m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_37m
    z <- length(unique(y))
    template_vector_37m[length(template_vector_37m)]=z
    rm(y,i,z)
  }
}

No_of_species_37m<-as.integer(template_vector_37m)

Chart_37m <- data.frame(x_axis_37m,No_of_species_37m)

Chart_37m[nrow(Chart_37m),1]=length(species_column_37m)

ggplot(data=Chart_37m,mapping=aes(x=x_axis_37m,y=No_of_species_37m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Families accumulation curve at 37m")


#Getting Species for 16m1
species_column_16m1 <- Eco_16m1_Clean$X.Cenostigma.eriostachys.
useful_16m1 <- round_any(length(species_column_16m1), 10, f = ceiling)
x_axis_16m1 <- seq(10,useful_16m1,10)
template_vector_16m1 <- character(length(x_axis_16m1))


for (i in seq(1,length(template_vector_16m1))) {
  if (x_axis_16m1[i]<length(species_column_16m1)){
    y <-species_column_16m1[1:x_axis_16m1[i]]
    z <- length(unique(y))
    template_vector_16m1[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_16m1
    z <- length(unique(y))
    template_vector_16m1[length(template_vector_16m1)]=z
    rm(y,i,z)
  }
}

No_of_species_16m1<-as.integer(template_vector_16m1)

Chart_16m1 <- data.frame(x_axis_16m1,No_of_species_16m1)
Chart_16m1[nrow(Chart_16m1),1]=length(species_column_16m1)

ggplot(data=Chart_16m1,mapping=aes(x=x_axis_16m1,y=No_of_species_16m1))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 16m")

#Getting Species for 52m
species_column_52m <- Eco_52m$scientific_name
useful_52m <- round_any(length(species_column_52m), 10, f = ceiling)
x_axis_52m <- seq(10,useful_52m,10)
template_vector_52m <- character(length(x_axis_52m))


for (i in seq(1,length(template_vector_52m))) {
  if (x_axis_52m[i]<length(species_column_52m)){
    y <-species_column_52m[1:x_axis_52m[i]]
    z <- length(unique(y))
    template_vector_52m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_52m
    z <- length(unique(y))
    template_vector_52m[length(template_vector_52m)]=z
    rm(y,i,z)
  }
}

No_of_species_52m<-as.integer(template_vector_52m)

Chart_52m <- data.frame(x_axis_52m,No_of_species_52m)

Chart_52m[nrow(Chart_52m),1]=length(species_column_52m)

ggplot(data=Chart_52m,mapping=aes(x=x_axis_52m,y=No_of_species_52m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 52m")


#Getting Species for 498m
species_column_498m <- Eco_498m$scientific_name
useful_498m <- round_any(length(species_column_498m), 10, f = ceiling)
x_axis_498m <- seq(10,useful_498m,10)
template_vector_498m <- character(length(x_axis_498m))


for (i in seq(1,length(template_vector_498m))) {
  if (x_axis_498m[i]<length(species_column_498m)){
    y <-species_column_498m[1:x_axis_498m[i]]
    z <- length(unique(y))
    template_vector_498m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_498m
    z <- length(unique(y))
    template_vector_498m[length(template_vector_498m)]=z
    rm(y,i,z)
  }
}

No_of_species_498m<-as.integer(template_vector_498m)

Chart_498m <- data.frame(x_axis_498m,No_of_species_498m)

Chart_498m[nrow(Chart_498m),1]=length(species_column_498m)

ggplot(data=Chart_498m,mapping=aes(x=x_axis_498m,y=No_of_species_498m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of familiies")+ggtitle("Family accumulation curve at 498m")


#Getting Species for 457m
species_column_457m <- Eco_457m$`Species name`
useful_457m <- round_any(length(species_column_457m), 10, f = ceiling)
x_axis_457m <- seq(10,useful_457m,10)
template_vector_457m <- character(length(x_axis_457m))


for (i in seq(1,length(template_vector_457m))) {
  if (x_axis_457m[i]<length(species_column_457m)){
    y <-species_column_457m[1:x_axis_457m[i]]
    z <- length(unique(y))
    template_vector_457m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_457m
    z <- length(unique(y))
    template_vector_457m[length(template_vector_457m)]=z
    rm(y,i,z)
  }
}

No_of_species_457m<-as.integer(template_vector_457m)

Chart_457m <- data.frame(x_axis_457m,No_of_species_457m)

Chart_457m[nrow(Chart_457m),1]=length(species_column_457m)

ggplot(data=Chart_457m,mapping=aes(x=x_axis_457m,y=No_of_species_457m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 457m")


#Getting Species for 131m
species_column_131m <- Eco_131m$taxon_family_name
useful_131m <- round_any(length(species_column_131m), 10, f = ceiling)
x_axis_131m <- seq(10,useful_131m,10)
template_vector_131m <- character(length(x_axis_131m))


for (i in seq(1,length(template_vector_131m))) {
  if (x_axis_131m[i]<length(species_column_131m)){
    y <-species_column_131m[1:x_axis_131m[i]]
    z <- length(unique(y))
    template_vector_131m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_131m
    z <- length(unique(y))
    template_vector_131m[length(template_vector_131m)]=z
    rm(y,i,z)
  }
}

No_of_species_131m<-as.integer(template_vector_131m)

Chart_131m <- data.frame(x_axis_131m,No_of_species_131m)

Chart_131m[nrow(Chart_131m),1]=length(species_column_131m)

ggplot(data=Chart_131m,mapping=aes(x=x_axis_131m,y=No_of_species_131m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Family accumulation curve at 131m")


#Getting Species for 38m
species_column_38m <- Eco_38m$scientific_name
useful_38m <- round_any(length(species_column_38m), 10, f = ceiling)
x_axis_38m <- seq(10,useful_38m,10)
template_vector_38m <- character(length(x_axis_38m))


for (i in seq(1,length(template_vector_38m))) {
  if (x_axis_38m[i]<length(species_column_38m)){
    y <-species_column_38m[1:x_axis_38m[i]]
    z <- length(unique(y))
    template_vector_38m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_38m
    z <- length(unique(y))
    template_vector_38m[length(template_vector_38m)]=z
    rm(y,i,z)
  }
}

No_of_species_38m<-as.integer(template_vector_38m)

Chart_38m <- data.frame(x_axis_38m,No_of_species_38m)

Chart_38m[nrow(Chart_38m),1]=length(species_column_38m)

ggplot(data=Chart_38m,mapping=aes(x=x_axis_38m,y=No_of_species_38m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 38m")

#Getting Species for 30m
species_column_30m <- Eco_30m$scientific_name
useful_30m <- round_any(length(species_column_30m), 10, f = ceiling)
x_axis_30m <- seq(10,useful_30m,10)
template_vector_30m <- character(length(x_axis_30m))


for (i in seq(1,length(template_vector_30m))) {
  if (x_axis_30m[i]<length(species_column_30m)){
    y <-species_column_30m[1:x_axis_30m[i]]
    z <- length(unique(y))
    template_vector_30m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_30m
    z <- length(unique(y))
    template_vector_30m[length(template_vector_30m)]=z
    rm(y,i,z)
  }
}

No_of_species_30m<-as.integer(template_vector_30m)

Chart_30m <- data.frame(x_axis_30m,No_of_species_30m)

Chart_30m[nrow(Chart_30m),1]=length(species_column_30m)

ggplot(data=Chart_30m,mapping=aes(x=x_axis_30m,y=No_of_species_30m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 30m")

#Getting Species for 21m
species_column_21m<- Eco_21m$scientific_name
useful_21m<- round_any(length(species_column_21m), 10, f = ceiling)
x_axis_21m<- seq(10,useful_21m,10)
template_vector_21m<- character(length(x_axis_21m))


for (i in seq(1,length(template_vector_21m))) {
  if (x_axis_21m[i]<length(species_column_21m)){
    y <-species_column_21m[1:x_axis_21m[i]]
    z <- length(unique(y))
    template_vector_21m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_21m
    z <- length(unique(y))
    template_vector_21m[length(template_vector_21m)]=z
    rm(y,i,z)
  }
}

No_of_species_21m<-as.integer(template_vector_21m)

Chart_21m<- data.frame(x_axis_21m,No_of_species_21m)

Chart_21m[nrow(Chart_21m),1]=length(species_column_21m)

ggplot(data=Chart_21m,mapping=aes(x=x_axis_21m,y=No_of_species_21m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 21m")

#Getting Species for 16m
species_column_16m <- Eco_16m_Clean$X.Calotropis.gigantea.
useful_16m <- round_any(length(species_column_16m), 10, f = ceiling)
x_axis_16m <- seq(10,useful_16m,10)
template_vector_16m <- character(length(x_axis_16m))


for (i in seq(1,length(template_vector_16m))) {
  if (x_axis_16m[i]<length(species_column_16m)){
    y <-species_column_16m[1:x_axis_16m[i]]
    z <- length(unique(y))
    template_vector_16m[i]=z
    rm(y,i,z)
  }
  else{
    y <-species_column_16m
    z <- length(unique(y))
    template_vector_16m[length(template_vector_16m)]=z
    rm(y,i,z)
  }
}

No_of_species_16m<-as.integer(template_vector_16m)

Chart_16m <- data.frame(x_axis_16m,No_of_species_16m)

Chart_16m[nrow(Chart_16m),1]=length(species_column_16m)

ggplot(data=Chart_16m,mapping=aes(x=x_axis_16m,y=No_of_species_16m))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of families")+ggtitle("Family accumulation curve at 16m")




