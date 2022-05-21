#Getting Species names For 884m
species_column_884m <- Eco_884m$`Species name`
Eco_884m_Clean <-data.frame()

#Cleaning 884m Data
for(k in seq(1,length(species_column_884m))){
  for (i in seq(1,as.integer(Eco_884m[k,4]))){
    Eco_884m_Clean <- rbind(unname(Eco_884m_Clean),c(k,Eco_884m[k,3],k))
    rm(i)
    }
}

species_column_884m_Clean <- Eco_884m_Clean$`species name`
useful_884m <- round_any(length(species_column_884m_Clean), 10, f = ceiling)
x_axis_884m <- seq(10,useful_884m,10)
template_vector_884m <- character(length(x_axis_884m))


#Making Chart for 884m
for (i in seq(1,length(template_vector_884m))) {
  if (x_axis_884m[i]<length(species_column_884m_Clean)){
    y <-species_column_884m_Clean[1:x_axis_884m[i]]
    z <- length(unique(y))
    template_vector_884m[i]=z
    rm(y,z,i)
  }
  else{
    y <-species_column_884m_Clean
    z <- length(unique(y))
    template_vector_884m[length(template_vector_884m)]=z
    rm(y,z,i)
  }
}

No_of_species_884m<-as.integer(template_vector_884m)

Chart_884m <- data.frame(x_axis_884m,No_of_species_884m)
Chart_884m[nrow(Chart_884m),1]=length(species_column_884m_Clean)

ggplot(data=Chart_884m,mapping=aes(x=x_axis_884m,y=No_of_species_884m))+geom_point()+xlim(0,170)+ylim(0,80)+geom_smooth()


#Getting Species names For 131m
species_column_131m <- Eco_131m$`Taxon family Name`
Eco_131m_Clean <-data.frame()

#Cleaning 131m Data
for(k in seq(1,length(species_column_131m))){
  for (i in seq(1,as.integer(Eco_131m[k,4]))){
    Eco_131m_Clean <- rbind(unname(Eco_131m_Clean),c(k,Eco_131m[k,2],k))
    rm(i)
  }
}

#Getting Species names For 16m
species_column_16m <- Eco_16m$taxon_family_name
Eco_16m_Clean <-data.frame()

#Cleaning 16m Data
for(k in seq(1,length(species_column_16m))){
  for (i in seq(1,as.integer(Eco_16m[k,4]))){
    Eco_16m_Clean <- rbind(Eco_16m_Clean,c(k,Eco_16m[k,2],k))
    rm(i)
  }
}
rm(k)


#Getting Species names For 16m1
species_column_16m1 <- Eco_16m1$scientific_name
Eco_16m1_Clean <-data.frame()


#Cleaning 16m1 Data
for(k in seq(1,length(species_column_16m1))){
  for (i in seq(1,as.integer(Eco_16m1[k,5]))){
    Eco_16m1_Clean <- rbind(Eco_16m1_Clean,c(k,Eco_16m1[k,2],k))
    rm(i)
  }
}
rm(k)



