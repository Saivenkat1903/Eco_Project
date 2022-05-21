#Getting Species names For Meenaksi
species_column_Meen <- Eco_Meen$`Species name`
Eco_Meen_Clean <-data.frame()

#Cleaning Meenakshi Data
for(k in seq(1,length(species_column_Meen))){
  for (i in seq(1,as.integer(Eco_Meen[k,4]))){
    Eco_Meen_Clean <- rbind(unname(Eco_Meen_Clean),c(k,Eco_Meen[k,3],k))
    rm(i)
    }
}
#more Crap I need
species_column_Meen_Clean <- Eco_Meen_Clean$`species name`
useful_Meen <- round_any(length(species_column_Meen_Clean), 10, f = ceiling)
x_axis_Meen <- seq(10,useful_Meen,10)
template_vector_Meen <- character(length(x_axis_Meen))


#Making Chart for Madhu
for (i in seq(1,length(template_vector_Meen))) {
  if (x_axis_Meen[i]<length(species_column_Meen_Clean)){
    y <-species_column_Meen_Clean[1:x_axis_Meen[i]]
    z <- length(unique(y))
    template_vector_Meen[i]=z
    rm(y,z,i)
  }
  else{
    y <-species_column_Meen_Clean
    z <- length(unique(y))
    template_vector_Meen[length(template_vector_Meen)]=z
    rm(y,z,i)
  }
}

No_of_species_Meen<-as.integer(template_vector_Meen)

Chart_Meen <- data.frame(x_axis_Meen,No_of_species_Meen)
Chart_Meen[nrow(Chart_Meen),1]=length(species_column_Meen_Clean)

ggplot(data=Chart_Meen,mapping=aes(x=x_axis_Meen,y=No_of_species_Meen))+geom_point()+xlim(0,170)+ylim(0,80)+geom_smooth()


#Getting Species names For Rajagopal
species_column_Raja <- Eco_Raja$`Taxon family Name`
Eco_Raja_Clean <-data.frame()

#Cleaning Rajagopal Data
for(k in seq(1,length(species_column_Raja))){
  for (i in seq(1,as.integer(Eco_Raja[k,4]))){
    Eco_Raja_Clean <- rbind(unname(Eco_Raja_Clean),c(k,Eco_Raja[k,2],k))
    rm(i)
  }
}

#Getting Species names For Mukesh
species_column_Mukesh <- Eco_Mukesh$taxon_family_name
Eco_Mukesh_Clean <-data.frame()

#Cleaning Mukesh Data
for(k in seq(1,length(species_column_Mukesh))){
  for (i in seq(1,as.integer(Eco_Mukesh[k,4]))){
    Eco_Mukesh_Clean <- rbind(Eco_Mukesh_Clean,c(k,Eco_Mukesh[k,2],k))
    rm(i)
  }
}
rm(k)


#Getting Species names For Shashank
species_column_Shashank <- Eco_Shashank$scientific_name
Eco_Shashank_Clean <-data.frame()


#Cleaning Shashank Data
for(k in seq(1,length(species_column_Shashank))){
  for (i in seq(1,as.integer(Eco_Shashank[k,5]))){
    Eco_Shashank_Clean <- rbind(Eco_Shashank_Clean,c(k,Eco_Shashank[k,2],k))
    rm(i)
  }
}
rm(k)



