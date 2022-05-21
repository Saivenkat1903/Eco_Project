#helps to find similiar species and their smallest frequencies
similar_1 <- semi_join(Species_below_family,Species_above_family, by="Family")
similar_2 <- semi_join(Species_above_family,Species_below_family, by="Family")
names(similar_2) <- names(similar_1)

blank <- data.frame()             
                                 
for (i in similar_1$Family){
  if (similar_1[similar_1$Family==i,][1,3] < similar_2[similar_2$Family==i,][1,3]){
    blank <- rbind(blank,similar_1[similar_1$Family==i,])
    rm(i)
  } else{
    blank <- rbind(blank,similar_2[similar_2$Family==i,])
    rm(i)
  }
}         

#randomization
random1 <-read_excel("Eco_Project\\Processed_data\\Total_Observations_Below(Families).xlsx")
random1 <-read_excel("Eco_Project\\Processed_data\\Total_Observations_Above(Families).xlsx")

set.seed(42)

rows <- sample(nrow(random1))

random1 <-random1[rows,]

useful_above <- round_any(length(random1$Family), 10, f = ceiling)
x_axis_above <- seq(10,useful_above,10)
template_vector_above <- character(length(x_axis_above))


for (i in seq(1,length(template_vector_above))) {
  if (x_axis_above[i]<length(random1$Family)){
    y <-random1$Family[1:x_axis_above[i]]
    z <- length(unique(y))
    template_vector_above[i]=z
    rm(z,y,i)
  }
  else{
    y <-random1$Family
    z <- length(unique(y))
    template_vector_above[length(template_vector_above)]=z
    rm(z,y,i)
  }
}

No_of_species_above<-as.integer(template_vector_above)

Chart_above <- data.frame(x_axis_above,No_of_species_above)
Chart_above[nrow(Chart_above),1]=length(random1$Family)


ggplot(data=Chart_above,mapping=aes(x=x_axis_above,y=No_of_species_above))+geom_point()+geom_smooth()+xlab("No. of observations")+ylab("No. of Families")+ggtitle("Randomized Families accumulation above 100m")


write.xlsx(Chart_above,"Eco_Project\\Processed_data\\Randomized_Family_Accumulation_Above.xlsx")




#Rank abundance curves
data <- read_excel("Eco_Project\\Processed_data\\Species_Freq_Above.xlsx")

data <- data[order(data$Freq,decreasing=TRUE),]

ggplot(data=data,mapping=aes(x=seq(1,nrow(data)),y=`n/N`))+geom_point()+geom_smooth()+xlab("rank")+ylab("Relative Abundance")+ggtitle("Rank abundance curve for Species above 100m")
ggplot(data=data[1:10,],mapping=aes(x=seq(1,10),y=`n/N`))+geom_point()+geom_smooth()+xlab("rank")+ylab("Relative Abundance")+ggtitle("Rank abundance curve for top Species above 100m")

rm(list=ls())


#species index graph
alt <-c(14,16,16,21,30,37,38,52,131,240,457,498,611,884)

shanon_species <-c(3.866584,2.476979,2.846023,3.479366,4.563172,3.031591,4.513214,4.16316,2.165883,3.854903,4.33831,4.881248,4.264301,2.848029)
simpson_species <-c(0.9698192,0.8940833,0.9136461,0.9493594,0.988203,0.9472,0.9836408,0.9800889,0.6813376,0.9747475,0.9830332,0.9920889,0.9826467,0.8993778)
shanon_species_ENS <- exp(shanon_species)


shanon_family <-c(3.019625,2.001441,2.323187,2.059972,3.500476,2.844305,3.384811,3.077082,1.945216,2.847841,3.279573,3.687408,3.048608,2.425589)
simpson_family <-c(0.9316878,0.8473338,0.8791045,0.7966586,0.962085,0.9344,0.9519673,0.9419556,0.6758556,0.9205693,0.945464,0.9632,0.9230924,0.8532444)
shanon_family_ENS <- exp(shanon_family)

main <-data.frame(alt,shanon_species_ENS,shanon_family_ENS)

colors <-c("Shanon Diversity Index" = "green", "Simpson Diversity Index" = "blue")

ggplot(data=main,mapping=aes(x=alt,y=shanon_species_ENS))+geom_line()+geom_point()+xlab("Altitude")+ylab("Effective Number of Species")+ggtitle("Variation of ENS with altitude")
ggplot(data=main,mapping=aes(x=alt,y=shanon_family_ENS))+geom_line()+geom_point()+xlab("Altitude")+ylab("Effective Number of Families")+ggtitle("Variation of ENF with altitude")


ggplot()+geom_line(data = main, aes(x = alt, y = shanon_species, colour = "Shanon Diversity Index"))+
  geom_line(data = main, aes(x = alt, y = simpson_species, colour = "Simpson Diversity Index"))+
  geom_point(data=main,mapping=aes(x=alt,y=simpson_species))+
  geom_point(data=main,mapping=aes(x=alt,y=shanon_species))+
  ggtitle("Variation of Species Indexes with Altitude")+xlab("Altitude")+ylab("Value of Index")+
  scale_colour_manual(name = "Legend",values = c( "Shanon Diversity Index" = "Green", "Simpson Diversity Index" = "Blue"))+
  theme(legend.position="bottom")
                                                                                                                                                                                                   
ggplot()+geom_line(data = main, aes(x = alt, y = shanon_family, colour = "Shanon Diversity Index"))+
  geom_line(data = main, aes(x = alt, y = simpson_family, colour = "Simpson Diversity Index"))+
  geom_point(data=main,mapping=aes(x=alt,y=simpson_family))+
  geom_point(data=main,mapping=aes(x=alt,y=shanon_family))+
  ggtitle("Variation of Family Indexes with Altitude")+xlab("Altitude")+ylab("Value of Index")+
  scale_colour_manual(name = "Legend",values = c( "Shanon Diversity Index" = "Green", "Simpson Diversity Index" = "Blue"))+
  theme(legend.position="bottom")

#Rank abundance for all
data1 <-read_excel("Eco_Project\\Processed_data\\Total_Observations_Above(Families).xlsx")
data2 <-read_excel("Eco_Project\\Processed_data\\Total_Observations_Below(Families).xlsx")

main <- rbind(data1,data2)

main2 <-data.frame(table(main$Family))

total <- 1982

for (i in seq(1,100)) {
  main2[i,3] <- main2[i,2]/total
}

main2 <- main2[order(main2$Freq,decreasing=TRUE),]


ggplot(data=main2,mapping=aes(x=seq(1,100),y=V3))+geom_point()+geom_smooth()+xlab("rank")+ylab("Relative Abundance")+ggtitle("Rank abundance curve for Families")
















