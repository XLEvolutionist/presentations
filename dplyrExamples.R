library(nycflights13)
library(ggplot2)
library(dplyr)

dim(flights)
head(flights)

#filter flights by miniute of departure
filter(flights, dep_time == 609)

#find all the unique carriers
distinct(select(flights, carrier))

#or filter by carrier "FL"
filter(flights, carrier == "FL")

#select dep_time, carrier only where the origin is at LGA
filter(select(flights, dep_time, carrier, origin), origin == "LGA")
#but it also works the other way
select(filter(flights, origin == "LGA" ),dep_time, carrier, origin)

#now only those going to Orlando
filter(select(flights, dep_time, carrier, origin, dest), origin == "LGA", dest=="ORD")

#now find all the carriers that do that route
distinct(select(filter(flights, origin == "LGA", dest=="ORD"), carrier))

#now what about adding a new column
mutate(flights,loss=arr_delay-dep_delay)

#add another column at the same time, but operating on the new columnm
#now what about adding a new column
mutate(flights,loss=arr_delay-dep_delay, speed=(distance/air_time)*60)

#just keep the new columns..
transmute(flights,loss=arr_delay-dep_delay, speed=(distance/air_time)*60)

#can dplyr handle matrices
mat<-matrix(nrow=10, runif(1:100))
colnames(mat)<-c("A","B","C","D","E","F","G","H","I","J")
filter(mat, A >= 0.1)
#does not work, turn into data.frame
mat.df<-data.frame(mat)
filter(mat.df, A >= 0.1, B >=0.1, C >= 0.1)
#it now works!

#now find all the carriers that do that route, but with the departure times, sorted by carrier
delays<-arrange(distinct(select(filter(flights, origin == "LGA", dest=="ORD"), carrier, dep_time, arr_delay)),carrier)
delays<-data.frame(delays)
#what about the distribution of delay times
ggplot(data=delays, aes(x=arr_delay, fill=carrier)) +
  geom_histogram(alpha=0.8)

ggplot(sample_frac(flights,0.01), aes(distance,arr_delay)) + 
  geom_point() +
    geom_smooth() +
      scale_size_area()

#try looking at delays per carrier

planes <- group_by(flights, dest)
delay <- summarise(planes,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(dep_delay, na.rm = TRUE))
delay <- filter(delay, count > 5000, dist < 2000)

ggplot(delay, aes(dist, delay, col = as.factor(dest))) +
  geom_point(aes(size = count, col=dest)) 
