
setwd("C:\\Users\\moonf\\Desktop\\2020.2학기\\제안보고서\\회귀")
hotel_t<-read.csv("encoding_resort_hotel.csv")


hotel_t=hotel_t[c('is_canceled','adr','adults','arrival_date_month','assigned_room_type',
                         'booking_changes','is_repeated_guest','lead_time',
                         'market_segment','customer_type','previous_cancellations','prt','total_of_special_requests',"stays_in_nights")]



hotel_t=transform(hotel_t, 
          market_segment_direct=ifelse(market_segment=="Direct",1,0),
          market_segment_corporate=ifelse(market_segment=="Corporate",1,0),
          market_segment_onlineTA=ifelse(market_segment=="Online TA",1,0),
          market_segment_offlineTATO=ifelse(market_segment=="Offline TA/TO",1,0),
          market_segment_complementary=ifelse(market_segment=="Complementary",1,0),
          market_segment_groups=ifelse(market_segment=="Groups",1,0)
          
          )

hotel_t=transform(hotel_t,
          customer_type_transient=ifelse(customer_type=="Transient",1,0),
          customer_type_contract=ifelse(customer_type=="Contract",1,0),
          customer_type_transientparty=ifelse(customer_type=="Transient-Party",1,0),
          customer_type_group=ifelse(customer_type=="Group",1,0)
          )

hotel_t=subset(hotel_t,select=-c(market_segment,customer_type))
hotel_t=as.data.frame(as.matrix(hotel_t))

#rpart
library(rpart) #rpart()함수 포함 패키지
library(rpart.plot) #rpart.plot()함수 포함패키지
r_tree <- rpart(is_canceled~., data=hotel_t,method = "class")
rpart.plot(r_tree)
r_tree$variable.importance


val_var <- predict(r_tree, newdata=hotel_t, type="class") 

sum(val_var == hotel_t$is_canceled)/nrow(hotel_t) * 100



#ctree
library(party)
innerWeights <- function(node){
  grid.circle(gp = gpar(fill = "White", col = 1))
  mainlab <- paste( node$psplit$variableName, "\n(n = ")
  mainlab <- paste(mainlab, sum(node$weights),")" , sep = "")
  grid.text(mainlab,gp = gpar(col='red'))
}

a=ctree_control(maxdepth=4)
c_tree=ctree(is_canceled~., data=hotel_t,control=a)
plot(c_tree)
c_tree
exp(-1.2441)

val_var <- predict(c_tree, newdata=hotel_t) 
val_var[val_var>0.5]=1
val_var[val_var<=0.5]=0
sum(val_var == hotel_t$is_canceled)/nrow(hotel_t) * 100


#가지치기 별도로 필요없음

#################의심되는 변수 시각화 해보기

#arrival_date_year
library(ggplot2)
library(dplyr)

#stays_in_nights
#arrival_date_year 표본기간 때문에 수가 차이가 남.없애는 게 좋음.
hotel_t %>% group_by(arrival_date_year) %>% summarize(sum_cancel=sum(is_canceled))

u=hotel_t %>% group_by(stays_in_nights) %>% summarize(sum_cancel=sum(is_canceled))
plot(as.data.frame(u),type='l')

hotel_t %>% group_by(arrival_date_month) %>% summarize(sum_cancel=sum(is_canceled))

## deposit type

hotel_t %>% group_by(deposit_type) %>% summarize(cancel=sum(is_canceled))

hotel_t_d=hotel_t %>% group_by(lead_time, deposit_type)  %>% summarize(cancel=sum(is_canceled))
hotel_t_d=as.data.frame(hotel_t_d)
ggplot(hotel_t_d) + geom_line(aex(x=)) 


glm(is_canceled~adr+assigned_room_type+ booking_changes+
      children+deposit_type +is_repeated_guest +lead_time+market_segment_direct+market_segment_corporate+
      market_segment_onlineTA+market_segment_offlineTATO+market_segment_complementary+
      previous_cancellations +prt+total_of_special_requests +stays_in_nights,data=hotel_t, family="binomial")
colnames(hotel_t)

a=lm(is_canceled~(hotel_t$deposit_type+1), data=hotel_t)
hotel_t_d=hotel_t %>% group_by(deposit_type) %>% summarize(cancel=sum(is_canceled))
hotel_t_d=as.data.frame(hotel_t_d)
plot(hotel_t_d$deposit_type+1, hotel_t_d$cancel)
plot(hotel_t$is_canceled~hotel_t$deposit_type+1)
abline(a)



## adr

hotel_t_d=hotel_t %>% group_by(is_canceled, children) %>% summarize(n=n())
hotel_t_d=as.data.frame(hotel_t_d)
ggplot(hotel_t_d) + geom_dotplot(aes(x=adr, y=cancel)) 
plot(x=hotel_t_d$adr, y=hotel_t_d$cancel)
plot(x=hotel_t_d$children, y=hotel_t_d$cancel)
colnames(hotel_t_d)





