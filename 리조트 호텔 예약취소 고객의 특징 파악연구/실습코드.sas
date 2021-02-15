proc import datafile="C:\Users\moonf\Desktop\2020.2�б�\���Ⱥ���\ȸ��\encoding_resort_hotel.csv" dbms = csv replace
out=rhotel;
getnames = yes;
guessingrows=40000;
run;
/*column names*/
proc contents data=rhotel out=meta (keep=NAME) ; 
run ; 
proc print data=meta ; run ;
/*��� ������ �־��� ��*/
proc logistic  data=rhotel descending; class distribution_channel customer_type market_segment;
model is_canceled=adr adults arrival_date_day_of_month arrival_date_month arrival_date_week_number 
assigned_room_type babies booking_changes customer_type days_in_waiting_list distribution_channel 
is_repeated_guest lead_time market_segment meal previous_bookings_not_canceled previous_cancellations prt 
required_car_parking_spaces reserved_room_type stays_in_nights stays_in_week_nights 
stays_in_weekend_nights total_of_special_requests;
run;
/*stepwise*/
proc logistic  data=rhotel descending; class distribution_channel customer_type market_segment;
model is_canceled=adr adults arrival_date_day_of_month arrival_date_month arrival_date_week_number 
assigned_room_type babies booking_changes customer_type days_in_waiting_list distribution_channel 
is_repeated_guest lead_time market_segment meal previous_bookings_not_canceled previous_cancellations prt 
required_car_parking_spaces reserved_room_type stays_in_nights stays_in_week_nights 
stays_in_weekend_nights total_of_special_requests/selection=stepwise sls=0.15 sle=0.1;
run;
/*ù ����-���õ� ������*/
proc logistic  data=rhotel plots(maxpoints=none) descending; class customer_type market_segment;
model is_canceled=adr adults arrival_date_day_of_month arrival_date_month
assigned_room_type booking_changes customer_type
is_repeated_guest lead_time market_segment meal  previous_cancellations prt required_car_parking_spaces total_of_special_requests 
stays_in_nights /aggregate scale=none rsq  lackfit risklimits iplots;
run;quit;
/*������������ ù ���տ��� �������� ���� ��� �����Ѱ�*/
proc logistic  data=rhotel plots(maxpoints=none) descending; class customer_type market_segment;
model is_canceled=adr adults arrival_date_month 
assigned_room_type booking_changes
is_repeated_guest lead_time market_segment customer_type  previous_cancellations prt total_of_special_requests stays_in_nights/aggregate scale=none rsq  lackfit risklimits iplots;
output out=ex1  pred=prob reschi=pearson resdev=deviance;
run;quit;
/*�Ǿ ������ deviance ����*/
proc gplot data=ex1;
plot pearson*prob;
plot deviance*prob;
run;
/*���Ǽ��� ���� ���� �� ������ ��*/
proc logistic  data=rhotel plots(maxpoints=none) descending; class market_segment;
model is_canceled=adr
assigned_room_type booking_changes
is_repeated_guest lead_time market_segment previous_cancellations prt total_of_special_requests 
stays_in_nights /aggregate scale=none rsq  lackfit risklimits iplots;
output out=ex2  pred=prob reschi=pearson resdev=deviance;
run;quit;
/*�Ǿ ������ deviance ����*/
proc gplot data=ex2;
plot pearson*prob;
plot deviance*prob;
run;
/*�̻�ġ ����*/
data rhotel_final;
set ex1;
if deviance>2 then delete;
if deviance<-2 then delete;
if pearson>2 then delete;
if pearson<-2 then delete;
run;
/*�̻�ġ ������ ��¥ ���� ����*/
proc logistic  data=rhotel_final plots(maxpoints=none) descending; class customer_type market_segment;
model is_canceled=adr adults arrival_date_month 
assigned_room_type booking_changes
is_repeated_guest lead_time market_segment customer_type  previous_cancellations prt total_of_special_requests stays_in_nights/aggregate scale=none rsq  lackfit risklimits iplots;
output out=ex3  pred=prob reschi=pearson resdev=deviance;
run;quit;
proc gplot data=ex3;
plot pearson*prob;
plot deviance*prob;
run;

