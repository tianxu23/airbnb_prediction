setwd("C:/Users/31221/Desktop/Kaggle/airbnb")
library(dplyr)
library(sqldf)
install.packages("ggplot2")
library(ggplot2)
library(sqldf)
age_gender_bkts = read.csv("age_gender_bkts.csv")
countries = read.csv("countries.csv")
sample_submission_NDF = read.csv("sample_submission_NDF.csv")
sessions = read.csv("sessions.csv")
test_users = read.csv("test_users.csv")
train_users_2 = read.csv("train_users_2.csv")
str(age_gender_bkts)
str(countries)
str(sample_submission_NDF)
names(age_gender_bkts)
names(sessions)
names(countries)
names(train_users_2)
names(test_users)
sessions$action
summary(train_users_2)
summary(countries)
head(countries)
head(train_users_2)
head(sessions,100)
tail(sessions,100)
summary(test_users)
head(test_users)
summary(test_users)
##start to explore
#understand the countries
head(countries)
summary(countries)
nrow(countries)
countries$country_destination
#understand the train_users_2
head(train_users_2)
nrow(train_users_2)
ncol(train_users_2)
names(train_users_2)
train_users_2%>%
  ggplot(aes(x=gender))+
  geom_bar()
train_users_2%>%
  ggplot(aes(x=age))+
  geom_histogram()
train_users_2%>%
  filter(age<130&age>0)%>%
  ggplot(aes(x=age))+
  geom_histogram()
train_users_2%>%
  ggplot(aes(x=language))+
  geom_bar()
train_users_2%>%
  filter(language!='en')%>%
  ggplot(aes(x=language))+
  geom_bar()
train_users_2%>%
  ggplot(aes(x=signup_method))+
  geom_bar()
train_users_2%>%
  ggplot(aes(x=signup_flow))+
  geom_bar()
train_users_2%>%
  ggplot(aes(x=signup_app))+
  geom_bar()
train_users_2%>%
  ggplot(aes(x=country_destination))+
  geom_bar()

###sessions
names(sessions)
sessions%>%
  group_by(action_type)%>%
  summarise(
    n()
  )
sessions%>%
  group_by(action_detail)%>%
  summarise(
    n()
  )
sessions%>%
  group_by(action)%>%
  summarise(
    n()
  )

##age_gender_bks
names(age_gender_bkts)
head(age_gender_bkts)
age_gender_bkts%>%
  ggplot(aes(x=age_bucket,fill = gender))+
  geom_bar()
tail(age_gender_bkts$year,5)
plot(x = age_gender_bkts$age_bucket,y =age_gender_bkts$population_in_thousands,type="h")

#data join
names(sessions)
sql01 = "select user_id,
                sum(secs_elapsed) as tot_secs,
                avg(secs_elapsed) as avg_secs,
                count() as cot_secs
        from
        sessions
        group by user_id;"
table01 = sqldf(sql01)
head(table01)
nrow(table01)
sql02 = "select user_id,active_type,
                sum(secs_elapsed) as tot_secs,
                avg(secs_elapsed) as avg_secs,
                count() as cot_secs
        from
        sessions
        group by user_id,action_type;"
table02 = sqldf(sql02)
head(table02)

sql_id_and_action = "
        select user_id as id,
               action as action,
               count() as action_num,
               sum(secs_elapsed) as spend_secs,
               sum(secs_elapsed)/count() as mean_secs
        from   sessions
        group by user_id, action;"
id_and_action = sqldf(sql_id_and_action)
tail(id_and_action)

sql_action = "
        select action as action,
               count() as action_num
        from   sessions
        group by action
        order by action_num desc;
"
action = sqldf(sql_action)
action

sql_action02 = "
        select *from action
        where action_num>10000 and 
              action != '';
"
action02 = sqldf(sql_action02)

library(ggplot2)
?geom_bar()
action02

sql_user = "
        select user_id as id,
        count() as id_num
        from sessions
        group by user_id
        order by id;
"
user = sqldf(sql_user)
user

sql_total_num = "
        select count() from user;
"
sqldf(sql_total_num)
names(id_and_action)
names(action)
sql_id_and_action03 = "
       select a.id,a.action,a.action_num,a.mean_secs
       from id_and_action a
       inner join
       action02 b
       on a.action = b.action
       where a.id!='';"
id_and_action03 = sqldf(sql_id_and_action03)
head(id_and_action03,100)

action_num = read.csv("id_and_action04.csv")
action_sec_mean = read.csv("id_and_action03.csv")

head(action_num)
head(action_sec_mean)
training_set01 = left_join(train_users_2,action_num,by="id")
names(training_set01)
training_set02 = left_join(training_set01,action_sec_mean,by="id")
names(training_set02)

write.table (id_and_action03, file ="C:\\Users\\31221\\Desktop\\Kaggle\\airbnb\\id_and_action03.csv", sep =",", row.names =TRUE)

write.table (training_set02, file ="C:\\Users\\31221\\Desktop\\Kaggle\\airbnb\\training_set_with_actions.csv", sep =",", row.names =TRUE)




id_and_action = sqldf(sql_id_and_action)
tail(id_and_action)





names(train_users_2)
str(train_users_2$date_account_created)
train_users_2$date_account_created = as.Date(train_users_2$date_account_created)
weekdays(train_users_2$date_account_created)
month(train_users_2$date_account_created)

names(sessions)

clean_train_final = read.csv("newdata2.csv")
names(clean_train_final)
clean_train_final$timestamp_first_active

clean_train_final%>%
  filter(age<200)%>%
  ggplot(aes(x = country_destination,y = age))+
  geom_boxplot()


clean_train_final$data_account_week = week(clean_train_final$date_account_created)
clean_train_final$data_account_month = month(clean_train_final$date_account_created)
clean_train_final$data_account_weekday = weekdays(clean_train_final$date_account_created)

unique(clean_train_final$data_account_week) 
unique(clean_train_final$data_account_month)
clean_train_final = mutate(clean_train_final,first_book_duration = as.Date(date_first_booking)-as.Date(date_account_created))
unique(clean_train_final$first_book_duration)
clean_train_final%>%
  ggplot(aes(x=country_destination, y = (first_book_duration)))+
  geom_boxplot()
sql01 = "
        select first_book_duration 
        
        from clean_train_final
"


#kmeans
cou_use = countries[,2:ncol(countries)]
cou_use$
summary(cou_use)
model.kmeans = kmeans(cou_use,centers = 4,nstart = 20)

####
newdata2 = read.csv("newdata2.csv")
names(newdata2)
sql_age = "
    select *,
    case when age>10 and age <130 then 0
    else 1
    end as age_abnormal
    from newdata2;
"
.
library(sqldf)
newdata3 = sqldf(sql_age)
newdata3$age_abnormal
names(newdata3)
str(newdata3$date_account_created)
newdata3$date_account_created = as.Date(newdata3$date_account_created)
newdata3$date_first_booking = as.Date(newdata3$date_first_booking)

?
week(newdata3$date_account_created)
newdata3$date_account_created_month = months(newdata3$date_account_created)
newdata3$date_account_created_weekday = weekdays(newdata3$date_account_created)
newdata3$date_account_created_quarter = quarters(newdata3$date_account_created)
newdata3$date_diff = newdata3$date_first_booking-newdata3$date_account_created
newdata3