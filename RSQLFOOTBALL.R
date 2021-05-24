


install.packages("RSQLite")
install.packages("tidyverse")


#check your working directory to make sure R will connect to the specific database.
library(DBI)
library(dplyr)


setwd("C:/Users/ÅÕÑÉÐÉÄÇÓ/Desktop")
con<-dbConnect(RSQLite::SQLite(),"database.sqlite")


con


dbReadTable(con,"Country")
dbReadTable(con,"Match")
dbReadTable(con,"League")
dbReadTable(con,"Player")
dbReadTable(con,"Player_Attributes")
dbReadTable(con,"Team")
dbReadTable(con,"Team_Attributes")


str(dbReadTable(con,"Country"))
str(dbReadTable(con,"Match"))
str(dbReadTable(con,"League"))
str(dbReadTable(con,"Player"))
str(dbReadTable(con,"Player_Attributes"))
str(dbReadTable(con,"Team"))
str(dbReadTable(con,"Team_Attributes"))




country<-tbl(con,"Country")
country

player_attributes<-tbl(con,"Player_Attributes")
player<-tbl(con,"Player")



player2<-data.frame(player)

match<-tbl(con,"Match")

league<-tbl(con,"League")
team_attributes<-tbl(con,"Team_attributes")


player<-tbl(con,"Player")
team_attributes<-tbl(con,"Player")

#??????? 4



glimpse(player)

x1<-player_attributes%>%
  mutate(id=1)
x1


number_of_preferred_foot<-x1%>%
  group_by(preferred_foot)%>%
  summarise(sum(id))

number_of_preferred_foot




install.packages("ggplot2")
library(ggplot2)


#barplot for favorite foot. NA meaning missing value
ggplot(player_attributes,aes(x=preferred_foot))+geom_bar(width=0.5)

#height vs weight scatter plot
ggplot(player,aes(x=weight,y=height))+geom_point()

#height vs weight again but with geom_jitter to avoid over plotting 
ggplot(player,aes(x=weight,y=height))+geom_jitter(alpha=0.1)




#Height and weight of players with the use of dplyr
glimpse(player)



player_height_limit<-player%>%
  select(player_name,height,weight)%>%
  filter(height>=160 & height<=180)

player_height_limit

player_height_limit<-player_height_limit%>%
  arrange(desc(weight))
player_height_limit


#Select a random match, manipulate data and create a table with the home team, the date of the match and the goals of the home team
#Doing that manually without JOIN commands

#manipulating initial data
the_data_needed<- match%>%
   select(match_api_id,date,home_team_goal,home_team_api_id)

the_data_needed<-as.data.frame(the_data_needed)

the_data_needed
str(the_data_needed)


#Selecting randomly a home team api id
y<-sample_n(the_data_needed[1],1)

class(y)
y<-as.vector(y)
y<-as.numeric(y)
class(y)

#for this random id I find the respective home team goal and season
the_random_number_match_api<-the_data_needed%>%
  filter(match_api_id==y)

#finding the number of goals 

the_random_number_for_season_team_goal<-the_random_number_match_api%>%
  select(date,home_team_goal)


#Taking the id of the previous dataset and finding it on the team dataset
x<-the_random_number_match_api[4]
x

x<-as.numeric(x)
class(x)


team<-tbl(con,"Team")

team<-as.data.frame(team)


name_using_the_random_number<-team%>%
  filter(team_api_id==x)%>%
  select(team_long_name)

name_using_the_random_number


#Result
#Making the dataframe that contains the name of the home team, the date and the number of home team goals  
new_dataframest<-c(name_using_the_random_number,the_random_number_for_season_team_goal)
new_dataframest



###########################################################################


#Now again for a random id of match find the data of the match the home team name,away team name,
#the date, the number of goals of the home team and the number of goals for the away team


#Same methodology as before
glimpse(match)
the_data_needed_2<- match%>%
  select(match_api_id,date,home_team_goal,home_team_api_id,away_team_api_id,away_team_goal)

the_data_needed_2<-as.data.frame(the_data_needed_2)

the_data_needed_2
str(the_data_needed_2)

y2<-sample_n(the_data_needed_2[1],1)
y2

class(y2)
y2<-as.vector(y2)
y2<-as.numeric(y2)
class(y2)


the_random_number_for_match_api_id<-the_data_needed_2%>%
  filter(match_api_id==y2)

the_random_number_for_season_homegoal_awaygoal<-the_random_number_for_match_api_id%>%
  select(date,home_team_goal,away_team_goal)


x<-the_random_number_for_match_api_id[4]
x
y<-the_random_number_for_match_api_id[5]
y
x<-as.numeric(x)
class(x)
team<-tbl(con,"Team")

y<-as.numeric(y)
class(y)

team<-as.data.frame(team)

name_of_home_team<-team%>%
  filter(team_api_id==x)%>%
  select(team_long_name)




name_of_away_team<-team%>%
  filter(team_api_id==y)%>%
  select(team_long_name)

name_of_away_team

#Result
the_complete_matrix<-data.frame(name_of_home_team,name_of_away_team,the_random_number_for_season_homegoal_awaygoal)
colnames(the_complete_matrix)<-c("team_name_home","team_name_away","Season","home_team_goal","away_team_goal")
the_complete_matrix






#find the mean number of goals each team scored and their name

match_points <- match %>%   
  mutate(home_team_points = if_else((home_team_goal > away_team_goal), 3, if_else((home_team_goal == away_team_goal), 1, 0))) %>% 
  mutate(away_team_points = if_else((home_team_goal > away_team_goal), 0, if_else((home_team_goal == away_team_goal), 1, 3))) 


glimpse(match_points)


home_points<-match_points%>%
  select(league_id,home_team_api_id,home_team_points)%>%
  group_by(home_team_api_id,league_id)%>%
  summarize(mean_home_score=mean(home_team_points))%>%
  arrange(desc(home_team_api_id))

home_points

head(team)


home_points<-inner_join(team,home_points,by=c("team_api_id"="home_team_api_id"),copy=TRUE)

home_points<-data.frame(home_points)
str(home_points)


#Result
home_points<-home_points%>%
  select(team_api_id,league_id,mean_home_score,team_long_name)


#Do the same but for the away score
away_points<-match_points%>%
  group_by(away_team_api_id,league_id)%>%
  summarize(mean_score_away=mean(away_team_points))%>%
  arrange(desc(away_team_api_id))

away_points

away_points<-left_join(team,away_points,by=c("team_api_id"="away_team_api_id"),copy=TRUE)
away_points<-away_points%>%
  select(team_api_id,league_id,mean_score_away)

#Result
away_points


#Now unite these two tables 
new_completed<-left_join(home_points,away_points,copy=TRUE)


new_completed

#Is there a linear relationship between mean home score and mean away score?
 
ggplot(new_completed,aes(x=mean_home_score,y=mean_score_away))+geom_point()+geom_smooth(method=lm)

#plotly is better. Interactive plot making each data point show the respective team.

library(plotly)

fit <- lm(mean_score_away ~ mean_home_score, data = new_completed)


new_completed%>%
  plot_ly(x=~mean_home_score,y=~mean_score_away,text = ~paste(" Team: ",team_long_name))%>%
  add_markers(colors=c('red'),size=3,opacity=0.6)%>%
  add_lines(x = ~mean_home_score, y =fitted(fit))%>%
  layout(xaxis=list(title="Mean Home Score"),yaxis=list(title="Mean Away Score"),showlegend=FALSE)
  



