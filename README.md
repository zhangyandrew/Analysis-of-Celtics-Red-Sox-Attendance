# MA615-Midterm-Project

#####
- Albert Ding
- Andrew Zhang
- Ningze Zu
- Miller

This project revolved around studying the effects of weather on the attendance of Celtics and Red-Sox home games. While the weather and Red Sox data was readily available, the Celtics data required a signficant amount of webscraping. Rather than webscrape URLs for 5 seasons(which totalled around 205 URLs), we only utilized 5 URLs, allowing the program to scrape the URLs from these 5 URLs containing all the links to the boxscores for the 2011-2016 seasons, and then accessing the URLs and scraping the attendance of each game. All of this was then put into a shiny application.

## Methods
- Webscraping(with rvest)
- Data Aggregation
- Exploratory Data Analysis
- Shiny Application

## Conclusion
Weather played a signficant role in the attendance of Red-Sox games because Fenway Park is an open field. This did not play as much of a role in the attendance of Celtic's games as TD-Garden houses all players and fans. However, we can see drops in attendance and attribute them to a couple of different reasons. Boston, being a historic sports city, houses many great sports teams. If we look at how well other teams are doing in conjunction to the Celtics or the Red-Sox, we might be able to draw conclusions regarding the attendance. For example, if the Patriots are doing really well, we could potentially see a dip in attendance at Celtics games if their record isn't great. These are things that require further analysis to accurately provide a conclusion.

## Link
[Link to the Shiny Application](https://yaphets0128.shinyapps.io/ma615-midterm-project-master/)
