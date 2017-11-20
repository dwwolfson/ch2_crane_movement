# Get most complete data from movebank
library(move)

login<-movebankLogin(username="wolfs064", password="Poutinecart2!")

#download all data
# crane_data<-getMovebankData(study="Grus canadensis Minnesota", login=login)

#download a data range
yr17<-getMovebankData(study="Grus canadensis Minnesota",
                      login=login, timestamp_start="20170401000000000",
                      timestamp_end="20170831000000000")
#takes longer than just downloading it from the websit itself, and gave an error
# because there were NA values in coordinates....
