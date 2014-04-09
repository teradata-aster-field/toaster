# Demo compute percentile and create box plot
#
# To install baseball demo dataset in Aster
# download baseball.zip from
# https://bitbucket.org/grigory/toaster/downloads/baseball.zip
# and run
# sh load_baseball_data.sh -d mydbname -U beehive 

library(toaster)

# update ODBC data source name
dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

# Single boxplot: BA of all players
allBAPerc = computePercentiles(conn, "batting_enh", "ba")
createBoxplot(allBAPerc, fill=NULL, title="BA Boxplot")

# Team BA in 2000s 
teamBA2000sPerc = computePercentiles(conn, "batting_enh", "ba",
                                     by="teamid", where="yearid >= 2000")
createBoxplot(teamBA2000sPerc, x="teamid", fill="teamid", useIQR=TRUE,
              title="Team BA in 2000s", xlab="Team", ylab=NULL,
              coordFlip=TRUE, legendPosition="none")
# and without using IQR
createBoxplot(teamBA2000sPerc, x="teamid", fill="teamid", 
              title="Team BA in 2000s (no IQR)", xlab="Team", ylab=NULL,
              coordFlip=TRUE, legendPosition="none")

# Team BA by League facets in 2000s
teamBA2000byLgPerc = computePercentiles(conn, "batting_enh", "ba", 
                                        by=c('teamid','lgid'), where="yearid >= 2000")
createBoxplot(teamBA2000byLgPerc, x="teamid", fill="lgid", useIQR=TRUE,
              facet=c("lgid"),
              paletteValues = c("red","blue"), title="Team BA by League in 2000s",
              coordFlip=TRUE, legendPosition="none")

# League BA by decade and league facet grid
teamsBAbyDecadePerc = computePercentiles(conn, "batting_enh", "ba",
                                         by=c("lgid","decadeid"), 
                                         where="yearid>=1960 and lgid in ('AL','NL')")
createBoxplot(teamsBAbyDecadePerc, facet=c("lgid","decadeid"), fill="lgid", useIQR=TRUE,
              paletteValues = c("red","blue"), legendPosition="none",
              title="League BA by Decade")
