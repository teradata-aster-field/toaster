# Demo compute and create heatmap
#
# To install baseball demo dataset in Aster
# download baseball.zip from
# https://bitbucket.org/grigory/toaster/downloads/baseball.zip
# and run
# sh load_baseball_data.sh -d mydbname -U beehive 

library(toaster)
require(scales)

# update ODBC data source name
dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

# Heatmap of Average Wins by Franchise and Decade
hmFranchWins = computeHeatmap(conn, "teams_enh", 'franchid', 'decadeid', 'avg(w) w', 
                    where="decadeid >= 1950")
createHeatmap(hmFranchWins, 'decadeid', 'franchid', 'w',
              title = "Average Wins by Franchise and Decade")
                     
# Heatmap of Average Diff W-L with diverging color gradient
hmFranchWinLoss = computeHeatmap(conn, "teams_enh", 'franchid', 'decadeid', 'avg(w-l) wl', 
                    where="decadeid >= 1950")
createHeatmap(hmFranchWinLoss, 'decadeid', 'franchid', 'wl', divergingColourGradient = TRUE,
              title = "Average Win-Loss by Franchise and Decade")

# BA Heatmaps by Leagues
heatBA = computeHeatmap(conn, 'batting_enh', 'teamid', 'yearid',
                        aggregates="SUM(BA*AB)/SUM(AB) ba",
                        where="yearid between 1990 and 2012", by="lgid",
                        withMelt=FALSE)
createHeatmap(heatBA, 'yearid', 'teamid', 'ba',
              title='BA Heatmap AL vs. NL 1990-2012', xlab='Year', ylab='Team',
              lowGradient=muted("grey", l=30, c=250), highGradient="red",
              text=TRUE, percent=FALSE, digits=3,
              facet='lgid', legendPosition="none")

# ERA Heatmaps by Leagues
pitchERA = computeHeatmap(conn, 'pitching_enh', 'teamid', 'yearid',
                         aggregates=c("SUM(IPOuts*ERA)/SUM(IPOuts) era"),
                         where="yearid between 1990 and 2012", by="lgid")
createHeatmap(pitchERA, 'yearid', 'teamid', 'era',
              title='ERA Heatmap AL vs. NL 1990-2012', xlab='Year', ylab='Team',
              lowGradient="red", highGradient=muted("grey", l=30, c=250),
              text=TRUE, percent=FALSE, digits=3,
              facet='lgid', legendPosition="none")
