require(toaster)
require(scales)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

hmFranchWins = computeHeatmap(conn, "teams_enh", 'franchid', 'decadeid', 'avg(w) w', 
                    where="decadeid >= 1950")
createHeatmap(hmFranchWins, 'decadeid', 'franchid', 'w')
                     
# with diverging color gradient
hmFranchWinLoss = computeHeatmap(conn, "teams_enh", 'franchid', 'decadeid', 'avg(w-l) wl', 
                    where="decadeid >= 1950")
                    hm$decadeid = factor(hm$decadeid)
createHeatmap(hmFranchWinLoss, 'decadeid', 'franchid', 'wl', divergingColourGradient = TRUE)

# Heatmap Analysis: BA by lgid
heatBA = computeHeatmap(conn, 'batting_enh', 'teamid', 'yearid',
                        aggregates="SUM(BA*AB)/SUM(AB) ba",
                        where="yearid between 1990 and 2012", by="lgid",
                        withMelt=FALSE)
createHeatmap(heatBA, 'yearid', 'teamid', 'ba',
              title='BA Heatmap AL vs. NL 1990-2012', xlab='Year', ylab='Team',
              lowGradient=muted("grey", l=30, c=250), highGradient="red",
              text=TRUE, percent=FALSE, digits=3,
              facet='lgid', legendPosition="none")

pitchERA = computeHeatmap(conn, 'pitching_enh', 'teamid', 'yearid',
                         aggregates=c("SUM(IPOuts*ERA)/SUM(IPOuts) era"),
                         where="yearid between 1990 and 2012", by="lgid")
createHeatmap(pitchERA, 'yearid', 'teamid', 'era',
              title='ERA Heatmap AL vs. NL 1990-2012', xlab='Year', ylab='Team',
              lowGradient="red", highGradient=muted("grey", l=30, c=250),
              text=TRUE, percent=FALSE, digits=3,
              facet='lgid', legendPosition="none")
