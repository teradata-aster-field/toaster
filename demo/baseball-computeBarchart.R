# Demo compute bar chart
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

# simple bar chart
bc = computeBarchart(channel=conn, tableName="pitching_enh", category="teamid",
                     aggregates="AVG(era) era", 
                     where="yearid >= 2000 and lgid='AL'")
createHistogram(bc, "teamid", "era", fill="teamid", 
                title = "AL Teams Average ERA in 2000s", legendPosition="none")

# multipe aggregates in the same bar chart (with melt)
bc = computeBarchart(channel=conn, tableName="pitching_enh", category="teamid",
                     aggregates=c("AVG(era) era", "AVG(whip) whip"), withMelt=TRUE,
                     where="yearid >= 2000 and lgid='AL'")
createHistogram(bc, "teamid", "value", fill="teamid", facet="variable",
                title = "AL Teams Average ERA and WHIP in 2000s", legendPosition="none")

# Average pitcher stats by team and decade
bc = computeBarchart(conn, "pitching_enh", "teamid", 
                     aggregates=c("AVG(era) era", "AVG(whip) whip", "AVG(ktobb) ktobb"),
                     where="yearid >= 1990 and lgid='AL'", by="decadeid", withMelt=TRUE)
createHistogram(bc, "teamid", "value", fill="teamid", facet=c("variable", "decadeid"), 
                legendPosition="bottom",
                title = "AL Teams Pitching Stats by decades (1990-2012)",
                themeExtra = guides(fill=guide_legend(nrow=2)))

# Average Franchise wins-loss difference trends by decades
franchwl = computeBarchart(conn, "teams_enh", "franchid",
                           aggregates=c("AVG(w) w", "AVG(l) l", "AVG(w-l) wl"),
                           by="decadeid",
                           where="yearid >=1960 and lgid = 'AL'")
createHistogram(franchwl, "decadeid", "wl", fill="franchid",
                facet="franchid", ncol=5, facetScales="fixed",
                legendPosition="none",
                trend=TRUE,
                title="Average W-L difference by decade per team (AL)",
                ylab="Average W-L")

# Players with top KtoBB average and average number of ipouts
playerTopKtoBB = computeBarchart(conn, "pitching_enh", "playerid",
               aggregates=c("AVG(ktobb) ktobb", "AVG(ipouts) ipouts"),
               where="lgid in ('AL','NL') and yearid > 1960",
               top=30, orderBy="ktobb desc")
createHistogram(playerTopKtoBB, "playerid", "ktobb", fill="ipouts",
                scaleGradient=scale_fill_gradient("IP Outs", high="red4", low="tan"))
