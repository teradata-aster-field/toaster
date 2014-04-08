detach("package:toaster", unload=T)
require(toaster)
require(scales)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(asterConn)
asterConn = odbcConnect(dsn, uid, pwd)

pitchingInfo = getTableSummary(asterConn, 'pitching_enh', where='yearid between 2000 and 2013')
battingInfo = getTableSummary(asterConn, 'batting_enh', where='yearid between 2000 and 2013')
teamsInfo = getTableSummary(asterConn, tableName='teams_enh', where='yearid between 1960 and 2013')

# Boxplots
teamBaLgidDecadeid = computePercentiles(asterConn, "teams_enh", "ba", by=c('lgid','decadeid'),
                                        where="lgid in ('AL','NL')")
createBoxplot(teamBaLgidDecadeid, fill='lgid', facet=c('lgid', 'decadeid'),
              legendPosition="none")

# Histograms
h2000s = computeHistogram(asterConn, 'pitching_enh', 'era',
                 binsize=0.2, startvalue=0, endvalue=10, 
                 where="yearID between 2000 and 2012 and teamid in ('NYA','TEX')", by='teamid')
createHistogram(h2000s, fill='teamid', facet='teamid', title='TEX vs. NYY 2000-2012', xlab='ERA', ylab='count',
                legendPosition='none')

h2009s = computeHistogram(asterConn, 'pitching_enh', 'era',
                          binsize=0.2, startvalue=0, endvalue=10, 
                          where="yearID between 2009 and 2012 and teamid in ('NYA','TEX')", by='teamid')
createHistogram(h2009s, fill='teamid', facet='teamid', title='TEX vs. NYY 2009-2012', xlab='ERA', ylab='count',
                legendPosition='none')

# Analysis of BA by decade AL vs. NL
hBADecadeNL=computeHistogram(asterConn, 'batting_enh', 'ba',
                             startvalue=0.01, endvalue=0.5, binsize=0.01,
                             where="lgid='NL' and yearid between 1960 and 2009", by='decadeid')
hBADecadeAL=computeHistogram(asterConn, 'batting_enh', 'ba',
                             startvalue=0.01, endvalue=0.5, binsize=0.01,
                             where="lgid='AL' and yearid between 1960 and 2009", by='decadeid')
hBADecadeNL=cbind(hBADecadeNL, lgid='NL')
hBADecadeAL=cbind(hBADecadeAL, lgid='AL')
hBADecade = rbind(hBADecadeAL, hBADecadeNL)

createHistogram(hBADecade, fill='lgid', facet=c('decadeid','lgid'), facetScales='fixed', 
                title='AL vs. NL Batting Avg 1960-2009', xlab='BA', ylab='count',
                breaks=seq(from=0.0, to=0.5, by=0.05),
                legendPosition='none')

# Analysis of ERA by decade AL vs. NL
hERADecadeNL=computeHistogram(asterConn, 'pitching_enh', 'era',
                            startvalue=0.01, endvalue=10., binsize=0.1,
                            where="lgid='NL' and yearid between 1960 and 2009", by='decadeid')
hERADecadeAL=computeHistogram(asterConn, 'pitching_enh', 'era',
                              startvalue=0.01, endvalue=10., binsize=0.1,
                             where="lgid='AL' and yearid between 1960 and 2009", by='decadeid')
hERADecadeNL=cbind(hERADecadeNL, lgid='NL')
hERADecadeAL=cbind(hERADecadeAL, lgid='AL')
hERADecade = rbind(hERADecadeAL, hERADecadeNL)

createHistogram(hERADecade, fill='lgid', facet=c('lgid','decadeid'), facetScales='fixed', 
                title='AL vs. NL ERA 1960-2009', xlab='ERA', ylab='count',
                breaks=seq(from=0.0, to=10.0, by=0.5),
                legendPosition='none')


# Bubble chart example

bubble = computeHeatmap(asterConn, 'teams_enh', 'franchid', 'decadeid',
                        aggregateFun=c("SUM(BA*AB)/SUM(AB)", 
                                       "SUM(IPOuts*ERA)/SUM(IPOuts)",
                                       "ROUND(AVG(8-rank))",
                                       "MIN(lgid)"),
                        aggregateAlias=c("ba", 
                                         "era",
                                         "rank",
                                         "lgid"),
                        where="yearid between 1970 and 2009")


createBubblechart(bubble, "ba", "era", "rank", label="franchid", fill="franchid",
                  facet=c("decadeid","lgid"), #ncol=1, 
                  scaleSize = FALSE, shapeSizeRange=c(1,15), shapeMaxSize = 15,
                  title="Team Ranks by BA and ERA", 
                  labelSize = 5, labelColour = "black", labelVJust = 1,
                  legendPosition="none", themeExtra = guides(fill = "legend", size = "legend"))

createBubblechart(bubble, "ba", "era", "rank", label="franchid", fill="franchid",
                  facet=c("lgid", "decadeid"), ncol=1, shapeSizeRange=c(1,20),
                  title="Team Ranks by BA and ERA", 
                  labelSize = 5, labelColour = "black", labelVJust = 1,
                  legendPosition="none", themeExtra = guides(fill = "legend", size = "legend"))