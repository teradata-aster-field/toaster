# Demo showData type of analysis
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

pitchingInfo = getTableSummary(conn, 'pitching_enh', where='yearid between 2000 and 2013')
battingInfo = getTableSummary(conn, 'batting_enh', where='yearid between 2000 and 2013')
teamsInfo = getTableSummary(conn, tableName='teams_enh', where='yearid between 1960 and 2013')

# overview
showData(channel=NULL, 'pitching_enh', pitchingInfo, type='numeric', format='overview',
         except=c('lgid', 'teamid', 'yearid','playerid', 'bfp', 'ipouts', 'h', 'so','r','g','bb','er'), 
         measures = c('average','deviation','IQR','0%','10%','25%','50%','75%','90%','100%'),
         where='yearid between 2000 and 2013')

showData(channel=NULL, 'pitching_enh', pitchingInfo, type='numeric', format='overview',
         include=c('h','so','r','g','bb','er','hr'), 
         measures = c('average','deviation','IQR','0%','10%','25%','50%','75%','90%','100%'),
         scales="free_y",
         where='yearid between 2000 and 2013')

summary = getTableSummary(conn, 'pitching_enh', include=c('baopp', 'era', 'whip', 'ktobb', 'fip'),
                          where='yearid between 2000 and 2013')
showData(channel=NULL, 'pitching_enh', summary, type='numeric', format='overview',
         include=c('era','fip','whip','ktobb'), 
         measures = c('average','deviation','IQR','0%','10%','25%','50%','75%'),
         scales="fixed",
         where='yearid between 2000 and 2013')

showData(channel=NULL, 'batting_enh', battingInfo, type='numeric', format='overview',
         except=c('legid', 'teamid', 'yearid','playerid'), where='yearid between 2000 and 2013',
         scales="free_y")

# boxplots
showData(channel=NULL, tableName='pitching_enh', tableInfo=pitchingInfo, format='boxplot')

showData(channel=NULL, tableName='pitching_enh', tableInfo=pitchingInfo, format='boxplot',
         coordFlip=TRUE)

showData(channel=NULL, tableName='pitching_enh', tableInfo=pitchingInfo, format='boxplot',
         except=c('yearid', 'teamid','bfp','ipouts','so','r','h','er','era','sv','g','bb'),
         defaultTheme=theme_grey(),
         where='yearid between 2000 and 2013')

showData(channel=NULL, tableName='batting_enh', tableInfo=battingInfo, format='boxplot', 
         include = c('ab','tb','tob','so','h','bb','g_batting','g'),
         coordFlip=TRUE,
         defaultTheme=theme_grey(),
         where='yearid between 2000 and 2013')

showData(channel=NULL, tableName='pitching_enh', tableInfo=pitchingInfo, format='boxplot',
         include=c('bfp','er','h','ipouts','r','so'), ncol=3,
         facet=TRUE, scale="free_x")

# histograms
showData(conn, 'pitching_enh', summary,
         include=c('baopp', 'era', 'whip', 'ktobb', 'fip'), 
         # include=c('w', 'l', 'g', 'ipouts', 'h', 'er', 'hr', 'bb', 'so', 'bfp', 'r'),
         format='histogram', numBins=50, facet=TRUE,
         where='yearid between 2000 and 2013')

showData(conn, 'batting_enh', battingInfo,
         include=c('ba','ta','slg'),
         format='histogram', numBins=100, facet=TRUE,
         where='yearid between 2000 and 2013 
                and ab >= 30')

# correlation
showData(conn, tableName='pitching_enh', tableInfo=pitchingInfo, 
         include=c('w', 'l', 'g', 'ipouts', 'h', 'er', 'hr', 'bb', 'so', 'bfp', 'r'),
                #   'baopp', 'era', 'whip', 'ktobb', 'fip'),
         format='corr', corrLabel='value', digits=2, shapeSizeRange=c(5,25), 
         defaultTheme=theme_classic(base_size=12), legendPosition="none")

showData(conn, tableName='batting_enh', tableInfo=battingInfo,
         include=c('ab', 'r', 'h', 'x2b', 'x3b', 'hr', 'rbi', 'sb', 'cs', 'bb', 'so', 'ibb', 'hbp', 'sh', 'sf', 'gidp',
                   'tb', 'tob', 'xbh', 'ba', 'slg', 'ta'),
         format='corr', corrLabel='value', digits=2, shapeSizeRange=c(5,25),
         defaultTheme=theme_classic(base_size=12), legendPosition="none")

showData(conn, tableName='teams_enh', tableInfo=teamsInfo,
         except=c('yearid','decadeid','ghome','g'),
         format='corr', corrLabel='value', digits=2, shapeSizeRange=c(5,25),
         defaultTheme=theme_classic(base_size=12), legendPosition="none")

# scatterplots
showData(conn, 'pitching_enh', format='scatterplot', 
         include=c('so', 'er'), facetName="lgid", pointColour="lgid", 
         sampleSize=10000, regressionLine=TRUE,
         title="SO vs ER by League 1980-2000",
         where='yearid between 1980 and 2000', legendPosition="none")

showData(conn, 'pitching_enh', format='scatterplot', 
         include=c('so', 'er'), facetName="lgid", pointColour="lgid", 
         sampleFraction=0.3, regressionLine=TRUE,
         title="SO vs ER by League 1980-2000",
         where='yearid between 1980 and 2000', legendPosition="none")

showData(conn, 'pitching_enh', format='scatterplot', 
         include=c('so','er'), facetName=c('lgid','decadeid'), pointColour="lgid",
         sampleFraction=0.1, regressionLine=TRUE,
         title="SO vs ER by League by Decade 1980 - 2012",
         where='yearid between 1980 and 2012', legendPosition="none")

showData(conn, 'pitching_enh', format='scatterplot', include=c('ktobb', 'fip', 'teamid','yearid'),
         sampleSize=10000, facetName=c('teamid','yearid'), regressionLine=TRUE,
         where="yearid in (2010,2011,2012) and teamid in ('TEX','NYA')")

