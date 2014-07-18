# Demo compute TF-IDF

library(toaster)
library(tm)

close(conn)
conn = odbcDriverConnect(connection="driver={Aster ODBC Driver};server=153.64.249.88;port=2406;database=usps;uid=db_superuser;pwd=db_superuser")
conn = odbcDriverConnect("DSN=PresalesPartnersDB")

# term frequency on narratives
# divided into 4 time groups: 0-3
tfIdfDallasByTime = computeTfIdf(channel=conn, tableName="public.dallaspoliceall", 
                                 docId="(extract('hour' from offensestarttime)::int/6)%4", 
                    textColumns=c('offensedescription','offensenarrative'),
                    parser=nGram(1, punctuation="[\\\\[\\\\]%/`~#^&*()''\":;><@0-9-]+"),
                    test=FALSE)

dim(inspect(tfIdfDallasByTime))

findFreqTerms(tfIdfDallasByTime, 0.00008)

findAssocs(tfIdfDallasByTime, "FINANCIAL INVESTIGATIONS", 0.98)

tfNarrByTimeDF <- cbind(term=dimnames(tfNarrByTime)[[1]], as.data.frame(inspect( tfNarrByTime )), stringsAsFactors=FALSE)
tfNarrByTimeDFMelt = melt(tfNarrByTimeDF, id.vars='term', measure.vars=as.character(0:23), 
                          variable.name='docid', value.name='tf')
tfOrdered <- tfNarrByTimeDFMelt[order(-tfNarrByTimeDFMelt$tf), ]
by(tfOrdered, tfOrdered["docid"], head, n=20)



tf1 = computeTf(channel=conn, tableName="texts.containertrailerplanpaths", docId="orig", 
                textColumns=c("commentsall"),
                parser=nGram(3, punctuation="[\\\\[\\\\]%/`~#^&*()-]+"))
words = tf1$rs
pal = rev(brewer.pal(8, "Set1"))[c(-3,-1)]
createWordcloud(words$term, words$tf, maxWords=20, scale=c(4,1), palette=pal)

tfidf1 = computeTfIdf(channel=conn, tableName="texts.containertrailerplanpaths", docId="orig", 
                      textColumns=c("commentsall"),
                      parser=nGram(2))
words=tfidf1$rs
createWordcloud(words$term, words$tf_idf, maxWords=25, scale=c(4, 0.5), palette=pal)

tfidf1 = computeTfIdf(channel=conn, tableName="texts.containertrailerplanpaths", docId="orig", 
                      textColumns=c("f5466all", "commentsall"),
                      where="f5466all not like '%null%' and commentsall not like '%null%'",
                      parser=nGram(2:3, punctuation="[\\\\[\\\\]%/`~#^&*()-]+"))

sort(table(tfidf1$docid))
words = tfidf1[tfidf1$docid == '913',]
createWordcloud(words$term, words$tf_idf, maxWords=25, scale=c(4, 0.5), title="Top 25 2-grams for 913")



