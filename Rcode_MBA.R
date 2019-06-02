#Reading the dataset
retail<-read.csv("C:\\Users\\user\\Desktop\\Market Basket Analysis\\Online Retail.csv")

install.packages('tidyverse')
install.packages('readxl')
install.packages('knitr')
install.packages('arules')
install.packages('arulesViz')
install.packages('ggplot2')
install.packages('lubridate')
install.packages('plyr')
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)


retail <- retail[complete.cases(retail), ] #removing the missing valuse from dataset
retail %>% mutate(Description = as.factor(Description)) #Datatype conversion
retail %>% mutate(Country = as.factor(Country)) #Datatype conversion
Res2 <- as.POSIXct(retail$InvoiceDate,  format = "%d/%m/%Y %H:%M") 
retail$Date <- sapply(strsplit(as.character(Res2), " "), "[", 1) #Splitting Time and Date
retail$Time <- sapply(strsplit(as.character(Res2), " "), "[", 2)
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)


#Number of items each customer buy? Please check Pic1 in attachment
detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

# Preprocessing dataset to convert to Transactions format
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

#Since We only need item transactions, we remove customerID and Date columns.
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

# Writing the Dataset
write.csv(itemList,"C:\\Users\\user\\Desktop\\Market Basket Analysis\\market_basket.csv", quote = FALSE, row.names = TRUE)

# Reading the datset in Transaction format
tr <- read.transactions('C:\\Users\\user\\Desktop\\Market Basket Analysis\\market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

"
> summary(tr)
transactions as itemMatrix in sparse format with
19297 rows (elements/itemsets/transactions) and
27165 columns (items) and a density of 0.0006701659 

most frequent items:
WHITE HANGING HEART T-LIGHT HOLDER           REGENCY CAKESTAND 3 TIER 
1758                               1660 
JUMBO BAG RED RETROSPOT                      PARTY BUNTING 
1434                               1271 
ASSORTED COLOUR BIRD ORNAMENT                            (Other) 
1237                             343943 

element (itemset/transaction) length distribution:
sizes
1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20 
1 2263 1189  851  768  725  662  618  597  582  554  572  506  487  508  504  503  449  413  477 
21   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40 
420  383  304  313  270  237  253  223  204  222  216  171  147  138  147  130  111  116   89  104 
41   42   43   44   45   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60 
96   92   85   94   61   67   73   67   64   52   49   59   50   41   53   50   35   24   40   35 
61   62   63   64   65   66   67   68   69   70   71   72   73   74   75   76   77   78   79   80 
29   27   23   21   21   17   27   31   24   16   24   18   19   18   13   14   17   14    7    9 
81   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96   97   98   99  100 
18   17   11   10    8   13   10   14    6    7    9    6    7    8    5    4    5    5    3    3 
101  102  103  104  105  106  107  108  109  110  111  112  113  114  115  116  117  118  119  120 
3    4    5    5    2    3    3    7    4    6    3    4    1    2    2    1    3    4    3    1 
121  122  123  124  126  127  128  132  133  134  135  140  141  142  143  144  146  147  148  150 
2    1    3    2    4    1    1    1    1    3    1    1    1    1    2    1    1    3    1    1 
151  155  158  162  167  169  172  178  179  181  199  200  203  205  206  210  230  237  250  251 
1    2    2    1    1    1    2    1    1    1    1    1    1    1    1    1    1    1    1    1 
287  322  402  421 
1    1    1    1 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.00    5.00   13.00   18.21   24.00  421.00 

includes extended item information - examples:
labels
1        1
2 1 HANGER
3       10"

# Item frequency plot - Please check Pic2

itemFrequencyPlot(tr, topN=20, type='absolute') 

# * We use the Apriori algorithm in Arules library to mine frequent itemsets and association rules. The algorithm employs level-wise search for frequent itemsets.
# * We pass supp=0.001 and conf=0.8 to return all the rules that have a support of at least 0.1% and confidence of at least 80%.
# * We sort the rules by decreasing confidence.

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

"set of 87110 rules

rule length distribution (lhs + rhs):sizes
2     3     4     5     6     7     8     9    10 
105  3133  9732 26228 29873 14020  3218   680   121 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
2.000   5.000   6.000   5.627   6.000  10.000 

summary of quality measures:
support           confidence          lift             count       
Min.   :0.001036   Min.   :0.8000   Min.   :  8.781   Min.   : 20.00  
1st Qu.:0.001088   1st Qu.:0.8333   1st Qu.: 19.305   1st Qu.: 21.00  
Median :0.001192   Median :0.8750   Median : 24.786   Median : 23.00  
Mean   :0.001383   Mean   :0.8834   Mean   : 50.921   Mean   : 26.69  
3rd Qu.:0.001503   3rd Qu.:0.9231   3rd Qu.: 43.662   3rd Qu.: 29.00  
Max.   :0.018086   Max.   :1.0000   Max.   :622.484   Max.   :349.00  

mining info:
data ntransactions support confidence
tr         19297   0.001        0.8 "


inspect(rules[1:10])

"    lhs                         rhs             support     confidence lift      count
[1]  {WOBBLY CHICKEN}         => {DECORATION}    0.001451003 1          385.94000 28   
[2]  {WOBBLY CHICKEN}         => {METAL}         0.001451003 1          385.94000 28   
[3]  {DECOUPAGE}              => {GREETING CARD} 0.001191895 1          344.58929 23   
[4]  {BILLBOARD FONTS DESIGN} => {WRAP}          0.001502824 1          622.48387 29   
[5]  {WOBBLY RABBIT}          => {DECORATION}    0.001761932 1          385.94000 34   
[6]  {WOBBLY RABBIT}          => {METAL}         0.001761932 1          385.94000 34   
[7]  {BLACK TEA}              => {SUGAR JARS}    0.002331969 1          212.05495 45   
[8]  {BLACK TEA}              => {COFFEE}        0.002331969 1           61.06646 45   
[9]  {ART LIGHTS}             => {FUNK MONKEY}   0.001969218 1          507.81579 38   
[10] {FUNK MONKEY}            => {ART LIGHTS}    0.001969218 1          507.81579 38"

"The interpretation is pretty straightforward:

* 100% customers who bought "WOBBLY CHICKEN" also bought "DECORATION".
* 100% customers who bought "BLACK TEA" also bought "SUGAR JAR"."


topRules <- rules[1:10]
#Plotting the top 10 rules -Pic3
plot(topRules)
#Plotting the top 10 rules graphically -Pic4 
plot(topRules, method="graph")
#Plotting the top 10 rules based on grouping -Pic5
plot(topRules, method = "grouped")


