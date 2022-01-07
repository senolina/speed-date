train7<-read.csv("C:/Users/HOME/Desktop/train_v5.1.csv",header=T);


##1. from 범주화

#1) 94115 -> San Francisco
#2) 동부 -> 노스다코타, 사우스다코타, 네브래스카, 캔사스, 오클라호마,
#      텍사스 Dallas, Houston, Austin, San Antonio
#      미네소타, 위스콘신, 아이오와, 일리노이 Chicago
#      미시간 Detroit, 미주리, 아칸소, 미시시피, 루이지애나 New Orleans
#      앨라배마, 테네시 Nashville, 캔터키, 인디아나, 미시간, 오하이오
#      웨스트 버지니아, 버지니아 Washington, 노스캐롤라이나, 사우스 캐롤라이나, 조지아 Atlanta
#      플로리다 Orlando, Jacksonville, Tampa, Miami
#      펜실베니아, 뉴욕, 버몬트, 뉴햄프셔, 메사추세츠 Boston, 코네티컷,
#      
#3) 서부 ->  워싱턴 Seattle Portland
#      오레곤 , 아이다호, 몬타나, 와이오밍, 네바다, 유타, 콜로라도 Denber
#      캘리포니아 San Francisco, San Jose, Los Angeles, Sandiego
#      네바다 Las Vegas
#      유타, 아리조나 Phoenix, 뉴멕시코

train7$from <- as.character(train7$from)

train7$from[train7$from==94115] <- 'west'
train7$from[train7$from=='Arizona'] <- 'west'
train7$from[train7$from=='Alabama'] <- 'east'
train7$from[train7$from=='Tuscaloosa, Alabama'] <- 'east'

train7$from[train7$from=='alabama'] <- 'east'
train7$from[train7$from=='Albuquerque, NM'] <- 'east'
train7$from[train7$from=='Ann Arbor, MI'|train7$from=='Ann Arbor'] <- 'east'
train7$from[train7$from=='Atlanta, GA'|train7$from=='Atlanta'] <- 'east'
train7$from[train7$from=='atlanta, ga'] <- 'east'
train7$from[train7$from=='Austin, TX'] <- 'east'
train7$from[train7$from=='Baltimore'] <- 'east'
train7$from[train7$from=='Berkeley'] <- 'west'
train7$from[train7$from=='Berkeley, CA'] <- 'west'
train7$from[train7$from=='Born in Montana, raised in South Jersey (nr. Philadelphia)'] <- 'west'
train7$from[train7$from=='Boston'|train7$from=='Boston, MA'] <- 'east'
train7$from[train7$from=='boston, ma'|train7$from=='Boston, Ma'] <- 'east'
train7$from[train7$from=='Boulder, Colorado'] <- 'west'
train7$from[train7$from=='Bowdoin College'] <- 'east'
train7$from[train7$from=='Brandeis University'] <- 'east'
train7$from[train7$from=='Bronx Science'] <- 'east'
train7$from[train7$from=='Brooklyn'|train7$from=='Brooklyn, NY'|train7$from=='brooklyn ny'|train7$from=='brooklyn, ny'] <- 'east'
train7$from[train7$from=='Buffalo, NY'] <- 'east'
train7$from[train7$from=='Burlington, Vermont'] <- 'east'
train7$from[train7$from=='California'|train7$from=='california'|train7$from=='California (West Coast)'|train7$from=='CALIFORNIA'|train7$from=='California and New York'|train7$from=='California, New Jersey'] <- 'west'
train7$from[train7$from=='Cambridge, MA'|train7$from=='Cambridge, Massachusetts'] <- 'east'

train7$from[train7$from=='Cherry Hill, NJ'|train7$from=='Chicago'] <- 'east'
train7$from[train7$from=='Cincinnati, Ohio'|train7$from=='Cincinnati, OH'] <- 'east'
train7$from[train7$from=='Cleveland'] <- 'east'
train7$from[train7$from=='colorado'|train7$from=='Colorado'] <- 'west'
train7$from[train7$from=='Connecticut'] <- 'east'
train7$from[train7$from=='CT, FL, TN'] <- 'east'
train7$from[train7$from=='Dallas, Texas'] <- 'east'
train7$from[train7$from=='DC'] <- 'east'
train7$from[train7$from=='Detroit'|train7$from=='Detroit, Michigan, USA'|train7$from=='Detroit suburbs'] <- 'east'
train7$from[train7$from=='Erie, PA'] <- 'east'
train7$from[train7$from=='Florida'|train7$from=='Florida and Virginia'] <- 'east'
train7$from[train7$from=='Georgia, USA'] <- 'east'
train7$from[train7$from=='Great Neck, NY'] <- 'east'
train7$from[train7$from=='Greenwich, CT'] <- 'east'
train7$from[train7$from=='Hastings-on-Hudson, NY'] <- 'east'
train7$from[train7$from=='Hawaii and Los Angeles'|train7$from=='Hawaii'] <- 'west'
train7$from[train7$from=='Houston'] <- 'east'
train7$from[train7$from=='I am from NYC'] <- 'east'
train7$from[train7$from=='Iowa'] <- 'east'
train7$from[train7$from=='J.P. Morgan'] <- 'east'
train7$from[train7$from=='Kansas'|train7$from=='Kansas City, Missouri'] <- 'east'
train7$from[train7$from=='Katonah, NY (more recently, Boston)'] <- 'east'
train7$from[train7$from=='Las Vegas, Nevada'] <- 'west'
train7$from[train7$from=='Lexington, MA'] <- 'east'
train7$from[train7$from=='Long Island'|train7$from=='Long Island, NY'] <- 'east'
train7$from[train7$from=='Los Angeles'|train7$from=='Los Angeles, CA'] <- 'west'
train7$from[train7$from=='lOS aNGELES'|train7$from=='los angeles'] <- 'west'
train7$from[train7$from=='Louisiana'] <- 'east'
train7$from[train7$from=='Manhattan'] <- 'east'
train7$from[train7$from=='Maryland'] <- 'east'
train7$from[train7$from=='Massachusetts'] <- 'east'
train7$from[train7$from=='MD'] <- 'east'
train7$from[train7$from=='Memphis, TN'] <- 'east'
train7$from[train7$from=='Miami'] <- 'east'
train7$from[train7$from=='Michigan'] <- 'east'
train7$from[train7$from=='Midwest USA'] <- 'east'
train7$from[train7$from=='Milwaukee, Wisconsin'] <- 'east'
train7$from[train7$from=='Minneapolis, MN'|train7$from=='Minneapolis'|train7$from=='Minnesota'] <- 'east'
train7$from[train7$from=='nashville, TN'] <- 'east'
train7$from[train7$from=='Nebraska'] <- 'east'
train7$from[train7$from=='New Hope, PA'] <- 'east'
train7$from[train7$from=='New Jersey'|train7$from=='new jersey'] <- 'east'
train7$from[train7$from=='New Mexico'] <- 'west'
train7$from[train7$from=='New York'|train7$from=='new york'|train7$from=='New York, NY'|train7$from=='new york city'] <- 'east'
train7$from[train7$from=='New York City'|train7$from=='New York Area/ New Jersey'|train7$from=='New York/South Korea/Japan'] <- 'east'
train7$from[train7$from=='NJ'] <- 'east'
train7$from[train7$from=='North Carolina'] <- 'east'
train7$from[train7$from=='Northern California'] <- 'west'
train7$from[train7$from=='Northern New Jersey'] <- 'east'
train7$from[train7$from=='Northern Virginia'] <- 'east'
train7$from[train7$from=='NY'|train7$from=='NYC'|train7$from=='NYC (Staten Island)'|train7$from=='nyc'|train7$from=='NYC-6 yrs. Grew up in Nebraska'|train7$from=='NYC, San Francisco'] <- 'east'
train7$from[train7$from=='Ohio'] <- 'east'
train7$from[train7$from=='Oregon'] <- 'west'
train7$from[train7$from=='PA'] <- 'east'
train7$from[train7$from=='Palm Springs, California'|train7$from=='Palo Alto, CA'] <- 'west'
train7$from[train7$from=='Pennsylvania'] <- 'east'
train7$from[train7$from=='Philadelphia'|train7$from=='Philadelphia, PA'] <- 'east'
train7$from[train7$from=='Pittsburgh'|train7$from=='Pittsburgh, PA'] <- 'east'
train7$from[train7$from=='Portland, Oregon'|train7$from=='Portland, OR'] <- 'west'
train7$from[train7$from=='Pougkeepsie NY'] <- 'east'
train7$from[train7$from=='Queens'] <- 'east'
train7$from[train7$from=='Rochester, NY'] <- 'east'
train7$from[train7$from=='Salt Lake City'] <- 'west'
train7$from[train7$from=='San Diego, CA'|train7$from=='San Diego'] <- 'west'
train7$from[train7$from=='San Francisco(home)/Los Angeles(undergrad)'|train7$from=='San Francisco'|train7$from=='San Francisco, CA'|train7$from=='San Francisco Bay Area'|train7$from=='Santa Barbara, California'|train7$from=='San Francisco/LA'] <- 'west'
train7$from[train7$from=='Saratoga, NY'] <- 'east'
train7$from[train7$from=='Seattle'] <- 'west'
train7$from[train7$from=='SF Bay Area, CA'] <- 'west'
train7$from[train7$from=='Silver Spring, MD'] <- 'east'
train7$from[train7$from=='south carolina'] <- 'east'
train7$from[train7$from=='South Orange, New Jersey'] <- 'east'
train7$from[train7$from=='Southern California'] <- 'west'
train7$from[train7$from=='St. Louis, MO'] <- 'east'
train7$from[train7$from=='State College, PA'] <- 'east'
train7$from[train7$from=='Staten Island'] <- 'east'
train7$from[train7$from=='Texas'|train7$from=='Texas & Boston'|train7$from=='Texas/London'] <- 'east'
train7$from[train7$from=='TN'] <- 'east'
train7$from[train7$from=='Torrance, CA'] <- 'west'
train7$from[train7$from=='UNCC'] <- 'east'
train7$from[train7$from=='Upstate New York'] <- 'east'
train7$from[train7$from=='USA/American'] <- 'east'
train7$from[train7$from=='Virginia'] <- 'east'
train7$from[train7$from=='Wash DC (4 yrs)'] <- 'east'
train7$from[train7$from=='Washington, DC'|train7$from=='Washington DC'|train7$from=='Washington State'|train7$from=='Washington DC Metro Region'|train7$from=='Washington, D.C.'|train7$from=='WASHINGTON, D.C.'] <- 'east'
train7$from[train7$from=='Westchester, NY'|train7$from=='Westchester, new York'|train7$from=='Westchester County, N.Y.'] <- 'east'
train7$from[train7$from=='Wisconsin'] <- 'east'
train7$from[train7$from=='Woburn, MA'] <- 'east'
train7$from[train7$from=='working'] <- 'east'
train7$from[train7$from=='International Student'] <- 'east'
train7$from[train7$from=='Midwest USA'] <- 'west'
train7$from[train7$from=='Palo Alto, California'] <- 'west'
train7$from[train7$from=='Puerto Rico'] <- 'east'
train7$from[train7$from=='Vestal'] <- 'east'

train7$from[train7$from=='way too little space here. world citizen.'] <- 'east'

train7$from[train7$from=="Cameroon"]<- "Africa"


##유럽

train7$from[train7$from=="France"]<- "EU"
train7$from[train7$from=="Italy"]<- "EU"
train7$from[train7$from=="Milan - Italy"]<- "EU"
train7$from[train7$from=="Milan, Italy"]<- "EU"
train7$from[train7$from=="Milano, Italy"]<- "EU"
train7$from[train7$from=="Spain"]<- "EU"
train7$from[train7$from=="spain"]<- "EU"
train7$from[train7$from=="London, England"]<- "EU"
train7$from[train7$from=="London, UK"]<- "EU"
train7$from[train7$from=="Bulgaria"]<- "EU"
train7$from[train7$from=="UK"]<- "EU"
train7$from[train7$from=="UK/Turkey"]<- "EU"

train7$from[train7$from=="czech republic"]<- "EU"
train7$from[train7$from=="Genova, Italy"]<- "EU"
train7$from[train7$from=="Iceland"]<- "EU"
train7$from[train7$from=="London"]<- "EU"
train7$from[train7$from=="France"]<- "EU"
train7$from[train7$from=="france"]<- "EU"

train7$from[train7$from=="Romania"]<- "EU"
train7$from[train7$from=="Germany"]<- "EU"
train7$from[train7$from=="England"]<- "EU"
train7$from[train7$from=="Greece"]<- "EU"
train7$from[train7$from=="Greece/Germany"]<- "EU"

train7$from[train7$from=="Genova, EU"]<- "EU"
train7$from[train7$from=="Milan, EU"]<- "EU"
train7$from[train7$from=="Belgium"]<- "EU"
train7$from[train7$from=="Budapest"]<- "EU"
train7$from[train7$from=="czech republic"]<- "EU"
train7$from[train7$from=="Europe"]<- "EU"
train7$from[train7$from=="Genova, Italy"]<- "EU"
train7$from[train7$from=="Bulgaria"]<- "EU"
train7$from[train7$from=="Romania"]<- "EU"
train7$from[train7$from=="Paris"]<- "EU"
train7$from[train7$from=="Sweden"]<- "EU"
train7$from[train7$from=="Paris"]<- "EU"
train7$from[train7$from=="Cameroon"]<- "EU"
train7$from[train7$from=="Cameroon"]<- "EU"
train7$from[train7$from=="Switzerland"]<- "EU"
train7$from[train7$from=="Albania"]<- "EU"
train7$from[train7$from=="France  / New York"]<- "EU"
train7$from[train7$from=="London & New York"]<- "EU"
train7$from[train7$from=="sofia, bg"]<- "EU"
train7$from[train7$from=="Warsaw, Poland"]<- "EU"
train7$from[train7$from=="poland"]<- "EU"




##북아메리나

train7$from[train7$from=="Canada"]<- "North America"
train7$from[train7$from=="ottawa, canada"]<- "North America"
train7$from[train7$from=="Ottawa, Canada"]<- "North America"
train7$from[train7$from=="Canada"]<- "North America"
train7$from[train7$from=="Toronto, Canada"]<- "North America"
train7$from[train7$from=="Toronto, London, India"]<- "North America"
train7$from[train7$from=="Canada"]<- "North America"
train7$from[train7$from=="Toronto"]<- "North America"

train7$from[train7$from=="Toronto,North America"]<- "North America"


#Azerbaijan
##아시아
train7$from[train7$from=="Malaysia, then Massachusetts"]<- "Asia"
train7$from[train7$from=="Nepal"]<- "Asia"
train7$from[train7$from=="Manila, Philippines"]<- "Asia"
train7$from[train7$from=="Azerbaijan"]<- "Asia"
train7$from[train7$from=="Asia, Singapore"]<- "Asia"
train7$from[train7$from=="Philippines"]<- "Asia"
train7$from[train7$from=="BEIJING, CHINA"]<- "Asia"
train7$from[train7$from=="Persia"]<- "Asia"
train7$from[train7$from=="China"]<- "Asia"
train7$from[train7$from=="china"]<- "Asia"
train7$from[train7$from=="P. R. China"]<- "Asia"
  

train7$from[train7$from=="India"]<- "Asia"
train7$from[train7$from=="india"]<- "Asia"
train7$from[train7$from=="India and NJ"]<- "Asia"
train7$from[train7$from=="India, Holland"]<- "Asia"
train7$from[train7$from=="India/Venezuela"]<- "Asia"
train7$from[train7$from=="Indiana"]<- "Asia"
train7$from[train7$from=="Indonesia"]<- "Asia"
train7$from[train7$from=="India"]<- "Asia"
train7$from[train7$from=="New Delhi, India"]<- "Asia"



train7$from[train7$from=="Bombay, India"]<- "Asia"
train7$from[train7$from=="Taiwan"]<- "Asia"
train7$from[train7$from=="Hong Kong"]<- "Asia"
train7$from[train7$from=="HKG"]<- "Asia"
train7$from[train7$from=="Bangladesh"]<- "Asia"
train7$from[train7$from=="Singapore"]<- "Asia"
train7$from[train7$from=="HKG"]<- "Asia"
train7$from[train7$from=="Nepal"]<- "Asia"
train7$from[train7$from=="SOUTH KOREA"]<- "Asia"
train7$from[train7$from=="Taipei, Taiwan"]<- "Asia"
train7$from[train7$from=="Taiwan"]<- "Asia"
train7$from[train7$from=="taiwan"]<- "Asia"

train7$from[train7$from=="Indonesia"]<- "Asia"
train7$from[train7$from=="Malaysia"]<- "Asia"
train7$from[train7$from=="Israel"]<- "Asia"
train7$from[train7$from=="Korea"]<- "Asia"

train7$from[train7$from=="Russia"]<- "Asia"
train7$from[train7$from=="Japan"]<- "Asia"
train7$from[train7$from=="japan"]<- "Asia"
train7$from[train7$from=="Tokyo and Texas"]<- "Asia"
train7$from[train7$from=="Tokyo, Japan"]<- "Asia"
train7$from[train7$from=="SIngapore"]<- "Asia"


train7$from[train7$from=="Persia"]<- "Asia"
train7$from[train7$from=="philippines"]<- "Asia"
train7$from[train7$from=="Philippines"]<- "Asia"
train7$from[train7$from=="Russia/Germany"]<- "Asia"
train7$from[train7$from=="Shanghai, China"]<- "Asia"
train7$from[train7$from=="Siberia"]<- "Asia"
train7$from[train7$from=="Singapore"]<- "Asia"
train7$from[train7$from=="Uzbekistan"]<- "Asia"

train7$from[train7$from=="Yugoslavia"]<- "Asia"
train7$from[train7$from=="Yugoslavia"]<- "Asia"
train7$from[train7$from=="Yugoslavia"]<- "Asia"
train7$from[train7$from=="Born in Iran"]<- "Asia" 
train7$from[train7$from=="Australia"]<- "Asia" 


##남미
train7$from[train7$from=="Brazil"]<- "South America"
train7$from[train7$from=="Bogota, Colombia"]<- "South America"
train7$from[train7$from=="Argentina"]<- "South America"
train7$from[train7$from=="Chile"]<- "South America"
train7$from[train7$from=="Colombia"]<- "South America"
train7$from[train7$from=="Panama"]<- "South America"
train7$from[train7$from=="uruguay"]<- "South America"
train7$from[train7$from=="Uruguay"]<- "South America"
train7$from[train7$from=="Colombia"]<- "South America"
train7$from[train7$from=="Colombia, South America"]<- "South America"
train7$from[train7$from=="Costa Rica"]<- "South America"
train7$from[train7$from=="Mexico"]<- "South America"



train7$from<-as.factor(train7$from)
summary(train7$from)
table(train7$from)
str(train7$from)


# installing/loading the package:
 if(!require(installr)) {
+ install.packages("installr"); require(installr)}
# using the package:
updateR()
install.packages("installr")

install.packages("car")



