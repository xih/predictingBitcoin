library(pracma)

#Number of tweets on that day(mentioniing bitcoin)
# count the number of unique users that post on that day
# Y is the change in bitcoin. 



bitcoin = read.csv(file = "/Users/dennis/Documents/STAT154/STAT154_FINALPROJECT/anvitapandit-btcpredictor-35c640d3d8fb/okcoin5s.csv", sep=",", header = TRUE)
tweets = read.csv(file = "/Users/dennis/Documents/STAT154/STAT154_FINALPROJECT/twitter_bitcoin.csv", header = TRUE)

price_data = read.csv(file = "/Users/dennis/Documents/STAT154/STAT154_FINALPROJECT/market-price", sep = " ", header = FALSE)


btc_mentions = vector()
for (i in 1:length(tweets$body)) {
  btc_mentions[i] = length(gregexpr("[Bb]itcoin", strsplit(as.character(tweets$body[i]), " "))[[1]])
}
bitcoin_tweets = data.frame(tweets$actor_postedtime, tweets$body, btc_mentions)


dates = substr(as.character(bitcoin_tweets$tweets.actor_postedtime), 1, 10)
date = as.Date(dates)

bitcoin_tweets$date = date
bitcoin_tweets = bitcoin_tweets[order(bitcoin_tweets$date), ]

bitcoin_dates = vector()
bitcoin_dates = as.Date(price_data$V1, format = "%d/%m/%Y")

historic_bitcoin = data.frame(bitcoin_prices, bitcoin_dates)

bitcoin_prices = vector()
for (i in 1:length(price_data$V2)) {
  bitcoin_prices[i] = as.numeric(strsplit(as.character(price_data$V2), ",")[[i]][2])
}





intersection = vector()
for ( i in 1:length(bitcoin_dates)) {
  intersection = append(x = intersection, values = which(bitcoin_dates[i] == bitcoin_tweets$date))
}


i = 1
j = 1
index = 1
price_differential = vector(length = 2000)
while (i <  length(historic_bitcoin$bitcoin_dates) && j < length(bitcoin_tweets$date)) { 
  if (historic_bitcoin$bitcoin_dates[i] < bitcoin_tweets$date[j]) {
    i = i +1
  }
  else if (historic_bitcoin$bitcoin_dates[i] > bitcoin_tweets$date[j]) {
    j = j + 1
  }
  else {
    price_differential[index] = historic_bitcoin$bitcoin_prices[i+1] -historic_bitcoin$bitcoin_prices[i]
    index = index + 1
    i = i+1
    j = j+1
  }
}

predictors = data.frame(bitcoin_tweets$btc_mentions, bitcoin_tweets$date)
names(predictors) = c("btc_mentions", "date")




results = vector(length = length(unique(predictors$date)))
index = 1
results[1] = predictors$btc_mentions[1]
for (i in 2:length(predictors$date)) {
  if (length(which(predictors$date[i] == bitcoin_dates)) != 0) {
    if (predictors$date[i] == predictors$date[i-1]) {
      results[index] = results[index] + predictors$date[i]
    }
    else {
      index = index + 1
      results[index] = predictors$btc_mentions[i]
    }
  }
}

results = results[1:1833]
price_differential = price_differential[1:1833]

lm.fit = lm(price_differential~results)
summary(lm.fit)

plot(results, price_differential, xlab = "Number of Bitcoin Tweets on a Particular Date", ylab = "Difference in Price between Dates")
abline(lm.fit, lwd = 3, col = "red")






















prices = bitcoin[,2]
askVolume = bitcoin[,3]
bidVolume = bitcoin[,4]

prices = prices[seq(1,length(prices),2)]
askVolume = askVolume[seq(1,length(askVolume),2)]
bidVolume = bidVolume[seq(1,length(bidVolume),2)]

prices1 = prices[1:20000] 
prices2 = prices[20001:40000]
prices3 = prices[40001:length(prices)]

#create list of all 720*10s, 360*10s and 180*10s intervals
#each item is (interval of prices, NEXT TEN SEC interval price change)

priceDiff = diff(prices)
validIntSize = length(prices1)-750
interval720s = matrix(0,validIntSize,720+1)
interval360s = matrix(0,validIntSize,360+1)
interval180s = matrix(0,validIntSize,180+1)

for (i in 1:validIntSize) {
  interval180s[i,] = c(prices1[i:(i+179)],priceDiff[i+179])
  interval360s[i,] = c(prices1[i:(i+359)],priceDiff[i+359])
  interval720s[i,] = c(prices1[i:(i+719)],priceDiff[i+719])
}

clusters = 20

kmeans180s = kmeans(interval180s,clusters)$centers
kmeans360s =kmeans(interval360s,clusters)$centers
kmeans720s =kmeans(interval720s,clusters)$centers






for (i in 1:clusters) {
  kmeans180s[i,1:180] = (kmeans180s[i,1:180] - mean(kmeans180s[i,1:180]) )/sd(kmeans180s[i,1:180])
  kmeans360s[i,1:360] = (kmeans360s[i,1:180] - mean(kmeans360s[i,1:180]) )/sd(kmeans360s[i,1:180])
  kmeans720s[i,1:720] = (kmeans720s[i,1:180] - mean(kmeans720s[i,1:180]) )/sd(kmeans720s[i,1:180])
}

#finished clustering and normalizing

regressorX = matrix(0,length(prices2)-750-1,4)
regressorY = matrix(0,1, (length(prices2)-750-1))

for (i in 750:(length(prices2)-2)) {
  price180 = prices2[(i-179):i]      
  price360 = prices2[(i-359):i]      
  price720 = prices2[(i-719):i]
  
  dp1 = bayesian(price180, kmeans180s)
  dp2 = bayesian(price360, kmeans360s)
  dp3 = bayesian(price720, kmeans720s)
  
  r = (bidVolume[i]-askVolume[i])/(bidVolume[i]+askVolume[i])
  if (i == 19999) {
    print("HELLO")
  }
  regressorX[(i-749),] = c(dp1,dp2,dp3,r)
  
  regressorY[,(i-749)] = c(prices2[i+1] - prices2[i])
}

ret_theta = trainRegressor(regressorX, t(regressorY), 1)
theta = ret_theta[,1]
theta0 = ret_theta[,2]
m = length(prices1) + length(prices2)

brtrade(prices3, kmeans180s,kmeans360s,kmeans720s,theta,theta0,bidVolume[m:length(bidVolume)],askVolume[m:length(askVolume)])

predicted = vector()
brtrade = function(prices, kmeans180s,kmeans360s,kmeans720s,theta, theta0 ,bidVolume,askVolume) {
  threshold = .6
  position = 0
  bank = 0
  error = 0
  
  predicted = vector()
  for (t in 750:(length(prices)-2)) {
    price180 = prices[(t-179):t]
    price360 = prices[(t-359):t]      
    price720 = prices[(t-719):t]
    
    dp1 = bayesian(price180,kmeans180s)
    dp2 = bayesian(price360,kmeans360s)
    dp3 = bayesian(price720,kmeans720s)
    
    r = (bidVolume[t]-askVolume[t])/(bidVolume[t]+askVolume[t])
    dp = theta0 +  theta[1] * dp1 + theta[2] * dp2 + theta[3] * dp3 + theta[4] * r
    
    error = error + abs(prices[t+1]- (prices[t]+dp))
    
    append(predicted, prices[t]+dp)
    
    if (dp > threshold && position <= 0) {
      position = position + 1
      bank = bank- prices[t]
    }
    if (dp < -threshold && position >= 0) {
      position = position -1
      bank = bank + prices(t)
    }
  }
  error = error/(length(prices)-1)
  
  if (position == 1){
   bank = bank + prices(length(prices)) 
  }
  if (position == -1) {
    bank = bank - prices(length(prices))
  }
  return(cbind(bank,error))
}






#S is a matrix
#x is the vector of current empirical prices
bayesian = function(x, S) {
  dpj = 1
  c = -1
  
  num = 0.0
  den = 0.0
  for (i in 1:20) {
    cutS = S[i, 1:length(x)]
    distance = exp(c * vecsim(x, cutS))
    num = num + S[i,length(x)]*distance
    den = den + distance
  }
  if (den != 0) {
    dpj = num/den
  }
  return(dpj)
}

#Find the similarity between the 2 vectors
vecsim = function(x,y) {
  if (length(x) != length(y)) {
    print("These vectors are different lengths.")
  }
  num = sum((x-mean(x)) * (y-mean(y)))
  den = length(x) * sd(x) * sd(y)
  
  if (den == 0) {
    s = num
  }
  else {
    s = num/den
  }
  return(s)
}

trainRegressor = function(X, y, gamma) {
  d = ncol(X)
  n = nrow(X) 
  
  last_col = matrix(1,n,1) 
  Xp = cbind(X, last_col) #[X, last_col]
  if (gamma > 0) {
    I_np1 = diag(1, d+1) #I_np1 = speye(d+1);
    I_n = I_np1[1:nrow(I_np1)-1, ] #I_n = I_np1(1:end-1, :);
    Xp = rbind(Xp, sqrt(gamma) * I_n) #Xp = [Xp; sqrt(gamma)*I_n]
    y = rbind(y, matrix(0,d,1)) #y = [y; zeros(d,1)]
  }
  
  theta_theta_0 = mldivide(Xp, y) #theta_theta_0 = Xp \ y;
  if (length(theta_theta_0) == d+1) { #assert(length(theta_theta_0 ) == d+1);
    #print("lengths are equal")
  }
  theta = theta_theta_0[1:d] #theta = theta_theta_0(1:d);
  theta_0 = theta_theta_0[length(theta_theta_0)] #theta_0 = theta_theta_0(end);

  return(cbind(theta, theta_0))
}













