##Random Walk
# Generate a RW model using arima.sim
random_walk <- arima.sim(model = list(order=c(0,1,0)), n = 100)
plot.ts(random_walk)
adf.test(random_walk)
random_walk_diff <- diff(random_walk)
plot.ts(random_walk_diff)




# Generate a RW model with a drift uing arima.sim
rw_drift <- arima.sim(model = list(order=c(0,1,0)), n = 100, mean = 1)
ts.plot(rw_drift)

rw_drift_diff <- diff(rw_drift)
ts.plot(rw_drift_diff)


# Difference your random_walk data
rw_diff <- diff(random_walk)
ts.plot(rw_diff)
model_wn <- arima(rw_diff, order = c(0, 0, 0))
int_wn <- model_wn$coef

ts.plot(random_walk)
abline(0, int_wn)


# Use arima.sim() to generate WN data
white_noise <- arima.sim(model=list(order=c(0,0,0)), n=100)
plot.ts(white_noise)
# Use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)
plot.ts(random_walk)
# Use arima.sim() to generate WN drift data
wn_drift <- arima.sim(model=list(order=c(0,0,0)), n=100, mean=0.4)
plot.ts(wn_drift)
# Use cumsum() to convert your WN drift data to RW
rw_drift <- cumsum(wn_drift)
plot.ts(rw_drift, col="blue")

# Plot all four data objects
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))


f#Deterministic trend