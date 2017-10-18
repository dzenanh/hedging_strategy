# Create the object data using 5 random numbers
data <- rnorm(5)

# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-01-01"), length = 5, by = "days")

# Use xts() to create smith
smith <- xts(x = data, order.by = dates)

# Create bday (1899-05-08) using a POSIXct date class object
bday <- as.POSIXct("1899-05-08")

# Create hayek and add a new attribute called born
hayek <- xts(x = data, order.by = dates, born = bday)



temp_db_draw <- dbReadTable(db, "Stock_Information")
temp_db_draw$Pricing_Date <- as.Date(as.POSIXct(temp_db_draw$Pricing_Date))
temp_xts_draw <- xts(x = temp_db_draw[,4], order.by=temp_db_draw[,6])
plot(xtsq1)

