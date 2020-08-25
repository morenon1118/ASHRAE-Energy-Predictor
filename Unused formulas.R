#Plot under certain criteria
# with(bld.avg[bld.avg$avg.meter.read < 4000,], plot(building_id, avg.meter.read))


#standard ggplot formula
# ggplot(data = train.0, aes(x = log(meter_reading + 1), y = time.day)) + geom_point()

#Subset df based on column criteria
# bld.1099 <- subset(train.final, building_id == 1099)




#DAY OF THE WEEK BY METER READING
ggplot(train.final, aes(x = time.day, y = log(meter_reading + 1))) +
  geom_boxplot(alpha = 0.5, colour = "blue") +
  coord_flip()
#Weekdays generally higher energy usage, Weekends less w/ sunday the smallest

#METER READING THROUGH TIME
ggplot(train.final, aes(x = timestamp, y = log(meter_reading + 1))) +
  geom_point() +
  ggtitle("Time by energy usage")

#BUILDING DETAILS BY METER READING
ggplot(train.final, aes(x = square_feet, y = log(meter_reading + 1))) +
  geom_point() +
  ggtitle("Square feet by energy usage")
ggplot(train.final, aes(x = year_built, y = log(meter_reading + 1))) +
  geom_point() +
  ggtitle("Building age by energy usage")

#WEATHER BY METER READING PLOTS
ggplot(train.final, aes(x = air_temperature, y = log(meter_reading + 1))) +
  geom_point() +
  ggtitle("air temp by energy usage")
ggplot(train.final, aes(x = cloud_coverage, y = log(meter_reading + 1))) +
  geom_point() +
  ggtitle("cloud coverage by energy usage")
ggplot(train.final, aes(x = dew_temperature, y = log(meter_reading + 1))) +
  geom_point() +
  ggtitle("dew temp by energy usage")
ggplot(train.final, aes(x = precip_depth_1_hr, y = log(meter_reading + 1))) +
  geom_point() +
  ggtitle("precipritation depth by energy usage")
ggplot(train.final, aes(x = sea_lvl_pressure, y = log(meter_reading + 1))) +
  geom_point() +
  ggtitle("sea level pressure by energy usage")
