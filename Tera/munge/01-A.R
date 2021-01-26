#Auto load and renaming the data files
tasks = task.x.y
cps = application.checkpoints

#removing duplicate entries
cps = cps %>% distinct()
gpu = gpu %>% distinct()

# merging the checkpoints and task_x_y data on taskId
cps_tasks = merge(x = cps, y = tasks, by = "taskId", all = TRUE)

# order check points tasks data
cps_tasks = cps_tasks[order(cps_tasks$hostname, cps_tasks$timestamp),]

# splitting the time stamp to calculate the run times of the taks
prep_data = cbind(cps_tasks, read.table(text = as.character(cps_tasks$timestamp), sep = "T"))
prep_data = cbind(prep_data, read.table(text = as.character(prep_data$V2), sep = "."))
prep_data = prep_data[,-c(12,14)]
prep_data = cbind(prep_data, read.table(text = as.character(prep_data$V1.1), sep = ":"))
prep_data = prep_data[,-12]
prep_data$seconds = prep_data$V1.1*60*60+prep_data$V2*60+prep_data$V3
prep_data = prep_data[,-c(12,13,14)]
prep_data$V1 = as.Date(prep_data$V1)
prep_data$seconds = as.numeric(prep_data$seconds)
names(prep_data)[11] = "date"

prep_data_piv = prep_data[,c(1,4,5,12)]

prep_data_piv = prep_data_piv %>% pivot_wider(names_from = eventType, values_from = seconds)
prep_data_piv = as.data.frame(prep_data_piv)

prep_data_piv$runtime = prep_data_piv$STOP-prep_data_piv$START

## pivot data after joining back with checkpoints_tasks
prep_data_piv2 = full_join(prep_data_piv, cps_tasks, by = c("eventName", "taskId"))
prep_data_piv2 = prep_data_piv2[,c(1,7,2,11,12,13,5)]
prep_data_piv2 = prep_data_piv2 %>% distinct()
