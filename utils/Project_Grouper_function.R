
# Install these packages if you don't have them.
if(!require(pacman)) install.packages("pacman")

pacman::p_load(readxl, data.table, tidyverse , stringr , ggplot2) # load and install if user doesnot have



#' Title Function to read Excel or CSV into a data frame
#'
#' @param file_path path bearing data to be used for grouping
#'
#' @returns a dataframe containing the data read from the uploaded file
#' 
read_student_file <- function(file_path) {
  # Detect file extension (lowercase)
  ext <- tools::file_ext(file_path) |> tolower()
  
  # Read based on extension
  if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(file_path, .name_repair = "minimal") |> 
      as.data.frame()
  } else if (ext == "csv") {
    df <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unsupported file type: ", ext)
  }
  
  return(df)
}



#' Group Students into Project Teams Based on CWA and Department of Choice
#'
#' This function reads two Excel files containing student academic information
#' (Index Number, CWA, Name) and department assignments, then filters students
#' from a specified department. It categorizes students into performance bands
#' based on their CWA, randomizes within each band, and assigns them to balanced
#' project groups. It also has decision or inference supporting graphs
#'
#' @param student_and_cwa Character string. Path to the Excel file containing
#' student index numbers, CWAs, and names.
#' @param student_and_department Character string. Path to the Excel file containing
#' student index numbers, names, and department assignments.
#' @param stud_per_grp Integer. The number of students to assign per group.
#' @param department Character string. The department name to filter students by.
#' @param lecturer_names Enter names of lecturers, comma-separated
#' @param dist_grp_to_lecturers Logical input as to whether students should be distributed to lecturers or not
#' 
#' @return A list with two elements:
#' A data frame of students assigned to groups.}
#' A data frame of students who could not be placed into any group.
#' 
#' Example usage
#' Group_project_students(
#'   student_and_cwa = "path/to/student_cwa.xlsx",
#'   student_and_department = "path/to/student_department.xlsx",
#'   stud_per_grp = 7,
#'   lecturer_names = c("SA","AWK","RA","BA" ),
#'   dist_grp_to_lecturers = TRUE,
#'   department = 'animal')
#'
#'
Group_project_students <- function(student_and_cwa,
                                   student_and_department,
                                   stud_per_grp,
                                   dist_grp_to_lecturers = FALSE,
                                   lecturer_names = NULL,
                                   department,
                                   project_year = "2024") {
  # Read data for student + cwa
  SC <- read_student_file(student_and_cwa)
  # student + department
  SD <- read_student_file(student_and_department)
  
  # Check if cwa, index number and name exists
  required <- c("INDEX NUMBER", "CWA", "NAME")
  if (!all(required %in% colnames(SC))) {
    stop("Couldn't find columns : INDEX NUMBER,CWA and NAME")
  }
  # Check if department , index number and name exists
  required_2 <- c("INDEX NUMBER", "NAME", "DEPARTMENT")
  
  if (!all(required_2 %in% colnames(SD))) {
    stop("Couldn't find columns : INDEX NUMBER,DEPARTMENT,NAME")
  }
  
  # Alter column names.
  colnames(SC) <- gsub(" ", replacement = "_", x = colnames(SC)) |> tolower()
  
  # Convert Index_No to integer and select just needed column.
  needed_columns <- grep(pattern = "index|cwa|name", x = colnames(SC), ignore.case = TRUE, value = TRUE)
  
  # Subset the needed column from the actual data.
  SC_filtered <- SC[needed_columns]
  
  # Force the needed columns to their various data types.
  # CWA + Name + index number
  SC_filtered[["index_number" ]] <- as.integer(SC_filtered[["index_number" ]])
  SC_filtered[["name"]] <- as.character(SC_filtered[["name"]])
  SC_filtered[["cwa"]] <- as.numeric(SC_filtered[["cwa"]])
  
  
  # Extract column names
  index_col <- grep("inde", x = colnames(SC_filtered), ignore.case = TRUE, value = TRUE)
  
  # Correctly format column names
  colnames(SD) <- gsub(" ", replacement = "_", x = colnames(SD)) |> tolower()
  
  # subset just index number and department.
  SD_filtered <- SD[grep("depa|ind", x = colnames(SD), ignore.case = TRUE, value = TRUE)]
  # Force the needed columns to their various data types.
  # Department + Name + index number
  SD_filtered[['index_number']] <- as.numeric(SD_filtered[['index_number']])
  department_col <- grep("depa", x = colnames(SD_filtered), ignore.case = TRUE, value = TRUE)
  # A graph to show the number of students who chose what department.
  # current_date <- Sys.Date() |> stringr::str_split(pattern = '-') |> unlist()
  # year <- current_date[1]
  year <- project_year
  
  plott <- ggplot2::ggplot(mapping = aes(x = department_col ,fill = department),data = SD_filtered ) +
    ggplot2::geom_bar(position = position_dodge(width = 1),width = 0.5)+
    ggplot2::geom_text(stat = "count",aes(label = after_stat(count)),
                       position = position_dodge(width = 1),vjust = -0.3)+
    ggplot2::theme_classic()+
    ggplot2::labs(
      x = 'DEPARTMENT',
      y =  'COUNT',
      title = paste("A Graph Showing Population of Students Under Departments in Year",year),
    ) +
    ggplot2::scale_fill_brewer(palette = 'Set1')+
    ggplot2::theme(axis.text.x = element_blank(),
                   axis.title = element_text(face = 'bold'),
                   plot.title = element_text(face = 'bold',hjust = 0.5) )
  
  # Merge it all together.
  complete_df <- merge.data.frame(x = SC_filtered, y = SD_filtered) # use the common colnames
  
  # Next plot to show the distribution of students within what classes are choosing this department
  plott2 <- ggplot2::ggplot(mapping = aes(x = department ,y = cwa ,colour  = department),data = complete_df ) +
    ggplot2::geom_point(position = position_jitter(width = 0.2),size = 3) +
    ggplot2::theme_classic()+
    ggplot2::labs(
      x = 'DEPARTMENT',
      y =  'CWA',
      title = paste("CWA Distribution of Students Across Departments in Year",year),
    ) +
    ggplot2::scale_color_brewer(palette = 'Set1')+
    ggplot2::theme(axis.text.x = element_blank(),
                   axis.title = element_text(face = 'bold'),
                   plot.title = element_text(face = 'bold',hjust = 0.5) )
  
  # Next draw a normal distribution graph
  plott3  <- ggplot2::ggplot(complete_df, aes(x = cwa)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "grey", color = "black", alpha = 0.7) +
    ggplot2::stat_function(fun = dnorm,
                           args = list(mean = mean(complete_df$cwa, na.rm = TRUE), sd = sd(complete_df$cwa, na.rm = TRUE)),
                           color = "red",linewidth = 1) +
    ggplot2::geom_vline(xintercept = mean(complete_df$cwa, na.rm = TRUE), linetype = "dashed", color = "blue", linewidth = 1) +
    ggplot2::annotate("text", x = (mean(complete_df$cwa, na.rm = TRUE) + 2.5), y = 0.02, label = "Mean", color = "blue", vjust = -1) +
    ggplot2::labs(title = paste("Normal Distribution of CWA Scores in Year",year),
                  x = "CWA",
                  y = "Density") +
    ggplot2::theme_classic()+
    ggplot2::theme(axis.title = element_text(face = 'bold'),
                   plot.title = element_text(face = 'bold',hjust = 0.5) )
  
  
  # Extract colnames for cwa
  cwa_col <- grep("cwa", x = colnames(complete_df), ignore.case = TRUE, value = TRUE)
  
  # Refilter based on specification of user
  # unique_depart <- unique(complete_df[[department_col]])
  # filterout  department
  complete_df <- complete_df[complete_df[department_col] == department, ]
  
  
  # After filtering out department. Take out department column.
  complete_df <- complete_df[!colnames(complete_df) %in% department_col]
  # Now we goup students into dist classes.
  # 70-100 , 60-69 , 50-59 , 40-49
  
  # Lets create a dataframe to store the partitioned students
  # Filter those with their cwa between 70 to 100
  dataframe_70_100 <- complete_df %>%
    filter(cwa >= 70)
  # randomize
  dataframe_70_100 <- slice_sample(.data = dataframe_70_100, n = nrow(dataframe_70_100), replace = FALSE)
  
  # Filter out those with cwa between 70 to 100
  dataframe_60_69 <- complete_df %>%
    filter(cwa >= 60 & cwa < 70)
  # randomize
  dataframe_60_69 <- slice_sample(.data = dataframe_60_69, n = nrow(dataframe_60_69), replace = FALSE)
  
  # Filter out those with cwa between 50 to 59
  dataframe_50_59 <- complete_df %>%
    filter(cwa >= 50 & cwa < 60)
  # randomize
  dataframe_50_59 <- slice_sample(.data = dataframe_50_59, n = nrow(dataframe_50_59), replace = FALSE)
  
  # Filter out those with cwa between 40 to 49
  dataframe_40_49 <- complete_df %>%
    filter(cwa >= 40 & cwa < 50)
  # randomize
  dataframe_40_49 <- slice_sample(.data = dataframe_40_49, n = nrow(dataframe_40_49), replace = FALSE) # randomise
  
  # Organize this into a dataframe thus the students falling under which category.
  statistic_df <- data.frame(
    CWA_Range = c('70 - 100','60 - 69','50 - 59','40 - 49'),
    Count = c(nrow(dataframe_70_100), nrow(dataframe_60_69),nrow(dataframe_50_59) ,nrow(dataframe_40_49))
  )
  # Now to grouping the students.
  nstd_per_grp <- stud_per_grp # user specifies the number of students per group.
  
  # Create empty data frames and store in a list.
  group_list <- list() # an empty list object
  success_grp <- (nrow(complete_df) / nstd_per_grp) |> trunc()
  
  for (i in seq_len(success_grp)) {
    Group <- rep(i, times = nstd_per_grp)
    group_list[[paste("Group", i, sep = "_")]] <- cbind(Group, as.data.frame(matrix(
      nrow = nstd_per_grp,
      ncol = 3,
      dimnames = list(NULL, c("Index_Number", "Name", "CWA"))
    )))
  }
  
  # bind rows.
  compact <- bind_rows(
    dataframe_70_100,
    dataframe_60_69,
    dataframe_50_59,
    dataframe_40_49
  )
  
  # for loop to populate the groups.
  counter <- 1
  for (row in 1:nstd_per_grp) {
    for (grp in names(group_list)) {
      if (counter <= nrow(compact)) {
        group_list[[grp]][row, 2:4] <- compact[counter, ]
        counter <- counter + 1
      }
    }
  }
  
  # find those that had no group.
  data_1 <- rbindlist(group_list) |> as.data.frame()
  left <- complete_df[!(complete_df[[index_col]] %in% data_1[[grep("index",
                                                                   x = colnames(data_1),
                                                                   ignore.case = TRUE,
                                                                   value = TRUE
  )]]), ]
  
  # Lets make sure though the user specifies that the groups should be a particular number 
  # we could fit the excess under under categories.
  #- sample groups to fit them.
  sample_grp <- sample(x = seq_len(length(group_list)),size = nrow(left),replace = TRUE)
  
  # Pull out the group from the list and row bind.
  for (h in seq_len(nrow(left))) {
    left_updated <- data.frame(Group = sample_grp[h] , left[h,] ) # add Group column to make rbind feasible
    #update colnmaes
    colnames(left_updated) <- colnames(data_1)
    group_list[[sample_grp[h]]] <- rbind(group_list[[sample_grp[h]]], left_updated)
  }
  
  # recombine list to dataframe
  data_01 <- rbindlist(group_list) |> as.data.frame()
  
  # Now return a table that would inform the user how many individuals per group.
  scores <- table(data_01['Group']) |> as.data.frame()
  
  # incase the user wants to share the groups for each lecturer.
  if(dist_grp_to_lecturers){
    # Validate lecturer input is not null
    if(length(lecturer_names) < 0){
      stop("Please enter a name of the lecturers to share groups to")
    }
    
    lect <- unlist(strsplit(x = lecturer_names ,split = ',')) |> trimws()
    # first lets find how many groups could fit the dataframe.
    r <- nrow(scores) %% length(lect)
    v <- length(lect) - r
    grp_to_share <- c(seq_len(nrow(scores)))
    
    # Random shuffle groups
    shuffled <- sample(grp_to_share)
    
    # Split into segments for each column
    split_groups <- split(shuffled, rep(lect, length.out = length(shuffled)))
    
    # Make a data frame (fill shorter columns with NA)
    max_len <- max(lengths(split_groups))
    dataa <- as.data.frame(lapply(split_groups, function(x) c(x, rep(NA, max_len - length(x)))))
    
    # filter out groups in a list.
    group_for_lect <- list()
    
    # for loop to filter things out.
    for (z in seq_len(length(lect))) {
      group_for_lect[[lect[z]]] <- data_01[data_01[['Group']] %in% dataa[[z]],]
    }
  }
  
  # Rerturn overall result
  return(list(Grouped_students = if(dist_grp_to_lecturers) group_for_lect else data_01, 
              Group_and_freq = scores,
              Bar_plot = plott,
              Scatter_plot = plott2,
              Distribution_plot = plott3,
              statistic_df = statistic_df
  )
  )
}





