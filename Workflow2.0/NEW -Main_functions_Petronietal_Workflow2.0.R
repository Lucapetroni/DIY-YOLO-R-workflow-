
#     - - - - - DEEP LEARNING FUNCTIONS - - - - -

################################################################################

# This script comes as Supplementary material for : 
# An ecologist-friendly R workflow for expediting species-level classification of camera trap images

# Function to copy and rename previously tagged images of the target species
  # this function uses the data contained in the .csv database to copy the defined number of images for each species (total_images_per_species) 
    # to a selected folder (img_tr_dir is the default value here)
  # The function balances among color and black/white images based on the values of sunset and sunrise provided 
    # In case there are enough pictures, the function randomly selects half color and half black/white images. When one of the two
    # groups (color or black/white) has less than half the defined number of images, it takes all the images from the group with less images, 
    # and randomly select the remaining ones from the other group .The copies are renamed based on the species_com_names and a progressive 
    # number is added

copy_and_rename_images <- function(species_data, species_name, species_com_name, dest_dir, total_images, sunset, sunrise) {
  # Split the species_name into multiple species names if it contains "|"
  split_species_names <- unlist(strsplit(species_name, " \\| "))
  
  # Initialize variables to collect data for all split species names
  all_species_data_d <- data.frame()
  all_species_data_n <- data.frame()
  
  # Loop through each split species_name
  for (split_species in split_species_names) {
    # Filter rows where species matches the given split_species and only a single species was detected
    species_data_filtered <- species_data %>%
      filter(Species == split_species & Species2 == "")
    
    # Divide the data into color and black/white images
    species_data_d <- species_data_filtered[species_data_filtered$Time <= sunset & species_data_filtered$Time >= sunrise, ]
    species_data_n <- species_data_filtered[species_data_filtered$Time > sunset | species_data_filtered$Time < sunrise, ]
    
    # Combine color and black/white data for all split species_names
    all_species_data_d <- rbind(all_species_data_d, species_data_d)
    all_species_data_n <- rbind(all_species_data_n, species_data_n)
  }
  
  # Calculate the counts of color and black/white images for all split species_names
  count_d <- nrow(all_species_data_d)
  count_n <- nrow(all_species_data_n)
  
  # Determine how many images to take from each dataset (col and b/w)
  if (min(count_d, count_n) >= total_images / 2) {
    # If the minimum value among count_d and count_n is >= half total_images for a given species,
    # select half from a datasets and half from the other one
    species_image_d <- sample(all_species_data_d$Address, total_images / 2)
    species_image_n <- sample(all_species_data_n$Address, total_images / 2)
  } else {
    # If the minimum value among count_d and count_n is < half total_images for a given species,
    # select all the images from the dataset with the minimum count and the remaining ones from the other dataset
    if (count_d <= count_n) {
      species_image_d <- all_species_data_d$Address
      species_image_n <- sample(all_species_data_n$Address, total_images - count_d)
    } else {
      species_image_d <- sample(all_species_data_d$Address, total_images - count_n)
      species_image_n <- all_species_data_n$Address
    }
  }
  
  # Combine daytime and nighttime images
  species_image <- c(species_image_d, species_image_n)
  
  # Shuffle the combined images
  species_image <- sample(species_image)
  
  # Initialize a counter for the progressive number
  existing_files <- list.files(dest_dir, full.names = TRUE)
  
  # Find the index of the current species name in the original species_names vector
  species_index <- which(species_names == species_name)
  
  # Use the corresponding common name from species_com_names
  common_name <- species_com_names[species_index]
  
  #Set starting progressive number
  progressive_number <- ifelse(length(existing_files[str_detect(existing_files, common_name)]) == 0, 1,
                               length(existing_files[str_detect(existing_files, common_name)]) + 1)
  
  # Loop through each image path and copy to the destination directory
  for (image_path in species_image) {
    if (file.exists(image_path)) {
      # Extract the file name from the image path
      file_name <- basename(image_path)
      
      file_extension <- tolower(tools::file_ext(image_path))
      
      # Find the index of the current species name in the original species_names vector
      species_index <- which(species_names == species_name)
      
      # Use the corresponding common name from species_com_names
      common_name <- species_com_names[species_index]
      
      # Construct the destination file path with progressive number
      destination_path <- file.path(dest_dir, paste0(common_name, " (", progressive_number, ")", ".", file_extension))
      
      # Copy the image to the destination directory
      file.copy(image_path, destination_path)
      
      cat("Copied:", image_path, "to", destination_path, "\n")
      
      # Increment the progressive number
      progressive_number <- progressive_number + 1
    } else {
      cat("File not found:", image_path, "\n")
    }
  }
}



#Function to determine the number of images of each target species in a selected folder  
 # this function simply lists all the files in a given directory and counts the ones adhering to a given pattern (the abbreviations of species 
   # common names, species_com_names, in this case)

get_image_number <- function(dest_dir, species_com_names) {
  # List all the files in the selected folder
  existing_files <- list.files(dest_dir, full.names = TRUE)
  
  # Define the pattern for the species names and progressive number
  pattern <- paste0(species_com_names)
  
  # Filter the existing files based on the desired pattern
  existing_files <- existing_files[str_detect(existing_files, pattern)]
  
  # Find the maximum N value
  last_n_used <- length(existing_files)
  return(last_n_used)
}



# Function to convert the coordinates of the bounding boxes obtained from the MegaDetector model (in xmin, ymin, width, height) in a format
  # compatible with YOLO model training (xcenter, ycenter, width, height). 
  # Specifically, the function calculates xcenter and ycenter.
calculate_yolo_coordinates <- function(row) {
  x_center <- row$bbox_1 + (row$bbox_3/ 2)  # x_center = (xmin + xmax) / 2
  y_center <- row$bbox_2 + (row$bbox_4/ 2) # y_center = (ymin + ymax) / 2
  return(paste(x_center, y_center, collapse = " "))
}



# Function to create the text files (labels) for each image, containing the YOLO-compatible coordinates of the bounding boxes for each subject 
  # and the numeric values associated to the species contained based on image name and data.yaml file 

create_text_file <- function(image_name) {
  # Create a data frame for the current image
  image_data <- labels_def[labels_def$images.file == image_name, ]
  file_name <- tools::file_path_sans_ext(basename(image_name))
  
  # Define the text file name based on the image name
  txt_file_name <- paste0(file_name, ".txt")
  
  # Open the text file for writing
  txt_file <- file(txt_file_name, "w")
  
  # Write the category and bbox values to the text file
  for (i in 1:nrow(image_data)) {
    cat(image_data$category[i], 
        image_data$x_center[i], 
        image_data$y_center[i], 
        image_data$bbox_3[i], 
        image_data$bbox_4[i], 
        sep = " ", 
        file = txt_file)
    cat("\n", file = txt_file)  # Add a newline
  }
  
  # Close the text file
  close(txt_file)
  
  cat("Created text file:", txt_file_name, "\n")
}


# Function to perform image augmentation on the training pictures for the species with less than the chosen num_images
  # this function selects the species/categories having less than the chosen num_images. Then, it applies a transformation selected 
  # by the user. When less than half the num_images are available for a given species/category, the transformation is performed on all the images;
  # otherwise, it is applied to randomly selected images until the num_images is reached. 
  # Augmented images posses a suffix to identify the transformation used. 

perform_augmentation <- function(data, image_dir, numbers, transformation, dest_dir, suffix) {
  selected_images <- character(0)
  
  for (i in seq_along(data$Species)) {
    species <- data$Species[i]
    total <- num_images
    threshold <- total / 2
    image_files <- list.files(image_dir, full.names = TRUE)
    
    # Use grepl for case-insensitive comparison
    species_match <- grepl(species, image_files, ignore.case = TRUE)
    
    if (numbers[i] < threshold) {
      selected_images <- c(selected_images, image_files[species_match])
    } else {
      N <- total - numbers[i]
      random_selection <- sample(image_files[species_match], N)
      selected_images <- c(selected_images, random_selection)
    }
  }
  
  for (file in selected_images) {
    img <- image_read(file)
    img_transformed <- transformation(img)
    
    file_name <- tools::file_path_sans_ext(basename(file))
    destination_file <- file.path(dest_dir, paste0(file_name, suffix, ".png"))
    
    image_write(img_transformed, path = destination_file)
  }
}



# move_random_images_and_text: this function moves images and their corresponding label file from their original folders to other ones, 
  # and allows to move different number of images for different species/categories (including Background)

move_random_images_and_text <- function(image_folder, text_folder, output_image_folder, output_text_folder, categories, num_images_per_category) {
  
  # List all image files in the image folder
  image_files <- list.files(image_folder, pattern = "*.jpg", full.names = TRUE)
  
  # Create a list to store selected image and text file pairs for each category
  selected_pairs <- list()
  
  # Iterate through each animal category
  for (category in categories) {
    # Filter image files for the current category
    animal_image_files <- basename(image_files)[str_detect(basename(image_files), paste0("^", category))]
    
    # Randomly select num_images_per_category images for the current category
    selected_images <- sample(animal_image_files, num_images_per_category[category])
    
    # Copy selected images to the output folder
    for (image_file in selected_images) {
      file.rename(file.path(image_folder, image_file), file.path(output_image_folder, image_file))
    }
    
    # Copy corresponding text files
    for (image_file in selected_images) {
      base_name <- tools::file_path_sans_ext(basename(image_file))
      text_file <- file.path(text_folder, paste0(base_name, ".txt"))
      if (file.exists(text_file)) {
        file.rename(text_file, file.path(output_text_folder, basename(text_file)))
        selected_pairs[[base_name]] <- c(image_file, text_file)
      }
    }
  }
  
  # Return a list of selected image and text file pairs for each category
  return(selected_pairs)
}



# process_detect_folder: the labels generated in inference tasks contain coordinates for the bounding boxes of all the objects identified, 
  # that are sorted in decreasing order of confidence. The function creates a dataframe with file name, file path, species id (the one with the 
  # highest confidence level), the confidence level for the species identified, species2 id and species3 id (the second and third species identified,
  # if any, again based on the confidence level of the predictions) with their corresponding confidence values, and finally, the number of instances
  # each of the three species was detected by the model. 

  # In our workflow each image is attributed to the species identified with the highest confidence level. In fact, although it is possible to have 
  # more than one species in a single picture/frame, it is uncommon to have multiple species in the same event, thus the detection of multiple species
  # by the model can be the result of a misidentification

#################################################################################

                          ###### UPDATE 2.0 ######

##### COUNTING THE NUMBER OF INDIVIDUALS FOR EACH CATEGORY

#################################################################################

# The function was modified to count the number of individuals for each category: species, species2 and species3 - still distinguished by confidence level.
# The number of individuals reported corresponds to the number of bounding boxes identified for each category


process_detect_folder <- function(detect_folder, categories) {
  # Get the site name from the folder name
  site_name <- detect_folder
  
  # Create a list of file paths for the "*.txt" files in the labels subfolder
  label_files <- list.files(file.path(detect_folder), pattern = "\\.txt$", full.names = TRUE)
  
  # Process each label file
  for (label_file in label_files) {
    # Read all lines from the label file
    lines <- readLines(label_file)
    
    # Skip empty files or cases where no detections are present
    if (length(lines) == 0) next
    
    # Initialize an empty list to store category-confidence pairs
    detections <- list()
    
    # Iterate through each line and extract category and confidence values
    for (line in lines) {
      fields <- strsplit(line, " ")[[1]]
      numeric_category <- as.numeric(fields[1])
      numeric_conf <- as.numeric(fields[6])
      detections <- append(detections, list(c(category = numeric_category, conf = numeric_conf)))
    }
    
    # Convert the list to a data frame
    detections_df <- do.call(rbind, detections) %>%
      as.data.frame() %>%
      setNames(c("Category", "Confidence"))
    
    # Get the maximum confidence for each unique category
    max_conf_by_category <- aggregate(Confidence ~ Category, data = detections_df, max)
    
    # Count the number of instances for each category
    category_counts <- as.data.frame(table(detections_df$Category))
    colnames(category_counts) <- c("Category", "Count")
    
    # Merge counts with max_conf_by_category
    max_conf_by_category <- merge(max_conf_by_category, category_counts, by = "Category", all.x = TRUE)
    
    # Sort by confidence (highest to lowest)
    max_conf_by_category <- max_conf_by_category[order(-max_conf_by_category$Confidence), ]
    
    # Convert numeric categories to text using the mapping
    text_categories <- categories[max_conf_by_category$Category + 1]  # +1 because R uses 1-based indexing
    
    # Ensure the length of categories and counts is handled correctly
    species <- ifelse(length(text_categories) >= 1, text_categories[1], NA)
    species2 <- ifelse(length(text_categories) >= 2, text_categories[2], NA)
    species3 <- ifelse(length(text_categories) >= 3, text_categories[3], NA)
    
    conf_species <- ifelse(nrow(max_conf_by_category) >= 1, max_conf_by_category$Confidence[1], NA)
    conf_species2 <- ifelse(nrow(max_conf_by_category) >= 2, max_conf_by_category$Confidence[2], NA)
    conf_species3 <- ifelse(nrow(max_conf_by_category) >= 3, max_conf_by_category$Confidence[3], NA)
    
    nind <- ifelse(nrow(max_conf_by_category) >= 1, max_conf_by_category$Count[1], 0)
    nind2 <- ifelse(nrow(max_conf_by_category) >= 2, max_conf_by_category$Count[2], 0)
    nind3 <- ifelse(nrow(max_conf_by_category) >= 3, max_conf_by_category$Count[3], 0)
    
    # Extract the filename and remove the ".txt" extension
    filename <- basename(label_file)
    filename_no_ext <- str_remove(filename, "\\.txt")
    
    # Create a data frame with the top three species, confidence levels, and counts
    data <- data.frame(
      RelativePath = site_name,
      File = paste0(filename_no_ext, ".JPG"),
      Species = species,
      Species2 = species2,
      Species3 = species3,
      Conf_Species = conf_species,
      Conf_Species2 = conf_species2,
      Conf_Species3 = conf_species3,
      NumberOfIndividuals = nind,
      Nind2 = nind2,
      Nind3 = nind3
    )
    
    # Append the data to the result data frame
    inference_df <<- bind_rows(inference_df, data)
  }
}



#extract_datetime_parallel: this function extracts the timestamp from PICTURES metadata (date and time when the PICTURES were taken)

extract_datetime_parallel <- function(file) {
  library(exiftoolr)
  exif_data <- exif_read(file)
  
  if (!is.null(exif_data) && "DateTimeOriginal" %in% names(exif_data)) {
    datetime_original <- exif_data$DateTimeOriginal
    return(data.frame(File = basename(file), Address = file, DateTime = datetime_original))
  }
  
  return(NULL)
}


#extract_datetime_parallel: this function extracts the timestamp from VIDEO metadata (date and time when the VIDEOS were taken)
 # In this case, we used FileModifyDate, but it depends on the camera model and manufacturer
 # Try to use exif_read(file) on a single video "file" and see which of the metadata columns is the correct one
 # Then, replace FileModifyDate below accordingly 

extract_datetime_parallelV <- function(file) {
  library(exiftoolr)
  exif_data <- exif_read(file)
  
  if (!is.null(exif_data) && "FileModifyDate" %in% names(exif_data)) {
    datetime_original <- exif_data$FileModifyDate
    return(data.frame(File = basename(file), Address = file, DateTime = datetime_original))
  }
  
  return(NULL)
}


#################################################################################

                         ###### UPDATE 2.0 ######

##### EXTRACTING FRAMES FROM VIDEOS

#################################################################################

# extract_and_rename_frames: this function uses the path to the directory where videos are stored and extracts N frames per second, 
  # where N is defined by users.
  # The function then renames the frames based on the source video name and adds a suffix to identify the frames with a progressive number "_fxxx.jgp"

# Extract from one video into a shared folder, but only rename the NEWLY created frames
extract_and_rename_frames <- function(video_path, output_dir, fps = 1) {
  # ensure the folder exists
  dir_create(output_dir, recurse = TRUE)
  
  # check which .jpgs are already there
  existing <- dir_ls(output_dir, regexp = "\\.jpg$")
  
  # extract N frames based on user-established frames per second (fps)
  av_video_images(video_path, output_dir, format = "jpg", fps = fps)
  
  # list again and get just the new files
  all_jpgs   <- dir_ls(output_dir, regexp = "\\.jpg$")
  new_frames <- setdiff(all_jpgs, existing)
  
  # build a prefix from the video basename
  prefix <- file_path_sans_ext(path_file(video_path))
  
  # rename the frames (only the new ones)
  for (i in seq_along(new_frames)) {
    new_name <- sprintf("%s_f%d.jpg", prefix, i)         # the suffix for frames is _fxxx.jpg, where xxx is a sequential number
    file_move(new_frames[i], path(output_dir, new_name))
  }
}
