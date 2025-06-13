################################################################################
################################################################################


#                   -----STAGE 2.5: VIDEO FRAMES EXTRACTION-----


################################################################################
################################################################################

# THIS STAGE IS FOR VIDEO USERS ONLY

# MegaDetector v5 is only capable of handling images, and YOLOv8 training is performed only on pictures. Therefore, in this stage it is possible to convert videos to images to create the training dataset that will subsequently be passed to the MegaDetector, image augmentation and model training

# This stage works for mp4, avi and mov formats, and it works also when videos from different formats are available

# The frames are stored in a directory with the exact same structure of the directory containing the original videos
# Frames created are named as the original video and a suffix is added in the format _f plus a progressive number indicating the frame (e.g., _f1, _f2, ..., f_60 etc.)
# Additionally, the frames hold the date and time of the source video

################################################################################

library(av)
library(fs)
library(exiftoolr)
library(tools)
library(doParallel)
library(lubridate)
library(tidyverse)


# source(".../Supplementary 1 - Main_functions.R")
source(file.choose())

# Define the main directory where the original videos are stored
video_base  <- "C:/Users/petro/Desktop/cam_video"

# Deinfe and create the main directory where frames will be stored
frames_base <- "C:/Users/petro/Desktop/cam_frames"
dir_create(frames_base, recurse = TRUE)

# Find videos in the main directory (including subfolders)
video_files <- dir_ls(video_base, recurse = TRUE, regexp = "\\.(mp4|avi|mov)$")

# Use a Video version of extract_datetime_parallel function (V) to extract date and time metadata
# In this case, we used ModifyDateTime, but it depends on the camera trap model and manufacturer 

video_info <- foreach(i = 1:length(video_files), .combine = rbind) %dopar% {
  extract_datetime_parallelV(video_files[i])
}

# Now extract the frames and place them in the frames_base directory while recreating the same subfolder structure as video_base

for (vid in video_files) {
  # compute the relative subfolder under prova video
  sub <- path_dir(path_rel(vid, start = video_base))
  
  # create the exact same subfolder under prova frames
  target_dir <- path(frames_base, sub)
  
  extract_and_rename_frames(vid, target_dir, fps = 1)   # CHOOSE THE FRAMES PER SECOND TO EXTRACT HERE
}

# Now find videos in the main directory (including subfolders)
frame_files <- list.files(frames_base, pattern = "\\.jpg$", recursive = TRUE, full.names = TRUE)

# Now adjust the video and frames dataframes to match and associate the creation date and time of source videos to the 
# corresponding frames created

video_lookup <- video_info %>%
  mutate(video_file  = tools::file_path_sans_ext(File),                            # strip “.avi” (etc) → “vid1”
         subfolder   = basename(dirname(Address)),                                 # extract the camera subfolder in video_base
  ) %>%
  select(subfolder, video_file, DateTime)

frames_meta <- tibble(frame_path = frame_files) %>%
  mutate(file_only  = basename(frame_path),                                        # extract file names
         video_file = str_remove(tools::file_path_sans_ext(file_only),"_f\\d+$"),  # strip off "_fxxx.jpg" 
         frame_dir  = dirname(frame_path),                                         # extract storage directory of frames
         subfolder  = str_remove(frame_dir,paste0("^", frames_base, "/?")))        # derive the subfolder, should match video_lookup$subfolder

# Now join the two dataframes based on subfolder and video_file
# In fact, it is common to have camera trap subfolders having the videos/images with the identical names

metadata_corr <- frames_meta %>%
  left_join(video_lookup, by = c("subfolder", "video_file"))

# Finally, call exiftool to correct the metadata of the frames based on the creation date and time obtained from source videos 

metadata_corr %>%
  select(frame_path, DateTime) %>%       # keep only the columns you’ll map over
  pwalk(function(frame_path, DateTime) {
    exif_call(
      args = c(
        "-overwrite_original",
        paste0("-DateTimeOriginal=", DateTime),
        frame_path
      )
    )
    message("Stamped ", basename(frame_path), " → ", DateTime)
  })
