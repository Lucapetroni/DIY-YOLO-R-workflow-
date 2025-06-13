This branch contains two R script files:

1) New_Main_functions_Petronietal_Workflow2.0: this is the Main_Functions file defining all the functions necessary to implement the Workflow
   In the latest 2.0 version we added two new functions:
   1.1: extract_datetime_parallelV, which extracts metadata from VIDEO files (similarly to extract_datetime_parallel)
   1.2: extract_and_rename_frames, which extracts frames from VIDEO files and renames them accordingly 
   Moreover, we provided an updated version of one of the core functions for stage 10:
   1.3: process_detect_folder, which now is capable of returning the Number of Individuals as obtained from YOLO inference tasks

Further details on each function are provided along with each function.

2) Video_To_Frames_Petronietal_Workflow2.0: this script will function as Stage 2 or Stage 2.5 in the main Workflow and contains the necessary code 
   to extract still frames from camera trap video files
   The code allows users to extract frames, place them in a directory that mirrors the structure of the directory containing the original videos, and
   also assigns the correct metadata to the generated frames


   