squeeze_sim <-
  function(cmf_file,
           zip_file,
           add_files = NULL,
           output = F,
           bat = T) {
    
    #' @title Squeeze the simulation folder.
    #' @name squeeze_sim
    #' @description Squeeze the simulation folder by selecting only the essential files and putting them in a .zip file. It also creates a .bat file that makes it easy to run the simulation later.The files that are included are those specified in the .cmf file and that have the extension .tab, .cmf, .sti, .bat, .har, .prm, .shk, .cls, and in the case output = F, .sl4, .upd, .slc.
    #' @param cmf_file Path to .cmf file which manages the simulation.
    #' @param zip_file Name of the .zip file that will be created.
    #' @param add_files Vector with the names or extensions of the files that will also be included in the .zip file in addition to the files mentioned in the description.
    #' @param output Includes simulation output files (default = F).
    #' @param bat Create a batch file to compile (if necessary) and run the simulation. (default = V). It is necessary to have Gempack installed.
    #'
    #' @export
    
    zip_file <- sub("\\..*$", "", zip_file)
    
    cmf <- tolower(readChar(cmf_file, file.info(cmf_file)$size))
    
    cmf <- gsub("!.*?(!|\n)", "", cmf, perl = TRUE)
    
    cmf <- unlist(strsplit(cmf, "\\r\\n"))
    
    main_ext <- c(".tab",
                  ".cmf",
                  ".sti",
                  ".bat")
    
    input_ext <- c(".har",
                   ".prm",
                   ".shk",
                   ".cls")
    
    output_ext <- c(".sl4",
                    ".upd",
                    ".slc")
    
    # ext <- paste(paste(main_ext, input_ext, out_ext, sep = '|'), collapse = '|')
    
    get_paths <- function(ext) {
      files <- c()
      for (l in 1:length(cmf)) {
        if (grepl("(file |shock ).*=.*", cmf[l])) {
          path <- unlist(strsplit(cmf[l], '\"| |;'))
          ext <- paste(paste(ext, sep = "|"), collapse = "|")
          path <- path[grepl(paste0("\\w*(", ext, ")\\b"), tolower(path))]
          files <- c(files, path)
        } else if (grepl("(solution file).*=.*", cmf[l])) {
          path <- unlist(strsplit(cmf[l], '\"| |;'))
          path <- tail(path[path!=""], n=1)
          files <- c(files, path)
        }
      }
      return(files)
    }
    
    input <- get_paths(input_ext)
    out <- get_paths(c(output_ext, "/"))
    files_list <- tolower(list.files(dirname(cmf_file), recursive = T))
    main <- files_list[grepl(paste0(
      "\\w*(",
      paste(c(main_ext,add_files), collapse = "|"),
      ")\\b"
    ),
    files_list)]
    folder_solutions = grep(".*\\..*", files, value = TRUE, invert = T)
    solutions <- files_list[grepl(paste0(
      "\\w*(",
      paste(folder_solutions, collapse = "|"),
      ")\\b"
    ),
    files_list)]
    
    if (output) {
      files <- c(input, out, solutions, main)
    } else {
      writeLines("Just a temp file used while building the zip file. It can be deleted without problems.", 
                 file.path(dirname(cmf_file), dirname(folder_out), "temp.txt"))
      files <- c(input, file.path(dirname(folder_out), "temp.txt"), main)
    }

    cmf_name <- sub("\\..*$", "", basename(cmf_file))
    files <- lapply(files, function(x) gsub("<cmf>", cmf_name, x))
    files <- unlist(files)
    
    if (bat) {
      aux <- cmf[grepl(paste0("(aux |auxiliary ).*files.*=.*"), tolower(cmf))]
      aux <- unlist(strsplit(aux, '\"| |;'))
      aux <- tail(aux[aux!=""],n=1)
      
      if (is.null(aux) | !(paste0(aux, ".tab") %in% files)){
        aux = cmf_name
        
      } else if (!(paste0(cmf_name, ".tab") %in% files)){
        aux = files[grepl(paste0(
          "\\w*(",
          ".tab",
          ")\\b"
        ),
        files)]
        aux = basename(aux[1])
      }
      
      writeLines(
        paste0(
      '
      echo on
      REM this BAT runs TABLO and LTG for termdyn_hou.TAB only IF NECESSARY 
      REM helper programs LATER.EXE and SETERR.EXE are used
      REM Check if EXE, AXS and AXT are later than TAB and STI; if so, skip
      LATER termdyn_hou.tab termdyn_hou.sti / termdyn_hou.exe termdyn_hou.axt termdyn_hou.axs
      if errorlevel 1 goto skip
      REM One of TAB or STI is later than EXE, AXS and AXT, so rerun TABLO and LTG
      del termdyn_hou.ax?
      del termdyn_hou.for
      del termdyn_hou.exe
      echo on
      tablo<termdyn_hou.sti  >tbtermdyn_hou.log
      if errorlevel 1 goto error
      call ltg termdyn_hou
      if errorlevel 1 goto error
      dir termdyn_hou.exe
      echo SUCCESSFULLY COMPILED termdyn_hou
      echo off
      REM clean up junk files
      del *.for
      del *.lib
      del *.mod
      del modtable.txt
      del opt
      del opt90
      del opt95
      seterr 0
      goto simulation
      :error
      echo off
      echo ###### ERROR: FAILED TO COMPILE termdyn_hou #####
      echo Check log file below
      dir tbtermdyn_hou.log
      dir termdyn_hou.inf
      echo Please press CTRL-C to terminate batch job
      pause
      :skip
      seterr 0
      echo off
      echo COMPILE IS NOT NEEDED: termdyn_hou.exe is later than termdyn_hou.TAB


      :simulation
      dir/od termdyn_hou.*
      echo on
      termdyn_hou -cmf termdyn_hou.cmf
      if errorlevel 1 goto error
      dir/od *.har
      echo BATCH JOB SUCCESSFUL
      dir/od *.sl4
      goto endbat
      :error
      echo off
      echo ###### ERROR: BATCH JOB FAILED #####
      echo Check log file; most recent is listed last
      dir/od *.log
      :again
      echo Please press CTRL-C to terminate batch job
      pause > nul
      goto again
      :endbat'),
      file.path(dirname(cmf_file), paste0('RUN_', cmf_name, '.bat'))
      )
      files = c(files, paste0('RUN_', cmf_name, '.bat'))
    }
    
    new_zip <- file.path(dirname(cmf_file), paste0(zip_file, ".zip"))
    unlink(new_zip)
    zip(new_zip,
        files = file.path(dirname(cmf_file), files))
  }



# squeeze(cmf_file = "data/test/teste/termdyn_hou.cmf",
#         zip_file = "novo.zip",
#         add_files = "standard",
#         output = F)
# 
# 
# squeeze(cmf_file = "data/test/gtapV7_condensed/UniEUSSA.cmf",
#         zip_file = "novo.zip",
#         add_files = "standard",
#         output = F)
# 
# gtaptools::squeeze(cmf_file = "data/test/teste/termdyn_hou.cmf",
#                    zip_file = "novo.zip",
#                    add_files = "standard",
#                    output = F)

