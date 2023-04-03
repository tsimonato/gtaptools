squeeze <-
  function(cmf_file,
           zip_file,
           add_files = NULL,
           output = F,
           bat = T) {
    
    #' @title Squeeze the simulation folder.
    #' @name squeeze
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
    
    main_ext <- c("tab",
                  "cmf",
                  "sti",
                  "bat")
    
    input_ext <- c("har",
                   "prm",
                   "shk",
                   "cls")
    
    output_ext <- c("sl4",
                    "upd",
                    "slc")
    
    # ext <- paste(paste(main_ext, input_ext, out_ext, sep = '|'), collapse = '|')
    
    get_paths <- function(ext) {
      files <- c()
      for (l in 1:length(cmf)) {
        if (grepl("(file|shock)\\s", cmf[l])) {
          path <- unlist(strsplit(cmf[l], '\"| |;'))
          ext <- paste(paste(ext, sep = "|"), collapse = "|")
          path <- path[grepl(paste0("\\w*\\.(", ext, ")\\b"), tolower(path))]
          files <- c(files, path)
        }
      }
      return(files)
    }
    
    input <- get_paths(input_ext)
    out <- get_paths(output_ext)
    files_list <- tolower(list.files(dirname(cmf_file)))
    main <- files_list[grepl(paste0(
      "\\w*\\.(",
      paste(main_ext, collapse = "|"),
      ")\\b|.*",
      add_files,
      ".*"
    ),
    files_list)]
    
    if (output) {
      files <- c(input, out, main)
    } else {
      files <- c(input, main)
    }
    
    cmf_name <- sub("\\..*$", "", basename(cmf_file))
    files <- lapply(files, function(x) gsub("<cmf>", cmf_name, x))
    files <- unlist(files)
    
    if (bat) {

      aux <- cmf[grepl(paste0("(aux |auxiliary ).*files.*=.*"), tolower(cmf))]
      aux <- unlist(strsplit(aux, '\"| |;'))
      aux <- tail(aux[aux!=""],n=1)
      
      if (is.null(aux)){
        aux = cmf_name
      }
      
      writeLines(
        paste0(
          'echo on
      REM this BAT runs TABLO and LTG for ', aux, '.TAB only IF NECESSARY 
      REM helper programs LATER.EXE and SETERR.EXE are used
      REM Check if EXE, AXS and AXT are later than TAB and STI; if so, skip
      LATER ', aux, '.tab ', aux, '.sti / ', aux, '.exe ', aux, '.axt ', aux, '.axs
      if errorlevel 1 goto skip
      REM One of TAB or STI is later than EXE, AXS and AXT, so rerun TABLO and LTG
      del ', aux, '.ax?
      del ', aux, '.for
      del ', aux, '.exe
      echo on
      tablo<', aux, '.sti  >tb', aux, '.log
      if errorlevel 1 goto error
      call ltg ', aux, '
      if errorlevel 1 goto error
      dir ', aux, '.exe
      echo SUCCESSFULLY COMPILED ', aux, '
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
      goto endbat
      :error
      echo off
      echo ###### ERROR: FAILED TO COMPILE ', aux, ' #####
      echo Check log file below
      dir tb', aux, '.log
      dir ', aux, '.inf
      echo Please press CTRL-C to terminate batch job
      pause
      goto endbat
      :skip
      seterr 0
      echo off
      echo COMPILE IS NOT NEEDED: ', aux, '.exe is later than ', aux, '.TAB
      dir/od ', aux, '.*
      ', aux, ' -cmf ', cmf_name, '.cmf
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
      :endbat
      :endbat
      echo on'),
      file.path(dirname(cmf_file), paste0('RUN_', cmf_name, '.bat'))
      )
      files = c(files, paste0('RUN_', cmf_name, '.bat'))
    }
    
    zip(file.path(dirname(cmf_file), paste0(zip_file, ".zip")),
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

