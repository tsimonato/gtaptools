      echo on
      REM this BAT runs TABLO and LTG for oranig03.TAB only IF NECESSARY 
      REM helper programs LATER.EXE and SETERR.EXE are used
      REM Check if EXE, AXS and AXT are later than TAB and STI; if so, skip
      LATER oranig03.tab oranig03.sti / oranig03.exe oranig03.axt oranig03.axs
      if errorlevel 1 goto skip
      REM One of TAB or STI is later than EXE, AXS and AXT, so rerun TABLO and LTG
      del oranig03.ax?
      del oranig03.for
      del oranig03.exe
      echo on
      tablo<oranig03.sti  >tboranig03.log
      if errorlevel 1 goto error
      call ltg oranig03
      if errorlevel 1 goto error
      dir oranig03.exe
      echo SUCCESSFULLY COMPILED oranig03
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
      echo ###### ERROR: FAILED TO COMPILE oranig03 #####
      echo Check log file below
      dir tboranig03.log
      dir oranig03.inf
      echo Please press CTRL-C to terminate batch job
      pause
      :skip
      seterr 0
      echo off
      echo COMPILE IS NOT NEEDED: oranig03.exe is later than oranig03.TAB


      :simulation
      dir/od oranig03.*
      echo on
      oranig03 -cmf ORNG03SR.cmf
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
