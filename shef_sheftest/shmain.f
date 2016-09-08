C  =====================================================================
C  pgm: SHMAIN .. Sample mainline for the "shefpars" program
C
C  use:     SHMAIN
C
C   in: <terminal> ... user is prompted for the name of the input
C   in:                shef message file - CHAR*128
C
C  cmt: Input data to be decoded:          <input>    (text file)
C  cmt: Input parameter file:              SHEFPARM   (text file)
C
C  cmt: Output filtered data in binary:    shefout    (binary file)
C  cmt: Output filtered data in text:      display    (text file)
C  cmt: Output errors and messages:        errors     (text file)
C
C  cmt: Note, the input file can be a full pathname (128 char limit),
C  cmt:       the other files are all in the current directory.
C  =====================================================================
      PROGRAM MAIN

      EXTERNAL       SHDRIV,SHERRK,SHPOUT

      LOGICAL        EXS
      INTEGER        IERR,NWAR,NERR,LWAR,LERR,NUM
      INTEGER        IC,IC1,IC2,IC3,IC4,IC5
      INTEGER        LUINP,LUOUT,LUPAR,LUERR,LUCPY,LUDIS
      CHARACTER*128  FILINP

      DATA    LUINP,LUOUT,LUPAR,LUERR,LUCPY,LUDIS / 10,11,12,13,13,15 /

C                   Enter the pathname of an input shef message file

        READ(*,'(A)') FILINP
        INQUIRE(FILE=FILINP, IOSTAT=IERR, EXIST=EXS)
        IF (IERR.EQ.0 .AND. EXS) THEN

C                   Open files needed by SHDRIV for these unit numbers:
C                     LUINP ... input shef message file
C                     LUOUT ... binary output file for decoded mesages
C                     LUPAR ... SHEFPARM resource file (in current dir)
C                     LUERR ... output error messages and summary
C                     LUCPY ... output file to contain a copy of all
C                               input messages (in this program LUCPY
C                               is set to the same number as LUERR so
C                               that all messages are sent to the error
C                               output file ... it could be set to -1
C                               if no copy of the messages is needed)
C                     LUDIS ... set to a text output file that will
C                               be used by subroutine SHPOUT which
C                               reads the binary shefout file and
C                               writes the data in text format in this
C                               display file

          OPEN(LUINP,FILE=FILINP,IOSTAT=IC1,STATUS='OLD')
          OPEN(LUOUT,FILE='shefout',IOSTAT=IC2,FORM='UNFORMATTED',
     $         STATUS='NEW')
          OPEN(LUPAR,FILE='SHEFPARM',IOSTAT=IC3,STATUS='OLD')
          OPEN(LUERR,FILE='error',IOSTAT=IC4,STATUS='NEW')
          OPEN(LUDIS,FILE='display',IOSTAT=IC5,STATUS='NEW')

C                   Call parsing program SHDRIV, then output display
C                   using program SHPOUT

          IC = IC1+IC2+IC4+IC5
          IF (IC .EQ. 0) THEN
            CALL SHDRIV(LUINP,LUOUT,LUPAR,LUERR,LUCPY)
            CALL SHPOUT(LUOUT,LUDIS)
          ENDIF

C                   Close shef files

          CLOSE(LUINP)
          CLOSE(LUOUT)
          CLOSE(LUPAR)
          CLOSE(LUERR)
          CLOSE(LUCPY)
          CLOSE(LUDIS)

C                   Get number of warning, errors; and last warning
C                   number and error number with subroutine SHERRK
C                     (note, this is not necessary since the error
C                     message file will also contain the number of
C                     errors and warnings, but is included here as
C                     an example of how a calling program can obtain
C                     error information directly)

          NUM = 0
          CALL SHERRK('N',NUM,NWAR,NERR)
          CALL SHERRK('L',NUM,LWAR,LERR)
          WRITE(*,'(/'' num-wars:'',I4,''  num-errs:'',I4  )') NWAR,NERR
          WRITE(*,'( '' last-war:'',I4,''  last-err:'',I4,/)') LWAR,LERR

        ENDIF

      END
