C  =====================================================================
C  pgm: SHPOUT .. Display "shefout" file
C
C  use:     CALL SHPOUT(LUOUT,LUDIS)
C
C   in: LUOUT ........ logical unit number of "shefout" file -INT
C   in: LUDIS ........ logical unit number of display - INT
C  =====================================================================
      SUBROUTINE SHPOUT(LUOUT,LUDIS)

      CHARACTER*8        ID,JID,PC
      CHARACTER*1        IQUAL
      CHARACTER*3        D1,D2,D3,D4,D5,D6
      INTEGER            LUOUT,LUDIS,IEND
      INTEGER            LDT(6),JDT(6),IDUR,IREV,ITIME
      DOUBLE PRECISION   VALU
      REAL               CODP

  200 FORMAT(A8,I4,5I2,I5,5I2,1X,A2,1X,A3,
     $       F10.3,1X,A1,F6.2,I5,I2,1X,A8,I1)

        IF (LUOUT.GE.0 .AND. LUDIS.GE.0) THEN

          REWIND (LUOUT,IOSTAT=IEND)

  100     IF (IEND .NE. 0) GOTO 110
            READ(LUOUT,IOSTAT=IEND) ID,LDT,JDT,PC(1:1),D1,PC(2:2),D2,
     $                         IDUR,PC(4:4),D3,PC(5:5),D4,PC(6:6),D5,
     $                         CODP,VALU,IQUAL,D6,IREV,JID,ITIME
            IF (IEND .EQ. 0) THEN
              WRITE(LUDIS,200,IOSTAT=IEND) ID,LDT,JDT,PC(1:2),
     $              PC(4:6),VALU,IQUAL,CODP,IDUR,IREV,JID,ITIME
            ENDIF
            GOTO 100
  110     CONTINUE

        ENDIF

      RETURN
      END
