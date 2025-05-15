DECLARE SUB DoAltBar ()
DECLARE SUB ermes (tp$, sw%)
DECLARE FUNCTION thatch$ ()
DECLARE SUB MouseArea (y1%, y2%, min%, max%)
DECLARE SUB dowindo (num%, cl1%, cl2%)
DECLARE SUB mouseget (mouseb%)
DECLARE SUB mousepointer (OnOff%)
DECLARE SUB soundfx (sw%)
DECLARE SUB ScrnScroll (ULRow%, ULColumn%, LRRow%, LRColumn%, NumOfLines%, sw%)
DECLARE SUB PutText2 (text$, TextColor%, BackColor%)
DECLARE SUB disp (title$, cl%, row%)
DECLARE SUB QSORT (first%, Last%)
DECLARE SUB playvoc (file$, snd%)
DECLARE SUB tinyfont (hor%, ver%, inline$, CLR%)
DECLARE SUB cubeit (h%, v%, WD%, HT%, C1%, C2%, FILD%)
DECLARE SUB icon (h%, v%, WD%, HT%, C1%, C2%, FILD%, OUTIN%)
DECLARE SUB smallprint (spx%, spy%, sp$, spcolor%)
DECLARE FUNCTION SubtractDate$ (DateString$, NumberOfDays%)
DECLARE FUNCTION JulianDate! (DateString$)
DECLARE FUNCTION JulianToDate$ (Julian!)
DECLARE FUNCTION CurDir$ ()
DECLARE FUNCTION Iinkey$ (max%, row%, col%, fore%, sw%)
DECLARE FUNCTION DIR$ (FileSpec$)
DECLARE FUNCTION Exists% (filen$)
DECLARE FUNCTION BufIn$ (filen$, done%, reading%)
DECLARE FUNCTION DRAWSTR% (mode%, strg$, x0%, y0%, fg%, bg%, gap%)
DECLARE FUNCTION ival% (number$)
DECLARE FUNCTION ReadChar% CDECL ALIAS "_readchar" ()
DECLARE FUNCTION DataWaiting% CDECL ALIAS "_data_waiting" ()

TYPE RegTypeX
  ax    AS INTEGER
  bx    AS INTEGER
  cx    AS INTEGER
  dx    AS INTEGER
  bp    AS INTEGER
  si    AS INTEGER
  di    AS INTEGER
  flags AS INTEGER
  DS    AS INTEGER
  es    AS INTEGER
END TYPE
DIM SHARED inregs AS RegTypeX, outregs AS RegTypeX
DECLARE SUB InterruptX (intnum AS INTEGER, inregs AS RegTypeX, outregs AS RegTypeX)
CONST dos = &H21
CONST SetDTA = &H1A00, FindFirst = &H4E00, FindNext = &H4F00
'
DEFINT A-Z
'$DYNAMIC
COMMON SHARED nomouse%, mouseh%, mousev%, aattack
COMMON SHARED uinput$(), macro$(), snd%, maxline, line$, rest
COMMON SHARED help()  AS STRING * 76

DATA Thirsty,Hungry,Poisoned,Vitality,When Lose,Mana,When Have

REM $STATIC
SUB atakmenu (anum, atak$(), menusav(), await, regit)
mousepointer 0
cubeit 200, 0, 240, 198, 15, 8, 7
icon 207, 6, 227, 186, 8, 15, 7, 0
icon 256, 10, 128, 15, 8, 15, 7, 0
nz = DRAWSTR%(2, "N", 230, 36, 4, 7, 8)
nz = DRAWSTR%(2, "umber of attacks", 240, 36, 8, 7, 8)
nz = DRAWSTR%(2, "S", 246, 50, 4, 7, 8)
nz = DRAWSTR%(2, "econds to rest", 256, 50, 8, 7, 8)
nz = DRAWSTR%(2, "M", 218, 180, 4, 7, 8)
nz = DRAWSTR%(2, "onitor health via:", 228, 180, 8, 7, 8)
GOSUB redis
smallprint 271, 21, "ATTACK MENU", 0
smallprint 270, 20, "ATTACK MENU", 14
MouseArea 30, 190, 230, 400
mousepointer 1
'
DO
  clm = 0
  DO
    choice$ = UCASE$(INKEY$)
    mouseget mouseb
    IF mouseb = 2 THEN choice$ = CHR$(27)
    IF mouseb = 1 THEN
      SELECT CASE mousev
        CASE IS < 48: choice$ = "n"
        CASE IS < 65: choice$ = "s"
        CASE IS < 83: choice$ = "1"
        CASE IS < 97: choice$ = "2"
        CASE IS < 111: choice$ = "3"
        CASE IS < 125: choice$ = "4"
        CASE IS < 139: choice$ = "5"
        CASE IS < 153: choice$ = "6"
        CASE IS < 167: choice$ = "7"
        CASE IS > 175: choice$ = "m"
      END SELECT
      WHILE mouseb%
        mouseget mouseb
      WEND
    END IF
  LOOP UNTIL choice$ <> ""
  IF choice$ = CHR$(13) OR choice$ = CHR$(27) THEN EXIT DO
  '
  choice$ = UCASE$(choice$)
  x = INSTR("1234567NSM", choice$)
  y = 35: z = 58
  IF x <> 0 THEN
    IF x > 7 THEN
      IF x = 8 THEN anum = anum + 1: IF anum = 8 THEN anum = 1
      IF x = 9 THEN await = await + 1: IF await = 16 THEN await = 1
      IF x = 10 THEN
        SELECT CASE uinput$(2)
          CASE "HE": uinput$(2) = "GR"
          CASE "GR": uinput$(2) = "NONE"
          CASE "NONE": uinput$(2) = "HE"
        END SELECT
        IF regit = 0 AND uinput$(2) <> "HE" THEN
          uinput$(2) = "HE"
          ermes "Sorry, only registered users can toggle this feature.", 1
        END IF
      END IF
      mousepointer 0
      GOSUB redis
      mousepointer 1
    ELSE
      IF x <= anum THEN
        mousepointer 0
        atak$(x) = "              "
        nz = DRAWSTR%(2, atak$(x), 275, z + (x * 14), 15, 9, 8)
        atak$(x) = ""
        DO
          choice$ = INKEY$
          IF choice$ = CHR$(13) THEN EXIT DO
          IF choice$ = CHR$(8) THEN
            atak$(x) = LEFT$(atak$(x), LEN(atak$(x)) - 1)
            nz = DRAWSTR%(2, atak$(x) + " ", 275, z + (x * 14), 15, 9, 8)
          ELSE
            atak$(x) = atak$(x) + choice$
          END IF
          nz = DRAWSTR%(2, atak$(x), 275, z + (x * 14), 15, 9, 8)
        LOOP UNTIL LEN(atak$(x)) > 13
        atak$(x) = RTRIM$(atak$(x)) + " "
        nz = DRAWSTR%(2, atak$(x), 275, z + (x * 14), 15, 9, 8)
        mousepointer 1
      END IF
    END IF
  END IF
LOOP
'
mousepointer 0
EXIT SUB
'
redis:
icon 385, 33, 28, 12, 8, 15, 9, 0
icon 385, 48, 28, 12, 8, 15, 9, 0
icon 377, 177, 44, 12, 8, 15, 9, 0
LINE (230, 64)-(400, 168), 7, BF
z = 70
FOR x = 1 TO anum
  nz = DRAWSTR%(2, STR$(x), 237, z + 2, 4, 7, 8)
  icon 265, z, 132, 12, 8, 15, 9, 0
  nz = DRAWSTR%(2, atak$(x), 275, z + 2, 15, 9, 8)
  z = z + 14
NEXT x
nz = DRAWSTR%(2, STR$(anum), 388, 36, 15, 9, 8)
IF await > 9 THEN z = 392 ELSE z = 396
nz = DRAWSTR%(2, LTRIM$(STR$(await)), z, 50, 15, 9, 8)
nz = DRAWSTR%(2, uinput$(2), 384, 180, 15, 9, 8)
RETURN
END SUB

FUNCTION BufIn$ (filen$, done, reading) STATIC
IF NOT reading THEN            'if the first time through
  reading = -1                 'show that we're now reading
  done = 0                     'clear Done just in case
  CR = 0                       'no return found yet.
  CR$ = CHR$(13)               'define for speed later
  filenum = FREEFILE           'open the file
  OPEN filen$ FOR BINARY AS #filenum
  Remaining& = LOF(filenum)    'byte count to be read
  BufSize = 4096               'bytes to read each pass
  buffer$ = SPACE$(BufSize)    'assume BufSize bytes
END IF

'---- This is the main outer loop.
DO WHILE Remaining&              'while more in the file
  IF CR = 0 THEN                 'if no Return was found
    IF Remaining& < BufSize THEN 'read only what remains
      BufSize = Remaining&       'resize the buffer
      IF BufSize < 1 THEN EXIT DO'possible only if EOF 26
      buffer$ = SPACE$(BufSize)  'create the file buffer
    END IF
    GET #filenum, , buffer$      'read a block
    BufPos = 1                   'start at the beginning
  END IF                         '  of that block
  DO                                 'walk through buffer
    CR = INSTR(BufPos, buffer$, CR$) 'look for a Return
    IF CR THEN                       'we found one
      SaveCR = CR                    'save where
      BufIn$ = MID$(buffer$, BufPos, CR - BufPos)
      BufPos = CR + 2                'skip inevitable LF
      EXIT FUNCTION                  'all done for now
    ELSE                             'back up in the file
      '---- If we reached the end of the file and no 13
      '     was found, return what remains in the string.
      IF SEEK(filenum) >= LOF(filenum) THEN
        Output$ = MID$(buffer$, SaveCR + 2)
        '---- Trap a trailing CHR$(26) EOF marker.
        IF RIGHT$(Output$, 1) = CHR$(26) THEN
          Output$ = LEFT$(Output$, LEN(Output$) - 1)
        END IF
        BufIn$ = Output$             'assign the function
        Remaining& = BufSize         'set to fall out
        EXIT DO                      'and exit now
      END IF
      Slop = BufSize - SaveCR - 1    'calc buffer excess
      Remaining& = Remaining& + Slop 'calc file excess
      SEEK #filenum, SEEK(filenum) - Slop  'seek to start
    END IF
  LOOP WHILE CR                'while more in buffer
  Remaining& = Remaining& - BufSize
LOOP
done = 1: reading = 0: CLOSE #filenum: buffer$ = ""
END FUNCTION

SUB Credits
COLOR 11, 0
PRINT CHR$(201) + STRING$(78, CHR$(196)) + CHR$(187)
LOCATE 22, 1: PRINT CHR$(200) + STRING$(78, CHR$(196)) + CHR$(188)
LOCATE 2, 28
COLOR 4: PRINT "-";
COLOR 12: PRINT "=";
COLOR 6: PRINT "ð";
COLOR 14: PRINT "[ ";
COLOR 15: PRINT "TermTA CREDITS";
COLOR 14: PRINT " ]";
COLOR 6: PRINT "ð";
COLOR 12: PRINT "=";
COLOR 4: PRINT "-"
'
COLOR 14, 0
LOCATE 4, 1
PRINT "      Program Language Used: "
PRINT "                Programmers: "
PRINT : PRINT
LOCATE 8, 23: PRINT "Where To Find The Latest Releases"
LOCATE 17, 22: PRINT "How To Reach Me Via E-Mail Or Phone"
'
COLOR 15, 0
LOCATE 4, 30: PRINT "QuickBasic 4.5 and Assembler"
LOCATE 5, 30: PRINT "Will Boyett"
LOCATE 6, 30: PRINT "Jeff Sumberg (Communications - QBSerial)"

LOCATE 9, 3: PRINT "For info on latest version, Internet users can: FINGER TERMS@BBS.ANNEX.COM"
LOCATE 11, 22: PRINT "1-213-227-7127  or  1-213-221-0172"
LOCATE 12, 3: PRINT "Log on with the name: *WILLY BOY SOFTWARE to gain instant access for d/l."
LOCATE 14, 14: PRINT "Telnet to:  204.74.67.1    or Dial:  1-818-779-5600"
LOCATE 15, 10: PRINT "Log on with the name: BOYETT to gain instant access for d/l."
LOCATE 18, 26: PRINT "Internet:   wboyett@annex.com"
LOCATE 19, 26: PRINT "MajorNet:   Will Boyett@anx"
LOCATE 20, 20: PRINT "TA Conferences:   Majornet"
LOCATE 21, 29: PRINT "Voice:   1-310-924-4184"
END SUB

SUB Crypt (buffer$, key$, start)
'encrypt text strings and data files
BufLen = LEN(buffer$)
KeyLen = LEN(key$)
IF start = 0 OR start > KeyLen THEN
  start = 1
END IF
FOR I = 1 TO BufLen
  MID$(buffer$, I, 1) = CHR$(ASC(MID$(buffer$, I, 1)) XOR (ASC(MID$(key$, start, 1)) XOR (ASC(MID$(key$, 1, 1)) * start)) AND &HFF)
  IF start < KeyLen THEN
    MID$(key$, start, 1) = CHR$((ASC(MID$(key$, start, 1)) + ASC(MID$(key$, start + 1, 1))) AND &HFF)
    start = start + 1
  ELSE
    MID$(key$, start, 1) = CHR$((ASC(MID$(key$, start, 1)) + ASC(MID$(key$, 1, 1))) AND &HFF)
    start = 1
  END IF
NEXT
END SUB

FUNCTION DIR$ (FileSpec$) STATIC
DIM dta AS STRING * 44 ', inregs AS RegTypeX
Null$ = CHR$(0)
'-----  Set up our own DTA so we don't destroy COMMAND$
inregs.ax = SetDTA                    'Set DTA function
inregs.dx = VARPTR(dta)               'DS:DX points to our DTA
inregs.DS = -1                        'Use current value for DS
InterruptX dos, inregs, inregs          'Do the interrupt
'-----  Check to see if this is First or Next
IF LEN(FileSpec$) THEN              'FileSpec$ isn't null, so FindFirst
  FileSpecZ$ = FileSpec$ + Null$   'Make FileSpec$ into an ASCIIZ string
  inregs.ax = FindFirst              'Perform a FindFirst
  inregs.cx = 0                      'Only look for normal files
  inregs.dx = SADD(FileSpecZ$)       'DS:DX points to ASCIIZ file
  inregs.DS = -1                     'Use current DS
ELSE                                'We have a null FileSpec$,
  inregs.ax = FindNext               'so FindNext
END IF
InterruptX dos, inregs, inregs          'Do the interrupt
'-----  Return file name or null
IF inregs.flags AND 1 THEN            'No files found
  DIR$ = ""                        'Return null string
ELSE
  Null = INSTR(31, dta, Null$)     'Get the filename found
  DIR$ = MID$(dta, 31, Null - 30)  'It's an ASCIIZ string starting
END IF                              'at offset 30 of the DTA
END FUNCTION

SUB disp (title$, cl, row)
y = (81 - LEN(title$)) / 2
COLOR cl: LOCATE row, y: PRINT title$
END SUB

SUB doprint (ch0$, line$, fore0%, back0%, crai%, row%, col%)
IF ch0$ = CHR$(8) THEN
  row% = CSRLIN: col% = POS(0)
  IF col% <> 1 THEN col% = col% - 1: ch0$ = " "
  IF line$ <> "" THEN line$ = LEFT$(line$, LEN(line$) - 1)
  hor% = col% * 8 - 8: ver% = row% * 8 - 8
  nz = DRAWSTR%(0, ch0$, hor%, ver%, fore0%, back0%, 8)
ELSE
  IF LEN(line$) > 79 THEN
    IF POS(0) = 1 THEN
      ScrnScroll 1, 1, maxline, 80, 1, 0
    END IF
  END IF
  line$ = line$ + ch0$
  'IF crai = 0 THEN          '#how we turn stuff off
    row% = CSRLIN: col% = POS(0)
    IF row% > maxline THEN row% = maxline
    ver% = row% * 8 - 8
    hor% = col% * 8 - 8
    nz = DRAWSTR%(0, ch0$, hor%, ver%, fore0%, back0%, 8)
    col% = col% + 1
  'END IF
END IF
IF col% = 81 THEN row = row + 1: col = 1
END SUB

SUB edittext (rw, clm, which, min, max)
vert = max * 8
text$ = macro$(which)
DO
  choice$ = INKEY$
  SELECT CASE choice$
  CASE CHR$(8)                           'backspace
    rw1 = rw * 8 - 8
    LINE (35, rw1)-(vert, rw1 + 7), 1, BF
    IF clm = min THEN
      BEEP
      clm = clm + 1
    ELSE
      text$ = LEFT$(text$, clm - 7) + RIGHT$(text$, (LEN(text$) + 6) - clm)
    END IF
    clm = clm - 1
    LOCATE rw, min: PutText2 text$, 14, 1
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE CHR$(0) + "M"                        'right
    z = LEN(text$) + 6
    IF clm < z THEN
      LOCATE rw, clm: PutText2 "Û", 2, 1
      clm = clm + 1
      IF clm > max THEN clm = max: BEEP
      LOCATE rw, clm: PutText2 "Û", 2, 1
    END IF
  CASE CHR$(0) + "K"                        'left
    LOCATE rw, clm: PutText2 "Û", 2, 1
    clm = clm - 1
    IF clm < min THEN clm = min: BEEP
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE CHR$(0) + "G"                        'home
    LOCATE rw, clm: PutText2 "Û", 2, 1
    clm = 6
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE CHR$(0) + "O"                        'end
    LOCATE rw, clm: PutText2 "Û", 2, 1
    clm = LEN(text$) + 6
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE ELSE
    IF choice$ > CHR$(31) AND choice$ < CHR$(126) THEN
      IF LEN(text$) + 6 > max THEN
        BEEP
      ELSE
      rw1 = rw * 8 - 8                               'if entering text
      LINE (35, rw1)-(vert, rw1 + 7), 1, BF
      z = (LEN(text$) + 6) - clm
      IF z < 1 THEN temp$ = "" ELSE temp$ = RIGHT$(text$, z)
      text$ = LEFT$(text$, clm - 6) + choice$ + temp$
      LOCATE rw, min: PutText2 text$, 14, 1
      clm = clm + 1
      IF clm > max THEN clm = max: BEEP
      LOCATE rw, clm: PutText2 "Û", 2, 1
      END IF
    END IF
    IF choice$ = CHR$(0) + "S" AND ((LEN(text$) + 5) - clm) > -1 THEN
      rw1 = rw * 8 - 8                             'if deleting
      LINE (35, rw1)-(vert, rw1 + 7), 1, BF
      text$ = LEFT$(text$, clm - 6) + RIGHT$(text$, (LEN(text$) + 5) - clm)
      LOCATE rw, min: PutText2 text$, 14, 1
      LOCATE rw, clm: PutText2 "Û", 2, 1
    END IF
  END SELECT
LOOP UNTIL choice$ = CHR$(13)
LOCATE rw, clm: PutText2 "Û", 2, 1
macro$(which) = text$
END SUB

SUB ermes (tp$, sw)
mousepointer 0
IF snd = 1 THEN sw = 1: IF sw = 1 THEN PLAY "l16 b g"
LINE (4, 336)-(634, 344), 9, BF
x = (640 - (LEN(tp$) * 8)) \ 2
nz = DRAWSTR%(2, tp$, x, 337, 14, 9, 8)
SLEEP 3
LINE (4, 336)-(634, 344), 9, BF
mousepointer 1
END SUB

SUB healmenu (atak$(), awhen, ahave)
mousepointer 0
cubeit 179, 0, 280, 199, 15, 8, 7
icon 184, 6, 270, 187, 8, 15, 7, 0
icon 219, 10, 200, 18, 8, 15, 7, 0
GOSUB reddis
smallprint 265, 23, "RESTORE - HEAL", 0
smallprint 264, 22, "RESTORE - HEAL", 14
MouseArea 30, 190, 230, 400
mousepointer 1
'
DO
  GOSUB gchoice
  IF choice$ = CHR$(13) OR choice$ = CHR$(27) THEN EXIT DO
  '
  y = 35: z = 58
  x = INSTR("1234567", choice$)
  IF x <> 0 THEN
    mousepointer 0
    x = x + 7
    nz = DRAWSTR%(2, "               ", 304, (z + (x * 14) - 98), 15, 9, 8)
    atak$(x) = ""
    DO
      choice$ = INKEY$
      IF choice$ = CHR$(13) THEN EXIT DO
      IF choice$ = CHR$(8) THEN
        atak$(x) = LEFT$(atak$(x), LEN(atak$(x)) - 1)
        nz = DRAWSTR%(2, atak$(x) + " ", 304, (z + (x * 14) - 98), 15, 9, 8)
      ELSE
        atak$(x) = atak$(x) + choice$
      END IF
      nz = DRAWSTR%(2, atak$(x), 304, (z + (x * 14) - 98), 15, 9, 8)
    LOOP UNTIL LEN(atak$(x)) > 13
    nz = DRAWSTR%(2, atak$(x), 304, (z + (x * 14) - 98), 15, 9, 8)
    mousepointer 1
  END IF
LOOP
mousepointer 0
awhen = VAL(atak$(12))
ahave = VAL(atak$(14))
EXIT SUB
'
reddis:
RESTORE
z = 70
atak$(12) = LTRIM$(STR$(awhen))
atak$(14) = LTRIM$(STR$(ahave))
FOR x = 1 TO 7
  READ temp$
  IF x = 5 OR x = 7 THEN w = 7 ELSE w = 8
  y = 285 - (LEN(temp$) * w)
  nz = DRAWSTR%(2, temp$, y, z + 2, 0, 7, w)
  nz = DRAWSTR%(2, STR$(x), 188, z + 2, 4, 7, 8)
  icon 294, z, 146, 12, 8, 15, 9, 0
  nz = DRAWSTR%(2, atak$(x + 7), 304, z + 2, 15, 9, 8)
  z = z + 14
NEXT x
RETURN
'
gchoice:
  DO
    choice$ = UCASE$(INKEY$)
    mouseget mouseb
    IF mouseb = 2 THEN choice$ = CHR$(27)
    IF mouseb = 1 THEN
      SELECT CASE mousev
        CASE 65 TO 82: choice$ = "1"
        CASE 83 TO 96: choice$ = "2"
        CASE IS < 111: choice$ = "3"
        CASE IS < 125: choice$ = "4"
        CASE IS < 139: choice$ = "5"
        CASE IS < 153: choice$ = "6"
        CASE IS < 167: choice$ = "7"
      END SELECT
      WHILE mouseb%
        mouseget mouseb
      WEND
    END IF
  LOOP UNTIL choice$ <> ""
RETURN
END SUB

SUB hilite (temp$, fore, back, high, many)
LINE (4, 336)-(634, 344), 9, BF
hor = (640 - ((LEN(temp$) - many) * 8)) / 2
FOR x = 1 TO many
 z = INSTR(temp$, "&")
 nz = DRAWSTR%(0, LEFT$(temp$, z - 1), hor, 337, fore, back, 8)
 hor = hor + LEN((LEFT$(temp$, z - 1))) * 8
 nz = DRAWSTR%(0, MID$(temp$, z + 1, 1), hor, 337, high, back, 8)
 hor = hor + 8
 temp$ = MID$(temp$, z + 2)
NEXT x
nz = DRAWSTR%(0, temp$, hor, 337, fore, back, 8)
END SUB

FUNCTION Iinkey$ (max%, row%, col%, fore, sw)
CONST YES = 1, NO = 0, enter = &HD, bs = 8
LOCATE row%, col%
COLOR fore
DO
  DO
    k$ = INKEY$
    mouseget mouseb
  LOOP UNTIL k$ <> "" OR mouseb = 2
  IF mouseb = 2 THEN k$ = CHR$(13)
    WHILE mouseb%
      mouseget mouseb
    WEND
  IF LEN(k$) = 2 THEN
    Extend% = YES
    k% = ASC(RIGHT$(k$, 1))
  ELSE
    Extend% = NO
    k% = ASC(k$)
  END IF
  IF (Extend% = NO) AND (k% = enter) THEN
    Iinkey$ = I$
    EXIT FUNCTION
  ELSEIF (Extend% = NO) AND (k% = bs) AND (Le% > 0) AND (CurPos% > 0) THEN
    Work1$ = LEFT$(I$, CurPos% - 1)
    Work2$ = RIGHT$(I$, Le% - CurPos%)
    I$ = Work1$ + Work2$
    CurPos% = CurPos% - 1
    Le% = Le% - 1
    LOCATE row%, col% ', 1
    IF sw = 456 THEN LINE (sw, 302)-(480, 312), 3, BF ELSE LINE (sw, 335)-(634, 345), 9, BF
    PRINT I$ + SPACE$(1);
    LOCATE row%, col% + CurPos%
  ELSEIF (Extend% = NO) AND (k% > 31) AND (k% < 127) AND (Le% < max%) THEN
    Work1$ = LEFT$(I$, CurPos%)
    Work2$ = RIGHT$(I$, Le% - CurPos%)
    I$ = Work1$ + CHR$(k%) + Work2$
    Le% = Le% + 1
    CurPos% = CurPos% + 1
    LOCATE row%, col% ', 1
    IF sw = 456 THEN LINE (sw, 302)-(480, 312), 3, BF ELSE LINE (sw, 335)-(634, 345), 9, BF
    PRINT I$;
    LOCATE row%, col% + Le%
  ELSE
    BEEP
  END IF
LOOP
END FUNCTION

SUB inbar (prompt$, sel$, lenofinput%, txtclr%, fg%, bg%, x)
IF x = 1 THEN soundfx 6
row% = CSRLIN: col% = POS(0)
z = (LEN(prompt$) * 8) + ((lenofinput + 2) * 8)
z = (639 - z) / 2
y = (LEN(prompt$) * 8) + z
cl2 = fg + 128 XOR bg
mousepointer 0
LINE (4, 336)-(634, 344), 9, BF
nz = DRAWSTR%(2, prompt$, z, 337, txtclr, 9, 8)
nz = (y + 24) / 8
sel$ = Iinkey$(lenofinput, 43, nz, cl2, y)
LINE (4, 336)-(634, 344), 9, BF
mousepointer 1
LOCATE row, col
END SUB

SUB LOADhlp (hlp$())
hlp$(0, 0) = "N"
hlp$(1, 0) = "M"
hlp$(2, 0) = "S"
hlp$(3, 0) = "Y"
hlp$(0, 1) = "C"
hlp$(1, 1) = "L"
hlp$(2, 1) = "T"
hlp$(3, 1) = "X"
hlp$(0, 2) = "U"
hlp$(1, 2) = "A"
hlp$(2, 2) = "P"
hlp$(3, 2) = "B"
hlp$(0, 3) = "O"
hlp$(1, 3) = "D"
hlp$(2, 3) = "V"
hlp$(3, 3) = "H"
hlp$(0, 4) = "I"
hlp$(1, 4) = "G"
hlp$(2, 4) = "E"
hlp$(3, 4) = "R"
END SUB

SUB Nocary STATIC
IF cl = 99 THEN EXIT SUB
row% = CSRLIN
cl = 3 + 128 XOR 0: COLOR cl
IF row > 23 THEN ScrnScroll 1, 1, maxline, 80, 1, 0: row = 24
LOCATE row, 1: PRINT "NO CARRIER"
cl = 99
END SUB

SUB PutText2 (text$, TextColor%, BackColor%)
col% = POS(0) - 1
inregs.bx = (TextColor% XOR BackColor%) XOR 128
inregs.cx = 1
FOR ptr% = 1 TO LEN(text$)
  inregs.ax = &H900 + ASC(MID$(text$, ptr%, 1))
  LOCATE , col% + ptr%
  CALL InterruptX(&H10, inregs, outregs)
NEXT
END SUB

SUB QSORT (first%, Last%)
 '*****  Quick-Sort
 '*** sorts(ascending) the fields in array(), from field First% thru Field Last%
 IF first% >= Last% THEN EXIT SUB
 REDIM Lft%(10), Rght%(10)
 temp% = 1
 Lft%(1) = first%
 Rght%(1) = Last%
 DO
   start% = Lft%(temp%)
   Ende% = Rght%(temp%)
   temp% = temp% - 1
   DO           '** (WHILE Start% < Ende%)
    IndexLft% = start%
    IndexRght% = Ende%
    x$ = UCASE$(help((start% + Ende%) \ 2))
    DO
     WHILE UCASE$(help(IndexLft%)) < x$: IndexLft% = IndexLft% + 1: WEND
     WHILE x$ < UCASE$(help(IndexRght%)): IndexRght% = IndexRght% - 1: WEND
     IF IndexLft% > IndexRght% THEN EXIT DO
     SWAP help(IndexLft%), help(IndexRght%)   '*** switch elements
     IndexLft% = IndexLft% + 1
     IndexRght% = IndexRght% - 1
    LOOP
    IF IndexRght% - start% >= Ende% - IndexLft% THEN
      IF start% < IndexRght% THEN
        temp% = temp% + 1
        Lft%(temp%) = start%
        Rght%(temp%) = IndexRght%
      END IF
      start% = IndexLft%
    ELSE
      IF IndexLft% < Ende% THEN
        temp% = temp% + 1
        Lft%(temp%) = IndexLft%
        Rght%(temp%) = Ende%
      END IF
       Ende% = IndexRght%
    END IF
  LOOP WHILE start% < Ende%
LOOP WHILE temp%
END SUB

SUB readdir (ext$)
  REDIM help(88) AS STRING * 76
  IF ext$ = "*.MAP" THEN cl = 14 + 128 XOR 8 ELSE cl = 8 + 128 XOR 7
  COLOR cl
  found$ = DIR$(ext$)
  IF LEN(found$) THEN
    help(1) = LEFT$(found$, LEN(found$) - 5): cnt = 2
    DO WHILE LEN(found$)
      found$ = DIR$("")
      x = INSTR(found$, ".SND")
      IF x < 1 THEN
        IF found$ <> "" THEN help(cnt) = LEFT$(found$, LEN(found$) - 5)
        cnt = cnt + 1: IF cnt = 89 THEN EXIT DO
      END IF
    LOOP
  END IF
  QSORT 1, cnt - 1
  sw = 2: z = 34
  IF cnt > 20 THEN z = 24
  IF cnt > 39 THEN z = 16
  IF cnt > 58 THEN z = 6
  FOR cnt = z TO 61 STEP 18
    FOR x = 3 TO 21
      LOCATE x, cnt: PRINT LEFT$(help(sw), 12)
      sw = sw + 1
    NEXT x
  NEXT cnt
END SUB

SUB reg
WIDTH , 25
PALETTE
PAINT (1, 1), thatch$
LINE (8, 8)-(629, 31), 6, BF
LINE (160, 304)-(479, 323), 6, BF
cl = 14 + 128 XOR 6
disp "TERMTA 3.1                   Copyright 1995 by Will Boyett", cl, 2
disp "PRESS ANY KEY TO EXIT", cl, 23
cubeit 33, 50, 566, 236, 6, 14, 1

cl = 14 + 128 XOR 4: COLOR cl
disp "If you find this utility useful and want more power, then please", cl, 6
disp "order TermTA's Registration packet today. With your registration", cl, 7
disp "packet you will get:", cl, 8
cl = 15 + 128 XOR 4: COLOR cl
disp "*  Registration Key to unlock more features  *", cl, 10
disp "*  Set of Maps for TA  *", cl, 11
disp "*  Unlimited Monster Database for use in Auto Attack  *", cl, 12
disp "*  Plus many more features (see !TTA.DOC for full list)  *", cl, 13
disp "*  This Sales Screen and the following pause will not appear  *", cl, 14
cl = 14 + 128 XOR 4: COLOR cl
disp "To order your registration package, send $15 + $3 p/h to:", cl, 16
disp "(better yet, print and fill out !ORDER.FRM included here)", cl, 17
disp "Will Boyett", cl, 18
disp "11955 209th St.", cl, 19
disp "Lakewood, CA 90715", cl, 20
DO: LOOP UNTIL INKEY$ <> ""
FOR x = 1 TO 25
  inregs.ax = &H601                         'scroll up 1 line
  inregs.bx = &H0                           'white on black
  inregs.cx = &H0                           'row =  0, col =  0
  inregs.dx = &H1813                        'row = 24, col = 19
  CALL InterruptX(&H10, inregs, outregs)
  inregs.ax = &H601                         'scroll up 1 line
  inregs.bx = &H0                           'white on black
  inregs.cx = &H28                          'row =  0, col = 40
  inregs.dx = &H183B                        'row = 24, col = 59
  CALL InterruptX(&H10, inregs, outregs)
  inregs.ax = &H701                         'scroll down 1 line
  inregs.bx = &H0                           'white on black
  inregs.cx = &H14                          'row =  0, col = 20
  inregs.dx = &H1827                        'row = 24, col = 39
  CALL InterruptX(&H10, inregs, outregs)
  inregs.ax = &H701                         'scroll down 1 line
  inregs.bx = &H0                           'white on black
  inregs.cx = &H3C                          'row =  0, col = 60
  inregs.dx = &H184F                        'row = 24, col = 79
  CALL InterruptX(&H10, inregs, outregs)
NEXT x
END SUB

SUB Response (swi, what$)
IF swi = 0 THEN z = 4 ELSE z = swi
start! = TIMER
LINE (4, 336)-(634, 344), 9, BF
DO
  IF DataWaiting THEN
    x$ = CHR$(ReadChar)
    I$ = I$ + x$
    IF ASC(x$) = 10 OR ASC(x$) = 13 THEN
      IF ASC(x$) = 10 THEN PRINT ""
      GOSUB scrll
      I$ = ""
    ELSE
      PRINT x$;
    END IF
  END IF
  j = TIMER - start!
  nz = DRAWSTR%(0, "Now Dialing  " + STR$(j), 260, 337, 14, 9, 8)
  IF j > z THEN
    GOSUB scrll
    LINE (4, 336)-(634, 344), 9, BF
    IF z = 4 THEN
      PRINT "Problem Initializing Modem": swi = -2: EXIT DO
    ELSE
      PRINT "Timeout!": swi = -1: EXIT DO
    END IF
  END IF
  IF swi <> 0 THEN  'if it's not medem init but actual dial
    IF INSTR(I$, "NO CARRIER") THEN swi = -1: EXIT DO
    IF INSTR(I$, "BUSY") THEN swi = -1: EXIT DO
    IF INSTR(I$, "NO DIAL") THEN swi = -1: EXIT DO
  END IF
  IF INKEY$ = CHR$(27) THEN swi = -1: EXIT DO
LOOP UNTIL INSTR(I$, what$)
DoAltBar
mousepointer 0
EXIT SUB
scrll:
  col = POS(0): row = CSRLIN
  IF row% > maxline THEN
    ScrnScroll 1, 1, maxline, 80, 1, 0
    mousepointer 0
    IF maxline = 24 THEN x = 184 ELSE x = 312
    LOCATE maxline, 1
  END IF
RETURN
END SUB

SUB ScrnScroll (ULRow%, ULColumn%, LRRow%, LRColumn%, NumOfLines%, sw)
mousepointer 0
DEF SEG = 0
IF sw = 0 THEN inregs.bx = &H0 * 256 ELSE inregs.bx = &H7 * 256
DEF SEG
inregs.cx = ((ULRow% - 1) * 256) OR (ULColumn% - 1)
inregs.dx = ((LRRow% - 1) * 256) OR (LRColumn% - 1)
IF NumOfLines% < 0 THEN
  inregs.ax = &H700 OR ABS(NumOfLines%)
ELSE
  inregs.ax = &H600 OR NumOfLines%
END IF
CALL InterruptX(&H10, inregs, outregs)
mousepointer 1
END SUB

SUB soundfx (sw) STATIC
SELECT CASE sw
  CASE 1                        'end of reroll
    y% = 30
    FOR x% = 700 TO 40 STEP -10
      SOUND x%, .1: y% = y% + 10: SOUND y%, .1
    NEXT x%
  CASE 2                        'for buttons
    FOR x = 1000 TO 1200 STEP 100
      SOUND x, .15
    NEXT
  CASE 3                       'rattlesnake for desert
    z = z + 1
    IF z >= 17000 THEN
      PLAY "MB T255"
      FOR y = 1 TO 10
        FOR x% = 1 TO 15
          SOUND RND * 700 + 90, .06
        NEXT
        PLAY "P42"
      NEXT y
      z = 0
    END IF
  CASE 4                       'water dripping
    z = z + 1
    IF z >= 10000 OR z = y THEN
      FOR x = 650 TO 950 STEP 100
        SOUND x, .15
      NEXT x
      z = 0: y = RND * 10000
    END IF
    IF y > 1000 AND y < 1101 THEN
      x% = 340
      FOR y = 700 TO 540 STEP -2
        SOUND x%, .11: x% = x% + 2: SOUND y%, .11
      NEXT y
      FOR y = 540 TO 800 STEP 4
        SOUND x%, .11: x% = x% - 4: SOUND y%, .11
      NEXT y
      FOR z = 1 TO 10000: NEXT z
      x = 442
      FOR y = 600 TO 800 STEP 4
        SOUND x%, .11: x% = x% - 4: SOUND y%, .11
      NEXT y
      x = 382
      FOR y = 660 TO 800 STEP 4
        SOUND x%, .11: x% = x% - 4: SOUND y%, .11
      NEXT y
      x = 322
      FOR y = 720 TO 800 STEP 4
        SOUND x%, .11: x% = x% - 4: SOUND y%, .11
      NEXT y
      y = RND * 8000
    END IF
  CASE 5                        'bird chirp
    z = z + 1
    IF z = 17000 THEN
      FOR j = 1 TO 3
        FOR I = 7000 TO 3000 STEP -150
          SOUND I, .07
        NEXT I
      NEXT j
      k = RND(1)
      IF k = 0 THEN
        FOR x = 1 TO 10000: NEXT x
        FOR j = 1 TO 2
          FOR I = 7000 TO 3000 STEP -150
            SOUND I, .07
          NEXT I
        NEXT j
      END IF
      z = 0
    END IF
  CASE 6                         'replace beep
    SOUND 3500, .8999999
    FOR x = 1 TO 50: NEXT x
    SOUND 3000, .8999999
    FOR x = 1 TO 50: NEXT x
    SOUND 4000, .8999999
    FOR x = 1 TO 50: NEXT x
  CASE 7                                  'swamp
   IF y > 5 THEN y = 0
   y = y + 1
   IF y = 1 THEN
    IF z <> 1 THEN z = 1 ELSE z = 0
    PLAY "mb T255 L52 O5"
    IF z = 1 THEN PLAY "caca#cB-cBcC" ELSE PLAY "baba#bB-bBbC"
   END IF
END SELECT
END SUB

DEFSNG A-Z
FUNCTION thatch$
'thatch pattern
pattern$ = CHR$(0) + CHR$(127) + CHR$(255) + CHR$(24)
pattern$ = pattern$ + CHR$(0) + CHR$(63) + CHR$(255) + CHR$(12)
pattern$ = pattern$ + CHR$(0) + CHR$(159) + CHR$(255) + CHR$(6)
pattern$ = pattern$ + CHR$(0) + CHR$(207) + CHR$(255) + CHR$(3)
pattern$ = pattern$ + CHR$(0) + CHR$(252) + CHR$(255) + CHR$(48)
pattern$ = pattern$ + CHR$(0) + CHR$(249) + CHR$(255) + CHR$(96)
pattern$ = pattern$ + CHR$(0) + CHR$(243) + CHR$(255) + CHR$(192)
pattern$ = pattern$ + CHR$(0) + CHR$(231) + CHR$(255) + CHR$(128)

thatch$ = pattern$
END FUNCTION

DEFINT A-Z
SUB updateSBack (scrollcnt, scrollback$(), line$)
FOR x = 1 TO 139
  SWAP scrollback$(x), scrollback$(x + 1)
NEXT x
scrollback$(scrollcnt) = line$
END SUB

