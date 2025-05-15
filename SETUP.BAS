DECLARE SUB edittext (rw%, clm%, choice$, text$, min%, max%)
DECLARE SUB disp3open ()
DECLARE SUB getcfg (file$)
DECLARE SUB savecfg ()
DECLARE SUB inbar (prompt$, sel$, lenofinput%, txtclr%, fg%, bg%, x%)
DECLARE FUNCTION Iinkey$ (max%, row%, col%, fore%, sw%)
DECLARE SUB initsb ()
DECLARE SUB disp2open ()
DECLARE SUB disp1open ()
DECLARE SUB disp (title$, cl%, row%)
DECLARE SUB windo (h%, v%, wd%, ht%, cl1%, cl2%, FILD%)
DECLARE SUB PutText2 (text$, TextColor%, BackColor%)
DECLARE SUB init (dosfg%, dosbg%)
DECLARE FUNCTION Exists% (filen$)
DECLARE FUNCTION inkey% (NoWait%, x%)
DECLARE FUNCTION BufIn$ (filename$, done%)
DECLARE FUNCTION EGAVIDEO% ()
DECLARE FUNCTION grid$ ()
DECLARE FUNCTION zig$ ()
DECLARE FUNCTION zig2$ ()
DECLARE SUB cubeit (h%, v%, wd%, ht%, C1%, C2%, FILD%)
DECLARE SUB icon (h%, v%, wd%, ht%, C1%, C2%, FILD%, OUTIN%)
DECLARE SUB smallprint (spx%, spy%, sp$, spcolor%)
DECLARE SUB tinyfont (hor%, ver%, inline$, CLR%)
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
CONST dos = &H21, SetDTA = &H1A00, FindFirst = &H4E00, FindNext = &H4F00
DEFINT A-Z
COMMON SHARED irq%, baud&, Port, HS
COMMON SHARED Modem$, OffHook$, dial$, wate$, phone$(), bbs$
'$DYNAMIC
REDIM SHARED menusav(1), phone$(1 TO 14), atak$(1 TO 14)

init dosfg, dosbg
disp1open   'press any key to start
IF NOT Exists(bbs$ + ".cfg") THEN
  disp2open  'gets baud, hs,port,IRQ
  savecfg
END IF
file$ = "TTAdial.cfg"
IF Exists(file$) THEN
  getcfg file$   'gets dial configs
ELSE
  phone$(1) = "AT M0"
  phone$(2) = "AT H0"
  phone$(3) = "AT DT"
  phone$(4) = "35"
END IF
disp3open
row = 9: col = 31
DO
  LOCATE row, col: PutText2 "Û", 2, 1
  SLEEP
  DO
    choice$ = INKEY$
  LOOP UNTIL choice$ <> INKEY$
  SELECT CASE choice$
   CASE CHR$(0) + "H": GOSUB gup
   CASE CHR$(0) + "P": GOSUB gdown
   CASE CHR$(27): EXIT DO
   CASE ELSE
     SELECT CASE row
       CASE 9, 10, 11, 12
         rw = row - 8: min = 31: max = 62
       CASE IS > 14
         rw = row - 10
         IF col = 41 THEN
           rw = rw + 5: min = 41: max = 62
         ELSE
           min = 18: max = 31
         END IF
     END SELECT
     edittext row, col, choice$, phone$(rw), min, max
  END SELECT
LOOP
CLOSE
OPEN "O", 1, file$
PRINT #1, "  Modem Init String: " + phone$(1)
PRINT #1, "Modem Hangup String: " + phone$(2)
PRINT #1, "  Modem Dial String: " + phone$(3)
PRINT #1, "          Dial Time: " + phone$(4)
PRINT #1, "-------------------------- Phone Numbers"
PRINT #1, phone$(5) + ":" + phone$(10)
PRINT #1, phone$(6) + ":" + phone$(11)
PRINT #1, phone$(7) + ":" + phone$(12)
PRINT #1, phone$(8) + ":" + phone$(13)
PRINT #1, phone$(9) + ":" + phone$(14)
CLOSE
SCREEN 0
WIDTH 80, 25
COLOR dosfg, dosbg     'restore
END
'
gup:
    LOCATE row, col: PutText2 "Û", 2, 1
    row = row - 1
    IF row = 8 THEN row = 19: col = 41
    IF row = 14 THEN
      IF col = 41 THEN
        row = 19: col = 18
      ELSE
        row = 12: col = 31
      END IF
    END IF
RETURN
gdown:
    LOCATE row, col: PutText2 "Û", 2, 1
    row = row + 1
    IF row = 13 THEN
      row = 15: col = 18
    END IF
    IF row = 20 THEN
      IF col = 18 THEN
        row = 15: col = 41
      ELSE
       row = 9: col = 31
      END IF
    END IF
RETURN

REM $STATIC
FUNCTION BufIn$ (filename$, done) STATIC

IF NOT reading THEN            'if the first time through
  reading = -1                 'show that we're now reading
  done = 0                     'clear Done just in case
  CR = 0                       'no return found yet.
  CR$ = CHR$(13)               'define for speed later

  filenum = FREEFILE           'open the file
  OPEN filename$ FOR BINARY AS #filenum
  Remaining& = LOF(filenum)    'byte count to be read

  BufSize = 4096               'bytes to read each pass
  Buffer$ = SPACE$(BufSize)    'assume BufSize bytes
END IF

'---- This is the main outer loop.
DO WHILE Remaining&              'while more in the file

  IF CR = 0 THEN                 'if no Return was found
    IF Remaining& < BufSize THEN 'read only what remains
      BufSize = Remaining&       'resize the buffer
      IF BufSize < 1 THEN EXIT DO'possible only if EOF 26
      Buffer$ = SPACE$(BufSize)  'create the file buffer
    END IF
    GET #filenum, , Buffer$      'read a block
    BufPos = 1                   'start at the beginning
  END IF                         '  of that block

  DO                                 'walk through buffer
    CR = INSTR(BufPos, Buffer$, CR$) 'look for a Return
    IF CR THEN                       'we found one
      SaveCR = CR                    'save where
      BufIn$ = MID$(Buffer$, BufPos, CR - BufPos)
      BufPos = CR + 2                'skip inevitable LF
      EXIT FUNCTION                  'all done for now
    ELSE                             'back up in the file
      '---- If we reached the end of the file and no 13
      '     was found, return what remains in the string.
      IF SEEK(filenum) >= LOF(filenum) THEN
        Output$ = MID$(Buffer$, SaveCR + 2)
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
done = 1: reading = 0: CLOSE #filenum
END FUNCTION

SUB disp (title$, cl, row)
y = 40 - (LEN(title$) \ 2)
cl = cl + 128 XOR 7
COLOR cl: LOCATE row, y: PRINT title$
END SUB

SUB disp1open
SCREEN 9, , 0, 1
WIDTH 80, 25
CLS
PAINT (317, 175), zig2$, 8
h = 132: v = 100
windo h, v, 359, 125, 15, 8, 7
SCREEN 9, , 0, 0
disp "WELCOME TO TERMTA'S SET-UP PROGRAM", 4, 9
cl = 0 + 128 XOR 7: COLOR cl
LOCATE 11, 21: PRINT "Use this utility to configure your modem"
LOCATE 12, 19: PRINT "to work with MMTERM. Also use this utility"
LOCATE 13, 19: PRINT "whenever you wish to modify your dial list"
disp "Press any key to continue..", 4, 15
x = inkey(0, 0)
PCOPY 0, 1
END SUB

SUB disp2open
SCREEN 9, , 0, 1
CLS
PAINT (251, 101), zig$, 8
windo 110, 70, 410, 140, 15, 8, 7
SCREEN 9, , 0, 0
disp "MODEM SETTINGS", 4, 7
disp "Tell me a little bit about your modem. Is it:", 6, 9
black = 0 + 128 XOR 7: COLOR black
LOCATE 11, 39: PRINT "a 2400"
LOCATE 12, 39: PRINT "a 9600"
LOCATE 13, 39: PRINT "a 14.4"
LOCATE 14, 39: PRINT "a 28.8"
red = 4 + 128 XOR 7: COLOR red
FOR x = 1 TO 4
 LOCATE x + 10, 34: PRINT STR$(x) + ">"
NEXT x
DO: choice = inkey(0, 0): LOOP UNTIL choice > 48 AND choice < 53
SELECT CASE choice
  CASE 49: HS = 0: baud& = 2400
  CASE 50: HS = 2: baud& = 9600
  CASE 51, 52: HS = 2: baud& = 19200
END SELECT
'
LINE (130, 100)-(500, 200), 7, BF
disp "What Port is it connected to:", 6, 9
COLOR black
LOCATE 11, 40: PRINT "COM1"
LOCATE 12, 40: PRINT "COM2"
LOCATE 13, 40: PRINT "COM3"
LOCATE 14, 40: PRINT "COM4"
COLOR red
FOR x = 1 TO 4
  LOCATE x + 10, 35: PRINT STR$(x) + ">"
NEXT x
DO: choice = inkey(0, 0): LOOP UNTIL choice > 48 AND choice < 53
Port = choice - 48
SELECT CASE Port
  CASE 1: irq = 4
  CASE 2: irq = 3
  CASE ELSE: irq = 5
END SELECT
'
LINE (130, 100)-(500, 200), 7, BF
disp "By default, COM" + LTRIM$(STR$(Port)) + " uses IRQ" + STR$(irq), 6, 9
disp "Shall we use this IRQ?", 6, 10
COLOR black
LOCATE 12, 39: PRINT "Yes"
LOCATE 13, 39: PRINT "No"
COLOR red
FOR x = 1 TO 2
  LOCATE x + 11, 35: PRINT STR$(x) + ">"
NEXT x
DO: choice = inkey(0, 0): LOOP UNTIL choice > 48 AND choice < 51
IF choice = 50 THEN
  LINE (130, 100)-(500, 200), 7, BF
  inbar "Enter the IRQ you wish to use: ", irq$, 2, 6, 4, 7, 0
  irq = VAL(irq$)
END IF
PCOPY 0, 1
END SUB

SUB disp3open
SCREEN 9, , 0, 1
CLS
PAINT (251, 101), grid$, 8
windo 85, 41, 460, 266, 15, 8, 7
icon 231, 108, 273, 60, 8, 15, 1, 0
icon 131, 194, 122, 75, 8, 15, 1, 0
icon 308, 194, 198, 75, 8, 15, 1, 0
SCREEN 9, , 0, 0
PCOPY 0, 1
disp "DIAL  SETTINGS", 4, 5
disp "Here is where you configure your", 6, 6
disp "modem commands and phone numbers.", 6, 7
cl = 0 + 128 XOR 7: COLOR cl
 x = 15
LOCATE 9, x: PRINT "  Init String:"
LOCATE 10, x: PRINT "Hangup String:"
LOCATE 11, x: PRINT "  Dial String:"
LOCATE 12, x: PRINT "    Dial Time:"
LOCATE 14, 21: PRINT "BBS Name"
LOCATE 14, 46: PRINT "Phone Number"
disp "Hit <ESC> to Exit", 4, 21
cl = 14 + 128 XOR 1: COLOR cl
clm = 1
FOR x = 9 TO 12
  LOCATE x, 31: PRINT phone$(clm)
  clm = clm + 1
NEXT x
FOR x = 5 TO 9
  LOCATE 10 + x, 18: PRINT phone$(x)
  LOCATE 10 + x, 41: PRINT phone$(x + 5)
NEXT x
END SUB

SUB edittext (rw, clm, choice$, text$, min, max)
vert = max * 8
DO
  SELECT CASE choice$
  CASE CHR$(8)                           'backspace
    rw1 = rw * 14 - 14
    LINE ((min - 1) * 8, rw1)-(vert, rw1 + 13), 1, BF
    IF clm = min THEN
      BEEP
      clm = clm + 1
    ELSE
      text$ = LEFT$(text$, (clm - min) - 1) + MID$(text$, (clm - min) + 1)
    END IF
    clm = clm - 1
    LOCATE rw, min: PutText2 text$, 14, 1
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE CHR$(0) + "M"                        'right
    LOCATE rw, clm: PutText2 "Û", 2, 1
    clm = clm + 1
    IF clm > max THEN clm = max: BEEP
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE CHR$(0) + "K"                        'left
    LOCATE rw, clm: PutText2 "Û", 2, 1
    clm = clm - 1
    IF clm < min THEN clm = min: BEEP
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE CHR$(0) + "G"                        'home
    LOCATE rw, clm: PutText2 "Û", 2, 1
    clm = (clm - LEN(text$)) + 1
    IF clm < min THEN clm = min: BEEP
    IF clm > max THEN clm = max: BEEP
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE CHR$(0) + "O"                        'end
    LOCATE rw, clm: PutText2 "Û", 2, 1
    clm = (LEN(text$) + clm) - 1
    IF clm > max THEN clm = max: BEEP
    IF clm < min THEN clm = min: BEEP
    LOCATE rw, clm: PutText2 "Û", 2, 1
  CASE ELSE
    IF choice$ > CHR$(31) AND choice$ < CHR$(123) THEN
      IF LEN(text$) + min > max THEN
        BEEP
      ELSE
        rw1 = rw * 14 - 14                               'if entering text
        LINE ((min - 1) * 8, rw1)-(vert, rw1 + 13), 1, BF
        z = (LEN(text$) + min) - clm
        IF z < 1 THEN temp$ = "" ELSE temp$ = RIGHT$(text$, z)
        text$ = LEFT$(text$, clm - min) + choice$ + temp$
        LOCATE rw, min: PutText2 text$, 14, 1
        clm = clm + 1
        IF clm > max THEN clm = max: BEEP
        LOCATE rw, clm: PutText2 "Û", 2, 1
      END IF
    END IF
    IF choice$ = CHR$(0) + "S" THEN 'AND ((LEN(text$) + 5) - clm) > -1 THEN
      rw1 = rw * 14 - 14                             'if deleting
      LINE ((min - 1) * 8, rw1)-(vert, rw1 + 13), 1, BF
      text$ = LEFT$(text$, clm - min) + MID$(text$, clm - min + 2)
      LOCATE rw, min: PutText2 text$, 14, 1
      LOCATE rw, clm: PutText2 "Û", 2, 1
    END IF
  END SELECT
  IF choice$ = CHR$(0) + "H" OR choice$ = CHR$(0) + "P" THEN EXIT DO
  choice$ = INKEY$
LOOP UNTIL choice$ = CHR$(27)
LOCATE rw, clm: PutText2 "Û", 2, 1
END SUB

SUB getcfg (file$)
  FOR x = 1 TO 4
    inline$ = BufIn$(file$, done)
    SELECT CASE x
      CASE 1: phone$(1) = MID$(inline$, 22)
      CASE 2: phone$(2) = MID$(inline$, 22)
      CASE 3: phone$(3) = MID$(inline$, 22)
      CASE 4: phone$(4) = MID$(inline$, 22)
    END SELECT
  NEXT x
  inline$ = BufIn$(file$, done)
  FOR x = 5 TO 9
    inline$ = BufIn$(file$, done)
    z = INSTR(inline$, ":")
    phone$(x) = LEFT$(inline$, z - 1)
    phone$(x + 5) = MID$(inline$, z + 1)
    IF done THEN x = 9
  NEXT x
END SUB

DEFSNG A-Z
FUNCTION grid$

'little metal pop up buttons
  pattern$ = CHR$(255) + CHR$(255) + CHR$(255) + CHR$(0)
  pattern$ = pattern$ + CHR$(255) + CHR$(255) + CHR$(255) + CHR$(124)
  pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(68)
  pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(68)
  pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(68)
  pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(68)
  pattern$ = pattern$ + CHR$(195) + CHR$(195) + CHR$(195) + CHR$(124)
  pattern$ = pattern$ + CHR$(255) + CHR$(255) + CHR$(255) + CHR$(0)

grid$ = pattern$
END FUNCTION

DEFINT A-Z
FUNCTION Iinkey$ (max%, row%, col%, fore, sw)
CONST YES = 1, NO = 0, ENTER = &HD, bs = 8
LOCATE row%, col%
COLOR fore
DO
  DO
    K$ = INKEY$
  LOOP UNTIL K$ <> ""

  IF LEN(K$) = 2 THEN
    Extend% = YES
    K% = ASC(RIGHT$(K$, 1))
  ELSE
    Extend% = NO
    K% = ASC(K$)
  END IF
  IF (Extend% = NO) AND (K% = ENTER) THEN
    Iinkey$ = I$
    EXIT FUNCTION
  ELSEIF (Extend% = NO) AND (K% = bs) AND (Le% > 0) AND (CurPos% > 0) THEN
    Work1$ = LEFT$(I$, CurPos% - 1)
    Work2$ = RIGHT$(I$, Le% - CurPos%)
    I$ = Work1$ + Work2$
    CurPos% = CurPos% - 1
    Le% = Le% - 1
    LOCATE row%, col%, 1
    LINE (sw, 138)-(sw + 30, 160), 7, BF
    PRINT I$ + SPACE$(1);
    LOCATE row%, col% + CurPos%
  ELSEIF (Extend% = NO) AND (K% > 31) AND (K% < 127) AND (Le% < max%) THEN
    Work1$ = LEFT$(I$, CurPos%)
    Work2$ = RIGHT$(I$, Le% - CurPos%)
    I$ = Work1$ + CHR$(K%) + Work2$
    Le% = Le% + 1
    CurPos% = CurPos% + 1
    LOCATE row%, col%, 1
    LINE (sw, 138)-(sw + 30, 160), 7, BF
    PRINT I$;
    LOCATE row%, col% + Le%
  ELSE
    BEEP
  END IF
LOOP
END FUNCTION

SUB inbar (prompt$, sel$, lenofinput%, txtclr%, fg%, bg%, x)
z = LEN(prompt$) + (lenofinput + 2)
z = (80 - z) / 2
cl2 = txtclr + 128 XOR bg: COLOR cl2
LOCATE 11, z: PRINT prompt$
cl2 = fg + 128 XOR bg
z = (z + LEN(prompt$)) + 1
y = (z * 8) - 10
sel$ = Iinkey$(lenofinput, 11, z, cl2, y)
END SUB

SUB init (dosfg, dosbg)
IF NOT EGAVIDEO% THEN
  CLS : BEEP
  PRINT "Sorry, EGA graphic card required to run SETUP.EXE."
  END
END IF
'--------  saves original pc colors
SCREEN 0
SHELL "CLS"
DEF SEG = &HB800
dosclr = PEEK(1)
dosfg = dosclr AND 15
dosbg = (dosclr \ 16) AND 7
DEF SEG
'--------  get name
bbs$ = UCASE$(COMMAND$)
DO
  IF bbs$ = "" THEN INPUT "Ooops.. you forgot to enter a database name for this game: ", bbs$
LOOP UNTIL bbs$ <> ""
IF LEN(bbs$) > 8 THEN bbs$ = LEFT$(bbs$, 8)
END SUB

FUNCTION inkey% (NoWait%, x)
DO
  K$ = UCASE$(INKEY$)
LOOP UNTIL NoWait% OR LEN(K$)
SELECT CASE LEN(K$)
  CASE 1: inkey% = ASC(K$)
  CASE 2: inkey% = -ASC(RIGHT$(K$, 1))   'Extended key, return ASCII
END SELECT                                '  value negated
IF x = 1 THEN LINE (4, 336)-(634, 344), 9, BF
END FUNCTION

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

SUB savecfg
file$ = bbs$ + ".cfg"
snd = 1: await = 12: afeed = 1: aheal = 0: awhen = 60: anum = 1: aexit = 1
health$ = "HE": atak$(8) = "dr wat": atak$(9) = "eat rat"
atak$(10) = "dr verb": atak$(11) = "dr anemone": atak$(13) = "dr yarrow"
FOR x = 1 TO 7
  atak$(x) = "a "
NEXT x
z = FREEFILE
  OPEN "O", #z, file$
  PRINT #z, snd%, afeed, anum
  PRINT #z, Port%, HS, atak$(11)
  PRINT #z, irq%, baud&, await
  WRITE #z, atak$(1), atak$(2), atak$(3)
  WRITE #z, atak$(4), atak$(5), atak$(6), atak$(7)
  WRITE #z, "HE", atak$(8), atak$(9), atak$(10)
  PRINT #z, aheal, awhen
  PRINT #z, aexit, ahave, atak$(13)
END SUB

SUB windo (h, v, wd, ht, cl1, cl2, FILD)
REDIM menusav(1 TO 4096)
z = (v / 14) + 2
y = ((v + ht) / 14)
w = ((h + wd) / 8) + 1
x = (h / 8 + 1)
cl = 0 + 128 XOR 7: COLOR cl
LOCATE y + 1, x + 1: PRINT STRING$((w - x), CHR$(223))
FOR x = z TO y
  LOCATE x, w: PRINT CHR$(219)
NEXT x
cubeit h%, v%, wd%, ht%, cl1, cl2, FILD%
icon h% + 4, v% + 4, wd% - 8, ht% - 8, cl2, cl1, FILD%, 0
END SUB

DEFSNG A-Z
FUNCTION zig$
'metal zig-zag
pattern$ = CHR$(191) + CHR$(191) + CHR$(191) + CHR$(72)
pattern$ = pattern$ + CHR$(223) + CHR$(223) + CHR$(223) + CHR$(36)
pattern$ = pattern$ + CHR$(239) + CHR$(239) + CHR$(239) + CHR$(18)
pattern$ = pattern$ + CHR$(253) + CHR$(253) + CHR$(253) + CHR$(18)
pattern$ = pattern$ + CHR$(251) + CHR$(251) + CHR$(251) + CHR$(36)
pattern$ = pattern$ + CHR$(247) + CHR$(247) + CHR$(247) + CHR$(72)
zig$ = pattern$
END FUNCTION

FUNCTION zig2$
'zig-zag pattern
pattern$ = CHR$(224) + CHR$(111) + CHR$(127) + CHR$(239)
pattern$ = pattern$ + CHR$(112) + CHR$(183) + CHR$(191) + CHR$(247)
pattern$ = pattern$ + CHR$(56) + CHR$(219) + CHR$(223) + CHR$(251)
pattern$ = pattern$ + CHR$(56) + CHR$(219) + CHR$(223) + CHR$(251)
pattern$ = pattern$ + CHR$(112) + CHR$(183) + CHR$(191) + CHR$(247)
pattern$ = pattern$ + CHR$(224) + CHR$(111) + CHR$(127) + CHR$(239)
zig2$ = pattern$
END FUNCTION

