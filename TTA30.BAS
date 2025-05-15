DECLARE SUB healmenu (atak$(), awhen%, ahave%)
DECLARE SUB chkpath (path$, lastpath$, sw%)
DECLARE SUB parseexits (ex$(), numexits%)
DECLARE SUB dialit (h%, v%)
DECLARE SUB init (dosfg%, dosbg%, regname$)
DECLARE SUB menubar2 ()
DECLARE SUB DoAltBar ()
DECLARE FUNCTION inkey% (NoWait%, x%)
DECLARE SUB Response (swi%, what$)
DECLARE SUB hilite (temp$, fore%, back%, high%, many%)
DECLARE SUB soundfx (sw%)
DECLARE SUB domacro (scrollback$(), keyin$, macro$(), recon%, line$, where%)
DECLARE SUB reg ()
DECLARE SUB atakmenu (anum%, atak$(), menusav%(), await%, regit%)
DECLARE SUB PutText2 (text$, TextColor%, BackColor%)
DECLARE SUB edittext (rw%, clm%, which%, min%, max%)
DECLARE SUB loadspell ()
DECLARE SUB reroll ()
DECLARE SUB findit (what$, offset%, sw%, which%, scrollback$())
DECLARE SUB makepath ()
DECLARE SUB LOADhlp (hlp$())
DECLARE SUB mousefind (keyin$)
DECLARE SUB mouseget (mouseb%)
DECLARE SUB mousepointer (OnOff%)
DECLARE SUB MouseArea (y1%, y2%, min%, max%)
DECLARE SUB mouseinit ()
DECLARE SUB mousespeed (Speed%)
DECLARE SUB viewhelp ()
DECLARE SUB disguise ()
DECLARE SUB savecfg (sw%)
DECLARE SUB config ()
DECLARE SUB recorder ()
DECLARE SUB emul (st$)
DECLARE SUB alertchek ()
DECLARE SUB sheltodos ()
DECLARE SUB viewfile (ext$, choice$)
DECLARE SUB adjustscreen ()
DECLARE SUB ckey (keyin$)
DECLARE SUB dooptions (keyin$)
DECLARE SUB menus ()
DECLARE SUB starlog (log$)
DECLARE SUB inbar (prompt$, sel$, lenofinput%, txtclr%, fg%, bg%, x%)
DECLARE SUB MacroScreen (menusav%())
DECLARE SUB loadmac (monsize%, regit%)
DECLARE SUB secempty (x%)
DECLARE SUB addwarps (temp$)
DECLARE SUB warning (bad%)
DECLARE SUB Credits ()
DECLARE SUB ermes (tp$, sw%)
DECLARE SUB scrollit (pntlow%, pnthigh%, size%, snd%)
DECLARE SUB readdir (ext$)
DECLARE SUB updateSBack (scrollcnt%, scrollback$(), line$)
DECLARE SUB doprint (ch0$, line$, fore0%, back0%, crai%, row%, col%)
DECLARE SUB ScrnScroll (ULRow%, ULColumn%, LRRow%, LRColumn%, NumOfLines%, sw%)
DECLARE SUB dowindo (num%, cl1%, cl2%)
DECLARE SUB Nocary ()
DECLARE SUB tinyfont (hor%, ver%, inline$, CLR%)
DECLARE SUB cubeit (h%, v%, WD%, HT%, C1%, C2%, FILD%)
DECLARE SUB icon (h%, v%, WD%, HT%, C1%, C2%, FILD%, OUTIN%)
DECLARE SUB Crypt (Buffer$, key$, start%)
DECLARE SUB smallprint (spx%, spy%, sp$, spcolor%)
DECLARE FUNCTION printerready! ()
DECLARE FUNCTION Exists% (filen$)
DECLARE FUNCTION Iinkey$ (max%, row%, col%, fore%, sw%)
DECLARE FUNCTION DIR$ (FileSpec$)
DECLARE FUNCTION CurDir$ ()
DECLARE FUNCTION BufIn$ (filen$, done%, reading%)
    DECLARE SUB OpenComm CDECL ALIAS "_open_comm" (BYVAL Port%, irq%, BYVAL Wlen%, BYVAL Parity%, BYVAL baud&, BYVAL HS%, BYVAL FOSSIL%)
    DECLARE SUB closecomm CDECL ALIAS "_close_comm" ()
    DECLARE FUNCTION ReadChar% CDECL ALIAS "_readchar" ()
    DECLARE SUB transmit CDECL ALIAS "_transmit_string" (addr$)
    DECLARE FUNCTION DataWaiting% CDECL ALIAS "_data_waiting" ()
    DECLARE SUB ClearInputBuffer CDECL ALIAS "_clear_input_buffer" ()
    DECLARE SUB carrierdetect CDECL ALIAS "_carrier_detect_flag" (BYVAL OnOff%)
    DECLARE FUNCTION CarrierLost% CDECL ALIAS "_carrier_state" ()
    DECLARE SUB DTRcontrol CDECL ALIAS "_dtr" (BYVAL OnOff%)
DECLARE FUNCTION ival% (number$)
DECLARE FUNCTION DRAWSTR% (mode%, strg$, x0%, y0%, fg%, bg%, gap%)
DECLARE FUNCTION EGAVIDEO% ()

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
DIM SHARED InRegs AS RegTypeX, OutRegs AS RegTypeX
DECLARE SUB InterruptX (intnum AS INTEGER, InRegs AS RegTypeX, OutRegs AS RegTypeX)
CONST dos = &H21
CONST SetDTA = &H1A00, FindFirst = &H4E00, FindNext = &H4F00

DEFINT A-Z
'$DYNAMIC
COMMON SHARED nomouse%, mouseh%, mousev%, aattack
COMMON SHARED uinput$(), macro$(), snd%, maxline, line$, rest
COMMON SHARED help()  AS STRING * 76, await, spels, spelsize
COMMON SHARED where%, fore0%, back0%, linport$
COMMON SHARED afeed, aheal, anum, awhen, ahave, bbs$, scrollcnt, aexit
COMMON SHARED log$, CRLF$, regit, irq%, baud&, recon, Port%, HS, monsize
COMMON SHARED doattack, getready, aroam, firstr, astart!, ashare
'
REDIM macro$(1 TO 37), uinput$(1), help(1) AS STRING * 76
REDIM SHARED menusav(1 TO 32767), scrollback$(140), info$(1)
REDIM SHARED hlp$(3, 4), spells(1) AS STRING * 32, atak$(1 TO 14)
REDIM SHARED ex$(1 TO 10)
'
RANDOMIZE TIMER
CLS
IF NOT EGAVIDEO% THEN PRINT "Sorry, EGA graphic card required to run TermTA.": BEEP: END
IF NOT Exists("tTAscrn0.gfx") THEN PRINT "Sorry, It seems you are missing TermTA's graphic file (TTASCRN0.GFX).": BEEP: END
bbs$ = UCASE$(COMMAND$)                 'gets gamename, seperates major
z = INSTR(bbs$, " ")
IF z <> 0 THEN bbs$ = LEFT$(bbs$, z - 1): dial = 78
IF LEN(bbs$) > 8 THEN bbs$ = LEFT$(bbs$, 8)
IF bbs$ = "" THEN
  DO
    INPUT "You forgot to specify a database to access.  Please do so now: ", bbs$
  LOOP UNTIL bbs$ <> ""
  bbs$ = UCASE$(bbs$)
  z = INSTR(bbs$, " ")
  IF z <> 0 THEN bbs$ = LEFT$(bbs$, z - 1): dial = 78
END IF
IF NOT Exists(bbs$ + ".cfg") THEN
  IF Exists("setup.exe") THEN
    SHELL "setup " + bbs$
  ELSE
    PRINT "Sorry, you are missing SETUP.EXE. Now aborting!": BEEP: END
  END IF
END IF
'
mouseinit
init dosfg, dosbg, regname$
MouseArea 194, 330, 2, 635
OpenComm Port, irq%, 8, 0, baud&, HS, 0
mousepointer 0
IF z = 0 THEN
  nz = DRAWSTR%(0, "Do you wish to dial a BBS?  (Y/n)", 188, 337, 14, 9, 8)
  dial = inkey(0, 1)
END IF
IF dial <> 78 THEN 'yes dial
  dialit 1, 9
ELSE              'we either offline or already connected
  LOCATE 3, 1
  IF CarrierLost% <> 1 THEN transmit CHR$(13)
END IF
IF regit = 0 THEN regname$ = "UNREGISTERED"
hilite "Database Name:  " + bbs$ + STRING$(10, " ") + "Licensed to:  " + regname$, 14, 9, 0, 0
'
mousepointer 1
fore0 = 7
DO                              ' Main communications loop.
  mouseget mouseb
  keyin$ = INKEY$
  IF CarrierLost% THEN Nocary
  IF snd = 1 THEN soundfx where
  IF mouseb = 1 THEN
    mousefind keyin$
    WHILE mouseb%
      mouseget mouseb
    WEND
  ELSEIF mouseb = 2 THEN
    IF aattack = 0 THEN keyin$ = CHR$(27) ELSE aattack = 0: dowindo 8, 15, 8
    WHILE mouseb%
      mouseget mouseb
    WEND
  END IF
  IF keyin$ = CHR$(27) THEN            'escape
    IF snd = 1 THEN soundfx 6
    mousepointer 0
    LINE (4, 336)-(634, 344), 9, BF
    nz = DRAWSTR%(0, "Do you really want to exit TermTA?  y/N", 160, 337, 14, 9, 8)
    DO: keyin$ = UCASE$(INKEY$): LOOP UNTIL keyin$ <> ""
    IF keyin$ = "Y" THEN
      EXIT DO
    ELSE
      keyin$ = ""
      LINE (4, 336)-(634, 344), 9, BF
      nz = DRAWSTR%(0, "TermTA 3.1", 152, 337, 15, 9, 8)
      nz = DRAWSTR%(0, "Copyright 1995 by Will Boyett", 264, 337, 14, 9, 8)
      mousepointer 1
    END IF
  END IF
  IF LEN(keyin$) = 2 THEN                'F keys
    dooptions keyin$
  ELSEIF keyin$ <> "" THEN               'other keys
    ckey keyin$
  END IF
  IF recon = 1 THEN                     'if playing back recorder
    LINE INPUT #7, modemin$
    IF EOF(7) THEN recorder
    emul modemin$
  END IF
  IF doattack = 10 THEN
    IF TIMER >= (astart! + await) THEN
      transmit CHR$(13)
      doattack = 0
    END IF
  END IF
  DO WHILE DataWaiting
    modemin$ = CHR$(ReadChar)
    IF recon = 2 THEN       'if doing black box
      IF LEFT$(modemin$, 1) = CHR$(10) THEN
        IF w$ <> "" THEN PRINT #7, w$
        PRINT #7, modemin$
        w$ = ""
      ELSE
        w$ = w$ + modemin$
      END IF
    END IF
    emul modemin$          'determines if menu display
  LOOP
LOOP
closecomm
savecfg 0
IF regit = 0 THEN reg
CLOSE
SCREEN 0
WIDTH 80, 25
Credits
COLOR dosfg, dosbg     'restore
IF regit = 0 THEN SLEEP 3
LOCATE 24, 1
END

REM $STATIC
SUB alertchek STATIC
IF line$ = "" THEN EXIT SUB
IF CarrierLost THEN EXIT SUB
'
IF chkgroup = 1 THEN GOSUB group
IF spels = 1 THEN GOSUB spells
'
IF LEFT$(line$, 6) = "You're" THEN
  z = 0
  IF doattack = 0 THEN                   'If we not in combat
    z = INSTR("the foresa marble the swampa sandy pthe town the desera stonewoa flagsto", MID$(line$, 11, 9))
    IF z <> 0 THEN
      SELECT CASE z                        '1 2 6 gone
        CASE 1: where = 5                  '5 forest
        CASE 10, 55, 64                       '9 marble hallway- tower
          where = 0                        '9 stoneworks
          IF snd = 1 THEN
            SOUND 12500, .3499999
            FOR x = 1 TO 10000: NEXT x
            SOUND 12500, .3499999
          END IF
        CASE 19: where = 7                 '7 swamp
        CASE 28, 46: where = 3             '3 sandy passage - desert
        CASE 37: IF MID$(line$, 20, 5) <> "vault" THEN where = 4   '4 town sewers
      END SELECT
    END IF
  END IF
  IF z = 0 THEN   'if not in one of the above.. check to see if in one of these
    z = INSTR("in a cain a crthirstyhungry.poisoneon a pa", MID$(line$, 8, 7))
    SELECT CASE z
      CASE 1, 8, 36: where = 0              'if in cave or crude building
      CASE 15: IF afeed = 1 THEN transmit atak$(8) + CHR$(13)  'thirsty
      CASE 22: IF afeed = 1 THEN transmit atak$(9) + CHR$(13) 'hungry
      CASE 29: IF afeed = 1 THEN transmit atak$(10) + CHR$(13) 'poisoned
    END SELECT
  END IF
ELSEIF RIGHT$(line$, 13) = "on the floor." THEN
  IF MID$(line$, 10, 7) <> "nothing" THEN
    IF snd = 1 THEN soundfx 6
    row0 = CSRLIN
    cl = 15 + 128 XOR 12: COLOR cl
    LINE (370, (row0 * 8) - 26)-(460, (row0 * 8) - 16), 12, BF
    LOCATE row0 - 2, 48: PRINT "ITEM ALERT"
    LOCATE row0 + 1, 1
  END IF
  IF aexit = 1 AND doattack = 0 THEN transmit "EX" + CHR$(13)
  getready = 0: health = 0
  IF doattack = 1 THEN GOSUB AttacK
ELSEIF LEFT$(line$, 10) = "There is a" OR LEFT$(line$, 10) = "There are " THEN
  GOSUB FindMonst
ELSEIF LEFT$(line$, 7) = "Exits: " THEN
  IF aroam = 1 AND doattack = 0 THEN
    parseexits ex$(), numexits
    IF firstr = 0 THEN
      inbar "Start in what direction? ", path$, 2, 14, 0, 9, snd
      IF path$ = "" THEN path$ = ex$(1) ELSE path$ = LCASE$(path$)
      firstr = 1
    ELSE
      IF numexits = 1 THEN
        path$ = ex$(1)
      ELSE
        DO
          z = INT(RND * numexits) + 1
          path$ = ex$(z)
          z = 0
          chkpath path$, lastpath$, z
        LOOP UNTIL z <> 0
      END IF
    END IF
    IF getready = 0 THEN lastpath$ = path$
    IF regit = 0 THEN
      IF regtot < 70 THEN
        regtot = regtot + 1
        IF getready = 0 THEN transmit path$ + CHR$(13): getready = 1
      ELSE
        ermes "Sorry, you'll need to register to use AUTO ROAM further.", 1
      END IF
    ELSE
      IF getready = 0 THEN transmit path$ + CHR$(13): getready = 1
    END IF
  END IF
ELSEIF LEFT$(line$, 11) = "Your attack" OR RIGHT$(line$, 7) = "attack!" THEN
  IF doattack = 1 THEN
    FOR x = 2 TO anum
      IF ASC(atak$(x)) <> 42 THEN
        transmit atak$(x) + uinput$(1) + CHR$(13)
      END IF
    NEXT x
    doattack = 10
    astart! = TIMER
  END IF
ELSEIF LEFT$(line$, 13) = "You are still" OR LEFT$(line$, 13) = "Sorry, you'll" THEN
  IF INSTR(line$, "have to rest") THEN
    IF aroam = 1 THEN SLEEP 4: transmit path$ + CHR$(13): getready = 1
  END IF
  doattack = 10
  astart! = TIMER - (await / 2)
ELSEIF RIGHT$(line$, 1) = "!" THEN
  SELECT CASE RIGHT$(line$, 7)
    CASE "feless!", " smoke!"     'animal dies or appears
      IF aattack = 1 THEN doattack = 10
    CASE "damage!", "e area!"
      GOSUB dispheal
      IF doattack = 0 AND getready = 0 THEN transmit CHR$(13): getready = 1
    CASE "eeding!"
      IF aroam = 1 THEN
        IF doattack = 0 THEN SLEEP 2: transmit path$ + CHR$(13): getready = 1
      END IF
    CASE "battle!"         'trying to leave while fighting
      IF getready = 0 THEN transmit CHR$(13): getready = 1
      doattack = 0
    CASE "fs you!": getready = 1: doattack = 0
    CASE "tomach!": GOSUB dispheal
    CASE ELSE
      IF RIGHT$(line$, 5) = "area!" THEN GOSUB dispheal
    END SELECT
ELSEIF LEFT$(line$, 4) = "The " THEN
  IF INSTR(line$, "just gone") THEN
    doattack = 0
    IF getready = 0 THEN transmit CHR$(13): getready = 1
  END IF
ELSEIF LEFT$(line$, 9) = "Vitality:" THEN    'checks to see if needs healing
  health = 0   'checked health
  IF aheal = 1 THEN
    line$ = LTRIM$(MID$(line$, 10))
    x = INSTR(line$, "/")
    IF Ovit = 0 THEN Ovit = ival(MID$(line$, x + 1))
    z = ival(LEFT$(line$, x - 1))
    IF z <> nvit THEN
      nvit = z
      IF Ovit - awhen > nvit THEN transmit atak$(11) + CHR$(13)
    END IF
    IF LEFT$(scrollback$(140), 4) = "Mana" THEN
      line$ = LTRIM$(MID$(scrollback$(140), 10))
      x = INSTR(line$, "/")
      z = ival(LEFT$(line$, x - 1))
      IF z <= ahave THEN
        IF yarr = 0 THEN
          transmit atak$(13) + CHR$(13): yarr = 1
        ELSE
          yarr = 0
        END IF
      END IF
    END IF
  END IF
ELSEIF LEFT$(line$, 2) = "A " OR LEFT$(line$, 3) = "An " THEN
  IF getready = 0 THEN   'check in case many monsters enter room
    SELECT CASE MID$(line$, 4, 3)
      CASE "odd", "pri"
      CASE ELSE: transmit CHR$(13): getready = 1
    END SELECT
  ELSE
    IF RIGHT$(line$, 4) = "ion." THEN transmit CHR$(13)'mystical force prevents exit
  END IF
ELSEIF LEFT$(line$, 7) = "You fou" THEN
  IF ashare = 1 THEN
    z = INSTR(line$, " gold cro")
    IF z <> 0 THEN
      temp$ = MID$(line$, 11, z - 11)
      transmit "share " + temp$ + CHR$(13)
    END IF
  END IF
ELSEIF RIGHT$(line$, 7) = "nearby." THEN
  IF doattack <> 10 THEN doattack = 0
ELSEIF RIGHT$(line$, 7) = "spells:" THEN
  spels = 1
END IF
EXIT SUB
'
FindMonst:
IF aattack <> 1 THEN RETURN
doattack = 0
FOR x = 10 TO monsize
  IF INSTR(3, line$, uinput$(x)) THEN
    uinput$(1) = uinput$(x)
    doattack = 1: x = monsize
  END IF
NEXT x
RETURN
'
AttacK:
IF TIMER >= (astart! + await) THEN
  IF ASC(atak$(1)) = 42 THEN
    IF RIGHT$(scrollback$(140), 8) = "is here." OR RIGHT$(scrollback$(140), 8) = "re here." THEN
      temp$ = LEFT$(scrollback$(140), LEN(scrollback$(140)) - 9)
      transmit "GR" + CHR$(13)
      transmit "get xxx" + CHR$(13)
      chkgroup = 1
    ELSE
      transmit MID$(atak$(1), 2) + CHR$(13)
    END IF
  ELSE
    transmit atak$(1) + uinput$(1) + CHR$(13)
  END IF
ELSE
  doattack = 10
END IF
RETURN
'
dispheal:
IF aheal = 1 AND uinput$(2) <> "NONE" THEN
  IF health = 0 THEN transmit uinput$(2) + CHR$(13): health = 1
END IF
RETURN
'
group:
IF LEFT$(line$, 8) = "Sorry, b" THEN    'if you don't have xxx
  findit "***", offset, sw, 2, scrollback$()
  x = offset - 1
  findit "Your", offset, sw, 2, scrollback$()
  z = offset + 1
  DO
    match = 0
    y = INSTR(temp$, " ")
    IF y = 0 THEN
      gr$ = temp$: temp$ = ""
    ELSE
      gr$ = LEFT$(temp$, y - 1): temp$ = MID$(temp$, y + 1)
    END IF
    IF gr$ <> "and" THEN
      FOR y = z TO x
        j = INSTR(scrollback$(y), gr$)
        IF j <> 0 THEN match = 1: y = x
      NEXT y
    ELSE
      match = 1
    END IF
    IF match = 0 THEN EXIT DO   'if someone doesnt match get out
  LOOP UNTIL temp$ = ""
  IF match = 1 THEN   'if we found all matches do attack
    transmit MID$(atak$(1), 2) + CHR$(13)
  ELSE
    IF ataknum > 1 THEN transmit MID$(atak$(2), 2) + CHR$(13)
  END IF
  chkgroup = 0
END IF
EXIT SUB
RETURN
'
spells:
FOR x = 1 TO spelsize
  z = LEN(line$)
  IF z > 3 AND LEFT$(UCASE$(line$), z) = LEFT$(UCASE$(spells(x)), z) THEN
    nz = DRAWSTR%(0, " " + MID$(spells(x), 16) + " ", 140, (CSRLIN - 2) * 8, 8, 14, 8)
    x = spelsize
  END IF
NEXT x
IF LEFT$(line$, 3) = "***" THEN spels = 0
EXIT SUB
RETURN
END SUB

SUB chkpath (path$, lastpath$, sw)
IF sw = 0 THEN
  sw = 10
  SELECT CASE path$
    CASE "n": IF lastpath$ = "s" THEN sw = 0
    CASE "ne": IF lastpath$ = "sw" THEN sw = 0
    CASE "e": IF lastpath$ = "w" THEN sw = 0
    CASE "se": IF lastpath$ = "nw" THEN sw = 0
    CASE "s": IF lastpath$ = "n" THEN sw = 0
    CASE "sw": IF lastpath$ = "ne" THEN sw = 0
    CASE "w": IF lastpath$ = "e" THEN sw = 0
    CASE "nw": IF lastpath$ = "se" THEN sw = 0
    CASE "u": IF lastpath$ = "d" THEN sw = 0
    CASE "d": IF lastpath$ = "u" THEN sw = 0
  END SELECT
END IF
END SUB

SUB ckey (keyin$)
SELECT CASE keyin$
CASE CHR$(1): dialit POS(0), CSRLIN         'A
CASE CHR$(6): transmit "LS I" + CHR$(13)    'F
CASE CHR$(12): transmit "LOOK" + CHR$(13)   'L
CASE CHR$(19): transmit "ST" + CHR$(13)     'S
CASE CHR$(9): transmit "I" + CHR$(13)       'I
CASE CHR$(5): transmit "HE" + CHR$(13)      'E
CASE CHR$(24): transmit "EXPERIENCE" + CHR$(13)  'x
CASE CHR$(16): viewhelp                      'T
CASE CHR$(2): viewfile "S", choice$          'B
CASE CHR$(7): transmit "GR" + CHR$(13)
CASE CHR$(4): sheltodos: transmit CHR$(13)   'D
CASE CHR$(18): recorder                      'R
CASE CHR$(11): transmit "SP" + CHR$(13)      'K
CASE CHR$(3): starlog log$
CASE ELSE
  transmit keyin$
  IF keyin$ = CHR$(13) THEN doattack = 0
END SELECT
END SUB

SUB config
row% = CSRLIN: col% = POS(0)
mousepointer 0
GET (0, 0)-(639, 200), menusav
dowindo 2, 8, 15
mousepointer 0
GOSUB drawscrn
DO
  DO
    choice$ = UCASE$(INKEY$)
    mouseget mouseb
  LOOP UNTIL choice$ <> "" OR mouseb <> 0
  '
  SELECT CASE mouseb
    CASE 1: GOSUB mchoice
    CASE 2: choice$ = CHR$(13)
  END SELECT
  x = ival%(choice$)
  '
  SELECT CASE x
    CASE 1: Port = Port + 1: IF Port = 5 THEN Port = 0
    CASE 2: HS = HS + 1: IF HS = 4 THEN HS = 0
    CASE 3: irq = irq + 1: IF irq = 8 THEN irq = 2
    CASE 4
      SELECT CASE baud&
        CASE 2400: baud& = 9600
        CASE 9600: baud& = 14400
        CASE 14400: baud& = 19200
        CASE 19200: baud& = 38400
        CASE 38400: baud& = 57600
        CASE 57600: baud& = 2400
      END SELECT
    CASE 5: IF snd = 0 THEN snd = 1 ELSE snd = 0
    CASE 7, 8: healmenu atak$(), awhen, ahave: GOSUB drawscrn
    CASE 9: atakmenu anum, atak$(), menusav(), await, regit: GOSUB drawscrn
  END SELECT
  GOSUB values
LOOP UNTIL choice$ = CHR$(13)
mousepointer 0
PUT (0, 0), menusav, PSET
MouseArea 194, 330, 2, 635
savecfg 0
dowindo 2, 15, 8
LOCATE row%, col%
EXIT SUB
'
mchoice:
SELECT CASE mousev
  CASE 40 TO 54: choice$ = "1"
  CASE 55 TO 69: choice$ = "2"
  CASE 70 TO 85: choice$ = "3"
  CASE 86 TO 100: choice$ = "4"
  CASE 101 TO 115: choice$ = "5"
  CASE 116 TO 130: choice$ = "6"
  CASE 131 TO 145: choice$ = "7"
  CASE 146 TO 160: choice$ = "8"
  CASE 161 TO 175: choice$ = "9"
  CASE 176 TO 190: choice$ = "0"
END SELECT
WHILE mouseb%
  mouseget mouseb
WEND
RETURN
'
drawscrn:
cubeit 179, 0, 280, 199, 15, 8, 7
icon 184, 6, 270, 187, 8, 15, 7, 0
icon 219, 10, 200, 18, 8, 15, 7, 0
x = 219
nz = DRAWSTR%(0, ") ......Comm Port:", x, 43, 0, 7, 8)
nz = DRAWSTR%(0, ") ....Handshaking:", x, 58, 0, 7, 8)
nz = DRAWSTR%(0, ") ............IRQ:", x, 73, 0, 7, 8)
nz = DRAWSTR%(0, ") ...........Baud:", x, 88, 0, 7, 8)
nz = DRAWSTR%(0, ") ..........Sound:", x, 103, 0, 7, 8)
nz = DRAWSTR%(0, ") ....Config Auto Restore", x, 133, 0, 7, 8)
nz = DRAWSTR%(0, ") ....Config Auto Heal", x, 148, 0, 7, 8)
nz = DRAWSTR%(0, ") ....Config Auto Attack", x, 163, 0, 7, 8)
cnt = 0: h = 199
FOR x = 43 TO 175 STEP 15
  cnt = cnt + 1
  IF cnt <> 6 THEN nz = DRAWSTR%(0, STR$(cnt), h, x, 4, 7, 8)
NEXT x
'nz = DRAWSTR%(0, "0", 207, 178, 4, 7, 8)
smallprint 244, 23, "CONFIGURATION MENU", 0
smallprint 243, 22, "CONFIGURATION MENU", 14
values:
LINE (377, 40)-(439, 125), 7, BF
SELECT CASE HS
  CASE 0: temp$ = "NONE"
  CASE 1: temp$ = "XON/XOFF"
  CASE 2: temp$ = "CTS/RTS"
  CASE 3: temp$ = "ALL"
END SELECT
y = 9: x = 377
nz = DRAWSTR%(0, LTRIM$(STR$(Port%)), x, 43, y, 7, 8)
nz = DRAWSTR%(0, temp$, x, 58, y, 7, 8)
nz = DRAWSTR%(0, LTRIM$(STR$(irq%)), x, 73, y, 7, 8)
nz = DRAWSTR%(0, LTRIM$(STR$(baud&) + " "), x, 88, y, 7, 8)
IF snd = 1 THEN temp$ = "ON" ELSE temp$ = "OFF"
nz = DRAWSTR%(0, temp$, x, 103, y, 7, 8)
MouseArea 40, 190, 200, 350
mousepointer 1
RETURN
END SUB

SUB dialit (h, v)
  dowindo 11, 8, 15
  ClearInputBuffer
  mousepointer 0
  IF h <> 1 THEN h = 1: v = v + 2
  IF v > maxline THEN v = maxline
  LOCATE v, h
  file$ = "TTAdial.cfg"
  GOSUB getcfg
  carrierdetect 0     ' <-- NEEDED TO TALK TO MODEM W/NO CARRIER!!
  DO                   'make sure modem OK first
    transmit modem$ + CRLF$
    z = 0
    Response z, "OK"
    IF z = -2 THEN closecomm: OpenComm Port, irq%, 8, 0, baud&, HS, 0: EXIT DO
    choice$ = INKEY$
    IF z >= 0 THEN EXIT DO
    SLEEP 2
  LOOP UNTIL choice$ = CHR$(27)
  '
  IF z >= 0 THEN    'if we got an OK
    DO
      transmit dial$ + phone$ + CRLF$
      z = wate
      Response z, "CONNECT"
      IF z >= 0 THEN EXIT DO
      SLEEP 3
      choice$ = INKEY$
    LOOP UNTIL choice$ = CHR$(27)
    IF choice$ = CHR$(27) THEN
      ClearInputBuffer
      transmit OffHook$ + CRLF$
      z = 0
      Response z, "OK"
      carrierdetect 2
    END IF
    LINE (4, 336)-(634, 344), 9, BF
  ELSE
    carrierdetect 2
  END IF
  dowindo 11, 15, 8
EXIT SUB
getcfg:
  IF Exists(file$) THEN
    FOR x = 1 TO 4
      inline$ = BufIn$(file$, done, reading)
      SELECT CASE x
        CASE 1: modem$ = MID$(inline$, 22)
        CASE 2: OffHook$ = MID$(inline$, 22)
        CASE 3: dial$ = MID$(inline$, 22)
        CASE 4: wate = VAL(MID$(inline$, 22))
      END SELECT
    NEXT x
    inline$ = BufIn$(file$, done, reading)
    REDIM phone$(1 TO 5)
    FOR x = 1 TO 5
      phone$(x) = BufIn$(file$, done, reading)
      IF done THEN x = 5
    NEXT x
    y = 260: v = 10
    icon y - 3, v - 3, 346, 96, 15, 8, 7, 0
    icon y, v, 340, 90, 8, 15, 7, 0
    nz = DRAWSTR%(0, "Choose BBS To Dial", y + 100, v + 6, 4, 7, 8)
    v = v + 30
    FOR x = 1 TO 5
      a = INSTR(phone$(x), ":")
      IF a <> 0 THEN
        nz = DRAWSTR%(0, STR$(x) + ">", y + 10, v, 4, 7, 8)
        nz = DRAWSTR%(0, LEFT$(phone$(x), a - 1), y + 45, v, 1, 7, 8)
        nz = DRAWSTR%(0, MID$(phone$(x), a + 1), y + 181, v, 1, 7, 8)
        phone$(x) = MID$(phone$(x), a + 1)
      END IF
      v = v + 9
    NEXT x
    choice = inkey(0, 0)
    choice = choice - 48
    phone$ = ""
    IF choice > 0 AND choice < 6 THEN
      FOR x = 1 TO LEN(phone$(choice))
        IF MID$(phone$(choice), x, 1) <> "-" THEN phone$ = phone$ + MID$(phone$(choice), x, 1)
      NEXT x
    END IF
    IF phone$ = "" THEN dowindo 11, 15, 8: EXIT SUB
  ELSE
    ermes "You need to run SETUP.EXE first!", snd
    EXIT SUB
  END IF
RETURN
END SUB

SUB disguise
mousepointer 0
LINE (4, 336)-(634, 344), 9, BF
nz = DRAWSTR%(0, "Now Loading Help File!", 238, 337, 15, 9, 8)
file$ = "TTA.HLP"
IF Exists(file$) THEN
  x = 0
  DO
    x = x + 1
    info$(1) = BufIn$(file$, done, reading)
  LOOP UNTIL done
  REDIM info$(1 TO x)
  x = 0
  DO
    x = x + 1
    info$(x) = BufIn$("TTA.hlp", done, reading)
  LOOP UNTIL done
END IF
LINE (4, 336)-(634, 344), 9, BF
mousepointer 1
END SUB

SUB DoAltBar
mousepointer 0
cl = 1
LINE (0, 193)-(639, 201), 3, BF
LINE (0, 193)-(639, 193), 11
nz = DRAWSTR%(0, "A   B   C   D   E   G   H   I   J   K   L   M   N   O   P   R   S   T ", 40, 194, cl, 3, 8)
mousepointer 1
END SUB

SUB dooptions (keyin$)
SELECT CASE RIGHT$(keyin$, 1)       '0 #$%&
CASE ";"                                  'f1
  MacroScreen menusav()
CASE "<"                                  'f2
  config
CASE "="                            'F3
  viewfile "*.map", choice$
CASE ">"                            'F4
  IF aexit = 1 THEN
    aexit = 0: dowindo 4, 15, 8
  ELSE
    aexit = 1: dowindo 4, 8, 15: transmit "EX" + CHR$(13)
  END IF
CASE "?"                            'F5
  IF ashare = 1 THEN
    ashare = 0: dowindo 5, 15, 8
  ELSE
    ashare = 1: dowindo 5, 8, 15
  END IF
CASE "@"                          'F6
  IF aroam = 1 THEN
    aroam = 0: dowindo 6, 15, 8: firstr = 0
  ELSE
    aroam = 1: dowindo 6, 8, 15
    IF aexit = 0 THEN aexit = 1: dowindo 4, 8, 15
    transmit "EX" + CHR$(13)
  END IF
CASE "A"                             'F7
  IF afeed = 0 THEN
    afeed = 1: dowindo 7, 8, 15
  ELSE
    afeed = 0: dowindo 7, 15, 8
  END IF
CASE "B"                             'F8
  IF CarrierLost THEN
    ermes "Sorry, need to be online to do your rerolls.", snd
  ELSE
    reroll
  END IF
CASE "C"                               'f9
  IF aattack = 1 THEN
    aattack = 0: dowindo 9, 15, 8
    doattack = 0: getready = 0
  ELSE
    aattack = 1: dowindo 9, 8, 15
  END IF
CASE "D"                              'f10
  IF aheal = 0 THEN
    aheal = 1: dowindo 10, 8, 15
  ELSE
    aheal = 0: dowindo 10, 15, 8
  END IF
CASE "O", "P", "Q", "M", "I", "H", "G", "K", "R", "S"
  IF CarrierLost% <> 1 THEN
    SELECT CASE RIGHT$(keyin$, 1)
      CASE "O": transmit "SW" + CHR$(13)
      CASE "P": transmit "S" + CHR$(13)
      CASE "Q": transmit "SE" + CHR$(13)
      CASE "M": transmit "E" + CHR$(13)
      CASE "I": transmit "NE" + CHR$(13)
      CASE "H": transmit "N" + CHR$(13)
      CASE "G": transmit "NW" + CHR$(13)
      CASE "K": transmit "W" + CHR$(13)
      CASE "R": transmit "UP" + CHR$(13)
      CASE "S": transmit "DOWN" + CHR$(13)
    END SELECT
    doattack = 0: getready = 0
  END IF
CASE ELSE
  domacro scrollback$(), keyin$, macro$(), recon, line$, where
END SELECT
keyin$ = ""
END SUB

SUB dowindo (num, cl1, cl2)
REDIM button(512)
IF snd = 1 THEN soundfx 2
SELECT CASE num
 CASE IS < 8: x = 4: z = 204 + ((num - 1) * 18)
 CASE 8 TO 10: x = 107: z = 204 + ((num - 4) * 18)
 CASE 11 TO 17: x = 434: z = 204 + ((num - 11) * 18)
 CASE 18 TO 23: x = 537: z = 204 + ((num - 17) * 18)
END SELECT
mousepointer 0
GET (x + 2, z + 3)-(x + 97, z + 14), button
icon x, z, 99, 16, cl1, cl2, 7, 0
IF cl1 = 8 THEN PUT (x + 2, z + 4), button, PSET ELSE PUT (x + 2, z + 2), button, PSET
mousepointer 1
END SUB

SUB emul (st$) STATIC
IF LEN(st$) > 2 THEN
  z = INSTR(st$, CRLF$)
  st$ = MID$(st$, z + 1)
END IF
FOR disp0% = 1 TO LEN(st$)
  ch0$ = MID$(st$, disp0%, 1)
  IF ANSIcode0% THEN
    IF LEFT$(ANSIst0$, 2) = "[M" OR LEFT$(ANSIst0$, 2) = "[m" THEN
      IF ASC(ch0$) = 14 THEN
        ANSIst0$ = ""
        ANSIcode0% = 0
      ELSE
        ANSIst0$ = ANSIst0$ + ch0$
      END IF
    ELSEIF INSTR("ABCDEFGHIJKLMNOPQRSTUVWXYZ", UCASE$(ch0$)) THEN
         SELECT CASE ch0$
            CASE "A": GOSUB CursorUp0
            CASE "B": GOSUB CursorDown0
            CASE "C": GOSUB CursorRight0
            CASE "D": GOSUB CursorLeft0
            CASE "H", "f": GOSUB CursorLocate0
            CASE "s": GOSUB SaveCursorPosn0
            CASE "u": GOSUB RestCursorPosn0
            CASE "J": GOSUB BigErase0
            CASE "K": GOSUB SmallErase0
            CASE "h", "l": REM  set display mode... ignored
            CASE "m": GOSUB SetColors0
            CASE ELSE
               PRINT ANSIst0$;
               ANSIcode0% = 0: ANSIst0$ = ""
         END SELECT
         ANSIst0$ = ""
         ANSIcode0% = 0
      ELSEIF ASC(ch0$) <= 32 OR LEN(ANSIst0$) > 60 THEN   'special chr
         ANSIcode0% = 0: ANSIst0$ = ""
      ELSE
         ANSIst0$ = ANSIst0$ + ch0$
      END IF
   ELSEIF ASC(ch0$) = 27 THEN       'turn on ansi characters
      ANSIcode0% = -1
      ANSIst0$ = ""
   ELSE
     IF ch0$ = CHR$(10) THEN                    'end of line
       PRINT ch0$;
       IF log$ = "ON" THEN PRINT #6, line$
       alertchek
       updateSBack scrollcnt, scrollback$(), line$
       col = POS(0): row = CSRLIN
       IF row% > maxline THEN
         ScrnScroll 1, 1, maxline, 80, 1, 0
         x = 184
         LOCATE maxline, 1
       END IF
       line$ = ""
     ELSEIF ch0$ <> CHR$(13) THEN '                        if not end of line
       doprint ch0$, line$, fore0, back0, crai, row, col  'print to screen
       IF col > 80 THEN col = 80
       LOCATE row, col
     END IF
   END IF
NEXT
EXIT SUB
'
CursorUp0:
Tmp0% = ival%(MID$(ANSIst0$, 2))
IF Tmp0% < 1 THEN Tmp0% = 1
row0% = CSRLIN: col0% = POS(0)
row0% = row0% - Tmp0%
IF row0% < 1 THEN row0% = 1
LOCATE row0%, col0%
RETURN
'
CursorDown0:
Tmp0% = ival%(MID$(ANSIst0$, 2))
IF Tmp0% < 1 THEN Tmp0% = 1
row0% = CSRLIN: col0% = POS(0)
row0% = row0% + Tmp0%
IF row0% > 25 THEN row0% = 25
LOCATE row0%, col0%
RETURN
'
CursorLeft0:
Tmp0% = ival%(MID$(ANSIst0$, 2))
IF Tmp0% < 1 THEN Tmp0% = 1
row0% = CSRLIN: col0% = POS(0)
col0% = col0% - Tmp0%
IF col0% < 1 THEN col0% = 1
LOCATE row0%, col0%
RETURN
'
CursorRight0:
Tmp0% = ival%(MID$(ANSIst0$, 2))
IF Tmp0% < 1 THEN Tmp0% = 1
row0% = CSRLIN: col0% = POS(0)
col0% = col0% + Tmp0%
IF col0% > 80 THEN col0% = 80
LOCATE row0%, col0%
RETURN
'
CursorLocate0:
row0% = ival%(MID$(ANSIst0$, 2))
Tmp0% = INSTR(ANSIst0$, ";")
IF Tmp0% THEN
   col0% = ival%(MID$(ANSIst0$, Tmp0% + 1))
ELSE
   col0% = 1
END IF
IF row0% < 1 THEN
   row0% = 1
ELSEIF row0% > 25 THEN
   row0% = 25
END IF
IF col0% < 1 THEN
   col0% = 1
ELSEIF col0% > 80 THEN
   col0% = 80
END IF
LOCATE row0%, col0%
RETURN
'
SaveCursorPosn0:
SaveRow0% = CSRLIN: SaveCol0% = POS(0)
RETURN
'
RestCursorPosn0:
IF SaveRow0% > 0 THEN LOCATE SaveRow0%, SaveCol0%
RETURN
'
BigErase0:
LINE (0, 0)-(639, 192), 0, BF
LOCATE 1, 1
RETURN
'
SmallErase0:
row0% = CSRLIN: col0% = POS(0)
ver% = row0% * 8 - 8
remain = 80 - col0%
FOR x = 1 TO remain + 1
  hor% = col0% * 8 - 8
  nz = DRAWSTR%(0, " ", hor%, ver%, fore0%, back0%, 8)
  col0% = col0% + 1
NEXT
IF col0% > 80 THEN col0% = 80
LOCATE row0%, col0%
RETURN
'
SetColors0:
ANSIst0$ = MID$(ANSIst0$, 2)
DO WHILE LEN(ANSIst0$)
  Tmp0% = ival%(ANSIst0$)
  SELECT CASE Tmp0%
    CASE 0: fore0% = 7: back0% = 0             ' reset colors
    CASE 1: fore0% = (fore0% OR 8)             ' high intensity
    CASE 2: fore0% = (fore0% AND &H17)         ' normal intensity
    CASE 5: fore0% = (fore0% OR 16)            ' blink
    CASE 7: fore0% = 0: back0% = 7             ' reverse video
    CASE 8: fore0% = 0: back0% = 0            ' invisible
    CASE 30: fore0% = (fore0% AND &H18)        ' black foreground
    CASE 31: fore0% = (fore0% AND &H18) OR 4   ' red foreground
    CASE 32: fore0% = (fore0% AND &H18) OR 2   ' green foreground
    CASE 33: fore0% = (fore0% AND &H18) OR 6   ' yellow foreground
    CASE 34: fore0% = (fore0% AND &H18) OR 1   ' blue foreground
    CASE 35: fore0% = (fore0% AND &H18) OR 5   ' magenta foreground
    CASE 36: fore0% = (fore0% AND &H18) OR 3   ' cyan foreground
    CASE 37: fore0% = (fore0% OR 7)            ' white foreground
    CASE 40: back0% = 0                        ' black background
    CASE 41: back0% = 4                        ' red background
    CASE 42: back0% = 2                        ' green background
    CASE 43: back0% = 6                        ' yellow background
    CASE 44: back0% = 1                        ' blue background
    CASE 45: back0% = 5                        ' magenta background
    CASE 46: back0% = 3                        ' cyan background
    CASE 47: back0% = 7                        ' white background
    CASE ELSE                                  ' ignore anything weird
  END SELECT
  Tmp0% = INSTR(ANSIst0$, ";")
  IF Tmp0% THEN
    ANSIst0$ = MID$(ANSIst0$, Tmp0% + 1)
  ELSE
    ANSIst0$ = ""
  END IF
LOOP
RETURN
END SUB

SUB findit (what$, offset, sw, which, scrollback$())
offset = 0
FOR x = 140 TO 1 STEP -1
  IF which = 1 THEN
    sw = INSTR(scrollback$(x), what$)
    IF sw <> 0 THEN offset = x: x = 1
  ELSE
    IF LEFT$(scrollback$(x), LEN(what$)) = what$ THEN offset = x: x = 1
  END IF
NEXT x
END SUB

SUB init (dosfg, dosbg, regname$)
'------- saves original colors
SCREEN 0
SHELL "CLS"
DEF SEG = &HB800
dosclr = PEEK(1)
dosfg = dosclr AND 15
dosbg = (dosclr \ 16) AND 7
DEF SEG
'---------  get regname if exists
regit = 0                            'checks if registered
IF Exists("TTAPower.PAK") THEN
  OPEN "i", 1, "TTAPOWER.PAK"
  LINE INPUT #1, regname$
  LINE INPUT #1, key1$
  key$ = "willy-boy"
  CALL Crypt(regname$, key$, 10)
  IF key$ = key1$ THEN regit = 1
  CLOSE #1
END IF
FOR x = 1 TO LEN(regname$)
  IF MID$(regname$, x, 1) < CHR$(60) OR MID$(regname$, x, 1) > CHR$(125) THEN
    IF MID$(regname$, x, 1) = CHR$(46) THEN
    ELSEIF MID$(regname$, x, 1) = CHR$(32) THEN
    ELSE
      regit = 0: x = LEN(regname$)
    END IF
  END IF
NEXT x
'--------  loads in databases
loadmac monsize, regit     ' loads in monster types , and any macros
LOADhlp hlp$()              ' loads in the choices of help menu
loadspell                   ' loads in the spell descriptions
'------ loads settings/data -----------------
log$ = "OFF": maxline = 24: CRLF$ = CHR$(10) + CHR$(13)
savecfg 1
'--------  display screen
menubar2
scrollcnt = 140
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

SUB loadspell
IF Exists("tTaspell.dat") THEN
  filenum = FREEFILE
  OPEN "I", filenum, "tTaspell.dat"
  LINE INPUT #filenum, inline$
  x = VAL(RIGHT$(RTRIM$(inline$), 2))
  REDIM spells(1 TO x) AS STRING * 32: spelsize = x
  FOR y = 1 TO x
    LINE INPUT #filenum, spells(y)
  NEXT y
  CLOSE filenum
ELSE
  ermes "Sorry, you are missing TTASPELL.DAT file. Spells bypassed.", 1
END IF
END SUB

SUB makepath

END SUB

SUB menubar2
SCREEN 9, 0, 0, 1
WIDTH 80, 43
DEF SEG = VARSEG(menusav(1))
BLOAD "tTAscrn0.gfx", VARPTR(menusav(1))
DEF SEG
PUT (0, 201), menusav
SCREEN 9, 0, 0, 0
PRINT "TermTA 3.1     Copyright 1995 by Will Boyett"
DoAltBar
x = snd: snd = 0
IF aexit = 1 THEN dowindo 4, 8, 15
IF afeed = 1 THEN dowindo 7, 8, 15
IF aheal = 1 THEN dowindo 10, 8, 15
snd = x
END SUB

SUB menus
row% = CSRLIN: col% = POS(0)
LINE (285, 205)-(636, 293), 7, BF
pixy = 206: ver = 290
SELECT CASE where
CASE 1
  RESTORE
CASE 2
  'RESTORE COMP
CASE 3
  'R'ESTORE Planet
  ver = 338
CASE 4
  'RESTORE star
  ver = 338
CASE 5
  'RESTORE corp
CASE 6
  'ESTORE citadel
CASE 7
  'ESTORE hard
CASE 8
  'RESTORE tav
  ver = 338
CASE ELSE
  'RESTORE
END SELECT
FOR offset = 1 TO 22
  READ sel$: IF sel$ <> "" THEN nz = DRAWSTR%(0, "( ) " + sel$, ver, pixy, 0, 7, 7)
  READ sel$: nz = DRAWSTR%(0, sel$, ver + 7, pixy, 4, 7, 7)
  pixy = pixy + 8
  IF offset = 11 THEN ver = 496: pixy = 206
NEXT
IF where = 1 OR where = 2 THEN
  ver = 432: pixy = 214
  IF where = 1 THEN y = 3 ELSE y = 4: ver = 412
  FOR x = 1 TO y
    READ sel$: nz = DRAWSTR%(0, "( ) " + sel$, ver, pixy, 0, 7, 7)
    READ sel$: nz = DRAWSTR%(0, sel$, ver + 7, pixy, 4, 7, 7)
    pixy = pixy + 8
  NEXT x
END IF
LOCATE row%, col%
END SUB

DEFSNG A-Z
SUB MouseArea (y1%, y2%, min%, max%)
 '** Define area for mouse movement, y1% thru y2%
 IF nomouse% THEN EXIT SUB
 InRegs.ax = 8
 InRegs.cx = y1%
 InRegs.dx = y2%
 CALL InterruptX(&H33, InRegs, OutRegs)
 InRegs.ax% = &H7
 InRegs.cx% = min%
 InRegs.dx% = max%
 CALL InterruptX(&H33, InRegs, OutRegs)
END SUB

DEFINT A-Z
SUB mousefind (keyin$)
IF mousev < 204 THEN
  SELECT CASE mouseh
    CASE 35 TO 50: keyin$ = CHR$(0) + ""
    CASE 68 TO 83: keyin$ = CHR$(0) + "0"
    CASE 100 TO 115: keyin$ = CHR$(0) + "."
    CASE 132 TO 147: keyin$ = CHR$(0) + " "
    CASE 164 TO 179: keyin$ = CHR$(0) + ""
    CASE 196 TO 211: keyin$ = CHR$(0) + CHR$(34)
    CASE 228 TO 243: keyin$ = CHR$(0) + "#"
    CASE 261 TO 276: keyin$ = CHR$(0) + ""
    CASE 293 TO 308: keyin$ = CHR$(0) + "$"
    CASE 325 TO 340: keyin$ = CHR$(0) + "%"
    CASE 357 TO 372: keyin$ = CHR$(0) + "&"
    CASE 389 TO 404: keyin$ = CHR$(0) + "2"
    CASE 421 TO 436: keyin$ = CHR$(0) + "1"
    CASE 453 TO 468: keyin$ = CHR$(0) + ""
    CASE 485 TO 500: keyin$ = CHR$(0) + ""
    CASE 517 TO 532: keyin$ = CHR$(0) + ""
    CASE 549 TO 564: keyin$ = CHR$(0) + ""
    CASE 580 TO 595: keyin$ = CHR$(0) + ""
  END SELECT
  EXIT SUB
END IF
SELECT CASE mouseh
  CASE 6 TO 104
    SELECT CASE mousev
      CASE 206 TO 221: keyin$ = CHR$(0) + ";" 'F1
      CASE 222 TO 239: keyin$ = CHR$(0) + "<" 'F2
      CASE 240 TO 257: keyin$ = CHR$(0) + "="   'F3
      CASE 258 TO 275: keyin$ = CHR$(0) + ">" 'F4
      CASE 276 TO 293: keyin$ = CHR$(0) + "?" 'F5
      CASE 294 TO 311: keyin$ = CHR$(0) + "@"'F6
      CASE IS > 311: keyin$ = CHR$(0) + "A" 'F7
    END SELECT
  CASE 107 TO 206
    SELECT CASE mousev
      CASE 276 TO 293: keyin$ = CHR$(0) + "B"   'F8
      CASE 294 TO 311: keyin$ = CHR$(0) + "C" 'F9
      CASE IS > 311: keyin$ = CHR$(0) + "D" 'F10
    END SELECT
  CASE 435 TO 532
    SELECT CASE mousev
      CASE 206 TO 221: keyin$ = CHR$(1)
      CASE 222 TO 239: keyin$ = CHR$(4)
      CASE 240 TO 257: keyin$ = CHR$(16)
      CASE 258 TO 275: keyin$ = CHR$(6)
      CASE 276 TO 293: keyin$ = CHR$(24)
      CASE 294 TO 311: keyin$ = CHR$(19)
      CASE IS > 311: keyin$ = CHR$(7)
    END SELECT
  CASE 538 TO 635
    SELECT CASE mousev
      CASE 222 TO 239: keyin$ = CHR$(2)
      CASE 240 TO 257: keyin$ = CHR$(3)
      CASE 258 TO 275: keyin$ = CHR$(12)
      CASE 276 TO 293: keyin$ = CHR$(11)
      CASE 294 TO 311: keyin$ = CHR$(9)
      CASE IS > 311: keyin$ = CHR$(5)
    END SELECT
  CASE 317 TO 352
    SELECT CASE mousev
      CASE 228 TO 241: temp$ = "U" + CHR$(13)
      CASE 251 TO 271: temp$ = "NW" + CHR$(13)
      CASE 275 TO 297: temp$ = "W" + CHR$(13)
      CASE 302 TO 322: temp$ = "SW" + CHR$(13)
    END SELECT
  CASE 353 TO 384
    SELECT CASE mousev
      CASE 251 TO 271: temp$ = "N" + CHR$(13)
      CASE 302 TO 322: temp$ = "S" + CHR$(13)
    END SELECT
  CASE 385 TO 423
    SELECT CASE mousev
      CASE 228 TO 241: temp$ = "D" + CHR$(13)
      CASE 251 TO 271: temp$ = "NE" + CHR$(13)
      CASE 275 TO 297: temp$ = "E" + CHR$(13)
      CASE 302 TO 322: temp$ = "SE" + CHR$(13)
    END SELECT
END SELECT
IF temp$ <> "" THEN
  IF CarrierLost% THEN
    ermes "Need to be on-line to move.", 1
  ELSE
    transmit temp$
    doattack = 0: getready = 0
  END IF
END IF
END SUB

SUB mouseget (mouseb)
  '*** Get mouse position and button status: mouseh%, mousev% and MouseB%
  IF nomouse% THEN EXIT SUB
  InRegs.ax = 3
  CALL InterruptX(&H33, InRegs, OutRegs)
  mouseb% = OutRegs.bx
  mouseh% = OutRegs.cx
  mousev% = OutRegs.dx
END SUB

DEFSNG A-Z
SUB mouseinit
 '*** Initialize mouse driver
 IF nomouse% THEN EXIT SUB
 InRegs.ax = 0                  '** Mouse initialization
 CALL InterruptX(&H33, InRegs, OutRegs)
 IF OutRegs.ax = 0 THEN nomouse% = 1: EXIT SUB   '*** no mouse driver
 InRegs.ax = 10                 '** Mouse should blink
 InRegs.bx = &H0
 InRegs.cx = &HFFFF
 InRegs.dx = &HFF00
 CALL InterruptX(&H33, InRegs, OutRegs)
 mousespeed 7            'set speed of mouse 0-9
END SUB

SUB mousepointer (OnOff%)
   '*** mouse pointer on/off
 IF nomouse% THEN EXIT SUB
 IF OnOff% = 0 THEN OnOff% = 2 ELSE OnOff% = 1
 InRegs.ax = OnOff%
 CALL InterruptX(&H33, InRegs, OutRegs)
 IF OnOff% = 1 THEN CALL InterruptX(&H33, InRegs, OutRegs) '** to be sure
END SUB

SUB mousespeed (Speed%)
   '*** Set mouse speed
 IF nomouse% THEN EXIT SUB
 InRegs.ax = &H15
 InRegs.cx = Speed%
 InRegs.dx = Speed% * 2
 CALL InterruptX(&H33, InRegs, OutRegs)
END SUB

DEFINT A-Z
SUB parseexits (ex$(), numexits)
REDIM ex$(1 TO 10)
temp$ = MID$(line$, 8, LEN(line$) - 8)
numexits = 0
DO
  numexits = numexits + 1
  z = INSTR(temp$, ",")
  IF z <> 0 THEN
    ex$(numexits) = LEFT$(temp$, z - 1): temp$ = MID$(temp$, z + 1)
  ELSE
    ex$(numexits) = temp$: temp$ = ""
  END IF
LOOP UNTIL temp$ = ""
END SUB

SUB recorder STATIC
file$ = "TERMTA.rec"
IF recfirst = 0 THEN
  IF CarrierLost THEN
    IF Exists(file$) THEN
      OPEN "i", 7, file$: recon = 1
      recfirst = 1
      IF LOF(7) = 0 THEN
        sw = 2: GOSUB clserec
        sw = 1: GOSUB clserec
        KILL file$
      END IF
    ELSE
      sw = 2: GOSUB clserec
    END IF
  ELSE
    OPEN "o", 7, file$: recon = 2: recfirst = 1
  END IF
ELSE
  sw = 1: GOSUB clserec
END IF
EXIT SUB
'
clserec:
SELECT CASE sw
CASE 1
  CLOSE #7: recon = 0: recfirst = 0
  IF crai <> 0 THEN crai = 0: LINE (4, 336)-(634, 344), 9, BF
CASE 2
  ermes "No Recording Found!", 1
END SELECT
RETURN
END SUB

SUB reroll
 dowindo 8, 8, 15
 inbar "Are you sure you want to REROLL?", choice$, 1, 14, 0, 9, snd
 IF UCASE$(choice$) = "Y" THEN
   mousepointer 0
   nz = DRAWSTR%(0, "Does your BBS require you to rest between rerolls?  y/N", 100, 337, 14, 9, 8)
   DO: choice$ = UCASE$(INKEY$): LOOP UNTIL choice$ <> ""
   IF choice$ = "Y" THEN
     LINE (4, 336)-(634, 344), 9, BF
     inbar "How many seconds would you like to rest between rerolls? ", choice$, 2, 15, 0, 9, snd
     rest = VAL(choice$)
   END IF
   inbar "What is the minimum amount of INTELLECT you will accept? ", choice$, 2, 15, 0, 9, snd
   IF LEN(choice$) <> 2 THEN choice$ = " " + choice$
   temp$ = choice$
   inbar "What is the minimum amount of KNOWLEDGE you will accept? ", choice$, 2, 15, 0, 9, snd
   IF LEN(choice$) <> 2 THEN choice$ = " " + choice$
   temp$ = temp$ + " " + choice$
   inbar "What is the minimum amount of PHYSIQUE you will accept? ", choice$, 2, 15, 0, 9, snd
   IF LEN(choice$) <> 2 THEN choice$ = " " + choice$
   temp$ = temp$ + " " + choice$
   inbar "What is the minimum amount of STAMINA you will accept? ", choice$, 2, 15, 0, 9, snd
   IF LEN(choice$) <> 2 THEN choice$ = " " + choice$
   temp$ = temp$ + " " + choice$
   inbar "What is the minimum amount of AGILITY you will accept? ", choice$, 2, 15, 0, 9, snd
   IF LEN(choice$) <> 2 THEN choice$ = " " + choice$
   temp$ = temp$ + " " + choice$
   inbar "What is the minimum amount of CHARISMA you will accept? ", choice$, 2, 15, 0, 9, snd
   IF LEN(choice$) <> 2 THEN choice$ = " " + choice$
   mousepointer 0
   temp$ = temp$ + " " + choice$
   nz = DRAWSTR%(0, "Continue with these figures?   " + temp$, 132, 337, 14, 9, 8)
   DO: choice$ = UCASE$(INKEY$): LOOP UNTIL choice$ <> ""
   LINE (4, 336)-(634, 344), 9, BF
   IF choice$ = "Y" OR choice$ = CHR$(13) THEN
     hilite "When Done:  &Log Off   &Drop Carrier   &Exit TA   Do &Nothing", 14, 9, 0, 4
     DO: choice$ = UCASE$(INKEY$): LOOP UNTIL choice$ <> ""
     LINE (4, 336)-(634, 344), 9, BF
     y = 0
     DIM bst(1 TO 6)
     GET (120, 204)-(518, 285), menusav
     icon 120, 204, 398, 81, 0, 15, 8, 0
     nz = DRAWSTR%(0, "Maximum Values Found On Each Stat Since Starting Reroll", 128, 210, 15, 8, 7)
     nz = DRAWSTR%(0, "INTEL  KNOW  PHYS  STAM  AGIL  CHAR", 180, 231, 7, 8, 8)
     nz = DRAWSTR%(0, "Your Goal:", 200, 270, 7, 8, 8)
     nz = DRAWSTR%(0, temp$, 304, 270, 15, 8, 8)
     DO
       z = 188
       FOR x = 1 TO 6
         nz = DRAWSTR%(0, STR$(bst(x)), z, 246, 14, 8, 8)
         z = z + 48
       NEXT x
       IF INKEY$ = CHR$(27) THEN EXIT DO
       total = 0: y = y + 1
       IF y = 30 THEN
         y = 1
         IF regit = 0 THEN
           transmit "Sorry, can't talk right now. Am rerolling with TermTA 3.1" + CHR$(13)
         '#ELSE
         '  transmit "Away from the keyboard, BRB!" + CHR$(13)
         END IF
       END IF
       domacro scrollback$(), CHR$(0) + ",", macro$(), recon, line$, where
       findit "Intellect:", offset, sw, 0, scrollback$()
       x = VAL(MID$(scrollback$(offset), 15, 2))
       IF x >= VAL(MID$(temp$, 1, 2)) THEN total = total + 1
       IF x > bst(1) THEN bst(1) = x
       offset = offset + 1
       x = VAL(MID$(scrollback$(offset), 15, 2))
       IF x >= VAL(MID$(temp$, 4, 2)) THEN total = total + 1
       IF x > bst(2) THEN bst(2) = x
       offset = offset + 1
       x = VAL(MID$(scrollback$(offset), 15, 2))
       IF x >= VAL(MID$(temp$, 7, 2)) THEN total = total + 1
       IF x > bst(3) THEN bst(3) = x
       offset = offset + 1
       x = VAL(MID$(scrollback$(offset), 15, 2))
       IF x >= VAL(MID$(temp$, 10, 2)) THEN total = total + 1
       IF x > bst(4) THEN bst(4) = x
       offset = offset + 1
       x = VAL(MID$(scrollback$(offset), 15, 2))
       IF x >= VAL(MID$(temp$, 13, 2)) THEN total = total + 1
       IF x > bst(5) THEN bst(5) = x
       offset = offset + 1
       x = VAL(MID$(scrollback$(offset), 15, 2))
       IF x >= VAL(MID$(temp$, 16, 2)) THEN total = total + 1
       IF x > bst(6) THEN bst(6) = x
     LOOP UNTIL total = 6
     mousepointer 0
     PUT (120, 204), menusav, PSET
     soundfx 1
   END IF
 END IF
 rest = 0
 dowindo 8, 15, 8
 SELECT CASE choice$
   CASE "E"
     GOSUB final
   CASE "D"
     GOSUB final
     DTRcontrol 0
   CASE "L"
     GOSUB final
     IF choice$ <> CHR$(27) THEN SLEEP 2: transmit "=x" + CHR$(13)
 END SELECT
EXIT SUB
final:
  mousepointer 0
  nz = DRAWSTR%(0, "Hit <ESC> to stay in TA or wait 15 seconds to exit.", 108, 337, 15, 9, 8)
  SLEEP 15
  mousepointer 1
  ch$ = INKEY$
  LINE (4, 336)-(634, 344), 9, BF
  IF ch$ <> CHR$(27) THEN
    IF choice$ = "D" THEN RETURN
    domacro scrollback$(), "", macro$(), recon, line$, where
  END IF
RETURN
END SUB

SUB savecfg (sw)
file$ = bbs$ + ".cfg"
z = FREEFILE
IF sw = 0 THEN
  OPEN "O", #z, file$
  PRINT #z, snd%, afeed, anum
  PRINT #z, Port%, HS, atak$(11)
  PRINT #z, irq%, baud&, await
  WRITE #z, atak$(1), atak$(2), atak$(3)
  WRITE #z, atak$(4), atak$(5), atak$(6), atak$(7)
  WRITE #z, uinput$(2), atak$(8), atak$(9), atak$(10)
  PRINT #z, aheal, awhen
  PRINT #z, aexit, ahave, atak$(13)
ELSE
  OPEN "I", z, file$
  INPUT #z, snd%, afeed, anum
  INPUT #z, Port%, HS, atak$(11)
  INPUT #z, irq%, baud&, await
  INPUT #z, atak$(1), atak$(2), atak$(3)
  INPUT #z, atak$(4), atak$(5), atak$(6), atak$(7)
  INPUT #z, uinput$(2), atak$(8), atak$(9), atak$(10)
  INPUT #z, aheal, awhen
  IF NOT EOF(z) THEN INPUT #z, aexit, ahave, atak$(13)
END IF
CLOSE #z
END SUB

SUB scrollit (pntlow, pnthigh, size, snd)
IF pnthigh < 22 THEN pnthigh = 22: size = 22
DO
  DO
    choice$ = INKEY$
    mouseget mouseb
    IF mouseb = 2 THEN
      choice$ = "A"
    ELSEIF mouseb = 1 THEN
      SELECT CASE mouseh
        CASE 22 TO 47: choice$ = CHR$(0) + "H"
        CASE 48 TO 71: choice$ = CHR$(0) + "P"
        CASE 72 TO 134: choice$ = CHR$(0) + "I"
        CASE 135 TO 210: choice$ = CHR$(0) + "Q"
      END SELECT
    END IF
  LOOP UNTIL choice$ <> ""
  IF LEN(choice$) = 1 THEN EXIT DO
    SELECT CASE RIGHT$(choice$, 1)
      CASE "H"
        IF pntlow = 1 THEN
          IF snd = 1 THEN BEEP
          pnthigh = 22
        ELSE
          pntlow = pntlow - 1: pnthigh = pnthigh - 1
          ScrnScroll 3, 3, 24, 79, -1, 1
          nz = DRAWSTR%(0, help(pntlow), 17, 16, 8, 7, 8)
        END IF
      CASE "P"
        IF pnthigh >= size THEN
          IF snd = 1 THEN BEEP
        ELSE
          pntlow = pntlow + 1: pnthigh = pnthigh + 1
          ScrnScroll 3, 3, 24, 79, 1, 1
          nz = DRAWSTR%(0, help(pnthigh), 17, 184, 8, 7, 8)
        END IF
      CASE "Q"
        IF pnthigh >= size THEN
          IF snd = 1 THEN BEEP
        ELSE
          pntlow = pnthigh
          pnthigh = pntlow + 22
          IF pnthigh > size THEN pnthigh = size: pntlow = size - 21
          LINE (10, 7)-(632, 193), 7, BF
          FOR x = 2 TO 23
            nz = DRAWSTR%(0, help(pntlow + x - 2), 17, x * 8, 8, 7, 8)
          NEXT x
        END IF
      CASE "I"
        IF pntlow = 1 THEN
          IF snd = 1 THEN BEEP
        ELSE
          pnthigh = pntlow
          pntlow = pnthigh - 22
          IF pntlow < 1 THEN pntlow = 1: pnthigh = 22
          LINE (10, 7)-(632, 193), 7, BF
          FOR x = 2 TO 23
            nz = DRAWSTR%(0, help(pntlow + x - 2), 17, x * 8, 8, 7, 8)
          NEXT x
        END IF
      CASE ELSE
        z = 1
    END SELECT
LOOP UNTIL z = 1
END SUB

SUB sheltodos
row% = CSRLIN: col% = POS(0)
mousepointer 0
GET (0, 0)-(639, 201), menusav
WIDTH 80, 25
SCREEN 0
COLOR 14, 0: PRINT "Type EXIT to return to TermTA"
SHELL
SCREEN 9
WIDTH 80, 43
PUT (0, 0), menusav
DEF SEG = VARSEG(menusav(1))
BLOAD "tTAscrn0.gfx", VARPTR(menusav(1))
DEF SEG
PUT (0, 201), menusav
MouseArea 194, 330, 2, 635
IF log$ = "ON" THEN dowindo 19, 8, 15
IF aattack = 1 THEN dowindo 9, 8, 15
IF aexit = 1 THEN dowindo 4, 8, 15
IF afeed = 1 THEN dowindo 7, 8, 15
IF aheal = 1 THEN dowindo 10, 8, 15
IF aroam = 1 THEN dowindo 6, 8, 15
IF ashare = 1 THEN dowindo 5, 8, 15
mousepointer 1
LOCATE row%, col%
END SUB

SUB starlog (log$) STATIC
row% = CSRLIN: col% = POS(0)
IF log$ = "OFF" THEN
  log$ = "ON"
  dowindo 19, 8, 15
  inbar "Enter the filename (an ext of .CAP will be added) for your file: ", file$, 8, 14, 0, 9, snd
  IF file$ <> "" THEN
    file$ = LTRIM$(file$)
    IF LEN(file$) > 8 THEN file$ = LEFT$(file$, 8)
    a = INSTR(file$, ".")
    IF a <> 0 THEN file$ = LEFT$(file$, a - 1): a = 0
    file$ = file$ + ".CAP"
    IF Exists(file$) THEN
      IF snd = 1 THEN soundfx 6
      nz = DRAWSTR%(0, "Capture File already exists, Overwrite file (Y/n)?", 99, 337, 14, 9, 8)
      DO
        choice$ = UCASE$(INKEY$)
      LOOP UNTIL choice$ <> ""
      mousepointer 0
      LINE (4, 336)-(634, 344), 9, BF
      mousepointer 1
      IF choice$ = "N" THEN
        OPEN "A", 6, file$
      ELSE
        OPEN "o", 6, file$
      END IF
    ELSE
      OPEN "o", 6, file$
    END IF
    PRINT #6, DATE$ + "      *****************************"
  ELSE
    log$ = "OFF": file$ = ""
  END IF
ELSE
  log$ = "OFF": file$ = "": CLOSE #6
END IF
IF log$ = "OFF" THEN dowindo 19, 15, 8
LOCATE row%, col%
END SUB

SUB viewfile (ext$, choice$)
STATIC map$
row% = CSRLIN: col% = POS(0)
mousepointer 0
GET (0, 0)-(639, 201), menusav

SELECT CASE UCASE$(ext$)
  CASE "S"                             'scrollback
     dowindo 18, 8, 15
     REDIM help(1 TO 140) AS STRING * 76
     FOR x = 1 TO 140: help(x) = scrollback$(x): NEXT x
     size = 140: cnt = 16: pnthigh = size: pntlow = size - 21
     GOSUB display
     dowindo 18, 15, 8
  CASE "*.MAP"
     dowindo 3, 8, 15
     mousepointer 0
     cubeit 0, 0, 640, 201, 15, 8, 7
     icon 7, 6, 627, 189, 8, 15, 8, 0
     readdir "*.MAP"
     nz = DRAWSTR%(0, "List of Maps in current directory.", 176, 185, 15, 8, 8)
     inbar "Name of Map to view (press ENTER to display last map):", file$, 8, 14, 0, 9, snd
     file$ = file$ + ".MAP"
     IF Exists(file$) THEN
       map$ = file$
       GOSUB loadndisp
     ELSE
       IF file$ = ".MAP" THEN
         IF map$ = "" THEN
           ermes "You haven't chosen a map.", 1
         ELSE
           GOSUB loadndisp
         END IF
       ELSE
         ermes "You entered an incorrect Map name.", 1
       END IF
     END IF
     dowindo 3, 15, 8
  CASE "H"
    mousepointer 1
    x = 1: pntlow = 1: pnthigh = 22
    DO UNTIL LEFT$(info$(x), 6) = "help " + choice$
      x = x + 1
    LOOP
    x = x + 1: y = x
    DO UNTIL LEFT$(info$(y), 4) = "help"
      y = y + 1
    LOOP
    size = y - x
    IF size < pnthigh THEN pnthigh = size
    REDIM help(1 TO size) AS STRING * 76
    FOR z = 1 TO size
      help(z) = info$(x)
      x = x + 1
    NEXT z
    cnt = 16
    GOSUB display
  CASE "REDISP"
    dowindo 4, 8, 15
    GOSUB loadndisp
    dowindo 4, 15, 8
END SELECT
REDIM help(1) AS STRING * 76
mousepointer 0
LINE (0, 0)-(639, 201), 0, BF
PUT (0, 0), menusav, PSET
LINE (4, 336)-(634, 344), 9, BF
mousepointer 1
LOCATE row%, col%
EXIT SUB
'
loadndisp:
cnt = 0: pntlow = 1: pnthigh = 22
DO
  inline$ = BufIn$(map$, done, reading)
  cnt = cnt + 1
LOOP UNTIL done
IF cnt < pnthigh THEN pnthigh = cnt
IF cnt > 1700 THEN cnt = 1700
REDIM help(1 TO cnt) AS STRING * 76
x = 0
DO
  inline$ = BufIn$(map$, done, reading)
  x = x + 1
  IF x <= cnt THEN help(x) = inline$
LOOP UNTIL done
LINE (8, 7)-(633, 194), 7, BF
size = cnt: cnt = 16
GOSUB display
RETURN
'
display:
mousepointer 0
cubeit 0, 0, 640, 201, 15, 8, 7
icon 7, 6, 627, 189, 8, 15, 7, 0
FOR x = pntlow TO pnthigh
  nz = DRAWSTR%(0, help(x), 17, cnt, 8, 7, 8)
  cnt = cnt + 8
NEXT
LINE (4, 336)-(634, 344), 9, BF
nz = DRAWSTR%(0, "[     PageUp  PageDown ]   Any other key or right mouse button to return", 15, 337, 14, 9, 8)
MouseArea 335, 345, 2, 570
mousepointer 1
scrollit pntlow, pnthigh, size, snd
MouseArea 194, 330, 2, 635
RETURN
END SUB

SUB viewhelp
file$ = "TTAscrn1.gfx"
IF Exists(file$) THEN
  dowindo 13, 8, 15
  GET (0, 0)-(639, 200), menusav
  REDIM helpr(1 TO 12160)
  DEF SEG = VARSEG(helpr(1))
  BLOAD file$, VARPTR(helpr(1))
  DEF SEG                 ' Restore default BASIC segment.
  PUT (28, 0), helpr, PSET
  IF LEFT$(info$(1), 1) = "" THEN disguise
  z = 0
  MouseArea 2, 70, 40, 590
  DO
    choice$ = UCASE$(INKEY$)
    mouseget mouseb
    IF mouseb <> 0 THEN
      IF mouseb = 2 THEN
        choice$ = CHR$(13)
      ELSE
        SELECT CASE mouseh
          CASE IS < 145: y = 0
          CASE 145 TO 259: y = 1
          CASE 260 TO 374: y = 2
          CASE 375 TO 489: y = 3
          CASE IS > 489: y = 4
        END SELECT
        SELECT CASE mousev
          CASE IS < 21: x = 0
          CASE 21 TO 36: x = 1
          CASE 37 TO 53: x = 2
          CASE IS > 53: x = 3
        END SELECT
        choice$ = hlp$(x, y)
      END IF
    END IF
    WHILE mouseb%
      mouseget mouseb
    WEND
    IF choice$ = CHR$(13) OR choice$ = CHR$(27) THEN EXIT DO
    IF choice$ <> "" THEN
      IF INSTR("NCUOIMLADGSTPVEYXBHR", choice$) > 0 THEN
        mousepointer 0
        PUT (0, 0), menusav, PSET
        viewfile "H", choice$
        PUT (28, 0), helpr, PSET
        MouseArea 2, 70, 40, 590
      END IF
    END IF
  LOOP
  ERASE helpr
  MouseArea 194, 330, 2, 635
  PUT (0, 0), menusav, PSET
  dowindo 13, 15, 8
ELSE
  ermes "You are missing the TTASCRN1.GFX, graphic file!", 1
END IF
END SUB

