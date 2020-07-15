'#include "./fbsound2-1.1/inc/fbsound_dynamic.bi"
'SCREEN 0
'width 100 / 8 , 37 / 16
SCREEN _NEWIMAGE(800, 600, 256)
RANDOMIZE TIMER
'ReDim Shared As STRING pass()
DIM SHARED points, level, Time2, money, HhackIndicator, WhackIndicator, Wcounter, Ehacked AS INTEGER
CONST punctuation = "?!,.:;<>(){}[]"
DIM SHARED Greeting AS STRING, You AS STRING, Script AS STRING
DIM SHARED kCnt AS INTEGER, rCnt AS INTEGER, wCnt AS INTEGER, NoKeyFoundIndex AS INTEGER
REDIM SHARED keywords(0) AS STRING, replies(0) AS STRING, wordIn(0) AS STRING, wordOut(0) AS STRING
REDIM SHARED rStarts(0) AS INTEGER, rEnds(0) AS INTEGER, rIndex(0) AS INTEGER
DIM SHARED sophie_state, passport, ticket
DIM SHARED escapeMoney AS LONG
DIM SHARED h&
'~ sophie_state = 0
'~ passport = 0
'~ ticket = 0
'~ escapeMoney = 0
'CONST data_path = "..\data\"
'chdir(exepath())
Time2 = 1
HhackIndicator = 1
WhackIndicator = 1
money = 1000
opening
start
hints
main



'center print
SUB cp (row AS INTEGER, s AS STRING)
    LOCATE row, (100 - LEN(s)) / 2: PRINT s;
END SUB

SUB sound2 (f AS STRING)

    h& = _SNDOPEN(f)
    _SNDPLAY h&

    'SLEEP
    '_SNDSTOP h&

END SUB

SUB the_end (f AS STRING, soundChoice AS INTEGER)
    IF soundChoice = 0 THEN
        sound2 ("Vespers.wav")
    ELSEIF soundChoice = 1 THEN
        BEEP
        BEEP
        BEEP
        BEEP
        BEEP
    END IF
    CLS
    DIM buffer AS STRING, tmp AS STRING
    ''dim h as long = freefile()
    OPEN f FOR INPUT AS #1
    WHILE NOT EOF(1)
        LINE INPUT #1, tmp 'reading one line at a time
        buffer = buffer + tmp + CHR$(13) 'adding the newline manually
    WEND
    PRINT buffer
    CLOSE #1





    SLEEP
    _SNDSTOP h&
    END
END SUB

'append to the string array the string item
SUB sAppend (arr() AS STRING, item AS STRING)
    REDIM _PRESERVE arr(LBOUND(arr) TO UBOUND(arr) + 1) AS STRING
    arr(UBOUND(arr)) = item
END SUB
'append to the integer array the integer item
SUB nAppend (arr() AS INTEGER, item AS INTEGER)
    REDIM _PRESERVE arr(LBOUND(arr) TO UBOUND(arr) + 1) AS INTEGER
    arr(UBOUND(arr)) = item
END SUB

'append to the integer array the integer item
SUB nAppend2 (arr() AS SINGLE, item AS SINGLE)
    REDIM _PRESERVE arr(LBOUND(arr) TO UBOUND(arr) + 1) AS SINGLE
    arr(UBOUND(arr)) = item
END SUB




' pull data out of some script file
SUB LoadArrays (scriptFile AS STRING)
    DIM startR AS INTEGER, endR AS INTEGER, ReadingR AS INTEGER, temp AS INTEGER
    DIM fline AS STRING, kWord AS STRING

    OPEN scriptFile FOR INPUT AS #1
    WHILE EOF(1) = 0
        LINE INPUT #1, fline
        SELECT CASE LEFT$(fline$, 2)
            CASE "g:": Greeting = _TRIM$(MID$(fline, 3))
            CASE "y:": You = _TRIM$(MID$(fline, 3))
            CASE "c:": Script = _TRIM$(MID$(fline, 3))
            CASE "s:"
                wCnt = wCnt + 1: temp = INSTR(fline, ">")
                IF temp THEN
                    sAppend wordIn(), " " + _TRIM$(MID$(fline, 3, temp - 3)) + " "
                    sAppend wordOut(), " " + _TRIM$(MID$(fline, temp + 1)) + " "
                END IF
            CASE "r:"
                rCnt = rCnt + 1
                sAppend replies(), _TRIM$(MID$(fline, 3))
                IF NOT ReadingR THEN
                    ReadingR = -1
                    startR = rCnt
                END IF
            CASE "k:"
                IF ReadingR THEN
                    endR = rCnt
                    ReadingR = 0
                END IF
                IF rCnt THEN
                    kCnt = kCnt + 1
                    kWord = _TRIM$(MID$(fline, 3))
                    sAppend keywords(), " " + kWord + " "
                    nAppend rStarts(), startR
                    nAppend rIndex(), startR
                    nAppend rEnds(), endR
                    IF kWord = "nokeyfound" THEN NoKeyFoundIndex = kCnt
                END IF
            CASE "e:": EXIT WHILE
        END SELECT
    WEND
    CLOSE #1
    IF ReadingR THEN 'handle last bits
        endR = rCnt
        kCnt = kCnt + 1
        sAppend keywords(), "nokeyfound"
        nAppend rStarts(), startR
        nAppend rIndex(), startR
        nAppend rEnds(), endR
        NoKeyFoundIndex = kCnt
    END IF
END SUB

' =============================== here is the heart of ELIZA / Player function
FUNCTION GetReply$ ()
    DIM inpt AS STRING, tail AS STRING, answ AS STRING
    DIM kFlag AS INTEGER, k AS INTEGER, kFound AS INTEGER, l AS INTEGER, w AS INTEGER

    ' USER INPUT SECTION
    PRINT You + ": ";: LINE INPUT "", inpt
    IF LCASE$(inpt) = "q" OR LCASE$(inpt) = "x" OR LCASE$(inpt) = "goodbye" OR LCASE$(inpt) = "good night" OR LCASE$(inpt) = "bye" THEN
        GetReply$ = "Goodbye!": EXIT FUNCTION
    END IF
    inpt = " " + inpt + " " '<< need this because keywords embedded in spaces to ID whole words only
    inpt = isolatePunctuation$(inpt)
    FOR k = 1 TO kCnt 'loop through key words until we find a match
        kFound = INSTR(LCASE$(inpt), LCASE$(keywords(k)))
        IF kFound > 0 THEN '>>> need the following for * in some replies
            tail = " " + MID$(inpt, kFound + LEN(keywords(k)))
            FOR l = 1 TO LEN(tail) 'DO NOT USE INSTR
                FOR w = 1 TO wCnt 'swap words in tail if used there
                    IF LCASE$(MID$(tail, l, LEN(wordIn(w)))) = LCASE$(wordIn(w)) THEN 'swap words exit for
                        tail = MID$(tail, 1, l - 1) + wordOut(w) + MID$(tail, l + LEN(wordIn(w)))
                        EXIT FOR
                    END IF
                NEXT w
            NEXT l
            kFlag = -1
            EXIT FOR
        END IF
    NEXT
    IF kFlag = 0 THEN k = NoKeyFoundIndex
    answ = replies(INT((rEnds(k) - rStarts(k) + 1) * RND) + rStarts(k))
    'set pointer to next reply in rIndex array
    IF k = NoKeyFoundIndex THEN 'let's not get too predictable for most used set of replies
        rIndex(k) = INT((rEnds(k) - rStarts(k) + 1) * RND) + rStarts(k)
        'ELSE
        '    rIndex(k) = rIndex(k) + 1 'set next reply index then check it
        '    IF rIndex(k) > rEnds(k) THEN rIndex(k) = rStarts(k)
    END IF
    IF RIGHT$(answ, 1) <> "*" THEN GetReply$ = answ: EXIT FUNCTION 'oh so the * signal an append to reply!
    IF _TRIM$(tail) = "" THEN
        GetReply$ = "Please elaborate on, " + keywords(k)
    ELSE
        tail = joinPunctuation$(tail)
        GetReply$ = MID$(answ, 1, LEN(answ) - 1) + tail
    END IF
END FUNCTION

FUNCTION isolatePunctuation$ (s AS STRING)
    'isolate punctuation so when we look for key words they don't interfere
    DIM b AS STRING, i AS INTEGER
    b = ""
    FOR i = 1 TO LEN(s)
        IF INSTR(punctuation, MID$(s, i, 1)) > 0 THEN b = b + " " + MID$(s, i, 1) + " " ELSE b = b + MID$(s, i, 1)
    NEXT
    isolatePunctuation$ = b
END FUNCTION

FUNCTION joinPunctuation$ (s AS STRING)
    'undo isolatePuntuation$
    DIM b AS STRING, find AS STRING, i AS INTEGER, place AS INTEGER
    b = s
    FOR i = 1 TO LEN(punctuation)
        find = " " + MID$(punctuation, i, 1) + " "
        place = INSTR(b, find)
        WHILE place > 0
            IF place = 1 THEN
                b = MID$(punctuation, i, 1) + MID$(b, place + 3)
            ELSE
                b = MID$(b, 1, place - 1) + MID$(punctuation, i, 1) + MID$(b, place + 3)
            END IF
            place = INSTR(b, find)
        WEND
    NEXT
    joinPunctuation$ = b
END SUB



SUB slow (text AS STRING)
    REDIM speed(0) AS SINGLE
    RESTORE 6
    FOR i = 1 TO 5
        READ r
        nAppend2 speed(), r
    NEXT
    6 DATA 0.50,0.100,0.20,0.300,0.250
    FOR i = 1 TO LEN(text)
        PRINT MID$(text, i, 1);
        _DELAY speed(INT(RND * UBOUND(speed)))
    NEXT
END SUB

SUB speakTotext (lines AS STRING) 'uses voice command line voice.exe
    PRINT Script + ": ";: slow lines: PRINT: PRINT
    '    Shell("voice -r -1 -n " & Chr(34) & TTSvoice & Chr(34) & " " & Chr(34) & lines & Chr(34))
    'SHELL _HIDE "espeak -ven-us+f2 -s150 " + CHR$(34) + lines$ + CHR$(34)
END SUB

SUB restart ()
    REDIM keywords(0) AS STRING, replies(0) AS STRING, wordIn(0) AS STRING, wordOut(0) AS STRING
    REDIM rStarts(0) AS INTEGER, rEnds(0) AS INTEGER, rIndex(0) AS INTEGER

END SUB

SUB conversation (file AS STRING)
    DIM i AS INTEGER
    restart
    sound2 ("./icq-horn.wav")
    CLS
    FOR i = 1 TO 10
        LOCATE 5, 5
        PRINT "             ";
        _DELAY 0.250
        LOCATE 5, 5
        PRINT "connecting...";
        _DELAY 0.250
    NEXT
    CLS
    DIM rply AS STRING '              for main loop
    LoadArrays file '   check file load, OK checks out
    PRINT Greeting: PRINT '           start testing main Eliza code
    DO
        rply = GetReply
        PRINT: speakTotext rply
    LOOP UNTIL rply = "Goodbye!"
    CLS
    LOCATE 5, 5
    PRINT "disconnected...  "
    SLEEP
END SUB






FUNCTION GetKeys$ (keysToCatch AS STRING)
    DIM k$
    DO
        k$ = INKEY$
        WHILE LEN(k$) = 0
            k$ = INKEY$
            _LIMIT 60
        WEND
    LOOP UNTIL INSTR(keysToCatch$, k$)
    GetKeys$ = k$
END FUNCTION



FUNCTION month$ (n) 'for dates1$ function
    RESTORE 4
    FOR i = 1 TO n
        READ month$
    NEXT
    4 DATA JANUARY,FEBRUARY,MARCH,APRIL,MAY,JUNE,JULY,AUGUST,SEPTEMBER,OCTOBER,NOVEMBER,DECEMBER
END FUNCTION

FUNCTION dates1$ (months)
    m = months MOD 12
    IF m = 0 THEN m = 12
    dates1$ = month$(m) + "," + STR$(INT((months - 1) / 12) + 2027)
END FUNCTION

'function as string arrays
FUNCTION hint$ (n AS INTEGER)
    SELECT CASE n
        CASE 1: hint$ = "the target is a single woman in her 30's working in a sensetive high level job in a international bank"
        CASE 2: hint$ = "the target has a drinking problem"
        CASE 3: hint$ = "the target uses a windoews 95 laptop at home and desktop with windoew 3.11 with antivirus and a security software in her job"
        CASE 4: hint$ = "the target is in a relasionship but she is also having an affair with someone - a strong opportonity for extortion"
        CASE 5: hint$ = "the target has a thing for the jazz singer 'chet backer'"
        CASE 6: hint$ = "the target likes gay porn - if she is in the closet it is a chance for extortion"
        CASE 7: hint$ = "the target has a cat named 'pogi'"
        CASE 8: hint$ = "the birthday yaer of the target is '1964'"
        CASE 9: hint$ = "the target's favorite movie is 'unforgiven'"
        CASE 10: hint$ = "the target uses an AOL email account 'sophie1964@aol.com' and she is also active is several forums on AOL under the nickname 'pogi64'"

    END SELECT
END FUNCTION




SUB opening ()
    DIM k AS STRING
    'Dim As Double start, current
    cp 5, "- H A C K E R -"
    cp 6, "- G A M E -"
    cp 8, " - B Y -"
    cp 9, "- R O N 7 7 -"
    COLOR 31: cp 11, "- P R E S S  E N T E R  T O  B E G I N -"
    cp 12, "- O R  E S C  T O  E X I T -": COLOR 7

    sound2 ("./Dial Up Modem-SoundBible.com-909377495.wav")

    k = GetKeys$(CHR$(13) + CHR$(27))
    IF k = CHR$(27) THEN
        END
    ELSE
        CLS
        cp 5, "- H A C K E D ! -"
    END IF
    SLEEP
    _SNDSTOP h&

END SUB

SUB start ()
    CLS
    cp 2, "START POINT"
    cp 4, "YOU ARE A HACKER WORKING FOR THE MAFIA"
    cp 5, "YOU ARE NAMELESS AND YOU WORK FOR A STRICT ONE TIME CONTRACT"
    cp 6, "YOU HAVE ONE YAER TO HACK A TARGET SECRETS (PASSWORDS, DATA ETC.)"
    cp 7, "IF YOU SUCCEED YOU CAN RETIRE FOR LIFE WITH 450,000$"
    cp 8, "IF YOU FAILE YOU WILL BE ASSASSINATED OR"
    cp 9, "TURNED OVER TO THE LAW AUTHORITIES"
    COLOR 4: cp 10, "MAKE NO MISTAKE! YOU HAVE NO CHOICE!": COLOR 7
    cp 12, "PRESS ANY KEY..."
    SLEEP
END SUB

SUB hints ()
    CLS
    DIM i AS INTEGER, k AS STRING
    cp 2, "HERE IS WHAT YOUR MAFIA BOSS IS WILLING TO TELL YOU ABOUT THE TARGET"
    cp 3, "PRESS SPACEBAR FOR INFORMATION HINTS"
    PRINT
    DO
        SLEEP
        PRINT hint$(i): PRINT
        i = i + 1

    LOOP UNTIL i = 11
    SLEEP

END SUB

SUB airplane ()
    DIM k AS STRING, message AS STRING
    CLS
    cp 4, "CHOOSE A DESTINATION..."
    cp 6, "1. GET LOST IN NORTH AMERICA - 20,000$"
    cp 7, "2. DISAPPEAR IN SOUTH AMERICA - 16,000$"
    cp 8, "3. VANISH IN ASIA - 12,000$"
    cp 9, "4. ON WAY TICKET TO NO-WHERE - 9,000$"
    cp 10, "GO BACK AND THINK OF IT"
    k = GetKeys$("12345")
    IF k = "1" AND money >= 20000 THEN
        message = "YOU CAN NOW GET LOST IN NORTH AMERICA"
        ticket = 1
    ELSEIF k = "2" AND money >= 16000 THEN
        message = "YOU CAN NOW DISAPPEAR IN SOUTH AMERICA"
        ticket = 1
    ELSEIF k = "3" AND money >= 12000 THEN
        message = "YOU CAN NOW VANISH IN ASIA"
        ticket = 1
    ELSEIF k = "4" AND money >= 9000 THEN
        message = "YOU CAN NOW GO TO NO-WHERE"
        ticket = 1
    ELSEIF k = "5" THEN
        EXIT SUB
    ELSE
        message = "COME BACK WHEN YOU ARE SERIOUS AND HAVE THE MONEY PAL!"
    END IF
    PRINT
    PRINT
    PRINT
    PRINT message
END SUB


SUB runawayMoney ()
    DIM cash AS LONG, message AS STRING
    CLS
    cp 4, "YOU HAVE NOW " + STR$(money) + "$"
    PRINT
    PRINT
    PRINT
    PRINT "HOW MUCH MONEY WILL YOU TRANSFER TO A SECRET BANK ACCOUNT FOR YOUR LIFE AFTER YOU ESCAPE?: "
    INPUT "", cash
    IF cash <= 0 THEN
        message = "YOU MUST BE JOKING! COME BACK WHEN YOUR SERIOUS"
    ELSEIF cash > money THEN
        message = "YOU DON'T HAVE THAT AMOUNT OF MONEY! COME BACK LATER"
    ELSE
        money = money - cash
        message = "MONEY TRANSFERED!"
        escapeMoney = escapeMoney + cash
    END IF

    PRINT
    PRINT
    PRINT message

    SLEEP
END SUB


SUB tor ()
    DIM k AS STRING, pass AS STRING
    CLS
    cp 4, "- WELCOME TO TOR DARKNET IDENTITY MARKETPLACE -"
    cp 7, "1. AN AMERICAN PASSPORT - 45,000 DOLLARS"
    cp 8, "2. A BRITISH PASSPORT - 40,000 DOLLARS"
    cp 9, "3. A CANADIAN PASSPORT - 38,000 DOLLARS"
    cp 10, "4. A SOUTH AMERICAN PASSPORT - 32,000 DOLLARS"
    cp 11, "5. AN AUSTRALIAN PASSPORT - 30,000 DOLLARS"
    cp 13, "6. EXIT"
    k = GetKeys$("123456")
    IF k = "1" AND money >= 45000 THEN
        pass = "AMERICAN PASSPORT AQUIERED!"
        money = money - 45000
        passport = 1
    ELSEIF k = "2" AND money >= 40000 THEN
        pass = "BRITISH PASSPORT AQUIERED!"
        money = money - 40000
        passport = 1
    ELSEIF k = "3" AND money >= 38000 THEN
        pass = "CANADIAN PASSPORT AQUIERED!"
        money = money - 38000
        passport = 1
    ELSEIF k = "4" AND money >= 32000 THEN
        pass = "SOUTH AMERICAN PASSPORT AQUIERED!"
        money = money - 32000
        passport = 1
    ELSEIF k = "5" AND money >= 30000 THEN
        pass = "AUSTRALIAN PASSPORT AQUIERED!"
        money = money - 30000
        passport = 1
    ELSEIF k = "6" THEN
        EXIT SUB
    ELSE
        pass = "NOT ENOGHT MONEY!"
    END IF

    PRINT
    PRINT
    PRINT pass
END SUB



SUB email ()
    DIM password AS STRING, guess AS STRING
    password = "almostblue"
    CLS
    cp 2, "- HACK TARGET'S EMAIL ACCOUNT -"
    LOCATE 4, 5
    INPUT "GUESS THE TARGET'S E-MAIL PASSWORD: ", guess
    IF guess = password THEN
        BEEP
        cp 7, "EMAIL HACKED"
        Ehacked = 1
    ELSE
        BEEP
        cp 7, "ACCESS DENIED!"
    END IF
    cp 8, "PRESS ANY KEY..."
    SLEEP

END SUB


SUB hack ()
    DIM HPC AS STRING, HW AS STRING, k AS STRING, h AS STRING, w AS STRING

    SELECT CASE HhackIndicator
        CASE 1: HPC = "- 1. HACK TARGET'S HOME PC -"
        CASE 2: HPC = "- 1. TARGET'S HOME PC HACKED -"
    END SELECT
    IF WhackIndicator = 1 THEN
        HW = "- 2. BRUTE FORCE ACCESS TO TARGET'S WORK ACCOUNT -"
    ELSEIF WhackIndicator = 2 AND Wcounter < 3 THEN
        HW = "- 2. BRUTE FORCE ACCESS TO TARGET'S WORK ACCOUNT IN PROGRESS-"
        Wcounter = Wcounter + 1
    ELSEIF WhackIndicator = 2 AND Wcounter >= 3 THEN
        WhackIndicator = 3
        HW = "- 2. TARGET'S WORK ACCOUNT HACKED -"
    ELSEIF WhackIndicator = 3 THEN
        HW = "- 2. TARGET'S WORK ACCOUNT HACKED -"
    END IF
    CLS
    cp 2, "- HACK TARGET -"
    cp 5, HPC
    cp 6, HW
    cp 7, "- 3. HACK TARGET'S EMAIL ACCOUNT -"
    '~ cp 8, "- 4. GO TO MAIN -"
    k = GetKeys$("123")
    IF k = "1" AND HhackIndicator = 1 THEN 'And HhackCounter = 0 Then
        h = "YOU SEND AN 'I LOVE YOU' EMAIL WITH A BACKDOOR VIRUS"
        HhackIndicator = 2
    ELSEIF k = "1" AND HhackIndicator = 2 THEN
        h = "TARGET'S HOME PC ALREADY HACKED"
    ELSEIF k = "2" AND WhackIndicator = 1 THEN
        w = "STARTING BRUTE FORCE... THIS MAY TAKE A WHILE"
        WhackIndicator = 2
    ELSEIF k = "2" AND WhackIndicator = 2 THEN
        w = "BRUTE FORCE ON TARGET'S WORK ACCOUNT IN PROGRESS"
    ELSEIF k = "2" AND WhackIndicator = 3 THEN
        w = "ACCESS TO TARGET'S WORK ACOUNT GRANTED"
        '~ ElseIf k = "4" Then
        '~ Exit Sub
    ELSEIF k = "3" THEN
        email
    END IF
    cp 11, h
    cp 13, w

    cp 14, "PRESS ANY KEY..."
    SLEEP

END SUB

SUB steal ()
    DIM k AS STRING, m AS STRING

    CLS
    cp 4, "YOU GAIN ACCESS TO THE BANK AND FEEL LIKE A KID IN A CANDY STORE..."
    cp 5, "WHAT DO YOU SAY SHELL WE STEAL SOME MONEY FROM THE BANK?"
    cp 6, "- 1. STEAL 100,000 DOLLARS OR 2. STAY HONEST AND DON'T TOUCH THE MONEY -"
    k = GetKeys$("12")
    IF k = "1" THEN
        m = "YOU STEAL 100,000 DOLLARS FROM THE BANK AND NOW YOU ARE A WANTED CRIMINAL"
        money = money + 100000
    ELSEIF k = "2" THEN
        m = "YOU DID THE RIGHT SMART THING AND KEPT FROM BEING EXPOUSED IT MAY NOT IMPROVE YOUR WALLET BUT YOU LIVE AT PEACE WITH YOURSELF"
    END IF
    PRINT
    PRINT
    PRINT m
    SLEEP
END SUB

SUB spy ()
    DIM k AS STRING, spy1 AS STRING
    CLS
    cp 2, "- SPY ON TARGET -"
    cp 5, "- 1. AOL FORUMS -"
    cp 6, "- 2. TARGET'S HOME PC -"
    cp 7, "- 3. TARGET'S EMAIL ACCOUNT -"
    cp 8, "- 4. TARGET'S WORK ACCOUNT -"
    k = GetKeys$("1234")
    IF k = "1" THEN
        spy1 = "YOU LOOK FOR SOPHY'S UNDER THE NICKNAME 'POGI64' FORUM'S MESSAGES AND YOU FIND SHE IS ACTIVE IN FORUMS OF A.A. AND CAT'S OWNERS YOU READ SOME OF HER MESSAGES THEY TELL YOU THAT SOPHY'S STRUGGELING WITH ALCOHOLIZEM AND DEPRESSION"
    ELSEIF k = "2" AND HhackIndicator = 1 THEN
        spy1 = "YOU DON'T HAVE YET ACCESS TO TARGET'S HOME PC"
    ELSEIF k = "2" AND HhackIndicator = 2 THEN
        spy1 = "YOU SEARCH DEEP INTO SOPHY'S PC AND FIND OUT A FEW THINGS - SHE IS BEING BLACKMAILED BY HER EX-LOVER AND SHE'S STUCK IN AN UN-HAPPY MARRIGE ONE THIG THAT WORRY YOU IS YOU FIND A SUICIDE LETTER UNDER HER NAME WITHOUT A DATE - IF SHE KILL'S HERSELF BEFORE YOU COMPLITE THE JOB YOU ARE A DEAD MAN!"
    ELSEIF k = "3" AND Ehacked = 0 THEN
        spy1 = "YOU DON'T HAVE ACCESS TO TARGET'S EMAIL"
    ELSEIF k = "3" AND Ehacked = 1 THEN
        spy1 = "SOPHY'S EMAIL JUST LIKE HER WHOLE LIFE IS A MESS OF MISSERY YOU FIND DESPERET REJECTED LOVE LETTERS AND DESPERET REQUESTS FOR A.A. REHAB CENTERS... SOPHY IS ONE MISSRABLE PERSON YOU TELL YOURSELF AND HER LIFE IS A MESS"
    ELSEIF k = "4" AND WhackIndicator < 3 THEN
        spy1 = "YOU DO NOT HAVE ACCESS TO TARGET'S WORK ACCOUNT"
    ELSEIF k = "4" AND WhackIndicator >= 3 THEN
        steal
        EXIT SUB
    END IF
    PRINT
    PRINT
    PRINT spy1
    PRINT

    SLEEP
END SUB

SUB escape ()
    CLS
    DIM k AS STRING, message AS STRING
    cp 2, dates1(Time2)
    cp 4, "MONEY: " + STR$(money) + "$"
    COLOR 4: cp 6, "- ESCAPE PLAN -"
    cp 8, "YOU KNOW BY NOW YOU CAN'T TRUST YOUR BOSS OR YOUR JOB CONTRACT"
    cp 9, "THEY DON'T HAVE ANY INTENTION TO PAY YOU WHEN YOU FINISH"
    cp 10, "MORE LIKELY THEY WILL KILL YOU AND GET RIDE OF THE EVIDENCE"
    cp 11, "IF YOU ARE SMART YOU SHOULD MAKE AN ESCAPE PLAN"
    cp 12, "AND DISAPPEAR COVERING YOUR TRACKS"
    cp 13, "OTHERWISE WHETHER OR NOT YOU FINISH YOUR JOB YOU ARE A DEAD MAN"
    cp 15, "- 1. CREATE A FAKE IDENTITIY -"
    cp 16, "- 2. PREPARE MONEY -"
    cp 17, "- 3. BUY A RUNAWAY TICKET -"
    cp 18, "- 4. DISAPPEAR -"
    cp 19, "- 5. SWITCH SIDE (CONTACT THE FBI) -": COLOR 7
    cp 20, "- 6. GO BACK -"
    k = GetKeys$("123456")
    IF k = "6" THEN
        EXIT SUB
    ELSEIF k = "4" THEN
        IF passport = 1 AND ticket = 1 AND escapeMoney > 0 THEN
            the_end "end.txt", 0
        ELSE
            message = "YOU ARE NOT READY TO DISAPPEAR"
        END IF
    ELSEIF k = "1" THEN
        SELECT CASE passport
            CASE 0
                tor
            CASE 1
                message = "YOU ALREADY HAVE A PASSPORT AND A FAKE IDENTITY!"
        END SELECT
    ELSEIF k = "2" THEN
        runawayMoney
    ELSEIF k = "3" THEN
        SELECT CASE ticket
            CASE 0
                airplane
            CASE 1
                message = "YOU ALREADY HAVE A ONE WAY TICKET OUT OF HERE!"
        END SELECT
    ELSEIF k = "5" THEN
        the_end "goodEnd.txt", 0

    END IF

    PRINT
    PRINT
    PRINT message
    SLEEP
END SUB

SUB job ()
    DO
        CLS
        DIM k AS STRING
        cp 2, dates1(Time2)
        cp 4, "MONEY: " + STR$(money) + "$"
        cp 6, "- DO THE JOB -"
        cp 8, "- 1. SPY ON TARGET -"
        cp 9, "- 2. HACK TARGET -"
        cp 10, "- 3. SOCIAL ENGINEERING -"
        cp 11, "- 4. EXTORT TARGET -"
        cp 12, "- 5. GO BACK -"
        k = GetKeys$("12345")
        IF k = "5" THEN
            EXIT SUB
        ELSEIF k = "2" THEN
            hack
        ELSEIF k = "1" THEN
            spy
        ELSEIF k = "3" THEN
            CLS
            IF sophie_state = 0 THEN
                conversation ("sophy.txt")
            ELSEIF sophie_state = 1 THEN
                conversation ("sophy2.txt")

            END IF
        ELSEIF k = "4" THEN
            cp 14, "YOU WRITE AN ANONYMOUS BLACKMAIL EMAIL FOR 40,000 DOLLARES AND SEND IT TO SOPHIE"
            cp 16, "PRESS ANY KEY..."
            SLEEP
            CLS
            cp 8, "SOPHIE TRYS TO COMMITE SUICIDE AND NOW IS AT THE HOSPITAL, YOU ARE WANTED BY THE POLICE"
            cp 10, "PRESS ANY KEY..."
            sophie_state = 1
            SLEEP

        END IF
    LOOP
END SUB

SUB main ()
    DIM k AS STRING
    DO
        '100
        IF Time2 = 14 THEN END
        CLS
        cp 2, dates1(Time2)
        'Time = Time + 1
        cp 4, "MONEY: " + STR$(money) + "$"
        'money = money + 800
        cp 7, "- 1. DO THE JOB -"
        COLOR 4: cp 10, "- 2. ESCAPE PLAN -": COLOR 7
        cp 13, "- 3. NEXT MONTH -"
        cp 16, "- PRESS ESC TO EXIT -"
        k = GetKeys$("123" + CHR$(27))
        IF k = "2" THEN
            escape
        ELSEIF k = "1" THEN
            job
        ELSEIF k = "3" THEN
            Time2 = Time2 + 1
            money = money + 800
            IF Time2 = 14 THEN
                the_end "badEnd.txt", 1
            END IF
        END IF
    LOOP UNTIL k = CHR$(27)
END SUB


