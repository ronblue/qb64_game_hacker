'#include "./fbsound2-1.1/inc/fbsound_dynamic.bi"
'SCREEN 0
'width 100 / 8 , 37 / 16
Screen _NewImage(800, 600, 256)
Randomize Timer
'ReDim Shared As STRING pass()
Dim Shared points, level, Time2, money, HhackIndicator, WhackIndicator, Wcounter, Ehacked As Integer
Const punctuation = "?!,.:;<>(){}[]"
Dim Shared Greeting As String, You As String, Script As String
Dim Shared kCnt As Integer, rCnt As Integer, wCnt As Integer, NoKeyFoundIndex As Integer
ReDim Shared keywords(0) As String, replies(0) As String, wordIn(0) As String, wordOut(0) As String
ReDim Shared rStarts(0) As Integer, rEnds(0) As Integer, rIndex(0) As Integer
Dim Shared sophie_state, passport, ticket
Dim Shared escapeMoney As Long
Dim Shared h&
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
Sub cp (row As Integer, s As String)
    Locate row, (100 - Len(s)) / 2: Print s;
End Sub

Sub sound2 (f As String)

    h& = _SndOpen(f)
    _SndPlay h&

    'SLEEP
    '_SNDSTOP h&

End Sub

Sub the_end (f As String, soundChoice As Integer)
    If soundChoice = 0 Then
        sound2 ("Vespers.wav")
    ElseIf soundChoice = 1 Then
        Beep
        Beep
        Beep
        Beep
        Beep
    End If
    Cls
    Dim buffer As String, tmp As String
    ''dim h as long = freefile()
    Open f For Input As #1
    While Not EOF(1)
        Line Input #1, tmp 'reading one line at a time
        buffer = buffer + tmp + Chr$(13) 'adding the newline manually
    Wend
    Print buffer
    Close #1





    Sleep
    _SndStop h&
    End
End Sub

'append to the string array the string item
Sub sAppend (arr() As String, item As String)
    ReDim _Preserve arr(LBound(arr) To UBound(arr) + 1) As String
    arr(UBound(arr)) = item
End Sub
'append to the integer array the integer item
Sub nAppend (arr() As Integer, item As Integer)
    ReDim _Preserve arr(LBound(arr) To UBound(arr) + 1) As Integer
    arr(UBound(arr)) = item
End Sub

'append to the integer array the integer item
Sub nAppend2 (arr() As Single, item As Single)
    ReDim _Preserve arr(LBound(arr) To UBound(arr) + 1) As Single
    arr(UBound(arr)) = item
End Sub




' pull data out of some script file
Sub LoadArrays (scriptFile As String)
    Dim startR As Integer, endR As Integer, ReadingR As Integer, temp As Integer
    Dim fline As String, kWord As String

    Open scriptFile For Input As #1
    While EOF(1) = 0
        Line Input #1, fline
        Select Case Left$(fline$, 2)
            Case "g:": Greeting = _Trim$(Mid$(fline, 3))
            Case "y:": You = _Trim$(Mid$(fline, 3))
            Case "c:": Script = _Trim$(Mid$(fline, 3))
            Case "s:"
                wCnt = wCnt + 1: temp = InStr(fline, ">")
                If temp Then
                    sAppend wordIn(), " " + _Trim$(Mid$(fline, 3, temp - 3)) + " "
                    sAppend wordOut(), " " + _Trim$(Mid$(fline, temp + 1)) + " "
                End If
            Case "r:"
                rCnt = rCnt + 1
                sAppend replies(), _Trim$(Mid$(fline, 3))
                If Not ReadingR Then
                    ReadingR = -1
                    startR = rCnt
                End If
            Case "k:"
                If ReadingR Then
                    endR = rCnt
                    ReadingR = 0
                End If
                If rCnt Then
                    kCnt = kCnt + 1
                    kWord = _Trim$(Mid$(fline, 3))
                    sAppend keywords(), " " + kWord + " "
                    nAppend rStarts(), startR
                    nAppend rIndex(), startR
                    nAppend rEnds(), endR
                    If kWord = "nokeyfound" Then NoKeyFoundIndex = kCnt
                End If
            Case "e:": Exit While
        End Select
    Wend
    Close #1
    If ReadingR Then 'handle last bits
        endR = rCnt
        kCnt = kCnt + 1
        sAppend keywords(), "nokeyfound"
        nAppend rStarts(), startR
        nAppend rIndex(), startR
        nAppend rEnds(), endR
        NoKeyFoundIndex = kCnt
    End If
End Sub

' =============================== here is the heart of ELIZA / Player function
Function GetReply$ ()
    Dim inpt As String, tail As String, answ As String
    Dim kFlag As Integer, k As Integer, kFound As Integer, l As Integer, w As Integer

    ' USER INPUT SECTION
    Print You + ": ";: Line Input "", inpt
    If LCase$(inpt) = "q" Or LCase$(inpt) = "x" Or LCase$(inpt) = "goodbye" Or LCase$(inpt) = "good night" Or LCase$(inpt) = "bye" Then
        GetReply$ = "Goodbye!": Exit Function
    End If
    inpt = " " + inpt + " " '<< need this because keywords embedded in spaces to ID whole words only
    inpt = isolatePunctuation$(inpt)
    For k = 1 To kCnt 'loop through key words until we find a match
        kFound = InStr(LCase$(inpt), LCase$(keywords(k)))
        If kFound > 0 Then '>>> need the following for * in some replies
            tail = " " + Mid$(inpt, kFound + Len(keywords(k)))
            For l = 1 To Len(tail) 'DO NOT USE INSTR
                For w = 1 To wCnt 'swap words in tail if used there
                    If LCase$(Mid$(tail, l, Len(wordIn(w)))) = LCase$(wordIn(w)) Then 'swap words exit for
                        tail = Mid$(tail, 1, l - 1) + wordOut(w) + Mid$(tail, l + Len(wordIn(w)))
                        Exit For
                    End If
                Next w
            Next l
            kFlag = -1
            Exit For
        End If
    Next
    If kFlag = 0 Then k = NoKeyFoundIndex
    answ = replies(Int((rEnds(k) - rStarts(k) + 1) * Rnd) + rStarts(k))
    'set pointer to next reply in rIndex array
    If k = NoKeyFoundIndex Then 'let's not get too predictable for most used set of replies
        rIndex(k) = Int((rEnds(k) - rStarts(k) + 1) * Rnd) + rStarts(k)
        'ELSE
        '    rIndex(k) = rIndex(k) + 1 'set next reply index then check it
        '    IF rIndex(k) > rEnds(k) THEN rIndex(k) = rStarts(k)
    End If
    If Right$(answ, 1) <> "*" Then GetReply$ = answ: Exit Function 'oh so the * signal an append to reply!
    If _Trim$(tail) = "" Then
        GetReply$ = "Please elaborate on, " + keywords(k)
    Else
        tail = joinPunctuation$(tail)
        GetReply$ = Mid$(answ, 1, Len(answ) - 1) + tail
    End If
End Function

Function isolatePunctuation$ (s As String)
    'isolate punctuation so when we look for key words they don't interfere
    Dim b As String, i As Integer
    b = ""
    For i = 1 To Len(s)
        If InStr(punctuation, Mid$(s, i, 1)) > 0 Then b = b + " " + Mid$(s, i, 1) + " " Else b = b + Mid$(s, i, 1)
    Next
    isolatePunctuation$ = b
End Function

Function joinPunctuation$ (s As String)
    'undo isolatePuntuation$
    Dim b As String, find As String, i As Integer, place As Integer
    b = s
    For i = 1 To Len(punctuation)
        find = " " + Mid$(punctuation, i, 1) + " "
        place = InStr(b, find)
        While place > 0
            If place = 1 Then
                b = Mid$(punctuation, i, 1) + Mid$(b, place + 3)
            Else
                b = Mid$(b, 1, place - 1) + Mid$(punctuation, i, 1) + Mid$(b, place + 3)
            End If
            place = InStr(b, find)
        Wend
    Next
    joinPunctuation$ = b
End Function



Sub slow (text As String)
    ReDim speed(0) As Single
    Restore 6
    For i = 1 To 5
        Read r
        nAppend2 speed(), r
    Next
    6 Data 0.50,0.100,0.20,0.300,0.250
    For i = 1 To Len(text)
        Print Mid$(text, i, 1);
        _Delay speed(Int(Rnd * UBound(speed)))
    Next
End Sub

Sub speakTotext (lines As String) 'uses voice command line voice.exe
    Print Script + ": ";: slow lines: Print: Print
    '    Shell("voice -r -1 -n " & Chr(34) & TTSvoice & Chr(34) & " " & Chr(34) & lines & Chr(34))
    'SHELL _HIDE "espeak -ven-us+f2 -s150 " + CHR$(34) + lines$ + CHR$(34)
End Sub

Sub restart ()
    ReDim keywords(0) As String, replies(0) As String, wordIn(0) As String, wordOut(0) As String
    ReDim rStarts(0) As Integer, rEnds(0) As Integer, rIndex(0) As Integer

End Sub

Sub conversation (file As String)
    Dim i As Integer
    restart
    sound2 ("./icq-horn.wav")
    Cls
    For i = 1 To 10
        Locate 5, 5
        Print "             ";
        _Delay 0.250
        Locate 5, 5
        Print "connecting...";
        _Delay 0.250
    Next
    Cls
    Dim rply As String '              for main loop
    LoadArrays file '   check file load, OK checks out
    Print Greeting: Print '           start testing main Eliza code
    Do
        rply = GetReply
        Print: speakTotext rply
    Loop Until rply = "Goodbye!"
    Cls
    Locate 5, 5
    Print "disconnected...  "
    Sleep
End Sub






Function GetKeys$ (keysToCatch As String)
    Dim k$
    Do
        k$ = InKey$
        While Len(k$) = 0
            k$ = InKey$
            _Limit 60
        Wend
    Loop Until InStr(keysToCatch$, k$)
    GetKeys$ = k$
End Function



Function month$ (n) 'for dates1$ function
    Restore 4
    For i = 1 To n
        Read month$
    Next
    4 Data JANUARY,FEBRUARY,MARCH,APRIL,MAY,JUNE,JULY,AUGUST,SEPTEMBER,OCTOBER,NOVEMBER,DECEMBER
End Function

Function dates1$ (months)
    m = months Mod 12
    If m = 0 Then m = 12
    dates1$ = month$(m) + "," + Str$(Int((months - 1) / 12) + 1997)
End Function

'function as string arrays
Function hint$ (n As Integer)
    Select Case n
        Case 1: hint$ = "the target is a single woman in her 30's working in a sensetive high level job in a international bank"
        Case 2: hint$ = "the target has a drinking problem"
        Case 3: hint$ = "the target uses a windoews 95 laptop at home and desktop with windoew 3.11 with antivirus and a security software in her job"
        Case 4: hint$ = "the target is in a relasionship but she is also having an affair with someone - a strong opportonity for extortion"
        Case 5: hint$ = "the target has a thing for the jazz singer 'chet backer'"
        Case 6: hint$ = "the target likes gay porn - if she is in the closet it is a chance for extortion"
        Case 7: hint$ = "the target has a cat named 'pogi'"
        Case 8: hint$ = "the birthday yaer of the target is '1964'"
        Case 9: hint$ = "the target's favorite movie is 'unforgiven'"
        Case 10: hint$ = "the target uses an AOL email account 'sophie1964@aol.com' and she is also active is several forums on AOL under the nickname 'pogi64'"

    End Select
End Function




Sub opening ()
    Dim k As String
    'Dim As Double start, current
    cp 5, "- H A C K E R -"
    cp 6, "- G A M E -"
    cp 8, " - B Y -"
    cp 9, "- R O N 7 7 -"
    Color 31: cp 11, "- P R E S S  E N T E R  T O  B E G I N -"
    cp 12, "- O R  E S C  T O  E X I T -": Color 7

    sound2 ("./Dial Up Modem-SoundBible.com-909377495.wav")

    k = GetKeys$(Chr$(13) + Chr$(27))
    If k = Chr$(27) Then
        End
    Else
        Cls
        cp 5, "- H A C K E D ! -"
    End If
    Sleep
    _SndStop h&

End Sub

Sub start ()
    Cls
    cp 2, "START POINT"
    cp 4, "YOU ARE A HACKER WORKING FOR THE MAFIA"
    cp 5, "YOU ARE NAMELESS AND YOU WORK FOR A STRICT ONE TIME CONTRACT"
    cp 6, "YOU HAVE ONE YAER TO HACK A TARGET SECRETS (PASSWORDS, DATA ETC.)"
    cp 7, "IF YOU SUCCEED YOU CAN RETIRE FOR LIFE WITH 450,000$"
    cp 8, "IF YOU FAILE YOU WILL BE ASSASSINATED OR"
    cp 9, "TURNED OVER TO THE LAW AUTHORITIES"
    Color 4: cp 10, "MAKE NO MISTAKE! YOU HAVE NO CHOICE!": Color 7
    cp 12, "PRESS ANY KEY..."
    Sleep
End Sub

Sub hints ()
    Cls
    Dim i As Integer, k As String
    cp 2, "HERE IS WHAT YOUR MAFIA BOSS IS WILLING TO TELL YOU ABOUT THE TARGET"
    cp 3, "PRESS SPACEBAR FOR INFORMATION HINTS"
    Print
    Do
        Sleep
        Print hint$(i): Print
        i = i + 1

    Loop Until i = 11
    Sleep

End Sub

Sub airplane ()
    Dim k As String, message As String
    Cls
    cp 4, "CHOOSE A DESTINATION..."
    cp 6, "1. GET LOST IN NORTH AMERICA - 20,000$"
    cp 7, "2. DISAPPEAR IN SOUTH AMERICA - 16,000$"
    cp 8, "3. VANISH IN ASIA - 12,000$"
    cp 9, "4. ON WAY TICKET TO NO-WHERE - 9,000$"
    cp 10, "GO BACK AND THINK OF IT"
    k = GetKeys$("12345")
    If k = "1" And money >= 20000 Then
        message = "YOU CAN NOW GET LOST IN NORTH AMERICA"
        ticket = 1
    ElseIf k = "2" And money >= 16000 Then
        message = "YOU CAN NOW DISAPPEAR IN SOUTH AMERICA"
        ticket = 1
    ElseIf k = "3" And money >= 12000 Then
        message = "YOU CAN NOW VANISH IN ASIA"
        ticket = 1
    ElseIf k = "4" And money >= 9000 Then
        message = "YOU CAN NOW GO TO NO-WHERE"
        ticket = 1
    ElseIf k = "5" Then
        Exit Sub
    Else
        message = "COME BACK WHEN YOU ARE SERIOUS AND HAVE THE MONEY PAL!"
    End If
    Print
    Print
    Print
    Print message
End Sub


Sub runawayMoney ()
    Dim cash As Long, message As String
    Cls
    cp 4, "YOU HAVE NOW " + Str$(money) + "$"
    Print
    Print
    Print
    Print "HOW MUCH MONEY WILL YOU TRANSFER TO A SECRET BANK ACCOUNT FOR YOUR LIFE AFTER YOU ESCAPE?: "
    Input "", cash
    If cash <= 0 Then
        message = "YOU MUST BE JOKING! COME BACK WHEN YOUR SERIOUS"
    ElseIf cash > money Then
        message = "YOU DON'T HAVE THAT AMOUNT OF MONEY! COME BACK LATER"
    Else
        money = money - cash
        message = "MONEY TRANSFERED!"
        escapeMoney = escapeMoney + cash
    End If

    Print
    Print
    Print message

    Sleep
End Sub


Sub tor ()
    Dim k As String, pass As String
    Cls
    cp 4, "- WELCOME TO TOR DARKNET IDENTITY MARKETPLACE -"
    cp 7, "1. AN AMERICAN PASSPORT - 45,000 DOLLARS"
    cp 8, "2. A BRITISH PASSPORT - 40,000 DOLLARS"
    cp 9, "3. A CANADIAN PASSPORT - 38,000 DOLLARS"
    cp 10, "4. A SOUTH AMERICAN PASSPORT - 32,000 DOLLARS"
    cp 11, "5. AN AUSTRALIAN PASSPORT - 30,000 DOLLARS"
    cp 13, "6. EXIT"
    k = GetKeys$("123456")
    If k = "1" And money >= 45000 Then
        pass = "AMERICAN PASSPORT AQUIERED!"
        money = money - 45000
        passport = 1
    ElseIf k = "2" And money >= 40000 Then
        pass = "BRITISH PASSPORT AQUIERED!"
        money = money - 40000
        passport = 1
    ElseIf k = "3" And money >= 38000 Then
        pass = "CANADIAN PASSPORT AQUIERED!"
        money = money - 38000
        passport = 1
    ElseIf k = "4" And money >= 32000 Then
        pass = "SOUTH AMERICAN PASSPORT AQUIERED!"
        money = money - 32000
        passport = 1
    ElseIf k = "5" And money >= 30000 Then
        pass = "AUSTRALIAN PASSPORT AQUIERED!"
        money = money - 30000
        passport = 1
    ElseIf k = "6" Then
        Exit Sub
    Else
        pass = "NOT ENOGHT MONEY!"
    End If

    Print
    Print
    Print pass
End Sub



Sub email ()
    Dim password As String, guess As String
    password = "almostblue"
    Cls
    cp 2, "- HACK TARGET'S EMAIL ACCOUNT -"
    Locate 4, 5
    Input "GUESS THE TARGET'S E-MAIL PASSWORD: ", guess
    If guess = password Then
        Beep
        cp 7, "EMAIL HACKED"
        Ehacked = 1
    Else
        Beep
        cp 7, "ACCESS DENIED!"
    End If
    cp 8, "PRESS ANY KEY..."
    Sleep

End Sub


Sub hack ()
    Dim HPC As String, HW As String, k As String, h As String, w As String

    Select Case HhackIndicator
        Case 1: HPC = "- 1. HACK TARGET'S HOME PC -"
        Case 2: HPC = "- 1. TARGET'S HOME PC HACKED -"
    End Select
    If WhackIndicator = 1 Then
        HW = "- 2. BRUTE FORCE ACCESS TO TARGET'S WORK ACCOUNT -"
    ElseIf WhackIndicator = 2 And Wcounter < 3 Then
        HW = "- 2. BRUTE FORCE ACCESS TO TARGET'S WORK ACCOUNT IN PROGRESS-"
        Wcounter = Wcounter + 1
    ElseIf WhackIndicator = 2 And Wcounter >= 3 Then
        WhackIndicator = 3
        HW = "- 2. TARGET'S WORK ACCOUNT HACKED -"
    ElseIf WhackIndicator = 3 Then
        HW = "- 2. TARGET'S WORK ACCOUNT HACKED -"
    End If
    Cls
    cp 2, "- HACK TARGET -"
    cp 5, HPC
    cp 6, HW
    cp 7, "- 3. HACK TARGET'S EMAIL ACCOUNT -"
    '~ cp 8, "- 4. GO TO MAIN -"
    k = GetKeys$("123")
    If k = "1" And HhackIndicator = 1 Then 'And HhackCounter = 0 Then
        h = "YOU SEND AN 'I LOVE YOU' EMAIL WITH A BACKDOOR VIRUS"
        HhackIndicator = 2
    ElseIf k = "1" And HhackIndicator = 2 Then
        h = "TARGET'S HOME PC ALREADY HACKED"
    ElseIf k = "2" And WhackIndicator = 1 Then
        w = "STARTING BRUTE FORCE... THIS MAY TAKE A WHILE"
        WhackIndicator = 2
    ElseIf k = "2" And WhackIndicator = 2 Then
        w = "BRUTE FORCE ON TARGET'S WORK ACCOUNT IN PROGRESS"
    ElseIf k = "2" And WhackIndicator = 3 Then
        w = "ACCESS TO TARGET'S WORK ACOUNT GRANTED"
        '~ ElseIf k = "4" Then
        '~ Exit Sub
    ElseIf k = "3" Then
        email
    End If
    cp 11, h
    cp 13, w

    cp 14, "PRESS ANY KEY..."
    Sleep

End Sub

Sub steal ()
    Dim k As String, m As String

    Cls
    cp 4, "YOU GAIN ACCESS TO THE BANK AND FEEL LIKE A KID IN A CANDY STORE..."
    cp 5, "WHAT DO YOU SAY SHELL WE STEAL SOME MONEY FROM THE BANK?"
    cp 6, "- 1. STEAL 100,000 DOLLARS OR 2. STAY HONEST AND DON'T TOUCH THE MONEY -"
    k = GetKeys$("12")
    If k = "1" Then
        m = "YOU STEAL 100,000 DOLLARS FROM THE BANK AND NOW YOU ARE A WANTED CRIMINAL"
        money = money + 100000
    ElseIf k = "2" Then
        m = "YOU DID THE RIGHT SMART THING AND KEPT FROM BEING EXPOUSED IT MAY NOT IMPROVE YOUR WALLET BUT YOU LIVE AT PEACE WITH YOURSELF"
    End If
    Print
    Print
    Print m
    Sleep
End Sub

Sub spy ()
    Dim k As String, spy1 As String
    Cls
    cp 2, "- SPY ON TARGET -"
    cp 5, "- 1. AOL FORUMS -"
    cp 6, "- 2. TARGET'S HOME PC -"
    cp 7, "- 3. TARGET'S EMAIL ACCOUNT -"
    cp 8, "- 4. TARGET'S WORK ACCOUNT -"
    k = GetKeys$("1234")
    If k = "1" Then
        spy1 = "YOU LOOK FOR SOPHY'S UNDER THE NICKNAME 'POGI64' FORUM'S MESSAGES AND YOU FIND SHE IS ACTIVE IN FORUMS OF A.A. AND CAT'S OWNERS YOU READ SOME OF HER MESSAGES THEY TELL YOU THAT SOPHY'S STRUGGELING WITH ALCOHOLIZEM AND DEPRESSION"
    ElseIf k = "2" And HhackIndicator = 1 Then
        spy1 = "YOU DON'T HAVE YET ACCESS TO TARGET'S HOME PC"
    ElseIf k = "2" And HhackIndicator = 2 Then
        spy1 = "YOU SEARCH DEEP INTO SOPHY'S PC AND FIND OUT A FEW THINGS - SHE IS BEING BLACKMAILED BY HER EX-LOVER AND SHE'S STUCK IN AN UN-HAPPY MARRIGE ONE THIG THAT WORRY YOU IS YOU FIND A SUICIDE LETTER UNDER HER NAME WITHOUT A DATE - IF SHE KILL'S HERSELF BEFORE YOU COMPLITE THE JOB YOU ARE A DEAD MAN!"
    ElseIf k = "3" And Ehacked = 0 Then
        spy1 = "YOU DON'T HAVE ACCESS TO TARGET'S EMAIL"
    ElseIf k = "3" And Ehacked = 1 Then
        spy1 = "SOPHY'S EMAIL JUST LIKE HER WHOLE LIFE IS A MESS OF MISSERY YOU FIND DESPERET REJECTED LOVE LETTERS AND DESPERET REQUESTS FOR A.A. REHAB CENTERS... SOPHY IS ONE MISSRABLE PERSON YOU TELL YOURSELF AND HER LIFE IS A MESS"
    ElseIf k = "4" And WhackIndicator < 3 Then
        spy1 = "YOU DO NOT HAVE ACCESS TO TARGET'S WORK ACCOUNT"
    ElseIf k = "4" And WhackIndicator >= 3 Then
        steal
        Exit Sub
    End If
    Print
    Print
    Print spy1
    Print

    Sleep
End Sub

Sub escape ()
    Cls
    Dim k As String, message As String
    cp 2, dates1(Time2)
    cp 4, "MONEY: " + Str$(money) + "$"
    Color 4: cp 6, "- ESCAPE PLAN -"
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
    cp 19, "- 5. SWITCH SIDE (CONTACT THE FBI) -": Color 7
    cp 20, "- 6. GO BACK -"
    k = GetKeys$("123456")
    If k = "6" Then
        Exit Sub
    ElseIf k = "4" Then
        If passport = 1 And ticket = 1 And escapeMoney > 0 Then
            the_end "end.txt", 0
        Else
            message = "YOU ARE NOT READY TO DISAPPEAR"
        End If
    ElseIf k = "1" Then
        Select Case passport
            Case 0
                tor
            Case 1
                message = "YOU ALREADY HAVE A PASSPORT AND A FAKE IDENTITY!"
        End Select
    ElseIf k = "2" Then
        runawayMoney
    ElseIf k = "3" Then
        Select Case ticket
            Case 0
                airplane
            Case 1
                message = "YOU ALREADY HAVE A ONE WAY TICKET OUT OF HERE!"
        End Select
    ElseIf k = "5" Then
        the_end "goodEnd.txt", 0

    End If

    Print
    Print
    Print message
    Sleep
End Sub

Sub job ()
    Do
        Cls
        Dim k As String
        cp 2, dates1(Time2)
        cp 4, "MONEY: " + Str$(money) + "$"
        cp 6, "- DO THE JOB -"
        cp 8, "- 1. SPY ON TARGET -"
        cp 9, "- 2. HACK TARGET -"
        cp 10, "- 3. SOCIAL ENGINEERING -"
        cp 11, "- 4. EXTORT TARGET -"
        cp 12, "- 5. GO BACK -"
        k = GetKeys$("12345")
        If k = "5" Then
            Exit Sub
        ElseIf k = "2" Then
            hack
        ElseIf k = "1" Then
            spy
        ElseIf k = "3" Then
            Cls
            If sophie_state = 0 Then
                conversation ("sophy.txt")
            ElseIf sophie_state = 1 Then
                conversation ("sophy2.txt")

            End If
        ElseIf k = "4" Then
            cp 14, "YOU WRITE AN ANONYMOUS BLACKMAIL EMAIL FOR 40,000 DOLLARES AND SEND IT TO SOPHIE"
            cp 16, "PRESS ANY KEY..."
            Sleep
            Cls
            cp 8, "SOPHIE TRYS TO COMMITE SUICIDE AND NOW IS AT THE HOSPITAL, YOU ARE WANTED BY THE POLICE"
            cp 10, "PRESS ANY KEY..."
            sophie_state = 1
            Sleep

        End If
    Loop
End Sub

Sub main ()
    Dim k As String
    Do
        '100
        If Time2 = 14 Then End
        Cls
        cp 2, dates1(Time2)
        'Time = Time + 1
        cp 4, "MONEY: " + Str$(money) + "$"
        'money = money + 800
        cp 7, "- 1. DO THE JOB -"
        Color 4: cp 10, "- 2. ESCAPE PLAN -": Color 7
        cp 13, "- 3. NEXT MONTH -"
        cp 16, "- PRESS ESC TO EXIT -"
        k = GetKeys$("123" + Chr$(27))
        If k = "2" Then
            escape
        ElseIf k = "1" Then
            job
        ElseIf k = "3" Then
            Time2 = Time2 + 1
            money = money + 800
            If Time2 = 14 Then
                the_end "badEnd.txt", 1
            End If
        End If
    Loop Until k = Chr$(27)
End Sub


