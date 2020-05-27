#include "./fbsound-1.1/inc/fbsound_dynamic.bi"
Screen 19
width 100 / 8 , 37 / 16
Randomize Timer
ReDim Shared As STRING pass()   
Dim Shared As Integer points, level, Time2 = 1, money, HhackIndicator = 1, WhackIndicator = 1, Wcounter, Ehacked 
CONST punctuation = "?!,.:;<>(){}[]"
DIM SHARED Greeting AS STRING, You AS STRING, Script AS String
DIM SHARED kCnt AS INTEGER, rCnt AS INTEGER, wCnt AS INTEGER, NoKeyFoundIndex AS INTEGER
REDIM SHARED keywords(0) AS STRING, replies(0) AS STRING, wordIn(0) AS STRING, wordOut(0) AS STRING
REDIM SHARED rStarts(0) AS INTEGER, rEnds(0) AS INTEGER, rIndex(0) AS INTEGER
dim shared as integer sophie_state = 0 , passport = 0, ticket = 0, escapeMoney = 0
'~ sophie_state = 0
'~ passport = 0
'~ ticket = 0
'~ escapeMoney = 0
const data_path = "..\data\"
chdir(exepath())

'center print
Sub cp(row As integer, s As String)
	Locate row, (100 - Len(s)) / 2 : Print s;
End Sub

Sub sound(f As String)
	Dim as boolean ok
	ok=fbs_Init()
	Dim as integer hWave
	fbs_Load_WAVFile(f,@hWave)
	fbs_Play_Wave(hWave)
	if inkey<>"" then
      fbs_Destroy_Wave(@hWave)
   endif
end sub

SUB the_end(f as String,soundChoice as integer)
	if soundChoice = 0 then
		sound("./Vespers.wav")
	elseif soundChoice = 1 then
		beep
		beep
		beep
		beep
		beep
	end if
	cls
	dim as string buffer
	dim h as long = freefile()
	open f for binary as #h
	buffer = space(lof(1))
	get #h,,buffer
	print buffer
	close #h
	sleep()
	end
end sub

'append to the string array the string item
SUB sAppend (arr() AS STRING, item AS STRING)
    REDIM Preserve arr(LBOUND(arr) TO UBOUND(arr) + 1) AS STRING
    arr(UBOUND(arr)) = item
end sub
'append to the integer array the integer item
SUB nAppend (arr() AS INTEGER, item AS INTEGER)
    REDIM Preserve arr(LBOUND(arr) TO UBOUND(arr) + 1) AS INTEGER
    arr(UBOUND(arr)) = item
end sub

' pull data out of some script file
SUB LoadArrays (scriptFile AS STRING)
    DIM startR AS INTEGER, endR AS INTEGER, ReadingR AS INTEGER, temp AS INTEGER
    DIM fline AS STRING, kWord AS STRING
    OPEN scriptFile FOR INPUT AS #1
    WHILE Not EOF(1)
        LINE INPUT #1, fline
        SELECT CASE LEFT(fline, 2)
           CASE "g:": Greeting = Trim(MID(fline, 3))
           CASE "y:": You = Trim(MID(fline, 3))
           CASE "c:": Script = Trim(MID(fline, 3))
            CASE "s:"
                wCnt = wCnt + 1: temp = INSTR(fline, ">")
                IF temp THEN
                    sAppend wordIn(), " " + Trim(MID(fline, 3, temp - 3)) + " "
                    sAppend wordOut(), " " + Trim(MID(fline, temp + 1)) + " "
                END IF
            CASE "r:"
                rCnt = rCnt + 1
                sAppend replies(), Trim(MID(fline, 3))
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
                    kWord = Trim(MID(fline, 3))
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


FUNCTION isolatePunctuation (s AS STRING) as string
    'isolate punctuation so when we look for key words they don't interfere
    DIM b AS STRING, i AS INTEGER
    b = ""
    FOR i = 1 TO LEN(s)
        IF INSTR(punctuation, MID(s, i, 1)) > 0 THEN b = b + " " + MID(s, i, 1) + " " ELSE b = b + MID(s, i, 1)
    NEXT
    isolatePunctuation = b
END FUNCTION

FUNCTION joinPunctuation (s AS STRING) as String
    'undo isolatePuntuation$
    DIM b AS STRING, find AS STRING, i AS INTEGER, place AS INTEGER
    b = s
    FOR i = 1 TO LEN(punctuation)
        find = " " + MID(punctuation, i, 1) + " "
        place = INSTR(b, find)
        WHILE place > 0
            IF place = 1 THEN
                b = MID(punctuation, i, 1) + MID(b, place + 3)
            ELSE
                b = MID(b, 1, place - 1) + MID(punctuation, i, 1) + MID(b, place + 3)
            END IF
            place = INSTR(b, find)
        WEND
    NEXT
    joinPunctuation = b
END Function

' =============================== here is the heart of ELIZA / Player function
FUNCTION GetReply () as string
    DIM inpt AS STRING, tail AS STRING, answ AS STRING
    DIM kFlag AS INTEGER, k AS INTEGER, kFound AS INTEGER, l AS INTEGER, w AS INTEGER

    ' USER INPUT SECTION
    PRINT You + ": ";: LINE INPUT "", inpt
    IF LCASE(inpt) = "q" OR LCASE(inpt) = "x" OR LCASE(inpt) = "goodbye" OR LCASE(inpt) = "good night" OR LCASE(inpt) = "bye" THEN
        GetReply = "Goodbye!": EXIT FUNCTION
    END IF
    inpt = " " + inpt + " " '<< need this because keywords embedded in spaces to ID whole words only
    inpt = isolatePunctuation(inpt)
    FOR k = 1 TO kCnt 'loop through key words until we find a match
        kFound = INSTR(LCASE(inpt), LCASE(keywords(k)))
        IF kFound > 0 THEN '>>> need the following for * in some replies
            tail = " " + MID(inpt, kFound + LEN(keywords(k)))
            FOR l = 1 TO LEN(tail) 'DO NOT USE INSTR
                FOR w = 1 TO wCnt 'swap words in tail if used there
                    IF LCASE(MID(tail, l, LEN(wordIn(w)))) = LCASE(wordIn(w)) THEN 'swap words exit for
                        tail = MID(tail, 1, l - 1) + wordOut(w) + MID(tail, l + LEN(wordIn(w)))
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
    IF RIGHT(answ, 1) <> "*" THEN GetReply = answ: EXIT FUNCTION 'oh so the * signal an append to reply!
    If Trim(tail) = "" THEN
        GetReply = "is that all you got to say? " 
    ELSE
        tail = joinPunctuation(tail)
        GetReply = MID(answ, 1, LEN(answ) - 1) + tail
    END IF
END FUNCTION

sub slow (text as String )
   DIM as integer speed(0 to 4) => {50,100,20,300,250}
   for i as integer = 1 to len(text)
      print mid(text, i, 1);
      SLEEP speed(INT(RND*ubound(speed))) 
   next
end sub

SUB speakTotext (lines as string) 'uses voice command line voice.exe
    PRINT Script & ": ";:slow  lines :print :print
'    Shell("voice -r -1 -n " & Chr(34) & TTSvoice & Chr(34) & " " & Chr(34) & lines & Chr(34))
    'SHELL _HIDE "espeak -ven-us+f2 -s150 " + CHR$(34) + lines$ + CHR$(34)
END Sub

sub restart()
   REDIM keywords(0) AS STRING, replies(0) AS STRING, wordIn(0) AS STRING, wordOut(0) AS STRING
   REDIM rStarts(0) AS INTEGER, rEnds(0) AS INTEGER, rIndex(0) AS INTEGER

end sub

sub conversation(file as String)
   dim i as Integer
   restart()
   sound("./icq-horn.wav")
   cls
   for i = 1 to 10
      locate 5, 5
      print "             ";
      sleep 250
      locate 5, 5
      print "connecting...";
      sleep 250
   next
   cls
   DIM rply AS STRING '              for main loop
   LoadArrays file '   check file load, OK checks out
   PRINT Greeting: PRINT '           start testing main Eliza code
   DO
      rply = GetReply
      PRINT: speakTotext rply
   LOOP UNTIL rply = "Goodbye!"
   cls
   locate 5, 5
   print "disconnected...  "
   sleep
end sub






function _
  getKeys( _
    byref keysToCatch as const string ) _
  as string
  
  dim as string _
    k
  
  do
    k => inkey()
    
    sleep( 1, 1 )
  loop until( inStr( keysToCatch, k ) )
  
  '' Clear keyboard buffer
  do while( len( inkey() ) > 0 )
    sleep( 1, 1 )
  loop
  
  return( k )
end Function



Function dates1 (months As Integer) As String
  dim as const string _
    monthNames(0 To 11) => { _
      "January", _
      "February", _
      "March", _
      "April", _
      "May", _
      "June", _
      "July", _
      "August", _
      "September", _
      "October", _
      "November", _
      "December" }
    
    dim as integer _
      m => ( months + 11 ) mod 12
    
    return( monthNames( m ) & "," & str( int( ( months - 1 ) / 12 ) + 1997 ) )
END Function

'function as string arrays
Function hint(n As Integer) As String
	Select Case n
		Case 1:Return "the target is a single woman in her 30's working in a sensetive high level job in a international bank"
		Case 2:Return "the target has a drinking problem"
		Case 3:Return "the target uses a windoews 95 laptop at home and desktop with windoew 3.11 with antivirus and a security software in her job"
		Case 4:Return "the target is in a relasionship but she is also having an affair with someone - a strong opportonity for extortion"
		Case 5:Return "the target has a thing for the jazz singer 'chet backer'"
		Case 6:Return "the target likes gay porn - if she is in the closet it is a chance for extortion"
		Case 7:Return "the target has a cat named 'pogi'"
		Case 8:Return "the birthday yaer of the target is '1964'"
		Case 9:Return "the target's favorite movie is 'unforgiven'"
		Case 10:Return "the target uses an AOL email account 'sophie1964@aol.com' and she is also active is several forums on AOL under the nickname 'pogi64'"

	End Select
End Function




Sub opening()
	Dim k As String
	'Dim As Double start, current 
	cp 5, "- H A C K E R -"
	cp 6, "- G A M E -"
	cp 8, " - B Y -"
	cp 9, "- R O N 7 7 -"
	Color 31:cp 11, "- P R E S S  E N T E R  T O  B E G I N -"
	cp 12, "- O R  E S C  T O  E X I T -": Color 7
	
	sound("./Dial Up Modem-SoundBible.com-909377495.wav")
	
	k = GetKeys(Chr(13) + Chr(27))
	If k = Chr(27) Then
		End
	Else
	Cls
	cp 5, "- H A C K E D ! -"
	EndIf
	sleep
	
	
End Sub

Sub start()
	Cls
	cp 2, "START POINT"
	cp 4, "YOU ARE A HACKER WORKING FOR THE MAFIA"
	cp 5, "YOU ARE NAMELESS AND YOU WORK FOR A STRICT ONE TIME CONTRACT"
	cp 6, "YOU HAVE ONE YAER TO HACK A TARGET SECRETS (PASSWORDS, DATA ETC.)"
	cp 7, "IF YOU SUCCEED YOU CAN RETIRE FOR LIFE WITH 450,000$"
	cp 8, "IF YOU FAILE YOU WILL BE ASSASSINATED OR"
	cp 9, "TURNED OVER TO THE LAW AUTHORITIES"
	Color 4:cp 10, "MAKE NO MISTAKE! YOU HAVE NO CHOICE!": Color 7
	cp 12, "PRESS ANY KEY..."
	Sleep
End Sub

Sub hints()
	cls
	Dim n As Integer, k As String
	cp 2, "HERE IS WHAT YOUR MAFIA BOSS IS WILLING TO TELL YOU ABOUT THE TARGET"
	cp 3, "PRESS SPACEBAR FOR INFORMATION HINTS"
	Print
	Do
		k = GetKeys(Chr(32))
		If k = Chr(32) Then
		Print hint(n): Print
		n = n+1
		End If
	Loop Until n = 11
	Sleep
	 
End Sub

sub airplane()
	dim k as string, message as string
	cls
	cp 4, "CHOOSE A DESTINATION..."
	cp 6, "1. GET LOST IN NORTH AMERICA - 20,000$"
	cp 7, "2. DISAPPEAR IN SOUTH AMERICA - 16,000$"
	cp 8, "3. VANISH IN ASIA - 12,000$"
	cp 9, "4. ON WAY TICKET TO NO-WHERE - 9,000$"
	cp 10, "GO BACK AND THINK OF IT"
	k = getkeys("12345")
	if k = "1" and money >= 20000 then
	message = "YOU CAN NOW GET LOST IN NORTH AMERICA"
	ticket = 1
	elseif k = "2" and money >= 16000 then
	message = "YOU CAN NOW DISAPPEAR IN SOUTH AMERICA"
	ticket = 1
	elseif k = "3" and money >= 12000 then
	message = "YOU CAN NOW VANISH IN ASIA"
	ticket = 1
	elseif k ="4" and money >= 9000 then
	message = "YOU CAN NOW GO TO NO-WHERE"
	ticket = 1
	elseif k ="5" then
	exit sub
	else
	message = "COME BACK WHEN YOU ARE SERIOUS AND HAVE THE MONEY PAL!"
	end if
	print
	print
	print
	print message
end sub


sub runawayMoney()
	dim cash as integer, message as string
	cls
	cp 4, "YOU HAVE NOW " & money & "$"
	print
	print
	print
	input "HOW MUCH MONEY WILL YOU TRANSFER TO A SECRET BANK ACCOUNT FOR YOUR LIFE AFTER YOU ESCAPE?: ", cash
	if cash <= 0 then 
		message = "YOU MUST BE JOKING! COME BACK WHEN YOUR SERIOUS"
	elseif cash > money then
		message = "YOU DON'T HAVE THAT AMOUNT OF MONEY! COME BACK LATER"
	else
		money = money - cash
	message = "MONEY TRANSFERED!"
		escapeMoney = escapeMoney + cash
	end if
	 
	print
	print
	print message
	
	sleep
end sub


sub tor()
	dim k as string, pass as string 
	cls
	cp 4, "- WELCOME TO TOR DARKNET IDENTITY MARKETPLACE -"
	cp 7, "1. AN AMERICAN PASSPORT - 45,000 DOLLARS"
	cp 8, "2. A BRITISH PASSPORT - 40,000 DOLLARS"
	cp 9, "3. A CANADIAN PASSPORT - 38,000 DOLLARS"
	cp 10, "4. A SOUTH AMERICAN PASSPORT - 32,000 DOLLARS"
	cp 11, "5. AN AUSTRALIAN PASSPORT - 30,000 DOLLARS"
	cp 13, "6. EXIT"
	k = GetKeys("123456")
	if k = "1" and money >= 45000 then
		pass = "AMERICAN PASSPORT AQUIERED!"
		money = money - 45000
		passport = 1
	elseif k = "2" and money >= 40000 then
		pass = "BRITISH PASSPORT AQUIERED!"
		money = money - 40000
		passport = 1
	elseif k = "3" and money >=38000 then
		pass = "CANADIAN PASSPORT AQUIERED!"
		money = money - 38000
		passport = 1 
	elseif k = "4" and money >= 32000 then
		pass = "SOUTH AMERICAN PASSPORT AQUIERED!"
		money = money - 32000
		passport = 1
	elseif k = "5" and money >= 30000 then
		pass = "AUSTRALIAN PASSPORT AQUIERED!"
		money = money - 30000
		passport = 1
	elseif k = "6" then
		exit sub
	else
	pass = "NOT ENOGHT MONEY!"
	end if
	
	print
	print
	print pass
end sub



Sub email()
	Dim As String password = "almostblue", guess
	
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
	EndIf
	cp 8, "PRESS ANY KEY..."
	Sleep()
	
End Sub


Sub hack()
	Dim As String HPC, HW, k, h, w
	
	Select Case HhackIndicator
		Case 1: HPC = "- 1. HACK TARGET'S HOME PC -"
		Case 2: HPC = "- 1. TARGET'S HOME PC HACKED -"
	End Select
	If WhackIndicator = 1 Then
		HW = "- 2. BRUTE FORCE ACCESS TO TARGET'S WORK ACCOUNT -"
	ElseIf  WhackIndicator = 2 And Wcounter < 3 Then
		HW = "- 2. BRUTE FORCE ACCESS TO TARGET'S WORK ACCOUNT IN PROGRESS-"
		Wcounter += 1
	ElseIf WhackIndicator = 2 And Wcounter >= 3 Then
		WhackIndicator = 3
		HW = "- 2. TARGET'S WORK ACCOUNT HACKED -"
	ElseIf WhackIndicator = 3 Then
		HW = "- 2. TARGET'S WORK ACCOUNT HACKED -"
	EndIf 
	Cls
	cp 2, "- HACK TARGET -"
	cp 5, HPC
	cp 6, HW
	cp 7, "- 3. HACK TARGET'S EMAIL ACCOUNT -"
	'~ cp 8, "- 4. GO TO MAIN -"
	k = GetKeys("123")
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
		email()
	EndIf
	cp 11, h
	cp 13, w
	
	cp 14, "PRESS ANY KEY..."
	Sleep()
	
End Sub

sub steal()
dim k as string, m as string

cls
cp 4, "YOU GAIN ACCESS TO THE BANK AND FEEL LIKE A KID IN A CANDY STORE... WHAT DO YOU SAY SHELL WE STEAL SOME MONEY FROM THE BANK?"
cp 6, "- 1. STEAL 100,000 DOLLARS OR 2. STAY HONEST AND DON'T TOUCH THE MONEY -"
k = getkeys("12")
if k = "1" then
      m = "YOU STEAL 100,000 DOLLARS FROM THE BANK AND NOW YOU ARE A WANTED CRIMINAL"
      money = money + 100000
   ELSEIF k = "2" then
      m = "YOU DID THE RIGHT SMART THING AND KEPT FROM BEING EXPOUSED IT MAY NOT IMPROVE YOUR WALLET BUT YOU LIVE AT PEACE WITH YOURSELF"
   end if
   print
   print
   print m
	sleep
end sub

Sub spy()
	Dim As String k, spy1
	Cls
	cp 2, "- SPY ON TARGET -"
	cp 5, "- 1. AOL FORUMS -"
	cp 6, "- 2. TARGET'S HOME PC -"
	cp 7, "- 3. TARGET'S EMAIL ACCOUNT -"
	cp 8, "- 4. TARGET'S WORK ACCOUNT -"
	k = GetKeys("1234")
   if k = "1" then
      spy1 = "YOU LOOK FOR SOPHY'S UNDER THE NICKNAME 'POGI64' FORUM'S MESSAGES AND YOU FIND SHE IS ACTIVE IN FORUMS OF A.A. AND CAT'S OWNERS YOU READ SOME OF HER MESSAGES THEY TELL YOU THAT SOPHY'S STRUGGELING WITH ALCOHOLIZEM AND DEPRESSION"
   ELSEIF k = "2" and HhackIndicator = 1 then
      spy1 = "YOU DON'T HAVE YET ACCESS TO TARGET'S HOME PC"
   ELSEIF k = "2" and HhackIndicator = 2 then
      spy1 = "YOU SEARCH DEEP INTO SOPHY'S PC AND FIND OUT A FEW THINGS - SHE IS BEING BLACKMAILED BY HER EX-LOVER AND SHE'S STUCK IN AN UN-HAPPY MARRIGE ONE THIG THAT WORRY YOU IS YOU FIND A SUICIDE LETTER UNDER HER NAME WITHOUT A DATE - IF SHE KILL'S HERSELF BEFORE YOU COMPLITE THE JOB YOU ARE A DEAD MAN!"
   ELSEIF k = "3" and Ehacked = 0  then
      spy1 = "YOU DON'T HAVE ACCESS TO TARGET'S EMAIL"
   elseif k = "3" and Ehacked = 1 then
      spy1 = "SOPHY'S EMAIL JUST LIKE HER WHOLE LIFE IS A MESS OF MISSERY YOU FIND DESPERET REJECTED LOVE LETTERS AND DESPERET REQUESTS FOR A.A. REHAB CENTERS... SOPHY IS ONE MISSRABLE PERSON YOU TELL YOURSELF AND HER LIFE IS A MESS"
   ELSEIF k = "4" and WhackIndicator < 3 then
      spy1 = "YOU DO NOT HAVE ACCESS TO TARGET'S WORK ACCOUNT"
   elseif k = "4" and WhackIndicator >= 3 then
      steal()
      exit sub  
   end if
   print
   print
   print spy1
   print
   
	sleep
End Sub

Sub escape()
	Cls
	Dim k As String, message as string
	cp 2, dates1(Time2)
	cp 4, "MONEY: " + Str(money) + "$"
	Color 4:cp 6, "- ESCAPE PLAN -"
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
	cp 19, "- 5. SWITCH SIDE (CONTACT THE FBI) -":Color 7
	cp 20, "- 6. GO BACK -"
	k = GetKeys("123456")
	If k = "6" Then
		Exit Sub
	elseif k = "4" then
	  if passport = 1 and ticket = 1 and escapeMoney > 0 then
		the_end("end.txt",0)
      else
		message = "YOU ARE NOT READY TO DISAPPEAR"
      end if
	elseif k = "1" then
		select case passport
		case 0
			tor()
		case 1
			message = "YOU ALREADY HAVE A PASSPORT AND A FAKE IDENTITY!"
		end select
	elseif k = "2" then
		runawayMoney()
	elseif k = "3" then
		select case ticket
		case 0
		airplane()
		case 1
		message = "YOU ALREADY HAVE A ONE WAY TICKET OUT OF HERE!"
		END SELECT
	elseif k = "5" then
		the_end("goodEnd.txt", 0)
	
	End If
	
	print
	print
	print message
	Sleep()
End Sub

Sub job()
	Do
	Cls
	Dim k As String
	cp 2, dates1(Time2)
	cp 4, "MONEY: " + Str(money) + "$"
	cp 6, "- DO THE JOB -"
	cp 8, "- 1. SPY ON TARGET -"
	cp 9, "- 2. HACK TARGET -"
	cp 10, "- 3. SOCIAL ENGINEERING -"
	cp 11, "- 4. EXTORT TARGET -"
	cp 12, "- 5. GO BACK -"
	k = GetKeys("12345")
	If k = "5" Then
		Exit Sub
   ElseIf k = "2" Then
      hack()
   elseif k = "1" then
      spy()
   elseif k = "3" then
      cls
      if sophie_state = 0 then
		conversation("sophy.txt")
      elseif sophie_state = 1 then
		conversation("sophy2.txt")
         
      end if
   elseif k = "4" then
      cp 14, "YOU WRITE AN ANONYMOUS BLACKMAIL EMAIL FOR 40,000 DOLLARES AND SEND IT TO SOPHIE"
      cp 16, "PRESS ANY KEY..."
      SLEEP
      CLS
      cp 8, "SOPHIE TRYS TO COMMITE SUICIDE AND NOW IS AT THE HOSPITAL, YOU ARE WANTED BY THE POLICE"
      cp 10, "PRESS ANY KEY..."
      sophie_state = 1
      sleep
      
   EndIf
	Loop
End Sub

Sub main()
	Dim k As String
	Do
		'100
		If Time2 = 14 Then end
		cls
		cp 2, dates1(Time2)
		'Time = Time + 1
		cp 4, "MONEY: " + Str(money) + "$"
		'money = money + 800
		cp 7, "- 1. DO THE JOB -"
		Color 4:cp 10,"- 2. ESCAPE PLAN -":Color 7
		cp 13, "- 3. NEXT MONTH -" 
		cp 16, "- PRESS ESC TO EXIT -"
		k = GetKeys("123" + Chr(27))
		If k = "2" Then 
			escape()
		ElseIf k = "1" Then
			job()
		ElseIf k = "3" Then
			Time2 += 1
			money += 800
			if Time2 = 14 then
			the_end("badEnd.txt", 1)
			end if
		End If 
	Loop Until k = Chr(27)
End Sub

money = 1000
Opening()
start()
hints()
main()
