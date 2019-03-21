    CLS
    MODE 0, 1, 1, 1, 1
    BORDER 1
    WAIT

RESTART:
    PRINT AT 62 COLOR 5, "Press any button"
    PRINT AT 86 COLOR 5, "to play"
    PRINT AT 124 COLOR 7, "Greensleeves"

    WHILE CONT = 0
        WAIT
    WEND

    FOR I = 60 TO 99
        #BACKTAB(I) = 0
    NEXT I

    WHILE CONT <> 0
        WAIT
    WEND

    PLAY FULL
    PLAY Greensleeves

    WHILE MUSIC.PLAYING
        WAIT
    WEND

    GOTO RESTART

    ASM CFGVAR "name" = "Greensleeves"

Greensleeves:
    DATA 9

    MUSIC A4,-,-,-
    MUSIC S,-,-,-

    MUSIC C5,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC D5,B3,-,-
    MUSIC S,S,-,-

    MUSIC E5,C4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC F5,S,-,-
    MUSIC E5,S,-,-
    MUSIC S,S,-,-

    MUSIC D5,G3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC B4,S,-,-
    MUSIC S,S,-,-

    MUSIC B3,G4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,A4,-,-
    MUSIC S,B4,-,-
    MUSIC S,S,-,-

    MUSIC C5,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC A4,S,-,-
    MUSIC S,S,-,-

    MUSIC A4,F4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC G4#,S,-,-
    MUSIC A4,S,-,-
    MUSIC S,S,-,-

    MUSIC B4,E4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC G4#,S,-,-
    MUSIC S,S,-,-

    MUSIC E3,E4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,A4,-,-
    MUSIC S,S,-,-

    MUSIC C5,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC D5,B3,-,-
    MUSIC S,S,-,-

    MUSIC E5,C4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC F5,S,-,-
    MUSIC E5,S,-,-
    MUSIC S,S,-,-

    MUSIC D5,G3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC B4,S,-,-
    MUSIC S,S,-,-

    MUSIC B3,G4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,A4,-,-
    MUSIC S,B4,-,-
    MUSIC S,S,-,-

    MUSIC C5,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC B4,S,-,-
    MUSIC A4,S,-,-
    MUSIC S,S,-,-

    MUSIC G4#,E3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC F4#,S,-,-
    MUSIC G4#,S,-,-
    MUSIC S,S,-,-

    MUSIC A4,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-

    MUSIC A3,A4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-

    MUSIC G5,C4,E4,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-

    MUSIC G5,C4,E4,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-
    MUSIC F5,S,S,-
    MUSIC E5,S,S,-
    MUSIC S,S,S,-

    MUSIC D5,G3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC B4,S,-,-
    MUSIC S,S,-,-

    MUSIC B3,G4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,A4,-,-
    MUSIC S,B4,-,-
    MUSIC S,S,-,-

    MUSIC C5,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC A4,S,-,-
    MUSIC S,S,-,-

    MUSIC A4,F4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC G4#,S,-,-
    MUSIC A4,S,-,-
    MUSIC S,S,-,-

    MUSIC B4,E4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC G4#,S,-,-
    MUSIC S,S,-,-

    MUSIC E3,E4,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-

    MUSIC G5,C4,E4,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-

    MUSIC G5,C4,E4,-
    MUSIC S,S,S,-
    MUSIC S,S,S,-
    MUSIC F5,S,S,-
    MUSIC E5,S,S,-
    MUSIC S,S,S,-

    MUSIC D5,G3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC B4,S,-,-
    MUSIC S,S,-,-

    MUSIC G4,B3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC A4,S,-,-
    MUSIC B4,S,-,-
    MUSIC S,S,-,-

    MUSIC C5,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC B4,S,-,-
    MUSIC A4,S,-,-
    MUSIC S,S,-,-

    MUSIC G4#,E3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC F4#,S,-,-
    MUSIC G4#,S,-,-
    MUSIC S,S,-,-

    MUSIC A4,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-

    MUSIC A4,A3,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-
    MUSIC S,S,-,-

    MUSIC -,-,-,-

    MUSIC STOP
