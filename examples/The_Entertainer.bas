    CLS
    MODE 0, 1, 1, 1, 1
    BORDER 1
    WAIT

RESTART:
    PRINT AT 62 COLOR 5, "Press any button"
    PRINT AT 86 COLOR 5, "to play"
    PRINT AT 122 COLOR 7, "The Entertainer"

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
    PLAY The_Entertainer

    WHILE MUSIC.PLAYING
        WAIT
    WEND

    GOTO RESTART

    ASM CFGVAR "name" = "The Entertainer"

The_Entertainer:
    DATA 10

    MUSIC D5,D6,-,-,-,-,-
    MUSIC E5,E6,-,-,-,-,-
    MUSIC C5,C6,-,-,-,-,-
    MUSIC A4,A5,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC B5,B4,-,-,-,-,-
    MUSIC G5,G4,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-

    MUSIC D5,D4,-,-,-,-,-
    MUSIC E5,E4,-,-,-,-,-
    MUSIC C5,C4,-,-,-,-,-
    MUSIC A4,A3,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC B4,B3,-,-,-,-,-
    MUSIC G4,G3,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-

    MUSIC D4,D3,-,-,-,-,-
    MUSIC E4,E3,-,-,-,-,-
    MUSIC C4,C3,-,-,-,-,-
    MUSIC A3,A2,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC B2,B3,-,-,-,-,-
    MUSIC A2,A3,-,-,-,-,-
    MUSIC G2#,G3#,-,-,-,-,-

    MUSIC G2,G3,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC -,-,-,-,-,-,-
    MUSIC -,-,-,-,-,-,-
    MUSIC G5,D5,B4,-,G4,G2,-            ' dropped G1
    MUSIC S,S,S,-,S,S,-
    MUSIC D4,B3,G3,-,-,-,-
    MUSIC D4#,S,S,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,G3,E3,-,C4,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G3,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4,A3#,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,C4,A3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC C6,S,S,-,E5,C5,-
    MUSIC F5,D5,D6,-,C4,G3,-
    MUSIC D6#,F5#,D5#,-,S,S,-

    MUSIC E6,G5,E5,-,G2,-,-
    MUSIC C6,E5,C5,-,S,-,-
    MUSIC D5,D6,F5,-,G3,C4,E3
    MUSIC G5,E5,E6,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B5,D5,B4,-,S,-,-
    MUSIC D6,F5,D5,-,B3,G3,F3
    MUSIC S,S,S,-,S,S,S

    MUSIC C6,C5,E5,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,E3,G3,C4
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,C4,G3,E3
    MUSIC S,S,S,-,S,S,S
    MUSIC D4,B3,G3,-,-,-,-
    MUSIC D4#,S,S,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,E3,G3,-,C4,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G3,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4,A3#,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,A3,C4,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC A5,C5,A4,-,D3#,D2#,-
    MUSIC G5,C5,G4,-,S,S,-

    MUSIC F5#,C5,F4#,-,D3,D2,-
    MUSIC A5,A4,-,-,S,S,-
    MUSIC C6,C5,D3,-,A3,C4,F3#
    MUSIC E6,E5,S,-,S,S,S
    MUSIC S,S,D3,-,-,-,-
    MUSIC D6,D5,S,-,-,-,-
    MUSIC C6,C5,C4,-,F3#,A3,-
    MUSIC A5,A4,S,-,S,S,-

    MUSIC D6,D5,F5,-,B3,G3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,G3,G2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,A3,A2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC D4,B3,B2,-,-,-,-
    MUSIC D4#,S,S,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,C4,G3,-,E3,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G2,G3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4,A3#,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,C4,A3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E2,E3,-,-,-,-
    MUSIC C6,S,S,-,C5,E5,-
    MUSIC D6,F5,D5,-,C4,G3,-
    MUSIC D6#,F5#,D5#,-,S,S,-

    MUSIC E5,G5,E6,-,G2,-,-
    MUSIC C6,E5,C5,-,S,-,-
    MUSIC D6,F5,D5,-,C4,G3,E3
    MUSIC E6,G5,E5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B5,D5,B4,-,S,-,-
    MUSIC D6,F5,D5,-,F3,B3,G3
    MUSIC S,S,S,-,S,S,S

    MUSIC C6,E5,C5,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,C4,G3,E3
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,E4,C4,G3
    MUSIC S,S,S,-,S,S,S
    MUSIC C6,C5,-,-,-,-,-
    MUSIC D6,D5,-,-,-,-,-

    MUSIC E6,E5,C4,-,C3,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,E4,-,G3,C4,-
    MUSIC E6,E5,S,-,S,S,-
    MUSIC S,S,A3#,-,A2#,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,E4,-,C4,G3,-
    MUSIC C6,C5,S,-,S,S,-

    MUSIC E6,E5,A3,-,A2,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,F4,-,C4,A3,-
    MUSIC E6,E5,S,-,S,S,-
    MUSIC S,S,G3#,-,G2#,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,F4,-,C4,G3#,-
    MUSIC C6,C5,S,-,S,S,-

    MUSIC G5,E5,E6,-,G3,G2,-
    MUSIC C6,E5,C5,-,S,S,-
    MUSIC D6,F5,D5,-,E4,C4,G3
    MUSIC E6,G5,E5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B5,B4,D5,-,S,-,-
    MUSIC D6,D5,F5,-,B3,G3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC C6,C5,E5,-,C3,G3,C4
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,G3,G2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,A3,A2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC D4,B3,B2,-,-,-,-
    MUSIC D4#,S,S,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,C4,G3,-,E3,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G3,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,A3#,G3,-,C4,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,C4,A3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC C6,S,S,-,E5,C5,-
    MUSIC D5,D6,F5,-,C4,G3,-
    MUSIC D6#,F5#,D5#,-,S,S,-

    MUSIC E6,G5,E5,-,G2,-,-
    MUSIC C6,E5,C5,-,S,-,-
    MUSIC F5,D5,D6,-,C4,G3,E3
    MUSIC E6,G5,E5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B5,D5,B4,-,S,-,-
    MUSIC D5,D6,F5,-,F3,G3,B3
    MUSIC S,S,S,-,S,S,S

    MUSIC E5,C5,C6,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,C4,G3,E3
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,C4,G3,E3
    MUSIC S,S,S,-,S,S,S
    MUSIC D4,B3,G3,-,-,-,-
    MUSIC D4#,S,S,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,G3,E3,-,C4,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G3,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4,A3#,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,C4,A3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC A5,C5,A4,-,D3#,D2#,-
    MUSIC G5,C5,G4,-,S,S,-

    MUSIC F4#,C5,F5#,-,D3,D2,-
    MUSIC A5,A4,-,-,S,S,-
    MUSIC C6,C5,C4,-,A3,F3#,D3
    MUSIC E6,E5,S,-,S,S,S
    MUSIC S,S,D3,-,-,-,-
    MUSIC D6,D5,S,-,-,-,-
    MUSIC C5,C6,C4,-,A3,F3#,-
    MUSIC A5,A4,S,-,S,S,-

    MUSIC D6,F5,D5,-,B3,G3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,G3,G2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,A3,A2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC D4,B3,B2,-,-,-,-
    MUSIC D4#,S,S,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,G3,C4,-,E3,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G3,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4,A3#,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,A3,C4,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC C6,S,S,-,E5,C5,-
    MUSIC D6,F5,D5,-,C4,G3,-
    MUSIC D5#,F5#,D6#,-,S,S,-

    MUSIC E6,G5,E5,-,G2,-,-
    MUSIC C6,E5,C5,-,S,-,-
    MUSIC D6,F5,D5,-,C4,G3,E3
    MUSIC E5,E6,G5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B5,D5,B4,-,S,-,-
    MUSIC D6,F5,D5,-,G3,F3,B3
    MUSIC S,S,S,-,S,S,S

    MUSIC C5,E5,C6,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,C4,G3,E3
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,G3,C4,E4
    MUSIC S,S,S,-,S,S,S
    MUSIC C6,C5,-,-,-,-,-
    MUSIC D6,D5,-,-,-,-,-

    MUSIC E6,E5,C4,-,C3,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,E4,-,C4,G3,-
    MUSIC E6,E5,S,-,S,S,-
    MUSIC S,S,A3#,-,A2#,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,E4,-,G3,C4,-
    MUSIC C6,C5,S,-,S,S,-

    MUSIC E6,E5,A3,-,A2,-,-
    MUSIC C5,C6,S,-,S,-,-
    MUSIC D6,D5,F4,-,C4,A3,-
    MUSIC E6,E5,S,-,S,S,-
    MUSIC S,S,G3#,-,G2#,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,F4,-,C4,G3#,-
    MUSIC C6,C5,S,-,S,S,-

    MUSIC E6,G5,E5,-,G3,G2,-
    MUSIC C6,E5,C5,-,S,S,-
    MUSIC D6,F5,D5,-,E4,C4,G3
    MUSIC E5,G5,E6,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B5,D5,B4,-,S,-,-
    MUSIC D6,F5,D5,-,G3,B3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC C5,E5,C6,-,C4,G3,C3
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,G3,G2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,C3,C2,-
    MUSIC E5,E4,C5,-,S,S,-
    MUSIC F5,F4,D5,-,-,-,-
    MUSIC F5#,D5#,F4#,-,-,-,-

    MUSIC G5,E5,G4,-,C2,C3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A5,E5,A4,-,E4,C4,G3
    MUSIC G5,G4,E5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC E5,C5,E4,-,S,-,-
    MUSIC F4,F5,D5,-,E4,C4,G3
    MUSIC F5#,D5#,F4#,-,S,S,S

    MUSIC G5,E5,G4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A5,E5,A4,-,C4,E4,G3
    MUSIC G4,E5,G5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC E5,-,-,-,S,-,-
    MUSIC C5,C4,E4,-,G3,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC A4,F2,-,-,-,-,-
    MUSIC B4,S,-,-,-,-,-
    MUSIC C5,F4,C4,-,A3,-,-
    MUSIC D5,S,S,-,S,-,-
    MUSIC E5,F3,-,-,-,-,-
    MUSIC D5,S,-,-,-,-,-
    MUSIC C5,F4,C4,-,G3#,-,-
    MUSIC D5,S,S,-,S,-,-

    MUSIC G4,E3,-,-,-,-,-
    MUSIC E5,S,-,-,-,-,-
    MUSIC F5,E4,C4,-,G3,-,-
    MUSIC G5,S,S,-,S,-,-
    MUSIC A5,G2,-,-,-,-,-
    MUSIC G5,S,-,-,-,-,-
    MUSIC E5,G3,E4,-,C4,-,-
    MUSIC F5,S,S,-,S,-,-

    MUSIC G5,E5,G4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A5,A4,E5,-,E4,C4,G3
    MUSIC G5,E5,G4,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC E5,C5,E4,-,S,-,-
    MUSIC F5,F4,D5,-,G3,C4,E4
    MUSIC F5#,D5#,F4#,-,S,S,S

    MUSIC G5,E5,G4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A5,E5,A4,-,E4,C4,G3
    MUSIC G4,E5,G5,-,S,S,S
    MUSIC S,S,S,-,E3,-,-
    MUSIC G5,-,-,-,S,-,-
    MUSIC A5,D3#,-,-,-,-,-
    MUSIC A5#,S,-,-,-,-,-

    MUSIC B5,G5,D5,-,D3,-,-
    MUSIC B5,D5,G5,-,S,-,-
    MUSIC S,S,S,-,G3,D4,B3
    MUSIC B5,C5,F5#,-,S,S,S
    MUSIC S,S,S,-,D3,-,-
    MUSIC A5,-,-,-,S,-,-
    MUSIC F5#,C5,D4,-,C4,A3,-
    MUSIC D5,-,S,-,S,S,-

    MUSIC G5,B4,D4,-,B3,G3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,F3,-,F2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,E3,-,E2,-,-
    MUSIC E5,E4,S,-,S,C5,-
    MUSIC F5,D5,F4,-,D3,D2,-
    MUSIC F5#,D5#,F4#,-,S,S,-

    MUSIC G4,G5,E5,-,C3,C2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A5,E5,A4,-,G3,E4,C4
    MUSIC G5,E5,G4,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC C5,E4,E5,-,S,-,-
    MUSIC F5,D5,F4,-,E4,C4,G3
    MUSIC F5#,D5#,F4#,-,S,S,S

    MUSIC G5,E5,G4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A4,E5,A5,-,E4,C4,G3
    MUSIC G5,G4,E5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC E5,-,-,-,S,-,-
    MUSIC C5,E4,C4,-,G3,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC A4,F2,-,-,-,-,-
    MUSIC B4,S,-,-,-,-,-
    MUSIC C5,F4,C4,-,A3,-,-
    MUSIC D5,S,S,-,S,-,-
    MUSIC E5,F3,-,-,-,-,-
    MUSIC D5,S,-,-,-,-,-
    MUSIC C5,G3#,F4,-,C4,-,-
    MUSIC D5,S,S,-,S,-,-

    MUSIC C5,E3,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC S,E4,G3,-,C4,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,C3,-,-,-,-,-
    MUSIC G4,S,-,-,-,-,-
    MUSIC F4#,E4,C4,-,A3#,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC C5,F4,C4,-,A3,F3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A4,F4,C4,-,A3,F3,-
    MUSIC C5,S,S,-,S,S,-
    MUSIC S,F3#,A3,-,D4#,C4,-
    MUSIC A4,S,S,-,S,S,-
    MUSIC C5,D4#,C4,-,A3,F3#,-
    MUSIC A4,S,S,-,S,S,-

    MUSIC G4,E4,C4,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-
    MUSIC E5,E4,G3,-,C4,-,-
    MUSIC G5,S,S,-,S,-,-
    MUSIC S,E4,C4,-,G3,-,-
    MUSIC E5,S,S,-,S,-,-
    MUSIC C5,E4,C4,-,G3,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC A4,F4#,D3,-,C4,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC C5,F4#,A3,-,D3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC E5,F4,B3,-,G3,-,-
    MUSIC D5,F4,S,-,S,-,-
    MUSIC S,S,B3,-,G3,-,-
    MUSIC C5,E4,S,-,S,-,-

    MUSIC S,S,C4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,G3,-,G2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,E3,-,E2,-,-
    MUSIC E6,C6,S,-,S,E5,-
    MUSIC F6,D6,F5,-,D3,D2,-
    MUSIC F6#,D6#,F5#,-,S,S,-

    MUSIC G5,E5,G4,-,C3,C2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A5,A4,E5,-,G3,E4,C4
    MUSIC G5,E5,G4,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC E5,C5,E4,-,S,-,-
    MUSIC F5,F4,D5,-,E4,C4,G3
    MUSIC F5#,D5#,F4#,-,S,S,S

    MUSIC G5,E5,G4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A5,E5,A4,-,E4,C4,G3
    MUSIC G4,E5,G5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC E5,-,-,-,S,-,-
    MUSIC C5,E4,C4,-,G3,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC A4,F2,-,-,-,-,-
    MUSIC B4,S,-,-,-,-,-
    MUSIC C5,F4,C4,-,A3,-,-
    MUSIC D5,S,S,-,S,-,-
    MUSIC E5,F3,-,-,-,-,-
    MUSIC D5,S,-,-,-,-,-
    MUSIC C5,G3#,C4,-,F4,-,-
    MUSIC D5,S,S,-,S,-,-

    MUSIC G4,E3,-,-,-,-,-
    MUSIC E5,S,-,-,-,-,-
    MUSIC F5,C4,E4,-,G3,-,-
    MUSIC G5,S,S,-,S,-,-
    MUSIC A5,G2,-,-,-,-,-
    MUSIC G5,S,-,-,-,-,-
    MUSIC E5,E4,C4,-,G3,-,-
    MUSIC F5,S,S,-,S,-,-

    MUSIC G5,E5,G4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A5,A4,E5,-,E4,C4,G3
    MUSIC G5,E5,G4,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC E5,C5,E4,-,S,-,-
    MUSIC F5,F4,D5,-,E4,C4,G3
    MUSIC F5#,D5#,F4#,-,S,S,S

    MUSIC G5,E5,G4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A5,E5,A4,-,G3,E4,C4
    MUSIC G4,E5,G5,-,S,S,S
    MUSIC S,S,S,-,E3,-,-
    MUSIC G5,-,-,-,S,-,-
    MUSIC A5,D3#,-,-,-,-,-
    MUSIC A5#,S,-,-,-,-,-

    MUSIC B5,G5,D5,-,D3,-,-
    MUSIC B5,D5,G5,-,S,-,-
    MUSIC S,S,S,-,D4,B3,G3
    MUSIC B5,C5,F5#,-,S,S,S
    MUSIC S,S,S,-,D3,-,-
    MUSIC A5,-,-,-,S,-,-
    MUSIC F5#,C5,C4,-,A3,D4,-
    MUSIC D5,-,S,-,S,S,-

    MUSIC G5,B4,D4,-,B3,G3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,F3,-,F2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,E3,-,E2,-,-
    MUSIC E5,E4,S,-,S,C5,-
    MUSIC F5,D5,F4,-,D2,D3,-
    MUSIC F5#,D5#,F4#,-,S,S,-

    MUSIC G4,G5,E5,-,C3,C2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A5,E5,A4,-,E4,C4,G3
    MUSIC G5,E5,G4,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC C5,E4,E5,-,S,-,-
    MUSIC F5,D5,F4,-,E4,C4,G3
    MUSIC F5#,D5#,F4#,-,S,S,S

    MUSIC G5,E5,G4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A4,A5,E5,-,E4,C4,G3
    MUSIC E5,G4,G5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC E5,-,-,-,S,-,-
    MUSIC C5,E4,C4,-,G3,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC A4,F2,-,-,-,-,-
    MUSIC B4,S,-,-,-,-,-
    MUSIC C5,C4,A3,-,F4,-,-
    MUSIC D5,S,S,-,S,-,-
    MUSIC E5,F3,-,-,-,-,-
    MUSIC D5,S,-,-,-,-,-
    MUSIC C5,F4,C4,-,G3#,-,-
    MUSIC D5,S,S,-,S,-,-

    MUSIC C5,E3,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC S,E4,C4,-,G3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,C3,-,-,-,-,-
    MUSIC G4,S,-,-,-,-,-
    MUSIC F4#,E4,C4,-,A3#,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC C5,F4,C4,-,A3,F3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A4,F4,F3,-,C4,A3,-
    MUSIC C5,S,S,-,S,S,-
    MUSIC S,D4#,C4,-,A3,F3#,-
    MUSIC A4,S,S,-,S,S,-
    MUSIC C5,D4#,C4,-,A3,F3#,-
    MUSIC A4,S,S,-,S,S,-

    MUSIC G4,E4,G3,-,C4,-,-
    MUSIC C5,S,S,-,S,-,-
    MUSIC E5,E4,C4,-,G3,-,-
    MUSIC G5,S,S,-,S,-,-
    MUSIC S,E4,C4,-,G3,-,-
    MUSIC E5,S,S,-,S,-,-
    MUSIC C5,E4,G3,-,C4,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC A4,F4#,C4,-,D3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC C5,F4#,A3,-,D3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC E5,F4,B3,-,G3,-,-
    MUSIC D5,F4,S,-,S,-,-
    MUSIC S,S,B3,-,G3,-,-
    MUSIC C5,E4,S,-,S,-,-

    MUSIC S,S,C4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,G3,-,G2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,C3,-,C2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC D4,-,-,-,-,-,-
    MUSIC D4#,-,-,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,C4,E3,-,G3,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G3,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4,A3#,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,C4,A3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC C6,S,S,-,E5,C5,-
    MUSIC D6,F5,D5,-,C4,G3,-
    MUSIC D6#,F5#,D5#,-,S,S,-

    MUSIC G5,E5,E6,-,G2,-,-
    MUSIC C6,E5,C5,-,S,-,-
    MUSIC D6,F5,D5,-,C4,G3,E3
    MUSIC E6,G5,E5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B5,D5,B4,-,S,-,-
    MUSIC D6,F5,D5,-,G3,F3,B3
    MUSIC S,S,S,-,S,S,S

    MUSIC C6,E5,C5,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,C4,G3,E3
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,G3,E3,C4
    MUSIC S,S,S,-,S,S,S
    MUSIC D4,B3,G3,-,-,-,-
    MUSIC D4#,S,S,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,C4,G3,-,E3,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G3,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4,G3,-,A3#,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,C4,A3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC C5,A5,A4,-,D3#,D2#,-
    MUSIC G5,C5,G4,-,S,S,-

    MUSIC F5#,C5,F4#,-,D3,D2,-
    MUSIC A5,A4,-,-,S,S,-
    MUSIC C6,C5,C4,-,A3,F3#,D3
    MUSIC E6,E5,S,-,S,S,S
    MUSIC S,S,D3,-,-,-,-
    MUSIC D6,D5,S,-,-,-,-
    MUSIC C6,C5,C4,-,A3,F3#,-
    MUSIC A5,A4,S,-,S,S,-

    MUSIC D5,D6,F5,-,B3,G3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,G3,G2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,A3,A2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC D4,B3,B2,-,-,-,-
    MUSIC D4#,S,S,-,-,-,-

    MUSIC E4,C3,-,-,-,-,-
    MUSIC C5,S,-,-,-,-,-
    MUSIC S,E3,G3,-,C4,-,-
    MUSIC E4,S,S,-,S,-,-
    MUSIC C5,G3,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4,A3#,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,A3,C4,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC C6,S,S,-,E5,C5,-
    MUSIC D6,F5,D5,-,C4,G3,-
    MUSIC D6#,F5#,D5#,-,S,S,-

    MUSIC E5,G5,E6,-,G2,-,-
    MUSIC C6,C5,E5,-,S,-,-
    MUSIC D6,D5,F5,-,C4,G3,E3
    MUSIC E6,G5,E5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B5,D5,B4,-,S,-,-
    MUSIC F5,D5,D6,-,B3,G3,F3
    MUSIC S,S,S,-,S,S,S

    MUSIC C6,E5,C5,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,C4,G3,E3
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,G3,E4,C4
    MUSIC S,S,S,-,S,S,S
    MUSIC C6,C5,-,-,-,-,-
    MUSIC D6,D5,-,-,-,-,-

    MUSIC E6,E5,C4,-,C3,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,E4,-,C4,G3,-
    MUSIC E6,E5,S,-,S,S,-
    MUSIC S,S,A3#,-,A2#,-,-
    MUSIC C5,C6,S,-,S,-,-
    MUSIC D6,D5,C4,-,G3,E4,-
    MUSIC C6,C5,S,-,S,S,-

    MUSIC E6,E5,A3,-,A2,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,F4,-,C4,A3,-
    MUSIC E6,E5,S,-,S,S,-
    MUSIC S,S,G3#,-,G2#,-,-
    MUSIC C6,C5,S,-,S,-,-
    MUSIC D6,D5,G3#,-,C4,F4,-
    MUSIC C6,C5,S,-,S,S,-

    MUSIC E6,G5,E5,-,G2,G3,-
    MUSIC C5,E5,C6,-,S,S,-
    MUSIC D6,F5,D5,-,E4,C4,G3
    MUSIC E6,G5,E5,-,S,S,S
    MUSIC S,S,S,-,G2,-,-
    MUSIC B4,D5,B5,-,S,-,-
    MUSIC D6,F5,D5,-,B3,G3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC C6,E5,C5,-,C4,G3,C3
    MUSIC S,S,S,-,S,S,S
    MUSIC S,S,S,-,G3,G2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC C6,E5,C5,-,C3,C2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC -,-,-,-,-,-,-
    MUSIC -,-,-,-,-,-,-

    MUSIC F5,A5,F2,-,-,-,-
    MUSIC G5#,-,S,-,-,-,-
    MUSIC A5,F5,A3,-,C4,F4,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,C3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC C6,A5,F5,-,F4,C4,A3
    MUSIC S,S,S,-,S,S,S

    MUSIC F5,A5#,D6,-,A2#,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,A4#,F4,D4             ' dropped A3#
    MUSIC S,S,S,-,A4,S,S
    MUSIC S,S,S,-,A4#,F3,-
    MUSIC S,S,S,-,C5,S,-
    MUSIC S,S,S,-,D5,F4,A3#             ' dropped D4
    MUSIC S,S,S,-,S,S,S

    MUSIC F5,D5,D2,-,-,-,-
    MUSIC E5,-,S,-,-,-,-
    MUSIC F5,D5,F4,-,D4,A3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,A2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC A5,F5,D5,-,A3,F4,D4
    MUSIC S,S,S,-,S,S,S

    MUSIC D5,G5,A5#,-,G2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,G4,A3#,D4
    MUSIC S,S,S,-,F4#,S,S
    MUSIC S,S,S,-,G4,D3,-
    MUSIC S,S,S,-,A4,S,-
    MUSIC S,S,S,-,A4#,D4,A3#
    MUSIC G5,-,-,-,S,S,S

    MUSIC D5,A3#,A2#,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC G5,A3#,D4,-,-,-,-
    MUSIC D5,S,S,-,-,-,-
    MUSIC S,G3,G2,-,-,-,-
    MUSIC G5,S,S,-,-,-,-
    MUSIC D5,G3#,G2#,-,-,-,-
    MUSIC S,S,S,-,-,-,-

    MUSIC C5,A3,A2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,A3,C4,-,F4,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC F5,D3,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC S,F4,D4,-,A3,-,-
    MUSIC S,S,S,-,S,-,-

    MUSIC E5,E3,-,-,-,-,-
    MUSIC G5#,S,-,-,-,-,-
    MUSIC B5,B3,D4,-,E4,-,-
    MUSIC E6,S,S,-,S,-,-
    MUSIC S,G3#,-,-,-,-,-
    MUSIC D6,S,-,-,-,-,-
    MUSIC B5,E4,D4,-,B3,-,-
    MUSIC C6,S,S,-,S,-,-

    MUSIC A5,E4,C4,-,A3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A5#,G3,C4,-,E4,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,S,C3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC A5,F5,F2,-,-,-,-
    MUSIC G5#,-,S,-,-,-,-
    MUSIC F5,A5,F4,-,C4,A3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,C3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC C6,A5,F5,-,F4,C4,A3
    MUSIC S,S,S,-,S,S,S

    MUSIC D6,A5#,F5,-,A2#,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,A4#,F4,A3#            ' dropped D4
    MUSIC S,S,S,-,A4,S,S
    MUSIC S,S,S,-,A4#,F3,-
    MUSIC S,S,S,-,C5,S,-
    MUSIC S,S,S,-,D5,F4,D4              ' dropped A3#
    MUSIC S,S,S,-,S,S,S

    MUSIC F5,D5,D2,-,-,-,-
    MUSIC E5,-,S,-,-,-,-
    MUSIC F5,D5,F4,-,A3,D4,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,A2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC D5,A5,F5,-,F4,D4,A3
    MUSIC S,S,S,-,S,S,S

    MUSIC A5#,G5,D5,-,G2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,G4,D4,A3#
    MUSIC S,S,S,-,F4#,S,S
    MUSIC S,S,S,-,G4,D3,-
    MUSIC S,S,S,-,A4,S,-
    MUSIC S,S,S,-,A4#,D4,A3#
    MUSIC G5,-,-,-,S,S,S

    MUSIC D5,A3#,A2#,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC G5,D4,A3#,-,-,-,-
    MUSIC D5,S,S,-,-,-,-
    MUSIC S,G2,G3,-,-,-,-
    MUSIC G5,S,S,-,-,-,-
    MUSIC D5,G3#,G2#,-,-,-,-
    MUSIC S,S,S,-,-,-,-

    MUSIC C5,A3,A2,-,-,-,-
    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC S,D2,D3,-,-,-,-
    MUSIC F5,B4,G4#,-,C3#,C2#,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,S,S,-
    MUSIC F5,-,-,-,S,S,-

    MUSIC A5,C5,A4,-,C3,C2,-
    MUSIC C6,C5,-,-,S,S,-
    MUSIC S,S,F4,-,C4,A3,-
    MUSIC G5,A4#,S,-,S,S,-
    MUSIC S,S,C4,-,C3,-,-
    MUSIC C5,S,S,-,S,-,-
    MUSIC D5,A4#,C3,-,C2,-,-
    MUSIC E5,S,S,-,S,-,-

    MUSIC F5,A4,F3,-,F2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC B4,-,-,-,-,-,-
    MUSIC C5,-,-,-,-,-,-
    MUSIC D5,-,-,-,-,-,-
    MUSIC E5,-,-,-,-,-,-
    MUSIC F5,-,-,-,-,-,-
    MUSIC G5,-,-,-,-,-,-

    MUSIC A5,F5,F2,-,-,-,-
    MUSIC G5#,-,S,-,-,-,-
    MUSIC F5,A5,F4,-,C4,A3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,C3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC C6,A5,F5,-,F4,C4,A3
    MUSIC S,S,S,-,S,S,S

    MUSIC D6,A5#,F5,-,A2#,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,A4#,F4,D4             ' dropped A3#
    MUSIC S,S,S,-,A4,S,S
    MUSIC S,S,S,-,A4#,F3,-
    MUSIC S,S,S,-,C5,S,-
    MUSIC S,S,S,-,D5,D4,F4              ' dropped A3#
    MUSIC S,S,S,-,S,S,S

    MUSIC F5,D5,D2,-,-,-,-
    MUSIC E5,-,S,-,-,-,-
    MUSIC F5,D5,F4,-,D4,A3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,A2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC A5,D5,F5,-,F4,D4,A3
    MUSIC S,S,S,-,S,S,S

    MUSIC D5,A5#,G5,-,G2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,G4,D4,A3#
    MUSIC S,S,S,-,F4#,S,S
    MUSIC S,S,S,-,G4,D3,-
    MUSIC S,S,S,-,A4,S,-
    MUSIC S,S,S,-,A4#,D4,A3#
    MUSIC G5,-,-,-,S,S,S

    MUSIC D5,A3#,A2#,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC G5,D4,A3#,-,-,-,-
    MUSIC D5,S,S,-,-,-,-
    MUSIC S,G3,G2,-,-,-,-
    MUSIC G5,S,S,-,-,-,-
    MUSIC D5,G3#,G2#,-,-,-,-
    MUSIC S,S,S,-,-,-,-

    MUSIC C5,A3,A2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC S,F4,C4,-,A3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC F5,D3,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC S,F4,D4,-,A3,-,-
    MUSIC S,S,S,-,S,-,-

    MUSIC E5,E3,-,-,-,-,-
    MUSIC G5#,S,-,-,-,-,-
    MUSIC B5,B3,E4,-,D4,-,-
    MUSIC E6,S,S,-,S,-,-
    MUSIC S,G3#,-,-,-,-,-
    MUSIC D6,S,-,-,-,-,-
    MUSIC B5,E4,D4,-,B3,-,-
    MUSIC C6,S,S,-,S,-,-

    MUSIC A5,E4,C4,-,A3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC A5#,E4,C4,-,G3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,S,C3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC A5,F5,F2,-,-,-,-
    MUSIC G5#,-,S,-,-,-,-
    MUSIC F5,A5,A3,-,F4,C4,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,C3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC C6,A5,F5,-,F4,C4,A3
    MUSIC S,S,S,-,S,S,S

    MUSIC D6,A5#,F5,-,A2#,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,A4#,F4,D4             ' dropped A3#
    MUSIC S,S,S,-,A4,S,S
    MUSIC S,S,S,-,A4#,F3,-
    MUSIC S,S,S,-,C5,S,-
    MUSIC S,S,S,-,D5,F4,D4              ' dropped A3#
    MUSIC S,S,S,-,S,S,S

    MUSIC D5,F5,D2,-,-,-,-
    MUSIC E5,-,S,-,-,-,-
    MUSIC F5,D5,F4,-,D4,A3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,A2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC A5,F5,D5,-,A3,D4,F4
    MUSIC S,S,S,-,S,S,S

    MUSIC A5#,G5,D5,-,G2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,G4,A3#,D4
    MUSIC S,S,S,-,F4#,S,S
    MUSIC S,S,S,-,G4,D3,-
    MUSIC S,S,S,-,A4,S,-
    MUSIC S,S,S,-,A4#,D4,A3#
    MUSIC G5,-,-,-,S,S,S

    MUSIC D5,A3#,A2#,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC G5,D4,A3#,-,-,-,-
    MUSIC D5,S,S,-,-,-,-
    MUSIC S,G3,G2,-,-,-,-
    MUSIC G5,S,S,-,-,-,-
    MUSIC D5,G3#,G2#,-,-,-,-
    MUSIC S,S,S,-,-,-,-

    MUSIC C5,A3,A2,-,-,-,-
    MUSIC S,F3,F2,-,-,-,-
    MUSIC S,E3,E2,-,-,-,-
    MUSIC S,D3,D2,-,-,-,-
    MUSIC F5,B4,G4#,-,C3#,C2#,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,S,S,-
    MUSIC F5,-,-,-,S,S,-

    MUSIC A4,C5,A5,-,C3,C2,-
    MUSIC C6,C5,-,-,S,S,-
    MUSIC S,S,F4,-,C4,A3,-
    MUSIC G5,A4#,S,-,S,S,-
    MUSIC S,S,C4,-,C3,-,-
    MUSIC C5,S,S,-,S,-,-
    MUSIC D5,A4#,C3,-,C2,-,-
    MUSIC E5,S,S,-,S,-,-

    MUSIC A4,F5,F3,-,F2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC -,-,-,-,-,-,-
    MUSIC -,-,-,-,-,-,-
    MUSIC C6,A5,F5,-,F6,F2,-            ' dropped F1
    MUSIC S,S,S,-,S,S,-
    MUSIC -,-,-,-,-,-,-
    MUSIC -,-,-,-,-,-,-

    MUSIC C5,F3,C4,-,A3,F4,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A4,F4,C4,-,A3,F3,-
    MUSIC C5,S,S,-,S,S,-
    MUSIC S,D4#,C4,-,A3,F3#,-
    MUSIC A4,S,S,-,S,S,-
    MUSIC C5,D4#,F3#,-,A3,C4,-
    MUSIC A4,S,S,-,S,S,-

    MUSIC G4,E4,C4,-,G3,-,-
    MUSIC C5,S,S,-,S,-,-
    MUSIC E5,G3,C4,-,E4,-,-
    MUSIC G5,S,S,-,S,-,-
    MUSIC S,E4,C4,-,G3,-,-
    MUSIC E5,S,S,-,S,-,-
    MUSIC C5,E4,C4,-,G3,-,-
    MUSIC G4,S,S,-,S,-,-

    MUSIC A4,F4#,C4,-,D3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC C5,F4#,A3,-,D3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC E5,F4,B3,-,G3,-,-
    MUSIC D5,F4,S,-,S,-,-
    MUSIC S,S,B3,-,G3,-,-
    MUSIC C5,E4,S,-,S,-,-

    MUSIC S,S,C4,-,C3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC C5,E5,G5,-,C6,C3,C2
    MUSIC S,S,S,-,S,S,S
    MUSIC -,-,-,-,-,-,-
    MUSIC -,-,-,-,-,-,-

    MUSIC F4,D4,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4#,F3,-,A3,-,-
    MUSIC F4,D4,S,-,S,-,-
    MUSIC S,S,A2,-,-,-,-
    MUSIC E4,C4#,S,-,-,-,-
    MUSIC D4,F4,A3,-,F3,-,-
    MUSIC S,S,S,-,S,-,-

    MUSIC F2,-,-,-,-,-,-
    MUSIC S,A4,-,-,-,-,-
    MUSIC F4,D5,A3,-,F3,-,-
    MUSIC A4,-,S,-,S,-,-
    MUSIC C5,A2,-,-,-,-,-
    MUSIC D5,S,-,-,-,-,-
    MUSIC C5,A3,F3,-,-,-,-
    MUSIC A4,S,S,-,-,-,-

    MUSIC G4,E4,C3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC F4#,D4#,C4,-,G3,E3,-
    MUSIC G4,E4,S,-,S,S,-
    MUSIC S,S,G2,-,-,-,-
    MUSIC F4#,D4#,S,-,-,-,-
    MUSIC G4,E4,E3,-,C4,G3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC C3,-,-,-,-,-,-
    MUSIC S,C5,-,-,-,-,-
    MUSIC E5,G4,C4,-,G3,E3,-
    MUSIC C5,-,S,-,S,S,-
    MUSIC D5,G2,-,-,-,-,-
    MUSIC E5,S,-,-,-,-,-
    MUSIC D5,E3,G3,-,C4,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC D5,B4,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC C5#,A4#,B3,-,G3,F3,-
    MUSIC D5,B4,S,-,S,S,-
    MUSIC S,S,B2,-,-,-,-
    MUSIC C5#,A4#,S,-,-,-,-
    MUSIC B4,D5,B3,-,G3,F3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC G2,-,-,-,-,-,-
    MUSIC S,F5,-,-,-,-,-
    MUSIC A5,B4,G3,-,B3,F3,-
    MUSIC F5,-,S,-,S,S,-
    MUSIC G5,D3,-,-,-,-,-
    MUSIC A5,S,-,-,-,-,-
    MUSIC G5,B3,G3,-,F3,-,-
    MUSIC F5,S,S,-,S,-,-

    MUSIC C6,C5,C4,-,F3#,D3#,-
    MUSIC C6,C5,S,-,S,S,-
    MUSIC C6,C5,C4,-,F3#,D3#,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A5,C5,C4,-,F3#,D3#,-
    MUSIC S,S,S,-,S,S,-

    MUSIC G5,C5,E3,-,C4,G3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC E4,G4,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-

    MUSIC F4,D4,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4#,A3,-,F3,-,-
    MUSIC F4,D4,S,-,S,-,-
    MUSIC S,S,A2,-,-,-,-
    MUSIC E4,C4#,S,-,-,-,-
    MUSIC F4,D4,A3,-,F3,-,-
    MUSIC S,S,S,-,S,-,-

    MUSIC F2,-,-,-,-,-,-
    MUSIC S,A4,-,-,-,-,-
    MUSIC D5,F4,F3,-,A3,-,-
    MUSIC A4,-,S,-,S,-,-
    MUSIC C5,A2,-,-,-,-,-
    MUSIC D5,S,-,-,-,-,-
    MUSIC C5,A3,F3,-,-,-,-
    MUSIC A4,S,S,-,-,-,-

    MUSIC G4,E4,C3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC D4#,F4#,C4,-,G3,E3,-
    MUSIC G4,E4,S,-,S,S,-
    MUSIC S,S,G2,-,-,-,-
    MUSIC F4#,D4#,S,-,-,-,-
    MUSIC G4,E4,E3,-,C4,G3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC C3,-,-,-,-,-,-
    MUSIC S,C5,-,-,-,-,-
    MUSIC E5,G4,C4,-,E3,G3,-
    MUSIC C5,-,S,-,S,S,-
    MUSIC D5,G2,-,-,-,-,-
    MUSIC E5,S,-,-,-,-,-
    MUSIC D5,C4,G3,-,E3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC A4,F3,F2,-,-,-,-
    MUSIC G4#,S,S,-,-,-,-
    MUSIC A4,D3,D2,-,-,-,-
    MUSIC G5,S,S,-,A4,-,-
    MUSIC S,E3,E2,-,S,-,-
    MUSIC F5,S,S,-,A4,-,-
    MUSIC S,F3,F2,-,S,-,-
    MUSIC C5,S,S,-,A4,-,-

    MUSIC E5,G4,G3,-,G2,-,-
    MUSIC D5#,-,S,-,S,-,-
    MUSIC E5,G3,E4,-,C4,-,-
    MUSIC A5,S,S,-,S,-,-
    MUSIC S,D4#,C4,-,F3#,-,-
    MUSIC C6,S,S,-,S,-,-
    MUSIC G5,E4,C4,-,G3,-,-
    MUSIC E5,S,S,-,S,-,-

    MUSIC C5,F4#,A2,-,A3,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC C5,F4#,D3,-,D2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC E5,F4,B4,-,G3,G2,-
    MUSIC D5,B4,F4,-,S,S,-
    MUSIC S,S,S,-,B3,B2,-
    MUSIC C5,G4,E4,-,S,S,-

    MUSIC S,S,S,-,C4,C3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC E4,G4,-,-,-,-,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-

    MUSIC F4,D4,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4#,F3,-,A3,-,-
    MUSIC D4,F4,S,-,S,-,-
    MUSIC S,S,A2,-,-,-,-
    MUSIC E4,C4#,S,-,-,-,-
    MUSIC F4,D4,A3,-,F3,-,-
    MUSIC S,S,S,-,S,-,-

    MUSIC F2,-,-,-,-,-,-
    MUSIC S,A4,-,-,-,-,-
    MUSIC D5,F4,A3,-,F3,-,-
    MUSIC A4,-,S,-,S,-,-
    MUSIC C5,A2,-,-,-,-,-
    MUSIC D5,S,-,-,-,-,-
    MUSIC C5,A3,F3,-,-,-,-
    MUSIC A4,S,S,-,-,-,-

    MUSIC G4,E4,C3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC F4#,D4#,C4,-,G3,E3,-
    MUSIC G4,E4,S,-,S,S,-
    MUSIC S,S,G2,-,-,-,-
    MUSIC F4#,D4#,S,-,-,-,-
    MUSIC G4,E4,E3,-,C4,G3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC C3,-,-,-,-,-,-
    MUSIC S,C5,-,-,-,-,-
    MUSIC E5,G4,C4,-,G3,E3,-
    MUSIC C5,-,S,-,S,S,-
    MUSIC D5,G2,-,-,-,-,-
    MUSIC E5,S,-,-,-,-,-
    MUSIC D5,C4,G3,-,E3,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC D5,B4,G2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC A4#,C5#,B3,-,G3,F3,-
    MUSIC D5,B4,S,-,S,S,-
    MUSIC S,S,B2,-,-,-,-
    MUSIC C5#,A4#,S,-,-,-,-
    MUSIC D5,B4,B3,-,G3,F3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC G2,-,-,-,-,-,-
    MUSIC S,F5,-,-,-,-,-
    MUSIC A5,B4,F3,-,B3,G3,-
    MUSIC F5,-,S,-,S,S,-
    MUSIC G5,D3,-,-,-,-,-
    MUSIC A5,S,-,-,-,-,-
    MUSIC G5,B3,G3,-,F3,-,-
    MUSIC F5,S,S,-,S,-,-

    MUSIC C5,C6,C4,-,F3#,D3#,-
    MUSIC C6,C5,S,-,S,S,-
    MUSIC C6,C5,D3#,-,F3#,C4,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,S,S,-
    MUSIC A5,C5,C4,-,F3#,D3#,-
    MUSIC S,S,S,-,S,S,-

    MUSIC G5,C5,C4,-,G3,E3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC E4,G4,-,-,-,-,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-
    MUSIC G4,E4,-,-,-,-,-
    MUSIC S,S,-,-,-,-,-

    MUSIC F4,D4,F2,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC E4,C4#,F3,-,A3,-,-
    MUSIC F4,D4,S,-,S,-,-
    MUSIC S,S,A2,-,-,-,-
    MUSIC E4,C4#,S,-,-,-,-
    MUSIC F4,D4,A3,-,F3,-,-
    MUSIC S,S,S,-,S,-,-

    MUSIC F2,-,-,-,-,-,-
    MUSIC S,A4,-,-,-,-,-
    MUSIC D5,F4,A3,-,F3,-,-
    MUSIC A4,-,S,-,S,-,-
    MUSIC C5,A2,-,-,-,-,-
    MUSIC D5,S,-,-,-,-,-
    MUSIC C5,A3,F3,-,-,-,-
    MUSIC A4,S,S,-,-,-,-

    MUSIC G4,E4,C3,-,-,-,-
    MUSIC S,S,S,-,-,-,-
    MUSIC F4#,D4#,C4,-,G3,E3,-
    MUSIC G4,E4,S,-,S,S,-
    MUSIC S,S,G2,-,-,-,-
    MUSIC D4#,F4#,S,-,-,-,-
    MUSIC G4,E4,C4,-,G3,E3,-
    MUSIC S,S,S,-,S,S,-

    MUSIC C3,-,-,-,-,-,-
    MUSIC S,C5,-,-,-,-,-
    MUSIC E5,G4,C4,-,E3,G3,-
    MUSIC C5,-,S,-,S,S,-
    MUSIC D5,G2,-,-,-,-,-
    MUSIC E5,S,-,-,-,-,-
    MUSIC D5,G3,E3,-,C4,-,-
    MUSIC C5,S,S,-,S,-,-

    MUSIC A4,F3,F2,-,-,-,-
    MUSIC G4#,S,S,-,-,-,-
    MUSIC A4,D3,D2,-,-,-,-
    MUSIC G5,S,S,-,A4,-,-
    MUSIC S,E3,E2,-,S,-,-
    MUSIC F5,S,S,-,A4,-,-
    MUSIC S,F3,F2,-,S,-,-
    MUSIC C5,S,S,-,A4,-,-

    MUSIC E5,G4,G3,-,G2,-,-
    MUSIC D5#,-,S,-,S,-,-
    MUSIC E5,E4,C4,-,G3,-,-
    MUSIC A5,S,S,-,S,-,-
    MUSIC S,F3#,C4,-,D4#,-,-
    MUSIC C6,S,S,-,S,-,-
    MUSIC G5,E4,C4,-,G3,-,-
    MUSIC E5,S,S,-,S,-,-

    MUSIC C5,F4#,A3,-,A2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC C5,F4#,D3,-,D2,-,-
    MUSIC S,S,S,-,S,-,-
    MUSIC E5,B4,F4,-,G2,G3,-
    MUSIC D5,F4,B4,-,S,S,-
    MUSIC S,S,S,-,B3,B2,-
    MUSIC C5,G4,E4,-,S,S,-

    MUSIC S,S,S,-,C4,C3,-
    MUSIC S,S,S,-,S,S,-
    MUSIC S,S,S,-,G3,G2,-
    MUSIC S,S,S,-,S,S,-
    MUSIC C6,G5,E5,-,C5,C3,C2
    MUSIC S,S,S,-,S,S,S
    MUSIC -,-,-,-,-,-,-

    MUSIC STOP
