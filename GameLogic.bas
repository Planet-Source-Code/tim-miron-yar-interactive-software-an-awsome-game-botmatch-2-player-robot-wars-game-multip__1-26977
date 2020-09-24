Attribute VB_Name = "GameLogic"
'Copyright 2001 yar interactive

'the following are the main
'variables, constants and types
'used in the game, a brief description
'of each variable is provided beside or
'above the variable

Public MsgStat As Byte 'message we're sending...
Public MsgFlag As Boolean 'yes/no, ok/cancel - MsgBox

Public MPGameType As Boolean 'is it two players?
Public HighSkill As Boolean 'is it high skill game level?
Public HGraphics As Boolean 'are they high graphics or low graphics?

'is game started, is game not paused
Public GameOn As Boolean

Public P1Color As Long
Public P2Color As Long

'number of weapons
Private Const NumOW As Byte = 3

'Speed of bullets
Public PBulletSpeed(0 To NumOW) As Integer

'Default Speed & Delay of Bullets
Public DBulletSpeed(0 To NumOW) As Integer
Public DBDelay(0 To NumOW) As Integer

'high graphics text effects
Public HGTE As Boolean

'make sure health stays on until toggled
Public CanToggleHelp As Boolean
Public CanToggleScore As Boolean
    
'is main grid showing
Public GridOn As Boolean

'is player we firing
Public P1Firing As Boolean 'Byte
Public P2Firing As Boolean 'Byte

Public P1HitsC As Long
Public P2HitsC As Long
Public P1ShotsC As Long
Public P2ShotsC As Long

'IMPACT frame* (not totally working YET)
'Public I1frame As Byte
'Public I2frame As Byte

'sleep (game speed)
Public Sleeper As Byte

'match frag limit
Public FragLimit As Integer

'toggle show current frags
Public ShowScores As Boolean 'Byte '0 = false, 1 = true

'toggle help screen
Public ShowHELP As Boolean 'Byte '0 = f, 1 = t

'Health Gain interval
Public HGInt As Integer

'did we hit them last shot?
Public HitP2 As Boolean
Public HitP1 As Boolean

'Current Health-Gain Interval
Public CHGInt As Integer
Public DoHG As Boolean 'should we perform health gain?

'bot widths/hieghts...
Private Const P1BotW = 32
Private Const P1BotH = 32
Private Const P2BotW = 32
Private Const P2BotH = 32

'player accuracy bonus/player victory bonus...
Private Const PACBonus As Integer = 500
Private Const PAVBonus As Integer = 1000

'credits per hit point
Private Const PHitValue As Byte = 15

'bullets made?
Public P1NewBulletMade As Boolean
Public P2NewBulletMade As Boolean

'display name coords
Public Const DispP1NameX As Integer = 494
Public Const DispP1NameY As Integer = 355
    Public Const DispP2NameX As Integer = 52
    Public Const DispP2NameY As Integer = 11
    
    'display weapons coords
    Public Const DispP1WeapX As Integer = 455
    Public Const DispP1WeapY As Integer = 386
        Public Const DispP2WeapX As Integer = 13
        Public Const DispP2WeapY As Integer = 42
        
    'DISPLAY health status Coords.
     Public Const DispP1HealthX As Integer = 530
     Public Const DispP1HealthY As Integer = 386
        Public Const DispP2HealthX As Integer = 90
        Public Const DispP2HealthY As Integer = 41
        
    'DISPLAY current score coords
    Public Const DispCSTitleX As Integer = 229
    Public Const DispCSTitleY As Integer = 155
        Public Const DispP1ScoreX As Integer = 236
        Public Const DispP1ScoreY As Integer = 175
            Public Const DispP2ScoreX As Integer = 236
            Public Const DispP2ScoreY As Integer = 205
        Public Const DispCSpanX As Integer = 228
        Public Const DispCSpanY As Integer = 240
        
        '####################################
        '############### RETRO ##############
        'retro rockets - show them or not?
        Public ShowRetro As Boolean
        
        'retro rocket glares - sprite coords
        Public Const RrUpX As Byte = 0
        Public Const RrDownX As Byte = 64
            Public Const RrLeftX As Byte = 128
                Public Const RrRightX As Byte = 192
            Public Const RrUpLeftX As Integer = 256
            Public Const RrUpRightX As Integer = 320
        Public Const RrDownLeftX As Integer = 384
        Public Const RrDownRightX As Integer = 448
            
        '####################################
        '####################################
        
        'fade out health???
        Public FadeScores As Boolean
        Public ShowGo As Boolean 'Show "Go!" or not??
        Public ShowFPS As Boolean 'show frames per second...
        Public ShowHUD As Boolean 'enable/show head-up-display
        
    'Display HELP
     Public Const DispHelpX As Integer = 149
     Public Const DispHelpY As Integer = 76
        
        Public P1D5 As Byte 'delay frame for P1 shields...
        Public P2D5 As Byte 'delay frame for P2 shields...
        Public SFDelay As Byte 'delay between frames of shield motion...
        
        Public P1ShieldCD As Integer 'player 1 shield points (countdown)
        Public P2ShieldCD As Integer 'player 2 shield points (countdown)
        
'number of bullets per player
Public Const NumOfBullets As Byte = 12
Public BDelay(0 To 3) As Byte         '8


    Public P1Name As String    'Player Name
    Public P1Health As Integer 'Current Player Heath
    Public P1MaxHealth As Integer 'Player 1 health potential
    Public P1S As Byte         'Player Sheild-Type (if any)
    Public P1SL As Integer     'Shield Level
    Public P1Speed As Byte     'player speed
    Public P1X As Integer      'X Coord.
    Public P1Y As Integer      'Y Coord.
    Public P1CurWeapon As Byte    'Current Weapon
    Public P1CurFrags As Integer  'Current Frags (match)
    Public P1Money As Currency    'how much dow are they pakin?

    Public P2Name As String    'Player Name
    Public P2Health As Integer 'Player Heath
    Public P2MaxHealth As Integer 'Player2 health potential
    Public P2S As Byte         'Player Sheild-Type (if any)
    Public P2SL As Integer     'Shield Level
    Public P2Speed As Byte     'player speed
    Public P2X As Integer      'X Coord.
    Public P2Y As Integer      'Y Coord.
    Public P2CurWeapon As Byte  'current weapon
    Public P2CurFrags As Integer  'Current Frags (match)
    Public P2Money As Currency    'how much dow is P2 pakin?
    
Public Type ProjectileP1
    X As Integer       'X coord
    Y As Integer       'Y coord
    Active As Boolean   'Visble
    FiredFrom As Byte  'what gun fired it?
End Type

Public Type ProjectileP2
    X As Integer       'X coord
    Y As Integer      'Y coord
    Active As Boolean    'Visible
    FiredFrom As Byte 'what gun fired this projectile?
End Type


'Player's display...
 Public P1WeapPic As PictureBox  'p1 weapon
 Public P2WeapPic As PictureBox 'p2 weapon
   
   'weapon properties
Public Type RWeapon
    WeaponPic As PictureBox   'display picture
    WeapDamage As Integer     'weapons damage
    WeapBSprite As Object     'bullet sprite
    WeapXShift As Integer       'X shift-over for firing
    WeapXshiftB As Integer      'X shift (distance from left to center)
    WeapYShiftB As Integer      'Y Shift (length of bullet)
    P1WeapYShift As Integer     'Y shift-over for firing
    P2WeapYShift As Integer     'Y shift-over for firing
    WeapBSparK As PictureBox        'impact spark
    
    WeaponName As String      'name of weapon
End Type

Public P1Bullet(0 To NumOfBullets) As ProjectileP1 'P1 bullets
Public P2Bullet(0 To NumOfBullets) As ProjectileP2 'P2 bullets
Public PWeapon(0 To NumOW) As RWeapon 'player weapon

Public CanTogP1W As Boolean 'P1 weapon toggle optional
Public CanTogP2W As Boolean 'P2 weapon toggle optional

Public P1ShieldRunning As Boolean
Public P2ShieldRunning As Boolean
Public P1ShieldFrame As Byte
Public P2ShieldFrame As Byte


'set health points
''''''''''''''''''
Public Sub SetP1Health()
        'player 1 health
With frmRes.picP1Health
     .Refresh
     .Cls
    End With
      frmRes.picP1Health.Print P1Health
End Sub
Public Sub SetP2Health()
        'player 2 health
With frmRes.picP2Health
     .Refresh
     .Cls
    End With
      frmRes.picP2Health.Print P2Health
End Sub

Public Sub SetFragScores()
'draw frag-scores (resources)
    With frmRes.picP1Frags
     .Refresh
     .Cls
    End With
      frmRes.picP1Frags.Print P1Name & " - " & P1CurFrags

    With frmRes.picP2Frags
     .Refresh
     .Cls
    End With
      frmRes.picP2Frags.Print P2Name & " - " & P2CurFrags
End Sub

Public Sub ShowMsg(MCaption As String, MTitle As String)
frmMsg.ResMsgButs
Select Case MsgStat
 
 Case 1
 frmMsg.lblCanBut.Visible = True
 frmMsg.Shape2.Visible = True
 
 Case 0
 frmMsg.lblCanBut.Visible = False
 frmMsg.Shape2.Visible = False
 
 Case 3
 frmMsg.lblCanBut.Visible = True
 frmMsg.Shape2.Visible = True
 MsgFlag = False
End Select

With frmMsg
  .lblCaption.Caption = MCaption
  .lblTitle.Caption = MTitle
  .Show 1
End With
End Sub

Public Function DidHitP1(X As Integer, Y As Integer, PW As Byte) As Boolean
If X + PWeapon(PW).WeapXshiftB >= P1X And _
X + PWeapon(PW).WeapXshiftB <= (P1X + P1BotW) And _
Y + PWeapon(PW).WeapYShiftB >= P1Y And Y <= (P1Y + P1BotH) Then DidHitP1 = True
End Function

Public Function DidHitP2(X As Integer, Y As Integer, PW As Byte) As Boolean
'If X + PWeapon(PW).WeapXshiftB >= (P2X + PWeapon(PW).WeapXshiftB) And _
X + PWeapon(PW).WeapXshiftB <= (P2X + P2BotW) And _
Y <= (P2Y + P2BotH) And Y >= (P2Y) Then DidHitP2 = True

If X + PWeapon(PW).WeapXshiftB >= P2X And _
X + PWeapon(PW).WeapXshiftB <= (P2X + P2BotW) And _
Y <= (P2Y + P2BotH) And Y >= (P2Y) Then DidHitP2 = True
End Function

Public Sub PGotFrag(Player As Byte)
Select Case Player

Case 1
   P1CurFrags = P1CurFrags + 1
   
   'reprint scores
   SetFragScores
   ShowHELP = False
   ShowScores = True
   

'if its at the frag limit someone won
'otherwise reset the health...
   If P1CurFrags = FragLimit Then
     Call MatchWon(1)
         Else
      P2Health = P2MaxHealth
      P1Health = P1MaxHealth
   End If

Case 2
P2CurFrags = P2CurFrags + 1

'reprint scores
SetFragScores
ShowHELP = False
ShowScores = True

'if its at the frag limit someone won
'otherwise reset the health...
If P2CurFrags = FragLimit Then
  Call MatchWon(2)
        Else
      P2Health = P2MaxHealth
      P1Health = P1MaxHealth
   End If
   
End Select
End Sub

Public Sub MatchWon(Winner As Byte)
'someone won the match...
MsgStat = 0
Select Case Winner
 Case 1
  ShowMsg P1Name & " Won the Match!", "BotMatch - WINNER"
 Case 2
  ShowMsg P2Name & " Won the Match!", "BotMatch - WINNER"
End Select
 Show2PlayerMO Winner
End Sub

Public Sub PauseGame()
MsgStat = 0
 ShowMsg "GAME PAUSED" & Chr(13) & "Press OK to Resume", "BotMatch - PAUSED"
End Sub

Public Sub Show2PlayerMO(PWin As Byte)
On Error Resume Next

'sub displays the 2 player match-over screen
'and it also sets up all the stats and
'calculates the money and everything else
'needed to be done

Dim P1Ac As Single 'player 1 accuracy
Dim P2Ac As Single 'player 2 accuracy
    Dim P1AcB As Integer 'p1 Accuracy Bonus
    Dim P2AcB As Integer 'p2 Accuracy Bonus
    
    Dim p1VicB As Integer 'p1 victory bonus...
    Dim p2VicB As Integer 'p2 victory bonus...

Dim P1EHC As Long 'P1 earned hit credits
Dim P2EHC As Long 'P1 earned hit credits

Dim P1TEC As Long 'total earned credits this match...
Dim P2TEC As Long 'total earned credits this match...

P1Ac = (P1HitsC / P1ShotsC)
P2Ac = (P2HitsC / P2ShotsC)

If P2Ac <= 0 Then P2Ac = 0
If P1Ac <= 0 Then P1Ac = 0

'if they scored an accuracy
'over 50% give them the bonus
If P1Ac >= 0.5 Then P1AcB = PACBonus
If P2Ac >= 0.5 Then P2AcB = PACBonus


'show match over screen...
 Load frm2PMatchOver
 GameOn = False

Select Case PWin
 Case 1
    p1VicB = PAVBonus
 Case 2
    p2VicB = PAVBonus
End Select

P1EHC = (P2MaxHealth - P2Health) * PHitValue
P2EHC = (P1MaxHealth - P1Health) * PHitValue

 'earned credits this match alone
P1TEC = P1AcB + p1VicB + P1EHC + 215
P2TEC = P2AcB + p2VicB + P2EHC + 215

 'total credits...
P1Money = P1Money + P1TEC
P2Money = P2Money + P2TEC

  With frm2PMatchOver
  
  .lblP1Name = P1Name
  .lblP2Name = P2Name
  .lblP1Name.ForeColor = P1Color
  .lblP2Name.ForeColor = P2Color
  
  .Shape3.BorderColor = P1Color
  .Shape4.BorderColor = P2Color
  
    .lblAccuracy(0).Caption = Format(P1Ac, "Percent")
    .lblAccuracy(1).Caption = Format(P2Ac, "Percent")
        .lblShots(0).Caption = P1ShotsC
        .lblShots(1).Caption = P2ShotsC
        
        .lblHits(0).Caption = P1HitsC
        .lblHits(1).Caption = P2HitsC
        
        'damage sustained
        .lblDS(0).Caption = P1MaxHealth - P1Health
        .lblDS(1).Caption = P2MaxHealth - P2Health
        
        'damage infliction credits
        .lblDI(0).Caption = Format(P1EHC, "Standard")
        .lblDI(1).Caption = Format(P2EHC, "Standard")
        
        'accuracy bonus
        .lblAB(0).Caption = Format(P1AcB, "Standard")
        .lblAB(1).Caption = Format(P2AcB, "Standard")
        
        'victory bonus
        .lblVB(0).Caption = Format(p1VicB, "Standard")
        .lblVB(1).Caption = Format(p2VicB, "Standard")
        
        .lblCE(0).Caption = Format(P1TEC, "Standard")
        .lblCE(1).Caption = Format(P2TEC, "Standard")
        
        .lblP1Balance = Format(P1Money, "Standard")
        .lblP2Balance = Format(P2Money, "Standard")
  .Show
 End With
 frmMain.Hide
 
 'RESET HEALTH...
      P2Health = P2MaxHealth
      P1Health = P1MaxHealth
 Unload frmMain
End Sub

Public Sub ToggleP1Weap()
'changes variables that indicate
'to the game what weapon P1 is using
Select Case P1CurWeapon
 Case 0
 P1CurWeapon = 1
 Case 1
 P1CurWeapon = 2
 Case 2
 P1CurWeapon = 0
End Select

CanTogP1W = False
End Sub

Public Sub ToggleP2Weap()
'changes variables that indicate
'to the game what weapon P1 is using
Select Case P2CurWeapon
 Case 0
 P2CurWeapon = 1
 Case 1
 P2CurWeapon = 2
 Case 2
 P2CurWeapon = 0
End Select

CanTogP2W = False
End Sub


Public Sub ResetDefs()
'reset most of the game variables before a new match...
       P1CurFrags = 0
       P2CurFrags = 0
       P1HitsC = 0
       P1ShotsC = 0
       P2HitsC = 0
       P2ShotsC = 0
        MPGameType = False
        HighSkill = False
        HGraphics = False
        For c = 0 To NumOfBullets
            P1Bullet(c).Active = False
            P2Bullet(c).Active = False
        Next c
        ShowScores = False
        ShowHELP = False
End Sub
