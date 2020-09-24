VERSION 5.00
Begin VB.Form frmMain 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Battle Droids 1.0"
   ClientHeight    =   6600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   9000
   ControlBox      =   0   'False
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MouseIcon       =   "frmMain.frx":030A
   MousePointer    =   99  'Custom
   ScaleHeight     =   440
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   600
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrStuff 
      Interval        =   1000
      Left            =   420
      Top             =   2970
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
''Module Created on June 8th, 2001
''Modified last on
''
'Copyright 2001 - yar interactive

''Created By Tim Miron - yar interactive
''
''Purpose: BotMatch Game, main procedures and
''main game loops

    
    Public P2Sprite As PictureBox    'main sprite
    Public P2Panel As PictureBox     'panel sprite
    Public P2DShield As PictureBox   'Dark Shield
    Public P2LShield As PictureBox   'Light Shield
    Public P2HShield As PictureBox   'Half-Shield
    Public P2Mask As PictureBox      'sprite mask

    
    Public P1Sprite As PictureBox    'main sprite
    Public P1Panel As PictureBox     'panel sprite
    Public P1DShield As PictureBox   'Dark Shield
    Public P1LShield As PictureBox   'Light Shield
    Public P1HShield As PictureBox   'Half-Shield
    Public P1Mask As PictureBox      'sprite mask

    Private HFIndex As Byte         'health show index (fade health)
    Private FadeColor As Long       'current color of Fade Text...
    Private CurFPS As Integer       'current frames per second (counter)
    Private YelFadeCol As Long
    Private YelFadeUp As Boolean
    
Public Sub DoGame()
'### MAIN GAME LOOP
Dim L1 As Boolean, L2 As Boolean    'left buttons
Dim R1 As Boolean, R2 As Boolean    'rigth buttons
Dim U1 As Boolean, U2 As Boolean    'up button
Dim D1 As Boolean, D2 As Boolean    'down button
Dim b As Integer
Dim c As Integer
Dim ScoreKey As Boolean, HelpKey As Boolean, ExitKey As Boolean, _
DPauseKey As Boolean, P1TogKey As Boolean, P2TogKey As Boolean, _
P1ShieldKey As Boolean, P2ShieldKey As Boolean, _
CanTogP1Shield As Boolean, CanTogP2Shield As Boolean

'Dim CanToggleP1W As Boolean, CanToggleP2W As Boolean

Dim B1Frame As Integer     'bullet delay frame
Dim B2Frame As Integer     'bullet delay frame

'for GO! and other faders...
'main game loop
Do


    'DoEvents is a built-in VB function
    'that temporarly exits a loop to
    'handle the mouse and keyboard so the
    'computer does now freeze up.
    ScoreKey = IsKeyDown(115) 'is F4 down?
    HelpKey = IsKeyDown(112) 'is F1 down?
    ExitKey = IsKeyDown(27) 'is Esc down?
    P1TogKey = IsKeyDown(35) 'p1 toggle key
    P2TogKey = IsKeyDown(9)  'p2 toggle key

    
  DoEvents
    
    If ExitKey = True Then
              MsgStat = 1
       ShowMsg "You are about to exit the current match.", "BotMatch - Exit Game?"
    End If
  
  
  'if they hit F4, show current scores...
If ScoreKey = True And CanToggleScore = False And ShowScores = False Then 'they pressed it
   CanToggleScore = True
   ShowScores = True
   ShowHELP = False
   ElseIf ScoreKey = False And CanToggleScore = True And ShowScores = True Then
   CanToggleScore = False
   ElseIf ScoreKey = True And CanToggleScore = False And ShowScores = True Then
   ShowScores = False
   CanToggleScore = True
   ElseIf ScoreKey = False And CanToggleScore = True And ShowScores = False Then
    CanToggleScore = False
End If
    
    'if they hit F1, show help...
    
   If HelpKey = True And CanToggleHelp = False And ShowHELP = False Then 'they pressed it
   CanToggleHelp = True
   ShowHELP = True
   ShowScores = False
   ElseIf HelpKey = False And CanToggleHelp = True And ShowHELP = True Then
   CanToggleHelp = False
   ElseIf HelpKey = True And CanToggleHelp = False And ShowHELP = True Then
   ShowHELP = False
   CanToggleHelp = True
   ElseIf HelpKey = False And CanToggleHelp = True And ShowHELP = False Then
    CanToggleHelp = False
   End If
   'elseif helpkey = True and CanToggleHelp = True and showhelp =
   
    'Following detects the key-state
    '(allows for multiple key-down detection)

  'note - fire1 is facing upwards
  'fire2 is facing down...
    P1ShieldKey = IsKeyDown(45)
    P2ShieldKey = IsKeyDown(90)
    
  P1Firing = IsKeyDown(17) 'fire one down? (Ctrl)
  P2Firing = IsKeyDown(16) 'fire two down? (Shift)
  
  DPauseKey = IsKeyDown(80)
  'done key detection

    L1 = IsKeyDown(37) 'left one down?
    R1 = IsKeyDown(39) 'rigth one down?
    U1 = IsKeyDown(38) 'up key down?
    D1 = IsKeyDown(40) 'down key down?

  L2 = IsKeyDown(65) 'left two down?
  R2 = IsKeyDown(68) 'right two down?
  U2 = IsKeyDown(87)
  D2 = IsKeyDown(83)
    
    'shield activation for player 1
If P1ShieldKey = True And CanTogP1Shield = False And P1ShieldRunning = False Then 'they pressed it
   CanTogP1Shield = True
   P1ShieldRunning = True
   ElseIf P1ShieldKey = False And CanTogP1Shield = True And P1ShieldRunning = True Then
   CanTogP1Shield = False
   ElseIf P1ShieldKey = True And CanTogP1Shield = False And P1ShieldRunning = True Then
   P1ShieldRunning = False
   CanTogP1Shield = True
   ElseIf P1ShieldKey = False And CanTogP1Shield = True And P1ShieldRunning = False Then
    CanTogP1Shield = False
End If

    'shield activation for player 2
If P2ShieldKey = True And CanTogP2Shield = False And P2ShieldRunning = False Then 'they pressed it
   CanTogP2Shield = True
   P2ShieldRunning = True
   ElseIf P2ShieldKey = False And CanTogP2Shield = True And P2ShieldRunning = True Then
   CanTogP2Shield = False
   ElseIf P2ShieldKey = True And CanTogP2Shield = False And P2ShieldRunning = True Then
   P2ShieldRunning = False
   CanTogP2Shield = True
   ElseIf P2ShieldKey = False And CanTogP2Shield = True And P2ShieldRunning = False Then
    CanTogP2Shield = False
End If

  'assume new player positions
If L1 = True And P1X > 4 Then P1X = P1X - P1Speed
If R1 = True And P1X < 566 Then P1X = P1X + P1Speed
If U2 = True And P2Y > 8 Then P2Y = P2Y - P2Speed
If D2 = True And P2Y < 172 Then P2Y = P2Y + P2Speed
    If L2 = True And P2X > 4 Then P2X = P2X - P2Speed
    If R2 = True And P2X < 566 Then P2X = P2X + P2Speed
        If D1 = True And P1Y < 400 Then P1Y = P1Y + P1Speed
        If U1 = True And P1Y > 236 Then P1Y = P1Y - P1Speed

If DPauseKey = True Then PauseGame

If P1TogKey = True And P1Firing = True And B1Frame = 0 _
Or CanTogP1W = True And P1TogKey = True Then
 ToggleP1Weap
End If

If P2TogKey = True And P2Firing = True And B2Frame = 0 _
Or CanTogP2W = True And P2TogKey = True Then
 ToggleP2Weap
End If

If GameOn = False Then Exit Sub
frmMain.Cls

    'P1 mask & object
    
'only draw mask if theres a grid...
       If GridOn = True Then BitBlt frmMain.hDC, P1X, P1Y, P1Mask.ScaleWidth, _
    P1Mask.ScaleHeight, P1Mask.hDC, 0, 0, vbSrcAnd

 BitBlt frmMain.hDC, P1X, P1Y, P1Sprite.ScaleWidth, _
 P1Sprite.ScaleHeight, P1Sprite.hDC, 0, 0, vbSrcPaint
 
 'P2 mask & object
             
If GridOn = True Then BitBlt frmMain.hDC, P2X, P2Y, P2Mask.ScaleWidth, _
            P2Mask.ScaleHeight, P2Mask.hDC, 0, 0, vbSrcAnd
            
 BitBlt frmMain.hDC, P2X, P2Y, P2Sprite.ScaleWidth, _
 P2Sprite.ScaleHeight, P2Sprite.hDC, 0, 0, vbSrcPaint
 
'######## RETRO START ################
    'draw retro burners if required...
    If ShowRetro = True Then
        
        'P1 lefts
        If L1 = True And U1 = True Then 'up-left
            BitBlt frmMain.hDC, P1X - 16, P1Y - 16, _
            64, 64, frmRes.picRetroF1.hDC, RrUpLeftX, 0, vbSrcPaint
        ElseIf L1 = True And D1 = True Then 'down left
            BitBlt frmMain.hDC, P1X - 16, P1Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrDownLeftX, 0, vbSrcPaint
        ElseIf L1 = True Then 'just left
            BitBlt frmMain.hDC, P1X - 16, P1Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrLeftX, 0, vbSrcPaint
        'P1 rights
        ElseIf R1 = True And U1 = True Then 'up-right
            BitBlt frmMain.hDC, P1X - 16, P1Y - 16, _
            64, 64, frmRes.picRetroF1.hDC, RrUpRightX, 0, vbSrcPaint
        ElseIf R1 = True And D1 = True Then 'down right
            BitBlt frmMain.hDC, P1X - 16, P1Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrDownRightX, 0, vbSrcPaint
        ElseIf R1 = True Then 'just right
            BitBlt frmMain.hDC, P1X - 16, P1Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrRightX, 0, vbSrcPaint
        'P1 Up and Down
        ElseIf U1 = True Then 'just up
            BitBlt frmMain.hDC, P1X - 16, P1Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrUpX, 0, vbSrcPaint
        ElseIf D1 = True Then 'just up
            BitBlt frmMain.hDC, P1X - 16, P1Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrDownX, 0, vbSrcPaint
        End If
        
        
        '##### PLAYER 2 ##########
        
        'P2 Rights
        If R2 = True And U2 = True Then 'up-right
            BitBlt frmMain.hDC, P2X - 16, P2Y - 16, _
            64, 64, frmRes.picRetroF1.hDC, RrUpRightX, 0, vbSrcPaint
        ElseIf R2 = True And D2 = True Then 'down right
            BitBlt frmMain.hDC, P2X - 16, P2Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrDownRightX, 0, vbSrcPaint
        ElseIf R2 = True Then 'just right
            BitBlt frmMain.hDC, P2X - 16, P2Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrRightX, 0, vbSrcPaint
         'P2 lefts...
        ElseIf L2 = True And U2 = True Then 'up-left
            BitBlt frmMain.hDC, P2X - 16, P2Y - 16, _
            64, 64, frmRes.picRetroF1.hDC, RrUpLeftX, 0, vbSrcPaint
        ElseIf L2 = True And D2 = True Then 'down left
            BitBlt frmMain.hDC, P2X - 16, P2Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrDownLeftX, 0, vbSrcPaint
        ElseIf L2 = True Then 'just left
            BitBlt frmMain.hDC, P2X - 16, P2Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrLeftX, 0, vbSrcPaint
        'Player 2 Up and Down
        ElseIf U2 = True Then 'just up
            BitBlt frmMain.hDC, P2X - 16, P2Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrUpX, 0, vbSrcPaint
        ElseIf D2 = True Then 'just up
            BitBlt frmMain.hDC, P2X - 16, P2Y - 16, 64, 64, _
            frmRes.picRetroF1.hDC, RrDownX, 0, vbSrcPaint
        End If
    End If
''######### END RETRO BOSTERS ########
 
 
 '###################################'
''''''''''''''' BULLETS '''''''''''''
For c = 0 To NumOfBullets

'PLAYER 1
  If P1Firing = True And P1Bullet(c).Active = False And _
  P1NewBulletMade = False And B1Frame = 0 Then
'we found our next bullet, and we havent made our new bullet yet
  
  P1Bullet(c).Active = True 'activate new bullet
  
  P1Bullet(c).X = P1X + PWeapon(P1CurWeapon).WeapXShift 'set X to middle of player
  P1Bullet(c).Y = P1Y + PWeapon(P1CurWeapon).P1WeapYShift 'set y right on top of player
  
  'what gun was it fired from in case they switch
  P1Bullet(c).FiredFrom = P1CurWeapon
  
  '''''##########
  P1NewBulletMade = True 'we have now made the bullet, dont make anymore
  
  P1ShotsC = P1ShotsC + 1 'shots counter

End If

If P1Bullet(c).Active = True Then
 If P1Bullet(c).Y < -32 Then
  P1Bullet(c).Active = False
 ElseIf DidHitP2(P1Bullet(c).X, P1Bullet(c).Y, P1Bullet(c).FiredFrom) = True Then '##### HIT
    HitP2 = True
    P1HitsC = P1HitsC + 1
 End If
        
   P1Bullet(c).Y = P1Bullet(c).Y - PBulletSpeed(P1Bullet(c).FiredFrom) 'move any active bullets ahead
        
    Select Case HitP2
        Case 0
        
        'replaced p1BSprite with personal bullet sprite...
        
        'PWeapon(P1Bullet(C).FiredFrom).WeapBSprite(0)
        
         BitBlt frmMain.hDC, P1Bullet(c).X, P1Bullet(c).Y, _
        PWeapon(P1Bullet(c).FiredFrom).WeapBSprite(0).ScaleWidth, _
        PWeapon(P1Bullet(c).FiredFrom).WeapBSprite(0).ScaleHeight, _
        PWeapon(P1Bullet(c).FiredFrom).WeapBSprite(0).hDC, _
        0, 0, vbSrcPaint
        
         Case 1
         
         'replaced p1bspark with
          'PWeapon(p1bullet(C).FiredFrom).WeapBSparK
       BitBlt frmMain.hDC, P1Bullet(c).X - 10, _
       P1Bullet(c).Y - 8, _
        PWeapon(P1Bullet(c).FiredFrom).WeapBSparK.ScaleWidth, _
        PWeapon(P1Bullet(c).FiredFrom).WeapBSparK.ScaleHeight, _
        PWeapon(P1Bullet(c).FiredFrom).WeapBSparK.hDC, _
        0, 0, vbSrcPaint
   
            HitP2 = False
            P1Bullet(c).Active = False
            
            'if shields are on it only takes away 1 point...
            'no matter what gun it is from.
            If P2ShieldRunning = True And P2ShieldCD > 0 Then
            P2Health = P2Health - 1
                Else
                    P2Health = P2Health - PWeapon(P1Bullet(c).FiredFrom).WeapDamage
            End If
            
        If P2Health <= 0 Then
             P2Health = 0
             Call PGotFrag(1) 'got a frag...
        '''     P2Health = P2MaxHealth
        '''     P1Health = P1MaxHealth
           HFIndex = 0
           FadeScores = True
         End If
             SetP1Health
             SetP2Health
    End Select
End If

'PLAYER 2
  If P2Firing = True And P2Bullet(c).Active = False And _
  P2NewBulletMade = False And B2Frame = 0 Then
'we found our next bullet, and we havent made our new bullet yet
  
  P2Bullet(c).Active = True 'activate new bullet
  
  P2Bullet(c).X = P2X + PWeapon(P2CurWeapon).WeapXShift 'set X to middle of player
  P2Bullet(c).Y = P2Y + PWeapon(P2CurWeapon).P2WeapYShift 'set y right on top of player
  
  'whast this fired from?
  P2Bullet(c).FiredFrom = P2CurWeapon
  
  P2NewBulletMade = True 'we have now made the bullet, dont make anymore
    P2ShotsC = P2ShotsC + 1 'shots counter
  End If

If P2Bullet(c).Active = True Then
    If P2Bullet(c).Y > 472 Then
    P2Bullet(c).Active = False
    ElseIf DidHitP1(P2Bullet(c).X, P2Bullet(c).Y, P2Bullet(c).FiredFrom) = True Then '##### HIT
     HitP1 = True
     P2HitsC = P2HitsC + 1
    End If
        
   P2Bullet(c).Y = P2Bullet(c).Y + PBulletSpeed(P2Bullet(c).FiredFrom) 'move any active bullets ahead
        
        'If P2Bullet(C).Y > 472 Then P2Bullet(C).Active = 0
        
     Select Case HitP1
     'replaced P2BSprite with
     'PWeapon(P2Bullet(C).FiredFrom).WeapBSprite(1)
     Case 0
          BitBlt frmMain.hDC, P2Bullet(c).X, P2Bullet(c).Y, _
        PWeapon(P2Bullet(c).FiredFrom).WeapBSprite(1).ScaleWidth, _
        PWeapon(P2Bullet(c).FiredFrom).WeapBSprite(1).ScaleHeight, _
        PWeapon(P2Bullet(c).FiredFrom).WeapBSprite(1).hDC, _
        0, 0, vbSrcPaint
        
               
     Case 1
          
          'replaced p2bspark with
          'PWeapon(p2bullet(C).FiredFrom).WeapBSparK
     BitBlt frmMain.hDC, P2Bullet(c).X - 10, _
     P2Bullet(c).Y + 8, _
        PWeapon(P2Bullet(c).FiredFrom).WeapBSparK.ScaleWidth, _
        PWeapon(P2Bullet(c).FiredFrom).WeapBSparK.ScaleHeight, _
        PWeapon(P2Bullet(c).FiredFrom).WeapBSparK.hDC, _
        0, 0, vbSrcPaint
        
        HitP1 = False
            P2Bullet(c).Active = False
            'show scores and stuff
            
            'if shields are on it only takes away 1 point...
            'no matter what gun it is from.
            If P1ShieldRunning = True And P1ShieldCD > 0 Then
            P1Health = P1Health - 1
                Else
            P1Health = P1Health - PWeapon(P2Bullet(c).FiredFrom).WeapDamage
            End If
            
        If P1Health <= 0 Then
             P1Health = 0
             Call PGotFrag(2)
       '      P1Health = P1MaxHealth
       '      P2Health = P2MaxHealth
         HFIndex = 0 'reset health counter for when health disapears
         FadeScores = True
         End If
             SetP1Health
             SetP2Health
    End Select
End If
Next c
P2NewBulletMade = False
P1NewBulletMade = False

                    '#########SHIELDS############
                        If P1ShieldRunning = True And P1ShieldCD > 0 Then
                        P1D5 = P1D5 + 1
                            If P1D5 = SFDelay Then P1D5 = 0
                        If P1D5 = 0 Then
                          P1ShieldFrame = P1ShieldFrame + 1
                          P1ShieldCD = P1ShieldCD - 1
                        End If
                                         If P1ShieldFrame = 4 Then P1ShieldFrame = 0
                            BitBlt frmMain.hDC, P1X - 17, P1Y - 17, 64, 64, _
                            frmRes.picShields.hDC, P1ShieldFrame * 64, 0, vbSrcPaint
                            
                            
                            'print sheild energy level into backbuffer
                            With frmRes.SCDbuffer1
                                .Refresh
                                .Cls
                          End With
                            frmRes.SCDbuffer1.Print P1ShieldCD
                            
  'Public Const DispP1HealthX As Integer = 530
     'Public Const DispP1HealthY As Integer = 386
      '  Public Const DispP2HealthX As Integer = 90
       ' Public Const DispP2HealthY As Integer = 41
                            
                            
                        End If
                           
                           'p2
                    If P2ShieldRunning = True And P2ShieldCD > 0 Then
                        P2D5 = P2D5 + 1
                            If P2D5 = SFDelay Then P2D5 = 0
                        If P2D5 = 0 Then
                           P2ShieldFrame = P2ShieldFrame + 1
                           P2ShieldCD = P2ShieldCD - 1
                           End If
                           
                                            
                                   If P2ShieldFrame = 4 Then P2ShieldFrame = 0
                                   
                            BitBlt frmMain.hDC, P2X - 17, P2Y - 17, 64, 64, _
                            frmRes.picShields.hDC, P2ShieldFrame * 64, 0, vbSrcPaint
                            
                               'print sheild energy level into backbuffer #2
                            With frmRes.SCDbuffer2
                                .Refresh
                                .Cls
                            End With
                            frmRes.SCDbuffer2.Print P2ShieldCD
                        End If

'blt shield display to game screen
BitBlt frmMain.hDC, 110, 65, 48, 16, frmRes.SCDbuffer2.hDC, _
0, 0, vbSrcPaint

BitBlt frmMain.hDC, 550, 410, 48, 16, frmRes.SCDbuffer1.hDC, _
0, 0, vbSrcPaint
'''''''''''''''''''''''''''''''''''''
'###################################'

If B1Frame = 0 And P1Firing = False Then
'do nothing...
Else
 B1Frame = B1Frame + 1
 If B1Frame >= BDelay(P1CurWeapon) Then B1Frame = 0
End If

If B2Frame = 0 And P2Firing = False Then
'do nothing...
Else
 B2Frame = B2Frame + 1
  If B2Frame >= BDelay(P2CurWeapon) Then B2Frame = 0
End If
'If P1Firing = True Then 'make delay time for firing...
' B1frame = B1frame + 1
' Else
 'B1frame = 0
'End If

'If BDelay > 0 And P2Firing = False Then
' B2frame = B2frame + 1
'  Else
 'B2frame = 0
' End If

 

'###########################################
'################ HUD ######################
'###########################################
'Panels (saved for last)
'red 530, 395
If ShowHUD = True Then
   BitBlt frmMain.hDC, 452, 354, P1Panel.ScaleWidth, P1Panel.ScaleHeight, _
   P1Panel.hDC, 0, 0, vbSrcPaint
   '0, 0,
   BitBlt frmMain.hDC, 10, 10, P2Panel.ScaleWidth, P2Panel.ScaleHeight, _
   P2Panel.hDC, 0, 0, vbSrcPaint
   
   BitBlt frmMain.hDC, DispP1NameX, DispP1NameY, frmRes.picP1Name.ScaleWidth, _
   frmRes.picP1Name.ScaleHeight, frmRes.picP1Name.hDC, _
   0, 0, vbSrcPaint
   
   BitBlt frmMain.hDC, DispP2NameX, DispP2NameY, frmRes.picP2Name.ScaleWidth, _
   frmRes.picP2Name.ScaleHeight, frmRes.picP2Name.hDC, _
   0, 0, vbSrcPaint
   
   'weapons diagrams...
   BitBlt frmMain.hDC, DispP1WeapX, DispP1WeapY, frmRes.picWeapon(P1CurWeapon).ScaleWidth, _
   frmRes.picWeapon(P1CurWeapon).ScaleHeight, _
   frmRes.picWeapon(P1CurWeapon).hDC, 0, 0, vbSrcPaint
   
   BitBlt frmMain.hDC, DispP2WeapX, DispP2WeapY, frmRes.picWeapon(P2CurWeapon).ScaleWidth, _
   frmRes.picWeapon(P2CurWeapon).ScaleHeight, _
   frmRes.picWeapon(P2CurWeapon).hDC, 0, 0, vbSrcPaint

   BitBlt frmMain.hDC, DispP1HealthX, DispP1HealthY, frmRes.picP1Health.ScaleWidth, _
   frmRes.picP1Health.ScaleHeight, _
   frmRes.picP1Health.hDC, 0, 0, vbSrcPaint
   
   BitBlt frmMain.hDC, DispP2HealthX, DispP2HealthY, frmRes.picP2Health.ScaleWidth, _
   frmRes.picP2Health.ScaleHeight, _
   frmRes.picP2Health.hDC, 0, 0, vbSrcPaint
     
   '###############################################
   '#################### END OF HUD ###############
   '###############################################
End If
    
    'show score if required
    If ShowScores = True Then
        
        BitBlt frmMain.hDC, DispCSTitleX, DispCSTitleY, 143, 14, frmRes.picCS.hDC, _
        0, 0, vbSrcPaint
        
        BitBlt frmMain.hDC, DispP1ScoreX, DispP1ScoreY, 128, 25, frmRes.picP1Frags.hDC, _
        0, 0, vbSrcPaint
        
        BitBlt frmMain.hDC, DispP2ScoreX, DispP2ScoreY, 128, 25, frmRes.picP2Frags.hDC, _
        0, 0, vbSrcPaint
        
        BitBlt frmMain.hDC, DispCSpanX, DispCSpanY, 138, 13, frmRes.picPF4.hDC, _
        0, 0, vbSrcPaint
    End If
    'end of showing scores
    
    'show help if required...
    If ShowHELP = True Then
        BitBlt frmMain.hDC, DispHelpX, DispHelpY, 303, 283, frmRes.picGameHelp.hDC, _
        0, 0, vbSrcPaint
    End If
    
    
    'show the "GO!" if required...
        If ShowGo = True And FadeColor > 0 Then
         ShowGoFade
        ElseIf FadeColor <= 0 Then
          ShowGo = False 'if its black then stop
        End If
        
                
        'if required, display the FPS
        If ShowFPS = True Then
            BitBlt frmMain.hDC, 535, 10, 64, 16, _
            frmRes.picFPS.hDC, 0, 0, vbSrcCopy
            'increase frames counter
                CurFPS = CurFPS + 1
        End If
        
'##### FADING HEALTH DISPLAY (yellow pulsating) #####
If HGTE = True Then
    Select Case YelFadeUp
 Case False
            
            Select Case YelFadeCol
                   Case Is > 37380 '32896 '41120, 37380
                   YelFadeCol = YelFadeCol - 514
                   Case Else
                   YelFadeUp = True
            End Select
            
 Case True

            Select Case YelFadeCol
                   Case Is < 65535
                   YelFadeCol = YelFadeCol + 514
                   Case Else
                   YelFadeUp = False
            End Select
End Select

'variant instructs
With frmRes
.picP1Health.ForeColor = YelFadeCol
.picP2Health.ForeColor = YelFadeCol
.picP1Frags.ForeColor = YelFadeCol
.picP2Frags.ForeColor = YelFadeCol
End With
Call SetP1Health
Call SetP2Health
If ShowScores = True Then Call SetFragScores

End If
 '####################################################
Loop
End Sub

Public Sub InitNewGame()
'initialize a new match...
'Set Default Values
'NOTE: temp
'With P1

'set weapon bullet sprites
Set PWeapon(0).WeapBSprite = frmRes.ybolt
Set PWeapon(1).WeapBSprite = frmRes.cgBullet
Set PWeapon(2).WeapBSprite = frmRes.PlasmaB

YelFadeCol = 65535

P1D5 = 0 'delays at 0...
P2D5 = 0

'fill up shield banks...
P1ShieldCD = 500
P2ShieldCD = 500

With frmRes.SCDbuffer2
 .Refresh
 .Cls
End With
 frmRes.SCDbuffer2.Print P2ShieldCD

With frmRes.SCDbuffer1
 .Refresh
 .Cls
End With
 frmRes.SCDbuffer1.Print P1ShieldCD

P1ShieldFrame = 0
P2ShieldFrame = 0
P1ShieldRunning = False
P2ShieldRunning = False

'set weapon impact sprites
Set PWeapon(0).WeapBSparK = frmRes.picLazI
Set PWeapon(1).WeapBSparK = frmRes.picSpark
Set PWeapon(2).WeapBSparK = frmRes.picPlasmaSpark

'Weapon Damages
PWeapon(0).WeapDamage = 4
PWeapon(1).WeapDamage = 2
PWeapon(2).WeapDamage = 10
PWeapon(3).WeapDamage = 16

'weapon x shifts
PWeapon(0).WeapXShift = 15
PWeapon(1).WeapXShift = 16
PWeapon(2).WeapXShift = -16

PWeapon(0).WeapXshiftB = 1
PWeapon(1).WeapXshiftB = 0
PWeapon(2).WeapXshiftB = 32

PWeapon(0).WeapYShiftB = 32
PWeapon(1).WeapYShiftB = 16
PWeapon(2).WeapYShiftB = 32

'weapon Y shifts
PWeapon(0).P1WeapYShift = -26
PWeapon(0).P2WeapYShift = 30
    PWeapon(1).P1WeapYShift = -8
    PWeapon(1).P2WeapYShift = 30
        PWeapon(2).P1WeapYShift = -40
        PWeapon(2).P2WeapYShift = 0

' player vars
P1CurWeapon = 0 'current weapon
P2CurWeapon = 0 'current weapon


   P1Health = 200
   P1MaxHealth = 200
   P1S = 0
   P1SL = 0
   P1X = 384
   P1Y = (frmMain.ScaleHeight - (8 + 32))
   
        Set P1Mask = frmRes.CMask
        Set P2Mask = frmRes.CMask
        
P2Health = 200 'set health to 50 for gain-test
P2MaxHealth = 200
P2S = 0
P2SL = 0

'clear clipboard to free up RAM
Clipboard.Clear

'assume P2 position...
    P2X = 184
    P2Y = 8

'set default health of 100
SetP1Health 'P1Health
SetP2Health 'P2Health

'MsgBox PWeapon(4).WeapBSparK.ScaleWidth

'set desault FPS counter
frmRes.picFPS.Print "0"

    GameOn = True
    frmMain.Cls
    GdiFlush
    
    ShowGo = True
    FadeColor = 65280
'Start Game loop
  DoGame
End Sub


Private Sub tmrStuff_Timer()
If DoHG = True Then
 CHGInt = CHGInt + 1
    If CHGInt = HGInt Then
        If P1Health < P1MaxHealth Then
            P1Health = P1Health + 1
            SetP1Health 'P1Health
        End If
        If P2Health < P2MaxHealth Then
            P2Health = P2Health + 1
            SetP2Health 'P2Health
        End If
     CHGInt = 0
     End If
End If

If FadeScores = True Then 'make health go away...
   If HFIndex = 2 Then
    ShowScores = False
    HFIndex = 0
    FadeScores = False
    
    'make it say go when scores are gone...
    ShowGo = True
    FadeColor = 65280
   End If
   HFIndex = HFIndex + 1
End If
If ShowFPS = True Then
frmRes.picFPS.Cls
frmRes.picFPS.Print CurFPS & " FPS"
CurFPS = 1
End If

CanTogP1W = True
CanTogP2W = True

'##################################
'####### Weapons Toggle Test ######
'##################################

    'Select Case P1CurWeapon
    'Case 0
    'P1CurWeapon = 1
    'Case 1
    'P1CurWeapon = 2
    'Case 2
    'P1CurWeapon = 0
    'End Select

'##################################
'##################################

End Sub

Public Sub SetRedtoP1()
   Set P1Panel = frmRes.RPanel
   Set P1Sprite = frmRes.CRed
   P1Color = 192      'color
End Sub

Public Sub SetRedtoP2()
   Set P2Panel = frmRes.RPanel
   Set P2Sprite = frmRes.CRed
   
   P2Color = 192 'color
End Sub

Public Sub SetBlueToP2()
 Set P2Panel = frmRes.BPanel
 Set P2Sprite = frmRes.CBlue
  P2Color = 12599296
End Sub

Public Sub SetBlueToP1()
 Set P1Panel = frmRes.BPanel
 Set P1Sprite = frmRes.CBlue
 P1Color = 12599296
End Sub

Public Sub SetGameSpeeds(GameSpeed As Byte)

Select Case GameSpeed
  Case 1 '1 = slow
      
  '## Cannon ##
      PBulletSpeed(0) = 7
      BDelay(0) = 19
  '############
  
  '## Chain Gun ##
       PBulletSpeed(1) = 12
      BDelay(1) = 11
  '############
  
  '## Plasma Cannon ##
       PBulletSpeed(2) = 3
      BDelay(2) = 29
  '############
  
      P2Speed = 2
      P1Speed = 2
  HGInt = 25
  
  SFDelay = 5
  
  Case 2    '2 = medium
  
SFDelay = 4
  '## Cannon##
       PBulletSpeed(0) = 14
      BDelay(0) = 9
  '############
  
  '## Chain Gun ##
       PBulletSpeed(1) = 26
      BDelay(1) = 6
  '############
  
  '## Plasma Cannon ##
       PBulletSpeed(2) = 9
      BDelay(2) = 18
  '############
  
      P2Speed = 7
      P1Speed = 7
      
      HGInt = 15
      '''''''''''''''
  Case 3    '3 = FAST
  

  '## Cannon ##
       PBulletSpeed(0) = 24
      BDelay(0) = 4
  '############
  
  '## Chain Gun ##
       PBulletSpeed(1) = 31
      BDelay(1) = 4
  '############
  
  '## Plasma Cannon ##
       PBulletSpeed(2) = 18
      BDelay(2) = 11
  '############
  
      P2Speed = 10
      P1Speed = 10
    HGInt = 7
    SFDelay = 3
 Case 4
   'Ultra Slow
     
SFDelay = 6
  '## Cannon ##
       PBulletSpeed(0) = 4
      BDelay(0) = 37
  '############
  
  '## Chain Gun ##
       PBulletSpeed(1) = 7
      BDelay(1) = 26
  '############
  
  '## Plasma Cannon ##
       PBulletSpeed(2) = 1
      BDelay(2) = 64
  '############
  
      P2Speed = 1
      P1Speed = 1
    HGInt = 35
End Select
End Sub

Public Sub ShowGoFade()
      If FadeColor <= 0 Then ShowGo = False 'if its black then stop
                
                With frmRes.picGo
                     .ForeColor = FadeColor
                     .Cls
                End With
                    frmRes.picGo.Print "GO!"
                    
               'decrease color shade for fade effect...
                  FadeColor = FadeColor - 512
                BitBlt frmMain.hDC, 252, 186, 95, 68, frmRes.picGo.hDC, _
                0, 0, vbSrcPaint
End Sub
