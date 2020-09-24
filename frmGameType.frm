VERSION 5.00
Begin VB.Form frmGameType 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "BotMatch - Select Game Options"
   ClientHeight    =   5250
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   6225
   HasDC           =   0   'False
   LinkTopic       =   "BotMatch - Game Type"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Picture         =   "frmGameType.frx":0000
   ScaleHeight     =   350
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   415
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   3735
      Top             =   2670
   End
   Begin VB.PictureBox rotF 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   1920
      Index           =   3
      Left            =   -1515
      Picture         =   "frmGameType.frx":28DE
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   128
      TabIndex        =   19
      Top             =   990
      Visible         =   0   'False
      Width           =   1920
   End
   Begin VB.PictureBox rotF 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   1920
      Index           =   2
      Left            =   -1425
      Picture         =   "frmGameType.frx":3810
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   128
      TabIndex        =   18
      Top             =   1260
      Visible         =   0   'False
      Width           =   1920
   End
   Begin VB.PictureBox rotF 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   1920
      Index           =   1
      Left            =   -1035
      Picture         =   "frmGameType.frx":4751
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   128
      TabIndex        =   17
      Top             =   780
      Visible         =   0   'False
      Width           =   1920
   End
   Begin VB.PictureBox rotF 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   1920
      Index           =   0
      Left            =   -1275
      Picture         =   "frmGameType.frx":56BF
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   128
      TabIndex        =   16
      Top             =   705
      Visible         =   0   'False
      Width           =   1920
   End
   Begin VB.PictureBox picAnim 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   2445
      Left            =   990
      Picture         =   "frmGameType.frx":65FA
      ScaleHeight     =   2445
      ScaleWidth      =   2460
      TabIndex        =   20
      Top             =   2775
      Width           =   2460
      Begin VB.PictureBox picColor 
         Appearance      =   0  'Flat
         BackColor       =   &H000000FF&
         ForeColor       =   &H80000008&
         HasDC           =   0   'False
         Height          =   345
         Left            =   465
         ScaleHeight     =   315
         ScaleWidth      =   990
         TabIndex        =   24
         Top             =   1320
         Visible         =   0   'False
         Width           =   1020
      End
      Begin VB.TextBox txtPName 
         Appearance      =   0  'Flat
         BackColor       =   &H00404040&
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FF00&
         Height          =   315
         Left            =   465
         MaxLength       =   8
         TabIndex        =   21
         Text            =   "VIPER"
         Top             =   450
         Visible         =   0   'False
         Width           =   1470
      End
      Begin VB.Label lblColor 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Player  Color:"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   210
         Left            =   75
         TabIndex        =   23
         Top             =   1050
         Visible         =   0   'False
         Width           =   1095
      End
      Begin VB.Label lblCompName 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Player  Name:"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   210
         Left            =   45
         TabIndex        =   22
         Top             =   195
         Visible         =   0   'False
         Width           =   1110
      End
   End
   Begin VB.Label cmdEsc 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "_"
      BeginProperty Font 
         Name            =   "Arial Black"
         Size            =   14.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   420
      Index           =   1
      Left            =   5205
      TabIndex        =   27
      Top             =   30
      Width           =   465
   End
   Begin VB.Shape shpEsc 
      BorderColor     =   &H0000C000&
      BorderWidth     =   2
      FillColor       =   &H00002000&
      FillStyle       =   0  'Solid
      Height          =   390
      Index           =   1
      Left            =   5235
      Shape           =   4  'Rounded Rectangle
      Top             =   75
      Width           =   435
   End
   Begin VB.Label cmdEsc 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "x"
      BeginProperty Font 
         Name            =   "Arial Black"
         Size            =   14.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   420
      Index           =   0
      Left            =   5715
      TabIndex        =   26
      Top             =   30
      Width           =   465
   End
   Begin VB.Shape shpEsc 
      BorderColor     =   &H0000C000&
      BorderWidth     =   2
      FillColor       =   &H00002000&
      FillStyle       =   0  'Solid
      Height          =   390
      Index           =   0
      Left            =   5730
      Shape           =   4  'Rounded Rectangle
      Top             =   75
      Width           =   435
   End
   Begin VB.Line Line1 
      BorderColor     =   &H0000C000&
      Index           =   1
      X1              =   342
      X2              =   342
      Y1              =   0
      Y2              =   216
   End
   Begin VB.Line Line1 
      BorderColor     =   &H0000C000&
      Index           =   0
      X1              =   281
      X2              =   0
      Y1              =   269
      Y2              =   269
   End
   Begin VB.Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Move Mouse Over to Configure Player"
      ForeColor       =   &H0000C000&
      Height          =   255
      Left            =   990
      TabIndex        =   25
      Top             =   2505
      Width           =   2775
   End
   Begin VB.Label lblOptVal 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "P1 vs. P2"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00E0E0E0&
      Height          =   240
      Index           =   3
      Left            =   3075
      TabIndex        =   15
      Top             =   1125
      Width           =   855
   End
   Begin VB.Label lblOptVal 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Easy"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00E0E0E0&
      Height          =   240
      Index           =   4
      Left            =   3075
      TabIndex        =   14
      Top             =   1500
      Width           =   435
   End
   Begin VB.Label lblOptVal 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Low"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00E0E0E0&
      Height          =   240
      Index           =   5
      Left            =   3075
      TabIndex        =   13
      Top             =   1875
      Width           =   405
   End
   Begin VB.Label lblOpt 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Graphics Level:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   210
      Index           =   5
      Left            =   75
      TabIndex        =   12
      Top             =   1875
      Width           =   2535
   End
   Begin VB.Label lblOpt 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Skill Level:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   210
      Index           =   4
      Left            =   75
      TabIndex        =   11
      Top             =   1500
      Width           =   2145
   End
   Begin VB.Label lblOpt 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Game Type:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   210
      Index           =   3
      Left            =   90
      TabIndex        =   10
      Top             =   1125
      Width           =   2220
   End
   Begin VB.Label lblOpt 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Configure My Player"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   210
      Index           =   2
      Left            =   105
      TabIndex        =   9
      Top             =   750
      Width           =   2895
   End
   Begin VB.Label lblOpt 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "http://www.yarinteractive.com"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   210
      Index           =   1
      Left            =   75
      TabIndex        =   8
      Top             =   375
      Width           =   3750
   End
   Begin VB.Label lblOpt 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Exit Game"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   210
      Index           =   0
      Left            =   75
      TabIndex        =   7
      Top             =   15
      Width           =   2070
   End
   Begin VB.Line linOpt 
      BorderColor     =   &H0000FFFF&
      Index           =   5
      Visible         =   0   'False
      X1              =   68
      X2              =   87
      Y1              =   130
      Y2              =   130
   End
   Begin VB.Line linOpt 
      BorderColor     =   &H0000FFFF&
      Index           =   4
      Visible         =   0   'False
      X1              =   68
      X2              =   87
      Y1              =   105
      Y2              =   105
   End
   Begin VB.Line linOpt 
      BorderColor     =   &H0000FFFF&
      Index           =   3
      Visible         =   0   'False
      X1              =   68
      X2              =   87
      Y1              =   80
      Y2              =   80
   End
   Begin VB.Line linOpt 
      BorderColor     =   &H0000FFFF&
      Index           =   2
      Visible         =   0   'False
      X1              =   68
      X2              =   87
      Y1              =   55
      Y2              =   55
   End
   Begin VB.Line linOpt 
      BorderColor     =   &H0000FFFF&
      Index           =   1
      Visible         =   0   'False
      X1              =   68
      X2              =   87
      Y1              =   30
      Y2              =   30
   End
   Begin VB.Line linOpt 
      BorderColor     =   &H0000FFFF&
      Index           =   0
      Visible         =   0   'False
      X1              =   68
      X2              =   87
      Y1              =   6
      Y2              =   6
   End
   Begin VB.Shape shpOpt 
      BorderColor     =   &H0000FFFF&
      Height          =   180
      Index           =   5
      Left            =   75
      Top             =   1860
      Visible         =   0   'False
      Width           =   945
   End
   Begin VB.Shape shpOpt 
      BorderColor     =   &H0000FFFF&
      Height          =   180
      Index           =   4
      Left            =   75
      Top             =   1485
      Visible         =   0   'False
      Width           =   945
   End
   Begin VB.Shape shpOpt 
      BorderColor     =   &H0000FFFF&
      Height          =   180
      Index           =   3
      Left            =   75
      Top             =   1110
      Visible         =   0   'False
      Width           =   945
   End
   Begin VB.Shape shpOpt 
      BorderColor     =   &H0000FFFF&
      Height          =   180
      Index           =   2
      Left            =   75
      Top             =   735
      Visible         =   0   'False
      Width           =   945
   End
   Begin VB.Shape shpOpt 
      BorderColor     =   &H0000FFFF&
      Height          =   180
      Index           =   1
      Left            =   75
      Top             =   360
      Visible         =   0   'False
      Width           =   945
   End
   Begin VB.Shape shpOpt 
      BorderColor     =   &H0000FFFF&
      Height          =   180
      Index           =   0
      Left            =   75
      Top             =   0
      Visible         =   0   'False
      Width           =   945
   End
   Begin VB.Label lblBut 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Accept"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   26.25
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   630
      Index           =   6
      Left            =   4275
      TabIndex        =   6
      Top             =   3810
      Width           =   1770
   End
   Begin VB.Label lblBut 
      BackStyle       =   0  'Transparent
      Height          =   210
      Index           =   5
      Left            =   4305
      TabIndex        =   5
      Top             =   4365
      Width           =   1665
   End
   Begin VB.Label lblBut 
      BackStyle       =   0  'Transparent
      Height          =   210
      Index           =   4
      Left            =   4470
      TabIndex        =   4
      Top             =   4575
      Width           =   1335
   End
   Begin VB.Label lblBut 
      BackStyle       =   0  'Transparent
      Height          =   210
      Index           =   3
      Left            =   4695
      TabIndex        =   3
      Top             =   4785
      Width           =   870
   End
   Begin VB.Label lblBut 
      BackStyle       =   0  'Transparent
      Height          =   255
      Index           =   2
      Left            =   4335
      TabIndex        =   2
      Top             =   3720
      Width           =   1605
   End
   Begin VB.Label lblBut 
      BackStyle       =   0  'Transparent
      Height          =   210
      Index           =   1
      Left            =   4470
      TabIndex        =   1
      Top             =   3510
      Width           =   1335
   End
   Begin VB.Label lblBut 
      BackStyle       =   0  'Transparent
      Height          =   210
      Index           =   0
      Left            =   4695
      TabIndex        =   0
      Top             =   3300
      Width           =   870
   End
   Begin VB.Shape shpOkBut 
      BorderColor     =   &H00008000&
      BorderWidth     =   4
      FillColor       =   &H00002000&
      FillStyle       =   0  'Solid
      Height          =   1755
      Left            =   4260
      Shape           =   3  'Circle
      Top             =   3270
      Width           =   1755
   End
End
Attribute VB_Name = "frmGameType"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private GTAnimFrame As Byte
Private canpermshow As Boolean
Private p1team As Byte 'player's team...

Private Sub cmdEsc_Click(Index As Integer)
 'exit (X) and minimize (_) buttons
 Select Case Index
  Case 0
       MsgStat = 5 'prompt for exit...
       ShowMsg "You are about to exit the program", "BotMatch - Exit Game?"
  Case 1
  
  'minimize the window...
   frmGameType.WindowState = 1
   End Select
End Sub

Private Sub cmdEsc_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'exit (X) and minimize (_) buttons Hover effect...
If cmdEsc(Index).ForeColor = &HC000& Then
   resGTButs
    shpEsc(Index).BorderColor = &HFF00&
    cmdEsc(Index).ForeColor = &HC0FFC0
End If
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
resGTButs
MPGameType = True
End Sub

Private Sub lblBut_Click(Index As Integer)
'Accept button...
If MPGameType = True Then
'if they have a 2 player game selected,
'make options on 2-player form match
Load frmSetup
frmSetup.Show
frmSetup.Timer1.Enabled = True
frmGameType.Hide
Unload frmGameType

'check graphics settings
Select Case HGraphics
 Case False
 frmSetup.OPLowG.Value = True
 Case True
 frmSetup.opHighG.Value = True
End Select
Else
MsgBox "Only 2 player game-play is available in this version..."
End If
End Sub

Private Sub lblBut_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
If shpOkBut.BorderColor = &H8000& Then
 resGTButs 'reset Game-Type button colors
 shpOkBut.BorderColor = &HFF00&
 lblBut(6).ForeColor = &HC0FFC0
End If
End Sub

Public Sub resGTButs()
Dim c As Byte
If shpOkBut.BorderColor = &H8000& And _
lblBut(6).ForeColor = &HC000& And _
shpOpt(0).Visible = False And shpOpt(1).Visible = False _
And shpOpt(2).Visible = False _
And shpOpt(3).Visible = False _
And shpOpt(4).Visible = False _
And shpOpt(5).Visible = False _
And Timer1.Enabled = True _
And txtPName.Visible = False _
And lblCompName.Visible = False _
And lblColor.Visible = False _
And picColor.Visible = False _
And lblOptVal(3).ForeColor = &HE0E0E0 _
And lblOptVal(4).ForeColor = &HE0E0E0 _
And lblOptVal(5).ForeColor = &HE0E0E0 _
And cmdEsc(0).ForeColor = &HC000& _
And cmdEsc(1).ForeColor = &HC000& _
And shpEsc(0).BorderColor = &HC000& _
And shpEsc(1).BorderColor = &HC000& Then Exit Sub

If canpermshow = False Then
lblColor.Visible = False
picColor.Visible = False
lblCompName.Visible = False
txtPName.Visible = False
Timer1.Enabled = True
End If

For c = 0 To 5
 shpOpt(c).Visible = False
 lblOpt(c).ForeColor = &HC000&
 linOpt(c).Visible = False
Next c

cmdEsc(0).ForeColor = &HC000&
cmdEsc(1).ForeColor = &HC000&
shpEsc(0).BorderColor = &HC000&
shpEsc(1).BorderColor = &HC000&

lblOptVal(3).ForeColor = &HE0E0E0
lblOptVal(4).ForeColor = &HE0E0E0
lblOptVal(5).ForeColor = &HE0E0E0

shpOkBut.BorderColor = &H8000&
lblBut(6).ForeColor = &HC000&
End Sub


Private Sub lblOpt_Click(Index As Integer)
On Error Resume Next
Select Case Index
Case 0
'exit prompt
       MsgStat = 5 'prompt for exit...
       ShowMsg "You are about to exit the program", "BotMatch - Exit Game?"
Case 1
Shell "Start http://www.yarinteractive.com"
Case 2
StopAnim 'stop animation, and set focus to Name field...
canpermshow = True
txtPName.SetFocus
Case 3
    Call ToggleGameType
Case 4
    Call ToggleSkill
Case 5
    Call SetGraphicsLvl
End Select
End Sub

Private Sub lblOpt_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
If lblOpt(Index).ForeColor = &HC000& Then
   resGTButs
   lblOpt(Index).ForeColor = &H80FFFF
   shpOpt(Index).Visible = True
   linOpt(Index).Visible = True
   
   lblOptVal(Index).ForeColor = &HFFFF&
End If
End Sub


Private Sub picAnim_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
StopAnim
canpermshow = False
End Sub

Private Sub picColor_Click()
If p1team = 0 Then
p1team = 1
picColor.BackColor = vbBlue
Else
p1team = 0
picColor.BackColor = vbRed
End If
End Sub

Private Sub Timer1_Timer()
Dim a As Double, b As Double
picAnim.Cls
BitBlt picAnim.hDC, 16, 16, 128, 128, rotF(GTAnimFrame).hDC, 0, 0, vbSrcPaint
'Label1.Caption = GTAnimFrame
GTAnimFrame = GTAnimFrame + 1
If GTAnimFrame = 4 Then GTAnimFrame = 0
End Sub

Public Sub ToggleGameType()
'toggle type of game...
If MPGameType = False Then
    MPGameType = True
    lblOptVal(3).Caption = "P1 vs. P2"
    Else
    MPGameType = False
    lblOptVal(3).Caption = "P1 vs. Computer"
End If
End Sub

Public Sub ToggleSkill()
'toggle game skill level
If HighSkill = True Then
   HighSkill = False 'low skill level
   DoHG = True
   lblOptVal(4).Caption = "Easy"

        Else         'high skill level
        HighSkill = True
        DoHG = False
        lblOptVal(4).Caption = "Hard"
    End If
End Sub

Public Sub SetGraphicsLvl()
'toggle graphics level...
If HGraphics = False Then
          HGraphics = True
            ShowFPS = False
            ShowHUD = True
            ShowRetro = True
            HGTE = True
            
                With frmMain
                    .Refresh
                    .Cls
                    .Picture = frmRes.picBGgrid.Picture
                End With
        GridOn = True
        
    With frmSetup
        .chkGrid.Value = 1
        .chkAutoH.Value = 0
        .chkFPS.Value = 0
        .chkHUD.Value = 1
        .chkRetro.Value = 1
        .chkHGT.Value = 1
        .opHighG.Value = True
    End With
    
    lblOptVal(5).Caption = "High"
        
        ElseIf HGraphics = True Then
        HGraphics = False
    lblOptVal(5).Caption = "Low"
                With frmMain
                    .Refresh
                    .Cls
                    .Picture = Null
                End With
            GridOn = False
        
        With frmSetup
             .chkGrid.Value = 0
             .chkAutoH.Value = 0
             .chkFPS.Value = 0
             .chkHUD.Value = 1
             .chkRetro.Value = 0
             .chkHGT.Value = 0
             .OPLowG.Value = True
        End With
End If
End Sub

Private Sub txtPName_GotFocus()
StopAnim
End Sub

Public Sub StopAnim()
If Timer1.Enabled = True And lblCompName.Visible = False _
And txtPName.Visible = False And lblColor.Visible = False _
And picColor.Visible = False Then
'stop rotation and show player options...
resGTButs
Timer1.Enabled = False
picColor.Visible = True
lblColor.Visible = True
txtPName.Visible = True
lblCompName.Visible = True
picAnim.Refresh
End If

End Sub

Public Sub SetUpColors()
If p1team = 0 Then
    Call frmMain.SetRedtoP1
    Call frmMain.SetBlueToP2
  Else
    Call frmMain.SetRedtoP2
    Call frmMain.SetBlueToP1
End If
End Sub
