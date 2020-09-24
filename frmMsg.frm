VERSION 5.00
Begin VB.Form frmMsg 
   BackColor       =   &H00404040&
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   2775
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   3660
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Picture         =   "frmMsg.frx":0000
   ScaleHeight     =   185
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   244
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Label lblCaption 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   15.75
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   1245
      Left            =   480
      TabIndex        =   3
      Top             =   720
      Width           =   2775
   End
   Begin VB.Shape Shape2 
      Height          =   375
      Left            =   1155
      Shape           =   4  'Rounded Rectangle
      Top             =   2250
      Width           =   900
   End
   Begin VB.Shape Shape1 
      FillColor       =   &H0000FF00&
      FillStyle       =   0  'Solid
      Height          =   210
      Left            =   570
      Shape           =   3  'Circle
      Top             =   315
      Width           =   135
   End
   Begin VB.Shape shpBorder 
      Height          =   375
      Left            =   210
      Shape           =   4  'Rounded Rectangle
      Top             =   2250
      Width           =   900
   End
   Begin VB.Label lblOK 
      Alignment       =   2  'Center
      BackColor       =   &H00404040&
      Caption         =   "Ok"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   14.25
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   345
      Left            =   225
      TabIndex        =   1
      Top             =   2265
      Width           =   870
   End
   Begin VB.Label lblTitle 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "BOT - Match"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   9.75
         Charset         =   0
         Weight          =   800
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080FF80&
      Height          =   210
      Left            =   750
      TabIndex        =   0
      Top             =   300
      Width           =   900
   End
   Begin VB.Label lblCanBut 
      Alignment       =   2  'Center
      BackColor       =   &H00404040&
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   14.25
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   345
      Left            =   1185
      TabIndex        =   2
      Top             =   2265
      Width           =   870
   End
   Begin VB.Shape Shape3 
      BorderColor     =   &H00004000&
      FillColor       =   &H00002000&
      FillStyle       =   6  'Cross
      Height          =   1365
      Left            =   390
      Shape           =   4  'Rounded Rectangle
      Top             =   555
      Width           =   2925
   End
End
Attribute VB_Name = "frmMsg"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Copyright 2001 - yar interactive

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
ResMsgButs
End Sub

Private Sub lblCanBut_Click()
Select Case MsgStat
Case 1
 frmMsg.Hide
 frmMain.Show
 frmMain.DoGame
 Case 3
       MsgFlag = False
        frmMsg.Hide
 Case 5
 frmMsg.Hide
End Select
lblCanBut.Visible = False
 Shape2.Visible = False
End Sub


Private Sub lblOK_Click()
Select Case MsgStat
    Case 0
        frmMsg.Hide
        Case 1
        frmMsg.Hide
        Load frmGameType
        Unload frmSetup
        'Unload frmMsg
        Unload frmMain
        Unload frmRes
            MPGameType = False
            HighSkill = False
            HGraphics = False
        frmGameType.Show
    Case 3
    'they pressed ok...
        MsgFlag = True
        frmMsg.Hide
    Case 5
    frmMsg.Hide
        'Unload frmMsg
        Unload frmMain
        Unload frmRes
        Sleep 50
        End 'exit game
End Select
 lblCanBut.Visible = False
 Shape2.Visible = False
End Sub

Public Sub ResMsgButs()
If lblCanBut.BackColor = &H404040 And _
lblOK.BackColor = &H404040 Then Exit Sub

lblCanBut.BackColor = &H404040
lblOK.BackColor = &H404040
End Sub

Private Sub lblOK_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If lblOK.BackColor = &H404040 Then
ResMsgButs
lblOK.BackColor = &H8000&
End If
End Sub

Private Sub lblCanBut_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If lblCanBut.BackColor = &H404040 Then
ResMsgButs
lblCanBut.BackColor = &H8000&
End If
End Sub
