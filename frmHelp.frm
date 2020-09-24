VERSION 5.00
Begin VB.Form frmHelp 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   ClientHeight    =   7200
   ClientLeft      =   120
   ClientTop       =   120
   ClientWidth     =   9600
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   480
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picDisplay 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   3015
      Left            =   90
      Picture         =   "frmHelp.frx":0000
      ScaleHeight     =   3015
      ScaleWidth      =   4260
      TabIndex        =   7
      Top             =   810
      Width           =   4260
      Begin VB.Label lblDead 
         BackStyle       =   0  'Transparent
         Caption         =   "Primary Thrusters"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   9.75
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   495
         Index           =   4
         Left            =   990
         TabIndex        =   12
         Top             =   2520
         Width           =   1020
      End
      Begin VB.Label lblDead 
         Alignment       =   1  'Right Justify
         BackStyle       =   0  'Transparent
         Caption         =   "BOT Systems Display"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   9.75
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FF00&
         Height          =   285
         Index           =   3
         Left            =   -90
         TabIndex        =   11
         Top             =   195
         Width           =   1425
      End
      Begin VB.Label lblDead 
         BackStyle       =   0  'Transparent
         Caption         =   "Weapons Bay"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   9.75
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Index           =   2
         Left            =   3165
         TabIndex        =   10
         Top             =   1185
         Width           =   885
      End
      Begin VB.Line Line1 
         BorderColor     =   &H00008000&
         X1              =   2025
         X2              =   3120
         Y1              =   1305
         Y2              =   1305
      End
      Begin VB.Label lblDead 
         BackStyle       =   0  'Transparent
         Caption         =   "Outer Hull Armor"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   9.75
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Index           =   1
         Left            =   2700
         TabIndex        =   9
         Top             =   60
         Width           =   1320
      End
      Begin VB.Label lblDead 
         BackStyle       =   0  'Transparent
         Caption         =   "Shield Emmiter Ports"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   9.75
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Index           =   0
         Left            =   2880
         TabIndex        =   8
         Top             =   2190
         Width           =   1320
      End
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   2250
      Top             =   4290
   End
   Begin VB.PictureBox picHolder 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   2115
      Left            =   165
      ScaleHeight     =   141
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   225
      TabIndex        =   2
      Top             =   4830
      Width           =   3375
      Begin VB.PictureBox picScroller 
         BackColor       =   &H00000000&
         BorderStyle     =   0  'None
         Height          =   8160
         Left            =   165
         ScaleHeight     =   8160
         ScaleWidth      =   3180
         TabIndex        =   3
         Top             =   2040
         Width           =   3180
         Begin VB.Label lblCreds 
            Alignment       =   2  'Center
            BackStyle       =   0  'Transparent
            Caption         =   "(c)  Copyright 2001 - Yar Interactive Software"
            BeginProperty Font 
               Name            =   "MS Serif"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0080FF80&
            Height          =   450
            Index           =   4
            Left            =   315
            TabIndex        =   19
            Top             =   6675
            Width           =   2550
         End
         Begin VB.Label lblCreds 
            Alignment       =   2  'Center
            BackStyle       =   0  'Transparent
            Caption         =   "Randy Perry"
            BeginProperty Font 
               Name            =   "MS Serif"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0080FF80&
            Height          =   240
            Index           =   3
            Left            =   60
            TabIndex        =   18
            Top             =   6045
            Width           =   3075
         End
         Begin VB.Label lblCreds 
            Alignment       =   2  'Center
            BackStyle       =   0  'Transparent
            Caption         =   "South Highschool (London ON, Canada)"
            BeginProperty Font 
               Name            =   "MS Serif"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0080FF80&
            Height          =   240
            Index           =   2
            Left            =   60
            TabIndex        =   17
            Top             =   5835
            Width           =   3075
         End
         Begin VB.Label lblCreds 
            Alignment       =   2  'Center
            BackStyle       =   0  'Transparent
            Caption         =   "Special Thanks to"
            BeginProperty Font 
               Name            =   "MS Serif"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0080FF80&
            Height          =   240
            Index           =   1
            Left            =   960
            TabIndex        =   16
            Top             =   5535
            Width           =   1275
         End
         Begin VB.Label lblCreds 
            Alignment       =   2  'Center
            BackStyle       =   0  'Transparent
            Caption         =   "Programming and Graphics by Tim Miron and Richard Pieters"
            BeginProperty Font 
               Name            =   "MS Serif"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0080FF80&
            Height          =   465
            Index           =   0
            Left            =   600
            TabIndex        =   6
            Top             =   4860
            Width           =   1950
         End
         Begin VB.Label lblCap2 
            Alignment       =   2  'Center
            BackStyle       =   0  'Transparent
            Caption         =   $"frmHelp.frx":559D
            BeginProperty Font 
               Name            =   "Arial"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0080FF80&
            Height          =   2400
            Left            =   105
            TabIndex        =   5
            Top             =   1725
            Width           =   3030
         End
         Begin VB.Label Label1 
            Alignment       =   2  'Center
            BackStyle       =   0  'Transparent
            Caption         =   "In the year 2320, the ancient sport of battling robots has become the favorite pastime of people everywhere…"
            BeginProperty Font 
               Name            =   "Haettenschweiler"
               Size            =   14.25
               Charset         =   0
               Weight          =   500
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0080FF80&
            Height          =   1665
            Left            =   -15
            TabIndex        =   4
            Top             =   0
            Width           =   2790
         End
      End
   End
   Begin VB.PictureBox picControls 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   4245
      Left            =   4980
      ScaleHeight     =   283
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   303
      TabIndex        =   0
      Top             =   2775
      Width           =   4545
   End
   Begin VB.Label cmdBut1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "BotMatch Homepage"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   14.25
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   360
      Index           =   1
      Left            =   6450
      TabIndex        =   14
      Top             =   1230
      Width           =   1935
   End
   Begin VB.Label cmdBut1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Exit Help"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   14.25
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   360
      Index           =   2
      Left            =   6480
      TabIndex        =   13
      Top             =   1845
      Width           =   1860
   End
   Begin VB.Label cmdBut1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "yarinteractive.com"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   14.25
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   360
      Index           =   0
      Left            =   6480
      TabIndex        =   15
      Top             =   600
      Width           =   1860
   End
   Begin VB.Shape shpBorder 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00004000&
      BorderWidth     =   2
      Height          =   480
      Index           =   0
      Left            =   6390
      Shape           =   4  'Rounded Rectangle
      Top             =   555
      Width           =   2070
   End
   Begin VB.Shape shpBorder 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00004000&
      BorderWidth     =   2
      Height          =   480
      Index           =   1
      Left            =   6390
      Shape           =   4  'Rounded Rectangle
      Top             =   1170
      Width           =   2070
   End
   Begin VB.Shape shpBorder 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00004000&
      BorderWidth     =   2
      Height          =   480
      Index           =   2
      Left            =   6390
      Shape           =   4  'Rounded Rectangle
      Top             =   1785
      Width           =   2070
   End
   Begin VB.Line Line3 
      BorderColor     =   &H00008000&
      X1              =   506
      X2              =   506
      Y1              =   160
      Y2              =   186
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00008000&
      X1              =   386
      X2              =   282
      Y1              =   87
      Y2              =   87
   End
   Begin VB.Shape Shape1 
      BorderColor     =   &H0000FF00&
      FillColor       =   &H00002000&
      FillStyle       =   6  'Cross
      Height          =   2010
      Left            =   5805
      Shape           =   4  'Rounded Rectangle
      Top             =   390
      Width           =   3195
   End
   Begin VB.Label lblTitle 
      BackStyle       =   0  'Transparent
      Caption         =   "BotMatch - Game Help Screen"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   24
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0C0C0&
      Height          =   510
      Left            =   180
      TabIndex        =   1
      Top             =   105
      Width           =   4260
   End
End
Attribute VB_Name = "frmHelp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Copyright 2001 - yar interactive
'game help form...

Private Sub cmdBut1_Click(Index As Integer)
Select Case Index
 Case 0
 Shell "Start http://www.yarinteractive.com" 'goto website
 Case 1
 Shell "Start http://www.yarinteractive.com"
 Case 2
  frmHelp.Hide 'exit help button
  Unload frmHelp
End Select
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
ResHLPCmds 'erset button colors
End Sub

Private Sub picControls_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
ResHLPCmds 'reset button colors
End Sub

Private Sub Timer1_Timer()
'scroll credits
Call DoScroll
Timer1.Enabled = True
End Sub

Public Sub DoScroll()
Do
DoEvents
picScroller.Top = picScroller.Top - 1
Sleep 75
Loop Until picScroller.Top < -544
End Sub

Private Sub cmdBut1_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
If cmdBut1(Index).ForeColor = &HC000& Then
 Call ResHLPCmds
shpBorder(Index).BorderColor = &HFF00&
cmdBut1(Index).ForeColor = &H80FF80
End If
End Sub

Public Sub ResHLPCmds()
Dim I As Byte
For I = 0 To 2
cmdBut1(I).ForeColor = &HC000&
shpBorder(I).BorderColor = &H4000&
Next I
End Sub
