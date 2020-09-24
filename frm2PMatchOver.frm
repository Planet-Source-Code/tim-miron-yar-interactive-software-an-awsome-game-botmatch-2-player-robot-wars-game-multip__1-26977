VERSION 5.00
Begin VB.Form frm2PMatchOver 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "BotMatch - Match Over"
   ClientHeight    =   7200
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   9600
   ForeColor       =   &H00000000&
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   480
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   250
      Left            =   525
      Top             =   255
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      ForeColor       =   &H00002000&
      Height          =   150
      Index           =   12
      Left            =   2280
      ScaleHeight     =   150
      ScaleWidth      =   5040
      TabIndex        =   52
      Top             =   6150
      Visible         =   0   'False
      Width           =   5040
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   10
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   17
      Top             =   5325
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   9
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   16
      Top             =   4875
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   8
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   15
      Top             =   4425
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   7
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   14
      Top             =   3975
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   6
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   13
      Top             =   3525
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   5
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   12
      Top             =   3075
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   4
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   11
      Top             =   2625
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   3
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   10
      Top             =   2175
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   2
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   9
      Top             =   1725
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   1
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   8
      Top             =   1275
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   0
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   7
      Top             =   825
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   11
      Left            =   4748
      ScaleHeight     =   345
      ScaleWidth      =   105
      TabIndex        =   6
      Top             =   5775
      Visible         =   0   'False
      Width           =   105
   End
   Begin VB.Timer tmrEffects 
      Interval        =   100
      Left            =   9255
      Top             =   5340
   End
   Begin VB.PictureBox picP2Stats 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   5400
      Left            =   4950
      ScaleHeight     =   5400
      ScaleWidth      =   4500
      TabIndex        =   1
      Top             =   780
      Width           =   4500
      Begin VB.Label lblP2Balance 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   285
         Left            =   2100
         TabIndex        =   51
         Top             =   4875
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   23
         Visible         =   0   'False
         X1              =   1710
         X2              =   2820
         Y1              =   3600
         Y2              =   3600
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   22
         Visible         =   0   'False
         X1              =   1560
         X2              =   2820
         Y1              =   3285
         Y2              =   3285
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   21
         Visible         =   0   'False
         X1              =   1845
         X2              =   2820
         Y1              =   2865
         Y2              =   2865
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   20
         Visible         =   0   'False
         X1              =   1725
         X2              =   2820
         Y1              =   2535
         Y2              =   2535
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   15
         Visible         =   0   'False
         X1              =   2145
         X2              =   2820
         Y1              =   2025
         Y2              =   2025
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   14
         Visible         =   0   'False
         X1              =   1320
         X2              =   2820
         Y1              =   1365
         Y2              =   1365
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   10
         Visible         =   0   'False
         X1              =   810
         X2              =   2820
         Y1              =   1710
         Y2              =   1710
      End
      Begin VB.Label lblShots 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   1
         Left            =   2865
         TabIndex        =   49
         Top             =   1215
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblHits 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   1
         Left            =   2865
         TabIndex        =   48
         Top             =   1560
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblAccuracy 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   1
         Left            =   2865
         TabIndex        =   47
         Top             =   1890
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblDI 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   1
         Left            =   2865
         TabIndex        =   46
         Top             =   2370
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblDS 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   1
         Left            =   2865
         TabIndex        =   45
         Top             =   2700
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblVB 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   1
         Left            =   2865
         TabIndex        =   44
         Top             =   3120
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblAB 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   1
         Left            =   2865
         TabIndex        =   43
         Top             =   3435
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblCE 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   14.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0080FF80&
         Height          =   330
         Index           =   1
         Left            =   2310
         TabIndex        =   42
         Top             =   3900
         Visible         =   0   'False
         Width           =   75
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00008000&
         Index           =   9
         Visible         =   0   'False
         X1              =   420
         X2              =   4095
         Y1              =   3765
         Y2              =   3765
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Total Credits Earned -"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FF00&
         Height          =   255
         Index           =   19
         Left            =   390
         TabIndex        =   33
         Top             =   3915
         Visible         =   0   'False
         Width           =   1680
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Accuracy Bonus"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   18
         Left            =   390
         TabIndex        =   32
         Top             =   3435
         Visible         =   0   'False
         Width           =   1305
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Victory Bonus"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   17
         Left            =   390
         TabIndex        =   31
         Top             =   3120
         Visible         =   0   'False
         Width           =   1125
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00008000&
         Index           =   8
         Visible         =   0   'False
         X1              =   420
         X2              =   4095
         Y1              =   2280
         Y2              =   2280
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Damage Sustained"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   16
         Left            =   390
         TabIndex        =   30
         Top             =   2700
         Visible         =   0   'False
         Width           =   1425
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Damage Credits"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   15
         Left            =   390
         TabIndex        =   29
         Top             =   2370
         Visible         =   0   'False
         Width           =   1305
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Accuracy Percentage"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   14
         Left            =   390
         TabIndex        =   28
         Top             =   1875
         Visible         =   0   'False
         Width           =   1710
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Hits"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   13
         Left            =   390
         TabIndex        =   27
         Top             =   1545
         Visible         =   0   'False
         Width           =   345
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Shots Fired"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   12
         Left            =   390
         TabIndex        =   26
         Top             =   1215
         Visible         =   0   'False
         Width           =   915
      End
      Begin VB.Line linDec 
         BorderColor     =   &H0000C000&
         Index           =   3
         Visible         =   0   'False
         X1              =   645
         X2              =   855
         Y1              =   4980
         Y2              =   4980
      End
      Begin VB.Label lblDead1 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Credits:"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   18
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   390
         Index           =   3
         Left            =   900
         TabIndex        =   5
         Top             =   4770
         Visible         =   0   'False
         Width           =   900
      End
      Begin VB.Line linDec 
         BorderColor     =   &H0000C000&
         Index           =   1
         Visible         =   0   'False
         X1              =   645
         X2              =   645
         Y1              =   4410
         Y2              =   4995
      End
      Begin VB.Shape Shape4 
         BorderColor     =   &H0000C000&
         FillStyle       =   0  'Solid
         Height          =   3315
         Left            =   165
         Shape           =   4  'Rounded Rectangle
         Top             =   1110
         Width           =   4170
      End
      Begin VB.Label lblP2Name 
         Alignment       =   2  'Center
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial Black"
            Size            =   18
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FF00&
         Height          =   495
         Left            =   2025
         TabIndex        =   3
         Top             =   195
         Visible         =   0   'False
         Width           =   120
      End
      Begin VB.Shape Shape2 
         BorderColor     =   &H00008000&
         FillColor       =   &H00002000&
         Height          =   5400
         Left            =   -480
         Shape           =   4  'Rounded Rectangle
         Top             =   0
         Width           =   4500
      End
   End
   Begin VB.PictureBox picP1Stats 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   5400
      Left            =   150
      ScaleHeight     =   5400
      ScaleWidth      =   4500
      TabIndex        =   0
      Top             =   780
      Width           =   4500
      Begin VB.Label lblP1Balance 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   285
         Left            =   2100
         TabIndex        =   50
         Top             =   4875
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   19
         Visible         =   0   'False
         X1              =   1710
         X2              =   2820
         Y1              =   3600
         Y2              =   3600
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   18
         Visible         =   0   'False
         X1              =   1560
         X2              =   2820
         Y1              =   3285
         Y2              =   3285
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   17
         Visible         =   0   'False
         X1              =   1845
         X2              =   2820
         Y1              =   2865
         Y2              =   2865
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   16
         Visible         =   0   'False
         X1              =   1725
         X2              =   2820
         Y1              =   2535
         Y2              =   2535
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   13
         Visible         =   0   'False
         X1              =   2145
         X2              =   2820
         Y1              =   2025
         Y2              =   2025
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   12
         Visible         =   0   'False
         X1              =   1320
         X2              =   2820
         Y1              =   1365
         Y2              =   1365
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         BorderStyle     =   3  'Dot
         Index           =   11
         Visible         =   0   'False
         X1              =   810
         X2              =   2820
         Y1              =   1710
         Y2              =   1710
      End
      Begin VB.Label lblCE 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   14.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0080FF80&
         Height          =   330
         Index           =   0
         Left            =   2310
         TabIndex        =   41
         Top             =   3900
         Visible         =   0   'False
         Width           =   75
      End
      Begin VB.Label lblAB 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   0
         Left            =   2865
         TabIndex        =   40
         Top             =   3435
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblVB 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   0
         Left            =   2865
         TabIndex        =   39
         Top             =   3120
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblDS 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   0
         Left            =   2865
         TabIndex        =   38
         Top             =   2700
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblDI 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   0
         Left            =   2865
         TabIndex        =   37
         Top             =   2370
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblAccuracy 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   0
         Left            =   2865
         TabIndex        =   36
         Top             =   1890
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblHits 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   0
         Left            =   2865
         TabIndex        =   35
         Top             =   1560
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Label lblShots 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0FFC0&
         Height          =   285
         Index           =   0
         Left            =   2865
         TabIndex        =   34
         Top             =   1215
         Visible         =   0   'False
         Width           =   60
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00008000&
         Index           =   7
         Visible         =   0   'False
         X1              =   420
         X2              =   4095
         Y1              =   3765
         Y2              =   3765
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Total Credits Earned -"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FF00&
         Height          =   255
         Index           =   11
         Left            =   390
         TabIndex        =   25
         Top             =   3915
         Visible         =   0   'False
         Width           =   1680
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Accuracy Bonus"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   10
         Left            =   390
         TabIndex        =   24
         Top             =   3435
         Visible         =   0   'False
         Width           =   1305
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Victory Bonus"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   9
         Left            =   390
         TabIndex        =   23
         Top             =   3120
         Visible         =   0   'False
         Width           =   1125
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00008000&
         Index           =   6
         Visible         =   0   'False
         X1              =   420
         X2              =   4095
         Y1              =   2280
         Y2              =   2280
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Damage Sustained"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   8
         Left            =   390
         TabIndex        =   22
         Top             =   2700
         Visible         =   0   'False
         Width           =   1425
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Damage Credits"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   7
         Left            =   390
         TabIndex        =   21
         Top             =   2370
         Visible         =   0   'False
         Width           =   1305
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Accuracy Percentage"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   6
         Left            =   390
         TabIndex        =   20
         Top             =   1875
         Visible         =   0   'False
         Width           =   1710
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Hits"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   5
         Left            =   390
         TabIndex        =   19
         Top             =   1545
         Visible         =   0   'False
         Width           =   345
      End
      Begin VB.Label lblDead1 
         BackStyle       =   0  'Transparent
         Caption         =   "Shots Fired"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   12
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   255
         Index           =   4
         Left            =   390
         TabIndex        =   18
         Top             =   1215
         Visible         =   0   'False
         Width           =   915
      End
      Begin VB.Line linDec 
         BorderColor     =   &H0000C000&
         Index           =   2
         Visible         =   0   'False
         X1              =   645
         X2              =   855
         Y1              =   4980
         Y2              =   4980
      End
      Begin VB.Label lblDead1 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Credits:"
         BeginProperty Font 
            Name            =   "Haettenschweiler"
            Size            =   18
            Charset         =   0
            Weight          =   500
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   390
         Index           =   2
         Left            =   900
         TabIndex        =   4
         Top             =   4770
         Visible         =   0   'False
         Width           =   900
      End
      Begin VB.Line linDec 
         BorderColor     =   &H0000C000&
         Index           =   0
         Visible         =   0   'False
         X1              =   645
         X2              =   645
         Y1              =   4410
         Y2              =   4995
      End
      Begin VB.Shape Shape3 
         BackColor       =   &H00000000&
         BorderColor     =   &H0000C000&
         FillStyle       =   0  'Solid
         Height          =   3315
         Left            =   180
         Shape           =   4  'Rounded Rectangle
         Top             =   1110
         Width           =   4170
      End
      Begin VB.Label lblP1Name 
         Alignment       =   2  'Center
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "Arial Black"
            Size            =   18
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FF00&
         Height          =   495
         Left            =   2415
         TabIndex        =   2
         Top             =   195
         Visible         =   0   'False
         Width           =   120
      End
      Begin VB.Shape Shape1 
         BorderColor     =   &H00008000&
         FillColor       =   &H00002000&
         Height          =   5400
         Left            =   480
         Shape           =   4  'Rounded Rectangle
         Top             =   0
         Width           =   4500
      End
   End
   Begin VB.Shape shpLP 
      BorderColor     =   &H00008000&
      Height          =   1020
      Left            =   1005
      Shape           =   4  'Rounded Rectangle
      Top             =   6405
      Visible         =   0   'False
      Width           =   7590
   End
   Begin VB.Label cmdBut1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Exit"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   15.75
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   390
      Index           =   2
      Left            =   6195
      TabIndex        =   55
      Top             =   6585
      Visible         =   0   'False
      Width           =   1860
   End
   Begin VB.Shape shpBorder 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00004000&
      BorderWidth     =   2
      Height          =   480
      Index           =   2
      Left            =   6180
      Shape           =   4  'Rounded Rectangle
      Top             =   6540
      Visible         =   0   'False
      Width           =   1920
   End
   Begin VB.Label cmdBut1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Play Again"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   15.75
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   390
      Index           =   1
      Left            =   1560
      TabIndex        =   54
      Top             =   6585
      Visible         =   0   'False
      Width           =   2085
   End
   Begin VB.Shape shpBorder 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00004000&
      BorderWidth     =   2
      Height          =   480
      Index           =   1
      Left            =   1500
      Shape           =   4  'Rounded Rectangle
      Top             =   6540
      Visible         =   0   'False
      Width           =   2145
   End
   Begin VB.Label cmdBut1 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "New Game"
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   15.75
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   375
      Index           =   0
      Left            =   4020
      TabIndex        =   53
      Top             =   6585
      Visible         =   0   'False
      Width           =   1815
   End
   Begin VB.Shape shpBorder 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00004000&
      BorderWidth     =   2
      Height          =   480
      Index           =   0
      Left            =   3960
      Shape           =   4  'Rounded Rectangle
      Top             =   6540
      Visible         =   0   'False
      Width           =   1920
   End
   Begin VB.Line linDec 
      BorderColor     =   &H00008000&
      Index           =   24
      Visible         =   0   'False
      X1              =   310
      X2              =   332
      Y1              =   52
      Y2              =   52
   End
   Begin VB.Line linDec 
      BorderColor     =   &H0000C000&
      Index           =   5
      Visible         =   0   'False
      X1              =   488
      X2              =   488
      Y1              =   401
      Y2              =   420
   End
   Begin VB.Line linDec 
      BorderColor     =   &H0000C000&
      Index           =   4
      Visible         =   0   'False
      X1              =   151
      X2              =   151
      Y1              =   401
      Y2              =   420
   End
End
Attribute VB_Name = "frm2PMatchOver"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Copyright 2001 - yar interactive

'form shows damage done, match stats such as accuracy
'shots fired, credits earned etc...


Private CurFlasher As Integer ' current flasher - ID

Private Sub cmdBut1_Click(Index As Integer)
Dim c As Byte
Select Case Index
Case 2
       MsgStat = 5 'prompt for exit...
       ShowMsg "You are about to exit the program", "BotMatch - Exit Game?"
Case 1
       frm2PMatchOver.Hide
       If GridOn = True Then
        
        With frmMain
          .Refresh
          .Cls
          .Picture = frmRes.picBGgrid.Picture
        End With
       End If
       
       P1CurFrags = 0
       P2CurFrags = 0
       P1HitsC = 0
       P1ShotsC = 0
       P2HitsC = 0
       P2ShotsC = 0
       
       For c = 0 To NumOfBullets
       P1Bullet(c).Active = False
       P2Bullet(c).Active = False
       Next c
       
       Unload frm2PMatchOver
       
       frmMain.Show
       frmMain.InitNewGame
Case 0

     Call ResetDefs 'reset default variables
     
       Load frmSetup
       Unload frmMain
       Unload frmMsg
       Unload frm2PMatchOver
       frmSetup.Show
End Select
End Sub

Private Sub cmdBut1_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
If cmdBut1(Index).ForeColor = &HC000& Then
 Call ResMECmds
shpBorder(Index).BorderColor = &HFF00&
cmdBut1(Index).ForeColor = &H80FF80
End If
End Sub

Private Sub Form_Load()
picP1Stats.Top = -360
picP2Stats.Top = 480
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
ResMECmds
End Sub

Private Sub picP1Stats_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
ResMECmds
End Sub

Private Sub Timer1_Timer()
Timer1.Enabled = False
Call SlidePans 'slide panels in...
End Sub

Private Sub tmrEffects_Timer()
'light effect at the bottom of the setup screen
On Error Resume Next
If CurFlasher = 17 Then CurFlasher = -4
    picFlasher(CurFlasher - 4).BackColor = 8192
    picFlasher(CurFlasher - 3).BackColor = 16384
    picFlasher(CurFlasher - 2).BackColor = 32768
    picFlasher(CurFlasher - 1).BackColor = 49152
    picFlasher(CurFlasher - 5).BackColor = 8192
    picFlasher(CurFlasher + 5).BackColor = 8192
    picFlasher(CurFlasher + 4).BackColor = 8192
    picFlasher(CurFlasher + 3).BackColor = 16384
    picFlasher(CurFlasher + 2).BackColor = 32768
    picFlasher(CurFlasher + 1).BackColor = 49152
    picFlasher(CurFlasher).BackColor = 65280
    CurFlasher = CurFlasher + 1
End Sub

Public Sub MakeStatsVis()
'make objects visble after
'panels slide in
On Error Resume Next
Dim FPic As Byte 'flash picture index

For FPic = 0 To 12
 picFlasher(FPic).Visible = True
Next FPic

FPic = 0
For FPic = 0 To 24
     linDec(FPic).Visible = True
Next FPic

    Shape1.FillStyle = 6
    Shape2.FillStyle = 6
        lblP1Name.Visible = True
        lblP2Name.Visible = True
    FPic = 0
For FPic = 0 To 19
  lblDead1(FPic).Visible = True
Next FPic

FPic = 0
For FPic = 0 To 1
 lblShots(FPic).Visible = True
 lblHits(FPic).Visible = True
 lblAccuracy(FPic).Visible = True
 lblDI(FPic).Visible = True
 lblDS(FPic).Visible = True
 lblVB(FPic).Visible = True
 lblAB(FPic).Visible = True
 lblCE(FPic).Visible = True
Next FPic

FPic = 0
For FPic = 0 To 2
 cmdBut1(FPic).Visible = True
 shpBorder(FPic).Visible = True
Next FPic
        lblP1Balance.Visible = True
        lblP2Balance.Visible = True
        
        shpLP.Visible = True
End Sub

Public Sub ResMECmds()
Dim I As Byte
For I = 0 To 2
cmdBut1(I).ForeColor = &HC000&
shpBorder(I).BorderColor = &H4000&
Next I
End Sub

Public Sub SlidePans()
Do 'slide panels in procedure...
DoEvents
Sleep 5
With picP2Stats
If .Top > 52 Then .Top = .Top - 2
End With

With picP1Stats
If .Top < 52 Then .Top = .Top + 2
End With
If picP2Stats.Top = 52 And picP1Stats.Top = 52 Then
MakeStatsVis
Exit Sub
End If
Loop
End Sub
