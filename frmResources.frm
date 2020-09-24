VERSION 5.00
Begin VB.Form frmRes 
   BackColor       =   &H00404040&
   ClientHeight    =   8010
   ClientLeft      =   165
   ClientTop       =   450
   ClientWidth     =   10770
   HasDC           =   0   'False
   LinkTopic       =   "Form1"
   ScaleHeight     =   534
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   718
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox SCDbuffer2 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   240
      Left            =   1200
      ScaleHeight     =   240
      ScaleWidth      =   720
      TabIndex        =   35
      Top             =   7140
      Width           =   720
   End
   Begin VB.PictureBox SCDbuffer1 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   240
      Left            =   1200
      ScaleHeight     =   240
      ScaleWidth      =   720
      TabIndex        =   34
      Top             =   6825
      Width           =   720
   End
   Begin VB.PictureBox picGameHelp 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
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
      ForeColor       =   &H0000FF00&
      Height          =   4245
      Left            =   5445
      Picture         =   "frmResources.frx":0000
      ScaleHeight     =   283
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   303
      TabIndex        =   25
      Top             =   1890
      Width           =   4545
   End
   Begin VB.PictureBox picShields 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   960
      Left            =   3555
      Picture         =   "frmResources.frx":1598
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   256
      TabIndex        =   33
      Top             =   6000
      Width           =   3840
   End
   Begin VB.PictureBox picNull 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   15
      Left            =   450
      ScaleHeight     =   15
      ScaleWidth      =   15
      TabIndex        =   32
      Top             =   645
      Width           =   15
   End
   Begin VB.PictureBox picLazI 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   480
      Left            =   1935
      Picture         =   "frmResources.frx":1CFF
      ScaleHeight     =   32
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   32
      TabIndex        =   31
      Top             =   4275
      Width           =   480
   End
   Begin VB.PictureBox picRetroF1 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   960
      Left            =   1575
      Picture         =   "frmResources.frx":2941
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   513
      TabIndex        =   30
      Top             =   870
      Width           =   7695
   End
   Begin VB.PictureBox picFPS 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
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
      ForeColor       =   &H0080FF80&
      Height          =   240
      Left            =   3540
      ScaleHeight     =   16
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   60
      TabIndex        =   29
      Top             =   1740
      Width           =   900
   End
   Begin VB.PictureBox picGo 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial Black"
         Size            =   36
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1020
      Left            =   3285
      ScaleHeight     =   68
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   94
      TabIndex        =   28
      Top             =   1905
      Width           =   1410
   End
   Begin VB.PictureBox picPlasmaSpark 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   960
      Left            =   2520
      Picture         =   "frmResources.frx":7510
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   64
      TabIndex        =   27
      Top             =   3780
      Width           =   960
   End
   Begin VB.PictureBox picSpark 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   240
      Left            =   2400
      Picture         =   "frmResources.frx":7B60
      ScaleHeight     =   16
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   16
      TabIndex        =   26
      Top             =   210
      Width           =   240
   End
   Begin VB.PictureBox picPF4 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   195
      Left            =   300
      Picture         =   "frmResources.frx":7EA2
      ScaleHeight     =   13
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   138
      TabIndex        =   24
      Top             =   3750
      Width           =   2070
   End
   Begin VB.PictureBox picP2Frags 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   18
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   390
      Left            =   4530
      ScaleHeight     =   26
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   134
      TabIndex        =   23
      Top             =   5400
      Width           =   2010
   End
   Begin VB.PictureBox picP1Frags 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Haettenschweiler"
         Size            =   18
         Charset         =   0
         Weight          =   500
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   390
      Left            =   3330
      ScaleHeight     =   26
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   134
      TabIndex        =   22
      Top             =   4245
      Width           =   2010
   End
   Begin VB.PictureBox picPAK 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   195
      Left            =   5055
      Picture         =   "frmResources.frx":82DA
      ScaleHeight     =   13
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   145
      TabIndex        =   21
      Top             =   540
      Width           =   2175
   End
   Begin VB.PictureBox picCS 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   210
      Left            =   5205
      Picture         =   "frmResources.frx":871F
      ScaleHeight     =   14
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   143
      TabIndex        =   20
      Top             =   3465
      Width           =   2145
   End
   Begin VB.PictureBox picP2Health 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   420
      Left            =   60
      ScaleHeight     =   28
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   61
      TabIndex        =   18
      Top             =   2745
      Width           =   915
   End
   Begin VB.PictureBox picP1Health 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   420
      Left            =   5130
      ScaleHeight     =   28
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   61
      TabIndex        =   17
      Top             =   1935
      Width           =   915
   End
   Begin VB.PictureBox picP2Name 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Fixedsys"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   315
      Left            =   5400
      ScaleHeight     =   21
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   99
      TabIndex        =   16
      Top             =   2730
      Width           =   1485
   End
   Begin VB.PictureBox picP1Name 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Fixedsys"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   315
      Left            =   6315
      ScaleHeight     =   21
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   99
      TabIndex        =   15
      Top             =   2340
      Width           =   1485
   End
   Begin VB.PictureBox PlasmaB 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   960
      Index           =   1
      Left            =   5160
      Picture         =   "frmResources.frx":9F01
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   64
      TabIndex        =   14
      Top             =   3795
      Width           =   960
   End
   Begin VB.PictureBox PlasmaB 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   960
      Index           =   0
      Left            =   6150
      Picture         =   "frmResources.frx":CF43
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   64
      TabIndex        =   13
      Top             =   3795
      Width           =   960
   End
   Begin VB.PictureBox cgBullet 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00C0FFFF&
      BorderStyle     =   0  'None
      Height          =   240
      Index           =   1
      Left            =   5970
      ScaleHeight     =   16
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   1
      TabIndex        =   12
      Top             =   450
      Width           =   15
   End
   Begin VB.PictureBox cgBullet 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00C0FFFF&
      BorderStyle     =   0  'None
      Height          =   240
      Index           =   0
      Left            =   4905
      ScaleHeight     =   16
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   1
      TabIndex        =   11
      Top             =   480
      Width           =   15
   End
   Begin VB.PictureBox picWeapon 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   585
      Index           =   3
      Left            =   5100
      Picture         =   "frmResources.frx":FF85
      ScaleHeight     =   39
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   121
      TabIndex        =   10
      Top             =   1020
      Width           =   1815
   End
   Begin VB.PictureBox picWeapon 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   645
      Index           =   2
      Left            =   3840
      Picture         =   "frmResources.frx":1373B
      ScaleHeight     =   43
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   72
      TabIndex        =   9
      Top             =   1095
      Width           =   1080
   End
   Begin VB.PictureBox picWeapon 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   585
      Index           =   1
      Left            =   3690
      Picture         =   "frmResources.frx":15BC5
      ScaleHeight     =   39
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   97
      TabIndex        =   8
      Top             =   870
      Width           =   1455
   End
   Begin VB.PictureBox picWeapon 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   435
      Index           =   0
      Left            =   5040
      Picture         =   "frmResources.frx":18883
      ScaleHeight     =   29
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   85
      TabIndex        =   7
      Top             =   90
      Width           =   1275
   End
   Begin VB.PictureBox RPanel 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   1185
      Left            =   390
      Picture         =   "frmResources.frx":1A5C5
      ScaleHeight     =   79
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   139
      TabIndex        =   6
      Top             =   1470
      Width           =   2085
   End
   Begin VB.PictureBox BPanel 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   1185
      Left            =   1050
      Picture         =   "frmResources.frx":1D9FF
      ScaleHeight     =   79
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   139
      TabIndex        =   5
      Top             =   765
      Width           =   2085
   End
   Begin VB.PictureBox ybolt 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   405
      Index           =   1
      Left            =   2580
      Picture         =   "frmResources.frx":20DB2
      ScaleHeight     =   27
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   2
      TabIndex        =   4
      Top             =   330
      Width           =   30
   End
   Begin VB.PictureBox ybolt 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   405
      Index           =   0
      Left            =   1530
      Picture         =   "frmResources.frx":20ECC
      ScaleHeight     =   27
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   2
      TabIndex        =   3
      Top             =   270
      Width           =   30
   End
   Begin VB.PictureBox CMask 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   480
      Left            =   1845
      Picture         =   "frmResources.frx":20FE6
      ScaleHeight     =   32
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   32
      TabIndex        =   2
      Top             =   285
      Width           =   480
   End
   Begin VB.PictureBox CRed 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   480
      Left            =   1245
      Picture         =   "frmResources.frx":21C28
      ScaleHeight     =   32
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   32
      TabIndex        =   1
      Top             =   255
      Width           =   480
   End
   Begin VB.PictureBox CBlue 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   480
      Left            =   645
      Picture         =   "frmResources.frx":2286A
      ScaleHeight     =   32
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   32
      TabIndex        =   0
      Top             =   210
      Width           =   480
   End
   Begin VB.PictureBox picBGgrid 
      AutoSize        =   -1  'True
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      Height          =   6600
      Left            =   585
      Picture         =   "frmResources.frx":234AC
      ScaleHeight     =   440
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   600
      TabIndex        =   19
      Top             =   15
      Width           =   9000
   End
End
Attribute VB_Name = "frmRes"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Copyright 2001 - yar interactive

'this form holds all the game resources
'(images, buffers, ect.)
