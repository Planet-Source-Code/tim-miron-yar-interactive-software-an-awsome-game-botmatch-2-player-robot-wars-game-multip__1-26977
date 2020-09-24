VERSION 5.00
Begin VB.Form frmSetup 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "BotMatch - Game Options"
   ClientHeight    =   7200
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   9600
   FillColor       =   &H00004000&
   ForeColor       =   &H00000000&
   HasDC           =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   480
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picAdOps 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      HasDC           =   0   'False
      Height          =   2775
      Left            =   2963
      Picture         =   "frmSetup.frx":0000
      ScaleHeight     =   2775
      ScaleWidth      =   3675
      TabIndex        =   42
      Top             =   2213
      Visible         =   0   'False
      Width           =   3675
      Begin VB.OptionButton OPcG 
         BackColor       =   &H00000000&
         Caption         =   "Custom"
         ForeColor       =   &H0000C000&
         Height          =   225
         Left            =   2535
         TabIndex        =   54
         Top             =   2280
         Width           =   825
      End
      Begin VB.OptionButton OPLowG 
         BackColor       =   &H00000000&
         Caption         =   "Low Grx."
         ForeColor       =   &H0000C000&
         Height          =   225
         Left            =   2310
         TabIndex        =   53
         Top             =   2040
         Value           =   -1  'True
         Width           =   1005
      End
      Begin VB.OptionButton opHighG 
         BackColor       =   &H00000000&
         Caption         =   "High Grx."
         ForeColor       =   &H0000C000&
         Height          =   225
         Left            =   2055
         TabIndex        =   52
         Top             =   1815
         Width           =   1290
      End
      Begin VB.CheckBox chkHGT 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "High Graphical Text Effects"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   -1  'True
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   510
         TabIndex        =   51
         Top             =   1515
         Width           =   2280
      End
      Begin VB.CheckBox chkRetro 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Show Booster Flame"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   -1  'True
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   510
         TabIndex        =   49
         Top             =   1275
         Width           =   1830
      End
      Begin VB.CheckBox chkHUD 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Display HUD"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   -1  'True
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   510
         TabIndex        =   46
         Top             =   1035
         Value           =   1  'Checked
         Width           =   1230
      End
      Begin VB.CheckBox chkFPS 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Show Frames Per Second (FPS)"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   -1  'True
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   510
         TabIndex        =   45
         Top             =   795
         Width           =   2700
      End
      Begin VB.CheckBox chkAutoH 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Auto Health Gain"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   -1  'True
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   510
         TabIndex        =   44
         Top             =   555
         Width           =   1680
      End
      Begin VB.CheckBox chkGrid 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Show Grid"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   -1  'True
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   510
         TabIndex        =   43
         Top             =   315
         Width           =   1170
      End
      Begin VB.Label lblOK 
         Alignment       =   2  'Center
         BackColor       =   &H00404040&
         BackStyle       =   0  'Transparent
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
         Left            =   405
         TabIndex        =   47
         Top             =   2220
         Width           =   870
      End
      Begin VB.Shape shpBorder 
         FillColor       =   &H00404040&
         FillStyle       =   0  'Solid
         Height          =   405
         Left            =   375
         Shape           =   4  'Rounded Rectangle
         Top             =   2190
         Width           =   930
      End
   End
   Begin VB.PictureBox picButs 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      HasDC           =   0   'False
      Height          =   2505
      Left            =   6540
      ScaleHeight     =   2505
      ScaleWidth      =   3000
      TabIndex        =   37
      Top             =   4200
      Width           =   3000
      Begin VB.Label cmdCancel 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "Cancel"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   330
         Left            =   465
         TabIndex        =   41
         Top             =   2010
         Width           =   2055
      End
      Begin VB.Shape shpBut4 
         BorderColor     =   &H00004000&
         BorderWidth     =   2
         FillStyle       =   0  'Solid
         Height          =   450
         Left            =   420
         Shape           =   4  'Rounded Rectangle
         Top             =   1920
         Width           =   2145
      End
      Begin VB.Label cmdHelp 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "Game Help"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   315
         Left            =   465
         TabIndex        =   40
         Top             =   1425
         Width           =   2055
      End
      Begin VB.Shape shpBut3 
         BorderColor     =   &H00004000&
         BorderWidth     =   2
         FillStyle       =   0  'Solid
         Height          =   450
         Left            =   420
         Shape           =   4  'Rounded Rectangle
         Top             =   1335
         Width           =   2145
      End
      Begin VB.Label cmdOpts 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "Advanced Options"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   330
         Left            =   465
         TabIndex        =   39
         Top             =   840
         Width           =   2055
      End
      Begin VB.Shape shpBut2 
         BorderColor     =   &H00004000&
         BorderWidth     =   2
         FillStyle       =   0  'Solid
         Height          =   450
         Left            =   420
         Shape           =   4  'Rounded Rectangle
         Top             =   750
         Width           =   2145
      End
      Begin VB.Label cmdStart 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "Start Game"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   330
         Left            =   465
         TabIndex        =   38
         Top             =   255
         Width           =   2055
      End
      Begin VB.Shape shpBut1 
         BorderColor     =   &H00004000&
         BorderWidth     =   2
         FillStyle       =   0  'Solid
         Height          =   450
         Left            =   420
         Shape           =   4  'Rounded Rectangle
         Top             =   165
         Width           =   2145
      End
      Begin VB.Shape Shape1 
         BorderColor     =   &H0000C000&
         FillColor       =   &H00002000&
         FillStyle       =   6  'Cross
         Height          =   2490
         Left            =   30
         Shape           =   4  'Rounded Rectangle
         Top             =   0
         Width           =   2910
      End
   End
   Begin VB.PictureBox picLblSwitch 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   240
      Left            =   5325
      ScaleHeight     =   240
      ScaleWidth      =   1305
      TabIndex        =   35
      Top             =   2580
      Visible         =   0   'False
      Width           =   1305
      Begin VB.Label lblSwitch 
         BackStyle       =   0  'Transparent
         Caption         =   "Change Colors"
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
         Height          =   225
         Left            =   0
         TabIndex        =   36
         Top             =   0
         Width           =   1335
      End
   End
   Begin VB.PictureBox picLogo 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H00404040&
      BorderStyle     =   0  'None
      HasDC           =   0   'False
      Height          =   1500
      Left            =   0
      Picture         =   "frmSetup.frx":21422
      ScaleHeight     =   1500
      ScaleWidth      =   4740
      TabIndex        =   34
      Top             =   0
      Width           =   4740
   End
   Begin VB.PictureBox picLoading 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   1410
      Left            =   2190
      ScaleHeight     =   1410
      ScaleWidth      =   5220
      TabIndex        =   32
      Top             =   2895
      Width           =   5220
      Begin VB.Label lblLoading 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Loading"
         BeginProperty Font 
            Name            =   "Arial Black"
            Size            =   48
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   1350
         Left            =   0
         TabIndex        =   33
         Top             =   0
         Width           =   4185
      End
   End
   Begin VB.PictureBox picSgreen 
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   240
      Left            =   720
      Picture         =   "frmSetup.frx":24661
      ScaleHeight     =   240
      ScaleWidth      =   480
      TabIndex        =   28
      Top             =   660
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.PictureBox picSwitch 
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   240
      Left            =   4005
      Picture         =   "frmSetup.frx":24CA3
      ScaleHeight     =   240
      ScaleWidth      =   480
      TabIndex        =   23
      Top             =   1440
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.PictureBox picSwitchHL 
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   240
      Left            =   4035
      Picture         =   "frmSetup.frx":252E5
      ScaleHeight     =   240
      ScaleWidth      =   480
      TabIndex        =   22
      Top             =   1425
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   300
      Left            =   5415
      Top             =   5700
   End
   Begin VB.PictureBox picMnu 
      Align           =   1  'Align Top
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      HasDC           =   0   'False
      Height          =   240
      Left            =   0
      ScaleHeight     =   240
      ScaleWidth      =   9600
      TabIndex        =   14
      Top             =   0
      Width           =   9600
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   10
      Left            =   8978
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   12
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   9
      Left            =   8108
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   11
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   8
      Left            =   7238
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   10
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   7
      Left            =   6368
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   9
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   6
      Left            =   5498
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   8
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   5
      Left            =   4628
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   7
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   4
      Left            =   3758
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   6
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   3
      Left            =   2888
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   5
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   2
      Left            =   2018
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   4
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   1
      Left            =   1148
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   3
      Top             =   6825
      Width           =   345
   End
   Begin VB.PictureBox picFlasher 
      BackColor       =   &H00002000&
      BorderStyle     =   0  'None
      Height          =   150
      Index           =   0
      Left            =   278
      ScaleHeight     =   150
      ScaleWidth      =   345
      TabIndex        =   2
      Top             =   6825
      Width           =   345
   End
   Begin VB.Timer tmrEffects 
      Interval        =   100
      Left            =   7725
      Top             =   1935
   End
   Begin VB.PictureBox picSetup 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      HasDC           =   0   'False
      Height          =   3855
      Left            =   180
      ScaleHeight     =   257
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   353
      TabIndex        =   0
      Top             =   1770
      Width           =   5295
      Begin VB.OptionButton optSpeed 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Ultra Slow"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00808080&
         Height          =   345
         Index           =   3
         Left            =   330
         TabIndex        =   48
         Top             =   2475
         Visible         =   0   'False
         Width           =   1335
      End
      Begin VB.TextBox txtFragL 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00404040&
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0080FF80&
         Height          =   405
         Left            =   1500
         MaxLength       =   4
         TabIndex        =   30
         Text            =   "1"
         Top             =   3270
         Visible         =   0   'False
         Width           =   765
      End
      Begin VB.OptionButton optSpeed 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Fast"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00808080&
         Height          =   345
         Index           =   2
         Left            =   4380
         TabIndex        =   26
         Top             =   2475
         Visible         =   0   'False
         Width           =   765
      End
      Begin VB.OptionButton optSpeed 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Medium"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00808080&
         Height          =   345
         Index           =   1
         Left            =   2985
         TabIndex        =   25
         Top             =   2475
         Value           =   -1  'True
         Visible         =   0   'False
         Width           =   1170
      End
      Begin VB.OptionButton optSpeed 
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         Caption         =   "Slow"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00808080&
         Height          =   345
         Index           =   0
         Left            =   1890
         TabIndex        =   24
         Top             =   2475
         Visible         =   0   'False
         Width           =   870
      End
      Begin VB.PictureBox butSwitch 
         AutoSize        =   -1  'True
         BorderStyle     =   0  'None
         Height          =   240
         Left            =   4620
         Picture         =   "frmSetup.frx":25927
         ScaleHeight     =   240
         ScaleWidth      =   480
         TabIndex        =   21
         Top             =   945
         Visible         =   0   'False
         Width           =   480
      End
      Begin VB.PictureBox picP2 
         AutoSize        =   -1  'True
         BorderStyle     =   0  'None
         Height          =   480
         Left            =   3900
         Picture         =   "frmSetup.frx":25F69
         ScaleHeight     =   480
         ScaleWidth      =   480
         TabIndex        =   20
         Top             =   1350
         Visible         =   0   'False
         Width           =   480
      End
      Begin VB.PictureBox picP1 
         AutoSize        =   -1  'True
         BorderStyle     =   0  'None
         Height          =   480
         Left            =   3900
         Picture         =   "frmSetup.frx":26BAB
         ScaleHeight     =   480
         ScaleWidth      =   480
         TabIndex        =   19
         Top             =   285
         Visible         =   0   'False
         Width           =   480
      End
      Begin VB.TextBox txtP2Name 
         Appearance      =   0  'Flat
         BackColor       =   &H00404040&
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
         Height          =   360
         Left            =   1860
         MaxLength       =   8
         TabIndex        =   18
         Top             =   1275
         Visible         =   0   'False
         Width           =   1800
      End
      Begin VB.TextBox txtP1Name 
         Appearance      =   0  'Flat
         BackColor       =   &H00404040&
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
         Height          =   360
         Left            =   1860
         MaxLength       =   8
         TabIndex        =   16
         Top             =   270
         Visible         =   0   'False
         Width           =   1800
      End
      Begin VB.Line linDec 
         BorderColor     =   &H0000C000&
         Index           =   3
         Visible         =   0   'False
         X1              =   324
         X2              =   303
         Y1              =   31
         Y2              =   31
      End
      Begin VB.Line linDec 
         BorderColor     =   &H0000C000&
         Index           =   2
         Visible         =   0   'False
         X1              =   324
         X2              =   303
         Y1              =   110
         Y2              =   110
      End
      Begin VB.Line linDec 
         BorderColor     =   &H0000C000&
         Index           =   1
         Visible         =   0   'False
         X1              =   324
         X2              =   324
         Y1              =   32
         Y2              =   58
      End
      Begin VB.Line linDec 
         BorderColor     =   &H0000C000&
         Index           =   0
         Visible         =   0   'False
         X1              =   324
         X2              =   324
         Y1              =   85
         Y2              =   111
      End
      Begin VB.Label lblFPM 
         BackStyle       =   0  'Transparent
         Caption         =   "(Frags Per Match)"
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
         Height          =   315
         Left            =   2370
         TabIndex        =   31
         Top             =   3330
         Visible         =   0   'False
         Width           =   1785
      End
      Begin VB.Label lblFragL 
         BackStyle       =   0  'Transparent
         Caption         =   "Frag Limit:"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   330
         TabIndex        =   29
         Top             =   3060
         Visible         =   0   'False
         Width           =   1320
      End
      Begin VB.Shape shpGS 
         BorderColor     =   &H0000C000&
         Height          =   1785
         Left            =   0
         Top             =   2070
         Visible         =   0   'False
         Width           =   5295
      End
      Begin VB.Label lblGSpeed 
         BackStyle       =   0  'Transparent
         Caption         =   "Game Speed:"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   330
         TabIndex        =   27
         Top             =   2205
         Visible         =   0   'False
         Width           =   1380
      End
      Begin VB.Shape shpBord2 
         BorderColor     =   &H00E0E0E0&
         Height          =   615
         Left            =   3840
         Top             =   1290
         Visible         =   0   'False
         Width           =   600
      End
      Begin VB.Shape shpBord1 
         BorderColor     =   &H00E0E0E0&
         Height          =   615
         Left            =   3840
         Top             =   225
         Visible         =   0   'False
         Width           =   600
      End
      Begin VB.Label lblP2Name 
         BackStyle       =   0  'Transparent
         Caption         =   "Player 2 Name:"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   285
         TabIndex        =   17
         Top             =   1305
         Visible         =   0   'False
         Width           =   1515
      End
      Begin VB.Label lblP1Name 
         BackStyle       =   0  'Transparent
         Caption         =   "Player 1 Name:"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000C000&
         Height          =   240
         Left            =   285
         TabIndex        =   15
         Top             =   300
         Visible         =   0   'False
         Width           =   1515
      End
      Begin VB.Label lblTitle 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "2 Player - Game Setup and Options"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   -1  'True
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FF00&
         Height          =   270
         Left            =   75
         TabIndex        =   1
         Top             =   -45
         Visible         =   0   'False
         Width           =   3915
      End
   End
   Begin VB.PictureBox picBG 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      HasDC           =   0   'False
      Height          =   5040
      Left            =   3945
      Picture         =   "frmSetup.frx":277ED
      ScaleHeight     =   5040
      ScaleWidth      =   5760
      TabIndex        =   13
      Top             =   255
      Width           =   5760
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         Index           =   6
         Visible         =   0   'False
         X1              =   3795
         X2              =   3795
         Y1              =   2430
         Y2              =   3930
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         Index           =   5
         Visible         =   0   'False
         X1              =   885
         X2              =   3810
         Y1              =   2415
         Y2              =   2415
      End
      Begin VB.Line linDec 
         BorderColor     =   &H00004000&
         Index           =   4
         Visible         =   0   'False
         X1              =   1575
         X2              =   2640
         Y1              =   4995
         Y2              =   4995
      End
   End
   Begin VB.Line linDisp2 
      BorderColor     =   &H0000C000&
      Visible         =   0   'False
      X1              =   37
      X2              =   44
      Y1              =   412
      Y2              =   412
   End
   Begin VB.Line linDisp1 
      BorderColor     =   &H0000C000&
      Visible         =   0   'False
      X1              =   37
      X2              =   37
      Y1              =   411
      Y2              =   373
   End
   Begin VB.Label lblAdvice 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "During game Press F1 for Help"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   285
      Left            =   690
      TabIndex        =   50
      Top             =   6045
      Visible         =   0   'False
      Width           =   3495
   End
End
Attribute VB_Name = "frmSetup"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Copyright 2001 - yar interactive

'2 player keyboard game setup form...
'choose game settings for a 2 player
'battle (keyboard only)

Private CurFlasher As Integer ' current flasher - ID
Private p1team As Byte
Private SwitchState As Byte '0 = normal, 1 = green, 2 = white
Private SlideDir As Byte  'slide diredction - 0 in, 1 out...


Public Sub DrawGrid()
'draws the blue grid with
'a laser-print gradient effect
Dim cX1 As Integer
Dim cX2 As Integer
Dim cY1 As Integer
Dim cY2 As Integer
cX1 = 32
cX2 = 32
cY1 = 0
cY2 = picSetup.ScaleHeight
'With picSetup
Do
DoEvents
picSetup.Refresh
picSetup.Line (cX1 - 64, cY1)-(cX2 - 64, cY2), &H4000&                'green
picSetup.Line (cX1 - 32, cY1)-(cX2 - 32, cY2), &HC000&
picSetup.Line (cX1, cY1)-(cX2, cY2), &HC0FFC0
cX1 = cX1 + 32
cX2 = cX2 + 32
Sleep 50
Loop Until cX1 >= 430




cX1 = 0
cX2 = picSetup.ScaleWidth
cY1 = 32
cY2 = 32

Do
DoEvents
picSetup.Refresh
picSetup.Line (cX1, cY1 - 64)-(cX2, cY2 - 64), &H4000&            'dark green line
picSetup.Line (cX1, cY1 - 32)-(cX2, cY2 - 32), &HC000&            'lighter green
picSetup.Line (cX1, cY1)-(cX2, cY2), &HC0FFC0                     'light green line

cY1 = cY1 + 32
cY2 = cY2 + 32
Sleep 50
Loop Until cY1 >= 328
End Sub

Public Sub MakeVisible()
'make all objects visible after grid is plotted
lblTitle.Visible = True

lblP1Name.Visible = True
lblP2Name.Visible = True
    
    txtP1Name.Visible = True
    txtP2Name.Visible = True
    lblAdvice.Visible = True
    linDisp1.Visible = True
    linDisp2.Visible = True
    shpBord1.Visible = True
    shpBord2.Visible = True
    picP1.Visible = True
    picP2.Visible = True
    butSwitch.Visible = True
        optSpeed(0).Visible = True
        optSpeed(1).Visible = True
        optSpeed(2).Visible = True
        optSpeed(3).Visible = True
            lblGSpeed.Visible = True
            shpGS.Visible = True
            chkGrid.Visible = True
            chkAutoH.Visible = True
                lblFPM.Visible = True
                lblFragL.Visible = True
                txtFragL.Visible = True
            linDec(0).Visible = True
            linDec(1).Visible = True
            linDec(2).Visible = True
            linDec(3).Visible = True
            linDec(4).Visible = True
            linDec(5).Visible = True
            linDec(6).Visible = True
            
              '   tmrSlideInOut.Enabled = True
             Call DoButtSlide
End Sub

Private Sub butSwitch_Click()
Call SwitchTeams
End Sub

Private Sub butSwitch_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
butSwitch.Picture = picSwitchHL.Picture
SwitchState = 2
End Sub

Private Sub butSwitch_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Button > 0 And SwitchState <> 2 Then
 butSwitch.Picture = picSwitchHL.Picture
 SwitchState = 2
 ElseIf Button = 0 And SwitchState <> 1 Then
 butSwitch.Picture = picSgreen.Picture
 SwitchState = 1
 End If
 picLblSwitch.Visible = True
End Sub

Private Sub butSwitch_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
butSwitch.Picture = picSwitch.Picture
SwitchState = 0
End Sub

Private Sub chkAutoH_Click()
OPcG.Value = True
End Sub

Private Sub chkFPS_Click()
OPcG.Value = True
End Sub

Private Sub chkGrid_Click()
OPcG.Value = True
End Sub

'######## HOVER OPTIONS #######
Private Sub chkGrid_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If chkGrid.ForeColor = &HC000& Then
ResOpButs
 chkGrid.ForeColor = &HC0FFC0
End If
End Sub

Private Sub chkHGT_Click()
HGTE = True
OPcG.Value = True
End Sub

Private Sub chkHGT_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If chkHGT.ForeColor = &HC000& Then
ResOpButs
 chkHGT.ForeColor = &HC0FFC0
End If
End Sub

Private Sub chkHUD_Click()
OPcG.Value = True
End Sub

Private Sub chkHUD_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If chkHUD.ForeColor = &HC000& Then
ResOpButs
 chkHUD.ForeColor = &HC0FFC0
End If
End Sub
Private Sub chkAutoH_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If chkAutoH.ForeColor = &HC000& Then
ResOpButs
 chkAutoH.ForeColor = &HC0FFC0
End If
End Sub
Private Sub chkFPS_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If chkFPS.ForeColor = &HC000& Then
ResOpButs
 chkFPS.ForeColor = &HC0FFC0
End If
End Sub

Private Sub chkRetro_Click()
OPcG.Value = True
End Sub

Private Sub chkRetro_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If chkRetro.ForeColor = &HC000& Then
ResOpButs
 chkRetro.ForeColor = &HC0FFC0
End If
End Sub
'###### END HOVER OPS #########

Private Sub cmdCancel_Click()
Call ResetDefs 'reset default variables

Load frmGameType
frmGameType.Show

frmSetup.Hide
Unload frmSetup
Load frmSetup
Unload frmRes
End Sub

Private Sub cmdCancel_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If cmdCancel.ForeColor = &HC000& Then

Call ResSUButs 'refresh all buttons

shpBut4.BorderColor = &HFF00&
cmdCancel.ForeColor = &H80FF80
End If
End Sub

Private Sub cmdHelp_Click()
'show help screen...
With frmHelp
.picControls.Picture = frmRes.picGameHelp.Picture
.Show
End With
End Sub

Private Sub cmdHelp_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If cmdHelp.ForeColor = &HC000& Then

Call ResSUButs 'refresh all buttons

shpBut3.BorderColor = &HFF00&
cmdHelp.ForeColor = &H80FF80
End If
End Sub

Private Sub cmdOpts_Click()
picAdOps.Visible = True
End Sub

Private Sub cmdOpts_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If cmdOpts.ForeColor = &HC000& Then

Call ResSUButs 'refresh all buttons

shpBut2.BorderColor = &HFF00&
cmdOpts.ForeColor = &H80FF80
End If
End Sub

Private Sub cmdStart_Click()
On Error GoTo errout:

If Len(txtP1Name.Text) = 0 Or Len(txtP2Name.Text) = 0 Then
MsgStat = 0
   ShowMsg "Please Input a valid player name...", _
   "BotMatch - Invalid Player Name"
   Exit Sub
End If
FragLimit = txtFragL.Text

SlideDir = 1
'tmrSlideInOut.Enabled = True
Call DoButtSlide

Exit Sub
errout:
   MsgStat = 0
  ShowMsg "Please be sure that the match frag limit is a valid number", "BotMatch - Error"
  Exit Sub
End Sub

Private Sub cmdStart_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If cmdStart.ForeColor = &HC000& Then

Call ResSUButs 'refresh all buttons

shpBut1.BorderColor = &HFF00&
cmdStart.ForeColor = &H80FF80
End If
End Sub

Private Sub Form_Load()
p1team = 0
picButs.Left = 640
End Sub

Private Sub lblOK_Click()
picAdOps.Visible = False
End Sub

Private Sub lblOK_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If shpBorder.FillColor = &H404040 Then
ResOpButs
shpBorder.FillColor = &H8000&
End If
End Sub


Private Sub OPcG_Click()
HGraphics = False
End Sub

Private Sub OPcG_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If OPcG.ForeColor = &HC000& Then
ResOpButs
 OPcG.ForeColor = &HFFFF&
End If
End Sub

Private Sub opHighG_Click()
HGraphics = True
chkGrid.Value = 1
chkAutoH.Value = 0
chkFPS.Value = 0
chkHUD.Value = 1
chkRetro.Value = 1
chkHGT.Value = 1
opHighG.Value = True
End Sub

Private Sub opHighG_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If opHighG.ForeColor = &HC000& Then
ResOpButs
 opHighG.ForeColor = &HFFFF&
End If
End Sub

Private Sub OPLowG_Click()
HGraphics = False
chkGrid.Value = 0
chkAutoH.Value = 0
chkFPS.Value = 0
chkHUD.Value = 1
chkRetro.Value = 0
chkHGT.Value = 0
OPLowG.Value = True
End Sub

Private Sub OPLowG_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
'If chkFPS.ForeColor = &HC000& Then
'ResOpButs
' chkFPS.ForeColor = &HC0FFC0
'End If

If OPLowG.ForeColor = &HC000& Then
ResOpButs
 OPLowG.ForeColor = &HFFFF&
End If
End Sub

Private Sub picAdOps_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
ResOpButs
End Sub

Private Sub picBG_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Call TogSwch
Call ResSUButs
End Sub
Private Sub picButs_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Call ResSUButs
End Sub
Private Sub picSetup_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Call TogSwch
End Sub

Private Sub Timer1_Timer()
'load game resources...
lblLoading.Caption = "Loading."
Load frmMsg
    lblLoading.Caption = "Loading.."
    Load frmRes
        lblLoading.Caption = "Loading..."
        Load frmMain
        picAdOps.Picture = frmMsg.Picture
        
picLoading.Visible = False
Call DrawGrid       'Make the grid
Call MakeVisible    'Make all captions visble
Timer1.Enabled = False
End Sub

Private Sub tmrEffects_Timer()
'light effect at the bottom of the setup screen
On Error Resume Next
If CurFlasher = 15 Then CurFlasher = -4
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

Public Sub SwitchTeams()
'switch team colors
If p1team = 0 Then
 p1team = 1
 picP1.Picture = frmRes.CBlue.Picture
 picP2.Picture = frmRes.CRed.Picture
Else
    p1team = 0
    picP1.Picture = frmRes.CRed.Picture
    picP2.Picture = frmRes.CBlue.Picture
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

Public Sub SetTheSpeed()
If optSpeed(0).Value = True Then
 frmMain.SetGameSpeeds 1
        
        ElseIf optSpeed(1).Value = True Then
        frmMain.SetGameSpeeds 2
            
                ElseIf optSpeed(2).Value = True Then
                frmMain.SetGameSpeeds 3
                    
                    ElseIf optSpeed(3).Value = True Then
                     frmMain.SetGameSpeeds 4
 End If
End Sub

Public Sub GridIt()
'NOTE that i've set it up so it doesn't
'draw the mask if theres no grid, that saves
'processor time on slower machines...

If chkGrid.Value = 0 Then 'no grid
'clear playing area
frmMain.AutoRedraw = False
 With frmMain
 .Cls
 .Refresh
 .Picture = frmRes.picNull.Picture
 End With
 GridOn = False
 frmMain.AutoRedraw = True
ElseIf chkGrid.Value = 1 Then
 With frmMain
  .Refresh
  .Cls
 .Picture = frmRes.picBGgrid.Picture
 End With
 GridOn = True
 End If
End Sub

Public Sub CheckHG()
 If chkAutoH.Value = 1 Then
  DoHG = True
  Else
  DoHG = False
  End If
End Sub

Public Sub ResOpButs()
If shpBorder.FillColor = &H404040 And chkGrid.ForeColor = &HC000& And _
chkAutoH.ForeColor = &HC000& And chkHUD.ForeColor = &HC000& And _
chkFPS.ForeColor = &HC000& And chkRetro.ForeColor = &HC000& And _
chkHGT.ForeColor = &HC000& And OPcG.ForeColor = &HC000& And _
opHighG.ForeColor = &HC000& And OPLowG.ForeColor = &HC000& Then Exit Sub

shpBorder.FillColor = &H404040
  chkGrid.ForeColor = &HC000&
  chkAutoH.ForeColor = &HC000&
  chkHUD.ForeColor = &HC000&
  chkFPS.ForeColor = &HC000&
  chkRetro.ForeColor = &HC000&
  chkHGT.ForeColor = &HC000&
  
  OPcG.ForeColor = &HC000&
  opHighG.ForeColor = &HC000&
  OPLowG.ForeColor = &HC000&
End Sub

Public Sub ResSUButs()
'If shpBut1.FillColor = &HC000& Then
'shpBut1.FillColor = &H4000&
'cmdStart.ForeColor = 8454016
'End If

'If shpBut2.FillColor = &HC000& Then
'shpBut2.FillColor = &H4000&
'cmdOpts.ForeColor = &H80FF80
'End If

'If shpBut3.FillColor = &HC000& Then
 'shpBut3.FillColor = &H4000&
 'cmdHelp.ForeColor = &H80FF80
'End If

'If shpBut4.FillColor = &HC000& Then
' shpBut4.FillColor = &H4000&
' cmdCancel.ForeColor = &H80FF80
'End If

If shpBut1.BorderColor = &HFF00& Then
cmdStart.ForeColor = &HC000&
shpBut1.BorderColor = &H4000&
End If

If shpBut2.BorderColor = &HFF00& Then
cmdOpts.ForeColor = &HC000&
shpBut2.BorderColor = &H4000&
End If

If shpBut3.BorderColor = &HFF00& Then
cmdHelp.ForeColor = &HC000&
shpBut3.BorderColor = &H4000&
End If

If shpBut4.BorderColor = &HFF00& Then
cmdCancel.ForeColor = &HC000&
shpBut4.BorderColor = &H4000&
End If
End Sub

Public Sub TogSwch()
If SwitchState > 0 Then
 butSwitch.Picture = picSwitch.Picture
 picLblSwitch.Visible = False
 SwitchState = 0
 End If
End Sub

Public Sub PressedStart()
Unload frmMsg
Call SetUpColors

    P1Name = txtP1Name.Text
    P2Name = txtP2Name.Text
    Call SetTheSpeed
    
    'set up grid if requested...
    Call GridIt
    
    Call CheckHG
    If chkFPS.Value = 1 Then
      ShowFPS = True
    Else
      ShowFPS = False
    End If
         
    If chkHUD.Value = 1 Then
            ShowHUD = True
                Else
            ShowHUD = False
    End If
    
    If chkRetro.Value = 1 Then
    ShowRetro = True
        Else
    ShowRetro = False
    End If
    
    If chkHGT.Value = 1 Then
     HGTE = True
      Else
     HGTE = False
    End If
    
    'Make Player Names
    With frmRes.picP1Name
     .Refresh
     .Cls
     .AutoRedraw = True
    End With
      frmRes.picP1Name.Print P1Name
            With frmRes.picP2Name
               .Refresh
               .Cls
               .AutoRedraw = True
            End With
            frmRes.picP2Name.Print P2Name
            
    Call SetFragScores 'redraw scores for possible viewing
    Sleep 200
    
    frmSetup.Hide
    frmMain.Show
    frmMain.InitNewGame
    Unload frmSetup
    Exit Sub
End Sub

Private Sub DoButtSlide()
Select Case SlideDir
   Case 0 'slide in...
   
Do
  Sleep 10
  
  DoEvents
   With picButs
    If .Left > 440 Then
     .Left = .Left - 5
     ElseIf .Left = 440 Then
      
            Exit Sub
      SlideDir = 1
      Exit Sub
    End If

   End With
Loop

Case 1 'slide out...

Do
  Sleep 20
  
  DoEvents
With picButs
   If .Left < 640 Then
     .Left = .Left + 10
    ElseIf .Left >= 640 Then
    
    SlideDir = 0
     Call PressedStart
    Exit Sub
    End If
  End With
Loop
End Select
End Sub
