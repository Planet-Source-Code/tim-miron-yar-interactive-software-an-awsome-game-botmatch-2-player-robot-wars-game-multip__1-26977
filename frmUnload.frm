VERSION 5.00
Begin VB.Form frmUnload 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   630
   ClientLeft      =   15
   ClientTop       =   15
   ClientWidth     =   3705
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   630
   ScaleWidth      =   3705
   StartUpPosition =   3  'Windows Default
   Begin VB.Label lblUnload 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Unloading Game... Please Wait"
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
      Left            =   345
      TabIndex        =   0
      Top             =   180
      Width           =   3000
   End
End
Attribute VB_Name = "frmUnload"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

