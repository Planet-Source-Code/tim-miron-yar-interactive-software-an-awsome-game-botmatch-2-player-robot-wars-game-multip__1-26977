Attribute VB_Name = "YIGE"

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'yar-interactive 2D game engine for Visual Basic     :
'                                                    :
'  http://www.yarinteractive.com                     :
'                                                    :
'IMPORTANT:                                          :
'We'll be releasing a full version of this engine in :
'About two weeks from now (March 20th 2001) be sure  :
'to check our website then for your free copy        :
''''''''''''''''''''''''''''''''''''''''''''''''''''''

'read the comments in each sub for more info on
'how to utilize it...
Option Explicit

Private Const CCDEVICENAME = 32
Private Const CCFORMNAME = 32

Private Const DISP_CHANGE_SUCCESSFUL = 0
Private Const DISP_CHANGE_RESTART = 1
Private Const DISP_CHANGE_FAILED = -1
Private Const DISP_CHANGE_BADMODE = -2
Private Const DISP_CHANGE_NOTUPDATED = -3
Private Const DISP_CHANGE_BADFLAGS = -4
Private Const DISP_CHANGE_BADPARAM = -5

Private Const CDS_UPDATEREGISTRY = &H1
Private Const CDS_TEST = &H2

Private Const DM_BITSPERPEL = &H40000
Private Const DM_PELSWIDTH = &H80000
Private Const DM_PELSHEIGHT = &H100000

Private Type DEVMODE
  dmDeviceName As String * CCDEVICENAME
  dmSpecVersion As Integer
  dmDriverVersion As Integer
  dmSize As Integer
  dmDriverExtra As Integer
  dmFields As Long
  dmOrientation As Integer
  dmPaperSize As Integer
  dmPaperLength As Integer
  dmPaperWidth As Integer
  dmScale As Integer
  dmCopies As Integer
  dmDefaultSource As Integer
  dmPrintQuality As Integer
  dmColor As Integer
  dmDuplex As Integer
  dmYResolution As Integer
  dmTTOption As Integer
  dmCollate As Integer
  dmFormName As String * CCFORMNAME
  dmUnusedPadding As Integer
  dmBitsPerPel As Integer
  dmPelsWidth As Long
  dmPelsHeight As Long
  dmDisplayFlags As Long
  dmDisplayFrequency As Long
End Type

Private Declare Function EnumDisplaySettings Lib "user32" Alias "EnumDisplaySettingsA" (ByVal lpszDeviceName As Long, ByVal iModeNum As Long, lpDevMode As Any) As Boolean
Public Declare Function GetKeyState Lib "user32" (ByVal nVirtKey As Long) As Integer

Private Declare Function ChangeDisplaySettings Lib "user32" Alias "ChangeDisplaySettingsA" (lpDevMode As Any, ByVal dwflags As Long) As Long
Public Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal X As Long, ByVal Y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Public Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Public Declare Function GdiFlush Lib "gdi32" () As Long
Public Declare Function ShowCursor Lib "user32" (ByVal bShow As Long) As Long
Public Declare Function SetWindowPos Lib "user32" (ByVal hwnd As Long, ByVal hWndInsertAfter As Long, ByVal X As Long, ByVal Y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long

Public Declare Function GetTickCount Lib "kernel32" () As Long



'###### YIGE Functions #######

'Change Screen settings allows you to specifie the display mode,
'the recommended display mode for this engine is 640 X 480 With 16 bit color
'That display mode can be accessed like this:

'ChangeScreenSettings 640, 480, 16
'(or 24 bit color - it makes it run alot faster at 24)

'To restore their screen mode when the game is done, do as follows
'Call RestoreScrnMode

Public Function ChangeScreenSettings(lWidth As Integer, lHeight As Integer, lColors As Integer)
Dim tDevMode As DEVMODE, lTemp As Long, lIndex As Long
lIndex = 0
Do
  lTemp = EnumDisplaySettings(0&, lIndex, tDevMode)
  If lTemp = 0 Then Exit Do
  lIndex = lIndex + 1

  With tDevMode
    If .dmPelsWidth = lWidth And .dmPelsHeight = lHeight And .dmBitsPerPel = lColors Then
      lTemp = ChangeDisplaySettings(tDevMode, CDS_UPDATEREGISTRY)
      Exit Do
    End If
  End With
Loop
Select Case lTemp
  Case DISP_CHANGE_SUCCESSFUL
'    MsgBox "The display settings change was successful", vbInformation
  Case DISP_CHANGE_RESTART
    MsgBox "The computer must be restarted in order for the graphics mode to work", vbQuestion
  Case DISP_CHANGE_FAILED
    MsgBox "The display driver failed the specified graphics mode", vbCritical
  Case DISP_CHANGE_BADMODE
    MsgBox "The graphics mode is not supported", vbCritical
  Case DISP_CHANGE_NOTUPDATED
    MsgBox "Unable to write settings to the registry", vbCritical
  Case DISP_CHANGE_BADFLAGS
    MsgBox "An invalid set of flags was passed in", vbCritical
End Select
End Function

'Call this procedure directly before calling the ChangeScreenSettings
'function so we cn restore their settings later,
'(see RestoreRes function)
'Public Sub RememberScreenRes()
'iWidth = Screen.Width \ Screen.TwipsPerPixelX
'iHeight = Screen.Height \ Screen.TwipsPerPixelY
'End Sub
'
'Public Sub RestoreRes() 'restore the screen settings...
'ChangeScreenSettings iWidth, iHeight, 24
'GdiFlush
'ShowCursor 1
'End Sub


'This is the main part of this engine that does the blitting
'use the DrawSprite sub to draw your sprites to the game screen
'(See our example)

'Call the CLS and Refresh functions of the game
'surface (game surface is usually a picturebox
'or form, its the object that all the sprites are displayed in...
Public Sub DrawSprite(GameSurf As Object, SpriteSource As Object, SpriteX As Integer, SpriteY As Integer, SpriteWidth As Integer, SpriteHeight As Integer, DrawMode As Long)
BitBlt GameSurf.hDC, SpriteX, SpriteY, SpriteWidth, SpriteHeight, SpriteSource.hDC, SpriteSource.ScaleLeft, SpriteSource.ScaleTop, DrawMode
End Sub

Public Function IsKeyDown(AsciiKeyCode As Byte) As Boolean
'use this function to tell if a key
'is down, it can detect multiple kepresses, unlike the
'keydown function... (See Example Game)...
If GetKeyState(AsciiKeyCode) < -125 Then IsKeyDown = True
End Function

'gets the CPU ticks-per-second
'for use in code timers, etc.
Public Function GetTicksPerSecond() As Long
 Dim a, b
  a = GetTickCount
    Sleep 1000
    b = GetTickCount
      GetTicksPerSecond = b - a
End Function


Public Function XYGetDistance(X1 As Single, _
X2 As Single, Y1 As Single, Y2 As Single) As Double
'calculate distance
'between 2 points on a X-Y grid
XYGetDistance = Sqr(((X2 - X1) ^ 2 + (Y2 - Y1) ^ 2))
'notes: X is horizontal coord. (Left)
'       Y is Vertical coord.   (Top)
End Function

Public Function DidClick(X As Single, Y As Single, ObjectX As Single, _
ObjectY As Single, ObjectWidth As Integer, objectHeight As Integer) As Byte
'did mouse click on surface/sprite?
If X > ObjectX And X < (ObjectX + ObjectWidth) And Y > ObjectY And Y < _
(ObjectY + objectHeight) Then DidClick = 1
End Function
