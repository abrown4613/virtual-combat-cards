' $Id$
' Windows Script Host Sample Script
'
' ------------------
' Copyright (C) 1996-1997 Microsoft Corporation
'
' You have a royalty-free right to use, modify, reproduce and distribute
' the Sample Application Files (and/or any modified version) in any way
' you find useful, provided that you agree that Microsoft has no warranty,
' obligations or liability for any Sample Application Files.
' ------------------


' This sample demonstrates how to use the WSHShell object to create a shortcut
' on the desktop.

L_Welcome_MsgBox_Message_Text = "This script will create a shortcut for Virtual Combat Cards on the Desktop."
L_Welcome_MsgBox_Title_Text = "Virtual Combat Cards Shortcut"
Call Welcome()

' ********************
' *
' * Shortcut related methods.
' *

Dim WSHShell
Set WSHShell = WScript.CreateObject("WScript.Shell")

currentDirectory = left(WScript.ScriptFullName,(Len(WScript.ScriptFullName))-(len(WScript.ScriptName)))
'WScript.Echo currentDirectory

Dim MyShortcut, MyDesktop, DesktopPath

' Read desktop path using WshSpecialFolders object
DesktopPath = WSHShell.SpecialFolders("Desktop")

' Create a shortcut object on the desktop
Set MyShortcut = WSHShell.CreateShortcut(DesktopPath & "\VCC.lnk")

' Set shortcut object properties and save it
MyShortcut.TargetPath =  currentDirectory + "\vcc.bat"
' WSHShell.ExpandEnvironmentStrings("%windir%\notepad.exe")
MyShortcut.WorkingDirectory = currentDirectory
' WSHShell.ExpandEnvironmentStrings("%windir%")
MyShortcut.WindowStyle = 2
MyShortcut.IconLocation = currentDirectory + "\d20metal.icO"
'WSHShell.ExpandEnvironmentStrings("%windir%\notepad.exe, 0")
MyShortcut.Save

WScript.Echo "A shortcut to VCC now exists on your Desktop."

' ********************
' *
' * Welcome
' *
Sub Welcome()
    Dim intDoIt

    intDoIt =  MsgBox(L_Welcome_MsgBox_Message_Text,    _
                      vbOKCancel + vbInformation,       _
                      L_Welcome_MsgBox_Title_Text )
    If intDoIt = vbCancel Then
        WScript.Quit
    End If
End Sub