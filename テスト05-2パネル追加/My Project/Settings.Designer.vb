﻿'------------------------------------------------------------------------------
' <auto-generated>
'     このコードはツールによって生成されました。
'     ランタイム バージョン:4.0.30319.42000
'
'     このファイルへの変更は、以下の状況下で不正な動作の原因になったり、
'     コードが再生成されるときに損失したりします。
' </auto-generated>
'------------------------------------------------------------------------------

Option Strict On
Option Explicit On


Namespace My
    
    <Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute(),  _
     Global.System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "17.10.0.0"),  _
     Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)>  _
    Partial Friend NotInheritable Class MySettings
        Inherits Global.System.Configuration.ApplicationSettingsBase
        
        Private Shared defaultInstance As MySettings = CType(Global.System.Configuration.ApplicationSettingsBase.Synchronized(New MySettings()),MySettings)
        
#Region "My.Settings 自動保存機能"
#If _MyType = "WindowsForms" Then
    Private Shared addedHandler As Boolean

    Private Shared addedHandlerLockObject As New Object

    <Global.System.Diagnostics.DebuggerNonUserCodeAttribute(), Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)> _
    Private Shared Sub AutoSaveSettings(sender As Global.System.Object, e As Global.System.EventArgs)
        If My.Application.SaveMySettingsOnExit Then
            My.Settings.Save()
        End If
    End Sub
#End If
#End Region
        
        Public Shared ReadOnly Property [Default]() As MySettings
            Get
                
#If _MyType = "WindowsForms" Then
               If Not addedHandler Then
                    SyncLock addedHandlerLockObject
                        If Not addedHandler Then
                            AddHandler My.Application.Shutdown, AddressOf AutoSaveSettings
                            addedHandler = True
                        End If
                    End SyncLock
                End If
#End If
                Return defaultInstance
            End Get
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("ControlText")>  _
        Public Property FontColor文字の描画() As Global.System.Drawing.Color
            Get
                Return CType(Me("FontColor文字の描画"),Global.System.Drawing.Color)
            End Get
            Set
                Me("FontColor文字の描画") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("ControlText")>  _
        Public Property FontColor2文字の描画() As Global.System.Drawing.Color
            Get
                Return CType(Me("FontColor2文字の描画"),Global.System.Drawing.Color)
            End Get
            Set
                Me("FontColor2文字の描画") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("")>  _
        Public Property FontName() As String
            Get
                Return CType(Me("FontName"),String)
            End Get
            Set
                Me("FontName") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Gray")>  _
        Public Property FringeColor2縁取りの色() As Global.System.Drawing.Color
            Get
                Return CType(Me("FringeColor2縁取りの色"),Global.System.Drawing.Color)
            End Get
            Set
                Me("FringeColor2縁取りの色") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property TextColor1文字の描画() As Global.System.Drawing.Color
            Get
                Return CType(Me("TextColor1文字の描画"),Global.System.Drawing.Color)
            End Get
            Set
                Me("TextColor1文字の描画") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property TextColor2文字の描画() As Global.System.Drawing.Color
            Get
                Return CType(Me("TextColor2文字の描画"),Global.System.Drawing.Color)
            End Get
            Set
                Me("TextColor2文字の描画") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("False")>  _
        Public Property TextGradation文字の描画() As Boolean
            Get
                Return CType(Me("TextGradation文字の描画"),Boolean)
            End Get
            Set
                Me("TextGradation文字の描画") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("False")>  _
        Public Property StringGamma文字の描画ガンマ補正() As Boolean
            Get
                Return CType(Me("StringGamma文字の描画ガンマ補正"),Boolean)
            End Get
            Set
                Me("StringGamma文字の描画ガンマ補正") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("False")>  _
        Public Property TextItalic文字の描画() As Boolean
            Get
                Return CType(Me("TextItalic文字の描画"),Boolean)
            End Get
            Set
                Me("TextItalic文字の描画") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("False")>  _
        Public Property TextBold文字の描画太字() As Boolean
            Get
                Return CType(Me("TextBold文字の描画太字"),Boolean)
            End Get
            Set
                Me("TextBold文字の描画太字") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("True")>  _
        Public Property AntiAlias文字の描画アンチエイリアス() As Boolean
            Get
                Return CType(Me("AntiAlias文字の描画アンチエイリアス"),Boolean)
            End Get
            Set
                Me("AntiAlias文字の描画アンチエイリアス") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("False")>  _
        Public Property StringShadow文字の描画影() As Boolean
            Get
                Return CType(Me("StringShadow文字の描画影"),Boolean)
            End Get
            Set
                Me("StringShadow文字の描画影") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Gray")>  _
        Public Property StringShadowColor文字の描画影色() As Global.System.Drawing.Color
            Get
                Return CType(Me("StringShadowColor文字の描画影色"),Global.System.Drawing.Color)
            End Get
            Set
                Me("StringShadowColor文字の描画影色") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("0")>  _
        Public Property StringFringe2文字の描画縁取りの幅() As Long
            Get
                Return CType(Me("StringFringe2文字の描画縁取りの幅"),Long)
            End Get
            Set
                Me("StringFringe2文字の描画縁取りの幅") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Gray")>  _
        Public Property 文字の描画_影_色1() As Global.System.Drawing.Color
            Get
                Return CType(Me("文字の描画_影_色1"),Global.System.Drawing.Color)
            End Get
            Set
                Me("文字の描画_影_色1") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("128")>  _
        Public Property SelectRangeWidth() As Long
            Get
                Return CType(Me("SelectRangeWidth"),Long)
            End Get
            Set
                Me("SelectRangeWidth") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("128")>  _
        Public Property SelectRangeHeight() As Long
            Get
                Return CType(Me("SelectRangeHeight"),Long)
            End Get
            Set
                Me("SelectRangeHeight") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("32")>  _
        Public Property Grid() As Decimal
            Get
                Return CType(Me("Grid"),Decimal)
            End Get
            Set
                Me("Grid") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("90")>  _
        Public Property JpegQuality() As Decimal
            Get
                Return CType(Me("JpegQuality"),Decimal)
            End Get
            Set
                Me("JpegQuality") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Cyan")>  _
        Public Property 図形2線の色() As Global.System.Drawing.Color
            Get
                Return CType(Me("図形2線の色"),Global.System.Drawing.Color)
            End Get
            Set
                Me("図形2線の色") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Silver")>  _
        Public Property 図形2線の影の色() As Global.System.Drawing.Color
            Get
                Return CType(Me("図形2線の影の色"),Global.System.Drawing.Color)
            End Get
            Set
                Me("図形2線の影の色") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 図形の色1() As Global.System.Drawing.Color
            Get
                Return CType(Me("図形の色1"),Global.System.Drawing.Color)
            End Get
            Set
                Me("図形の色1") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 図形の色2() As Global.System.Drawing.Color
            Get
                Return CType(Me("図形の色2"),Global.System.Drawing.Color)
            End Get
            Set
                Me("図形の色2") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 図形の色3() As Global.System.Drawing.Color
            Get
                Return CType(Me("図形の色3"),Global.System.Drawing.Color)
            End Get
            Set
                Me("図形の色3") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 文字背景色1() As Global.System.Drawing.Color
            Get
                Return CType(Me("文字背景色1"),Global.System.Drawing.Color)
            End Get
            Set
                Me("文字背景色1") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 文字背景色2() As Global.System.Drawing.Color
            Get
                Return CType(Me("文字背景色2"),Global.System.Drawing.Color)
            End Get
            Set
                Me("文字背景色2") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 文字枠色1() As Global.System.Drawing.Color
            Get
                Return CType(Me("文字枠色1"),Global.System.Drawing.Color)
            End Get
            Set
                Me("文字枠色1") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("White")>  _
        Public Property 文字枠色2() As Global.System.Drawing.Color
            Get
                Return CType(Me("文字枠色2"),Global.System.Drawing.Color)
            End Get
            Set
                Me("文字枠色2") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 文字影色2() As Global.System.Drawing.Color
            Get
                Return CType(Me("文字影色2"),Global.System.Drawing.Color)
            End Get
            Set
                Me("文字影色2") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 文字縁取り色1() As Global.System.Drawing.Color
            Get
                Return CType(Me("文字縁取り色1"),Global.System.Drawing.Color)
            End Get
            Set
                Me("文字縁取り色1") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("Black")>  _
        Public Property 文字縁取り色2() As Global.System.Drawing.Color
            Get
                Return CType(Me("文字縁取り色2"),Global.System.Drawing.Color)
            End Get
            Set
                Me("文字縁取り色2") = value
            End Set
        End Property
    End Class
End Namespace

Namespace My
    
    <Global.Microsoft.VisualBasic.HideModuleNameAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute()>  _
    Friend Module MySettingsProperty
        
        <Global.System.ComponentModel.Design.HelpKeywordAttribute("My.Settings")>  _
        Friend ReadOnly Property Settings() As Global.Pixtack.My.MySettings
            Get
                Return Global.Pixtack.My.MySettings.Default
            End Get
        End Property
    End Module
End Namespace
