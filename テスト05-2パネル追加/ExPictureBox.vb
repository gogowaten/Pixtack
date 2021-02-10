Public Class ExPictureBox
    Inherits PictureBox

    Public Function Clone() As ExPictureBox

        Return CType(Me.MemberwiseClone(), ExPictureBox)

    End Function
    Private _PathPoints As New Generic.List(Of PointF) '頂点のリスト
    Public Property PathPoints As Generic.List(Of PointF)
        Get
            Return _PathPoints

        End Get
        Set(value As Generic.List(Of PointF))
            _PathPoints = value
        End Set
    End Property

    'ExPenを呼ばれた時に色、太さ、先端、終端を設定して渡す
    Private _ExPen As Pen
    Public Property ExPen As Pen
        Get
           
            'With _ExPen
            '    .StartCap = _ExStartLineCap
            '    .EndCap = _ExEndLineCap
            '    .Width = ExPenWidth
            '    .Color = ExPenColor
            'End With
            Return _ExPen
        End Get
        Set(value As Pen)
            _ExPen = value
            _ExStartLineCap = value.StartCap
            _ExEndLineCap = value.EndCap
            _ExPenWidth = value.Width
            _ExPenColor = value.Color
            'ExPenStartCap'いる？
            ExPenColor = value.Color
            ExPenWidth = value.Width
            _ExLineJoin = value.LineJoin

        End Set
    End Property
    ''エンドキャップ、矢印とか
    Enum PenCap
        平坦
        矢印
        四角
        丸
        三角
        四角アンカー
        丸アンカー
        ダイヤアンカー
        なし
        アンカーマスク
        カスタム
    End Enum
    '角の形状
    Enum PenLineJoin角の形状
        鋭角
        斜め
        丸
        制限鋭角
    End Enum

    Private _ExLineJoin
    Private _LineJoin角の形状
    Public Property ExLineJoin As PenLineJoin角の形状
        Get
            
            Return _LineJoin角の形状 'これはPenLineJoin角の形状に変換したほうがいいのかなあ
        End Get
        Set(value As PenLineJoin角の形状)
            Select Case True
                Case value = PenLineJoin角の形状.鋭角

                    _ExLineJoin = Drawing2D.LineJoin.Miter
                    'ExPen.LineJoin = Drawing2D.LineJoin.Miter'これじゃない
                    _ExPen.LineJoin = Drawing2D.LineJoin.Miter 'こっち
                Case value = PenLineJoin角の形状.丸
                    _ExLineJoin = Drawing2D.LineJoin.Round
                    _ExPen.LineJoin = Drawing2D.LineJoin.Round
                Case value = PenLineJoin角の形状.斜め
                    _ExLineJoin = Drawing2D.LineJoin.Bevel
                    _ExPen.LineJoin = Drawing2D.LineJoin.Bevel
                Case value = PenLineJoin角の形状.制限鋭角
                    _ExLineJoin = Drawing2D.LineJoin.MiterClipped
                    _ExPen.LineJoin = Drawing2D.LineJoin.MiterClipped
            End Select
            _LineJoin角の形状 = value

        End Set
    End Property

    Private _ExStartLineCap As Drawing2D.LineCap 'ラインキャップ、矢印とか
    Private _ExPenStartCap As PenCap
    Public Property ExPenStartCap As PenCap
        Get
            'Return _ExStartLineCap
            Return _ExPenStartCap
        End Get
        Set(value As PenCap)
            _ExStartLineCap = PenCap2LineCap(value)
            _ExPenStartCap = value
            _ExPen.StartCap = _ExStartLineCap

        End Set
    End Property

    Private _ExEndLineCap As Drawing2D.LineCap
    Private _ExPenEndCap As PenCap
    Public Property ExPenEndCap As PenCap
        Get
            'Return _ExEndLineCap
            Return _ExPenEndCap
        End Get
        Set(value As PenCap)
            _ExEndLineCap = PenCap2LineCap(value)
            _ExPenEndCap = value
            _ExPen.EndCap = _ExEndLineCap

        End Set
    End Property
    Private Function PenCap2LineCap(pc As PenCap) As Drawing2D.LineCap
        Dim lc As Drawing2D.LineCap
        Select Case pc
            Case PenCap.アンカーマスク
                lc = Drawing2D.LineCap.AnchorMask
            Case PenCap.カスタム
                lc = Drawing2D.LineCap.Custom
            Case PenCap.ダイヤアンカー
                lc = Drawing2D.LineCap.DiamondAnchor
            Case PenCap.丸
                lc = Drawing2D.LineCap.Round
            Case PenCap.丸アンカー
                lc = Drawing2D.LineCap.RoundAnchor
            Case PenCap.三角
                lc = Drawing2D.LineCap.Triangle
            Case PenCap.四角
                lc = Drawing2D.LineCap.Square
            Case PenCap.四角アンカー
                lc = Drawing2D.LineCap.SquareAnchor
            Case PenCap.平坦
                lc = Drawing2D.LineCap.Flat
            Case PenCap.矢印
                lc = Drawing2D.LineCap.ArrowAnchor
            Case PenCap.なし
                lc = Drawing2D.LineCap.NoAnchor

        End Select

        Return lc

    End Function

    'ペンの太さ、Penがあれば要らないかと思ったけど太さだけを変更したいときにあるとラク
    Private _ExPenWidth As Single
    Public Property ExPenWidth As Single
        Get
            Return _ExPenWidth
        End Get
        Set(value As Single)
            ExPen.Width = value
            _ExPenWidth = value

        End Set
    End Property
    'Penの色、これもあったほうが便利
    Private _ExPenColor As Color
    Public Property ExPenColor As Color
        Get
            Return _ExPenColor

        End Get
        Set(value As Color)
            ExPen.Color = value
            _ExPenColor = value

        End Set
    End Property
    '影の色
    Private _ShapdowColor As Color
    Public Property ShadowColor As Color
        Get
            Return _ShapdowColor
        End Get
        Set(value As Color)
            _ShapdowColor = value
        End Set
    End Property
    Public Property isShadow As Boolean

    Private _IsEdit As Boolean = False '編集できる画像がどうかの判定用
    Public Property IsEdit As Boolean
        Get
            Return _IsEdit
        End Get
        Set(value As Boolean)
            _IsEdit = value
        End Set
    End Property


    Private GraphicTypeString As String '描画のタイプ、これはPrivateでいい
    Public Enum DrawType
        '[Nothing]
        直線
        曲線
        ベジェ曲線
        四角枠
        DrawBeziers
        DrawClosedCurve
        DrawEllipse
        DrawLines
        DrawPath
        DrawPolygon
        DrawRectangle
        FillClosedCurve
        FillEllipse
        FillPath
        FillPolygon
        FillRectangle
        DrawCurve

    End Enum
    Public Property GraphicDrawType As DrawType '描画のタイプ、直線、曲線、塗りつぶしとか
        Get
            Return GraphicTypeString
        End Get
        Set(value As DrawType)
            GraphicTypeString = value
        End Set
    End Property

    Private _ExFillType As Drawing2D.FillMode ' GraphicFillType
    Public Enum GraphicFillType
        全域 'Winding
        交互 'Alternate
    End Enum
    Public Property ExFillType As GraphicFillType '塗りつぶしのタイプ、全域か交互
        Get
            Return _ExFillType
        End Get
        Set(value As GraphicFillType)
            If value = GraphicFillType.交互 Then
                _ExFillType = Drawing2D.FillMode.Alternate

            ElseIf value = GraphicFillType.全域 Then
                _ExFillType = Drawing2D.FillMode.Winding
            End If

        End Set
    End Property


    Private _CurveTension As Single
    Public Property CurveTension As Single 'カーディナルスプライン曲線のテンション記録用
        Get
            Return _CurveTension
        End Get
        Set(value As Single)
            _CurveTension = value
        End Set
    End Property

    
    '普通のプロパティの宣言はこれでいいの？Getやsetを書かなくてもいいの？
    '    Visual Basic 2010の新機能 － ＠IT
    'http://www.atmarkit.co.jp/fdotnet/chushin/vb2010features_01/vb2010features_01_02.html
    Public Property DrawMargin As Integer '余白、太い線だと頂点からはみ出るから





    '線を閉じているか
    Public Property CloseLine As Boolean

    '塗りつぶしモード、全域or交互
    Public Property FillMode As Drawing2D.FillMode

    '塗りつぶし？
    Public Property isFill As Boolean

    'アンチエイリアス？
    Public Property isAntiAlias As Boolean


    '塗りつぶし用の単色ブラシ
    'Public Property ExSolidBrushColor As Color'Penの色と共有するかどうか
    Private _ExSolidBrush As SolidBrush
    Public Property ExSolidBrush As SolidBrush
        Get
            'Dim sb As New SolidBrush(ExSolidBrushColor)
            Dim sb As New SolidBrush(ExPenColor)
            Return sb
        End Get
        Set(value As SolidBrush)
            _ExSolidBrush = value
        End Set
    End Property

    'グリッドに合わせているかどうか
    Public Property isGrid As Boolean
       

    '--------------------------------追加したら追加する場所
    'CloneExPic図形の設定の複製
    'ExPicBoxSetting図形の設定書き込み
    '頂点の初期化()
    '追加したら追加する場所





    'Private _ExPenEndCap As Drawing2D.LineCap
    'Public Property ExPenEndCap As Drawing2D.LineCap
    '    Get
    '        Return _ExPenEndCap
    '    End Get
    '    Set(value As Drawing2D.LineCap)
    '        _ExPenEndCap = value
    '    End Set
    'End Property

    'ここまで図形2


    '文字描画
    Public Property DrawString描画文字 As String
    Public Property IsDrawString As Boolean '文字描画画像かどうか
    'ここから未使用
    Public Property FontSetting文字描画設定 As Pixtack.FontSetting 'これもできるみたい、FontSettingクラスをPublic宣言してから

End Class

<Serializable> Class ExPictureBoxList
    Inherits List(Of ExPictureBox)

End Class



'フォントの設定
<Serializable> Public Class FontSetting 'Public宣言するとExPictureからも使えるようになった
    Public Property FontColor1 As Color 'この一行だけでも値は保存されるみたい、逆にGetとSetを書いて_foreColor1変数を書かないと値が保存されない
    '    Get

    '    End Get
    '    Set(value As Color)

    '    End Set
    'End Property
    Public Property FontColor2 As Color 'フォントカラー2
    Public Property ForeColor1 As Color
    Public Property ForeColor2 As Color
    Public Property FontTransparent1 As Integer 'フォントカラー1の透明度
    Public Property FontTransparent2 As Integer 'フォントカラー2の透明度

    Public Property IsGradation As Boolean 'グラデーションの有無
    Public Property IsGammaC As Boolean 'ガンマ補正の有無
    Public Property FontGradationAngel As Integer 'グラデーションの角度

    Public Property FontName As String 'フォントネーム
    Public Property Yokogaki As Boolean '横書きならTrue、縦書ならFalse
    Public Property Size As Integer 'フォントサイズ
    Public Property Italic As Boolean '斜体
    Public Property Bold As Boolean '太字
    Public Property LineSpace As Integer '行間
    Public Property WordSpace As Integer '文字間

    Public Property AntiAlias As Boolean 'アンチエイリアス
    Public Property Angle As Integer '傾き角度
    Public Property IsAngle As Boolean '傾き角度チェック
    Public Property IsAdjust As Boolean '文字描画位置調整の有無


    '背景
    Public Property IsNotBackColor As Boolean '背景色の有無
    Public Property BGColor1 As Color '背景色1
    Public Property BGColor2 As Color '背景色2
    Public Property BGTransparent1 As Integer '背景色1透明度
    Public Property BGTransparent2 As Integer '背景色2透明度

    Public Property IsBGGradation As Boolean 'グラデーションの有無
    Public Property IsBGGammaC As Boolean 'ガンマ補正の有無
    Public Property BGGradationAngel As Integer 'グラデーションの角度

    Public Property BGRound As Integer '角の丸さ
    'Public Property BGLinkedRound As Boolean '枠との角の丸さの連動有無

    '枠
    Public Property IsFrame As Boolean '枠の有無
    Public Property wakuColor1 As Color '枠の色1
    Public Property wakuColor2 As Color '枠の色2
    Public Property wakuTransparent1 As Integer '枠の色1透明度
    Public Property wakuTransparent2 As Integer '枠の色2透明度

    Public Property IsWakuGradation As Boolean '枠のグラデーションの有無
    Public Property IsWakuGammaC As Boolean 'ガンマ補正の有無
    Public Property wakuGradationAngle As Integer 'グラデーションの角度

    Public Property wakuRound As Integer '角の丸さ

    Public Property wakuWidth As Integer '枠幅


    '影
    Public Property IsShadow As Boolean '影の有無
    '影色
    Public Property ShadowColor1 As Color '影色1
    Public Property ShadowColor2 As Color '影色2
    Public Property ShadowTranstarent1 As Integer '影色透明度1
    Public Property ShadowTranstarent2 As Integer '影色透明度2
    '影グラデーション
    Public Property IsShadowGradation As Boolean 'グラデーションの有無
    Public Property IsShadowGammaC As Boolean 'ガンマ補正の有無
    Public Property ShadowGradationAngle As Integer 'グラデーションの角度
    '影位置
    Public Property ShadowH As Integer '影の横位置
    Public Property ShadowV As Integer '影の縦位置


    '縁取り
    Public Property IsFringe As Boolean '縁取りの有無
    Public Property FringeColor As Color '縁取りの色1
    Public Property FringeColor2 As Color '縁取りの色2
    Public Property FringeTransparent1 As Integer '縁取り色透明度1
    Public Property FringeTransparent2 As Integer '縁取り色透明度2
    '縁取りグラデーション
    Public Property IsFringeGradation As Boolean '縁取りグラデーションの有無
    Public Property IsFringeGammaC As Boolean '縁取りガンマ補正の有無
    Public Property FringeGradationAngle As Integer '縁取りグラデーションの角度

    Public Property FringeWidth As Integer '縁取りの幅


    'その他
    Public Property Image As Image '見本画像
    Public Property ItemName As String 'リストに表示するテキスト

End Class

<Serializable> Class FontSettingList '<serializable>がシリアライズできますよ宣言
    Inherits List(Of FontSetting) 'ジェネリック型？リストを継承

    'Private fList As New List(Of FontSetting)
    'Private NameList As New List(Of String)

    '設定の追加

    'Public Sub Add(fSetting As FontSetting)
    '    fList.Add(fSetting)
    '    NameList.Add(fSetting.Name)

    'End Sub


    'コンボボックスの項目用リストを返す
    Public Function GetNameList() As String()
        'Dim lc As Integer = NameList.Count - 1
        'Dim nl(lc) As String
        'For i As Integer = 0 To lc
        '    nl(i) = NameList.Item(i)
        'Next
        'Return nl
        Dim lc As Integer = Me.Count - 1
        Dim nl(lc) As String
        For i As Integer = 0 To lc
            nl(i) = Me.Item(i).ItemName
        Next
        Return nl

    End Function

    Public Sub Replace入れ替え(CurrentIndex As Integer, ReplaceIndex As Integer)
        Dim c As FontSetting = Me.Item(CurrentIndex)
        Item(CurrentIndex) = Item(ReplaceIndex)
        Item(ReplaceIndex) = c

    End Sub
    '
    'Public Function GetFontSetting(int As Integer) As FontSetting
    '    Dim fs As New FontSetting
    '    fs = fList.Item(int)
    '    Return fs
    'End Function
    'Public Function GetFontSetting(int As Integer) As FontSetting
    '    Dim fs As New FontSetting
    '    fs = Me.Item(int)
    '    Return fs
    'End Function

    'Public Property fs As FontSetting
    'Public ReadOnly Property Item(int As Integer) As FontSetting
    '    Get
    '        Return fList.Item(int)
    '    End Get
    'End Property
End Class

'文字列情報
<Serializable> Public Class StringShape

End Class