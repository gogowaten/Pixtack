Imports System.Drawing.Drawing2D
Public Class FormText

    Private Sub RadioButtonStringH_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonStringH.MouseClick
        Call Form1.TextSample2()
    End Sub

    Private Sub RadioButtonStringV_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonStringV.MouseClick
        Call Form1.TextSample2()
    End Sub
    '文字の色１
    Private Sub ButtonFontColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFontColor.Click
        If Me.ColorDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Me.ButtonFontColor.ForeColor = Me.ColorDialog1.Color
            Call Form1.TextSample2()
        End If
    End Sub
    '文字の色２
    Private Sub ButtonFontColor2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFontColor2.Click
        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.ButtonFontColor2.ForeColor = Me.ColorDialog1.Color
            Call Form1.TextSample2()
        End If
    End Sub

    Private Sub ButtonStringColorChange_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringColorChange.Click
        Dim col1 As Color = Me.ButtonFontColor.ForeColor
        Dim col2 As Color = Me.ButtonFontColor2.ForeColor
        Me.ButtonFontColor.ForeColor = col2
        Me.ButtonFontColor2.ForeColor = col1

        Call Form1.TextSample2()
    End Sub
    'イニシャライズ
    Private Sub FormText_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'ウィンドウの位置
        Dim myPoint As Point
        'myPoint = New Point(1000, 100)
        myPoint = Form1.Location
        'myPoint = New Point(Form1.Location.X + 32, Form1.Location.Y + 32)
        Me.Location = myPoint

        'フォントをコンボボックスに格納
        Dim affs As FontFamily() = FontFamily.Families
        Dim msIndex As Integer
        Dim count As Integer
        For Each c As FontFamily In affs

            If c.IsStyleAvailable(FontStyle.Regular) Then 'レギュラーをサポートしている文字だけ追加
                Me.ComboBoxAllFonts.Items.Add(c.Name)
                count += 1
                If c.Name = "ＭＳ ゴシック" Then
                    msIndex = Me.ComboBoxAllFonts.Items.Count
                End If
            End If

        Next
        '初期選択はＭＳ ゴシック
        Me.ComboBoxAllFonts.SelectedIndex = msIndex - 1
        Call Form1.TextSample2()

    End Sub
    '文字の描画ボタン
    Private Sub ButtonDrawString_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonDrawString.Click
        Dim iName As String = "文字_" & Me.ComboBoxString.Text
        Dim iString As String = Me.ComboBoxString.Text
        Dim bmp As Bitmap

        If Me.CheckBoxStringSplit.Checked = False Then
            bmp = DrawString(iString)
            Call Form1.PicBoxAdd(iName, bmp)
        Else
            '文字列を1文字ごとに分けて描画
            Dim iSprit As String
            For i = 0 To iString.Length - 1
                iSprit = iString.Chars(i)
                bmp = DrawString(iSprit)
                Call Form1.PicBoxAdd("文字_" & iSprit, bmp)

            Next
        End If

        Call StringComboboxItemAdd()
        Me.ComboBoxString.Focus()
    End Sub
    'フォント一覧描画
    Private Sub ButtonAllFontAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonAllFontAdd.Click
        Dim iName As String = "文字_" & Me.ComboBoxString.Text
        Dim iString As String = "abgy、フォント名"
        Dim iFont As Font = New Font(Me.ComboBoxAllFonts.Text, Me.NumericUpDownFontSize.Value, FontStyle.Regular)
        Dim sf As New StringFormat
        'Dim y As Integer = Form1.DrawStringLocate(iFont)
        Dim y As Integer ' = iFont.Size / 10 '描画する縦位置
        Dim bmp As Bitmap

        For i = 0 To Me.ComboBoxAllFonts.Items.Count - 1
            iFont = New Font(Me.ComboBoxAllFonts.Items.Item(i).ToString, 30, FontStyle.Regular)
            '特定のフォントは横書きのみずらさない
            If iFont.Name = "Meiryo UI" AndAlso sf.FormatFlags = 0 Then
                y = 0
            Else
                y = iFont.Size / 10
            End If

            iString = "key_0123456789_文字" & Me.ComboBoxAllFonts.Items.Item(i).ToString

            bmp = Form1.StringDraw6(iString, iFont, sf)
            Dim g As Graphics = Graphics.FromImage(bmp)
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
            g.DrawRectangle(Pens.Red, 0, 0, bmp.Width - 1, bmp.Height - 1)
            g.DrawString(iString, iFont, Brushes.Black, 0, y, sf)
            Call Form1.PicBoxAdd(iName, bmp)

        Next

    End Sub

    Private Sub ButtonStringShadowColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringShadowColor.Click

        If Me.ColorDialog1.ShowDialog = vbOK Then
            Me.ButtonStringShadowColor.ForeColor = Me.ColorDialog1.Color
            Call Form1.TextSample2()
        End If

    End Sub
    '文字のグラデーション角度0
    Private Sub ButtonGradAngle0_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGradAngle0.Click
        Me.NumericUpDownGradationAngle.Value = 0
    End Sub
    '文字のグラデーション角度90
    Private Sub ButtonGradAngleMin15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGradAngleMin15.Click
        Dim int As Integer = Me.NumericUpDownGradationAngle.Value + Me.ButtonGradAngleMin15.Text
        If int < 0 Then
            Me.NumericUpDownGradationAngle.Value = 0
        Else
            Me.NumericUpDownGradationAngle.Value = int
        End If

    End Sub
    '文字のグラデーション角度45
    Private Sub ButtonGradAngleAdd15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGradAngleAdd15.Click
        Dim int As Integer = Me.NumericUpDownGradationAngle.Value + Me.ButtonGradAngleAdd15.Text

        If int > Me.NumericUpDownGradationAngle.Maximum Then
            Me.NumericUpDownGradationAngle.Value = Me.NumericUpDownGradationAngle.Maximum
        Else
            Me.NumericUpDownGradationAngle.Value = int
        End If

    End Sub

    Private Sub ButtonGradAngle90_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGradAngle90.Click
        Me.NumericUpDownGradationAngle.Value = 90
    End Sub
    'コンボボックス内でEnterキーを押したとき音が出ないようにするのと文字の描画
    Private Sub ComboBoxString_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles ComboBoxString.KeyPress

        If e.KeyChar = Microsoft.VisualBasic.ChrW(Keys.Enter) Then
            Dim iName As String = "文字_" & Me.ComboBoxString.Text
            Dim iString As String = Me.ComboBoxString.Text
            Dim bmp As Bitmap

            bmp = DrawString(iString)
            Call Form1.PicBoxAdd(iName, bmp)
            Call StringComboboxItemAdd()
            Me.ComboBoxString.Focus()
            e.Handled = True
        End If

    End Sub
    Friend Function DrawString(ByVal iString As String) As Bitmap
        'Dim iName As String = "文字_" & Me.ComboBoxString.Text
        'Dim iString As String = Me.ComboBoxString.Text

        Dim col1 As Color = Me.ButtonFontColor.ForeColor
        Dim col2 As Color = Me.ButtonFontColor2.ForeColor
        'Dim br1 As New SolidBrush(col1)
        'Dim br2 As New SolidBrush(Me.ButtonFontColor2.ForeColor)


        'Dim iFont As Font = New Font(Me.ComboBoxAllFonts.Text, Me.NumericUpDownFontSize.Value, FontStyle.Italic Or FontStyle.Bold)

        Dim fStyle As FontStyle '斜体、太字
        If Me.CheckBoxTextItalic.Checked AndAlso Me.CheckBoxTextBold.Checked Then
            fStyle = FontStyle.Italic Or FontStyle.Bold
        ElseIf Me.CheckBoxTextBold.Checked Then
            fStyle = FontStyle.Bold
        ElseIf Me.CheckBoxTextItalic.Checked Then
            fStyle = FontStyle.Italic
        End If
        Dim iFont As Font
        iFont = New Font(Me.ComboBoxAllFonts.Text, Me.NumericUpDownFontSize.Value, fStyle)

        If iString = "見本." And Me.NumericUpDownFontSize.Value >= 30 Then '見本の表示用、フォントサイズが30位上なら無視
            iFont = New Font(Me.ComboBoxAllFonts.Text, 30, fStyle)
            iString = "見本"
        ElseIf iString = "見本." And Me.NumericUpDownFontSize.Value < 30 Then '見本．を見本に戻す
            iString = "見本"
        End If

        Dim sf As New StringFormat
        '縦横判定
        Dim stringV As Boolean = False
        If Me.RadioButtonStringV.Checked Then
            sf.FormatFlags = StringFormatFlags.DirectionVertical '縦書
            stringV = True
        End If
        'Dim ore = iFont.Height
        'Dim ore1 = iFont.Unit
        'Dim ore2 = iFont.Style
        'Dim fontfam As New FontFamily(Me.ComboBoxAllFonts.Text)
        'Dim ore3 = fontfam.GetEmHeight(FontStyle.Regular)
        'Dim ore4 = fontfam.GetCellAscent(FontStyle.Regular)
        'Dim ore5 = fontfam.GetCellDescent(FontStyle.Regular)
        'Dim ore6 = fontfam.GetLineSpacing(FontStyle.Regular)
        'Dim ore7 = iFont.Size
        'Dim ore8 = iFont.SizeInPoints
        'Dim ore9 = iFont.GdiCharSet
        'Dim ore10 = iFont.Unit

        'Dim y As Integer = Form1.DrawStringLocate(iFont) '描画する縦位置→やっぱやめた
        Dim offS As Integer = iFont.Size / 10
        '特定のフォントは横書きのみずらさない
        If iFont.Name = "Meiryo UI" AndAlso sf.FormatFlags = 0 Then
            offS = 0
        End If



        '描画する四角形範囲の作成
        Dim bmp As Bitmap
        bmp = Form1.StringDraw6(iString, iFont, sf)
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim rect As Rectangle
        Dim rect2 = New Rectangle(0, 0, bmp.Width - 1, bmp.Height - 1) '楕円用,角丸四角用,四角枠、
        rect = New Rectangle(0, 0, bmp.Width, bmp.Height) '四角形用


        'ブラシ作成
        Dim gradAngle As Integer = Me.NumericUpDownGradationAngle.Value
        Dim iBrush As Brush '文字のブラシ
        Dim dimBrush As Brush '文字の輪郭ぼかし用ブラシ
        Dim dimS As Integer = Me.NumericUpDownDimStrength.Value

        If dimS > 4 Then
            dimS = ((2 ^ dimS) / 4) - 1
        End If

        If Me.CheckBoxTextGradation.Checked Then 'グラデーション
            iBrush = New LinearGradientBrush(rect, col1, col2, gradAngle)
            dimBrush = New LinearGradientBrush(rect, Color.FromArgb(dimS, col1), Color.FromArgb(dimS, col2), gradAngle)

            If Me.CheckBoxStringGamma.Checked Then 'グラデーションガンマ補正
                DirectCast(iBrush, LinearGradientBrush).GammaCorrection = True
                DirectCast(dimBrush, LinearGradientBrush).GammaCorrection = True
            End If

        Else
            iBrush = New SolidBrush(col1) '単色
            dimBrush = New SolidBrush(Color.FromArgb(dimS, col1))

        End If




        'アンチエイリアス判定
        Dim antiA As Boolean = True
        If Me.CheckBoxAntiAlias.Checked = False Then
            antiA = False
        End If
        If antiA Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel 'アンチエイリアスなし
        End If

        'g.DrawRectangle(Pens.Red, 0, 0, bmp.Width - 1, bmp.Height - 1)

        '背景色用ブラシ作成
        Dim bBAngle As Integer = Me.NumericUpDownStringBackGradAngle.Value
        Dim bCol1 As Color = Color.FromArgb(Me.NumericUpDownTextBackTransparent.Value, Me.ButtonTextBackColor1.ForeColor)
        Dim bCol2 As Color = Color.FromArgb(Me.NumericUpDownTextBackTransparent.Value, Me.ButtonTextBackColor2.ForeColor)
        Dim bBrush As Brush
        bBrush = New SolidBrush(bCol1)
        'グラデーション
        If Me.CheckBoxTextBackC_Gradation.Checked Then
            bBrush = New LinearGradientBrush(rect, bCol1, bCol2, bBAngle)
            'グラデーションガンマ補正
            If Me.CheckBoxTextBackC_Gamma.Checked Then
                DirectCast(bBrush, LinearGradientBrush).GammaCorrection = True
            End If

        End If

        '背景色塗り
        '背景色と枠あり
        Dim b As Single = Me.NumericUpDownStringBackFrameBold.Value '枠の太さ
        Dim fCol1 As Color = Me.ButtonStringBackFrameCol1.ForeColor
        Dim fCol2 As Color = Me.ButtonStringBackFrameCol2.ForeColor
        Dim fBrush As Brush
        fBrush = New SolidBrush(Color.FromArgb(255, fCol1)) '枠ブラシ


        If Me.RadioButtonStringBackSquare.Checked Then '四角形
            rect2 = New Rectangle(b, b, rect.Width - (b * 2), rect.Height - (b * 2))

            If Me.CheckBoxStringBackFrame.Checked AndAlso Me.CheckBoxStringBackColor.Checked Then
                g.FillRectangle(Brushes.Transparent, rect) '大きい外側塗り
                g.FillRectangle(bBrush, rect2) '小さい内側塗り

                Dim reg As [Region]
                reg = New [Region](rect)
                'reg.Exclude(rect2)
                reg.Xor(rect2)
                'reg.Union(rect2)
                'reg.Complement(rect2)
                'reg.Intersect(rect2)
                g.FillRegion(fBrush, reg)
            ElseIf Me.CheckBoxStringBackFrame.Checked Then '枠だけ
                Dim gp As New GraphicsPath
                gp.AddRectangle(rect)
                gp.AddRectangle(rect2)
                g.FillPath(fBrush, gp)
                gp.Dispose()

            ElseIf Me.CheckBoxStringBackColor.Checked Then '枠なし塗り
                g.FillRectangle(bBrush, rect)

            End If
            fBrush.Dispose()
            bBrush.Dispose()


        End If


        If Me.RadioButtonStringBackEllipse.Checked Then '楕円
            rect2 = New Rectangle(b, b, rect.Width - (b * 2), rect.Height - (b * 2))

            g.SmoothingMode = SmoothingMode.AntiAlias
            g.PixelOffsetMode = PixelOffsetMode.Half
            Dim gp As New GraphicsPath

            If Me.CheckBoxStringBackFrame.Checked Then '枠あり
                gp.AddEllipse(rect) '枠作成の外側
                gp.AddEllipse(rect2) '枠作成の内側
                g.FillPath(fBrush, gp) '枠の塗り、枠完成

            End If

            If Me.CheckBoxStringBackColor.Checked Then '背景色あり
                gp.AddEllipse(rect) '枠の内側の塗り用
                'g.SmoothingMode = SmoothingMode.None
                g.PixelOffsetMode = PixelOffsetMode.Half
                g.FillPath(bBrush, gp) '内側塗り
                g.PixelOffsetMode = PixelOffsetMode.None

            End If
            gp.Dispose()

        End If

        If Me.RadioButtonStringBackRoundrect.Checked Then '角丸四角

            Dim d As Single = Me.TrackBarStringBackRoundrect.Value
            Dim gp As New GraphicsPath

            If Me.CheckBoxStringBackFrame.Checked Then '枠あり

                gp = Form1.FrameRoundRectGPath(gp, rect, b, fBrush, d)
                g.PixelOffsetMode = PixelOffsetMode.Half
                g.SmoothingMode = SmoothingMode.AntiAlias
                g.FillPath(fBrush, gp)
                g.PixelOffsetMode = PixelOffsetMode.None


                'pen
                'Dim iPen As Pen
                'iPen = New Pen(Brushes.Black, b)
                'gp = Form1.FrameRoundRectGPath2(gp, rect, b, fBrush, d)
                'g.PixelOffsetMode = PixelOffsetMode.Half
                'g.SmoothingMode = SmoothingMode.AntiAlias
                'g.DrawPath(iPen, gp)
                'g.PixelOffsetMode = PixelOffsetMode.None

            End If

            If Me.CheckBoxStringBackColor.Checked Then '背景色あり

                gp = Form1.RoundRectGPath(gp, rect, d) 'gpは枠ありのモノからの引き継ぎ
                g.PixelOffsetMode = PixelOffsetMode.Half
                g.SmoothingMode = SmoothingMode.AntiAlias
                g.FillPath(bBrush, gp)
                g.PixelOffsetMode = PixelOffsetMode.None
            End If

            gp.Dispose()

        End If
        '背景の描画ここまで









        'ここから文字の描画
        '文字の縁取り
        Dim bold As Integer = Me.NumericUpDownStringFringe2.Value
        Dim fringeCol As Color = Color.FromArgb(Me.TrackBarStringFringeColT.Value, Me.ButtonStringFringeColor2.ForeColor)
        Dim fringeB As New SolidBrush(fringeCol)
        'Dim fringeB As New SolidBrush(Me.ButtonStringFringeColor2.ForeColor)
        If bold <> 0 Then
            Call Form1.StringFringe4(bmp, iString, iFont, sf, fringeB, offS, bold, antiA, stringV)
        End If
        fringeB.Dispose()



        '影の位置
        Dim sx As Integer = 1
        Dim sy As Integer = 1

        '影の判定して描画
        Dim shadowB As New SolidBrush(Me.ButtonStringShadowColor.ForeColor)
        'Dim shadow As Boolean
        If Me.CheckBoxStringShadow.Checked Then
            bmp = Form1.StringShadow(bmp, iString, iFont, sf, shadowB, offS, sx, sy, antiA, stringV)
        End If
        shadowB.Dispose()


        '縦横判定して描画位置決定
        Dim drawX As Integer
        Dim drawY As Integer
        If stringV = False Then
            drawX = 0
            drawY = offS
        Else
            drawX = -offS
            drawY = 0
        End If

        '文字の輪郭ぼかし
        If Me.CheckBoxDim.Checked Then
            Dim d As Integer = Me.NumericUpDownDim.Value
            bmp = Form1.StringDim(bmp, iString, iFont, sf, dimBrush, offS, d, antiA, stringV)

        End If



        '文字の描画
        g.DrawString(iString, iFont, iBrush, drawX, drawY, sf)


        '回転
        Dim rAngle As Single = Me.NumericUpDownStringAngle.Value
        If rAngle <> 0 Then
            bmp = Form1.PicAngle(bmp, -rAngle)
        End If

        g.Dispose()
        iFont.Dispose()
        sf.Dispose()

        Return bmp

        'Call Form1.PicBoxAdd(iName, bmp)

    End Function
    'コンボボックスにフォーカスが来たら文字を選択状態にする
    Private Sub ComboBoxString_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxString.Enter
        Me.ComboBoxString.SelectAll()

    End Sub
    '文字入力コンボボックスに同じ文字がなければ追加
    Private Sub StringComboboxItemAdd()
        Dim iStr As String = Me.ComboBoxString.Text

        For Each c As String In Me.ComboBoxString.Items
            If c = iStr Then
                Exit Sub 'コンボボックスに同じ文字があればなにもしないで終了
            End If
        Next

        'コンボボックスに文字を追加
        Me.ComboBoxString.Items.Insert(0, iStr)

    End Sub
    '文字の背景色１選択
    Private Sub ButtonTextBackColor1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTextBackColor1.Click

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.ButtonTextBackColor1.ForeColor = Me.ColorDialog1.Color
            Call Form1.TextSample2()

        End If

    End Sub
    '文字の背景色２選択
    Private Sub ButtonTextBackColor2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTextBackColor2.Click
        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.ButtonTextBackColor2.ForeColor = Me.ColorDialog1.Color
            Call Form1.TextSample2()

        End If
    End Sub
    '文字の背景色１と２の入れ替え
    Private Sub ButtonStringBackColorChange_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringBackColorChange.Click
        Dim col As Color = Me.ButtonTextBackColor1.ForeColor
        Me.ButtonTextBackColor1.ForeColor = Me.ButtonTextBackColor2.ForeColor
        Me.ButtonTextBackColor2.ForeColor = col
        Call Form1.TextSample2()

    End Sub

   
    Private Sub TrackBarStringBackRoundrect_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarStringBackRoundrect.Scroll
        Me.LabelStringBackRoundrect.Text = Me.TrackBarStringBackRoundrect.Value
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub ButtonStringTransparent0_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringTransparent0.Click
        Me.NumericUpDownTextBackTransparent.Value = 0
    End Sub

    Private Sub ButtonStringTransparent128_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringTransparent128.Click
        Me.NumericUpDownTextBackTransparent.Value = 128
    End Sub

    Private Sub ButtonStringTransparent255_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringTransparent255.Click
        Me.NumericUpDownTextBackTransparent.Value = 255
    End Sub


    Private Sub ButtonStringBackFrameCol1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringBackFrameCol1.Click

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.ButtonStringBackFrameCol1.ForeColor = Me.ColorDialog1.Color
            Call Form1.TextSample2()

        End If

    End Sub

    Private Sub ButtonStringBackFrameCol2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringBackFrameCol2.Click
        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.ButtonStringBackFrameCol2.ForeColor = Me.ColorDialog1.Color
            Call Form1.TextSample2()

        End If
    End Sub

    Private Sub ButtonStringBackFrameColChange_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringBackFrameColChange.Click
        Dim col As Color = Me.ButtonStringBackFrameCol1.ForeColor
        Me.ButtonStringBackFrameCol1.ForeColor = Me.ButtonStringBackFrameCol2.ForeColor
        Me.ButtonStringBackFrameCol2.ForeColor = col
        Call Form1.TextSample2()

    End Sub

    '縁取り文字の色ダイアログ
    Private Sub ButtonStringFringeColor2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringFringeColor2.Click

        If Me.ColorDialog1.ShowDialog = vbOK Then
            Me.ButtonStringFringeColor2.ForeColor = Me.ColorDialog1.Color
            Call Form1.TextSample2()

        End If

    End Sub
    '書式の適用
    Private Sub ButtonStringShift_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringShift.Click
       
        Call Form1.StringDrawShift2()

    End Sub

    Private Sub NumericUpDownFontSize_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownFontSize.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub ComboBoxAllFonts_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxAllFonts.SelectedIndexChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub NumericUpDownStringAngle_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownStringAngle.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next
    End Sub

    Private Sub NumericUpDownStringFringe2_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownStringFringe2.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next
    End Sub

    Private Sub NumericUpDownStringBackFrameBold_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownStringBackFrameBold.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next
    End Sub

    Private Sub NumericUpDownStringBackGradAngle_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownStringBackGradAngle.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next
    End Sub

    Private Sub NumericUpDownTextBackTransparent_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownTextBackTransparent.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next
    End Sub

    Private Sub NumericUpDownGradationAngle_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownGradationAngle.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next
    End Sub

    Private Sub CheckBoxTextItalic_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextItalic.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxAntiAlias_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxAntiAlias.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxTextBold_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextBold.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxStringShadow_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringShadow.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxTextGradation_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextGradation.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxStringGamma_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringGamma.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub RadioButtonStringBackSquare_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonStringBackSquare.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub RadioButtonStringBackEllipse_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonStringBackEllipse.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub RadioButtonStringBackRoundrect_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonStringBackRoundrect.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxStringBackColor_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringBackColor.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxStringBackFrame_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringBackFrame.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxTextBackC_Gradation_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextBackC_Gradation.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub CheckBoxTextBackC_Gamma_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextBackC_Gamma.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next

    End Sub

    Private Sub TrackBarStringFringeColT_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarStringFringeColT.Scroll
        Me.LabelStringFringeColT.Text = Me.TrackBarStringFringeColT.Value
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next
    End Sub

    Private Sub CheckBoxDim_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxDim.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "FormText" Then
                Call Form1.TextSample2()
            End If
        Next
    End Sub

    '文字の描画のフォームを閉じる時、設定ファルに書き込み
    Private Sub FormText_FormClosing(ByVal sender As System.Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles MyBase.FormClosing
        Dim iPath As String = My.Application.Info.DirectoryPath '設定ファイルのパス取得
        Dim wr As New IO.StreamWriter(iPath & "\Pixtack.txt")
        

        Dim col1 As Color = Me.ButtonTextBackColor1.ForeColor
        Dim col As String = col1.R & "," & col1.G & "," & col1.B
        wr.WriteLine("[ButtonTextBackColor1]")
        wr.WriteLine(col)

        wr.Close()
    End Sub
End Class