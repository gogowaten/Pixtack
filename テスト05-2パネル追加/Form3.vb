Imports System.Drawing.Drawing2D
Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Drawing.Imaging

Public Class Form3

    Friend myFormHistgram As FormHistogram = New FormHistogram
    Friend HalfRealtimeRendering As Boolean = True '半自動再描画のフラグ
    Private formSizeMini As Boolean = False 'フォームの伸び縮みの状態判定用
    Private formHeight As Long 'フォームの初期の高さ
    'Private DrawPicCount As Long '図形2で作成した画像のカウント、画像に名前を付けて識別する用
    Private startP As Point 'ラベルのドラッグ移動開始ポイント記録用
    Private isOreドラッグ中 As Boolean 'ラベルドラッグの判定
    '    Private isOre編集中 As Boolean '画像の編集中の判定
    Public EditNowPic As ExPictureBox '編集中のExPictureBox
    Private FontSLフォントの設定リスト As FontSettingList
    Friend isOpenedForm3 As Boolean '自身が開ききったかの判定

    Private Sub Form3_Shown(sender As Object, e As EventArgs) Handles MyBase.Shown
        isOpenedForm3 = True 'フォームが開いたよフラグ
    End Sub

    Private Sub Form3_FormClosed(sender As Object, e As FormClosedEventArgs) Handles MyBase.FormClosed
        isOpenedForm3 = False 'これは必要ないかも？
    End Sub


    '文字の描画
    Friend Sub StringDraw()
        
        'Call Form1.StringDraw4()

        'Dim mySt As String = Me.ComboBoxString.Text

        ''履歴に同じ文字列がなければ追加
        'Dim flag As Boolean = True
        'For Each c As String In Me.ComboBoxString.Items
        '    If c = mySt Then
        '        flag = False
        '    End If
        'Next
        'If flag Then
        '    'Me.ComboBoxString.Items.Add(Me.ComboBoxString.Text)
        '    Me.ComboBoxString.Items.Insert(0, Me.ComboBoxString.Text)
        'End If

        'Me.ComboBoxString.Focus()
        'Me.ComboBoxString.SelectAll()
    End Sub



    '元の画像から半透明にする
    Private Sub ButtonAbsTra_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonAbsTra.Click
        Call Form1.AbsColMatrixAlpha()
    End Sub

    '画像を元に戻す
    Private Sub ButtonColReset_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonColReset.Click
        'Call Form1.PicReset()
        Call Form1.PicRestor()
    End Sub
    '透過色の取得
    Private Sub PictureBoxTranspCol_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBoxTranspCol.MouseDown

        If Form1.FlagColor Then
            Form1.FlagColor = False
            Me.Cursor = Cursors.Default
            Form1.Cursor = Cursors.Default
            Form1.FlagForm3GetColor = False
        ElseIf Form1.FlagColor = False Then
            Me.Cursor = Cursors.Cross
            Form1.Cursor = Cursors.Cross
            Form1.FlagColor = True
            Form1.FlagForm3GetColor = True
        End If

    End Sub

    '今の画像から指定色で透過
    Private Sub ButtonTranspCol_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTranspCol.Click
        If Me.PictureBoxTranspCol.BackColor <> Color.FromKnownColor(KnownColor.Control) Then
            Form1.AlphaforOneColor()
        End If
    End Sub

    '今の画像から半透明にする
    Private Sub ButtonAddTra_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonAddTra.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.AddColMatrixAlpha()
        Me.Cursor = Cursors.Default
    End Sub

    'バイリニア法で拡大縮小
    Private Sub ButtonBilinear_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonBilinear.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.Bilinear()
        Call Form1.Transparent2()
        Me.Cursor = Cursors.Default
    End Sub

    'ニアレストネイバー法で拡大縮小
    Private Sub ButtonNearestNeighbor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonNearestNeighbor.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.NearestNeighbor()
        Call Form1.Transparent2()
        Me.Cursor = Cursors.Default

    End Sub

    'ハイクオリティバイキュービック法で拡大縮小
    Private Sub ButtonHighQualityBicubic_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHighQualityBicubic.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.HighQualityBicubic()
        Call Form1.Transparent2()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Call Form1.ColorMatrix()

    End Sub

    Private Sub ButtonGrayScale_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGrayScale.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.GrayScale()
        Me.Cursor = Cursors.Default
    End Sub


    Private Sub Button4sides_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4sides.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.Transparent4sides2()
        Me.Cursor = Cursors.Default
    End Sub




    Private Sub ButtonGamma_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGamma.Click
        Call Form1.Gamma()

    End Sub

    Private Sub ButtonBrightness1bpp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonBrightness1bpp.Click
        Me.Cursor = Cursors.WaitCursor

        Call Form1.Brightness1bpp()
        Me.Cursor = Cursors.Default

    End Sub

    Private Sub ButtonRandom1bpp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRandom1bpp.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.BrightnessRandom1bpp()
        Me.Cursor = Cursors.Default

    End Sub

    '右に90度回転
    Private Sub ButtonRotate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRotateR.Click
        Call Form1.RotateRight90()

    End Sub

    '左に90度回転
    Private Sub ButtonRotateL_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRotateL.Click
        Call Form1.RotateLeft90()
    End Sub

    '左右反転
    Private Sub ButtonFlipX_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFlipX.Click
        Call Form1.RotateFlipX()

    End Sub

    '文字の描画
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        'Call StringDraw()

    End Sub

    Private Sub Form3_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Dim g As Graphics = Me.PictureBoxTranspCol.CreateGraphics
        'Dim ffs As FontFamily() = FontFamily.GetFamilies(g)
        'For Each c As FontFamily In ffs
        '    ComboBoxFont.Items.Add(c.Name)
        'Next
        

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
        formHeight = Me.Height '初期の高さ記憶

        'コンボボックス設定

        'Dim ooo = [Enum].GetNames(GetType(ExPictureBox.DrawType)) 'stringの配列
        'Dim oooo As Array = [Enum].GetValues(GetType(ExPictureBox.DrawType)) 'これは列挙型の配列(整数)
        'Dim ooooo = System.Enum.GetNames(GetType(ExPictureBox.DrawType)) 'これもstringの配列
        Dim tc = [Enum].GetNames(GetType(ExPictureBox.DrawType))
        Me.ComboBoxLineType線の種類.Items.AddRange(tc)
        Me.ComboBoxLineType線の種類.SelectedIndex = 0

        Dim ec As String() = [Enum].GetNames(GetType(ExPictureBox.PenCap))
        'Dim ecn = [Enum].GetValues(GetType(ExPictureBox.PenCap))

        Me.ComboBoxEndCap.Items.AddRange(ec)
        Me.ComboBoxEndCap.SelectedIndex = 0
        Me.ComboBoxStartCap.Items.AddRange(ec)
        Me.ComboBoxStartCap.SelectedIndex = 0

        'Dim fm As String() = [Enum].GetNames(GetType(Drawing2D.FillMode))
        Dim fm As String() = [Enum].GetNames(GetType(ExPictureBox.GraphicFillType))
        Me.ComboBoxFillMode塗りモード.Items.AddRange(fm)
        Me.ComboBoxFillMode塗りモード.SelectedIndex = 0

        Dim lj As String() = [Enum].GetNames(GetType(ExPictureBox.PenLineJoin角の形状))
        Me.ComboBoxPenLineJoin角の形状.Items.AddRange(lj)
        Me.ComboBoxPenLineJoin角の形状.SelectedIndex = 0
        '各コンボボックス設定ここまで

        '色変えのカラーバー作成
        Dim bmp As New Bitmap(Me.PictureBoxHue.Width, Me.PictureBoxHue.Height)
        Dim col As Color
        For y As Integer = 0 To bmp.Height - 1
            For x As Integer = 0 To bmp.Width - 1
                col = Form1.HSLtoRGB(y * 2, 1, 0.5)
                bmp.SetPixel(x, y, col)
            Next
        Next
        Me.PictureBoxHue.Image = bmp
        Dim oldColorPic As PictureBox = Me.PictureBoxHueOld
        Dim newColorPic As PictureBox = Me.PictureBoxHueNew
        oldColorPic.BackColor = Form1.HSLtoRGB(TrackBarOppositeトラックバーの反対数値(Me.TrackBarHueOld), 1, 0.5)
        newColorPic.BackColor = Form1.HSLtoRGB(TrackBarOppositeトラックバーの反対数値(Me.TrackBarHueNew), 1, 0.5)
        Dim oldH, newH As Integer
        oldH = CInt(Math.Round(oldColorPic.BackColor.GetHue))
        Me.ToolTip1.SetToolTip(oldColorPic, "色相_" & oldH & vbNewLine & oldColorPic.BackColor.ToString)

        newH = CInt(Math.Round(newColorPic.BackColor.GetHue))
        Me.ToolTip1.SetToolTip(newColorPic, "色相_" & newH & vbNewLine & newColorPic.BackColor.ToString)
       
        Call ChangeSaturationColorBar彩度カラーバー更新(Me.PictureBoxSaturationOld, oldH, Me.TrackBarBrightnessOld.Value / 100)
        Call ChangeBrightnessColorBar明度カラーバー更新(Me.PictureBoxBrightnessOld, oldH, Me.TrackBarSaturationOld.Value / 100)

        Call ChangeSaturationColorBar彩度カラーバー更新(Me.PictureBoxSaturationNew, newH, Me.TrackBarBrightnessNew.Value / 100)
        Call ChangeBrightnessColorBar明度カラーバー更新(Me.PictureBoxBrightnessNew, newH, Me.TrackBarSaturationNew.Value / 100)


    End Sub
    Friend Function TrackBarOppositeトラックバーの反対数値(tb As TrackBar) As Integer
        Return tb.Maximum - tb.Value
    End Function
    Friend Sub SetTrackBarOppositeトラックバーに反対数値をセット(tb As TrackBar, val As Integer)
        tb.Value = tb.Maximum - val
    End Sub
    'カラーマトリックスの数値のReset
    Private Sub ButtonCMReset_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCMReset.Click
        Me.NumericUpDownCM00.Value = 1
        Me.NumericUpDownCM10.Value = 0
        Me.NumericUpDownCM20.Value = 0
        Me.NumericUpDownCM30.Value = 0
        Me.NumericUpDownCM40.Value = 0
        Me.NumericUpDownCM01.Value = 0
        Me.NumericUpDownCM11.Value = 1
        Me.NumericUpDownCM21.Value = 0
        Me.NumericUpDownCM31.Value = 0
        Me.NumericUpDownCM41.Value = 0
        Me.NumericUpDownCM02.Value = 0
        Me.NumericUpDownCM12.Value = 0
        Me.NumericUpDownCM22.Value = 1
        Me.NumericUpDownCM32.Value = 0
        Me.NumericUpDownCM42.Value = 0
    End Sub

    'カラーマトリックスをグレースケールに設定
    Private Sub ButtonCMGrayScale_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCMGrayScale.Click
        Me.NumericUpDownCM00.Value = 0.3
        Me.NumericUpDownCM10.Value = 0.6
        Me.NumericUpDownCM20.Value = 0.1
        Me.NumericUpDownCM30.Value = 0
        Me.NumericUpDownCM40.Value = 0
        Me.NumericUpDownCM01.Value = 0.3
        Me.NumericUpDownCM11.Value = 0.6
        Me.NumericUpDownCM21.Value = 0.1
        Me.NumericUpDownCM31.Value = 0
        Me.NumericUpDownCM41.Value = 0
        Me.NumericUpDownCM02.Value = 0.3
        Me.NumericUpDownCM12.Value = 0.6
        Me.NumericUpDownCM22.Value = 0.1
        Me.NumericUpDownCM32.Value = 0
        Me.NumericUpDownCM42.Value = 0
        Call Form1.ColorMatrix()
    End Sub

    'カラーマトリックスをセピア色に設定
    Private Sub ButtonCMSepia_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCMSepia.Click
        Me.NumericUpDownCM00.Value = 0.3
        Me.NumericUpDownCM10.Value = 0.6
        Me.NumericUpDownCM20.Value = 0.1
        Me.NumericUpDownCM30.Value = 0
        Me.NumericUpDownCM40.Value = 0.2
        Me.NumericUpDownCM01.Value = 0.3
        Me.NumericUpDownCM11.Value = 0.6
        Me.NumericUpDownCM21.Value = 0.1
        Me.NumericUpDownCM31.Value = 0
        Me.NumericUpDownCM41.Value = 0.1
        Me.NumericUpDownCM02.Value = 0.3
        Me.NumericUpDownCM12.Value = 0.6
        Me.NumericUpDownCM22.Value = 0.1
        Me.NumericUpDownCM32.Value = 0
        Me.NumericUpDownCM42.Value = -0.3
        Call Form1.ColorMatrix()
    End Sub

    Private Sub RedGrayScale_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRedGrayScale.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.RedGrayScaleLockBits()

        'Call Form1.RedGrayScale()
        Me.Cursor = Cursors.Default

    End Sub

    Private Sub ButtonFontColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFontColor.Click
        Me.ColorDialog1.FullOpen = True
        'If Me.ColorDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
        '    Me.ButtonFontColor.ForeColor = Me.ColorDialog1.Color
        '    Me.PictureBoxTextColor1.BackColor = ColorDialog1.Color
        '    Call Form1.TextSample()
        'End If
        ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxTextColor1, True)

    End Sub

    Private Sub ButtonStringFringe_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringFringe.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.StringFringe()
        Me.Cursor = Cursors.Default

    End Sub

    Private Sub ButtonFringeColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFringeColor.Click
        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.ButtonFringeColor.ForeColor = Me.ColorDialog1.Color
        End If
    End Sub


    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.StringFringe2()
        Me.Cursor = Cursors.Default
    End Sub
    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Call Form1.StringFringe3()
    End Sub

    '枠作成
    Private Sub ButtonFrameAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFrameAdd.Click
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ
        Dim pw As Integer = Me.NumericUpDownPenWidth.Value '枠の太さ

        Dim col As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent.Value, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent2.Value, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent3.Value, Me.ButtonSquareColor3.ForeColor)

        'Call Form1.FlameAdd(rw, rh, pw, col, col2, col3)
        Dim bmp As Bitmap

        bmp = Form1.FlameAdd(rw, rh, pw, col, col2, col3)
        Dim name As String = "枠"
        Call Form1.PicBoxAdd(name, bmp)
    End Sub
    '四角作成の1色め
    Private Sub ButtonFlameColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonSquareColor1.Click

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim col As Color = Me.ColorDialog1.Color
            Me.ButtonSquareColor1.ForeColor = col
            'Me.LabelShapeColor1.Text = col.R & " " & col.G & " " & col.B
            Me.LabelShapeColor1.Text = col.R.ToString("D3") & " " & col.G.ToString("D3") & " " & col.B.ToString("d3")
            Me.PictureBoxShapeColor1.BackColor = col
            Dim hsv As String = Form1.RGBtoHLS(col)
            Me.LabelShapeColor1HSV.Text = hsv

        End If
        Call Form1.SquareSample()
    End Sub
    '四角作成の2色め
    Private Sub ButtonSquareColor2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonSquareColor2.Click

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim col As Color = Me.ColorDialog1.Color
            Me.ButtonSquareColor2.ForeColor = Me.ColorDialog1.Color
            Me.LabelShapeColor2.Text = col.R.ToString("D3") & " " & col.G.ToString("D3") & " " & col.B.ToString("d3")
            Me.PictureBoxShapeColor2.BackColor = col

        End If
        Call Form1.SquareSample()
    End Sub
    '四角作成の3色め
    Private Sub ButtonSquareColor3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonSquareColor3.Click
        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim col As Color = Me.ColorDialog1.Color
            Me.ButtonSquareColor3.ForeColor = Me.ColorDialog1.Color
            Me.LabelShapeColor3.Text = col.R.ToString("D3") & " " & col.G.ToString("D3") & " " & col.B.ToString("d3")
            Me.PictureBoxShapeColor3.BackColor = col

        End If
        Call Form1.SquareSample()
    End Sub
    '四角形作成
    Private Sub ButtonRectangleAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRectangleAdd.Click
        Dim name As String = "四角形" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ
        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = Me.NumericUpDownSquareTransparent2.Value
        Dim transparent3 As Integer = Me.NumericUpDownSquareTransparent3.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(transparent2, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(transparent3, Me.ButtonSquareColor3.ForeColor)
        Dim bmp As New Bitmap(Form1.RectangleAdd(rw, rh, col, col2, col3))
        Call Form1.PicBoxAdd(name, bmp)
        'Call Form1.RectangleAdd(rw, rh, col, transparent)
    End Sub

    Private Sub ButtonAntiAliasText_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTranspGradation.Click
        Me.Cursor = Cursors.WaitCursor
        'Call Form1.StringAntiAliasDraw()
        Call Form1.TransparentGradation()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        Call Form1.StringFringeAntiAlias()

    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        Call Form1.StringFringeAntiAlias2()

    End Sub

    'Private Sub TextBoxString_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxString.Enter
    '    Me.TextBoxString.SelectAll() 'フォーカスしたらテキストを選択状態にする
    'End Sub

    Private Sub NumericUpDownRectWidth_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownRectWidth.Enter
        Me.NumericUpDownRectWidth.Select(0, 3) 'フォーカスしたら数値を選択状態にする
    End Sub

    Private Sub NumericUpDownRectHeight_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownRectHeight.Enter
        Me.NumericUpDownRectHeight.Select(0, 3)
    End Sub

    Private Sub NumericUpDownPenWidth_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownPenWidth.Enter
        Me.NumericUpDownPenWidth.Select(0, 3)
    End Sub
    '透明グラデーション2
    Private Sub TranspGradation_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTranspGradation2.Click
        Me.Cursor = Cursors.WaitCursor
        'Call Form1.StringAntiAliasDrawClearType()
        Call Form1.TransparentGradation2()
        Me.Cursor = Cursors.Default
    End Sub


    '選択画像の大きさ取得、セット
    Private Sub ButtonRectSize_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRectSize.Click
        Call Form1.RectSize()

    End Sub
    ''テキストボックス内でEnterキーを押したとき
    'Private Sub TextBoxString_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBoxString.KeyDown
    '    If e.KeyData = Keys.Enter Then

    '        If Me.CheckBoxAntiAlias.Checked Then
    '            Call Form1.StringAntiAliasDraw() 'アンチエイリアス
    '        Else
    '            Call Form1.StringDraw3()
    '        End If

    '        Me.TextBoxString.Focus()
    '        Me.TextBoxString.SelectAll()
    '    End If
    'End Sub

    ''テキストボックス内でEnterキーを押したとき音が出ないようにする
    'Private Sub TextBoxString_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBoxString.KeyPress

    '    If e.KeyChar = Microsoft.VisualBasic.ChrW(Keys.Enter) Then
    '        e.Handled = True
    '    End If

    'End Sub

    'Ctrl＋sで画像を保存
    Private Sub Form3_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyDown
        If e.KeyData = Keys.Control + Keys.S Then
            Call Form1.SavePic()
        End If
    End Sub


    '文字の描画のコンボボックス用
    Private Sub ComboBoxString_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles ComboBoxString.KeyDown
        If e.KeyData = Keys.Enter Then
            Dim cb As ComboBox = Me.ComboBoxString
            Dim str As String = cb.Text
            Me.TextBoxMultilineString.Text = str
            If str = "" Then Exit Sub
            Call DrawString複数行文字列の描画(str)

            'Dim statusStr As String = str.Replace(vbCrLf, "") 'ステータス表示用、画像の名前

            'Dim bmp As Bitmap = GetDrawStringBmp文字の描画作成(str, CreateFontフォント作成())
            'Call Form1.PicBoxAdd("文字_" & statusStr, bmp, drawString:=str)
            cb.Focus()
            cb.SelectAll()

            '描画する文字列をコンボボックスに追加する
            For Each c As String In cb.Items
                If c = str Then Exit Sub '同じ文字列があれば追加しないで終了
            Next
            cb.Items.Insert(0, str) '追加！


            'Call StringDraw()


        End If
    End Sub

    'コンボボックス内でEnterキーを押したとき音が出ないようにする
    Private Sub ComboBoxString_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles ComboBoxString.KeyPress

        If e.KeyChar = Microsoft.VisualBasic.ChrW(Keys.Enter) Then
            e.Handled = True
        End If
    End Sub
    'コンボボックスがフォーカスされたらテキストを選択状態にする
    Private Sub ComboBoxString_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxString.Enter
        Me.ComboBoxString.SelectAll()

    End Sub
    Private Sub ComboBoxString_MouseDoubleClick(sender As System.Object, e As System.Windows.Forms.MouseEventArgs) Handles ComboBoxString.MouseDoubleClick
        Me.ComboBoxString.SelectAll() '動かないーなんで？
    End Sub

    Private Sub ButtonTranspGradation3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTranspGradation3.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.TransparentGradation3()
        Me.Cursor = Cursors.Default

    End Sub

    Private Sub ButtonTranspGradation4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTranspGradation4.Click
        Me.Cursor = Cursors.WaitCursor

        Call Form1.TransparentGradation4()
        Me.Cursor = Cursors.Default

    End Sub
    '透明グラデーション斜めLockBitsバージョン
    Private Sub ButtonTranspGradation4LockBit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTranspGradation4LockBit.Click
        Call Form1.TransparentGradation4LockBit()

    End Sub
    '緑以外白黒
    Private Sub ButtonGreenGrayScale_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGreenGrayScale.Click
        Me.Cursor = Cursors.WaitCursor

        Call Form1.GreenGrayScale()
        Me.Cursor = Cursors.Default

    End Sub
    '青以外白黒
    Private Sub ButtonBlueGrayscale_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonBlueGrayscale.Click
        Me.Cursor = Cursors.WaitCursor

        Call Form1.BlueGrayScale()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub TrackBarTranspGradation_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarTranspGradation.Scroll
        Dim val As Integer = Me.TrackBarTranspGradation.Value
        val = Math.Abs(val - 11)
        Me.LabelTranspGradationRange.Text = val * 10 & "％"
    End Sub
    '入れ替え、書式の適用
    Private Sub ButtonStringShift_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonButtonStringShift2.Click
        Call Form1.StringDrawShift()
        If Me.CheckBoxSelectPicFrameView選択時に枠表示.Checked Then
            Call Form1.PicBoderLineLabel画像に枠を作成表示(Form1.ActExPic)
        End If
    End Sub

    '緑以外白黒2
    Private Sub ButtonGreenGrayScale2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGreenGrayScale2.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.GreenGrayScale2()
        Me.Cursor = Cursors.Default
    End Sub
    '青以外白黒2
    Private Sub ButtonBlueGrayscale2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonBlueGrayscale2.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.BlueGrayScale2()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub ButtonRedGrayScale2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRedGrayScale2.Click
        Me.Cursor = Cursors.WaitCursor
        Call Form1.RedGrayScale2()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub TrackBarRed_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarRed.Scroll
        Me.Cursor = Cursors.WaitCursor
        Call Form1.RGBGrayScale()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub TrackBarGreen_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarGreen.Scroll
        Me.Cursor = Cursors.WaitCursor
        Call Form1.RGBGrayScale()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub TrackBarBlue_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarBlue.Scroll
        Me.Cursor = Cursors.WaitCursor
        Call Form1.RGBGrayScale()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub RadioButtonRed_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonRed.CheckedChanged


    End Sub

    'Private Sub RadioButtonGreen_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonGreen.CheckedChanged
    '    If Me.RadioButtonGreen.Checked Then
    '        With Me
    '            .TrackBarRed.Value = 100
    '            .TrackBarGreen.Value = 100
    '            .TrackBarBlue.Value = 100
    '            .TrackBarRed.Enabled = True
    '            .TrackBarGreen.Enabled = False
    '            .TrackBarBlue.Enabled = True
    '        End With
    '    End If
    '    'Call Form1.RGBGrayScale()
    'End Sub

    'Private Sub RadioButtonBlue_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonBlue.CheckedChanged
    '    If Me.RadioButtonBlue.Checked Then
    '        With Me
    '            .TrackBarRed.Value = 100
    '            .TrackBarGreen.Value = 100
    '            .TrackBarBlue.Value = 100
    '            .TrackBarRed.Enabled = True
    '            .TrackBarGreen.Enabled = True
    '            .TrackBarBlue.Enabled = False
    '        End With
    '    End If
    '    'Call Form1.RGBGrayScale()
    'End Sub

    Private Sub RadioButtonRed_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonRed.MouseClick
        If Me.RadioButtonRed.Checked Then
            With Me
                With .TrackBarRed
                    .Value = 100
                    .BackColor = Color.FromKnownColor(KnownColor.Control)
                    .Enabled = False
                End With
                With .TrackBarGreen
                    .Value = 100
                    .BackColor = Color.FromKnownColor(KnownColor.Chartreuse)
                    .Enabled = True
                End With

                With .TrackBarBlue
                    .Value = 100
                    .Enabled = True
                    .BackColor = Color.FromKnownColor(KnownColor.DodgerBlue)
                End With

            End With
            Call Form1.RGBGrayScale()
        End If
    End Sub

    Private Sub RadioButtonGreen_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonGreen.MouseClick
        If Me.RadioButtonGreen.Checked Then
            With Me.TrackBarRed
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Tomato)
                .Enabled = True
            End With
            With Me.TrackBarGreen
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Control)
                .Enabled = False
            End With
            With Me.TrackBarBlue
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.DodgerBlue)
                .Enabled = True
            End With
            Call Form1.RGBGrayScale()
        End If
    End Sub

    Private Sub RadioButtonBlue_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonBlue.MouseClick
        If Me.RadioButtonBlue.Checked Then
            With Me.TrackBarRed
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Tomato)
                .Enabled = True
            End With
            With Me.TrackBarGreen
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Chartreuse)
                .Enabled = True
            End With
            With Me.TrackBarBlue
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Control)
                .Enabled = False
            End With
            Call Form1.RGBGrayScale()
        End If
    End Sub

    Private Sub RadioButtonYellow_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonYellow.MouseClick
        If Me.RadioButtonBlue.Checked Then
            With Me.TrackBarRed
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Tomato)
                .Enabled = True
            End With
            With Me.TrackBarGreen
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Chartreuse)
                .Enabled = True
            End With
            With Me.TrackBarBlue
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Control)
                .Enabled = False
            End With
            Call Form1.RGBGrayScale()
        End If
    End Sub

    Private Sub RadioButtonYellow_MouseClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonYellow.MouseClick
        If Me.RadioButtonYellow.Checked Then
            With Me.TrackBarRed
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Tomato)
                .Enabled = True
            End With
            With Me.TrackBarGreen
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Chartreuse)
                .Enabled = True
            End With
            With Me.TrackBarBlue
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Control)
                .Enabled = False
            End With
            Call Form1.RGBGrayScale()
        End If
    End Sub

    Private Sub RadioButtonAqua_MouseClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonAqua.MouseClick
        If Me.RadioButtonAqua.Checked Then
            With Me.TrackBarRed
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Control)
                .Enabled = False
            End With
            With Me.TrackBarGreen
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Chartreuse)
                .Enabled = True
            End With
            With Me.TrackBarBlue
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.DodgerBlue)
                .Enabled = True
            End With
            Call Form1.RGBGrayScale()
        End If
    End Sub

    Private Sub RadioButtonMagenta_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonMagenta.MouseClick
        If Me.RadioButtonMagenta.Checked Then
            With Me.TrackBarRed
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Tomato)
                .Enabled = True
            End With
            With Me.TrackBarGreen
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.Control)
                .Enabled = False
            End With
            With Me.TrackBarBlue
                .Value = 100
                .BackColor = Color.FromKnownColor(KnownColor.DodgerBlue)
                .Enabled = True
            End With
            Call Form1.RGBGrayScale()
        End If
    End Sub

    Private Sub ButtonFontColor2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFontColor2.Click

        'If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
        '    Me.ButtonFontColor2.ForeColor = Me.ColorDialog1.Color
        '    Me.PictureBoxTextColor2.BackColor = Me.ColorDialog1.Color
        '    Call Form1.TextSample()
        'End If
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxTextColor2, True)

    End Sub

    '-----------------------ここから文字の描画の見本の更新用-------------------------
    Private Sub ComboBoxAllFonts_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxAllFonts.TextChanged

        'Call Form1.TextSample()
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Call Form1.TextSample()
            End If
        Next
    End Sub

    Private Sub CheckBoxAntiAlias_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringAntiAlias.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Call Form1.TextSample()
            End If
        Next

    End Sub

    Private Sub CheckBoxTextBold_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextBold.CheckedChanged
        'Call Form1.TextSample()
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub

    Private Sub CheckBoxTextItalic_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextItalic.CheckedChanged
        'Call Form1.TextSample()
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub

    Private Sub CheckBoxTextGradation_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextGradation.CheckedChanged
        If CheckBoxTextGradation.Checked = False Then
            CheckBoxStringGamma.Enabled = False
        Else
            CheckBoxStringGamma.Enabled = True
        End If

        Call OpenFrom3の起動完了チェックして文字の見本の再描画()

        'For Each c As Form In Application.OpenForms
        '    If c.Name = "Form3" Then
        '        Call Form1.TextSample()
        '        Exit For
        '    End If
        'Next


    End Sub

    'Private Sub RadioButtonHorizontal_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
    '    Call Form1.TextSample()
    'End Sub

    'Private Sub RadioButtonVertical_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
    '    Call Form1.TextSample()
    'End Sub

    'Private Sub RadioButtonLeftUp_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
    '    Call Form1.TextSample()
    'End Sub

    'Private Sub RadioButtonRightUp_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
    '    Call Form1.TextSample()
    'End Sub

    Private Sub RadioButtonStringH_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonStringH.MouseClick
        Call Form1.TextSample()
    End Sub

    Private Sub RadioButtonStringV_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles RadioButtonStringV.MouseClick
        Call Form1.TextSample()
    End Sub

    Private Sub NumericUpDownFontSize_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownFontSize.ValueChanged
        ''これはなぜか上のほうでは起動時にエラーになる
        ''If Me.GroupBoxTextStyle.Enabled Then
        ''    Call Form1.TextSample()
        ''End If
        'If Me.TabControl1.Contains(GroupBoxTextStyle) Then
        '    Call Form1.TextSample()
        'End If
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Call Form1.TextSample()
            End If
        Next
    End Sub
    Private Sub NumericUpDownStringAngle_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownStringAngle.ValueChanged
        'これもエラーになるかも？
        'If Me.TabControl1.Contains(TabPageDrawString) Then
        '    Call Form1.TextSample()

        'End If
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Call Form1.TextSample()
            End If
        Next
    End Sub

    Private Sub RadioButtonTextBackC_LeftUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonTextBackC_LeftUp.Click
        Call Form1.TextSample()
    End Sub

    Private Sub RadioButtonTextBackC_Horizontal_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonTextBackC_Horizontal.Click
        Call Form1.TextSample()
    End Sub

    Private Sub RadioButtonTextBackC_RightUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonTextBackC_RightUp.Click
        Call Form1.TextSample()
    End Sub

    Private Sub RadioButtonTextBackC_Vertical_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonTextBackC_Vertical.Click
        Call Form1.TextSample()
    End Sub

    Private Sub CheckBoxTextBackColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextBackColor背景色の有無.Click
        Call Form1.TextSample()
    End Sub

    Private Sub NumericUpDownTextTransparent1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownTextTransparent1.ValueChanged '.Click

        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                If Me.CheckBoxLinkedStringBGTransparent.Checked Then '背景色透明度連動
                    Me.NumericUpDownTextTransparent2.Value = Me.NumericUpDownTextTransparent1.Value
                    Call Form1.TextSample()
                    Exit Sub
                End If

                Call Form1.TextSample()
            End If
        Next

    End Sub
    Private Sub NumericUpDown背景色透明度_ValueChange(sender As NumericUpDown, e As EventArgs) Handles NumericUpDownTextTransparent2.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                If Me.CheckBoxLinkedStringBGTransparent.Checked Then '背景色透明度連動
                    Me.NumericUpDownTextTransparent1.Value = Me.NumericUpDownTextTransparent2.Value
                    Exit Sub
                End If

                Call Form1.TextSample()
            End If
        Next

    End Sub
    Private Sub CheckBoxTextBackC_Gamma_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextBackC_Gamma.CheckedChanged
        'Call Form1.TextSample()
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub
    Private Sub CheckBoxStringShadow_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringShadow.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Call Form1.TextSample()
            End If
        Next
    End Sub
    '-----------------------文字の描画の見本の更新用ここまで-------------------------
    '文字の背景色1
    Private Sub ButtonTextBackColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTextBackColor1.Click
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxStringBGColor1, CheckBoxTextBackColor背景色の有無.Checked)

    End Sub
    '文字の背景色2
    Private Sub ButtonTextBackColor2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTextBackColor2.Click
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxStringBGColor2, CheckBoxTextBackColor背景色の有無.Checked)
    End Sub
    '文字の背景色透明度チェックボックス
    Private Sub CheckBoxTextBackColor_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextBackColor背景色の有無.CheckedChanged


        'If Me.CheckBoxTextBackColor背景色の有無.Checked Then
        '    For Each c As Control In Me.GroupBoxTextBackColor.Controls
        '        c.Enabled = True
        '    Next

        '    If Me.CheckBoxTextBackC_Gradation.Checked = False Then
        '        Me.ButtonTextBackColor2.Enabled = False
        '    End If

        'ElseIf Me.CheckBoxTextBackColor背景色の有無.Checked = False Then
        '    For Each c As Control In Me.GroupBoxTextBackColor.Controls
        '        c.Enabled = False
        '    Next
        '    Me.CheckBoxTextBackColor背景色の有無.Enabled = True
        'End If
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()

    End Sub

    Private Sub CheckBoxTextBackC_Gradation_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTextBackC_Gradation.CheckedChanged

        'If Me.CheckBoxTextBackC_Gradation.Checked Then
        '    Me.ButtonTextBackColor2.Enabled = True
        'Else
        '    Me.ButtonTextBackColor2.Enabled = False
        'End If
        If CheckBoxTextBackColor背景色の有無.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If
        'Call Form1.TextSample()

    End Sub



    '透明グラデーションの強さのラベル
    Private Sub TrackBarTransparentStrength_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarTransparentStrength.Scroll
        Dim val As Integer = Me.TrackBarTransparentStrength.Value
        val = Math.Abs(val - 11)
        Me.LabelTransparentStrength.Text = val
    End Sub

    Private Sub TrackBarTransparentStrength_MouseWheel(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TrackBarTransparentStrength.MouseWheel

    End Sub

    Private Sub ButtonZoom_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonZoom.Click
        Call Form1.ZoomPicAdd(0)

    End Sub

    '透明グラデーション
    Private Sub ButtonTransparentGradationEnter_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTransparentGradationEnter.Click
        Select Case True
            Case Me.RadioButtonTransprentGradationLeft.Checked
                Form1.TransparentGradation2()
            Case Me.RadioButtonTransprentGradationLeftRight.Checked
                Form1.TransparentGradation3()
            Case Me.RadioButtonTransprentGradationLeftUp.Checked
                Form1.TransparentGradation4LockBit()

        End Select


    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Call Form1.ZoomPicAddEachRange()

    End Sub

    Private Sub ButtonStringFringe2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringFringeKonjaku.Click

        If Me.CheckBoxStringFringe2.Checked Then

            Call Form1.StringFringeGPath()
        End If


    End Sub
    '文字の縁取りの色
    Private Sub ButtonStringFringeColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringFringeColor.Click

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.ButtonStringFringeColor.ForeColor = Me.ColorDialog1.Color
        End If

    End Sub
    '文字の角度
    Private Sub CheckBoxStringAngle_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringAngle.CheckedChanged

        If Me.CheckBoxStringAngle.Checked Then
            Me.NumericUpDownStringAngle.Enabled = True
        Else
            Me.NumericUpDownStringAngle.Enabled = False
        End If
        'Call Form1.TextSample()
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub

    Private Sub CheckBoxStringFringe_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringFringe2.CheckedChanged

        If Me.CheckBoxStringFringe2.Checked Then
            Me.ButtonStringFringeKonjaku.Enabled = True
        Else
            Me.ButtonStringFringeKonjaku.Enabled = False
        End If

    End Sub

    Private Sub ButtonZoomR_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonZoomR.Click
        Call Form1.ZoomPicAdd(1)

    End Sub
    '四角形の書式適用
    Private Sub ButtonRectShift_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRectShift.Click




        'Dim name As String = Form1.FocusPic.Name
        'If Not name.EndsWith("四角形_T") Then
        '    Exit Sub
        'End If

        'Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        'Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ
        'Dim col As Color = Me.ButtonSquareColor1.ForeColor
        'Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        'Dim bmp = New Bitmap(Form1.RectangleAdd(rw, rh, col, transparent))
        Call Form1.RectangleShift()

    End Sub

    '----------------------------四角形の見本更新ここから------------------------
    Private Sub RadioButtonSquareGradaH_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonSquareGradaH.Click
        Call Form1.SquareSample()

    End Sub

    Private Sub RadioButtonSquareGradaRUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonSquareGradaRUp.Click
        Call Form1.SquareSample()
    End Sub

    Private Sub RadioButtonSquareGradaV_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonSquareGradaV.Click
        Call Form1.SquareSample()
    End Sub

    Private Sub RadioButtonSquareGradaLUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonSquareGradaLUp.Click
        Call Form1.SquareSample()
    End Sub

    Private Sub NumericUpDownSquareTransparent_Enter(sender As Object, e As System.EventArgs) Handles NumericUpDownSquareTransparent.Enter
        Me.NumericUpDownSquareTransparent.Select(0, 3)

    End Sub

    Private Sub NumericUpDownSquareTransparent_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownSquareTransparent.ValueChanged
        'これなら起動するけど毎回エラーになっている

        'Try
        'Call Form1.SquareSample()

        'Catch ex As Exception
        '    MsgBox(ex.ToString)
        'End Try

        ''これで行けた
        'If Me.TabControl1.Contains(Me.GroupBoxSquareAdd) Then
        '    Call Form1.SquareSample()
        'End If


        If Me.CheckBoxSquareTransparentLink.Checked Then
            Me.NumericUpDownSquareTransparent2.Value = Me.NumericUpDownSquareTransparent.Value
        End If

    End Sub

    Private Sub NumericUpDownRectangleAngle_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownRectangleAngle.ValueChanged
        ''どちらでも行ける
        ''If Me.TabControl1.Contains(Me.GroupBoxSquareAdd) Then
        ''    Call Form1.SquareSample()
        ''End If
        'If Me.GroupBoxSquareAdd.Enabled Then
        'Call Form1.SquareSample()
        'End If
    End Sub
    Private Sub CheckBoxRectGradationGamma_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxRectGradationGamma.CheckedChanged
        Call Form1.SquareSample()
    End Sub
    Private Sub CheckBoxSquareGradation_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxSquareGradation.CheckedChanged
        Call Form1.SquareSample()
    End Sub
    '----------------------------四角形の見本更新ここまで------------------------


    Private Sub ButtonPicAngle_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonPicAngle.Click
        Call Form1.RotateAngle()

    End Sub

    Private Sub ButtonBackupOverWrite_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonBackupOverWrite.Click
        Call Form1.BackupOverWrite()

    End Sub
    '楕円作成
    Private Sub ButtonFillEllipseAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFillEllipseAdd.Click

        Dim name As String = "楕円" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ

        If rw < 1 OrElse rh < 1 Then
            Exit Sub
        End If

        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = Me.NumericUpDownSquareTransparent2.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(transparent2, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent3.Value, Me.ButtonSquareColor3.ForeColor)

        Dim bmp As New Bitmap(Form1.EllipseAdd4(rw, rh, col, col2, col3))
        'Dim bmp As New Bitmap(Form1.EllipseAdd(rw, rh, col, col2))
        Call Form1.PicBoxAdd(name, bmp)

    End Sub

    '---------------------30倍に拡大ここから----------------------------------
    Private Sub ButtonZoomG_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonZoomG.Click

        Call Form1.ZoomPicAdd(2)
    End Sub

    Private Sub ButtonZoomFontColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonZoomFontColor.Click

        If Me.ButtonZoomFontColor.ForeColor = Color.Black Then
            Me.ButtonZoomFontColor.ForeColor = Color.White
            'Me.ButtonZoomFontColor.BackColor = Color.Black
        Else
            Me.ButtonZoomFontColor.ForeColor = Color.Black
            'Me.ButtonZoomFontColor.BackColor = Color.White
        End If

    End Sub

    Private Sub ButtonZoomB_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonZoomB.Click
        Call Form1.ZoomPicAdd(3)
    End Sub

    Private Sub ButtonZoom30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonZoom30.Click
        Call Form1.ZoomPicAdd()
    End Sub
    Private Sub ButtonZoomAll_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonZoomAll.Click
        Call Form1.ZoomPicAllAdd()

    End Sub
    '---------------------30倍に拡大ここまで----------------------------------


    Private Sub CheckBoxSquareTransparentLink_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxSquareTransparentLink.CheckedChanged

        If Me.CheckBoxSquareTransparentLink.Checked Then
            Me.NumericUpDownSquareTransparent2.Value = Me.NumericUpDownSquareTransparent.Value
        End If

    End Sub

    Private Sub NumericUpDownSquareTransparent2_Enter(sender As Object, e As System.EventArgs) Handles NumericUpDownSquareTransparent2.Enter
        Me.NumericUpDownSquareTransparent2.Select(0, 3)

    End Sub

    Private Sub NumericUpDownSquareTransparent2_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownSquareTransparent2.ValueChanged
        If Me.CheckBoxSquareTransparentLink.Checked Then
            Me.NumericUpDownSquareTransparent.Value = Me.NumericUpDownSquareTransparent2.Value
        End If
        'Call Form1.SquareSample()

    End Sub
    Private Sub NumericUpDownSquareTransparent3_Enter(sender As System.Object, e As System.EventArgs) Handles NumericUpDownSquareTransparent3.Enter
        Me.NumericUpDownSquareTransparent3.Select(0, 3)
    End Sub

    Private Sub ButtonTriangle_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTriangle.Click
        Dim name As String = "三角" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ

        If rw < 1 OrElse rh < 1 Then
            Exit Sub
        End If

        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = Me.NumericUpDownSquareTransparent2.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(transparent2, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent3.Value, Me.ButtonSquareColor3.ForeColor)

        Dim bmp As New Bitmap(Form1.TriangleAdd(rw, rh, col, col2, col3))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'Call Form1.GradationAdd()

        Dim name As String = "四角形" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ
        Dim col As Color = Me.ButtonSquareColor1.ForeColor
        Dim col2 As Color = Me.ButtonSquareColor2.ForeColor
        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = Me.NumericUpDownSquareTransparent2.Value
        Dim bmp As New Bitmap(Form1.RectangleAddGomakasi(rw, rh, col, col2, transparent, transparent2))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim name As String = "四角形" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ
        Dim col As Color = Me.ButtonSquareColor1.ForeColor
        Dim col2 As Color = Me.ButtonSquareColor2.ForeColor
        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = Me.NumericUpDownSquareTransparent2.Value
        Dim bmp As New Bitmap(Form1.RectangleAddHantaiColor(rw, rh, col, col2, transparent, transparent2))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    Private Sub TrackBarShapeGamma_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarShapeGamma.Scroll
        Me.LabelShapeGamma.Text = Me.TrackBarShapeGamma.Value / 10
        Call Form1.SquareSample()

    End Sub
    '楕円作成2
    Private Sub ButtonFillEllipseAdd2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim name As String = "楕円" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ

        If rw < 1 OrElse rh < 1 Then
            Exit Sub
        End If

        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim bmp As New Bitmap(Form1.EllipseAdd2(rw, rh, col, transparent))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    Private Sub ButtonFillEllipseAdd3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim name As String = "楕円" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ

        If rw < 1 OrElse rh < 1 Then
            Exit Sub
        End If

        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim bmp As New Bitmap(Form1.EllipseAdd3(rw, rh, col, transparent))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    Private Sub ButtonTriangle2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim name As String = "三角" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ

        If rw < 1 OrElse rh < 1 Then
            Exit Sub
        End If

        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(transparent, Me.ButtonSquareColor2.ForeColor)
        Dim bmp As New Bitmap(Form1.TriangleAdd2(rw, rh, col, col2, transparent))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    Private Sub ButtonTriangle3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim name As String = "三角" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ

        If rw < 1 OrElse rh < 1 Then
            Exit Sub
        End If

        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(transparent, Me.ButtonSquareColor2.ForeColor)
        Dim bmp As New Bitmap(Form1.TriangleAdd3(rw, rh, col, col2, transparent))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub
    'ガンマ補正赤
    Private Sub TrackBarShapeGammaR_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarShapeGammaR.Scroll
        Me.LabelShapeGammaR.Text = Me.TrackBarShapeGammaR.Value / 10
        Call Form1.SquareSample()

    End Sub
    'ガンマ補正緑
    Private Sub TrackBarShapeGammaG_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarShapeGammaG.Scroll
        Me.LabelShapeGammaG.Text = Me.TrackBarShapeGammaG.Value / 10
        Call Form1.SquareSample()
    End Sub
    'ガンマ補正青
    Private Sub TrackBarShapeGammaB_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarShapeGammaB.Scroll
        Me.LabelShapeGammaBlue.Text = Me.TrackBarShapeGammaB.Value / 10
        Call Form1.SquareSample()
    End Sub
    'ガンマ補正赤緑青チェック
    Private Sub CheckBoxShapeGammaRGB_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxShapeGammaRGB.CheckedChanged

        If Me.CheckBoxShapeGammaRGB.Checked Then
            Me.TrackBarShapeGammaB.Enabled = True
            Me.TrackBarShapeGammaG.Enabled = True
            Me.TrackBarShapeGammaR.Enabled = True
            Me.TrackBarShapeGamma.Enabled = False
            Me.LabelShapeGamma.Enabled = False
            Me.LabelShapeGammaR.Enabled = True
            Me.LabelShapeGammaG.Enabled = True
            Me.LabelShapeGammaBlue.Enabled = True
        Else
            Me.TrackBarShapeGammaB.Enabled = False
            Me.TrackBarShapeGammaG.Enabled = False
            Me.TrackBarShapeGammaR.Enabled = False
            Me.TrackBarShapeGamma.Enabled = True
            Me.LabelShapeGamma.Enabled = True
            Me.LabelShapeGammaR.Enabled = False
            Me.LabelShapeGammaG.Enabled = False
            Me.LabelShapeGammaBlue.Enabled = False
        End If
        Call Form1.SquareSample()

    End Sub
    '図形作成の色1と色2の入れ替えボタン
    Private Sub ButtonShapeColorChange_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonShapeColorChange.Click
        Dim col1 As Color = Me.ButtonSquareColor1.ForeColor
        Dim col2 As Color = Me.ButtonSquareColor2.ForeColor
        Me.ButtonSquareColor1.ForeColor = col2
        Me.ButtonSquareColor2.ForeColor = col1
        Me.LabelShapeColor1.Text = col2.R.ToString("D3") & " " & col2.G.ToString("D3") & " " & col2.B.ToString("d3")
        Me.LabelShapeColor2.Text = col1.R.ToString("D3") & " " & col1.G.ToString("D3") & " " & col1.B.ToString("d3")
        Me.PictureBoxShapeColor1.BackColor = col2
        Me.PictureBoxShapeColor2.BackColor = col1

        Call Form1.SquareSample()

    End Sub

    '文字の描画の色1と色2の入れ替えボタン
    Private Sub ButtonStringColorChange_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringBackColorChange.Click
        Dim col1 As Color = PictureBoxStringBGColor1.BackColor
        'Dim col2 As Color = Me.ButtonTextBackColor2.ForeColor
        'Me.ButtonTextBackColor1.ForeColor = col2
        'Me.ButtonTextBackColor2.ForeColor = col1
        Me.PictureBoxStringBGColor1.BackColor = PictureBoxStringBGColor2.BackColor
        Me.PictureBoxStringBGColor2.BackColor = col1

        Dim tv As Integer = NumericUpDownTextTransparent1.Value
        NumericUpDownTextTransparent1.Value = NumericUpDownTextTransparent2.Value
        NumericUpDownTextTransparent2.Value = tv

        If CheckBoxTextBackColor背景色の有無.Checked Then
            Call Form1.TextSample()
        End If

    End Sub
    '色の取得、図形作成の
    Private Sub PictureBoxShapeColor1_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBoxShapeColor1.MouseDown
        If Form1.FlagColor Then
            Form1.FlagColor = False
            Me.Cursor = Cursors.Default
            Form1.Cursor = Cursors.Default
            Form1.FlagFrom3ShapeColor1 = False
        ElseIf Form1.FlagColor = False Then
            Dim sp As Cursor
            sp = New Cursor("E:\オレ\アイコン\マウスカーソル\スポイト1.cur")
            Me.Cursor = sp

            'Me.Cursor = Cursors.Cross
            Form1.Cursor = sp ' Cursors.Cross
            Form1.FlagColor = True
            Form1.FlagFrom3ShapeColor1 = True
        End If
    End Sub
    Private Sub RGB表示更新(sender As PictureBox, e As EventArgs) Handles PictureBoxShapeColor1.BackColorChanged
        Dim col As Color = sender.BackColor
        LabelShapeColor1.Text = col.R.ToString("D") & " " & col.G.ToString("D") & " " & col.B.ToString("D")
        ButtonSquareColor1.ForeColor = col


    End Sub
    '色の取得、図形作成の
    Private Sub PictureBoxShapeColor2_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBoxShapeColor2.MouseDown
        If Form1.FlagColor Then
            Form1.FlagColor = False
            Me.Cursor = Cursors.Default
            Form1.Cursor = Cursors.Default
            Form1.FlagFrom3ShapeColor2 = False
        ElseIf Form1.FlagColor = False Then
            Me.Cursor = Cursors.Cross
            Form1.Cursor = Cursors.Cross
            Form1.FlagColor = True
            Form1.FlagFrom3ShapeColor2 = True
        End If

    End Sub
    'Blend.Factors Positionsグラデーションのテスト
    Private Sub ButtonRectangleAddBlend_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRectangleAddBlend.Click
        Dim name As String = "四角形" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ
        Dim col As Color = Me.ButtonSquareColor1.ForeColor
        Dim col2 As Color = Me.ButtonSquareColor2.ForeColor
        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = Me.NumericUpDownSquareTransparent2.Value
        Dim bmp As New Bitmap(Form1.RectangleAddBlend(rw, rh, col, col2, transparent, transparent2))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    '文字背景色の不透明度数値指定0
    Private Sub ButtonStringTransparent0_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringTransparent0.Click
        Me.NumericUpDownTextTransparent1.Value = 0
        Call Form1.TextSample()

    End Sub
    '文字背景色の不透明度数値指定128
    Private Sub ButtonStringTransparent128_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringTransparent128.Click
        Me.NumericUpDownTextTransparent1.Value = 128
        Call Form1.TextSample()
    End Sub
    '文字背景色の不透明度数値指定255
    Private Sub ButtonStringTransparent255_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringTransparent255.Click
        Me.NumericUpDownTextTransparent1.Value = 255
        Call Form1.TextSample()
    End Sub

    Private Sub ButtonRectRange_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRectRange.Click
        Call Form1.CloseEdit編集終了()
        Call Form1.MouseEndDrawマウスで描画終了処理()
        Call Form1.RectRangeSelect範囲選択用画像表示()

    End Sub

    Private Sub NumericUpDownRectRangeH_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownRectRangeH.ValueChanged
        'If Not Form1.Contains(Form1.Panel2) Then
        '    Exit Sub

        'End If


        ''Form3がすでに開かれていたら何もしない
        'For Each f As Form In Form1.OwnedForms
        '    'For Each f As Form In Application.OpenForms
        '    If f.Name = "Form3" Then
        '        Exit Sub
        '    End If
        'Next


        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Dim rw As Integer = Me.NumericUpDownRectRangeH.Value
                Dim rh As Integer = Me.NumericUpDownRectRangeV.Value
                Call Form1.RectRangeSelectRealTime(rw, rh)

            End If

        Next
        'Call Form1.RectRangeSelect()

    End Sub

    Private Sub NumericUpDownRectRangeV_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDownRectRangeV.ValueChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Dim rw As Integer = Me.NumericUpDownRectRangeH.Value
                Dim rh As Integer = Me.NumericUpDownRectRangeV.Value
                Call Form1.RectRangeSelectRealTime(rw, rh)
            End If
        Next
    End Sub

   
    Private Sub ButtonRectRangeCopyPaste_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRectRangeCopyPaste.Click
        'Call Form1.RectRangeCopyPaste()
        Call Form1.SelectRangeCopyPaste()
        '範囲選択画像を一番上にする
        Call Form1.SelectPicBoxFront範囲選択画像を最前面にする()

    End Sub

    Private Sub PictureBoxRangeSelectColor_MouseClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBoxRangeSelectColor.MouseClick

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.PictureBoxRangeSelectColor.BackColor = Color.FromArgb(128, Me.ColorDialog1.Color)
            Dim rw As Integer = Me.NumericUpDownRectRangeH.Value
            Dim rh As Integer = Me.NumericUpDownRectRangeV.Value
            Call Form1.RectRangeSelectRealTime(rw, rh)
        End If

    End Sub

    Private Sub ButtonSelectRangeSizeReset_Click(sender As Object, e As EventArgs) Handles ButtonSelectRangeSizeReset.Click
        Me.NumericUpDownRectRangeH.Value = 128
        Me.NumericUpDownRectRangeV.Value = 128
    End Sub

    Private Sub ButtonRangeSelectGetSize_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRangeSelectGetSize.Click
        Dim rectSize As Size = Form1.GetSize

        If rectSize.Width = 0 OrElse rectSize.Height = 0 Then
            Exit Sub

        End If

        If rectSize.Width > Me.NumericUpDownRectRangeH.Maximum OrElse rectSize.Height > Me.NumericUpDownRectRangeV.Maximum Then
            MsgBox("選択画像が大きすぎてサイズ取得できません")
            Exit Sub

        End If

        Me.NumericUpDownRectRangeH.Value = rectSize.Width
        Me.NumericUpDownRectRangeV.Value = rectSize.Height

    End Sub

    Private Sub RadioButtonRactRange_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonRactRange.CheckedChanged

        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Dim rw As Integer = Me.NumericUpDownRectRangeH.Value
                Dim rh As Integer = Me.NumericUpDownRectRangeV.Value
                Call Form1.RectRangeSelectRealTime(rw, rh)
            End If
        Next
    End Sub
    '選択範囲のコピペ楕円
    Private Sub RadioButtonEllipseRange_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonEllipseRange.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Dim rw As Integer = Me.NumericUpDownRectRangeH.Value
                Dim rh As Integer = Me.NumericUpDownRectRangeV.Value
                Call Form1.RectRangeSelectRealTime(rw, rh)
            End If
        Next
    End Sub

    Private Sub CheckBoxReverseRange_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxReverseRange.CheckedChanged
        For Each c As Form In Application.OpenForms
            If c.Name = "Form3" Then
                Dim rw As Integer = Me.NumericUpDownRectRangeH.Value
                Dim rh As Integer = Me.NumericUpDownRectRangeV.Value
                Call Form1.RectRangeSelectRealTime(rw, rh)
            End If
        Next

    End Sub

    Private Sub ButtonGrayScaleNTSC_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGrayScaleNTSC.Click
        Call Form1.GrayScaleNTSC()

    End Sub

    Private Sub Button8_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        Call Form1.GrayScaleHDTV()

    End Sub

    Private Sub TrackBarGrayScaleGamma_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarGrayScaleGamma.Scroll
        Me.LabelGrayScaleGamma.Text = Me.TrackBarGrayScaleGamma.Value / 10

    End Sub

    Private Sub TrackBarGrayScaleGamma2_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarGrayScaleGamma2.Scroll
        Me.LabelGrayScaleGamma2.Text = Me.TrackBarGrayScaleGamma2.Value / 10
    End Sub

    Private Sub ButtonGrayScaleCenter_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGrayScaleCenter.Click
        Call Form1.GrayScaleCenter()

    End Sub

    Private Sub ButtonGrayScaleAverage_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGrayScaleAverage.Click
        Call Form1.GrayScaleAverage()

    End Sub

    Private Sub ButtonSelectRangeSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonSelectRangeSave.Click
        Call Form1.SelectRangeSave()

    End Sub

    Private Sub ButtonGammaLockBits_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonGammaLockBits.Click
        Call Form1.GammaLockBit()

    End Sub

    Private Sub ButtonSaturation_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonSaturation.Click
        Call Form1.SaturationLockBit()

    End Sub


    Private Sub RadioButtonSquareGradaUD_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonSquareGradaUD.Click
        Call Form1.SquareSample()
    End Sub


    Private Sub RadioButtonSquareGradaLR_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonSquareGradaLR.Click
        Call Form1.SquareSample()
    End Sub

    Private Sub RadioButtonSquareGradaLU2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonSquareGradaLU2.Click
        Call Form1.SquareSample()
    End Sub

    Private Sub RadioButtonSquareGradaRU2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonSquareGradaRU2.Click
        Call Form1.SquareSample()
    End Sub


    Private Sub PictureBoxShapeColor3_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBoxShapeColor3.MouseDown
        If Form1.FlagColor Then
            Form1.FlagColor = False
            Me.Cursor = Cursors.Default
            Form1.Cursor = Cursors.Default
            Form1.FlagFrom3ShapeColor3 = False
        ElseIf Form1.FlagColor = False Then
            Me.Cursor = Cursors.Cross
            Form1.Cursor = Cursors.Cross
            Form1.FlagColor = True
            Form1.FlagFrom3ShapeColor3 = True
        End If
    End Sub

    Private Sub RadioButtonShape2ColorGrada_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonShape2ColorGrada.Click
        Call Form1.SquareSample()

    End Sub

    Private Sub RadioButtonShape3ColorGrada_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonShape3ColorGrada.Click
        Call Form1.SquareSample()

    End Sub

    Private Sub ButtonShapeColorChange2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonShapeColorChange2.Click
        Dim col2 As Color = Me.ButtonSquareColor2.ForeColor
        Dim col3 As Color = Me.ButtonSquareColor3.ForeColor
        Me.ButtonSquareColor2.ForeColor = col3
        Me.ButtonSquareColor3.ForeColor = col2
        Me.LabelShapeColor2.Text = col3.R.ToString("D3") & " " & col3.G.ToString("D3") & " " & col3.B.ToString("d3")
        Me.LabelShapeColor3.Text = col2.R.ToString("D3") & " " & col2.G.ToString("D3") & " " & col2.B.ToString("d3")
        Me.PictureBoxShapeColor2.BackColor = col3
        Me.PictureBoxShapeColor3.BackColor = col2

        Call Form1.SquareSample()
    End Sub

    Private Sub ButtonStringColorChange_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonStringColorChange.Click
        Dim col1 As Color = PictureBoxTextColor1.BackColor
        'Dim col2 As Color = Me.ButtonFontColor2.ForeColor
        'Me.ButtonFontColor.ForeColor = col2
        'Me.ButtonFontColor2.ForeColor = col1
        Me.PictureBoxTextColor1.BackColor = PictureBoxTextColor2.BackColor
        Me.PictureBoxTextColor2.BackColor = col1

        Dim tv As Integer = NumericUpDownStringColorT1_不透明度.Value
        NumericUpDownStringColorT1_不透明度.Value = NumericUpDownStringColorT2_不透明度.Value
        NumericUpDownStringColorT2_不透明度.Value = tv
        Call Form1.TextSample()
    End Sub

    Private Sub CheckBoxStringGamma_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxStringGamma.CheckedChanged

        Call OpenFrom3の起動完了チェックして文字の見本の再描画()

    End Sub

    Private Sub RadioButtonStringBackSquare_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonStringBackSquare.Click
        'Call Form1.TextSample()
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()

    End Sub

    Private Sub RadioButtonStringBackEllipse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonStringBackEllipse.Click
        'Call Form1.TextSample()
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub


    Private Sub ButtonRoundRectanglAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRoundRectanglAdd.Click
        Dim name As String = "角丸四角"
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value
        If rw < 2 OrElse rh < 2 Then
            Exit Sub
        End If

        Dim diameter As Single = Me.TrackBarRoundRect.Value
        Dim col As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent.Value, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent2.Value, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent3.Value, Me.ButtonSquareColor3.ForeColor)

        Dim bmp As New Bitmap(Form1.RoundRectAdd(rw, rh, diameter, col, col2, col3))

        Call Form1.PicBoxAdd(name, bmp)

    End Sub
    '角丸四角の丸さ
    Private Sub TrackBarRoundRect_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarRoundRect.Scroll
        Me.LabelRoundRect.Text = Me.TrackBarRoundRect.Value

    End Sub

    Private Sub ButtonTriangle2_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTriangle2.Click
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value
        Dim col As Color = Me.ButtonSquareColor1.ForeColor
        Dim col2 As Color = Me.ButtonSquareColor2.ForeColor
        Dim tp As Integer = Me.NumericUpDownSquareTransparent.Value

        Dim bmp As New Bitmap(Form1.TriangleAdd2(rw, rh, col, col2, tp))
        Call Form1.PicBoxAdd("test", bmp)



    End Sub

    Private Sub ButtonTriangle3_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTriangle3.Click
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value
        Dim col As Color = Me.ButtonSquareColor1.ForeColor
        Dim col2 As Color = Me.ButtonSquareColor2.ForeColor
        Dim tp As Integer = Me.NumericUpDownSquareTransparent.Value

        Dim bmp As New Bitmap(Form1.TriangleAdd3(rw, rh, col, col2, tp))
        Call Form1.PicBoxAdd("test", bmp)
    End Sub

    Private Sub ButtonRectAddtest_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRectAddtest.Click
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value
        Dim col As Color = Me.ButtonSquareColor1.ForeColor
        Dim col2 As Color = Me.ButtonSquareColor2.ForeColor
        Dim bmp As New Bitmap(rw + 1, rh)
        Dim g As Graphics = Graphics.FromImage(bmp)

        g.PixelOffsetMode = Drawing2D.PixelOffsetMode.Half
        'g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias

        Dim gp As New Drawing2D.GraphicsPath
        gp.AddRectangle(New Rectangle(0, 0, rw, rh))
        'gp.AddEllipse(New Rectangle(0, 0, rw, rh))
        Dim pBrush As New Drawing2D.PathGradientBrush(gp)
        pBrush.CenterColor = Color.Red '中心の色
        Dim colors As Color() = {Color.Blue}
        pBrush.SurroundColors = colors

        g.FillRectangle(pBrush, 0, 0, rw, rh)
        'g.FillEllipse(pBrush, 0, 0, rw, rh)



        Call Form1.PicBoxAdd("test", bmp)

    End Sub





    Private Sub TrackBarStringBackRoundrect_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarStringBackRoundrect.Scroll
        Dim tb As TrackBar = sender

        Me.LabelStringBackRoundrect背景丸さ.Text = tb.Value
        If CheckBoxLinkedStringRoundRect背景.Checked Then
            TrackBarStringBGRoundRect文字枠の丸さ.Value = tb.Value
            LabelStringBGRoundRect文字枠の丸さ.Text = tb.Value
        End If
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()

    End Sub

    Private Sub RadioButtonStringBackRoundrect_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonStringBackRoundrect.Click
        'Call Form1.TextSample()
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub

    Private Sub ButtonFlameRoundRectAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFrameRoundRectAdd.Click
        Dim name As String = "角丸枠"
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value
        If rw < 2 OrElse rh < 2 Then
            Exit Sub
        End If

        Dim diameter As Single = Me.TrackBarRoundRect.Value
        Dim col As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent.Value, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent2.Value, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent3.Value, Me.ButtonSquareColor3.ForeColor)
        Dim penW As Integer = Me.NumericUpDownPenWidth.Value
        Dim rAngle As Single = -Me.NumericUpDownRectangleAngle.Value
        Dim gradation As Boolean

        If Me.CheckBoxSquareGradation.Checked Then
            gradation = True
        Else
            gradation = False
        End If
        Dim bmp As New Bitmap(Form1.FrameRoundRectAdd(rw, rh, diameter, col, col2, col3, penW, rAngle, gradation))

        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    Private Sub ButtonFrameRoundRectAdd2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFrameRoundRectAdd2.Click
        Dim name As String = "角丸枠2"
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value
        If rw < 2 OrElse rh < 2 Then
            Exit Sub
        End If

        Dim diameter As Single = Me.TrackBarRoundRect.Value
        Dim col As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent.Value, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent2.Value, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent3.Value, Me.ButtonSquareColor3.ForeColor)
        Dim penW As Integer = Me.NumericUpDownPenWidth.Value
        Dim rAngle As Single = -Me.NumericUpDownRectangleAngle.Value
        Dim gradation As Boolean

        If Me.CheckBoxSquareGradation.Checked Then
            gradation = True
        Else
            gradation = False
        End If
        Dim bmp As New Bitmap(Form1.FrameRoundRectAdd2(rw, rh, diameter, col, col2, col3, penW, rAngle, gradation))

        Call Form1.PicBoxAdd(name, bmp)
    End Sub



    Private Sub ButtonFrameEllipseAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFrameEllipseAdd.Click
        Dim name As String = "楕円枠" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ

        If rw < 1 OrElse rh < 1 Then
            Exit Sub
        End If

        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = Me.NumericUpDownSquareTransparent2.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(transparent2, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent3.Value, Me.ButtonSquareColor3.ForeColor)
        Dim penW As Integer = Me.NumericUpDownPenWidth.Value

        Dim bmp As New Bitmap(Form1.EllipseFrameAdd(rw, rh, col, col2, col3, penW))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    Private Sub ButtonFrameEllipseAdd2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFrameEllipseAdd2.Click
        Dim name As String = "楕円枠2" 'ピクチャーボックスの名前
        Dim rw As Integer = Me.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = Me.NumericUpDownRectHeight.Value '枠の高さ

        If rw < 1 OrElse rh < 1 Then
            Exit Sub
        End If

        Dim transparent As Integer = Me.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = Me.NumericUpDownSquareTransparent2.Value
        Dim col As Color = Color.FromArgb(transparent, Me.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(transparent2, Me.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(Me.NumericUpDownSquareTransparent3.Value, Me.ButtonSquareColor3.ForeColor)
        Dim penW As Integer = Me.NumericUpDownPenWidth.Value

        Dim bmp As New Bitmap(Form1.EllipseFrameAdd2(rw, rh, col, col2, col3, penW))
        Call Form1.PicBoxAdd(name, bmp)
    End Sub

    '影をつける
    Private Sub ButtonShadowAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonShadowAdd.Click
        Dim A1 As Integer = Me.TrackBarShadowTransparentAbs.Value
        Dim R1 As Integer = Me.ButtonShadowColor.ForeColor.R
        Dim G1 As Integer = Me.ButtonShadowColor.ForeColor.G
        Dim B1 As Integer = Me.ButtonShadowColor.ForeColor.B
        Dim shade As Single
        Dim sH As Integer = Me.TrackBarShadowH.Value
        Dim sV As Integer = -Me.TrackBarShadowV.Value '感覚的には垂直位置はyの値を逆にした方があっている

        If Me.RadioButtonShadowTransparentRelative.Checked Then '濃淡、相対値
            shade = Me.TrackBarShadowTransparentRelative.Value
            If Me.CheckBoxShadowColorOrigin.Checked Then
                Call Form1.ShadowAdd(sH, sV, 3, shade, A1, R1, G1, B1)
            ElseIf Me.RadioButtonShadowTransparentRelative.Checked Then '濃淡、相対値
                'shade = Me.TrackBarShadowTransparentRelative.Value
                Call Form1.ShadowAdd(sH, sV, 1, shade, A1, R1, G1, B1)
            End If

        Else
            shade = Me.TrackBarShadowTransparentAbs.Value '濃淡、絶対値
            If Me.CheckBoxShadowColorOrigin.Checked Then
                Call Form1.ShadowAdd(sH, sV, 4, shade, A1, R1, G1, B1)
            Else
                'shade = Me.TrackBarShadowTransparentAbs.Value '濃淡、絶対値
                Call Form1.ShadowAdd(sH, sV, 2, shade, A1, R1, G1, B1)
            End If
        End If

        'If Me.CheckBoxShadowColorOrigin.Checked Then
        '    Call Form1.ShadowAdd(sH, sV, 3, shade, A1, R1, G1, B1)

        'ElseIf Me.RadioButtonShadowTransparentRelative.Checked Then '濃淡、相対値
        '    'shade = Me.TrackBarShadowTransparentRelative.Value
        '    Call Form1.ShadowAdd(sH, sV, 1, shade, A1, R1, G1, B1)
        'Else
        '    'shade = Me.TrackBarShadowTransparentAbs.Value '濃淡、絶対値
        '    Call Form1.ShadowAdd(sH, sV, 2, shade, A1, R1, G1, B1)
        'End If

    End Sub
    '影の絶対値表示
    Private Sub TrackBarShadowTransparent_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarShadowTransparentAbs.Scroll
        Me.LabelShadowTransparent.Text = Me.TrackBarShadowTransparentAbs.Value

    End Sub
    '影の色ダイアログ
    Private Sub ButtonShadowColor_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonShadowColor.Click

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.ButtonShadowColor.ForeColor = Me.ColorDialog1.Color
        End If

    End Sub
    '影の相対値表示
    Private Sub TrackBarShadowTransparentRelative_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarShadowTransparentRelative.Scroll
        Me.LabelShadowTransparentRelative.Text = Me.TrackBarShadowTransparentRelative.Value

    End Sub
    '影の縦位置
    Private Sub TrackBarShadowV_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarShadowV.Scroll
        Me.LabelShadowV.Text = Me.TrackBarShadowV.Value
        Me.LabelShadowV.Location = New Point(116, -Me.TrackBarShadowV.Value + 65)
    End Sub
    '影の横位置
    Private Sub TrackBarShadowH_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBarShadowH.Scroll
        Me.LabelShadowH.Text = Me.TrackBarShadowH.Value
        Me.LabelShadowH.Location = New Point(Me.TrackBarShadowH.Value + 44, 121)

    End Sub
    Private Sub Form3_FormClosing(ByVal sender As System.Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles MyBase.FormClosing
        'フォームを閉じるときに範囲選択画像があれば消去する
        With Form1
            Call .SelectRangeDel()
            If .IsGetColor表示画像から色取得中 Then '色の取得中ならキャンセル
                .IsGetColor表示画像から色取得中 = False
                Me.Cursor = Cursors.Default
                .Cursor = Cursors.Default
            End If
        End With

    End Sub

    'Histogramの表示
    Private Sub ButtonHistgram_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHistgram.Click
        ''Form3がすでに開かれていたら何もしない
        'For Each f As Form In Me.OwnedForms
        '    'For Each f As Form In Application.OpenForms
        '    If f.Name = "FormHistogram" Then
        '        Exit Sub
        '    End If
        'Next


        'Me.Cursor = Cursors.WaitCursor

        'myFormHistgram = New FormHistogram 'これを付けないと一度閉じたサブフォームを開こうとするとリソースがないと言われる

        'myFormHistgram.Show(Me)
        'Call SquareSample()
        Call Form1.HistogramShow()




        'omake.Dispose()
        'Me.Cursor = Cursors.Default

    End Sub

    Private Sub ButtonHistgramZ_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHistgramZ.Click
        Call Form1.HistogramSubYZ()

    End Sub

    Private Sub フォームを左下へ移動_Click(sender As System.Object, e As System.EventArgs) Handles ButtonMoveLeftDown.Click
        Me.Left = Me.Owner.Left

        Dim h As Long = SystemInformation.FrameBorderSize.Height 'ウィンドウの枠の高さ
        'Me.Top = h - Me.DisplayRectangle.Height + Me.Owner.Top
        Me.Top = Me.Owner.Height - Me.Height + Me.Owner.Top - Form1.StatusStrip1.Height - h
        'Me.DisplayRectangleだとスクロールバーが表示されているときに元の大きさを取得するから不適切だった

    End Sub

    Private Sub フォームの伸び縮み_Click(sender As System.Object, e As System.EventArgs) Handles ButtonDrawer.Click
        If formSizeMini = False Then
            Dim sukima As Long = Me.ButtonDrawer.Top - Me.TabControl1.Height
            'Dim h As Long = Me.ButtonDrawer.Top - Me.TabControl1.Height
            Dim fbs As Long = SystemInformation.FrameBorderSize.Height * 2
            Dim ch As Long = SystemInformation.CaptionHeight

            Me.Height = Me.TabControl1.Height + Me.ButtonDrawer.Height + fbs + ch + sukima
            Me.ButtonDrawer.Text = "↓"
            formSizeMini = True
        ElseIf formSizeMini = True Then
            Me.Height = formHeight
            Me.ButtonDrawer.Text = "↑"
            formSizeMini = False

        End If

    End Sub



    Private Sub ore編集中の解除(iPic As ExPictureBox)
        'ラベルの全消去
        iPic.Controls.Clear()

    End Sub
    '頂点ドラッグ移動用ここから-------------------------------
    Private Sub oreラベルドラッグ移動開始(sender As Object, e As MouseEventArgs)
        'MouseDown用
        'Focusをクリックした画像に移す
        Dim iLabe As Label
        iLabe = DirectCast(sender, Label)
        iLabe.Parent.Select()
        Form1.ActExPic = iLabe.Parent 'クリックしたラベルがある画像にFocusを移す

        startP = e.Location
        isOreドラッグ中 = True

    End Sub
    Private Sub oreラベルドラッグ移動中(sender As Object, e As MouseEventArgs)
        'MouseMove用
        If e.Button = Windows.Forms.MouseButtons.Left Then
            Dim newP As Point
            Dim iLabel As Control
            iLabel = DirectCast(sender, Label)
            newP = New Point(e.X - startP.X + iLabel.Location.X, e.Y - startP.Y + iLabel.Location.Y)
            iLabel.Location = newP
            Dim i As Long = iLabel.Tag
            Form1.ActExPic.PathPoints.Item(i) = newP
            Dim bmp As New Bitmap(ore図形2描画(Form1.ActExPic))
            Form1.ActExPic.Image = bmp
            'Call Form1.ore加工した画像を各アレイリストに書き込む(bmp) '頂点の移動中も透過になるけど激重でムリ
            'Call Form1.Transparent4’透過処理だけの呼び出しだと頂点の移動中は線の描画が反映されない
            If iLabel.Focused = False Then 'Focusが途中でなくなったらアレイリストに書き込み
                Call Form1.ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)
                isOreドラッグ中 = False
            End If

        End If
    End Sub
    Private Sub oreラベルドラッグ移動終わり(sender As Object, e As MouseEventArgs)
        ''MouseUP用
        'If isOreドラッグ中 And e.Button = Windows.Forms.MouseButtons.Left Then
        '    Dim bmp As New Bitmap(ore図形2描画(Form1.ActExPic))
        '    'Form1.FocusPic.Image = bmp

        '    Call Form1.ore加工した画像を各アレイリストに書き込む(bmp)

        '    isOreドラッグ中 = False

        'End If
    End Sub
    '頂点ドラッグ移動用ここまで-------------------------------

    Private Function ore図形2描画(ByVal iPic As ExPictureBox) As Bitmap
        Dim ps() As PointF = DirectCast(iPic.PathPoints.ToArray, PointF())
        Dim canvas As New Bitmap(iPic.Width, iPic.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim iPen As Pen = iPic.ExPen
        g.DrawLines(iPen, ps)
        Return canvas

        'iPic.Image = canvas
        g.Dispose()

    End Function

    Private Sub Button確認用_Click(sender As System.Object, e As System.EventArgs) Handles Button確認用.Click
        'Dim orepic = EditNowPic


    End Sub

    Private Sub ButtonDraw直線_Click(sender As System.Object, e As System.EventArgs) Handles ButtonDraw直線.Click
        Call Form1.CloseEdit編集終了()
        Call Form1.MouseEndDrawマウスで描画終了処理()
        Me.ComboBoxLineType線の種類.SelectedIndex = ExPictureBox.DrawType.直線
        Me.CheckBoxLineClose線を閉じる.Checked = False
        Me.CheckBoxFill塗りつぶす.Checked = False

        Call Form1.Shape2Add図形2追加その2()

        'Dim PicName As String = Me.ButtonDraw直線.Tag

        'Call Form1.Shape2Add図形2追加(ExPictureBox.DrawType.直線)

    End Sub
    Private Sub ButtonDraw曲線_Click(sender As System.Object, e As System.EventArgs) Handles ButtonDraw曲線.Click
        Call Form1.CloseEdit編集終了()
        Call Form1.MouseEndDrawマウスで描画終了処理()
        Me.ComboBoxLineType線の種類.SelectedIndex = ExPictureBox.DrawType.曲線
        Call Form1.Shape2Add図形2追加その2()

        'Dim cT As Single = Me.NumericUpDown曲線のテンション.Value

        'Call Form1.Shape2Add図形2追加(ExPictureBox.DrawType.曲線, cT)
    End Sub

    Private Sub Button編集開始2_Click(sender As System.Object, e As System.EventArgs) Handles Button編集開始2.Click
        Call StartEdit編集開始()

    End Sub
    Friend Overloads Sub StartEdit編集開始()
        Dim exp As ExPictureBox = Form1.ActExPic
        Call StartEdit編集開始(exp)
    End Sub
    Friend Overloads Sub StartEdit編集開始(exp As ExPictureBox)
        If exp.PathPoints.Count = 1 Then Exit Sub

        Call Form1.頂点の初期化(exp)
        Call Form1.ExPictureをグリッドに移動(exp)
        Call Form1.AllPointsMoveGridすべての頂点をグリッドに移動()
        If Me.ComboBoxLineType線の種類.SelectedIndex <> ExPictureBox.DrawType.ベジェ曲線 Then
            Me.CheckBoxLinkedMove連動移動.Enabled = False
            Me.CheckBoxLinkedMove連動移動.Checked = False
            Me.CheckBoxLinkedControlPoint制御点連動.Enabled = False
            Me.CheckBoxLinkedControlPoint制御点連動.Checked = False
            Me.CheckBoxLinkedAngle角度連動.Enabled = False
            Me.CheckBoxAnchorAngle角度固定.Enabled = False
            'Me.CheckBoxLinkedPictureSize頂点と画像サイズ連動.Enabled = False

        Else
            Me.CheckBoxLinkedMove連動移動.Enabled = True
            Me.CheckBoxLinkedMove連動移動.Checked = True '連動移動するを初期値にしてみた
            Me.CheckBoxLinkedControlPoint制御点連動.Enabled = True
            'Me.CheckBoxLinkedControlPoint制御点連動.Checked = True
            Me.CheckBoxLinkedAngle角度連動.Enabled = True
            Me.CheckBoxAnchorAngle角度固定.Enabled = True
        End If

        If Me.ComboBoxLineType線の種類.SelectedIndex <> ExPictureBox.DrawType.曲線 Then
            Me.ComboBoxPenLineJoin角の形状.Enabled = True
            Me.NumericUpDown曲線のテンション.Enabled = False
        Else
            Me.ComboBoxPenLineJoin角の形状.Enabled = False
            Me.NumericUpDown曲線のテンション.Enabled = True

        End If

    End Sub

    Private Sub Labelアクティブコントロール名前_MouseEnter(sender As System.Object, e As System.EventArgs) Handles Labelアクティブコントロール名前.MouseEnter
        Me.Labelアクティブコントロール名前.Text = Form1.ActiveControl.Name

    End Sub



    Private Sub Button編集終了_Click(sender As System.Object, e As System.EventArgs) Handles Button編集終了.Click
        Call Form1.CloseEdit編集終了()
        'Call Form1.編集終了＿背景付き描画テスト用()

    End Sub

    Private Sub NumericUpDown曲線のテンション_ValueChanged(sender As System.Object, e As System.EventArgs) Handles NumericUpDown曲線のテンション.ValueChanged

        For Each f As Form In My.Application.OpenForms
            If f.Name = "Form1" Then
                If Form1.isDrawEditNow = False Then Exit Sub
                Form1.EditNowPic.CurveTension = Me.NumericUpDown曲線のテンション.Value
                Call Form1.Draw再描画()
                Exit For
            End If
        Next
    End Sub

    Private Sub NumericUpDownPenの太さ_ValueChanged(sender As System.Object, e As System.EventArgs) Handles NumericUpDownPathPenの太さ.ValueChanged
        For Each f As Form In My.Application.OpenForms
            If f.Name = "Form1" Then
                If Form1.isDrawEditNow = False Then Exit Sub
                Form1.EditNowPic.ExPenWidth = Me.NumericUpDownPathPenの太さ.Value

                Call Form1.Draw再描画()
                Exit For
            End If
        Next
    End Sub

    Private Sub Button線の色_Click(sender As System.Object, e As System.EventArgs) Handles ButtonColor線の色.Click

        Me.ColorDialog1.FullOpen = True

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.PictureBoxPenの色.BackColor = Me.ColorDialog1.Color
            If Form1.isDrawEditNow = True Then
                'Dim P1 As New Pen(Me.PictureBoxPenの色.BackColor, Form1.EditNowPic.ExPen.Width)
                'P1.EndCap = Form1.EditNowPic.ExPen.EndCap
                'Form1.EditNowPic.ExPen = P1
                Form1.EditNowPic.ExPenColor = Me.ColorDialog1.Color

                Call Form1.Draw再描画()
            End If
        End If
    End Sub
    Private Sub ButtonColor影の色_Click(sender As Object, e As EventArgs) Handles ButtonColor影の色.Click
        Me.ColorDialog1.FullOpen = True

        If Me.ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.PictureBoxShape2影の色.BackColor = Me.ColorDialog1.Color
            If Form1.isDrawEditNow = True Then
                Form1.EditNowPic.ShadowColor = Me.ColorDialog1.Color
                Call Form1.Draw再描画()
            End If
        End If
    End Sub


    Private Function PenCreateペンの作成() As Pen
        Dim P1 As New Pen(Me.PictureBoxPenの色.BackColor, Me.NumericUpDownPathPenの太さ.Value)
        P1.EndCap = Me.ComboBoxEndCap.SelectedIndex

        Return P1

    End Function

    '先端形状コンボボックス
    Private Sub ComboBoxStartCap_SelectedIndexChanged(sender As ComboBox, e As System.EventArgs) Handles ComboBoxStartCap.SelectedIndexChanged
        For Each f As Form In My.Application.OpenForms
            If f.Name = "Form1" Then
                If Form1.isDrawEditNow = False Then Exit Sub
                Form1.EditNowPic.ExPenStartCap = Me.ComboBoxStartCap.SelectedIndex
                'Form1.EditNowPic.ExPenStartCap = sender.SelectedIndex
                Call Form1.Draw再描画()
                Exit For
            End If
        Next

    End Sub
    '終端形状コンボボックス
    Private Sub ComboBoxEndCap_SelectedIndexChanged(sender As ComboBox, e As System.EventArgs) Handles ComboBoxEndCap.SelectedIndexChanged
        For Each f As Form In My.Application.OpenForms
            If f.Name = "Form1" Then
                If Form1.isDrawEditNow = False Then Exit Sub
                Form1.EditNowPic.ExPenEndCap = Me.ComboBoxEndCap.SelectedIndex
                'Form1.EditNowPic.ExPenStartCap = sender.SelectedIndex
                Call Form1.Draw再描画()
                Exit For
            End If
        Next

    End Sub
    Private Sub ComboBoxFillMode塗りモード_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles ComboBoxFillMode塗りモード.SelectedIndexChanged
        For Each f As Form In My.Application.OpenForms
            If f.Name = "Form1" Then
                If Form1.isDrawEditNow = False Then Exit Sub
                Form1.EditNowPic.ExFillType = Me.ComboBoxFillMode塗りモード.SelectedIndex
                Call Form1.Draw再描画()
                Exit For

            End If
        Next
    End Sub
    Private Sub ComboBoxPenLineJoin角の形状_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles ComboBoxPenLineJoin角の形状.SelectedIndexChanged
        For Each f As Form In My.Application.OpenForms
            If f.Name = "Form1" Then
                If Form1.isDrawEditNow = False Then Exit Sub
                Form1.EditNowPic.ExLineJoin = Me.ComboBoxPenLineJoin角の形状.SelectedIndex
                Call Form1.Draw再描画()
                Exit For

            End If
        Next
    End Sub

    'Private Sub CheckBoxDrawPointGridFit_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBoxDrawPointGridFit.CheckedChanged
    '    If Me.CheckBoxDrawPointGridFit.Checked Then
    '        Me.CheckBoxShapeSizeGridFit.Checked = True

    '        Call Form1.AllPointsMoveGridすべての頂点をグリッドに移動()
    '    Else
    '        Me.CheckBoxShapeSizeGridFit.Checked = False

    '    End If

    'End Sub

    'Private Sub CheckBoxShapeSizeGridFit_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBoxShapeSizeGridFit.CheckedChanged
    '    If Me.CheckBoxShapeSizeGridFit.Checked Then
    '        Call Form1.ExPictureをグリッドに移動()
    '        Me.CheckBoxDrawPointGridFit.Checked = True
    '    Else
    '        Me.CheckBoxDrawPointGridFit.Checked = False
    '    End If
    'End Sub
    Private Sub CheckBoxGridFitShape図形をグリッドに合わせる_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxGridFitShape図形をグリッドに合わせる.CheckedChanged
        If Me.CheckBoxGridFitShape図形をグリッドに合わせる.Checked Then
            Call Form1.AllPointsMoveGridすべての頂点をグリッドに移動()
            Call Form1.ExPictureをグリッドに移動(Form1.ActExPic)

        End If
    End Sub


    Private Sub CheckBoxLineClose線を閉じる_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBoxLineClose線を閉じる.CheckedChanged

        If Form1.isDrawEditNow = False Then Exit Sub
        Form1.EditNowPic.CloseLine = Me.CheckBoxLineClose線を閉じる.Checked
        Call Form1.Bezierベジェ曲線の開閉()

        Call Form1.Draw再描画()

    End Sub

    Private Sub CheckBoxFill塗りつぶす_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBoxFill塗りつぶす.CheckedChanged
        If Form1.isDrawEditNow = False Then Exit Sub
        Form1.EditNowPic.isFill = Me.CheckBoxFill塗りつぶす.Checked
        Call Form1.Draw再描画()

    End Sub
    Private Sub CheckBoxAntiAliasアンチエイリアス_Check(sender As System.Object, e As System.EventArgs) Handles CheckBoxAntiAliasアンチエイリアス.Click
        'クリックチェンジだと起動時にエラー
        If Form1.isDrawEditNow = False Then Exit Sub
        Form1.EditNowPic.isAntiAlias = Me.CheckBoxAntiAliasアンチエイリアス.Checked
        Call Form1.Draw再描画()
    End Sub
    Private Sub CheckBoxShape2Shadow図形2影_Check(sender As Object, e As EventArgs) Handles CheckBoxShape2Shadow図形2影.Click
        'たぶんクリックチェンジイベントだと起動時にエラーになる可能性
        If Form1.isDrawEditNow = False Then Exit Sub
        Form1.EditNowPic.isShadow = Me.CheckBoxShape2Shadow図形2影.Checked
        Call Form1.Draw再描画()
    End Sub

    Private Sub CheckBoxVisiblePointNumber頂点番号表示_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBoxVisiblePointNumber頂点番号表示.CheckedChanged

        Call Form1.VisiblePointState頂点表示用にラベルを渡す()

    End Sub
    Private Sub CheckBoxVisiblePointLocate頂点座標表示_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBoxVisiblePointLocate頂点座標表示.CheckedChanged
        Call Form1.VisiblePointState頂点表示用にラベルを渡す()
    End Sub

    Private Sub 図形追加2_Click_1(sender As System.Object, e As System.EventArgs) Handles ButtonAddShape図形追加2.Click
        Call Form1.Shape2Add図形2追加その2()
        'Call Form1.ExPictureをグリッドに移動()
    End Sub


    Private Sub CheckBoxLinkedAngle角度連動_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBoxLinkedAngle角度連動.CheckedChanged
        If Me.CheckBoxLinkedAngle角度連動.Checked And Me.CheckBoxLinkedControlPoint制御点連動.Checked Then
            Me.CheckBoxLinkedControlPoint制御点連動.Checked = False
        End If
    End Sub

    Private Sub CheckBoxLinkedControlPoint制御点連動_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBoxLinkedControlPoint制御点連動.CheckedChanged
        If Me.CheckBoxLinkedAngle角度連動.Checked And Me.CheckBoxLinkedControlPoint制御点連動.Checked Then
            Me.CheckBoxLinkedAngle角度連動.Checked = False
        End If
    End Sub

    Private Sub ButtonPointChange頂点変更_Click(sender As System.Object, e As System.EventArgs) Handles ButtonPointChange頂点変更.Click
        With Form1
            If .isDrawEditNow = False Then Exit Sub

        End With
        Form1.isLabelSizeSmall = Not Form1.isLabelSizeSmall
        Form1.LabelAdd_頂点ラベルの全部作成(EditNowPic)

    End Sub

    Private Sub ButtonMouseDeDrawLineマウスで描画_Click(sender As Object, e As EventArgs) Handles ButtonMouseDeDrawLineマウスで描画.Click
        Call Form1.MouseDrawマウスで描画()

    End Sub

    Private Sub Button9_Click_1(sender As Object, e As EventArgs) Handles Button9.Click
        Call Form1.MouseEndDrawマウスで描画終了処理()

    End Sub

    Private Sub ButtonAddShapeFitRectangleピッタリ_Click(sender As Object, e As EventArgs) Handles ButtonAddShapeFitRectangleピッタリ.Click
        Call Form1.DrawLineFitSizeピッタリ()

    End Sub




    Private Sub ButtonFontSetTest_Click(sender As Object, e As EventArgs) Handles ButtonFontSetTest.Click
        Dim c1 As Color = Color.Aqua
        Dim ooo = c1.GetBrightness() '0.5
        Dim hue = c1.GetHue() '180
        Dim sa = c1.GetSaturation() '1.0
        Dim c2 As Color = Color.FromArgb(255, 255, 201, 200)
        Dim b2 As Single = c2.GetBrightness()
        Dim s2 As Single = c2.GetSaturation()
        Dim h2 As Single = c2.GetHue
        Dim c3 As Color = Form1.HSLtoRGB(h2, s2, b2)
        Dim h3 As Single = c3.GetHue
        Dim s3 As Single = c3.GetSaturation
        Dim l3 As Single = c3.GetBrightness


        Dim bmp As Bitmap = Form1.ActExPic.Image
        Dim lockW As Integer = bmp.Width
        Dim lockH As Integer = bmp.Height
        Dim lockRect As New Rectangle(0, 0, lockW, lockH)
        Dim bmpdata As BitmapData = bmp.LockBits(lockRect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * lockH - 1
        Dim pixels(data) As Byte
        Dim pos As Integer
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
        Dim x, y, r, g, b As Long
        'Dim a As Long
        Dim h, s, l As Single
        Dim col As Color

        For y = 0 To bmp.Height - 1
            For x = 0 To bmp.Width - 1
                pos = bmpdata.Stride * y + x * 4
                If pixels(pos + 3) <> 0 Then '完全透明以外のピクセルだけ計算
                    'a += pixels(pos + 3)
                    r += pixels(pos + 2)
                    g += pixels(pos + 1)
                    b += pixels(pos)
                    col = Color.FromArgb(pixels(pos + 3), pixels(pos + 2), pixels(pos + 1), pixels(pos))
                    h = (col.GetHue + 60) Mod 360
                    s = col.GetSaturation
                    l = col.GetBrightness
                    col = Form1.HSLtoRGB(h, s, l)

                    pixels(pos + 2) = col.R
                    pixels(pos + 1) = col.G
                    pixels(pos) = col.B

                End If

            Next
        Next

        Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)

        Form1.ActExPic.Image = bmp

    End Sub

    Private Sub ButtonFontSetting詳細設定_Click(sender As Object, e As EventArgs) Handles ButtonFontSetting詳細設定.Click
        'Form6がすでに開かれていたら何もしない
        For Each f As Form In Me.OwnedForms
            'For Each f As Form In Application.OpenForms
            If f.Name = "Form6" Then
                Exit Sub
            End If
        Next


        Me.Cursor = Cursors.WaitCursor

        Dim f6 As New Form6
        'f6 = New Form6
        Dim l As Integer = Me.Left + Me.Width
        With f6
            '.FormBorderStyle = Windows.Forms.FormBorderStyle.FixedToolWindow'ウィンドウのサイズ変更不可

            .StartPosition = FormStartPosition.Manual
            .Location = New Point(l, Me.Top)
            .Show(Me)
            .ShowInTaskbar = False 'タスクバーに表示しない
        End With

        Me.Cursor = Cursors.Default
    End Sub

    '表示画像からスポイトで色を取得
    Private Sub GetColorFromMouseCursor画像から色取得()

        Dim spc As Cursor
        spc = New Cursor("E:\オレ\アイコン\マウスカーソル\スポイト1.cur")
        Me.Cursor = spc
        If True Then

        End If

    End Sub
    Private Sub PictureBoxTextColor1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxTextColor1.MouseDown
        'If Form1.IsGetColor表示画像から色取得中 And Form1.GetColorPic色取得中のPictureBox.Equals(sender) = False Then Exit Sub

        If Form1.IsGetColor表示画像から色取得中 Then ' And sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
            If sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
                Call GetColorEnd表示画像から色取得終了(sender, Me.ButtonFontColor)
            Else
                Exit Sub
            End If
        Else
            Call GetColorStart表示画像から色取得開始(sender)
        End If

    End Sub

    Private Sub PictureBoxTextColor2_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxTextColor2.MouseDown
        'If Form1.IsGetColor表示画像から色取得中 And Form1.GetColorPic色取得中のPictureBox.Equals(sender) = False Then Exit Sub

        'If Form1.IsGetColor表示画像から色取得中 And Form1.GetColorPic色取得中のPictureBox.Equals(sender) Then
        If Form1.IsGetColor表示画像から色取得中 Then ' And sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
            If sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
                Call GetColorEnd表示画像から色取得終了(sender, Me.ButtonFontColor2)
            Else
                Exit Sub
            End If

        Else
            Call GetColorStart表示画像から色取得開始(sender)
        End If
    End Sub
    Private Sub GetColorStart表示画像から色取得開始(PicBox As PictureBox)
        With Form1
            .IsGetColor表示画像から色取得中 = True
            Dim spc As Cursor
            spc = New Cursor("E:\オレ\アイコン\マウスカーソル\スポイト3.cur")
            Me.Cursor = spc
            .Cursor = spc
            .GetColorPic色取得中のPictureBox = PicBox

        End With
    End Sub
    Private Overloads Sub GetColorEnd表示画像から色取得終了(PicBox As PictureBox, Bt As Button) 'こっちは消す予定

        With Form1
            If .GetColorPic色取得中のPictureBox.Equals(PicBox) = False Then Exit Sub

            .Cursor = Cursors.Default
            Me.Cursor = Cursors.Default
            .IsGetColor表示画像から色取得中 = False
            .GetColorPic色取得中のPictureBox = Nothing
            Bt.ForeColor = PicBox.BackColor
            Call Form1.TextSample()

        End With
    End Sub
    Private Overloads Sub GetColorEnd表示画像から色取得終了(PicBox As PictureBox)

        With Form1
            If .GetColorPic色取得中のPictureBox.Equals(PicBox) = False Then Exit Sub

            .Cursor = Cursors.Default
            Me.Cursor = Cursors.Default
            .IsGetColor表示画像から色取得中 = False
            .GetColorPic色取得中のPictureBox = Nothing

            Call Form1.TextSample()

        End With
    End Sub

    Private Sub OpenFrom3の起動完了チェックして文字の見本の再描画()
        If isOpenedForm3 Then
            Call Form1.TextSample()
        End If
        'For Each c As Form In Application.OpenForms
        '    If c.Name = "Form3" Then
        '        Call Form1.TextSample()
        '        Exit For
        '    End If
        'Next
    End Sub
    '文字の背景色
    Private Sub PictureBoxStringBGColor1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxStringBGColor1.MouseDown
        If Form1.IsGetColor表示画像から色取得中 Then ' And sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
            If sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
                Call GetColorEnd表示画像から色取得終了(sender, Me.ButtonTextBackColor1)
            Else
                Exit Sub
            End If
        Else
            Call GetColorStart表示画像から色取得開始(sender)
        End If
    End Sub

    Private Sub PictureBoxStringBGColor2_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxStringBGColor2.MouseDown
        If Form1.IsGetColor表示画像から色取得中 Then ' And sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
            If sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
                Call GetColorEnd表示画像から色取得終了(sender, Me.ButtonTextBackColor2)
            Else
                Exit Sub
            End If
        Else
            Call GetColorStart表示画像から色取得開始(sender)
        End If
    End Sub
    Friend Function CreateFontフォント作成() As Font
        Dim myFont As Font
        Dim fName As String = Me.ComboBoxAllFonts.SelectedItem.ToString()
        Dim fSize As Integer = Me.NumericUpDownFontSize.Value
        Dim fStyle As FontStyle
        If Me.CheckBoxTextItalic.Checked AndAlso Me.CheckBoxTextBold.Checked Then
            fStyle = FontStyle.Bold Or FontStyle.Italic Or FontStyle.Bold
        ElseIf Me.CheckBoxTextItalic.Checked Then
            fStyle = FontStyle.Italic
        ElseIf Me.CheckBoxTextBold.Checked Then
            fStyle = FontStyle.Bold
        End If

        myFont = New Font(fName, fSize, fStyle)

        Return myFont
    End Function

    Friend Function GetBrush文字用グラデーションブラシ作成(sizeF As SizeF) As LinearGradientBrush
        Dim myBrush As LinearGradientBrush
        Dim c1 As Color = Color.FromArgb(Me.NumericUpDownStringColorT1_不透明度.Value, Me.PictureBoxTextColor1.BackColor)
        Dim c2 As Color = Color.FromArgb(Me.NumericUpDownStringColorT2_不透明度.Value, Me.PictureBoxTextColor2.BackColor)
        Dim angle As Integer = Me.NumericUpDownStringGradientAngle.Value
        Dim p As New Point(0, 0)
        Dim rectF As New RectangleF(p, sizeF)
        myBrush = New LinearGradientBrush(rectF, c1, c2, angle)
        If CheckBoxStringGamma.Checked Then
            myBrush.GammaCorrection = True
        End If
        Return myBrush
    End Function
    Friend Function GetBrush文字用単色ブラシ作成() As SolidBrush
        Dim myBrush As SolidBrush
        myBrush = New SolidBrush(Color.FromArgb(NumericUpDownStringColorT1_不透明度.Value, PictureBoxTextColor1.BackColor))
        myBrush = DirectCast(myBrush, Brush)
        Return myBrush
    End Function

    'Friend Function GetBrush背景用グラデーションブラシ作成(sizeF As SizeF) As LinearGradientBrush
    '    Dim myBrush As LinearGradientBrush
    '    Dim c1 As Color = Color.FromArgb(NumericUpDownTextTransparent1.Value, PictureBoxStringBGColor1.BackColor)
    '    Dim c2 As Color = Color.FromArgb(NumericUpDownTextTransparent2.Value, PictureBoxStringBGColor2.BackColor)
    '    Dim angle As Integer = Me.NumericUpDownStringBackGradAngle.Value
    '    Dim p As New Point(0, 0)
    '    Dim rectF As New RectangleF(p, sizeF)

    '    myBrush = New LinearGradientBrush(rectF, c1, c2, angle)
    '    If Me.CheckBoxTextBackC_Gamma.Checked Then
    '        myBrush.GammaCorrection = True
    '    End If
    '    Return myBrush
    'End Function
    'Friend Function GetBrush背景用単色ブラシ作成() As SolidBrush
    '    Dim myBrush As Brush
    '    myBrush = New SolidBrush(Color.FromArgb(NumericUpDownTextTransparent1.Value, PictureBoxStringBGColor1.BackColor))
    '    myBrush = DirectCast(myBrush, Brush)
    '    Return myBrush
    'End Function
    Friend Function DrawSizeLineAjust文字の描画サイズ取得行間調節用(str As String, myFont As Font, sFormat As StringFormat,
                                                     LineCount As Integer, Ajust As Integer) As Size
        Dim bmp As New Bitmap(1, 1)
        Dim g As Graphics = Graphics.FromImage(bmp)

        Dim motoF As SizeF = g.MeasureString(str, myFont, New PointF(0, 0), sFormat)

        Dim w, h, itigyou, myLineSpace As Integer
        Dim itigyouF As SizeF = g.MeasureString("文字jgy", myFont, New Point(0, 0), sFormat)


        If sFormat.FormatFlags = StringFormatFlags.NoClip Then
            '横書きの場合
            w = CInt(Math.Ceiling(motoF.Width))
            itigyou = CInt(itigyouF.Height)
            myLineSpace = myFont.Height + Ajust
            h = itigyou + (Math.Abs(myLineSpace) * (LineCount - 1))
        Else
            '縦書での場合
            h = CInt(Math.Ceiling(motoF.Height))
            itigyou = CInt(itigyouF.Width)
            myLineSpace = myFont.Height + Ajust
            w = itigyou + (Math.Abs(myLineSpace) * (LineCount - 1))

        End If

        Return New Size(w, h)

    End Function
    Friend Function DrawSize文字の描画サイズ取得(str As String, myFont As Font, sFormat As StringFormat) As SizeF
        Dim canvas As New Bitmap(1, 1)
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim drawSizeF As SizeF
        'str = str & vbNewLine & "改行"
        'str = str & myFont.Name
        'drawSizeF = g.MeasureString(str, myFont, 300, sFormat)
        'Call testdraw(drawSizeF, str, myFont, TateYoko)

        drawSizeF = g.MeasureString(str, myFont, New PointF(0, 0), sFormat)

        'Call testdraw(drawSizeF, str, myFont, TateYoko)
        'drawSizeF = g.MeasureString(str, myFont, New SizeF(0, 0), sFormat)
        'Call testdraw(drawSizeF, str, myFont, TateYoko)
        'drawSizeF = g.MeasureString(str, myFont, New SizeF(100, 100), sFormat, str.Length, 1)
        'Call testdraw(drawSizeF, str, myFont, TateYoko)

        'Dim drawSize As New Size(drawSizeF.Width, drawSizeF.Height)
        Dim w As Integer = CInt(Math.Ceiling(drawSizeF.Width))
        Dim h As Integer = CInt(Math.Ceiling(drawSizeF.Height))
        Return drawSizeF

        'Return New Size(w, h)

    End Function

    Private Sub testdraw(bSizeF As SizeF, str As String, myFont As Font, sf As StringFormat, rect As Rectangle)
        'Dim bSize As New Size(CInt(bSizeF.Width), CInt(bSizeF.Height)) '切り上げにしないと表示がおかしくなる
        Dim bSize As New Size(CInt(Math.Ceiling(bSizeF.Width)), CInt(Math.Ceiling(bSizeF.Height))) 'sizeFからSizeへの変換は切り上げ

        'Dim rect As New Rectangle(drawP, bSize)
        Dim canvas As New Bitmap(bSize.Width, bSize.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        'g.DrawString(str, myFont, Brushes.Aqua, 0, 0, sf)
        'canvas = New Bitmap(bSize.Width, bSize.Height)
        g = Graphics.FromImage(canvas)
        g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        g.DrawString(str, myFont, Brushes.Black, rect, sf)

        Dim wakuRect As New Rectangle(New Point(0, 0), bSize)
        wakuRect.Size = New Size(bSize.Width - 1, bSize.Height - 1)
        g.DrawRectangle(Pens.Red, wakuRect)

        'Form1.Panel2.BackgroundImage = canvas


        Dim sgh = myFont.GetHeight
        Dim sh = myFont.Height
        Dim ses = myFont.SizeInPoints 'em size
        Dim su = myFont.Unit
        Dim sghfsr = myFont.GetHeight(FontStyle.Regular)
        Dim fontFamily As New FontFamily(myFont.Name)
        Dim ascent = fontFamily.GetCellAscent(FontStyle.Regular)
        Dim descent = fontFamily.GetCellDescent(FontStyle.Regular)
        Dim he = fontFamily.GetEmHeight(FontStyle.Regular)
        Dim lineSpace = fontFamily.GetLineSpacing(FontStyle.Regular)
        Dim asds = ascent + descent

        Dim pas = myFont.Size * ascent / fontFamily.GetEmHeight(FontStyle.Regular)
        Dim pdes = myFont.Size * descent / fontFamily.GetEmHeight(FontStyle.Regular)
        Dim pls = myFont.Size * lineSpace / fontFamily.GetEmHeight(FontStyle.Regular)
        Dim emsp = myFont.SizeInPoints
        Dim gh = myFont.GetHeight(g)
        Dim gh2 = myFont.GetHeight()
        Dim gh3 = myFont.GetHeight(96)

        g.Dispose()


        Call Form1.PicBoxAdd("test", canvas)
    End Sub
    Private Function GetDrawStringBitmap文字画像作成(str As String, myFont As Font, b As Brush, rect As Rectangle, sf As StringFormat)
        Dim canvas As New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        If CheckBoxStringAntiAlias.Checked Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        End If
        g.DrawString(str, myFont, b, rect, sf)
        g.Dispose()
        Return canvas

    End Function
    Private Function GetDrawStringBitmapPath文字画像作成パスで描く(str As String, myFont As Font, b As Brush, rect As Rectangle, sFormat As StringFormat)
        Dim canvas As New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim gp As New GraphicsPath
        Dim emp As Single = myFont.SizeInPoints * g.DpiX / 72

        If CheckBoxStringAntiAlias.Checked Then
            g.SmoothingMode = SmoothingMode.AntiAlias
        End If

        gp.AddString(str, myFont.FontFamily, myFont.Style, emp, rect, sFormat)
        g.FillPath(b, gp)
        gp.Dispose()
        g.Dispose()
        Return canvas

    End Function
    Private Function GetStringDrawPoint文字の描画起点(myFont As Font) As Point '未使用、GetDrawStringPoint文字の描画起点に変更
        '文字を描画するy座標を求める
        '方法:     フォント メトリックを取得する
        'https://msdn.microsoft.com/ja-jp/library/xwf9s90b(v=vs.110).aspx

        Dim ff As New FontFamily(myFont.Name)
        Dim fSize As Integer = CInt(myFont.Size)
        Dim fStyle As FontStyle = myFont.Style
        Dim pxAscent, pxDescent, pxLineSpace As Single
        Dim emHeight As Integer
        emHeight = ff.GetEmHeight(fStyle)
        pxAscent = ff.GetCellAscent(fStyle) * fSize / emHeight 'フォントサイズを考慮したpixelでのアセント
        pxDescent = ff.GetCellDescent(fStyle) * fSize / emHeight 'ディセント
        pxLineSpace = ff.GetLineSpacing(fStyle) * fSize / emHeight '行間
        Dim y As Integer = CInt((pxLineSpace - (pxAscent + pxDescent)) / 2) 'y座標！
        Dim y2 As Integer = CInt(fSize / 10)
        y += y2
        Return New Point(0, y)
    End Function
    Private Function GetDrawStringLineSpace文字列の行間スペース取得(str As String) As Single
        Dim s As Single
        Return s

    End Function
    Private Function GetDrawString描画位置判定用文字取得(str As String, myFont As Font, sFormat As StringFormat) As String
        Dim motoStr As SizeF = DrawSize文字の描画サイズ取得(Str, myFont, sFormat) '描画文字のサイズ
        'str.Replace(vbNewLine, "")
        'str.Replace(Chr(13), "").Replace(Chr(10), "") '改行を削除しようとしたけどこれではできなかった
        'str.Replace(vbLf, "").Replace(vbCr, "").Replace(vbCrLf, "").Replace(vbNullChar, "") '.Replace(vbNullString, "")
        'Dim newStr As SizeF = DrawSize文字の描画サイズ取得(str, myFont, sFormat)

        Str = "jygfkl" '英数字だけの時のサイズ
        Dim eigoSizeF As SizeF = DrawSize文字の描画サイズ取得(Str, myFont, sFormat)
        'str = "文字"
        'Dim jSizeF As SizeF = DrawSize文字の描画サイズ取得(str, myFont, sFormat)
        Str = "jygfkl文字" '混載の時のサイズ
        Dim ejSizeF As SizeF = DrawSize文字の描画サイズ取得(Str, myFont, sFormat)

        '上限下限の判定に使う文字列の決定
        '元の文字列のサイズが混載の時のサイズ以上なら複数行か混載なのであとは調整の有無
        '元の文字列のサイズが英数字だけの時と同じなら英数字だけ
        If sFormat.FormatFlags = StringFormatFlags.NoClip Then '横書きの場合
            Dim hs As Single = motoStr.Height
            If hs >= ejSizeF.Height Then
                If CheckBoxStringAdjust位置調整.Checked Then
                    Str = "jygfkl文字"
                Else
                    Str = "文字"
                End If

            ElseIf hs = eigoSizeF.Height Then
                Str = "jygfkl"

            End If
        Else '縦書の場合
            Dim ws As Single = motoStr.Width
            If ws >= ejSizeF.Width Then
                If CheckBoxStringAdjust位置調整.Checked Then
                    Str = "jygfkl文字"
                Else
                    Str = "文字"

                End If
            ElseIf ws = eigoSizeF.Width Then
                Str = "jygfkl"
            End If
        End If
        Return str
    End Function
    Private Function GetDrawStringPoint文字の描画起点(str As String, myFont As Font, sFormat As StringFormat) As Point
        ''        Bitmapビジュアライザ、BitmapData、LockBits、配列に入れた時の順番 ( ソフトウェア ) - 午後わてんのブログ - Yahoo!ブログ
        ''http://blogs.yahoo.co.jp/gogowaten/12716715.html
        ''        Pixtack紫陽花2.7.68.153_グラデーションの角度指定変更、文字の描画のズレを力技で修正テスト ( ソフトウェア ) - 午後わてんのブログ - Yahoo!ブログ
        ''http://blogs.yahoo.co.jp/gogowaten/12716986.html

        ''E:\オレ\エクセル\VisualBasicでアプリ作成2.xlsm_lockbit_$B$5

        ''実際に描画して計測
        ''英数字だけの時とそれ以外で描画範囲が変化するフォントが在る、英数字だけの時だけのほうが低くなる
        ''文字列から判定するのは面倒だから、描画しようとしている文字列の描画範囲と英数字だけの描画範囲を比べて違っていたら
        ''英数字だけの文字列を描画しようとしていると判定してjygfklの文字列で描画起点を求める
        ''元の文字列が複数行の場合の描画範囲は単純に行数分増えるわけではなくて、Font.Height分が加算される


        str = GetDrawString描画位置判定用文字取得(str, myFont, sFormat)


        Dim dSizeF As SizeF = DrawSize文字の描画サイズ取得(str, myFont, sFormat)
        Dim dSize As New Size(CInt(Math.Ceiling(dSizeF.Width)), CInt(Math.Ceiling(dSizeF.Height))) 'sizeFからSizeへの変換は切り上げ
        '文字の描画
        Dim w As Integer = dSize.Width
        Dim h As Integer = dSize.Height
        Dim canvas As New Bitmap(w, h)
        Dim g As Graphics = Graphics.FromImage(canvas)
        If CheckBoxStringAntiAlias.Checked Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        End If
        Dim rect As New Rectangle(New Point(0, 0), dSize)
        g.DrawString(str, myFont, Brushes.Red, rect, sFormat) '文字の描画
        'LockBitsしてコピーして探査
        Dim bd As BitmapData = canvas.LockBits(rect, ImageLockMode.ReadOnly, canvas.PixelFormat)
        Dim ptr As IntPtr = bd.Scan0 'Bitmapデータが在るメモリのアドレス？
        Dim data As Integer = bd.Stride * h - 1 '入れ物の大きさ、Strideはbmp.width＊4になる？
        Dim pixels(data) As Byte '入れ物
        Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length) '入れ物にデータが入る、コピー
        Dim pos As Integer = 0 '入れ物のアドレス指定用
        Dim upperY, lowY, leftX, rightX, x, y As Integer '上限、下限,左端、右端
        Dim isFind As Boolean = False '上限、下限見つかったよフラグ
        Dim offP As Point 'オフセットポイント、返す値、答え

        '探査
        If sFormat.FormatFlags = StringFormatFlags.NoClip Then '横書きの場合
            '上の行から順番に色があるピクセルを探す
            For y = 0 To h - 1
                For x = 0 To w - 1
                    pos = y * bd.Stride + x * 4
                    If pixels(pos + 3) <> 0 Then 'アルファの値が0以上なら色があると判定、upperYに今の行位置を入れてループを抜ける
                        upperY = y
                        isFind = True
                        Exit For
                    End If
                Next
                If isFind Then Exit For
            Next
            isFind = False

            '下の行から順番に色があるピクセルを探す
            For y = h - 1 To 0 Step -1
                For x = 0 To w - 1
                    pos = y * bd.Stride + x * 4
                    If pixels(pos + 3) <> 0 Then
                        lowY = y
                        isFind = True
                        Exit For
                    End If
                Next
                If isFind Then Exit For
            Next
            Dim moziH As Integer = lowY - upperY + 1 '文字だけの描画Height、行間スペース無しの高さ、座標は0から始まるので＋１している
            Dim spaceH As Integer = rect.Height - moziH '文字以外の空白Height、文字の上下の空白の高さ
            Dim offY As Integer = CInt((spaceH / 2) - upperY) '空白を半分にして、それから上限を引く、これがオフセット値になる
            offP = New Point(0, offY) '答え、オフセットポイント
        Else '縦書の場合
            '左端
            For x = 0 To w - 1
                For y = 0 To h - 1
                    pos = y * bd.Stride + x * 4
                    If pixels(pos + 3) <> 0 Then
                        leftX = x
                        isFind = True
                        Exit For
                    End If
                Next
                If isFind Then Exit For
            Next
            isFind = False

            '右端探査
            For x = w - 1 To 0 Step -1
                For y = 0 To h - 1
                    pos = y * bd.Stride + x * 4
                    If pixels(pos + 3) <> 0 Then
                        rightX = x
                        isFind = True
                        Exit For
                    End If
                Next
                If isFind Then Exit For
            Next
            Dim moziWid As Integer = rightX - leftX + 1 ' lowY - upperY + 1 '文字だけの描画Height
            Dim spaceW As Integer = rect.Width - moziWid - 1 '文字以外の空白Height
            Dim offX As Integer = CInt((spaceW / 2) - leftX) ' upperY) '空白を半分にして、それから上限を引く、これがオフセット値になる
            offP = New Point(offX, 0) '答え、オフセットポイント

        End If

        canvas.UnlockBits(bd) 'アンロック

        ''デバッグ用、判定に使った文字列と赤枠の画像を追加
        'Dim wakuSize As New Size(rect.Width - 1, rect.Height - 1)
        'Dim wakuRect As New Rectangle(New Point(0, 0), wakuSize)
        'rect = New Rectangle(offP, dSize)
        'canvas = New Bitmap(w, h)
        'g = Graphics.FromImage(canvas)
        'g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        'g.DrawRectangle(Pens.Red, wakuRect)
        'g.DrawString(str, myFont, Brushes.Blue, rect, sFormat)
        'Call Form1.PicBoxAdd("test", canvas)


        Return offP

    End Function
    Private Function GetDrawStringRect文字の描画起点(str As String, myFont As Font, sFormat As StringFormat) As Rectangle
        ''        Bitmapビジュアライザ、BitmapData、LockBits、配列に入れた時の順番 ( ソフトウェア ) - 午後わてんのブログ - Yahoo!ブログ
        ''http://blogs.yahoo.co.jp/gogowaten/12716715.html
        ''        Pixtack紫陽花2.7.68.153_グラデーションの角度指定変更、文字の描画のズレを力技で修正テスト ( ソフトウェア ) - 午後わてんのブログ - Yahoo!ブログ
        ''http://blogs.yahoo.co.jp/gogowaten/12716986.html

        ''E:\オレ\エクセル\VisualBasicでアプリ作成2.xlsm_lockbit_$B$5

        ''実際に描画して計測
        ''英数字だけの時とそれ以外で描画範囲が変化するフォントが在る、英数字だけの時だけのほうが低くなる
        ''文字列から判定するのは面倒だから、描画しようとしている文字列の描画範囲と英数字だけの描画範囲を比べて違っていたら
        ''英数字だけの文字列を描画しようとしていると判定してjygfklの文字列で描画起点を求める
        ''元の文字列が複数行の場合の描画範囲は単純に行数分増えるわけではなくて、Font.Height分が加算される


        str = GetDrawString描画位置判定用文字取得(str, myFont, sFormat)


        Dim dSizeF As SizeF = DrawSize文字の描画サイズ取得(str, myFont, sFormat)
        Dim dSize As New Size(CInt(Math.Ceiling(dSizeF.Width)), CInt(Math.Ceiling(dSizeF.Height))) 'sizeFからSizeへの変換は切り上げ
        '文字の描画
        Dim w As Integer = dSize.Width
        Dim h As Integer = dSize.Height
        Dim canvas As New Bitmap(w, h)
        Dim g As Graphics = Graphics.FromImage(canvas)
        If CheckBoxStringAntiAlias.Checked Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        End If
        Dim rect As New Rectangle(New Point(0, 0), dSize)

        g.DrawString(str, myFont, Brushes.Red, rect, sFormat) '文字の描画
        'LockBitsしてコピーして探査
        Dim bd As BitmapData = canvas.LockBits(rect, ImageLockMode.ReadOnly, canvas.PixelFormat)
        Dim ptr As IntPtr = bd.Scan0 'Bitmapデータが在るメモリのアドレス？
        Dim data As Integer = bd.Stride * h - 1 '入れ物の大きさ、Strideはbmp.width＊4になる？
        Dim pixels(data) As Byte '入れ物
        Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length) '入れ物にデータが入る、コピー
        Dim pos As Integer = 0 '入れ物のアドレス指定用
        Dim upperY, lowY, leftX, rightX, x, y As Integer '上限、下限,左端、右端
        Dim isFind As Boolean = False '上限、下限見つかったよフラグ
        'Dim offP As Point 'オフセットポイント、返す値、答え
        Dim reRect As Rectangle

        '探査
        If sFormat.FormatFlags = StringFormatFlags.NoClip Then '横書きの場合
            '上の行から順番に色があるピクセルを探す
            For y = 0 To h - 1
                For x = 0 To w - 1
                    pos = y * bd.Stride + x * 4
                    If pixels(pos + 3) <> 0 Then 'アルファの値が0以上なら色があると判定、upperYに今の行位置を入れてループを抜ける
                        upperY = y + 1 '例えば2行目で色付きピクセルを見つけても1が記録されるので＋１している
                        isFind = True
                        Exit For
                    End If
                Next
                If isFind Then Exit For
            Next
            isFind = False

            '下の行から順番に色があるピクセルを探す
            For y = h - 1 To 0 Step -1
                For x = 0 To w - 1
                    pos = y * bd.Stride + x * 4
                    If pixels(pos + 3) <> 0 Then
                        lowY = y + 1
                        isFind = True
                        Exit For
                    End If
                Next
                If isFind Then Exit For
            Next
            isFind = False

            Dim moziH As Integer = lowY - upperY + 1 '文字だけの描画Height、行間スペース無しの高さ、座標は0から始まるので＋１している
            Dim spaceH As Integer = rect.Height - moziH '文字以外の空白Height、文字の上下の空白の高さ
            Dim offY As Integer = CInt((spaceH / 2) - upperY) '空白を半分にして、それから上限を引く、これがオフセット値になる
            'offP = New Point(0, offY) '答え、オフセットポイント
            'reRect = New Rectangle(offP, New Size(0, moziH))
            Dim uSpace As Integer = upperY + 1
            Dim dSpace As Integer = dSize.Height - lowY

            '縦書の場合
            '左端
            For x = 0 To w - 1
                For y = 0 To h - 1
                    pos = y * bd.Stride + x * 4
                    If pixels(pos + 3) <> 0 Then
                        leftX = x
                        isFind = True
                        Exit For
                    End If
                Next
                If isFind Then Exit For
            Next
            isFind = False

            '右端探査
            For x = w - 1 To 0 Step -1
                For y = 0 To h - 1
                    pos = y * bd.Stride + x * 4
                    If pixels(pos + 3) <> 0 Then
                        rightX = x
                        isFind = True
                        Exit For
                    End If
                Next
                If isFind Then Exit For
            Next
            Dim moziWid As Integer = rightX - leftX + 1 ' lowY - upperY + 1 '文字だけの描画Height
            Dim spaceW As Integer = rect.Width - moziWid '文字以外の空白Height
            Dim offX As Integer = CInt((spaceW / 2) - leftX) ' upperY) '空白を半分にして、それから上限を引く、これがオフセット値になる
            'offP = New Point(offX, 0) '答え、オフセットポイント
            'reRect = New Rectangle(offP, New Size(moziWid, 0))

            'offP = New Point(offX, offY)
            reRect = New Rectangle(New Point(offX, offY), New Size(moziWid, moziH))

        End If

        canvas.UnlockBits(bd) 'アンロック

        ''デバッグ用、判定に使った文字列と赤枠の画像を追加
        'Dim wakuSize As New Size(rect.Width - 1, rect.Height - 1)
        'Dim wakuRect As New Rectangle(New Point(0, 0), wakuSize)
        'rect = New Rectangle(offP, dSize)
        'canvas = New Bitmap(w, h)
        'g = Graphics.FromImage(canvas)
        'g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        'g.DrawRectangle(Pens.Red, wakuRect)
        'g.DrawString(str, myFont, Brushes.Blue, rect, sFormat)
        'Call Form1.PicBoxAdd("test", canvas)


        Return reRect

    End Function
    Friend Function GetDrawStringWaku文字の枠作成(rect As Rectangle, wakuWidth As Integer) As Bitmap
        Dim c1, c2 As Color
        Dim angle As Integer 'グラデーションの角度
        Dim myBrush As Brush '背景ブラシ

        c1 = Color.FromArgb(Me.NumericUpDownStringBGFrameTransparent1.Value, Me.PictureBoxStringBGFrameColor1.BackColor)
        c2 = Color.FromArgb(Me.NumericUpDownStringBGFrameTransparent2.Value, Me.PictureBoxStringBGFrameColor2.BackColor)
        angle = Me.NumericUpDownStringBGFrameGradationAngle.Value
        If Me.CheckBoxStringBGFrameGradation.Checked Then
            myBrush = New LinearGradientBrush(rect, c1, c2, angle)
            If Me.CheckBoxStringBGFrameColorGamma.Checked Then
                DirectCast(myBrush, LinearGradientBrush).GammaCorrection = True
            End If
        Else
            myBrush = New SolidBrush(c1)
        End If

        Dim maru As Integer = CInt(Me.LabelStringBGRoundRect文字枠の丸さ.Text)
        Dim bgFrame As Bitmap = GetDrawFrame枠画像(rect, myBrush, wakuWidth, maru) '枠画像取得
        'Dim g As Graphics = Graphics.FromImage(bgFrame) 'これが良くなかった、正解は↓
        Dim g As Graphics = Graphics.FromImage(New Bitmap(bgFrame.Width, bgFrame.Height))
        g.DrawImage(bgFrame, 0, 0)

        Return bgFrame

    End Function
    Friend Function GetDrawStringBG文字の背景作成(rect As Rectangle, wakuWidth As Integer) As Bitmap
        Dim c1, c2 As Color
        Dim angle As Integer 'グラデーションの角度
        Dim myBrush As Brush '背景ブラシ
        Dim bmp As New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(bmp)

        '背景の描画
        c1 = Color.FromArgb(NumericUpDownTextTransparent1.Value, PictureBoxStringBGColor1.BackColor)
        c2 = Color.FromArgb(NumericUpDownTextTransparent2.Value, PictureBoxStringBGColor2.BackColor)
        angle = Me.NumericUpDownStringBackGradAngle文字背景グラデ角度.Value

        If CheckBoxTextBackC_Gradation.Checked Then
            myBrush = New LinearGradientBrush(rect, c1, c2, angle)
            If CheckBoxTextBackC_Gamma.Checked Then
                DirectCast(myBrush, LinearGradientBrush).GammaCorrection = True
            End If
        Else
            myBrush = New SolidBrush(c1)
        End If
        Dim maru As Integer = Me.TrackBarStringBackRoundrect.Value
        '枠を付ける時は枠の内側だけ塗る、そのために枠の太さが必要wakuWidth
        Dim bg As Bitmap = GetDrawRect領域塗り(rect, myBrush, maru, wakuWidth)

        g.DrawImage(bg, 0, 0)
        g.Dispose()

        Return bmp

    End Function

    Friend Function GetDrawStringAll文字と背景画像作成(str As String, myFont As Font) As Bitmap
        Dim bmp As Bitmap

        '縦書、横書きチェック
        Dim sFormat As New StringFormat
        If Me.RadioButtonStringV.Checked Then
            sFormat.FormatFlags = StringFormatFlags.DirectionVertical Or StringFormatFlags.DirectionRightToLeft
        ElseIf Me.RadioButtonStringH.Checked Then
            sFormat.FormatFlags = StringFormatFlags.NoClip
        End If
        bmp = GetDrawBGOnly背景だけの描画(str, myFont, sFormat)
        Dim g As Graphics = Graphics.FromImage(bmp)
        'Dim g As Graphics = Graphics.FromImage(New Bitmap(bmp.Width, bmp.Height))
        ''行数チェック
        Dim ss() As String = str.Split(Chr(10)) '複数行文字列を行ごとに分割して配列へ
        Dim ssl As New List(Of String) '行ごとの文字列リスト
        '空白行を取り除いたリスト作成
        For i = 0 To UBound(ss)
            If ss(i) <> "" Then
                ssl.Add(ss(i))
            End If
        Next
        Dim lineSpace As Integer = NumericUpDownLineSpace行間.Value '行間調整値
        Dim lineCount As Integer = ssl.Count '行数
        Dim wordSpace As Integer = NumericUpDownWordSpace文字間.Value '文字間調整値


        '文字の描画ここから
        Dim myLineSpace As Integer = lineSpace + myFont.Height
        Dim tempBmp As Bitmap
        Dim gBrushRect As New Rectangle(0, 0, bmp.Width, bmp.Height) 'グラデーションブラシ用Rectangle
        Dim moziSizeI As Size 'グラデーションブラシ用Size
        Dim lineSize As SizeF 'その行の描画サイズ
        Dim sslCount As Integer = lineCount - 1
        Select Case True

            Case wordSpace <> 0 '文字間指定がある場合、行間指定の有無は関係ない
                'E:\オレ\エクセル\VisualBasicでアプリ作成2.xlsm_文字_$A$243
                Dim dP As Single = GetWordDrawPointDiff(myFont, sFormat)
                Dim allWidth As Single = 0
                Dim tateCount As Integer = sslCount
                Dim mozi As String
                Dim moziSize As SizeF
                Dim moziBmp As Bitmap
                Dim lineAdjust As Integer = 0 '半角英字の高さが小さくなるフォント用
                For i = 0 To sslCount
                    allWidth = 0
                    mozi = ssl(i)
                    lineSize = DrawSize文字の描画サイズ取得(ssl(i), myFont, sFormat) 'その行の描画サイズ
                    If wordSpace <> 0 AndAlso mozi.Length > 0 Then '文字間指定有りの時
                        For j As Integer = 0 To mozi.Length - 1
                            moziSize = DrawSize文字の描画サイズ取得(mozi.Chars(j), myFont, sFormat)
                            moziSizeI = New Size(CInt(Math.Ceiling(moziSize.Width)), CInt(Math.Ceiling(moziSize.Height)))
                            If sFormat.FormatFlags = StringFormatFlags.NoClip Then
                                '横書き
                                If Me.CheckBoxGradationParts.Checked Then
                                    gBrushRect.Size = moziSizeI 'グラデーションブラシ用Size
                                Else
                                    gBrushRect.Location = New Point(-allWidth, -((myLineSpace * i) + lineAdjust)) 'グラデーションブラシ用Rectangle
                                End If

                                moziBmp = GetDrawStringOnly文字だけの描画(mozi.Chars(j), myFont, sFormat, gBrushRect)
                                lineAdjust = CInt((lineSize.Height - moziSize.Height) / 2) '半角英字の高さが小さくなるフォント用
                                g.DrawImage(moziBmp, CInt(allWidth), (myLineSpace * i) + lineAdjust)
                                allWidth = allWidth + moziSize.Width - dP + wordSpace
                            Else
                                '縦書
                                If Me.CheckBoxGradationParts.Checked Then
                                    gBrushRect.Size = moziSizeI 'グラデーションブラシ用Size
                                Else
                                    gBrushRect.Location = New Point(-(myLineSpace * tateCount), -allWidth) 'グラデーションブラシ用Rectangle
                                End If

                                moziBmp = GetDrawStringOnly文字だけの描画(mozi.Chars(j), myFont, sFormat, gBrushRect)
                                g.DrawImage(moziBmp, myLineSpace * tateCount, CInt(allWidth))
                                allWidth = allWidth + moziSize.Height - dP + wordSpace
                            End If
                        Next
                    End If
                    tateCount -= 1
                Next

            Case lineSpace <> 0 ', lineCount > 1 '行間指定ありで複数行の場合
                For i As Integer = 0 To sslCount

                    '描画座標
                    Dim drawPoint As Point
                    If myLineSpace >= 0 Then '行間指定ありで複数行の場合
                        If sFormat.FormatFlags = StringFormatFlags.NoClip Then
                            drawPoint = New Point(0, myLineSpace * i)
                        Else
                            drawPoint = New Point(myLineSpace * sslCount, 0)
                            sslCount -= 1
                        End If
                    Else '行間がマイナスなら下から上へ描画
                        If sFormat.FormatFlags = StringFormatFlags.NoClip Then
                            drawPoint = New Point(0, Math.Abs(myLineSpace) * sslCount)
                            sslCount -= 1
                        Else
                            drawPoint = New Point(Math.Abs(myLineSpace) * i, 0)
                        End If
                    End If

                    If Me.CheckBoxGradationParts.Checked Then
                        lineSize = DrawSize文字の描画サイズ取得(ssl(i), myFont, sFormat)
                        gBrushRect.Size = New Size(CInt(Math.Ceiling(lineSize.Width)), CInt(Math.Ceiling(lineSize.Height))) 'グラデーションブラシ用Rectangle
                    Else
                        gBrushRect.Location = New Point(-(drawPoint.X), -(drawPoint.Y)) 'グラデーションブラシ用Rectangle
                    End If

                    tempBmp = GetDrawStringOnly文字だけの描画(ssl(i), myFont, sFormat, gBrushRect)
                    g.DrawImage(tempBmp, drawPoint)


                Next

            Case Else 'それ以外の場合(通常)
                g.DrawImage(GetDrawStringOnly文字だけの描画(str, myFont, sFormat, gBrushRect), 0, 0)

        End Select

        '回転
        If Me.CheckBoxStringAngle.Checked Then
            Dim bmpAngle As Integer = Me.NumericUpDownStringAngle.Value
            bmp = Form1.PicAngle(bmp, -bmpAngle)
        End If


        Return bmp

    End Function
    Private Function GetWordDrawPointDiff(myFont As Font, sFormat As StringFormat) As Single
        Dim diff As Single
        Dim w1, w1to2 As SizeF
        Dim mozi As String = "a"
        w1 = DrawSize文字の描画サイズ取得(mozi, myFont, sFormat)
        w1to2 = DrawSize文字の描画サイズ取得(mozi & mozi, myFont, sFormat)
        If sFormat.FormatFlags = StringFormatFlags.NoClip Then
            diff = (w1.Width * 2) - w1to2.Width
        Else
            diff = (w1.Height * 2) - w1to2.Height
        End If

        Return diff

    End Function
    Private Function GetDrawBGOnly背景だけの描画(str As String, myFont As Font, sFormat As StringFormat) As Bitmap
        'E:\オレ\エクセル\VisualBasicでアプリ作成2.xlsm_文字_$A$146

        Dim bmp As Bitmap

        '行数チェック
        Dim ss() As String = str.Split(Chr(10)) '複数行文字列を行ごとに分割して配列へ
        Dim ssl As New List(Of String) '行ごとの文字列リスト
        '空白行を取り除いたリスト作成
        For i = 0 To UBound(ss)
            If ss(i) <> "" Then
                ssl.Add(ss(i))
            End If
        Next
        Dim lineSpace As Integer = NumericUpDownLineSpace行間.Value '行間調整値
        Dim lineCount As Integer = ssl.Count '行数
        Dim wordSpace As Integer = NumericUpDownWordSpace文字間.Value '文字間調整値

        '画像の大きさ取得してBitmap作成
        Dim dSizeF As SizeF
        Dim dSize As Size
        If lineSpace = 0 OrElse lineCount = 1 Then
            '行間調整なしの場合
            dSizeF = DrawSize文字の描画サイズ取得(str, myFont, sFormat)
            dSize = New Size(CInt(Math.Ceiling(dSizeF.Width)), CInt(Math.Ceiling(dSizeF.Height))) 'sizeFからSizeへの変換は切り上げ
            'dSize = New Size(CInt(dSizeF.Width), CInt(dSizeF.Height))
        Else
            '行間調整ありの場合
            dSize = DrawSizeLineAjust文字の描画サイズ取得行間調節用(str, myFont, sFormat, lineCount, lineSpace)
        End If
        bmp = New Bitmap(dSize.Width, dSize.Height)

        If wordSpace <> 0 Then
            '文字間調整ありの場合
            '複数行の場合は一番長いもの（大きい）を探す
            Dim lines As New List(Of Single)
            Dim moziCount As Integer

            For Each stl As String In ssl
                Dim tempSize As SizeF = DrawSize文字の描画サイズ取得(stl, myFont, sFormat)
                '今の行の文字列カウント、改行(vbCr)は除く
                If stl.Contains(vbCr) Then
                    moziCount = stl.Length - 2
                Else
                    moziCount = stl.Length - 1
                End If
                '今の行の幅(高さ)
                If sFormat.FormatFlags = StringFormatFlags.NoClip Then
                    lines.Add(Math.Abs(tempSize.Width + ((moziCount) * wordSpace))) '横書き
                Else
                    lines.Add(Math.Abs(tempSize.Height + ((moziCount) * wordSpace))) '縦書
                End If
            Next
            lines.Sort() '小さい順に並べ替え

            '一番長いものを画像の幅(高さ)にする
            If sFormat.FormatFlags = StringFormatFlags.NoClip Then
                bmp = New Bitmap(CInt(Math.Ceiling(lines.Last)), bmp.Height) '横書き
            Else
                bmp = New Bitmap(bmp.Width, CInt(Math.Ceiling(lines.Last))) '縦書
            End If
        End If

        Dim g As Graphics = Graphics.FromImage(bmp)


        'Dim dPoint As New Point(GetDrawStringPoint文字の描画起点(str, myFont, sFormat)) '描画のオフセットポイント取得
        'Dim r As Rectangle = GetDrawStringRect文字の描画起点(str, myFont, sFormat)

        Dim rect As New Rectangle(New Point(0, 0), New Size(bmp.Width, bmp.Height))

        Dim wakuWidth As Integer = 0
        If Me.CheckBoxStringBGFrame.Checked Then '枠ありなら枠幅を入れる
            wakuWidth = Me.NumericUpDownStringBGFrameWidth枠幅.Value
        End If

        ''背景の描画
        If CheckBoxTextBackColor背景色の有無.Checked Then
            Dim bmpBG As Bitmap = GetDrawStringBG文字の背景作成(rect, wakuWidth)
            g.DrawImage(bmpBG, 0, 0)
        End If

        ''枠の描画
        If CheckBoxStringBGFrame.Checked AndAlso wakuWidth > 0 Then
            Dim bmpWaku As Bitmap = GetDrawStringWaku文字の枠作成(rect, wakuWidth)
            g.DrawImage(bmpWaku, 0, 0)
        End If

        Return bmp

    End Function
    Private Function GetDrawStringOnly文字だけの描画(str As String, myFont As Font, sFormat As StringFormat, gBrushRect As Rectangle) As Bitmap
        Dim c1, c2 As Color
        Dim angle As Integer
        Dim myBrush As Brush
        Dim gradationParts As Boolean = Me.CheckBoxGradationParts.Checked '個別グラデーション
        Dim dSizeF As SizeF = DrawSize文字の描画サイズ取得(str, myFont, sFormat)
        Dim dSize As New Size(CInt(Math.Ceiling(dSizeF.Width)), CInt(Math.Ceiling(dSizeF.Height))) 'sizeFからSizeへの変換は切り上げ
        Dim dPoint As New Point(GetDrawStringPoint文字の描画起点(str, myFont, sFormat)) '描画のオフセットポイント取得
        'Dim rect As New Rectangle(New Point(0, -500), New Size(1000, 1000))
        Dim rect As New Rectangle(New Point(0, 0), dSize)
        Dim bmp As New Bitmap(dSize.Width, dSize.Height)
        Dim g As Graphics = Graphics.FromImage(bmp)

        Dim AA As Boolean = Me.CheckBoxStringAntiAlias.Checked
        '縁取り描画
        Dim fWidth As Integer = Me.NumericUpDownStringFringeWidth.Value  '縁取り幅
        If Me.CheckBoxStringFringe.Checked AndAlso fWidth > 0 Then
            c1 = Color.FromArgb(Me.NumericUpDownStringFringeTransparent1.Value, Me.PictureBoxStringFringeColor1.BackColor)
            c2 = Color.FromArgb(Me.NumericUpDownStringFringeTransparent2.Value, Me.PictureBoxStringFringeColor2.BackColor)
            angle = Me.NumericUpDownStringFringeGradationAngleグラデ角度.Value
            If Me.CheckBoxStringFringeGradation.Checked Then 'グラデーションブラシ作成
                If gradationParts Then
                    myBrush = New LinearGradientBrush(rect, c1, c2, angle)

                Else
                    myBrush = New LinearGradientBrush(gBrushRect, c1, c2, angle)

                End If
                If Me.CheckBoxStringFringeGamma.Checked Then
                    DirectCast(myBrush, LinearGradientBrush).GammaCorrection = True
                End If
            Else
                myBrush = New SolidBrush(c1)
            End If
            Dim fRect As New Rectangle(dPoint, rect.Size)
            Dim fringeBmp As Bitmap
            'fringeBmp = GetDrawFringe文字の縁取り(str, myFont, sFormat, fRect, myBrush, fWidth, AA)
            'g.DrawImage(fringeBmp, 0, 0)

            'パスで描く
            If Me.CheckBoxDrawPathパスで描く.Checked Then
                Dim myPen As New Pen(myBrush, fWidth)
                myPen.LineJoin = LineJoin.Round
                'myPen.Alignment = PenAlignment.Outset 'Inset以外は同じ
                fringeBmp = GetDrawFringePath文字の縁取りパスで描画(str, myFont, sFormat, fRect, myPen, AA)
            Else
                fringeBmp = GetDrawFringe文字の縁取り(str, myFont, sFormat, fRect, myBrush, fWidth, AA)
            End If
            g.DrawImage(fringeBmp, 0, 0)
        End If


        '影の描画
        If Me.CheckBoxStringShadow.Checked Then

            c1 = Color.FromArgb(Me.NumericUpDownStringShadowColorTransparent1.Value, Me.PictureBoxStringShadowColor1.BackColor)
            c2 = Color.FromArgb(Me.NumericUpDownStringShadowColorTransparent2.Value, Me.PictureBoxStringShadowColor2.BackColor)
            angle = Me.NumericUpDownStringShadowAngle.Value
            If Me.CheckBoxStringShadowGradation.Checked Then
                If gradationParts Then
                    myBrush = New LinearGradientBrush(rect, c1, c2, angle)
                Else
                    myBrush = New LinearGradientBrush(gBrushRect, c1, c2, angle)
                End If


                If Me.CheckBoxStringShadowGamma.Checked Then
                    DirectCast(myBrush, LinearGradientBrush).GammaCorrection = True
                End If
            Else
                myBrush = New SolidBrush(c1)
            End If
            Dim offx As Single = Me.NumericUpDownStringShadowH横.Value / 10
            Dim offy As Single = Me.NumericUpDownStringShadowV縦.Value / 10
            'Dim offP As New Point(dPoint.X + 1, dPoint.Y + 1)
            'Dim kageRect As New Rectangle(offP, rect.Size)
            'Dim kageBmp As Bitmap = GetDrawShadow文字の影(str, myFont, sFormat, kageRect, myBrush, AA)
            'g.DrawImage(kageBmp, 0, 0)
            Dim offPF As New PointF(dPoint.X + offx, dPoint.Y + offy)
            Dim kageRectF As New RectangleF(offPF, rect.Size)
            Dim kageFBmp As Bitmap = GetDrawShadowF文字の影(str, myFont, sFormat, kageRectF, myBrush, AA)
            g.DrawImage(kageFBmp, 0, 0)

            'offPF = New PointF(dPoint.X - offx, dPoint.Y - offy)
            'kageRectF = New RectangleF(offPF, rect.Size)
            'kageFBmp = GetDrawShadowF文字の影(str, myFont, sFormat, kageRectF, myBrush, AA)
            'g.DrawImage(kageFBmp, 0, 0)

        End If

        '文字の描画
        Dim moziRect As New Rectangle(dPoint, rect.Size)

        c1 = Color.FromArgb(NumericUpDownStringColorT1_不透明度.Value, PictureBoxTextColor1.BackColor)
        c2 = Color.FromArgb(NumericUpDownStringColorT2_不透明度.Value, PictureBoxTextColor2.BackColor)
        angle = NumericUpDownStringGradientAngle.Value
        If CheckBoxTextGradation.Checked Then
            myBrush = New LinearGradientBrush(gBrushRect, c1, c2, angle)

            If CheckBoxStringGamma.Checked Then
                DirectCast(myBrush, LinearGradientBrush).GammaCorrection = True
            End If
        Else
            myBrush = New SolidBrush(c1)
        End If
        Dim strBmp As Bitmap


        'strBmp = GetDrawStringBitmap文字画像作成(str, myFont, myBrush, moziRect, sFormat)
        strBmp = GetDrawStringBitmapPath文字画像作成パスで描く(str, myFont, myBrush, moziRect, sFormat)
        g.DrawImage(strBmp, 0, 0)

        myBrush.Dispose()
        g.Dispose()


        Return bmp

    End Function
    Private Function GetDrawFringe文字の縁取り(str As String, myFont As Font, sFormat As StringFormat, rect As Rectangle,
                                         b As Brush, fWidth As Integer, AA As Boolean) As Bitmap
        Dim canvas As New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim y, x As Integer

        If AA Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        End If
        '複数行の縦書の場合におかしくなるので描画にはRectangleが必要
        Dim nRect As New Rectangle(rect.Location, rect.Size)

        For y = -fWidth To fWidth
            For x = -fWidth To fWidth
                nRect.Location = New Point(x + rect.X, y + rect.Y)
                g.DrawString(str, myFont, b, nRect, sFormat)
                'g.DrawString(str, myFont, b, x + rect.X, y + rect.Y, sFormat) 'これだと縦書の時に範囲外に描画されてしまう
            Next
        Next
        g.Dispose()

        Return canvas

    End Function
    Private Function GetDrawFringePath文字の縁取りパスで描画(str As String, myFont As Font, sFormat As StringFormat, rect As Rectangle,
                                                  myPen As Pen, AA As Boolean) As Bitmap
        Dim canvas As New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim gp As New GraphicsPath
        'Dim y, x As Integer

        If AA Then
            'g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
            g.SmoothingMode = SmoothingMode.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        End If

        'Dim fontHeight As Single = myFont.GetHeight
        'Dim fontEmHeight As Integer = myFont.FontFamily.GetEmHeight(myFont.Style)
        Dim fh As Single = myFont.SizeInPoints * g.DpiX / 72
        gp.AddString(str, myFont.FontFamily, myFont.Style, fh, rect, sFormat)
        g.DrawPath(myPen, gp)

        g.Dispose()
        gp.Dispose()

        Return canvas


    End Function
    Private Function GetDrawShadow文字の影(str As String, myFont As Font, sFormat As StringFormat, rect As Rectangle, b As Brush, AA As Boolean) As Bitmap
        'こっちは未使用、整数だけの座標指定版
        Dim canvas As New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        If AA Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        End If
        g.DrawString(str, myFont, b, rect, sFormat)
        g.Dispose()

        Return canvas
    End Function
    Private Function GetDrawShadowF文字の影(str As String, myFont As Font, sFormat As StringFormat, rectF As RectangleF, b As Brush, AA As Boolean) As Bitmap
        '小数点ありの座標指定版

        Dim canvas As New Bitmap(CInt(rectF.Width), CInt(rectF.Height))
        Dim g As Graphics = Graphics.FromImage(canvas)
        If AA Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        End If
        g.DrawString(str, myFont, b, rectF, sFormat)
        g.Dispose()

        Return canvas
    End Function

    Private Function GetDrawRect領域塗り(rect As Rectangle, b As Brush, maru As Integer, wakuWidth As Integer) As Bitmap

        Dim canvas As New Bitmap(rect.Width, rect.Height) '(CInt(Math.Ceiling(rectF.Width)), CInt(Math.Ceiling(rectF.Height)))
        Dim g As Graphics = Graphics.FromImage(canvas)
        If maru = 0 Then '普通の四角形の場合
            g.FillRectangle(b, rect)
            g.Dispose()
            Return canvas
        End If

        '角丸四角の場合
        Dim gp As New GraphicsPath
        gp = Form1.RoundRectInSideGPath(gp, rect, maru, wakuWidth) 'パスの取得
        g.SmoothingMode = SmoothingMode.AntiAlias
        g.PixelOffsetMode = PixelOffsetMode.Half

        g.FillPath(b, gp)
        g.Dispose()
        gp.Dispose()


        Return canvas
    End Function
    Private Function GetDrawFrame枠画像(rect As Rectangle, wakuBrush As Brush, wakuWidth As Integer, maru As Integer) As Bitmap

        Dim canvas As New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim p As New Pen(wakuBrush, wakuWidth)
        If maru = 0 Then '普通の四角形の場合
            rect.Height -= 1
            rect.Width -= 1
            p.Alignment = PenAlignment.Inset
            g.DrawRectangle(p, rect)
            g.Dispose()
            p.Dispose()
            Return canvas
        End If

        '角丸四角の場合GraphicsPathを使って描画
        g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス
        g.PixelOffsetMode = PixelOffsetMode.Half 'pixelオフセット

        Dim gp As New GraphicsPath
        'rect.Width = 1025

        gp = Form1.FrameRoundRectGPath(gp, rect, wakuWidth, wakuBrush, maru) 'パスの取得
        'Dim mypen As New Pen(Brushes.Red, 10)
        'mypen.Alignment = PenAlignment.Inset

        'g.DrawPath(mypen, gp)
        g.FillPath(wakuBrush, gp)



        'Dim diameter As Single = maru
        'diameter = (Math.Min(rect.Width, rect.Height)) * (diameter / 100) '角丸の直径、最低値は2、最高値は縦横の小さい方の値
        'Dim xOff As Single = rect.Width - diameter - wakuWidth
        'Dim yOff As Single = rect.Height - diameter - wakuWidth

        ''外側
        'gp.AddArc(wakuWidth, wakuWidth, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        'gp.AddArc(xOff, 0 + wakuWidth, diameter, diameter, 270, 90)
        'gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
        'gp.AddArc(0 + wakuWidth, yOff, diameter, diameter, 90, 90)
        'gp.CloseFigure()
        'Dim myPen As New Pen(Brushes.Red, wakuWidth)
        'g.DrawPath(myPen, gp)


        g.Dispose()
        gp.Dispose()
        p.Dispose()

        Return canvas
    End Function

    Private Sub NumericUpDown背景色透明度_ValueChange(sender As Object, e As EventArgs) Handles NumericUpDownTextTransparent2.ValueChanged

    End Sub




    Private Sub ButtonTextAddボタンの表示数値をヌメリックに加算(btn As Button, num As NumericUpDown)
        Dim btnValue As Integer = CInt(btn.Text)
        Dim numValue As Integer = num.Value
        Dim nMax As Integer = num.Maximum
        Dim nMin As Integer = num.Minimum
        Dim newValue As Integer = numValue + btnValue
        If newValue < nMin Then
            newValue = nMin
        ElseIf newValue > nMax Then
            newValue = nMax
        End If
        num.Value = newValue

    End Sub
    Private Sub ButtonStringGradientAngleMin45文字グラデ角度45_Click(sender As Object, e As EventArgs) Handles ButtonStringGradientAngleMin45文字グラデ角度45.Click

        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, Me.NumericUpDownStringGradientAngle)

    End Sub

    Private Sub ButtonStringGradientAngleAdd45文字グラデ角度45_Click(sender As Object, e As EventArgs) Handles ButtonStringGradientAngleAdd45文字グラデ角度45.Click

        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, Me.NumericUpDownStringGradientAngle)

    End Sub

    Private Sub NumericUpDownStringGradientAngle_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringGradientAngle.ValueChanged

        Call OpenFrom3の起動完了チェックして文字の見本の再描画()

    End Sub


    Private Sub ButtonStringGradientAngleMin45文字背景グラデ角度45_Click(sender As Object, e As EventArgs) Handles ButtonStringGradientAngleMin45文字背景グラデ角度45.Click
        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, Me.NumericUpDownStringBackGradAngle文字背景グラデ角度)
    End Sub

    Private Sub ButtonStringGradientAngleAdd45文字背景グラデ角度45_Click(sender As Object, e As EventArgs) Handles ButtonStringGradientAngleAdd45文字背景グラデ角度45.Click
        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, Me.NumericUpDownStringBackGradAngle文字背景グラデ角度)
    End Sub

    Private Sub NumericUpDownStringBackGradAngle文字背景グラデ角度_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringBackGradAngle文字背景グラデ角度.ValueChanged
        If CheckBoxTextBackColor背景色の有無.Checked AndAlso CheckBoxTextBackC_Gradation.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If

    End Sub


    Private Sub ButtonStringBGFrameGradationAngleMin45枠_Click(sender As Object, e As EventArgs) Handles ButtonStringBGFrameGradationAngleMin45枠.Click

        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, Me.NumericUpDownStringBGFrameGradationAngle)


    End Sub

    Private Sub ButtonStringBGFrameGradationAngleAdd45枠_Click(sender As Object, e As EventArgs) Handles ButtonStringBGFrameGradationAngleAdd45枠.Click

        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, Me.NumericUpDownStringBGFrameGradationAngle)


    End Sub

    Private Sub NumericUpDownStringBGFrameGradationAngle_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringBGFrameGradationAngle.ValueChanged
        If CheckBoxStringBGFrame.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If
    End Sub

    Private Sub ColorDialogカラーダイアログで色取得して見本更新(picBox As PictureBox, kousin As Boolean)
        Dim cd1 As ColorDialog = Me.ColorDialog1
        cd1.Color = picBox.BackColor
        If cd1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            picBox.BackColor = cd1.Color
            If kousin Then
                Form1.TextSample()
            End If

        End If

    End Sub
    Private Sub ButtonStringBGFrameColor1_Click(sender As Object, e As EventArgs) Handles ButtonStringBGFrameColor1.Click
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxStringBGFrameColor1, CheckBoxStringBGFrame.Checked)
    End Sub

    Private Sub ButtonStringBGFrameColor2_Click(sender As Object, e As EventArgs) Handles ButtonStringBGFrameColor2.Click
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxStringBGFrameColor2, CheckBoxStringBGFrame.Checked)
    End Sub

    Private Sub PictureBoxStringBGFrameColor1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxStringBGFrameColor1.MouseDown
        If Form1.IsGetColor表示画像から色取得中 Then
            If sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
                Call GetColorEnd表示画像から色取得終了(sender)
            Else
                Exit Sub
            End If
        Else
            Call GetColorStart表示画像から色取得開始(sender)
        End If
    End Sub

    Private Sub PictureBoxStringBGFrameColor2_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxStringBGFrameColor2.MouseDown
        If Form1.IsGetColor表示画像から色取得中 Then
            If sender.Equals(Form1.GetColorPic色取得中のPictureBox) Then
                Call GetColorEnd表示画像から色取得終了(sender)
            Else
                Exit Sub
            End If
        Else
            Call GetColorStart表示画像から色取得開始(sender)
        End If
    End Sub

    Private Sub ButtonStringBGFrameColorChange_Click(sender As Object, e As EventArgs) Handles ButtonStringBGFrameColorChange.Click
        Dim c1 As Color = Me.PictureBoxStringBGFrameColor1.BackColor
        Me.PictureBoxStringBGFrameColor1.BackColor = Me.PictureBoxStringBGFrameColor2.BackColor
        Me.PictureBoxStringBGFrameColor2.BackColor = c1
        Dim t1 As Integer = Me.NumericUpDownStringBGFrameTransparent1.Value
        Me.NumericUpDownStringBGFrameTransparent1.Value = Me.NumericUpDownStringBGFrameTransparent2.Value
        Me.NumericUpDownStringBGFrameTransparent2.Value = t1
        If CheckBoxStringBGFrame.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If
    End Sub



    Private Sub NumericUpDownStringBGFrameTransparent1_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringBGFrameTransparent1.ValueChanged
        If isOpenedForm3 = False Then Exit Sub
        Dim v1 As NumericUpDown = sender
        Dim v2 As NumericUpDown = Me.NumericUpDownStringBGFrameTransparent2

        If Me.CheckBoxStringBGFrameLinkedTransparent.Checked Then '透明度連動
            If v2.Value = v1.Value Then Exit Sub
            v2.Value = v1.Value
            If Me.CheckBoxStringBGFrame.Checked Then
                Call Form1.TextSample()
            End If
            Exit Sub
        End If

        If Me.CheckBoxStringBGFrame.Checked Then
            Call Form1.TextSample()
        End If

    End Sub

    Private Sub NumericUpDownStringBGFrameTransparent2_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringBGFrameTransparent2.ValueChanged
        If isOpenedForm3 = False Then Exit Sub
        Dim v1 As NumericUpDown = Me.NumericUpDownStringBGFrameTransparent1
        Dim v2 As NumericUpDown = sender

        If Me.CheckBoxStringBGFrameLinkedTransparent.Checked Then '透明度連動
            If v2.Value = v1.Value Then Exit Sub
            v1.Value = v2.Value
            If Me.CheckBoxStringBGFrame.Checked Then
                Call Form1.TextSample()
            End If
            Exit Sub
        End If

        If Me.CheckBoxStringBGFrame.Checked Then
            Call Form1.TextSample()
        End If

    End Sub


    Private Sub TrackBarStringBGRoundRect文字枠の丸さ_Scroll(sender As Object, e As EventArgs) Handles TrackBarStringBGRoundRect文字枠の丸さ.Scroll
        Dim tb As TrackBar = sender
        Me.LabelStringBGRoundRect文字枠の丸さ.Text = tb.Value.ToString
        If CheckBoxLinkedStringRoundRect枠.Checked Then
            TrackBarStringBackRoundrect.Value = tb.Value
            LabelStringBackRoundrect背景丸さ.Text = tb.Value.ToString
        End If
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()

    End Sub
    Private Sub GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(picBox As PictureBox)
        If Form1.IsGetColor表示画像から色取得中 Then
            If picBox.Equals(Form1.GetColorPic色取得中のPictureBox) Then
                Call GetColorEnd表示画像から色取得終了(picBox)
            Else
                Exit Sub
            End If
        Else
            Call GetColorStart表示画像から色取得開始(picBox)
        End If

    End Sub

    '縁取りタブ
    Private Sub ButtonStringFringeGradationAngleMin45縁取りグラデ角度45_Click(sender As Object, e As EventArgs) Handles ButtonStringFringeGradationAngleMin45縁取りグラデ角度45.Click
        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, NumericUpDownStringFringeGradationAngleグラデ角度)
    End Sub

    Private Sub ButtonStringFringeGradationAngleAdd45グラデ角度45_Click(sender As Object, e As EventArgs) Handles ButtonStringFringeGradationAngleAdd45グラデ角度45.Click
        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, NumericUpDownStringFringeGradationAngleグラデ角度)
    End Sub

    Private Sub NumericUpDownStringFringeTransparent1_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringFringeTransparent1.ValueChanged
        If isOpenedForm3 = False Then Exit Sub
        Dim n1 As NumericUpDown = sender
        Dim n2 As NumericUpDown = Me.NumericUpDownStringFringeTransparent2
        If Me.CheckBoxLinkedStringFringeTransparent.Checked Then '透明度連動
            If n2.Value = n1.Value Then Exit Sub
            n2.Value = n1.Value
            If Me.CheckBoxStringBGFrame.Checked Then '文字の書式の縁取りにチェックonなら
                Call Form1.TextSample()
            End If
            Exit Sub
        End If
        If Me.CheckBoxStringBGFrame.Checked Then '文字の書式の縁取りにチェックonなら
            Call Form1.TextSample()
        End If

    End Sub

    Private Sub NumericUpDownStringFringeTransparent2_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringFringeTransparent2.ValueChanged
        If isOpenedForm3 = False Then Exit Sub
        Dim n2 As NumericUpDown = sender
        Dim n1 As NumericUpDown = Me.NumericUpDownStringFringeTransparent1
        If Me.CheckBoxLinkedStringFringeTransparent.Checked Then '透明度連動
            If n1.Value = n2.Value Then Exit Sub
            n1.Value = n2.Value
            If Me.CheckBoxStringBGFrame.Checked Then '文字の書式の縁取りにチェックonなら
                Call Form1.TextSample()
            End If
            Exit Sub
        End If
        If Me.CheckBoxStringBGFrame.Checked Then '文字の書式の縁取りにチェックonなら
            Call Form1.TextSample()
        End If
    End Sub

    Private Sub ButtonStringFringeColorChange_Click(sender As Object, e As EventArgs) Handles ButtonStringFringeColorChange.Click
        Dim c1 As Color = Me.PictureBoxStringFringeColor1.BackColor
        Me.PictureBoxStringFringeColor1.BackColor = Me.PictureBoxStringFringeColor2.BackColor
        Me.PictureBoxStringFringeColor2.BackColor = c1
        Dim t1 As Integer = Me.NumericUpDownStringFringeTransparent1.Value
        Me.NumericUpDownStringFringeTransparent1.Value = Me.NumericUpDownStringFringeTransparent2.Value
        Me.NumericUpDownStringFringeTransparent2.Value = t1

    End Sub

    Private Sub PictureBoxStringFringeColor1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxStringFringeColor1.MouseDown
        Call GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(sender)
    End Sub

    Private Sub PictureBoxStringFringeColor2_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxStringFringeColor2.MouseDown
        Call GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(sender)
    End Sub

    Private Sub ButtonStringFringeColor1_Click(sender As Object, e As EventArgs) Handles ButtonStringFringeColor1.Click
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxStringFringeColor1, CheckBoxStringFringe.Checked)
    End Sub

    Private Sub ButtonStringFringeColor2_Click_1(sender As Object, e As EventArgs) Handles ButtonStringFringeColor2.Click
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxStringFringeColor2, CheckBoxStringFringe.Checked)
    End Sub

    '影
    Private Sub PictureBoxStringShadowColor1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxStringShadowColor1.MouseDown
        Call GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(sender)
    End Sub

    Private Sub PictureBoxStringShadowColor2_MouseDown(sender As Object, e As EventArgs) Handles PictureBoxStringShadowColor2.MouseDown
        Call GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(sender)
    End Sub

    Private Sub ButtonStringShadowColor1_Click(sender As Object, e As EventArgs) Handles ButtonStringShadowColor1.Click
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxStringShadowColor1, CheckBoxStringShadow.Checked)
    End Sub

    Private Sub ButtonStringShadowColor2_Click(sender As Object, e As EventArgs) Handles ButtonStringShadowColor2.Click
        Call ColorDialogカラーダイアログで色取得して見本更新(Me.PictureBoxStringShadowColor2, CheckBoxStringShadow.Checked)
    End Sub

    Private Sub ButtonStringShadowColorChange_Click(sender As Object, e As EventArgs) Handles ButtonStringShadowColorChange.Click
        Dim c1 As Color = Me.PictureBoxStringShadowColor1.BackColor
        Me.PictureBoxStringShadowColor1.BackColor = Me.PictureBoxStringShadowColor2.BackColor
        Me.PictureBoxStringShadowColor2.BackColor = c1
        Dim t1 As Integer = Me.NumericUpDownStringShadowColorTransparent1.Value
        Me.NumericUpDownStringShadowColorTransparent1.Value = Me.NumericUpDownStringShadowColorTransparent2.Value
        Me.NumericUpDownStringShadowColorTransparent2.Value = t1

    End Sub

    Private Sub NumericUpDownStringShadowColorTransparent1_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringShadowColorTransparent1.ValueChanged
        If isOpenedForm3 = False Then Exit Sub
        Dim n1 As NumericUpDown = sender
        Dim n2 As NumericUpDown = Me.NumericUpDownStringShadowColorTransparent2
        If Me.CheckBoxLinkedStringShadowColorTransparent.Checked Then '透明度連動
            If n2.Value = n1.Value Then Exit Sub
            n2.Value = n1.Value
            If Me.CheckBoxStringShadow.Checked Then '文字の書式の縁取りにチェックonなら
                Call Form1.TextSample()
            End If
            Exit Sub
        End If
        If Me.CheckBoxStringShadow.Checked Then '文字の書式の縁取りにチェックonなら
            Call Form1.TextSample()
        End If

    End Sub

    Private Sub NumericUpDownStringShadowColorTransparent2_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringShadowColorTransparent2.ValueChanged
        If isOpenedForm3 = False Then Exit Sub
        Dim n2 As NumericUpDown = sender
        Dim n1 As NumericUpDown = Me.NumericUpDownStringShadowColorTransparent1
        If Me.CheckBoxLinkedStringShadowColorTransparent.Checked Then '透明度連動
            If n1.Value = n2.Value Then Exit Sub
            n1.Value = n2.Value
            If Me.CheckBoxStringShadow.Checked Then '文字の書式の縁取りにチェックonなら
                Call Form1.TextSample()
            End If
            Exit Sub
        End If
        If Me.CheckBoxStringShadow.Checked Then '文字の書式の縁取りにチェックonなら
            Call Form1.TextSample()
        End If
    End Sub

    Private Sub ButtonStringShadowGradationAngleMin45_Click(sender As Object, e As EventArgs) Handles ButtonStringShadowGradationAngleMin45.Click
        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, Me.NumericUpDownStringShadowAngle)
    End Sub

    Private Sub ButtonStringShadowGradationAngleAdd45_Click(sender As Object, e As EventArgs) Handles ButtonStringShadowGradationAngleAdd45.Click
        Call ButtonTextAddボタンの表示数値をヌメリックに加算(sender, Me.NumericUpDownStringShadowAngle)
    End Sub

    '丸さの連動チェックボックスリンク
    Private Sub CheckBoxLinkedStringRoundRect背景_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxLinkedStringRoundRect背景.CheckedChanged

        Dim you As CheckBox = Me.CheckBoxLinkedStringRoundRect枠
        Dim iCS As CheckState = CheckBoxLinkedStringRoundRect背景.CheckState
        If iCS = CheckState.Checked And you.CheckState = CheckState.Unchecked Then
            Dim maru As Integer = TrackBarStringBackRoundrect.Value
            TrackBarStringBGRoundRect文字枠の丸さ.Value = maru
            LabelStringBGRoundRect文字枠の丸さ.Text = maru.ToString
            'you.CheckState = iCS

        End If
        you.CheckState = iCS
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub

    Private Sub CheckBoxLinkedStringRoundRect枠_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxLinkedStringRoundRect枠.CheckedChanged
        Dim youCB As CheckBox = CheckBoxLinkedStringRoundRect背景
        Dim iCS As CheckState = CheckBoxLinkedStringRoundRect枠.CheckState
        If iCS = CheckState.Checked And youCB.CheckState = CheckState.Unchecked Then
            Dim maru As Integer = TrackBarStringBGRoundRect文字枠の丸さ.Value
            TrackBarStringBackRoundrect.Value = maru
            LabelStringBackRoundrect背景丸さ.Text = maru.ToString
            'youCB.CheckState = iCS
        End If
        youCB.CheckState = iCS
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()

    End Sub
    Private Sub DrawString複数行文字列の描画(str As String)
        'Dim str As String = Me.TextBoxMultilineString.Text
        If str = "" Then Exit Sub

        Dim statusStr As String = str.Replace(vbCrLf, "") 'ステータス表示用、画像の名前
        'statusStr = str.Replace(vbCr, "")
        'statusStr = str.Replace(vbLf, "")
        'statusStr = statusStr.Replace(vbCr, "")
        'statusStr = statusStr.Replace(vbLf, "")
        Dim bmp As Bitmap
        Dim myFont As Font = CreateFontフォント作成()
        Dim lineSpace As Integer = NumericUpDownLineSpace行間.Value

        If Me.CheckBoxDrawStringSplit1文字づつ.Checked Then
            For i As Integer = 0 To str.Length - 1
                Dim mozi As String = str.Chars(i)

                If mozi <> vbCr AndAlso mozi <> vbLf Then '改行文字は描画しない、vbCrLfだとすり抜ける
                    bmp = GetDrawStringAll文字と背景画像作成(mozi, myFont)
                    Call Form1.PicBoxAdd("文字_" & mozi, bmp, drawString:=mozi)
                End If

            Next
        Else
            bmp = GetDrawStringAll文字と背景画像作成(str, myFont)
            Call Form1.PicBoxAdd("文字_" & statusStr, bmp, drawString:=str)

            '
            Dim sFormat As New StringFormat
            If RadioButtonStringH.Checked Then
                sFormat.FormatFlags = StringFormatFlags.NoClip
            Else
                sFormat.FormatFlags = StringFormatFlags.DirectionVertical Or StringFormatFlags.DirectionRightToLeft
            End If


        End If

    End Sub
    Private Sub ButtonDrawString複数行文字列の描画_Click(sender As Object, e As EventArgs) Handles ButtonDrawString複数行文字列の描画.Click
        Dim str As String = Me.TextBoxMultilineString.Text
        If str = "" Then Exit Sub

        Call DrawString複数行文字列の描画(str)
        Call AddStringコンボボックスに文字列を追加()

    End Sub


    Private Sub ComboBoxString_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBoxString.SelectedIndexChanged
        Me.TextBoxMultilineString.Text = Me.ComboBoxString.SelectedItem
        Me.ComboBoxString.SelectAll()
    End Sub

    Private Sub TextBoxMultilineString_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBoxMultilineString.KeyDown
        Dim str As String = Me.TextBoxMultilineString.Text
        If str = "" Then Exit Sub

        If e.KeyData = Keys.Enter + Keys.Control Then
            Call DrawString複数行文字列の描画(str)

            'Dim statusStr As String = str.Replace(vbCrLf, "") 'ステータス表示用、画像の名前

            'Dim bmp As Bitmap = GetDrawStringBmp文字の描画作成(str, CreateFontフォント作成())
            'Call Form1.PicBoxAdd("文字_" & statusStr, bmp, drawString:=str)

            ''Call DrawString文字の描画()
            Call AddStringコンボボックスに文字列を追加()
            ''e.Handled = True
            ''一個前の改行を消す

            ''Dim str As String = Me.TextBoxMultilineString.Text
            ''Dim ooo = str.Length
            ' ''str = str.Chars(str.Length - 1)
            ' ''str.Substring(0, str.Length - 3)
            ''Me.TextBoxMultilineString.Text = str
            ''SendKeys.Send(Keys.Back)

        End If
    End Sub
    Private Sub AddStringコンボボックスに文字列を追加()
        Dim str As String = Me.TextBoxMultilineString.Text
        If str = "" Then Exit Sub
        Dim cb As ComboBox = Me.ComboBoxString

        Dim tb As TextBox = Me.TextBoxMultilineString
        tb.Focus()
        tb.SelectionStart = tb.TextLength

        'Dim f As Boolean = True
        For Each c As String In cb.Items
            If c = str Then
                Exit Sub
            End If
        Next

        cb.Items.Insert(0, str)
        cb.SelectedIndex = 0

    End Sub

    Private Sub CheckBoxStringBGFrame_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxStringBGFrame.CheckedChanged
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub

    Private Sub CheckBoxStringFringe_CheckedChanged_1(sender As Object, e As EventArgs) Handles CheckBoxStringFringe.CheckedChanged
        Call OpenFrom3の起動完了チェックして文字の見本の再描画()
    End Sub

    Private Sub PictureBoxStringBGFrameColor1_BackColorChanged(sender As Object, e As EventArgs) Handles PictureBoxStringBGFrameColor1.BackColorChanged
        If CheckBoxStringBGFrame.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If

    End Sub

    Private Sub PictureBoxStringBGFrameColor2_BackColorChanged(sender As Object, e As EventArgs) Handles PictureBoxStringBGFrameColor2.BackColorChanged
        If CheckBoxStringBGFrame.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If
    End Sub

    Private Sub CheckBoxStringBGFrameGradation_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxStringBGFrameGradation.CheckedChanged
        If CheckBoxStringBGFrame.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If
    End Sub

    Private Sub CheckBoxStringBGFrameColorGamma_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxStringBGFrameColorGamma.CheckedChanged
        If CheckBoxStringBGFrame.Checked AndAlso Me.CheckBoxStringBGFrameGradation.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If
    End Sub

    Private Sub NumericUpDownStringBGFrameWidth枠幅_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringBGFrameWidth枠幅.ValueChanged
        If CheckBoxStringBGFrame.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()
        End If
    End Sub

    Private Sub PictureBoxStringShadowColor1_BackColorChanged(sender As Object, e As EventArgs) Handles PictureBoxStringShadowColor1.BackColorChanged
        If CheckBoxStringShadow.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()

        End If
    End Sub

    Private Sub PictureBoxStringShadowColor2_BackColorChanged(sender As Object, e As EventArgs) Handles PictureBoxStringShadowColor2.BackColorChanged
        If CheckBoxStringShadow.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()

        End If
    End Sub

    Private Sub CheckBoxStringShadowGradation_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxStringShadowGradation.CheckedChanged
        If CheckBoxStringShadow.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()

        End If
    End Sub

    Private Sub CheckBoxStringShadowGamma_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxStringShadowGamma.CheckedChanged
        If CheckBoxStringShadow.Checked AndAlso Me.CheckBoxStringShadowGradation.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()

        End If
    End Sub

    Private Sub NumericUpDownStringShadowAngle_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringShadowAngle.ValueChanged
        If CheckBoxStringShadow.Checked AndAlso Me.CheckBoxStringShadowGradation.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()

        End If
    End Sub

    Private Sub NumericUpDownStringShadowH横_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringShadowH横.ValueChanged
        If CheckBoxStringShadow.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()

        End If
    End Sub

    Private Sub NumericUpDownStringShadowV縦_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringShadowV縦.ValueChanged
        If CheckBoxStringShadow.Checked Then
            Call OpenFrom3の起動完了チェックして文字の見本の再描画()

        End If
    End Sub


    Private Sub NumericUpDownStringColorT1_不透明度_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringColorT1_不透明度.ValueChanged
        If isOpenedForm3 = False Then Exit Sub
        Dim n1 As NumericUpDown = sender
        Dim n2 As NumericUpDown = Me.NumericUpDownStringColorT2_不透明度
        If Me.CheckBoxLinkedStringColorTransparent.Checked Then
            If n2.Value = n1.Value Then Exit Sub
            n2.Value = n1.Value
            Call Form1.TextSample()
            Exit Sub
        End If
        Call Form1.TextSample()

    End Sub

    Private Sub NumericUpDownStringColorT2_不透明度_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDownStringColorT2_不透明度.ValueChanged
        If isOpenedForm3 = False Then Exit Sub
        Dim n1 As NumericUpDown = Me.NumericUpDownStringColorT1_不透明度
        Dim n2 As NumericUpDown = sender
        If Me.CheckBoxLinkedStringColorTransparent.Checked Then
            If n1.Value = n2.Value Then Exit Sub
            n1.Value = n2.Value
            If Me.CheckBoxTextGradation.Checked Then
                Call Form1.TextSample()
            End If
        End If
        Call Form1.TextSample()

    End Sub

    Private Sub ButtonStringT透明_Click(sender As Object, e As EventArgs) Handles ButtonStringT透明.Click
        Me.NumericUpDownStringColorT1_不透明度.Value = 0
    End Sub

    Private Sub ButtonStringT半透明_Click(sender As Object, e As EventArgs) Handles ButtonStringT半透明.Click
        Me.NumericUpDownStringColorT1_不透明度.Value = 128
    End Sub

    Private Sub ButtonStringT不透明_Click(sender As Object, e As EventArgs) Handles ButtonStringT不透明.Click
        Me.NumericUpDownStringColorT1_不透明度.Value = 255
    End Sub

    Private Sub Button12_Click(sender As Object, e As EventArgs) Handles ButtonStringFringeT透明.Click
        NumericUpDownStringFringeTransparent1.Value = 0
    End Sub

    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles ButtonStringFringeT半透明.Click
        NumericUpDownStringFringeTransparent1.Value = 128
    End Sub

    Private Sub Button10_Click(sender As Object, e As EventArgs) Handles ButtonStringFringeT不透明.Click
        NumericUpDownStringFringeTransparent1.Value = 255
    End Sub

    Private Sub ButtonStringWakuT透明_Click(sender As Object, e As EventArgs) Handles ButtonStringWakuT透明.Click
        NumericUpDownStringBGFrameTransparent1.Value = 0
    End Sub

    Private Sub ButtonStringWakuT半透明_Click(sender As Object, e As EventArgs) Handles ButtonStringWakuT半透明.Click
        NumericUpDownStringBGFrameTransparent1.Value = 128
    End Sub

    Private Sub ButtonStringWakuT不透明_Click(sender As Object, e As EventArgs) Handles ButtonStringWakuT不透明.Click
        NumericUpDownStringBGFrameTransparent1.Value = 255
    End Sub

    Private Sub ButtonStringShadowT透明_Click(sender As Object, e As EventArgs) Handles ButtonStringShadowT透明.Click
        NumericUpDownStringShadowColorTransparent1.Value = 0
    End Sub

    Private Sub ButtonStringShadowT半透明_Click(sender As Object, e As EventArgs) Handles ButtonStringShadowT半透明.Click
        NumericUpDownStringShadowColorTransparent1.Value = 128
    End Sub

    Private Sub ButtonStringShadowT不透明_Click(sender As Object, e As EventArgs) Handles ButtonStringShadowT不透明.Click
        NumericUpDownStringShadowColorTransparent1.Value = 255
    End Sub


    Private Sub CheckBoxSelectPicFrameView選択時に枠表示_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxSelectPicFrameView選択時に枠表示.CheckedChanged
        If Form1.ActExPic.IsDrawString = False Then Exit Sub

        If Me.CheckBoxSelectPicFrameView選択時に枠表示.Checked Then ' And Form1.ActExPic.IsDrawString Then
            Call Form1.PicBoderLineLabel画像に枠を作成表示(Form1.ActExPic)
        Else 'If Form1.ActExPic.IsDrawString Then
            Call Form1.PicBoderLineLabel画像に付けた枠を消去()
        End If
    End Sub

    Private Sub ButtonColorRemap色の入れ替え_Click(sender As Object, e As EventArgs) Handles ButtonColorRemap色の入れ替え.Click
        Dim c1 As Color = Me.PictureBoxColorRemapOld.BackColor
        Dim c2 As Color = Me.PictureBoxColorRemapNew.BackColor
        Call Form1.ColorRemap色の入れ替え(c1, c2)

    End Sub

    Private Sub ButtonColorRemapOld_Click(sender As Object, e As EventArgs) Handles ButtonColorRemapOld.Click
        Call ColorDialogToPictureBackColorカラーダイアログの色をピクチャーボックスに(Me.PictureBoxColorRemapOld)
    End Sub
    Private Sub ColorDialogToPictureBackColorカラーダイアログの色をピクチャーボックスに(pbox As PictureBox)
        Dim cd1 As ColorDialog = Me.ColorDialog1
        If cd1.ShowDialog = Windows.Forms.DialogResult.OK Then
            pbox.BackColor = cd1.Color
        End If
    End Sub

    Private Sub ButtonColorRemapNew_Click(sender As Object, e As EventArgs) Handles ButtonColorRemapNew.Click
        Call ColorDialogToPictureBackColorカラーダイアログの色をピクチャーボックスに(Me.PictureBoxColorRemapNew)
    End Sub

    Private Sub PictureBoxColorRemapOld_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxColorRemapOld.MouseDown
        Call GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(sender)
    End Sub

    Private Sub PictureBoxColorRemapNew_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxColorRemapNew.MouseDown
        Call GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(sender)
    End Sub


    Private Sub ButtonGetAverageColor_Click(sender As Object, e As EventArgs) Handles ButtonGetAverageColor.Click
        Call Form1.GetAverageColor平均色()
    End Sub

    '色相変換ここから
    Private Sub TrackBarHueOld_Scroll(sender As Object, e As EventArgs) Handles TrackBarHueOld.Scroll
        Dim valH As Integer = TrackBarOppositeトラックバーの反対数値(sender)
        Dim picbox As PictureBox = Me.PictureBoxHueOld
        Dim s As Single = Me.TrackBarSaturationOld.Value / 100
        Dim l As Single = Me.TrackBarBrightnessOld.Value / 100
        picbox.BackColor = Form1.HSLtoRGB(valH, s, l)
        'Dim h As Integer = CInt(Math.Round(picbox.BackColor.GetHue))
        Me.LabelHueOld.Text = valH 'h
        Me.ToolTip1.SetToolTip(picbox, "色相_" & valH & vbNewLine & picbox.BackColor.ToString)

        Call ChangeSaturationColorBar彩度カラーバー更新(Me.PictureBoxSaturationOld, valH, l)
        Call ChangeBrightnessColorBar明度カラーバー更新(Me.PictureBoxBrightnessOld, valH, s)

    End Sub
    Private Sub TrackBarHueNew_Scroll(sender As Object, e As EventArgs) Handles TrackBarHueNew.Scroll
        Dim pbox As PictureBox = Me.PictureBoxHueNew
        'pbox.BackColor = Form1.HSLtoRGB(TrackBarOppositeトラックバーの反対数値(sender), 1, 0.5)
        Dim h As Integer = TrackBarOppositeトラックバーの反対数値(Me.TrackBarHueNew) ' CInt(Math.Round(pbox.BackColor.GetHue))
        Dim s As Single = Me.TrackBarSaturationNew.Value / 100
        Dim l As Single = Me.TrackBarBrightnessNew.Value / 100
        pbox.BackColor = Form1.HSLtoRGB(h, s, l)

        Me.ToolTip1.SetToolTip(pbox, "色相_" & h & vbNewLine & pbox.BackColor.ToString)
        Me.LabelHueNew.Text = h

        Call ChangeBrightnessColorBar明度カラーバー更新(Me.PictureBoxBrightnessNew, h, s)
        Call ChangeSaturationColorBar彩度カラーバー更新(Me.PictureBoxSaturationNew, h, l)

    End Sub
    Private Sub ChangeSaturationColorBar彩度カラーバー更新(pBox As PictureBox, h As Integer, l As Single)
        Dim bmp As New Bitmap(pBox.Width, pBox.Height)
        Dim bmpH As Integer = bmp.Height

        For y As Integer = 0 To bmp.Height - 1
            For x As Integer = 0 To bmp.Width - 1
                bmp.SetPixel(x, y, Form1.HSLtoRGB(h, (bmpH - y) / bmpH, l))

            Next
        Next
        pBox.Image = bmp

    End Sub
    Private Sub ChangeBrightnessColorBar明度カラーバー更新(pBox As PictureBox, h As Integer, s As Single)
        Dim bmp As New Bitmap(pBox.Width, pBox.Height)
        Dim bmpH As Integer = bmp.Height
        For y As Integer = 0 To bmp.Height - 1
            For x As Integer = 0 To bmp.Width - 1
                bmp.SetPixel(x, y, Form1.HSLtoRGB(h, s, (bmpH - y) / bmpH))

            Next
        Next
        pBox.Image = bmp

    End Sub

    Private Sub PictureBoxHueOld_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxHueOld.MouseDown
        Dim pbox As PictureBox = sender
        Call GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(pbox)
        Call PictureBoxToolTipピクチャーボックスのツールチップ更新(pbox)
    End Sub

    Private Sub PictureBoxHueNew_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBoxHueNew.MouseDown
        Dim pbox As PictureBox = sender
        Call GetColorToPictureBoxBackcolorピクチャーボックスに画像から色の取得(pbox)
        Call PictureBoxToolTipピクチャーボックスのツールチップ更新(pbox)
    End Sub

    Private Sub PictureBoxHueOld_BackColorChanged(sender As Object, e As EventArgs) Handles PictureBoxHueOld.BackColorChanged
        If Form1.IsGetColor表示画像から色取得中 Then '画像から色を取得中の時だけ有効
            Dim pb As PictureBox = sender
            Dim col As Color = pb.BackColor
            Dim h As Integer = CInt(col.GetHue)
            If h = 360 Then h = 0
            Dim s As Single = col.GetSaturation
            Dim l As Single = col.GetBrightness
            'トラックバーに値をセット
            SetTrackBarOppositeトラックバーに反対数値をセット(Me.TrackBarHueOld, h)
            Me.TrackBarSaturationOld.Value = CInt(s * 100)
            Me.TrackBarBrightnessOld.Value = CInt(l * 100)
            'カラーバーーの更新
            Call ChangeBrightnessColorBar明度カラーバー更新(Me.PictureBoxBrightnessOld, h, s)
            Call ChangeSaturationColorBar彩度カラーバー更新(Me.PictureBoxSaturationOld, h, l)
            'ラベル表示更新
            Me.LabelHueOld.Text = String.Format("{0:000}", h)
            Me.LabelSaturationOld.Text = Format("d3", s)
            Me.LabelBrightnessOld.Text = Format("d2", l)
            Me.LabelRGB_Old.Text = "赤=" & String.Format("{0:D3}", col.R) & vbNewLine &
                "緑=" & String.Format("{0:000}", col.G) & vbNewLine &
                "青=" & String.Format("{0:d3}", col.B)

            Call PictureBoxToolTipピクチャーボックスのツールチップ更新(pb)
        End If
    End Sub
    Private Sub PictureBoxHueNew_BackColorChanged(sender As Object, e As EventArgs) Handles PictureBoxHueNew.BackColorChanged
        If Form1.IsGetColor表示画像から色取得中 Then
            Dim pb As PictureBox = sender
            Dim col As Color = pb.BackColor
            Dim h As Integer = CInt(col.GetHue)
            If h = 360 Then h = 0
            Dim s As Single = col.GetSaturation
            Dim l As Single = col.GetBrightness
            'トラックバーに値をセット
            SetTrackBarOppositeトラックバーに反対数値をセット(Me.TrackBarHueNew, h)
            Me.TrackBarSaturationNew.Value = CInt(s * 100)
            Me.TrackBarBrightnessNew.Value = CInt(l * 100)
            'カラーバーーの更新
            Call ChangeBrightnessColorBar明度カラーバー更新(Me.PictureBoxBrightnessNew, h, s)
            Call ChangeSaturationColorBar彩度カラーバー更新(Me.PictureBoxSaturationNew, h, l)
            'ラベル表示更新
            Me.LabelHueNew.Text = h
            Me.LabelSaturationNew.Text = Format("d3", s)
            Me.LabelBrightnessNew.Text = Format("d2", l)
            Call PictureBoxToolTipピクチャーボックスのツールチップ更新(pb)
        End If
    End Sub
    Private Sub PictureBoxToolTipピクチャーボックスのツールチップ更新(pb As PictureBox)
        'Dim h As Integer = CInt(Math.Round(pb.BackColor.GetHue))
        'Dim s As Single = pb.BackColor.GetSaturation
        'Dim l As Single = pb.BackColor.GetBrightness
        'Call SetTrackBarOppositeトラックバーに反対数値をセット(tb, h)
        'Me.ToolTip1.SetToolTip(pb, "色相=" & h & "、彩度=" & s & "、明るさ=" & l & vbNewLine & pb.BackColor.ToString)
        Me.ToolTip1.SetToolTip(pb, pb.BackColor.ToString)

        'lbl.Text = h.ToString

        'Me.TrackBarSaturationOld.Value = CInt(s * 100)
        'Me.TrackBarBrightnessOld.Value = CInt(l * 100)
        'Me.LabelSaturationOld.Text = Format("d3", s)
        'Me.LabelBrightnessOld.Text = Format("d2", l)

    End Sub
    Private Sub ButtonChangeHue_Click(sender As Object, e As EventArgs) Handles ButtonChangeColor.Click
        Dim c As Cursor = Me.Cursor
        Me.Cursor = Cursors.WaitCursor
        Call ChangeHue色相変換()
        Me.Cursor = c

    End Sub
    Private Sub ChangeHue色相変換()
        Dim oldHue As Integer = CInt(Me.LabelHueOld.Text) '変換の対象になる色の色相値
        Dim newHue As Integer = CInt(Me.LabelHueNew.Text)
        Dim colorWidth As Integer = Me.NumericUpDownChangeHueWidth.Value '変換対象となる色相の幅
        Dim oneColor As Boolean = Me.CheckBoxHueOneColor.Checked
        Call Form1.ShiftHue色相移動(Form1.ActExPic, oldHue, newHue, colorWidth, oneColor)

    End Sub
    Private Sub TrackBarSaturation_Scroll(sender As Object, e As EventArgs) Handles TrackBarSaturationOld.Scroll
        Dim picbox As PictureBox = Me.PictureBoxHueOld
        'Dim col As Color = picbox.BackColor
        Dim s As Single = Me.TrackBarSaturationOld.Value / 100
        Dim h As Single = TrackBarOppositeトラックバーの反対数値(Me.TrackBarHueOld)
        Dim l As Single = Me.TrackBarBrightnessOld.Value / 100
        picbox.BackColor = Form1.HSLtoRGB(h, s, l)
        Me.LabelSaturationOld.Text = Strings.Format("d3", s)

        Call ChangeBrightnessColorBar明度カラーバー更新(Me.PictureBoxBrightnessOld, h, s)
        Call PictureBoxToolTipピクチャーボックスのツールチップ更新(picbox)
    End Sub
    Private Sub TrackBarBrightnessOld_Scroll(sender As Object, e As EventArgs) Handles TrackBarBrightnessOld.Scroll
        '明るさ旧
        Dim pbox As PictureBox = Me.PictureBoxHueOld
        Dim tb As TrackBar = sender
        'Dim col As Color = pbox.BackColor
        Dim h As Single = TrackBarOppositeトラックバーの反対数値(Me.TrackBarHueOld)
        Dim s As Single = Me.TrackBarSaturationOld.Value / 100
        Dim l As Single = tb.Value / 100
        pbox.BackColor = Form1.HSLtoRGB(h, s, l)
        Me.LabelBrightnessOld.Text = Strings.Format("d3", l)

        Call ChangeSaturationColorBar彩度カラーバー更新(Me.PictureBoxSaturationOld, h, l)
        Call PictureBoxToolTipピクチャーボックスのツールチップ更新(pbox)
    End Sub
    Private Sub TrackBarSaturationNew_Scroll(sender As Object, e As EventArgs) Handles TrackBarSaturationNew.Scroll
        Dim h As Integer = TrackBarOppositeトラックバーの反対数値(Me.TrackBarHueNew)
        Dim s As Single = Me.TrackBarSaturationNew.Value / 100
        Dim l As Single = Me.TrackBarBrightnessNew.Value / 100
        Me.LabelSaturationNew.Text = Format("d", s) 'ラベル表示更新
        Dim pb As PictureBox = Me.PictureBoxHueNew
        pb.BackColor = Form1.HSLtoRGB(h, s, l) 'ピクチャーボックスの色更新
        Call ChangeBrightnessColorBar明度カラーバー更新(Me.PictureBoxBrightnessNew, h, s) '明度カラーバーの更新
        Call PictureBoxToolTipピクチャーボックスのツールチップ更新(pb)
    End Sub

    Private Sub TrackBarBrightnessNew_Scroll(sender As Object, e As EventArgs) Handles TrackBarBrightnessNew.Scroll
        Dim h As Integer = TrackBarOppositeトラックバーの反対数値(Me.TrackBarHueNew)
        Dim s As Single = Me.TrackBarSaturationNew.Value / 100
        Dim l As Single = Me.TrackBarBrightnessNew.Value / 100
        Me.LabelBrightnessNew.Text = Format("d", l) 'ラベル表示更新
        Dim pb As PictureBox = Me.PictureBoxHueNew
        pb.BackColor = Form1.HSLtoRGB(h, s, l) 'ピクチャーボックスの色更新
        Call ChangeSaturationColorBar彩度カラーバー更新(Me.PictureBoxSaturationNew, h, l) '彩度カラーバーの更新
        Call PictureBoxToolTipピクチャーボックスのツールチップ更新(pb)
    End Sub
    Private Sub CheckBoxBrightnessNew_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxBrightnessNew.CheckedChanged
        Dim nume As NumericUpDown = Me.NumericUpDownBrightnessRevise明度補正
        If Me.CheckBoxBrightnessNew.Checked Then
            nume.Enabled = False
        Else
            nume.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxSaturationNew_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxSaturationNew.CheckedChanged
        Dim nume As NumericUpDown = Me.NumericUpDownSaturationRevise彩度補正
        If Me.CheckBoxSaturationNew.Checked Then
            nume.Enabled = False
        Else
            nume.Enabled = True
        End If
    End Sub
    Private Sub ButtonHueInvert色相反転_Click(sender As Object, e As EventArgs) Handles ButtonColorChange変換実行.Click
        Dim sftHue As Integer = 0
        If Me.CheckBoxHueShift色相移動.Checked Then
            sftHue = Me.NumericUpDownHueShift.Value
        End If

        Call Form1.HueInvert色相反転(Form1.ActExPic, sftHue)

    End Sub
   
    '色相変換ここまで




 
End Class