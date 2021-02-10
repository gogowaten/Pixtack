Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Public Class Form6
    Private FontSLフォントの設定リスト As New FontSettingList
    Private myForm3 As Form3 = New Form3 'テスト機能のウィンドウ
    Private fileName As String '設定ファイル名
    Private backupFontSetting As New FontSetting '直前に削除したフォント設定を復活する用
    'Private backupInteger As Integer '直前に削除したフォント設定を復活する用

    Private Sub Form6_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        'Dim fileName As String = "F:\png\fontsetting.bin"
        Dim fD As String = My.Application.Info.DirectoryPath 'アプリの実行ファイルのある場所
        Dim fN As String = "FontSetting.bin" '設定ファイル名
        fileName = fD & "\" & fN '設定ファイルのフルパス作成
        myForm3 = Me.Owner 'myForm3(テスト機能)をForm6の所有者にする、これがないとmyForm3に設定を書き込んでも無視される

        '設定ファイルの読み込み、デシリアライズ
        Call SettingLoadDeserializeデシリアライズ()

        Call InitializeDataGridデータグリッドビュー初期化()
        Dim nl() As String = FontSLフォントの設定リスト.GetNameList()
        Me.ComboBoxFontSetting.Items.AddRange(nl)

        'Call InitializeDataGridデータグリッドビュー初期化()

    End Sub
    Private Sub SettingLoadDeserializeデシリアライズ()
        If File.Exists(fileName) = False Then Exit Sub '設定ファイルがなければ読み込みをキャンセル
        '設定ファイルの読み込み、デシリアライズ
        Using fst As New FileStream(fileName, FileMode.Open)

            Dim bin As New BinaryFormatter()
            FontSLフォントの設定リスト = bin.Deserialize(fst)

        End Using

    End Sub

    'コンボボックスの項目変更した時、設定をForm3に書き込む
    Private Sub ComboBoxFontSetting_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBoxFontSetting.SelectedIndexChanged
        Dim int As Integer = Me.ComboBoxFontSetting.SelectedIndex
        If int < 0 Then Exit Sub

        Dim fs As FontSetting = FontSLフォントの設定リスト.Item(int)

        Call SetDataToForm3設定をForm3に書き込み(fs)
        Call Form1.TextSample() '見本の再描画’これは必要ないかも？

        With Me.DataGridView1 'コンボボックスと同じ行を選択、その行がスクロール範囲外にあったら、そこまでスクロールする
            If .Rows(int).Displayed = False Then
                .FirstDisplayedScrollingRowIndex = int

            End If

            Dim ac As Point = .CurrentCellAddress 'アクティブセルアドレス
            If ac.Y <> int Then '選択しているセルの行がコンボボックスと違っていたらコンボボックスに合わせる
                .ClearSelection()
                ''DataGridView1(1, int).Selected = True
                ''.CurrentCell = .SelectedCells.Item(0)
                .CurrentCell = DataGridView1(1, int) 'アクティブセルの指定
            End If
            Dim ooo = .SelectedCells
        End With

    End Sub

    
    Private Sub SetDataToForm3設定をForm3に書き込み(fs As FontSetting)
        Dim int As Integer = Me.ComboBoxFontSetting.SelectedIndex
        '設定をForm3に書き込み
        Form1.IsFontSettingNow = True '設定を書き込み中は見本を再描画しないフラグ
        With myForm3
            '.RadioButtonStringH.Checked = fs.Yokogaki
            If fs.Yokogaki Then
                .RadioButtonStringH.Checked = True
            Else
                .RadioButtonStringV.Checked = True
            End If
            '.ButtonFontColor.ForeColor = fs.ForeColor1
            '.ButtonFontColor2.ForeColor = fs.ForeColor2
            '色
            .PictureBoxTextColor1.BackColor = fs.FontColor1
            .PictureBoxTextColor2.BackColor = fs.FontColor2
            .NumericUpDownStringColorT1_不透明度.Value = fs.FontTransparent1
            .NumericUpDownStringColorT2_不透明度.Value = fs.FontTransparent2

            'グラデーション
            .CheckBoxTextGradation.Checked = fs.IsGradation
            .NumericUpDownStringGradientAngle.Value = fs.FontGradationAngel
            .CheckBoxStringGamma.Checked = fs.IsGammaC

            '基本
            .ComboBoxAllFonts.Text = fs.FontName
            .RadioButtonStringH.Checked = fs.Yokogaki
            .NumericUpDownFontSize.Value = fs.Size
            .CheckBoxTextItalic.Checked = fs.Italic
            .CheckBoxTextBold.Checked = fs.Bold
            .NumericUpDownLineSpace行間.Value = fs.LineSpace
            .NumericUpDownWordSpace文字間.Value = fs.WordSpace

            'その他
            .CheckBoxStringAntiAlias.Checked = fs.AntiAlias
            .NumericUpDownStringAngle.Value = fs.Angle
            .CheckBoxStringAngle.Checked = fs.IsAngle
            .CheckBoxStringAdjust位置調整.Checked = fs.IsAdjust



            '背景
            .CheckBoxTextBackColor背景色の有無.Checked = fs.IsNotBackColor
            If fs.IsNotBackColor Then
                .PictureBoxStringBGColor1.BackColor = fs.BGColor1
                .PictureBoxStringBGColor2.BackColor = fs.BGColor2
                .NumericUpDownTextTransparent1.Value = fs.BGTransparent1
                .NumericUpDownTextTransparent2.Value = fs.BGTransparent2
                'グラデーション
                .CheckBoxTextBackC_Gradation.Checked = fs.IsBGGradation
                .CheckBoxTextBackC_Gamma.Checked = fs.IsBGGammaC
                .NumericUpDownStringBackGradAngle文字背景グラデ角度.Value = fs.BGGradationAngel
                '角の丸さ
                '.CheckBoxLinkedStringRoundRect背景.Checked = fs.BGLinkedRound
                .TrackBarStringBackRoundrect.Value = fs.BGRound
            End If


            '枠
            .CheckBoxStringBGFrame.Checked = fs.IsFrame
            If fs.IsFrame Then
                '枠色
                .PictureBoxStringBGFrameColor1.BackColor = fs.wakuColor1
                .PictureBoxStringBGFrameColor2.BackColor = fs.wakuColor2
                .NumericUpDownStringBGFrameTransparent1.Value = fs.wakuTransparent1
                .NumericUpDownStringBGFrameTransparent2.Value = fs.wakuTransparent2
                '枠グラデーション
                .CheckBoxStringBGFrameGradation.Checked = fs.IsWakuGradation
                .CheckBoxStringBGFrameColorGamma.Checked = fs.IsWakuGammaC
                .NumericUpDownStringBGFrameGradationAngle.Value = fs.wakuGradationAngle
                '枠角の丸さ
                .TrackBarStringBGRoundRect文字枠の丸さ.Value = fs.wakuRound
                '枠幅
                .NumericUpDownStringBGFrameWidth枠幅.Value = fs.wakuWidth
            End If


            '影
            .CheckBoxStringShadow.Checked = fs.IsShadow
            If fs.IsShadow Then
                .PictureBoxStringShadowColor1.BackColor = fs.ShadowColor1
                .PictureBoxStringShadowColor2.BackColor = fs.ShadowColor2
                .NumericUpDownStringShadowColorTransparent1.Value = fs.ShadowTranstarent1
                .NumericUpDownStringShadowColorTransparent2.Value = fs.ShadowTranstarent2
                '影グラデーション
                .CheckBoxStringShadowGradation.Checked = fs.IsShadowGradation
                .CheckBoxStringShadowGamma.Checked = fs.IsShadowGammaC
                .NumericUpDownStringShadowAngle.Value = fs.ShadowGradationAngle
                '影位置
                .NumericUpDownStringShadowH横.Value = fs.ShadowH
                .NumericUpDownStringShadowV縦.Value = fs.ShadowV
            End If


            '縁取り
            .CheckBoxStringFringe.Checked = fs.IsFringe
            If fs.IsFringe Then
                .PictureBoxStringFringeColor1.BackColor = fs.FringeColor
                .PictureBoxStringFringeColor2.BackColor = fs.FringeColor2
                .NumericUpDownStringFringeTransparent1.Value = fs.FringeTransparent1
                .NumericUpDownStringFringeTransparent2.Value = fs.FringeTransparent2
                '縁取りグラデーション
                .CheckBoxStringFringeGradation.Checked = fs.IsFringeGradation
                .CheckBoxStringFringeGamma.Checked = fs.IsFringeGammaC
                .NumericUpDownStringFringeGradationAngleグラデ角度.Value = fs.FringeGradationAngle
                '縁取り幅
                .NumericUpDownStringFringeWidth.Value = fs.FringeWidth
            End If


            'その他設定
            .PictureBoxTextSample.Image = fs.Image

        End With

        Form1.IsFontSettingNow = False '見本の再描画フラグ解除

    End Sub
    Private Sub SettingSave設定ファイル保存()
        'Dim filename As String = "F:\png\fontsetting.bin"
        '        Dispose、Closeが確実に呼び出されるようにする: .NET Tips: C#, VB.NET
        'http://dobon.net/vb/dotnet/beginner/calldispose.html

        'ファイルに保存、シリアライズ
        Using fst As New FileStream(filename, FileMode.Create) 'Usingは確実に終了処理をしてくれる、FileStreamをCloseしてくれる
            Dim bin As New BinaryFormatter
            bin.Serialize(fst, FontSLフォントの設定リスト)
        End Using

    End Sub
    Private Sub AddFontSettingフォント設定新規登録(i As Integer)
        Dim fs As New FontSetting

        With myForm3
            'fs.ForeColor1 = .ButtonFontColor.ForeColor
            'fs.ForeColor2 = .ButtonFontColor2.ForeColor
            '色
            fs.FontColor1 = .PictureBoxTextColor1.BackColor
            fs.FontColor2 = .PictureBoxTextColor2.BackColor
            fs.FontTransparent1 = .NumericUpDownStringColorT1_不透明度.Value
            fs.FontTransparent2 = .NumericUpDownStringColorT2_不透明度.Value

            'グラデーション
            fs.IsGradation = .CheckBoxTextGradation.Checked
            fs.FontGradationAngel = .NumericUpDownStringGradientAngle.Value
            fs.IsGammaC = .CheckBoxStringGamma.Checked

            '基本
            fs.FontName = .ComboBoxAllFonts.Text
            fs.Yokogaki = .RadioButtonStringH.Checked
            fs.Size = .NumericUpDownFontSize.Value
            fs.Italic = .CheckBoxTextItalic.Checked
            fs.Bold = .CheckBoxTextBold.Checked
            fs.LineSpace = .NumericUpDownLineSpace行間.Value
            fs.WordSpace = .NumericUpDownWordSpace文字間.Value

            'その他
            fs.AntiAlias = .CheckBoxStringAntiAlias.Checked
            fs.Angle = .NumericUpDownStringAngle.Value
            fs.IsAngle = .CheckBoxStringAngle.Checked
            fs.IsAdjust = .CheckBoxStringAdjust位置調整.Checked


            '背景
            fs.IsNotBackColor = .CheckBoxTextBackColor背景色の有無.Checked
            fs.BGColor1 = .PictureBoxStringBGColor1.BackColor
            fs.BGColor2 = .PictureBoxStringBGColor2.BackColor
            fs.BGTransparent1 = .NumericUpDownTextTransparent1.Value
            fs.BGTransparent2 = .NumericUpDownTextTransparent2.Value
            'グラデーション
            fs.IsBGGradation = .CheckBoxTextBackC_Gradation.Checked
            fs.IsBGGammaC = .CheckBoxTextBackC_Gamma.Checked
            fs.BGGradationAngel = .NumericUpDownStringBackGradAngle文字背景グラデ角度.Value
            '角の丸さ
            'fs.BGLinkedRound = .CheckBoxLinkedStringRoundRect背景.Checked
            fs.BGRound = .TrackBarStringBackRoundrect.Value


            '枠
            fs.IsFrame = .CheckBoxStringBGFrame.Checked
            '枠色
            fs.wakuColor1 = .PictureBoxStringBGFrameColor1.BackColor
            fs.wakuColor2 = .PictureBoxStringBGFrameColor2.BackColor
            fs.wakuTransparent1 = .NumericUpDownStringBGFrameTransparent1.Value
            fs.wakuTransparent2 = .NumericUpDownStringBGFrameTransparent2.Value
            '枠グラデーション
            fs.IsWakuGradation = .CheckBoxStringBGFrameGradation.Checked
            fs.IsWakuGammaC = .CheckBoxStringBGFrameColorGamma.Checked
            fs.wakuGradationAngle = .NumericUpDownStringBGFrameGradationAngle.Value
            '枠角の丸さ
            fs.wakuRound = .TrackBarStringBGRoundRect文字枠の丸さ.Value
            '枠幅
            fs.wakuWidth = .NumericUpDownStringBGFrameWidth枠幅.Value


            '影
            fs.IsShadow = .CheckBoxStringShadow.Checked
            '影色
            fs.ShadowColor1 = .PictureBoxStringShadowColor1.BackColor
            fs.ShadowColor2 = .PictureBoxStringShadowColor2.BackColor
            fs.ShadowTranstarent1 = .NumericUpDownStringShadowColorTransparent1.Value
            fs.ShadowTranstarent2 = .NumericUpDownStringShadowColorTransparent2.Value
            '影グラデーション
            fs.IsShadowGradation = .CheckBoxStringShadowGradation.Checked
            fs.IsShadowGammaC = .CheckBoxStringShadowGamma.Checked
            fs.ShadowGradationAngle = .NumericUpDownStringShadowAngle.Value
            '影位置
            fs.ShadowH = .NumericUpDownStringShadowH横.Value
            fs.ShadowV = .NumericUpDownStringShadowV縦.Value


            '縁取り
            fs.IsFringe = .CheckBoxStringFringe.Checked
            fs.FringeColor = .PictureBoxStringFringeColor1.BackColor
            fs.FringeColor2 = .PictureBoxStringFringeColor2.BackColor
            fs.FringeTransparent1 = .NumericUpDownStringFringeTransparent1.Value
            fs.FringeTransparent2 = .NumericUpDownStringFringeTransparent2.Value
            '縁取りグラデーション
            fs.IsFringeGradation = .CheckBoxStringFringeGradation.Checked
            fs.IsFringeGammaC = .CheckBoxStringFringeGamma.Checked
            fs.FringeGradationAngle = .NumericUpDownStringFringeGradationAngleグラデ角度.Value
            '縁取り幅
            fs.FringeWidth = .NumericUpDownStringFringeWidth.Value

            'その他設定
            fs.Image = .PictureBoxTextSample.Image
            fs.ItemName = .NumericUpDownFontSize.Value & "_" & .ComboBoxAllFonts.Text

        End With
        'リストに追加
        'FontSLフォントの設定リスト.Add(fs)
        FontSLフォントの設定リスト.Insert(i, fs)

    End Sub
    'Private Overloads Sub AddFontSettingフォント設定新規登録()
    '    Dim i As Integer = Me.ComboBoxFontSetting.Items.Count
    '    Call AddFontSettingフォント設定新規登録(i)

    'End Sub
    Private Sub ButtonAddFontSetting登録_Click(sender As Object, e As EventArgs) Handles ButtonAddFontSetting登録.Click
     
        ''リストに追加
        Dim i As Integer = Me.ComboBoxFontSetting.Items.Count
        Call AddFontSettingフォント設定新規登録(i)

        Call SettingSave設定ファイル保存()

        'DataGridViewに新規行を追加
        'Call AddDataGridViewに行を追加(i)
        Me.DataGridView1.Rows.Add()
        Call SetData行にデータを書込(i)

        Call Comboboxコンボボックスの項目更新(i) 'DataGridViewに行を追加してからこれ、順番大切
        'Me.ComboBoxFontSetting.SelectedIndex = i '追加した項目を選択する


    End Sub
    Private Sub Comboboxコンボボックスの項目更新(Optional i As Integer = -1)
        Dim nl() As String = FontSLフォントの設定リスト.GetNameList
        With Me.ComboBoxFontSetting
            .Items.Clear()
            .Items.AddRange(nl)
            If i <> -1 Then
                .SelectedIndex = i '追加した項目を選択する

            End If

        End With


    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim ooo = FontSLフォントの設定リスト

    End Sub

    Private Sub DeleteFontSetting項目削除(int As Integer)
        'Dim int As Integer = Me.ComboBoxFontSetting.SelectedIndex
        If int < 0 Then Exit Sub
        backupFontSetting = FontSLフォントの設定リスト(int) '削除したものを復活する用
        'backupInteger = Me.ComboBoxFontSetting.SelectedIndex

        FontSLフォントの設定リスト.RemoveAt(int) '削除
        Me.ComboBoxFontSetting.Items.RemoveAt(int)
        Me.ComboBoxFontSetting.SelectedIndex = -1 ' int - 1'削除した時はコンボボックスの表示は空白にする、こうしないと削除した項目の上の設定が強制的に反映されてしまうから
        If int = 0 Then '項目が0になったら空白表示、こうしないと0になっても最後に表示されていたのが残って不自然
            Me.ComboBoxFontSetting.Text = ""
        End If

        Call DeleteDataGridView行の削除(int)

    End Sub
    '項目削除
    Private Sub ButtonDelete_Click(sender As Object, e As EventArgs) Handles ButtonDelete.Click

        Dim i As Integer = Me.ComboBoxFontSetting.SelectedIndex
        Call DeleteFontSetting項目削除(i)

        Call SettingSave設定ファイル保存()

    End Sub


    Private Sub ButtonOverWriteFontSetting上書き_Click(sender As Object, e As EventArgs) Handles ButtonOverWriteFontSetting上書き.Click
        Dim i As Integer = Me.ComboBoxFontSetting.SelectedIndex
        If i < 0 Then Exit Sub

        Call AddFontSettingフォント設定新規登録(i) '選択項目の一個前に新規登録して
        FontSLフォントの設定リスト.RemoveAt(i + 1) '上書きされる項目を削除

        Call SettingSave設定ファイル保存()
        Call Comboboxコンボボックスの項目更新(i)
        'Me.ComboBoxFontSetting.SelectedIndex = i '追加した項目を選択する
        Call SetData行にデータを書込(i)

    End Sub

    'データグリッドビュー初期化
    Private Sub InitializeDataGridデータグリッドビュー初期化()
        Dim DGV As DataGridView = Me.DataGridView1
        With DGV
            .AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.AllCells
            .AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
            .AllowUserToAddRows = False
            '.AlternatingRowsDefaultCellStyle.BackColor = Color.AliceBlue '奇数行の背景色設定
            .DefaultCellStyle.SelectionBackColor = Color.FromArgb(255, 200, 200, 200) ' Color.Aquamarine 'セルの選択時の背景色設定
            .DefaultCellStyle.SelectionForeColor = Color.Black 'セルの選択時の文字の色
            .DefaultCellStyle.WrapMode = DataGridViewTriState.True 'セル内の改行許可
            .RowHeadersVisible = False '左端の列は表示しない
            .AllowUserToAddRows = False '行の追加は不可
            .CellBorderStyle = DataGridViewCellBorderStyle.Sunken 'セルの枠の書式
            .ColumnHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter

        End With

        Dim colImage As New DataGridViewImageColumn
        colImage.HeaderText = "見本"
        DGV.Columns.Add(colImage)

        DGV.Columns(0).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter 'ヘッダーテキストを中央表示
        'DGV.Columns(0).HeaderCell.Style.BackColor = Color.Red 'これは無視される
        'DGV.Columns(0).HeaderCell.Style.SelectionBackColor = Color.Red 'これは無視される
        'DGV.Columns(0).HeaderCell.Style.SelectionForeColor = Color.Blue 'これは無視される
        'DGV.ColumnHeadersDefaultCellStyle.BackColor = Color.Red 'これは無視される

        'DGV.TopLeftHeaderCell.Style.BackColor = Color.Red '左上のセルの背景色、無視される
        'DGV.TopLeftHeaderCell.Value = "/" '左上のセルの値、意味なし

        Dim headerText() As String = {"フォント名", "サイズ", "色1", "色2", "文字間"} 'カラムヘッダ名前
        Dim colFontName, colSize, colcolor1, colcolor2, lineSpace As New DataGridViewTextBoxColumn
        Dim colc() As DataGridViewTextBoxColumn = {colFontName, colSize, colcolor1, colcolor2, lineSpace}

        For i As Integer = 0 To UBound(headerText)
            colc(i).HeaderText = headerText(i)
            DGV.Columns.Add(colc(i))
            DGV.Columns(i + 1).Name = headerText(i)

            'colc(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter

        Next
        Dim ooo = DGV.Columns(2)

        DGV.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
        DGV.Columns("文字間").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter

        '設定ファイルがなければここまで
        If FontSLフォントの設定リスト.Count = 0 Then Exit Sub
        For i As Integer = 0 To FontSLフォントの設定リスト.Count - 1
            DGV.Rows.Add()
            Call SetData行にデータを書込(i)

        Next

        '1行目の2列目をアクティブセルに指定する
        If DGV.Rows.Count > 0 Then
            DGV.CurrentCell = DGV(1, 0)

        End If

    End Sub
  
    Private Sub DeleteDataGridView行の削除(i As Integer)
        Me.DataGridView1.Rows.RemoveAt(i)
    End Sub
    Private Sub SetData行にデータを書込(i As Integer)
        '設定を読み込んでDataGridViewにレコードを追加
        Dim fs As New FontSetting
        Dim img As Image
        Dim DGV As DataGridView = Me.DataGridView1
        fs = FontSLフォントの設定リスト.Item(i)

        If fs.Image Is Nothing Then '例外用、設定ファイルに見本画像がなければ新たに描画して設定ファイルに保存
            Call SetDataToForm3設定をForm3に書き込み(fs)
            'img = Form1.StringDraw5("見本")
            Dim myFont As Font = myForm3.CreateFontフォント作成()
            img = myForm3.GetDrawStringAll文字と背景画像作成("見本", myFont)
            fs.Image = img
            Call SettingSave設定ファイル保存()
        Else
            img = fs.Image

        End If

        '互換性
        With fs
            If .FontColor1.A = 0 Then 'Color.FromArgb(0, 0, 0, 0) Then '互換性維持のため、あとで消す
                .FontColor1 = .ForeColor1
                .FontColor2 = .ForeColor2
                .ForeColor1 = Nothing
                .ForeColor2 = Nothing
                Call SettingSave設定ファイル保存()
            End If

            If .FontTransparent1 = Nothing AndAlso .FontTransparent2 = Nothing Then
                .FontTransparent1 = 255
                .FontTransparent2 = 255
                Call SettingSave設定ファイル保存()
            End If
            If .BGTransparent1 = Nothing AndAlso .BGTransparent2 = Nothing Then
                .BGTransparent1 = 128
                .BGTransparent2 = 128
                Call SettingSave設定ファイル保存()
            End If
            '枠
            If .wakuColor1.A = 0 AndAlso .wakuColor2.A = 0 AndAlso .wakuTransparent1 = 0 AndAlso .wakuTransparent2 = 0 Then
                .wakuTransparent1 = 255
                .wakuTransparent2 = 255
                .wakuWidth = 1
                Call SettingSave設定ファイル保存()
            End If
            'If fs.wakuWidth = 0 Then
            '    fs.wakuWidth = 1
            '    Call SettingSave設定ファイル保存()
            'End If
            '影
            If .ShadowColor1.A = 0 AndAlso .ShadowColor2.A = 0 AndAlso .ShadowTranstarent1 = 0 AndAlso .ShadowTranstarent2 = 0 Then
                .ShadowTranstarent1 = 128
                .ShadowTranstarent2 = 128
                .ShadowH = 10
                .ShadowV = 10
                Call SettingSave設定ファイル保存()
            End If
            '縁取り
            If .FringeTransparent1 = 0 AndAlso .FringeTransparent2 = 0 AndAlso .FringeTransparent1 = 0 AndAlso .FringeTransparent2 = 0 Then
                .FringeTransparent1 = 255
                .FringeTransparent2 = 255
                Call SettingSave設定ファイル保存()
            End If
        End With

        '互換性ここまで
        With fs
            Dim r1 As Integer = .FontColor1.R
            Dim g1 As Integer = .FontColor1.G
            Dim b1 As Integer = .FontColor1.B
            Dim r2 As Integer = .FontColor2.R
            Dim g2 As Integer = .FontColor2.G
            Dim b2 As Integer = .FontColor2.B
            '色見本のセルのの背景色によって文字色を変える
            Dim rk, gk, bk, gs1, gs2 As Single
            rk = 0.3
            gk = 0.6F
            bk = 0.1F

            gs1 = (r1 / 255 * rk) + (g1 / 255 * gk) + (b1 / 255 * bk)
            gs2 = (r2 / 255 * rk) + (g2 / 255 * gk) + (b2 / 255 * bk)


            Dim argb1 As String = "a " & .FontTransparent1 & vbNewLine & "R " & r1.ToString & vbNewLine &
                "G " & g1.ToString & vbNewLine & "B " & b1
            Dim argb2 As String = ""
            If .IsGradation Then
                argb2 = "透 " & .FontTransparent1 & vbNewLine & "赤 " & r2 & vbNewLine & "緑 " & g2 & vbNewLine & "青 " & b2
            End If
            Dim lineSpace As String = "行間 " & .LineSpace & vbNewLine & vbNewLine & "字間 " & .WordSpace
            'DGV.Rows.Add(img, .FontName, .Size, argb1, argb2)
            DGV.Rows(i).SetValues(img, .FontName, .Size, argb1, argb2, lineSpace) 'SetValuesで一度に入力できる
            If gs1 <= 0.3 Then
                DGV(3, i).Style.ForeColor = Color.White
            Else
                DGV(3, i).Style.ForeColor = Color.Black
            End If
            DGV(3, i).Style.BackColor = Color.FromArgb(255, .FontColor1) 'セルの背景色の指定、半透明を指定すると表示がおかしくなるので不透明にしている
            If .IsGradation Then
                If gs2 <= 0.3 Then
                    DGV(4, i).Style.ForeColor = Color.White
                Else
                    DGV(4, i).Style.ForeColor = Color.Black
                End If

                DGV(4, i).Style.BackColor = Color.FromArgb(255, .FontColor2)
                'DGV(4, i).Style.ForeColor = Color.White
            End If
            'DGV(3, i).ToolTipText = argb1
            'DGV(4, i).ToolTipText = argb2
        End With

    End Sub
    Private Sub SampleImageRefresh見本画像更新() '(i As Integer)
        Dim fs As New FontSetting
        Dim img As Image
        Dim myFont As Font

        For i As Integer = 0 To FontSLフォントの設定リスト.Count - 1
            Call SetDataToForm3設定をForm3に書き込み(FontSLフォントの設定リスト(i))
            myFont = myForm3.CreateFontフォント作成()
            img = myForm3.GetDrawStringAll文字と背景画像作成("見本", myFont)

            Me.DataGridView1(0, i).Value = img
            FontSLフォントの設定リスト(i).Image = img
            Call SettingSave設定ファイル保存()

        Next
    End Sub
    Private Sub 見本をクリックした時(sender As DataGridView, e As DataGridViewCellEventArgs) Handles DataGridView1.CellClick
        'コンボボックスの項目を変更することによってForm3に設定を書き込む
        Dim r As Integer = e.RowIndex
        If r < 0 Then Exit Sub 'カラムヘッダをクリックの場合は何もしない

        Me.ComboBoxFontSetting.SelectedIndex = r

    End Sub


    '行の入れ替え、移動
    Private Sub ButtonGoUp_Click(sender As Object, e As EventArgs) Handles ButtonGoUp.Click
        Dim DGV As DataGridView = Me.DataGridView1
        Dim i As Integer = DGV.CurrentCellAddress.Y
        If i < 1 Then Exit Sub
        Call ReplaceRow行の入れ替え(i, i - 1)
    End Sub

    Private Sub ButtonGoDown_Click(sender As Object, e As EventArgs) Handles ButtonGoDown.Click
        Dim i As Integer = Me.DataGridView1.CurrentCellAddress.Y
        If i = Me.DataGridView1.Rows.Count - 1 Then Exit Sub
        Call ReplaceRow行の入れ替え(i, i + 1)
    End Sub
    Private Sub ReplaceRow行の入れ替え(CurrentIndex As Integer, ReplaceIndex As Integer)
        Dim DGV As DataGridView = Me.DataGridView1
        Dim i As Integer = DGV.Rows.Count
        If i < 2 Then Exit Sub '2行以下なら何もしない

        Dim cc As Integer = DGV.CurrentCellAddress.X 'アクティブセルの行
        FontSLフォントの設定リスト.Replace入れ替え(CurrentIndex, ReplaceIndex) '入れ替え
        Call SetData行にデータを書込(CurrentIndex) 'DataGridViewの表示更新
        Call SetData行にデータを書込(ReplaceIndex)
        DGV.CurrentCell = DGV(cc, ReplaceIndex) 'アクティブセルを変更
        If Me.ComboBoxFontSetting.SelectedIndex <> -1 Then
            Call Comboboxコンボボックスの項目更新(ReplaceIndex)
        Else
            Call Comboboxコンボボックスの項目更新() 'コンボボックスで何も指定していない時は中の更新だけ

        End If
        Call SettingSave設定ファイル保存() '設定ファイルに保存

    End Sub

    Private Sub ButtonSampleImageRefresh見本画像更新_Click(sender As Object, e As EventArgs) Handles ButtonSampleImageRefresh見本画像更新.Click
        Call SampleImageRefresh見本画像更新()

    End Sub

    Private Sub ButtonZaoriku復活_Click(sender As Object, e As EventArgs) Handles ButtonZaoriku復活.Click
        ''リストに追加
        If backupFontSetting.FontName = "" Then Exit Sub

        Dim i As Integer = Me.ComboBoxFontSetting.Items.Count
        FontSLフォントの設定リスト.Insert(i, backupFontSetting)

        Call SettingSave設定ファイル保存()

        Me.DataGridView1.Rows.Add()
        Call SetData行にデータを書込(i)

        Call Comboboxコンボボックスの項目更新(i) 'DataGridViewに行を追加してからこれ、順番大切

        backupFontSetting.FontName = ""

    End Sub

    '背景色選択
    Private Sub CheckBoxMihonListBGBlack_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxMihonListBGBlack.CheckedChanged
        Dim dg As DataGridView = Me.DataGridView1
        If Me.CheckBoxMihonListBGBlack.Checked Then
            dg.DefaultCellStyle.BackColor = Color.Black
            dg.DefaultCellStyle.ForeColor = Color.White

        Else
            dg.DefaultCellStyle.BackColor = Color.FromKnownColor(KnownColor.Control)
            dg.DefaultCellStyle.ForeColor = Color.Black

        End If

    End Sub
End Class