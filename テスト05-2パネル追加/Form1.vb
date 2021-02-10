Imports System.Drawing.Imaging
Imports System.Drawing.Drawing2D
Imports System.Runtime.InteropServices

'見えないListBoxにFocus()しているのは画像の移動のためとカーソルキーでの画像移動のため
'PictureBoxはクリックしてもFocus(Active)状態にはならないけどFocus()メソッドを使うとFocus状態になる
'この状態で右下に移動するとガーッと無限に右下に移動し始める、これを防ぐ目的
'カーソルキーでの移動はたとえばテキストボックスにカーソルがあると画像が動かない
'かといって画像にFocusを移すと右下移動になる、そのためにListBoxのイベントにカーソル移動をつけている

Public Class Form1
    <DllImport("user32.dll")> _
    Private Shared Function OpenClipboard(ByVal hWndNewOwner As IntPtr) As Boolean
    End Function
    <DllImport("user32.dll")> _
    Private Shared Function IsClipboardFormatAvailable(ByVal wFormat As Integer) As Integer
    End Function
    <DllImport("user32.dll")> _
    Private Shared Function GetClipboardData(ByVal wFormat As Integer) As IntPtr
    End Function
    <DllImport("user32.dll")> Private Shared Function CloseClipboard() As Integer
    End Function

    Dim newpiclocate As New Point(0, 0) 'ピクチャーボックスの座標用
    Dim IsDragging As Boolean   'ドラッグ中の場合True
    Dim FlagRightDragging As Boolean '右クリックドラッグ用フラグ
    Dim DiffPoint As Point      'ドラッグ開始地点とドラッグ開始時のボタンの位置とのずれ 
    Dim NowControl As ExPictureBox 'カーソルの下にある画像か移動が終わった直後の画像
    Friend ActExPic As ExPictureBox   '左クリックしたピクチャーボックスを入れておく
    Private PastActExPic As ExPictureBox '左クリックで変更した一個前のEditNowPicだったExPicture、図形編集の終了に使用
    Dim myPicAr As New ArrayList 'ピクチャーボックスをすべて入れておく
    Dim myPicArR As New ArrayList
    Dim tagCount As Integer = 0  '画像の重なりの順番に使う連番
    Dim tagPic As Integer
    Dim OldLocate As Point
    Dim FullSize As New Point 'スクロールバーの外側も含めた領域サイズ、保存する時の画像のサイズになる
    Dim ClipCount As Integer = 0 'クリップボードから追加の名前のカウント用
    Dim oldPicLocate As New Point(0, 0) '移動画像の移動前の座標

    'Dim myForm3 As New Form3 'Form3にアクセスするのに必要
    Dim myForm3 As Form3 = New Form3

    Dim myFormText As FormText = New FormText
    Friend myFormHistgram As FormHistogram = New FormHistogram


    Dim DummyPicBox As New ExPictureBox
    Dim RFPoint As New Point '一番右下の座標
    Dim ScrollPoint As Point


    '透過色
    Public FlagColor As Boolean = False '色取得フラグ
    Dim FlagGetColor As Boolean = False 'Form1用の透過色取得フラグ
    Public FlagForm3GetColor As Boolean = False 'Form3用の透過色取得フラグ
    Public FlagFrom3ShapeColor1 As Boolean = False 'Form3の図形作成の色1取得フラグ
    Public FlagFrom3ShapeColor2 As Boolean = False 'Form3の図形作成の色2取得フラグ
    Public FlagFrom3ShapeColor3 As Boolean = False 'Form3の図形作成の色3取得フラグ
    Public IsGetColor表示画像から色取得中 As Boolean '表示画像からマウスカーソルで色取得中フラグ
    Public GetColorPic色取得中のPictureBox As PictureBox 'マウスカーソルで色取得中のオブジェクト(ピクチャーボックス)
    Dim CurPoint As Point 'カーソルの座標
    Dim TransparentColor As Color '取得した透過色
    Dim ZaoraruPic As ExPictureBox
    Dim BackupPicBox As ExPictureBox
    Dim CloneBackup As ExPictureBox
    Dim FlagZaoraru As Boolean = False
    Dim myPicArClone As New ArrayList 'すべての画像のクローン、バックアップ用、色を元に戻す時に使う
    Dim myPicArCloneR As New ArrayList 'クローンの並べ替え用
    Dim myPicArBackup As New ArrayList '真のバックアップ
    'Friend HalfRealtimeRendering As Boolean = True '半自動再描画のフラグ

    '範囲選択のサイズ変更
    Dim Xp範囲選択画像 As Integer
    Dim Yp範囲選択画像 As Integer
    Dim selectPicbox範囲選択画像 As ExPictureBox
    Dim Width範囲選択画像 As Integer
    Dim Height範囲選択画像 As Integer
    Dim LabelPoint範囲選択画像 As New List(Of Label) ' ArrayList
    Dim MMRSXマウスの移動距離 As Integer
    Dim MMRSYマウスの移動距離 As Integer

    'Dim OldLabelLocation As New Point


    '保存するときSavefiledialogクラスのインスタンス？を作成
    Dim SaveFile As New SaveFileDialog()

    Dim TestFlag As Boolean

    Const CF_ENHMETAFILE As Integer = 14 'メタファイル
    Const CF_METAFILEPICT As Integer = 3

    '図形2用
    Private ActLabel As Label '線描画の頂点表示用ラベル、クリックしたラベル
    Private sLPoint As Point 'ラベルドラッグ用、クリック初期位置記憶
    Private XClickPP As Integer '頂点表示用のラベルのクリック初期位置
    Private YClickPP As Integer '頂点表示用のラベルのクリック初期位置
    Private XPPM頂点移動距離 As Integer '頂点移動距離(実際はマウスの移動距離)
    Private YPPM頂点移動距離 As Integer
    Const LABEL_SIZE As Integer = 21 'ラベルの大きさ、奇数のみ指定
    Const LABEL_SIZE_S As Integer = 5 'ラベルの大きさ【小】
    Friend isLabelSizeSmall As Boolean = False 'ラベルの大きさ【小】のフラグ
    Private PPLabelOffset As Size '頂点ラベルの表示位置の調整用、オフセット
    Public EditNowPic As New ExPictureBox '編集中のExPicture
    Private RClickPoint As Point '右クリックした座標
    Public PPL描画頂点リスト As New Generic.List(Of Label) '頂点表示用のラベルのリスト用
    Public isDrawEditNow As Boolean = False 'Draw画像の編集中フラグ
    Private LayerExPic As Integer '編集中のExPictureの元の階層を記憶
    Private MemoryFreeCount As Integer 'メモリの開放カウンタ
    Const MEMORY_FREE_LIMIT As Integer = 20 'この数値になったらメモリの開放
    Private isLayerChange '編集中にレイヤー変更した？
    Private BorderLineLabel枠表示用ラベル As New List(Of Label)
    Private TempRect変更中 As Rectangle 'サイズ変更中の画像の大きさ
    Private isLabelMoveNow As Boolean = False '頂点ラベル移動中フラグ

    Private isMouseDeDrawNow As Boolean = False 'マウスで描画中フラグ
    Private MouseDeDrawPic As ExPictureBox 'マウスで描画しているExPicture
    Private MouseDeDrawPicPast As ExPictureBox 'マウスで描画しているExPictureと同じかどうかの判定用

    'ExPictureサイズ変更
    Private LSサイズ変更ラベル As New Generic.List(Of Label) '編集ExPicture大きさ変更用ラベルのリスト用
    Private Click_初期位置 As Point 'ExPictureのサイズ変更用
    Private MLドラッグラベル As Label 'ドラッグ中のラベル
    Private XLClick初期位置 As Integer 'ExPictureのサイズ変更用
    Private YLClick初期位置 As Integer 'ExPictureのサイズ変更用
    Private XMMマウスの移動距離 As Integer
    Private YMMマウスの移動距離 As Integer
    Const MEMORY_FREE_LIMIT_サイズ変更用 As Integer = 10
    Private dummyExPicBox As ExPictureBox 'スクロールバー固定用ダミー画像
    Private dummyButton As Button ''スクロールバー固定用ダミー
    Const LABEL_PIC_SIZE As Integer = 10 'サイズ変更用の頂点ラベルのサイズ
    Private xSizeLabelLocate As Integer '図形画像のサイズ変更用ラベルの座標、未使用
    Private ySizeLabelLocate As Integer '未使用

    Friend IsFontSettingNow As Boolean = False '文字の設定を反映中フラグ


    Private Sub Form1_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragDrop
        Dim myFileName() As String
        Dim EnterFileCount As Integer 'ドロップされたファイル数
        Dim EnterPicCount As Integer = 0 'ドロップされたファイルのうち画像だった数
        Dim NotPicFile As Integer = 0 'ドロップされたファイルが画像ではなかった数

        myFileName = e.Data.GetData(DataFormats.FileDrop)
        EnterFileCount = myFileName.Length 'ドロップされたファイル数
        Call CloseEdit編集終了() '編集中のものがあれば編集終了する
        Call MouseEndDrawマウスで描画終了処理()

        'ドロップされたファイルが画像ではなかった数を返す
        For Each c In myFileName
            Try
                Dim myBmp As New Bitmap(c)
                EnterPicCount = EnterPicCount + 1 'ここまでこれたなら画像ファイルなのでカウントする
                myBmp.Dispose()
            Catch ex As Exception
                NotPicFile = NotPicFile + 1
                MsgBox(c & "は画像ファイルとして開けませんでした")
            End Try
        Next
        'ドロップされた中で画像がひとつもなければ終了
        If myFileName.Count - NotPicFile = 0 Then
            Exit Sub
        End If

        'ラジオボタンのチェック、読み込む順番をファイル名で並べ替え
        If Me.RadioButton1.Checked = True Then '昇順なら
            Array.Sort(myFileName)
        ElseIf Me.RadioButton2.Checked = True Then '降順なら
            Array.Sort(myFileName)
            Array.Reverse(myFileName)
        End If

        'ラジオボタンをチェックして
        'タグの初期値を決定する
        If Me.RadioButtonLower.Checked Then '下層に追加する場合
            tagCount = myPicAr.Count + 1 'タグの初期値にピクチャーボックスの総数を入れる
        ElseIf Me.RadioButtonUpper.Checked Then '上層に追加する場合
            '既存の画像のタグに追加さる画像の枚数を足す
            For Each c As ExPictureBox In myPicAr
                c.Tag = c.Tag + (EnterFileCount - NotPicFile)
            Next
            'クローン用
            For Each c As ExPictureBox In myPicArClone
                c.Tag = c.Tag + (EnterFileCount - NotPicFile)
            Next
            'バックアップ用
            For Each c As ExPictureBox In myPicArBackup
                c.Tag = c.Tag + (EnterFileCount - NotPicFile)
            Next

            tagCount = EnterFileCount - NotPicFile
        End If

        Me.ListBox1.Items.AddRange(myFileName)


        '画像を追加する初期座標の決定
        newpiclocate = AddPicPoint()
        Dim StartPointChecke As Boolean = True



        For Each c In myFileName
            Try '画像として開けなかったファイルは無視
                EnterPicCount = EnterPicCount + 1 'ここまでこれたなら画像ファイルなのでカウントする

                'Dim myPic As Image = myBmp
                '開いている画像ファイルの移動や名前の変更ができるようにファイルストリームで開くようにした
                'これで開いているファイルに上書き保存してもエラーが出なくなったはず
                'System.IO.FileModeのSystem.IOを省くと無効になるみたい()
                Dim myFileS As New System.IO.FileStream(c, System.IO.FileMode.Open, System.IO.FileAccess.Read)
                Dim myPic As Image = Image.FromStream(myFileS)
                'Dim ico As New Icon(c, 256, 256)'256では取得できないけど一番大きい物を取得できる48
                'Dim ico = New Icon(myFileS, 256, 256) 'ファイルストリームからは読み込めない？
                'myFileS.Close()'このタイミングで閉じると↓のtempbmpでDGI+エラーになる

                '画像ファイルの解像度(dpi)を一律にする（bitmapのDefaultの96にする）
                'たった2行だけどこれに気づくのに二日かかった
                'この処理をしないと96dpi以下の画像だと右下が切れて、96dpiの画像ファイルだと縮小表示になる
                Dim tempBmp As New Bitmap(myPic)
                myPic = tempBmp
                myFileS.Close() 'このタイミング
                myFileS.Dispose()


                'ピクチャーボックスの作成、設定
                Dim myPicBox As New ExPictureBox
                myPicBox.SizeMode = PictureBoxSizeMode.AutoSize '必須
                'myPicBox.BackColor = Color.Transparent'背景色を透明にする
                'myPicBox.BackColor = Color.FromArgb(100, 255, 255, 255) '背景色を半透明にする

                'ラジオボタンのチェック初期は位置座標の決定
                If Me.RadioButtonStack.Checked AndAlso StartPointChecke Then 'ドロップ画像の1枚目なら
                    StartPointChecke = False 'フラグ
                    Dim WidthDiff As Integer = myPic.Width - Me.NumericXslide.Value
                    Dim HeightDiff As Integer = myPic.Height - Me.NumericYslide.Value


                    If Me.CheckBoxDropPoint.Checked Then
                        newpiclocate = newpiclocate
                    ElseIf Me.CheckBoxSelectPicPoint.Checked Then '選択画像を基準の場合
                        If myPicAr.Count = 0 Then
                            newpiclocate = New Point(0, 0)
                        Else
                            Dim xSlide As Integer = Me.NumericXslide.Value
                            Dim ySlide As Integer = Me.NumericYslide.Value
                            newpiclocate = New Point(Me.ActExPic.Location.X + xSlide, Me.ActExPic.Location.Y + ySlide)
                        End If

                    ElseIf Me.RadioButtonRight.Checked Then
                        newpiclocate = New Point(newpiclocate.X - WidthDiff, newpiclocate.Y)
                    ElseIf Me.RadioButtonDown.Checked Then
                        newpiclocate = New Point(newpiclocate.X, newpiclocate.Y - HeightDiff)
                    ElseIf Me.RadioButtonRightDown.Checked Then
                        newpiclocate = New Point(newpiclocate.X - WidthDiff, newpiclocate.Y - HeightDiff)
                    End If
                End If


                '画像座標決定！
                myPicBox.Location = New Point(newpiclocate)

                '次の画像の座標
                If Me.RadioButtonUnstack.Checked = True OrElse Me.RadioButtonStack.Checked Then '元の画像に重ねない場合
                    newpiclocate.X = newpiclocate.X + Me.NumericXslide.Value '連続読み込みでの次の画像のスライド座標
                    newpiclocate.Y = newpiclocate.Y + Me.NumericYslide.Value
                ElseIf Me.RadioButtonAbsoluteUnstack.Checked = True Then 'すべての画像を重ねない
                    If Me.RadioButtonDown.Checked Then
                        newpiclocate.Y = newpiclocate.Y + myPic.Height
                    ElseIf Me.RadioButtonRight.Checked Then
                        newpiclocate.X = newpiclocate.X + myPic.Width
                    ElseIf Me.RadioButtonRightDown.Checked Then
                        newpiclocate.X = newpiclocate.X + myPic.Width
                        newpiclocate.Y = newpiclocate.Y + myPic.Height
                    End If
                End If


                myPicBox.Image = myPic '画像をピクチャーボックスに割り当て


                'タグに数値を追加
                myPicBox.Tag = tagCount
                If Me.RadioButtonLower.Checked Then
                    tagCount = tagCount + 1
                ElseIf Me.RadioButtonUpper.Checked Then
                    tagCount = tagCount - 1
                End If

                '右クリックメニュー追加、2014/12/13
                myPicBox.ContextMenuStrip = Me.ContextMenuStrip1


                'myPicBox.Name = "myPic" & tagCount
                'ファイル名を取得してピクチャーボックスの名前にする
                myPicBox.Name = System.IO.Path.GetFileName(c)

                '保存ダイアログの初期ディレクトリを追加した画像のディレクトリにする
                SaveFile.InitialDirectory = System.IO.Path.GetDirectoryName(c)
                '最初に追加されたファイルネームに＿を付け足したものを保存ファイルネームにする
                If myPicAr.Count = 0 Then ' And SaveFile.FileName = Nothing Then
                    SaveFile.FileName = System.IO.Path.GetFileNameWithoutExtension(c) & "_"
                End If

                '完成したピクチャーボックスをコレクションに追加

                If Me.RadioButtonLower.Checked Then
                    myPicAr.Add(myPicBox)
                ElseIf Me.RadioButtonUpper.Checked Then
                    myPicAr.Insert(0, myPicBox)

                End If



                'クローンのピクチャーボックスを作成
                Dim myPicBox2 As New ExPictureBox
                With myPicBox2
                    .Location = myPicBox.Location
                    .SizeMode = myPicBox.SizeMode
                    .Name = "クローン" & myPicBox.Name
                    .Image = myPicBox.Image
                    .Tag = myPicBox.Tag
                End With

                'クローン用のコレクションに追加
                If Me.RadioButtonLower.Checked Then
                    myPicArClone.Add(myPicBox2)
                ElseIf Me.RadioButtonUpper.Checked Then
                    myPicArClone.Insert(0, myPicBox2)
                End If


                'バックアップ用のピクチャーボックス作成
                Dim myPicArBackupR As New ExPictureBox
                With myPicArBackupR
                    .Location = myPicBox.Location
                    .SizeMode = PictureBoxSizeMode.Normal
                    .Name = myPicBox.Name
                    .Image = myPicBox.Image
                    .Tag = myPicBox.Tag

                End With
                'バックアップ用のコレクションに追加
                If Me.RadioButtonLower.Checked Then
                    myPicArBackup.Add(myPicArBackupR)
                ElseIf Me.RadioButtonUpper.Checked Then
                    myPicArBackup.Insert(0, myPicArBackupR)
                End If


                'Me.Panel1.Controls.Add(myPicBox) 'ピクチャーボックスを表示
                Me.Panel2.Controls.Add(myPicBox) 'ピクチャーボックスを表示
                'イベントに関連付ける
                AddHandler myPicBox.MouseEnter, AddressOf PictureBox1_MouseEnter
                AddHandler myPicBox.MouseUp, AddressOf PictureBox1_MouseUp
                AddHandler myPicBox.MouseLeave, AddressOf PictureBox1_MouseLeave
                AddHandler myPicBox.MouseMove, AddressOf PictureBox1_MouseMove '大幅変更箇所
                AddHandler myPicBox.MouseDown, AddressOf PictureBox1_MouseDown '大幅変更箇所
                'AddHandler myPicBox.MouseWheel, AddressOf CurrentPic_MouseWheel

                'AddHandler myPicBox.MouseUp, AddressOf PictureBox1_PreviewKeyDown

                'これだと右下が1ドットづつ削れてしまうので別の方法にした
                'FullSize = New Size(Me.Panel2.HorizontalScroll.Maximum, Me.Panel2.VerticalScroll.Maximum)

                'myPicBox.Invalidate()
                'myBmp.Dispose()

            Catch ex As Exception
                'Debug.Print(ex.Source.ToString())
                Debug.Print(c.ToString()) 'エラーに成ったファイルのフルパスをdebug用表示
            End Try

        Next

        Me.NumNowPic.Maximum = myPicAr.Count '最大値設定

        Call SortPic()


        Me.TextBox1.Text = myPicAr.Count
        FullSize = RigthDownPoint()
        Me.TextBox9.Text = FullSize.ToString

        For Each c As ExPictureBox In myPicAr '最後に追加された画像をnowcontrolとFocusPicにする
            If Me.RadioButtonLower.Checked AndAlso c.Tag = myPicAr.Count Then
                NowControl = c
                ActExPic = c
            ElseIf Me.RadioButtonUpper.Checked AndAlso c.Tag = 1 Then
                NowControl = c
                ActExPic = c
            End If
        Next
        'ActExPic.Focus()
        'Me.ListBox1.Focus()
        'Me.CurrentPic.Image = FocusPic.Image '加工された後の画像
        Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
        Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
        'Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(FocusPic.Tag - 1), ExPictureBox).Image
        'Me.CurrentPic.Image = DirectCast(myPicArBackup(FocusPic.Tag - 1), ExPictureBox).Image '加工される前の画像

        myPicArCloneR = myPicArClone.Clone



        Me.NumNowPic.Value = ActExPic.Tag

        Call TopLeftMove()
        Call ChangeFocusTステータス表示更新()
        Call UpdateThumbnail()
        Call MoveAfter()

        Call Transparent1_2()


        'Call Transparent2()






    End Sub

    Private Sub Form1_DragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles MyBase.DragEnter

        If e.Data.GetDataPresent(DataFormats.FileDrop) Then
            e.Effect = DragDropEffects.Copy
        Else
            e.Effect = DragDropEffects.None
        End If

    End Sub
    Private Sub PictureBox1_MouseEnter(ByVal sender As Object, ByVal e As System.EventArgs)
        NowControl = DirectCast(sender, ExPictureBox)
        'AddHandler NowControl.MouseDown, AddressOf PictureBox1_MouseDown'大幅変更箇所
        Me.ListBox1.Focus()

    End Sub



    Private Sub PictureBox1_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        'Me.Label12.Text = CInt(My.Application.Info.WorkingSet / 1024 / 1024).ToString'メモリ使用量表示

        System.GC.Collect() 'メモリの開放
        Dim tempActEx As ExPictureBox = ActExPic 'クリックする前のExPictureを入れておく
        ActExPic = DirectCast(sender, ExPictureBox)

        'マウスで描画中の時
        If isMouseDeDrawNow And e.Button = Windows.Forms.MouseButtons.Left Then
            Call ExpGridMoveグリッド合わせ(MouseDeDrawPic)

            MouseDeDrawPicPast = MouseDeDrawPic 'マウスで描画しているExPictureと同じかどうかの判定用
            'クリックしたExPictureが同じなら頂点追加実行
            If MouseDeDrawPic.Equals(ActExPic) Then
                Dim pCount As Integer = MouseDeDrawPic.PathPoints.Count

                If MouseDeDrawPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then

                    If pCount = 0 Then
                        For i As Integer = 0 To 3
                            MouseDeDrawPic.PathPoints.Add(e.Location)
                        Next
                    ElseIf pCount >= 4 Then
                        Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic)
                        MouseDeDrawPic.Image = bmp

                        For i As Integer = 0 To 2
                            MouseDeDrawPic.PathPoints.Add(e.Location)

                        Next

                    End If
                    '背景だけ作成して描画
                    Dim bgi As Bitmap = BGImage背景画像作成ExPictureBox用(MouseDeDrawPic, , True)
                    MouseDeDrawPic.BackgroundImage = bgi

                Else
                    ActExPic.PathPoints.Add(e.Location)
                    pCount = MouseDeDrawPic.PathPoints.Count

                    If pCount >= 2 AndAlso
                        MouseDeDrawPic.isFill = False AndAlso
                        MouseDeDrawPic.CloseLine = False Then
                        '頂点が2個以上たまったら描画して背景に指定する
                        '閉じていないand塗りつぶしではない
                        Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(ActExPic)
                        'ActExPic.BackgroundImage = bmp
                        MouseDeDrawPic.Image = bmp

                        Dim bgi As Bitmap = BGImage背景画像作成ExPictureBox用(MouseDeDrawPic, False, True)
                        MouseDeDrawPic.BackgroundImage = bgi

                    Else
                        '背景だけ作成して描画
                        Dim bgi As Bitmap = BGImage背景画像作成ExPictureBox用(MouseDeDrawPic, , True)
                        MouseDeDrawPic.BackgroundImage = bgi
                    End If

                End If
                'Select Case MouseDeDrawPic.GraphicDrawType
                '    Case ExPictureBox.DrawType.ベジェ曲線
                '        If pCount = 0 Then
                '            For i As Integer = 0 To 3
                '                MouseDeDrawPic.PathPoints.Add(e.Location)
                '            Next
                '        ElseIf pCount >= 4 Then
                '            Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic)
                '            MouseDeDrawPic.Image = bmp

                '            For i As Integer = 0 To 2 '頂点を3つづつ増やす
                '                MouseDeDrawPic.PathPoints.Add(e.Location)

                '            Next

                '        End If
                '        '背景だけ作成して描画
                '        Dim bgi As Bitmap = BGImage背景画像作成ExPictureBox用(MouseDeDrawPic, , True)
                '        MouseDeDrawPic.BackgroundImage = bgi
                '    Case ExPictureBox.DrawType.曲線, ExPictureBox.DrawType.直線
                '        ActExPic.PathPoints.Add(e.Location)
                '        pCount = MouseDeDrawPic.PathPoints.Count

                '        If pCount >= 2 AndAlso
                '            MouseDeDrawPic.isFill = False AndAlso
                '            MouseDeDrawPic.CloseLine = False Then
                '            '頂点が2個以上たまったら描画して背景に指定する
                '            '閉じていないand塗りつぶしではない
                '            Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(ActExPic)
                '            'ActExPic.BackgroundImage = bmp
                '            MouseDeDrawPic.Image = bmp

                '            Dim bgi As Bitmap = BGImage背景画像作成ExPictureBox用(MouseDeDrawPic, False, True)
                '            MouseDeDrawPic.BackgroundImage = bgi

                '        Else
                '            '背景だけ作成して描画
                '            Dim bgi As Bitmap = BGImage背景画像作成ExPictureBox用(MouseDeDrawPic, , True)
                '            MouseDeDrawPic.BackgroundImage = bgi
                '        End If
                '    Case ExPictureBox.DrawType.四角枠
                '        'ActExPic.PathPoints.Add(e.Location)
                '        'pCount = MouseDeDrawPic.PathPoints.Count
                '        'If pCount >= 2 Then '対角線ができたら終了

                '        '    'ActExPic.PathPoints.Clear()
                '        '    Call MouseEndDrawマウスで描画終了処理()
                '        'End If

                '    Case Else

                'End Select
            ElseIf MouseDeDrawPic.Equals(ActExPic) = False Then
                '違うExPictureをクリックしていたらマウスで描画を終了する
                Call MouseEndDrawマウスで描画終了処理()
            End If

        End If



        If e.Button = Windows.Forms.MouseButtons.Left Then
            Select Case True
                Case isDrawEditNow
                    PastActExPic = EditNowPic
                    If PastActExPic.Equals(ActExPic) = False Then
                        Dim tempEx As ExPictureBox = ActExPic
                        '編集画像と違う画像を左クリックの時、編集中の画像を終了して新しくクリックした画像を編集開始にする
                        Call CloseEdit編集終了() '終了処理をするとActExPictureが編集終了した画像になるので不都合どうする？→tempExを用意していれた
                        ActExPic = tempEx
                        'If ActExPic.IsEdit Then 'クリックで編集状態にするにはこの中を有効にする
                        '    Call Form3.StartEdit編集開始()

                        'End If
                    End If
                    'Case Else 'クリックで編集状態にするにはこの中を有効にする
                    '    If ActExPic.IsEdit And isMouseDeDrawNow = False Then
                    '        'Call CloseEdit編集終了()
                    '        Call Form3.StartEdit編集開始()
                    '    End If
              
            End Select
          

        ElseIf e.Button = Windows.Forms.MouseButtons.Right Then
            RClickPoint = e.Location

            '右クリックメニューの変更
            '範囲選択画像だった場合
            If ActExPic.Equals(selectPicbox範囲選択画像) Then
                ToolStripMenuItem選択範囲を保存.Visible = True
                ToolStripMenuItem選択範囲をコピペ.Visible = True
                ToolStripMenuItemSave.Visible = False
                ToolStripMenuItemSaveT.Visible = False
            Else
                ToolStripMenuItem選択範囲を保存.Visible = False
                ToolStripMenuItem選択範囲をコピペ.Visible = False
                ToolStripMenuItemSave.Visible = True
                ToolStripMenuItemSaveT.Visible = True
            End If

            Select Case True
                Case isDrawEditNow '図形編集中
                    If ActExPic.IsEdit Then
                        ToolStripMenuItem頂点の編集開始.Visible = True
                    Else
                        ToolStripMenuItem頂点の編集開始.Visible = False
                    End If
                    PastActExPic = EditNowPic
                    If PastActExPic.Equals(ActExPic) = False Then
                        Dim tempEx As ExPictureBox = ActExPic '一時記憶
                        '編集画像と違う画像を右クリックしていたら編集終了
                        Call CloseEdit編集終了()
                        ActExPic = tempEx '終了処理をすると終了処理をした画像がActExになってしまい不都合なのでこの処理
                    Else
                        '編集画像と同じ画像を右クリックしていた時
                        ToolStripMenuItem頂点の追加.Enabled = True
                        ToolStripMenuItem頂点の削除.Enabled = False
                        ToolStripMenuItem頂点の編集終了.Visible = True

                    End If

                Case Else
                    If ActExPic.IsEdit Then
                        ToolStripMenuItem頂点の編集開始.Visible = True
                    Else
                        ToolStripMenuItem頂点の編集開始.Visible = False
                    End If
            End Select
           

        End If



        Me.Panel2.Refresh() '右クリックで範囲選択した表示を消す


        '透過色の取得
        If FlagColor = True And e.Button = Windows.Forms.MouseButtons.Left Then
            FlagColor = True
            'Me.Cursor = Cursors.Default
            'CurPoint = NowControl.PointToClient(Cursor.Position)
            CurPoint = ActExPic.PointToClient(Cursor.Position)
            'Dim myTempBmp As Bitmap = New Bitmap(NowControl.Image)
            Dim myTempBmp As Bitmap = New Bitmap(ActExPic.Image)
            TransparentColor = myTempBmp.GetPixel(CurPoint.X, CurPoint.Y)
            If FlagGetColor Then 'メインフォーム用
                Me.TransparentPictureBox.BackColor = TransparentColor
                Me.LabelTranspAlpha.Text = "透=" & TransparentColor.A
                Me.LabelTranspCol.Text = _
                    "赤=" & TransparentColor.R & _
                    " 緑=" & TransparentColor.G & _
                    " 青=" & TransparentColor.B
                FlagGetColor = True
            ElseIf FlagForm3GetColor Then 'サブフォーム用
                myForm3.PictureBoxTranspCol.BackColor = TransparentColor
                myForm3.LabelTranspAlpha.Text = "透=" & TransparentColor.A
                myForm3.LabelTranspCol.Text = _
                    "赤=" & TransparentColor.R & _
                    " 緑=" & TransparentColor.G & _
                    " 青=" & TransparentColor.B

                FlagForm3GetColor = True

            ElseIf FlagFrom3ShapeColor1 Then '図形作成の色1の取得用
                With myForm3
                    .PictureBoxShapeColor1.BackColor = TransparentColor
                    .LabelShapeColor1.Text = TransparentColor.R.ToString("D3") & " " & TransparentColor.G.ToString("D3") & " " & TransparentColor.B.ToString("d3")
                    .ButtonSquareColor1.ForeColor = TransparentColor
                End With

                FlagFrom3ShapeColor1 = True
                Call SquareSample()

            ElseIf FlagFrom3ShapeColor2 Then '図形作成の色2の取得用
                With myForm3
                    .PictureBoxShapeColor2.BackColor = TransparentColor
                    .LabelShapeColor2.Text = TransparentColor.R.ToString("D3") & " " & TransparentColor.G.ToString("D3") & " " & TransparentColor.B.ToString("d3")
                    .ButtonSquareColor2.ForeColor = TransparentColor
                End With
                FlagFrom3ShapeColor2 = True
                Call SquareSample()
            ElseIf FlagFrom3ShapeColor3 Then '図形作成の色2の取得用
                With myForm3
                    .PictureBoxShapeColor3.BackColor = TransparentColor
                    .LabelShapeColor3.Text = TransparentColor.R.ToString("D3") & " " & TransparentColor.G.ToString("D3") & " " & TransparentColor.B.ToString("d3")
                    .ButtonSquareColor3.ForeColor = TransparentColor
                End With
                FlagFrom3ShapeColor3 = True
                Call SquareSample()
            End If
            Exit Sub
        End If

        If IsGetColor表示画像から色取得中 And e.Button = Windows.Forms.MouseButtons.Left Then
            Dim col As Color
            Dim canvas As New Bitmap(ActExPic.Image)
            col = canvas.GetPixel(e.X, e.Y)
            GetColorPic色取得中のPictureBox.BackColor = col
            Call TextSample()

            Exit Sub

        End If

            'ここまで色取得用-----------------------------


            'ここから移動用---------------------------------
            'ドラッグ中に左クリック以外が押されたらドラッグをキャンセルするためMouseMove関連付けを剥がす(変なエラーの回避)
            If e.Button <> Windows.Forms.MouseButtons.Left Then
                IsDragging = False 'ドラッグしていない
                'RemoveHandler NowControl.MouseMove, AddressOf PictureBox1_MouseMove'大幅変更箇所
                Return
            End If

        '枠の表示非表示
        Select Case True
            Case ActExPic.IsEdit AndAlso isDrawEditNow = False And isMouseDeDrawNow = False
                If tempActEx.Equals(ActExPic) Then

                Else
                    Call PicBoderLineLabel画像に付けた枠を消去()
                    Call PicBoderLineLabel画像に枠を作成表示(ActExPic)
                End If
                Call PicBoderLineLabel画像に枠を作成表示(ActExPic)
            Case ActExPic.IsEdit = False And ActExPic.IsDrawString = False
                Call PicBoderLineLabel画像に付けた枠を消去()
                'Case tempActEx.Equals(ActExPic)
                '    Call PicBoderLineLabel画像に付けた枠を消去()
                '    Call PicBoderLineLabel画像に枠を作成表示(ActExPic)
            Case ActExPic.IsDrawString And myForm3.CheckBoxSelectPicFrameView選択時に枠表示.Checked
                Call PicBoderLineLabel画像に付けた枠を消去()
                Call PicBoderLineLabel画像に枠を作成表示(ActExPic)

        End Select





        '左クリックされたらMouseMoveイベントに関連付ける
        If e.Button = MouseButtons.Left And isMouseDeDrawNow = False Then '2015/01/25マウスで描画中はドラッグ状態にしない

            'If e.X Mod Me.NumericUpDown1.Value <> 0 Then

            '    CurPoint = NowControl.PointToClient(Cursor.Position)
            'End If
            ''マウスカーソルの位置修正？
            'CurPoint = NowControl.PointToClient(Cursor.Position)
            'CurPoint = Cursor.Position
            'Dim xMouse As Integer = RigthDownPoint.X Mod Me.NumericUpDown1.Value '保存画像サイズをグリッドで割った余りX
            'Dim yMouse As Integer = RigthDownPoint.Y Mod Me.NumericUpDown1.Value
            'Dim xyMouse As New Point(CurPoint.X + xMouse, CurPoint.Y + yMouse)
            'Cursor.Position = xyMouse


            IsDragging = True 'ドラッグ中
            DiffPoint = New Point(e.X, e.Y)
            oldPicLocate = New Point(ActExPic.Location.X, ActExPic.Location.Y) 'Transparent6()で仕様


            '右下の画像を動かした時にスクロールバーが勝手に動かないようにダミーの画像を置く
            Dim ScrollX As Integer = Me.Panel2.AutoScrollPosition.X
            Dim ScrollY As Integer = Me.Panel2.AutoScrollPosition.Y

            'OldLocate = New Point(NowControl.Location)
            'DummyPicBox.Size = NowControl.Size
            OldLocate = New Point(ActExPic.Location)
            DummyPicBox.Size = ActExPic.Size
            DummyPicBox.Location = OldLocate '意味ない
            DummyPicBox.Name = "dummypicbox"
            'DummyPicBox.Image = NowControl.Image
            Me.Panel2.Controls.Add(DummyPicBox)


            Call ChangeFocusTステータス表示更新() 'ステータス表示更新

            'ヒストグラム表示更新
            For Each f As Form In Me.OwnedForms
                'For Each f As Form In Application.OpenForms
                If f.Name = "FormHistogram" Then
                    Call Histogram()

                End If
            Next


            'AddHandler NowControl.MouseMove, AddressOf PictureBox1_MouseMove'大幅変更箇所
            'RemoveHandler NowControl.MouseDown, AddressOf PictureBox1_MouseDown 'これをつけても普通に動く…なんで？'大幅変更箇所

        End If


    End Sub
    Private Sub ActExPicNewLocate(MP As Point)
        '新しい座標に移動
        Dim Grid As Integer = Me.NumericUpDownGrid.Value

        '絶対座標のDestX
       
        Dim DestAbsoX As Integer = AbsolutePoint(ActExPic).X + (MP.X - DiffPoint.X)
        Dim DestAbsoY As Integer = AbsolutePoint(ActExPic).Y + (MP.Y - DiffPoint.Y)

        '絶対座標のMOD
        Dim xAbsoMod As Integer = DestAbsoX Mod Grid
        Dim yAbsoMod As Integer = DestAbsoY Mod Grid
        'スクロールバーの移動位置
        Dim ScrollX As Integer = Me.Panel2.AutoScrollPosition.X
        Dim ScrollY As Integer = Me.Panel2.AutoScrollPosition.Y
      


        '画像の座標決定！！！！！
        'NowControl.Location = New Point(DestAbsoX - xAbsoMod + ScrollX, DestAbsoY - yAbsoMod + ScrollY)
        ActExPic.Location = New Point(DestAbsoX - xAbsoMod + ScrollX, DestAbsoY - yAbsoMod + ScrollY)

    End Sub
    Private Sub PictureBox1_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        ''マウスクリックで線の描画
        'If isMouseDeDrawNow Then

        '    Dim PP(1) As PointF
        '    Dim ppc As Integer = MouseDeDrawPic.PathPoints.Count

        '    Dim bmp As Bitmap

        '    If ppc >= 1 AndAlso MouseDeDrawPic.isFill = False And MouseDeDrawPic.CloseLine = False Then
        '        PP(0) = MouseDeDrawPic.PathPoints(ppc - 1)
        '        PP(1) = e.Location
        '        bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic, PP)
        '        MouseDeDrawPic.Image = bmp

        '    ElseIf ppc >= 2 AndAlso MouseDeDrawPic.isFill Then
        '        Dim fillPP As New List(Of PointF)(MouseDeDrawPic.PathPoints)
        '        fillPP.Add(e.Location)
        '        Dim mPP() As PointF = DirectCast(fillPP.ToArray, PointF())
        '        bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic, mPP)
        '        MouseDeDrawPic.Image = bmp

        '    ElseIf ppc >= 2 AndAlso MouseDeDrawPic.CloseLine Then
        '        Dim fillPP As New List(Of PointF)(MouseDeDrawPic.PathPoints)
        '        fillPP.Add(e.Location)
        '        Dim mPP() As PointF = DirectCast(fillPP.ToArray, PointF())
        '        bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic, mPP)

        '        MouseDeDrawPic.Image = bmp

        '    End If

        'End If

        If IsDragging Then
            '------------------------------------過去の遺物----------------------------------------
            'Dim DestX As Integer = NowControl.Location.X + (e.X - DiffPoint.X)
            'Dim DestY As Integer = NowControl.Location.Y + (e.Y - DiffPoint.Y) 'カッコを付けたほうがわかりやすいか
            'Dim xMod As Integer = DestX Mod Me.NumericUpDown1.Value
            'Dim yMod As Integer = DestY Mod Me.NumericUpDown1.Value
            'Dim ControlXDiff As Integer = NowControl.Location.X Mod Me.NumericUpDown1.Value
            'Dim ControlYDiff As Integer = NowControl.Location.Y Mod Me.NumericUpDown1.Value
            '-------------------------------------過去の遺物ここまで------------------------------


            ''-----ここから-------
            'Dim Grid As Integer = Me.NumericUpDownGrid.Value

            ''絶対座標のDestX
            ''Dim debug As Integer = AbsolutePoint(NowControl).X
            ''Dim DestAbsoX As Integer = AbsolutePoint(NowControl).X + (e.X - DiffPoint.X)
            ''Dim DestAbsoY As Integer = AbsolutePoint(NowControl).Y + (e.Y - DiffPoint.Y)
            'Dim debug As Integer = AbsolutePoint(ActExPic).X
            'Dim DestAbsoX As Integer = AbsolutePoint(ActExPic).X + (e.X - DiffPoint.X)
            'Dim DestAbsoY As Integer = AbsolutePoint(ActExPic).Y + (e.Y - DiffPoint.Y)

            ''絶対座標のMOD
            'Dim xAbsoMod As Integer = DestAbsoX Mod Grid
            'Dim yAbsoMod As Integer = DestAbsoY Mod Grid
            ''スクロールバーの移動位置
            'Dim ScrollX As Integer = Me.Panel2.AutoScrollPosition.X
            'Dim ScrollY As Integer = Me.Panel2.AutoScrollPosition.Y
            ''Dim ScrollX As Integer = Math.Abs(Me.Panel2.AutoScrollPosition.X)
            ''Dim ScrollY As Integer = Math.Abs(Me.Panel2.AutoScrollPosition.Y)
            ''DummyPicBox.Location = OldLocate '意味ない
            ''Me.Panel2.AutoScrollPosition = New Point(-ScrollX, -ScrollY)
            ''Me.Panel2.VerticalScroll.Value = -ScrollY
            ''Me.Panel2.HorizontalScroll.Value = -ScrollX


            ''画像の座標決定！！！！！
            ''NowControl.Location = New Point(DestAbsoX - xAbsoMod + ScrollX, DestAbsoY - yAbsoMod + ScrollY)
            'ActExPic.Location = New Point(DestAbsoX - xAbsoMod + ScrollX, DestAbsoY - yAbsoMod + ScrollY)
            ''NowControl.Location = New Point(DestAbsoX + xAbsoMod + ScrollX, DestAbsoY + yAbsoMod + ScrollY)’動きが変、ブレる

            ''--------ここまでは-------------------
            'ActExPicNewLocateにまとめた

            Call ActExPicNewLocate(e.Location)
            Call MoveNow()

            'グリッドではなくカーソルの移動量がグリッド量に達したら移動になる、カーソルキーの動きと同じ
            'Dim xMouseMod As Integer = (e.X - DiffPoint.X) Mod Grid
            'Dim yMouseMod As Integer = (e.Y - DiffPoint.Y) Mod Grid
            'NowControl.Location = New Point(DestAbsoX - xMouseMod + ScrollX, DestAbsoY - yMouseMod + ScrollY)

            'テスト、カーソルの絶対座標をグリッドで割った、これもグリッド移動にならない失敗
            'Dim xAbsMouse As Integer = Me.Panel2.PointToClient(Cursor.Position).X + Math.Abs(Me.Panel2.AutoScrollPosition.X)
            'Dim yAbsMouse As Integer = Me.Panel2.PointToClient(Cursor.Position).Y + Math.Abs(Me.Panel2.AutoScrollPosition.Y)
            'Dim xxAbsMouse As Integer = xAbsMouse Mod Grid
            'Dim yyAbsMouse As Integer = yAbsMouse Mod Grid
            'NowControl.Location = New Point(DestAbsoX - xxAbsMouse + ScrollX, DestAbsoY - yyAbsMouse + ScrollY)


            '-------------------------------------残骸---------------------------------------
            ''絶対座標
            'Dim AbsoX As Integer
            'Dim AbsoY As Integer
            ''If TestFlag = True Then
            ''    AbsoX = AbsolutePoint(NowControl).X
            ''    AbsoY = AbsolutePoint(NowControl).Y
            ''    TestFlag = False
            ''End If


            ' ''絶対座標MOD
            ''Dim xAbsoMod As Integer = AbsoX Mod Me.NumericUpDown1.Value
            ''Dim yAbsoMod As Integer = AbsoY Mod Me.NumericUpDown1.Value
            ' ''相対座標
            ''Dim x As Integer = DestX Mod Me.NumericUpDown1.Value
            ''Dim y As Integer = DestY Mod Me.NumericUpDown1.Value
            ' ''絶対座標MOD-相対座標MOD
            ''NowControl.Location = New Point(DestX - xMod + (xAbsoMod - x), DestY - yMod + (yAbsoMod - y))

            'Dim ASBPx As Integer = Math.Abs(Me.Panel2.AutoScrollPosition.X)
            'Dim ASBpy As Integer = Math.Abs(Me.Panel2.AutoScrollPosition.Y)

            'Dim NeoDestX As Integer = AbsoX - ASBPx + e.X - DiffPoint.X
            'Dim NeoDestY As Integer = AbsoY - ASBpy + e.Y - DiffPoint.Y

            'Dim ASBPxMod As Integer = ASBPx Mod Me.NumericUpDown1.Value
            'Dim ASBPyMod As Integer = ASBpy Mod Me.NumericUpDown1.Value


            ''NowControl.Location = New Point(DestX + ASBPxMod, DestY + ASBPyMod)
            ''NowControl.Location = New Point(AbsoX - xMod, AbsoY - yMod)
            ''NowControl.Location = New Point(NeoDestX - xAbsoMod, NeoDestY - yAbsoMod)
            'NowControl.Location = New Point(DestX - xMod, DestY - yMod)’これが一番まともだった
            '-------------------------------------ここまで残骸---------------------------------------


            'これはつけたほうがいいのか？→直ったぽい？あれだけ悩んだのに…これだけ？本当に直った？
            '→直っていないけど少し頻度が下がった気がする
            'RemoveHandler NowControl.MouseDown, AddressOf PictureBox1_MouseDown'大幅変更箇所

            '自動再描画、超重い
            If Me.RadioButtonRealtimeRendering.Checked Then
                Call Transparent3()
            End If




            '透過処理
            '半リアルタイムにチェックが入っていたら透過する
            If Me.CheckBoxTransparent.Checked Then
                'Call TransparentForMove() '動かした画像のみ再描画
                'Call Transparent() 'すべての画像を再描画
                'Call TransparentForMove2()
                'Call TransparentForMove3()

            End If


            '範囲選択画像ならマーカーも移動
            If ActExPic.Equals(selectPicbox範囲選択画像) Then
                Call RSLabel選択範囲画像ラベルの移動()

            End If

            '図形の編集中の移動なら枠も移動
            If isDrawEditNow Then
                Call サイズ変更用のラベルの再表示()
            Else
                '編集中ではない時は枠だけの移動、枠がなければ何もしない
                Call PicBorderMove枠の移動(ActExPic.Bounds)
            End If

            'Call PicBorderMove枠の移動(ActExPic.Bounds)


        End If
        'マウスクリックで線の描画
        If isMouseDeDrawNow Then

            Dim PP(1) As PointF
            Dim ppc As Integer = MouseDeDrawPic.PathPoints.Count

            Dim bmp As Bitmap

            If MouseDeDrawPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
                If ppc > 0 Then
                    Call MouseDrawBezierベジェ曲線(e.Location)
                    bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic)
                    MouseDeDrawPic.Image = bmp

                End If

            Else
                If ppc >= 1 AndAlso MouseDeDrawPic.isFill = False And MouseDeDrawPic.CloseLine = False Then
                    '塗りつぶしではないand閉じていない場合
                    PP(0) = MouseDeDrawPic.PathPoints(ppc - 1)
                    PP(1) = e.Location
                    bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic, PP)
                    MouseDeDrawPic.Image = bmp

                ElseIf ppc >= 2 AndAlso MouseDeDrawPic.isFill Then
                    Dim fillPP As New List(Of PointF)(MouseDeDrawPic.PathPoints)
                    fillPP.Add(e.Location)
                    Dim mPP() As PointF = DirectCast(fillPP.ToArray, PointF())
                    bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic, mPP)
                    MouseDeDrawPic.Image = bmp

                ElseIf ppc >= 2 AndAlso MouseDeDrawPic.CloseLine Then
                    Dim fillPP As New List(Of PointF)(MouseDeDrawPic.PathPoints)
                    fillPP.Add(e.Location)
                    Dim mPP() As PointF = DirectCast(fillPP.ToArray, PointF())
                    bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic, mPP)

                    MouseDeDrawPic.Image = bmp

                End If

            End If

        End If

    End Sub

    Private Sub PictureBox1_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        'System.GC.Collect() 'メモリの開放、MouseDownの方だけでいいかも

        Dim LastControl As Control = DirectCast(sender, Control) '左クリックを離した時のコントロール
        'ドラッグが終わったらMouseDownとMouseMoveイベントを剥がす
        If e.Button = MouseButtons.Left Then
            IsDragging = False
            'RemoveHandler NowControl.MouseDown, AddressOf PictureBox1_MouseDown'大幅変更箇所
            'RemoveHandler NowControl.MouseMove, AddressOf PictureBox1_MouseMove'大幅変更箇所
            'RemoveHandler NowControl.MouseMove, AddressOf PictureBox1_MouseMove'大幅変更箇所
            'FocusPic = NowControl
            'Me.NumNowPic.Value = NowControl.Tag 'サムネイル画像の横



            'ドラッグしていた画像と離した時の画像が同じならMouseDownイベントは関連付けしたままにする
            If NowControl.Name = LastControl.Name Then
                'AddHandler NowControl.MouseDown, AddressOf PictureBox1_MouseDown'大幅変更箇所
                'AddHandler NowControl.PreviewKeyDown, AddressOf PictureBox1_PreviewKeyDown
                'AddHandler NowControl.KeyDown, AddressOf ListBox1_KeyDown
            End If

        End If

        'If e.Button = Windows.Forms.MouseButtons.Right Then
        '    If isMouseDeDrawNow Then
        '        'マウスクリックで描画中
        '        '終了、
        '        If MouseDeDrawPic.Equals(ActExPic) = False Then
        '            '違う画像をクリックしたらマウスで描画を終了する
        '            Call MouseEndDrawマウスで描画終了処理()
        '            'If ActExPic.IsEdit Then
        '            '    ToolStripMenuItem頂点の編集開始.Visible = True
        '            'Else
        '            '    ToolStripMenuItem頂点の編集開始.Visible = False
        '            'End If
        '            ToolStripMenuItemSave.Visible = True
        '            ToolStripMenuItemSaveT.Visible = True
        '            ToolStripMenuItem頂点の編集開始.Visible = True

        '        ElseIf e.Button = Windows.Forms.MouseButtons.Right Then
        '            ToolStripMenuItemクリックで描画を終了.Visible = True
        '            ToolStripMenuItemSave.Visible = False
        '            ToolStripMenuItemSaveT.Visible = False
        '            ToolStripMenuItem頂点の編集開始.Visible = False

        '        End If
        '    End If
        'End If

        'テスト
        'RemoveHandler NowControl.MouseMove, AddressOf PictureBox1_MouseMove


        'これもMoveAfterというメソッドに入れた
        'FullSize = Me.Panel2.PreferredSize
        '保存する画像の大きさはこれでOK、以前のMe.Panel2.PreferredSizeではおかしくなる
        'FullSize = RigthDownPoint()
        'Me.TextBox9.Text = FullSize.ToString

        'NowControl.Focus()
        'NowControl.AutoScrollOffset
        'ScrollPoint = Me.Panel2.AutoScrollPosition
        'Debug.Print(Me.RestoreBounds.ToString)

        '移動した画像がウィンドウの上か下に移動してはみ出た時の位置調整
        'Dim LastPic As Point
        'LastPic = AbsolutePoint(LastControl) '移動後の画像の絶対座標取得
        ''Debug.Print(LastPic.ToString)
        'Dim xDiff As Integer = Math.Abs(LastPic.X) '絶対座標の数値
        'Dim yDiff As Integer = Math.Abs(LastPic.Y)

        ''絶対座標がマイナスならウィンドウ外なのですべての画像を右か下に移動
        'If LastPic.X < 0 Then
        '    For Each c As ExPictureBox In myPicAr
        '        c.Location = New Point(c.Location.X + xDiff, c.Location.Y)
        '    Next
        'End If

        'If LastPic.Y < 0 Then
        '    For Each c As ExPictureBox In myPicAr
        '        c.Location = New Point(c.Location.X, c.Location.Y + yDiff)
        '    Next
        'End If


        '位置調整、左上が開いたら全体を詰める
        Call TopLeftMove() 'これをつけるとup直後にずれる
        Call MoveAfter()
        Call ChangeFocusTステータス表示更新()

        'FocusPic.Focus()'これはあったほうがいいのかどうか微妙、どうしよう
        Me.ListBox1.Focus()

        'ダミーの画像を消去
        If Me.Panel2.Controls.Contains(DummyPicBox) Then
            Me.Panel2.Controls.Remove(DummyPicBox)
            'DummyPicBox.Dispose()
        End If
        'Dim test As Integer = Me.Panel2.Controls.Count
        'Me.Panel2.Controls.Remove(DummyPicBox)

        '範囲選択画像ならポイントを移動させる

        'If ActExPic.Name = "範囲選択_T" Then
        '    'Call SelectRangePoint()
        '    Call RSLabel選択範囲画像ラベルの移動() '2015/01/14
        'End If

        ''範囲選択画像表示されていて最前面の時だけマーカーを表示→これはないほうがいいかな
        'If Me.Panel2.Controls.Contains(selectPicbox範囲選択画像) Then
        '    If selectPicbox範囲選択画像.Tag = 1 And
        '        ActExPic.Equals(selectPicbox範囲選択画像) Then
        '        Call RSLabel選択範囲画像ラベルの移動() '2015/01/14
        '    Else
        '        Call RSLabel選択範囲画像ラベルの非表示()
        '    End If

        'End If




        '半自動再描画のチェックが入っていれば透過処理再描画
        If Me.RadioButtonHalfRealtimeRendering.Checked Then
            'Call Transparent2() '3が問題なければ3のほうが軽い
            Call Transparent6()

        End If

        '透過処理
        ''半リアルタイムにチェックが入っていたら透過する
        'If Me.CheckBoxTransparent.Checked Then
        '    'Call TransparentForMove() '動かした画像のみ再描画
        '    Call Transparent() 'すべての画像を再描画
        'End If

        Call サイズ変更用のラベルの再表示()
        Call 編集画像の背景更新()
        Call PicBorderMove枠の移動(ActExPic.Bounds)
        If Not selectPicbox範囲選択画像 Is Nothing Then
            Call RSLabel選択範囲画像ラベルの移動()
        End If

    End Sub

    Private Sub PictureBox1_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) ' Handles PictureBox1.MouseLeave
        'ドラッグ中に何かの拍子で画像が離れたらMouseMoveイベントを剥がす
        'RemoveHandler NowControl.MouseDown, AddressOf PictureBox1_MouseDown'大幅変更箇所
        'RemoveHandler NowControl.MouseMove, AddressOf PictureBox1_MouseMove'大幅変更箇所

        'Debug.Print("leave")
    End Sub
    Private Sub PictureBox1_MouseDoubleClickダブルクリック(sender As ExPictureBox, e As MouseEventArgs)

        If isMouseDeDrawNow Then
            'ダブルクリックで追加された最後の頂点を削除する
            Dim pCount As Integer = MouseDeDrawPic.PathPoints.Count

            If MouseDeDrawPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
                'ここでは3つ削除、もう3つは終了処理で削除
                MouseDeDrawPic.PathPoints.RemoveRange(pCount - 3, 3)

            Else
                MouseDeDrawPic.PathPoints.RemoveAt(pCount - 1)
            End If


            Call MouseEndDrawマウスで描画終了処理()

        End If
    End Sub



    'キーボードで画像の操作、重なりの順番、画像の消去、画像の移動
    Private Sub ListBox1_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles ListBox1.KeyDown, NumNowPic.KeyDown

        If myPicAr.Count = 0 Then
            Exit Sub
        End If


        Select Case e.KeyData
            Case Keys.PageUp 'PageUpが押されたら画像の順番をひとつ上げる
                Call FocusPictureUp()
                sender.focus()

            Case Keys.PageDown 'PageDownが押されたら画像の順番をひとつ下げる
                Call FocusPictureDown()
                sender.focus()
            Case Keys.F4 '選択画像の削除
                Call DelPic()
            Case Keys.Control + Keys.V
                Call AddPicFromClipBクリップボードから追加()

        End Select
        'If e.KeyData = Keys.Control Then
        '    Select Case e.KeyData
        '        Case Keys.Control + Keys.V
        '            Call AddPicFromClipBクリップボードから追加()

        '    End Select
        'End If
        '選択画像の移動
        Dim x As Integer = ActExPic.Location.X
        Dim y As Integer = ActExPic.Location.Y
        Dim gv As Integer = NumericUpDownGrid.Value
        Dim nP As Point = ActExPic.Location
        Select Case e.KeyData
            Case Keys.Up
                nP = New Point(x, y - gv)
            Case Keys.Down
                nP = New Point(x, y + gv)
            Case Keys.Left
                nP = New Point(x - gv, y)
            Case Keys.Right
                nP = New Point(x + gv, y)
        End Select

        '移動していたら表示更新
        If ActExPic.Location <> nP Then
            ActExPic.Location = nP
            Call PicMove方向キーで画像移動()
            Call サイズ変更用のラベルの再表示()
        End If


        'selectPicbox範囲選択画像のときのサイズ変更、ShiftキーとAltキー
        'Shift＋で1ピクセルのサイズ増減
        'Alt＋でグリッド分のサイズ増減
        'Ctrl＋で1ピクセルの移動
        If ActExPic.Equals(selectPicbox範囲選択画像) Then
            With myForm3
                Dim lx As Integer = .NumericUpDownRectRangeH.Minimum '設定できる最小値
                Dim ly As Integer = .NumericUpDownRectRangeV.Minimum
                Dim mx As Integer = .NumericUpDownRectRangeH.Maximum '設定できる最大値
                Dim my As Integer = .NumericUpDownRectRangeV.Maximum
                Dim w As Integer = .NumericUpDownRectRangeH.Value
                Dim h As Integer = .NumericUpDownRectRangeV.Value
                'Dim gv As Integer = NumericUpDownGrid.Value

                If e.Shift Then
                    Select Case e.KeyData
                        Case Keys.Up + Keys.Shift ''Shift＋↑
                            If h - 1 < ly Then Exit Select
                            .NumericUpDownRectRangeV.Value -= 1
                            Call PicMove方向キーで画像移動()
                        Case Keys.Left + Keys.Shift ''Shift＋←
                            If w - 1 < lx Then Exit Select
                            .NumericUpDownRectRangeH.Value -= 1
                            Call PicMove方向キーで画像移動()
                        Case Keys.Down + Keys.Shift ''Shift＋↓
                            If h + 1 > mx Then Exit Select
                            .NumericUpDownRectRangeV.Value += 1
                            Call PicMove方向キーで画像移動()
                        Case Keys.Right + Keys.Shift ''Shift＋→
                            If w + 1 > my Then Exit Select
                            .NumericUpDownRectRangeH.Value += 1
                            Call PicMove方向キーで画像移動()
                    End Select

                ElseIf e.Alt Then
                    Select Case e.KeyData
                        Case Keys.Up + Keys.Alt ''Shift＋↑
                            If h - gv < ly Then Exit Select
                            .NumericUpDownRectRangeV.Value -= gv
                            Call PicMove方向キーで画像移動()
                        Case Keys.Left + Keys.Alt ''Shift＋←
                            If w - gv < lx Then Exit Select
                            .NumericUpDownRectRangeH.Value -= gv
                            Call PicMove方向キーで画像移動()
                        Case Keys.Down + Keys.Alt ''Shift＋↓
                            If h + gv > mx Then Exit Select
                            .NumericUpDownRectRangeV.Value += gv
                            Call PicMove方向キーで画像移動()
                        Case Keys.Right + Keys.Alt ''Shift＋→
                            If w + gv > my Then Exit Select
                            .NumericUpDownRectRangeH.Value += gv
                            Call PicMove方向キーで画像移動()
                    End Select
                ElseIf e.Control Then

                    Select Case e.KeyData
                        Case Keys.Up + Keys.Control ''Shift＋↑
                            selectPicbox範囲選択画像.Location = New Point(x, y - 1)
                            Call PicMove方向キーで画像移動()
                        Case Keys.Left + Keys.Control ''Shift＋←
                            selectPicbox範囲選択画像.Location = New Point(x - 1, y)
                            Call PicMove方向キーで画像移動()
                        Case Keys.Down + Keys.Control ''Shift＋↓
                            selectPicbox範囲選択画像.Location = New Point(x, y + 1)
                            Call PicMove方向キーで画像移動()
                        Case Keys.Right + Keys.Control ''Shift＋→
                            selectPicbox範囲選択画像.Location = New Point(x + 1, y)
                            Call PicMove方向キーで画像移動()
                       
                    End Select
                End If
            End With



        End If


        '範囲選択画像ならマーカーラベルも移動
        If ActExPic.Name = "範囲選択_T" Then
            Call RSLabel選択範囲画像ラベルの移動()
        End If

        '頂点ラベル移動中にaltキーが押されたらグリッドに移動
        If e.Alt And isLabelMoveNow Then
            Call 頂点座標の計算(ActLabel)
            'Dim PP() As Point = {New Point(EditNowPic.PathPoints.Item(ActLabel.Tag))}
            Dim PP As New List(Of PointF)(EditNowPic.PathPoints)
            Dim sPP As New PointF
            Dim alt As Integer = ActLabel.Tag
            Call Points2NearGrid頂点を一番近いグリッドに移動(PP, alt)

            'ベジェ曲線でアンカーポイントなら
            If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 And alt Mod 3 = 0 Then
                Dim ep As Integer = EditNowPic.PathPoints.Count - 1
                If alt = 0 Then '始点の時
                    'PP = {New Point(EditNowPic.PathPoints.Item(1))}
                    sPP = EditNowPic.PathPoints(1)
                    Call Points2NearGrid頂点を一番近いグリッドに移動(sPP, 1)
                    If EditNowPic.CloseLine Then '始点の時で閉じた線の時
                        'PP = {New Point(EditNowPic.PathPoints.Item(ep))}
                        PP.Clear()
                        PP.Add(EditNowPic.PathPoints(ep))
                        Call Points2NearGrid頂点を一番近いグリッドに移動(PP, ep)
                        'PP = {New Point(EditNowPic.PathPoints.Item(ep - 1))}
                        PP.Clear()
                        PP.Add(EditNowPic.PathPoints(ep - 1))
                        Call Points2NearGrid頂点を一番近いグリッドに移動(PP, ep - 1)
                    End If
                ElseIf alt = ep Then '終点の時
                    'PP = {New Point(EditNowPic.PathPoints.Item(ep - 1))}
                    PP.Clear()
                    PP.Add(EditNowPic.PathPoints(ep - 1))
                    Call Points2NearGrid頂点を一番近いグリッドに移動(PP, ep - 1)
                Else 'それ以外のアンカーポイント時
                    'PP = {New Point(EditNowPic.PathPoints.Item(alt - 1))}
                    PP.Clear()
                    PP.Add(EditNowPic.PathPoints(alt - 1))
                    Call Points2NearGrid頂点を一番近いグリッドに移動(PP, alt - 1)
                    'PP = {New Point(EditNowPic.PathPoints.Item(alt + 1))}
                    PP.Clear()
                    PP.Add(EditNowPic.PathPoints(alt + 1))
                    Call Points2NearGrid頂点を一番近いグリッドに移動(PP, alt + 1)
                End If
            End If

        End If
    End Sub
    Private Sub PicMove方向キーで画像移動()
        'ListBoxのKeyDownイベント用

        Call MoveAfter()

        '半自動再描画か自動再描画のチェックが入っていれば透過処理再描画
        If Me.RadioButtonNoRendering.Checked = False Then
            Call Transparent5()
        End If

        '半リアルタイムにチェックが入っていたら透過する
        If Me.CheckBoxTransparent.Checked Then
            'Call TransparentForMove() '動かした画像のみ再描画
            Call Transparent() 'すべての画像を再描画
        End If

    End Sub
    'Ctrl＋sで画像を保存
    '予めFormのプロパティでKeyPreviewをTrueにしておく
    'コントロールでキーが押されたことを知る: .NET Tips: C#, VB.NET
    'http://dobon.net/vb/dotnet/control/keyevent.html

    Private Sub Form1_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown

        If e.KeyData = Keys.Control + Keys.S Then
            Call SavePic()
        End If

    End Sub


    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        SaveFile.Filter = "*.png|*.png|*.jpg|*.jpg;*.jpeg|*.bmp|*.bmp|*.gif|*.gif"
        'Me.ListBox1.Focus()
        'Me.Panel1.BackgroundImage = My.Resources.ゆっくりメテオさん520x450_Exif付き
        'Me.Panel2.BackgroundImage = My.Resources.ゆっくりメテオさん520x450_Exif付き
        'Dim val As System.Drawing.Imaging.PixelFormat
        'For Each val In [Enum].GetValues(GetType(PixelFormat))
        '    Dim eName As String = [Enum].GetName(GetType(PixelFormat), val)
        '    Dim eVal As Integer = [Enum].Parse(GetType(PixelFormat), val)
        '    Me.ListBox2.Items.Add(eVal & eName)

        'Next
    End Sub

    '画像保存
    '保存用の画像の作成メソッドのPrivate Sub SavePicCreate()で一部使っている
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        'Dim ImageFull As Bitmap = New Bitmap(Me.Panel1.PreferredSize.Width, Me.Panel1.PreferredSize.Height)
        'Dim RectFull As Rectangle = New Rectangle(New Point(0, 0), Me.Panel1.PreferredSize)
        'Me.DrawToBitmap(ImageFull, RectFull)
        'ImageFull.Save("c:\png\Full.png")

        'Dim ImageFull As Bitmap = New Bitmap(Me.PreferredSize.Width, Me.PreferredSize.Height)
        'Dim RectFull As Rectangle = New Rectangle(New Point(0, 0), Me.PreferredSize)
        'Me.DrawToBitmap(ImageFull, RectFull)
        'ImageFull.Save("c:\png\Full.png")
        Call SortForSave() '保存用画像の並べ替え

        If myPicAr.Count = 0 Then '画像が一個もなければ何もしない
            Return
        End If

        FullSize = RigthDownPoint()
        Dim FullPicBox As New ExPictureBox
        'Dim BitFull As Bitmap = New Bitmap(FullSize.X, FullSize.Y, System.Drawing.Imaging.PixelFormat.Alpha)
        Dim BitFull As Bitmap = New Bitmap(FullSize.X, FullSize.Y)
        FullPicBox.Location = New Point(0, 0)
        FullPicBox.Image = BitFull
        'BitFull.MakeTransparent(Drawing.Color.Aqua)
        Dim FullGra As Graphics = Graphics.FromImage(FullPicBox.Image)
        'FullGra.FillRectangle(Brushes.Aqua, FullGra.VisibleClipBounds)
        Dim myBrusheColor As Color = Me.myBackColor.BackColor
        Dim myBrushe As New SolidBrush(myBrusheColor)
        FullGra.FillRectangle(myBrushe, FullGra.VisibleClipBounds)

        'FullGra.FillRectangle(Brushes.Transparent, FullGra.VisibleClipBounds)
        'Me.Controls.Add(FullPicBox)


        'If Me.Panel2.VerticalScroll.ValueThen Then

        'End If
        '------------------この辺りは残骸、絶対座標を得る関数作ったから不要---------------------
        '保存する時の画像座標の修正、絶対座標の取得(マイナスがありえない座標)
        'スクロールバーが表示されているとき用
        'If Me.Panel2.VerticalScroll.Visible And Me.Panel2.HorizontalScroll.Visible Then
        '    Dim ASPx As Integer = Me.Panel2.AutoScrollPosition.X 'スクロールバーの位置取得
        '    Dim ASPy As Integer = Me.Panel2.AutoScrollPosition.Y

        '    If ASPx < 0 Then 'スクロールバーが動いていたらマイナスになるので絶対値を取得
        '        ASPx = Math.Abs(ASPx)
        '    End If
        '    If ASPy < 0 Then
        '        ASPy = Math.Abs(ASPy)
        '    End If

        '    For Each c As ExPictureBox In myPicArR 'スクロールバーの移動ぶんを足して正しい座標に修正
        '        Dim ASP As New Point(c.Location.X + ASPx, c.Location.Y + ASPy)
        '        'FullGra.DrawImage(c.image, c.location)
        '        FullGra.DrawImage(c.Image, ASP)
        '    Next
        'End If

        ''スクロールバーが表示されていないとき用
        'For Each c As ExPictureBox In myPicArR
        '    FullGra.DrawImage(c.Image, c.Location)
        'Next

        For Each c As ExPictureBox In myPicArR
            FullGra.DrawImage(c.Image, AbsolutePoint(c))
        Next


        FullPicBox.Image.Save("c:\png\fullpicbox.png")
        FullPicBox.Dispose()

    End Sub




    '画像移動後の処理(座標の表示更新、ウィンドウ位置調整)メソッド
    Private Sub MoveAfter()
        'Call TopLeftMove()
        Me.TextBox3.Text = ActExPic.Location.ToString() '画像の相対座標(表示領域)
        Me.TextBoxAbsoluteLocate.Text = AbsolutePoint(ActExPic).ToString '画像の絶対座標を表示更新
        '保存する画像の大きさはこれでOK、以前のMe.Panel2.PreferredSizeではおかしくなる
        FullSize = RigthDownPoint()
        Me.TextBox9.Text = FullSize.ToString
        Me.ToolStripStatusLabel1.Text = "相対座標" & ActExPic.Location.ToString
        Me.ToolStripStatusLabel2.Text = "絶対座標" & AbsolutePoint(ActExPic).ToString
        Me.ToolStripStatusLabel3.Text = "保存サイズ" & RigthDownPoint.ToString
        Me.ToolStripStatusLabel4.Text = "画像サイズX=" & ActExPic.Width & ",Y=" & ActExPic.Height
    End Sub

    '移動中の座標表示
    Private Sub MoveNow()
        Me.TextBox3.Text = ActExPic.Location.ToString() '画像の相対座標(表示領域)
        Me.TextBoxAbsoluteLocate.Text = AbsolutePoint(ActExPic).ToString '画像の絶対座標を表示更新
        Me.TextBox9.Text = RigthDownPoint().ToString
        Me.ToolStripStatusLabel1.Text = "相対座標" & ActExPic.Location.ToString
        Me.ToolStripStatusLabel2.Text = "絶対座標" & AbsolutePoint(ActExPic).ToString
        Me.ToolStripStatusLabel3.Text = "保存サイズ" & RigthDownPoint.ToString
    End Sub
    'FocusPicが変わった時のステータス表示更新メソッド
    Private Sub ChangeFocusTステータス表示更新()
        Me.TextBox2.Text = ActExPic.Name
        Me.TextBox3.Text = ActExPic.Location.ToString
        Me.TextBoxAbsoluteLocate.Text = AbsolutePoint(ActExPic).ToString
        Me.TextBoxPicSize.Text = ActExPic.Size.ToString

        Me.NumNowPic.Value = ActExPic.Tag 'いる？→いる、数値変化のイベントでサムネの変更とかしている


        Me.ToolStripStatusLabel1.Text = "相対座標" & ActExPic.Location.ToString
        Me.ToolStripStatusLabel2.Text = "絶対座標" & AbsolutePoint(ActExPic).ToString
        Me.ToolStripStatusLabel4.Text = "画像サイズX=" & ActExPic.Width & ",Y=" & ActExPic.Height
        Me.ToolStripStatusLabel5.Text = Image.GetPixelFormatSize(ActExPic.Image.PixelFormat) & "bit"
        Me.ToolStripStatusLabel6.Text = [Enum].GetName(GetType(PixelFormat), ActExPic.Image.PixelFormat)
        Me.ToolStripStatusLabel7.Text = ActExPic.Name 

    End Sub

    '表示領域のサイズ表示の更新
    Private Sub Form1_SizeChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.SizeChanged
        Me.TextBox8.Text = Me.Panel2.ClientSize.ToString

    End Sub

    '一番右下の座標を得る関数、
    '保存する画像の大きさそのもの、スクロールバーが出ていてそれが移動している場合はその分を足す
    Private Function RigthDownPoint() As Point
        Dim RMax As Integer
        Dim DMax As Integer
        Dim xDiff As Integer
        Dim yDiff As Integer


        If myPicAr.Count = 0 Then '画像がひとつもなければ0,0を返して終わり
            RFPoint = New Point(0, 0)
            Return RFPoint
        End If

        xDiff = Math.Abs(Me.Panel2.AutoScrollPosition.X) 'スクロールバーの移動距離、スクロールバーが無いときは無視されるみたい
        yDiff = Math.Abs(Me.Panel2.AutoScrollPosition.Y)
        Dim xxDiff = Math.Abs(Me.Panel2.HorizontalScroll.Value)
        Dim yyDiff = Math.Abs(Me.Panel2.VerticalScroll.Value)

        'For Each c As ExPictureBox In myPicAr 'すべての画像の座標と幅と高さを調べて
        '    If c.Location.X + c.Size.Width > RMax Then
        '        RMax = c.Location.X + c.Size.Width + xDiff '一番右の座標
        '    End If

        '    If c.Location.Y + c.Size.Height > DMax Then
        '        DMax = c.Location.Y + c.Size.Height + yDiff '一番下の座標
        '    End If
        'Next

        For Each c As ExPictureBox In myPicAr
            Dim xAbs As Integer = c.Location.X + xDiff + c.Size.Width
            Dim yAbs As Integer = c.Location.Y + yDiff + c.Size.Height
            If xAbs > RMax Then
                RMax = xAbs
            End If

            If yAbs > DMax Then
                DMax = yAbs
            End If
        Next

        RFPoint = New Point(RMax, DMax) 'スクロールバーの移動ぶんを加味した最終的な座標
        Return RFPoint
    End Function

    '渡されたコントロールのパネル2の中での絶対座標を返す関数
    Private Function AbsolutePoint(ByVal LastPic As Control) As Point
        Dim xDiff As Integer = Math.Abs(Me.Panel2.AutoScrollPosition.X)
        Dim yDiff As Integer = Math.Abs(Me.Panel2.AutoScrollPosition.Y)
        Dim AbsPoint = New Point(LastPic.Location.X + xDiff, LastPic.Location.Y + yDiff)
        Return AbsPoint
    End Function

    '画像を移動した結果左上が開いたら詰めるメソッド
    Private Sub TopLeftMove()
        Dim LMax As Integer = RigthDownPoint.X '検索する初期値は全体画像の大きさ、つまり最大値
        Dim UMax As Integer = RigthDownPoint.Y
        'Dim xDiff As Integer’スクロールバーは関係ないみたい
        'Dim yDiff As Integer

        'xDiff = Math.Abs(Me.Panel2.AutoScrollPosition.X) 'スクロールバーの移動距離、スクロールバーが無いときは無視されるみたい
        'yDiff = Math.Abs(Me.Panel2.AutoScrollPosition.Y)

        For Each c As ExPictureBox In myPicAr '一番左上を探す

            If AbsolutePoint(c).X < LMax Then
                LMax = AbsolutePoint(c).X
            End If

            If AbsolutePoint(c).Y < UMax Then
                UMax = AbsolutePoint(c).Y '一番上の座標
            End If

        Next
        For Each c As ExPictureBox In myPicAr
            c.Location = New Point(c.Location.X - LMax, c.Location.Y - UMax)
        Next


        '    '自分以外は左下に移動、これはおかしい
        'For Each c As ExPictureBox In myPicAr
        '    If c.Tag <> FocusPic.Tag Then
        '        c.Location = New Point(c.Location.X - LMax, c.Location.Y - UMax)
        '    End If
        'Next
        'すべての画像を空いた左上に詰める



        ''動かした画像の座標がマイナスなら他の画像を右下にずらす
        'Dim xDiff As Integer = Math.Abs(FocusPic.Location.X)
        'Dim yDiff As Integer = Math.Abs(FocusPic.Location.Y)

        'If FocusPic.Location.X < 0 Or FocusPic.Location.Y < 0 Then
        '    For Each c As ExPictureBox In myPicAr
        '        c.Location = New Point(c.Location.X + xDiff, c.Location.Y + yDiff)
        '    Next
        'End If


    End Sub
    Private Overloads Sub FocusPictureUp(Exp As ExPictureBox)
        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        '一番上の画像のタグが1、数字が大きいほど下になる
        If Exp.Tag = 1 Or myPicAr.Count = 0 Then '一番上の画像か画像がなければ何もしないで終わる
            Return
        End If
        '一個上の画像のタグに1を足す
        For Each c As ExPictureBox In myPicAr
            If c.Tag = Exp.Tag - 1 Then
                c.Tag = c.Tag + 1
                Exit For
            End If
        Next

        '一個上の画像のタグに1を足す(クローン用)
        For Each c As ExPictureBox In myPicArClone
            If c.Tag = Exp.Tag - 1 Then
                c.Tag = c.Tag + 1
            ElseIf c.Tag = Exp.Tag Then
                c.Tag = c.Tag - 1
            End If
        Next

        '一個上の画像のタグに1を足す(backup用)
        For Each c As ExPictureBox In myPicArBackup
            If c.Tag = Exp.Tag - 1 Then
                c.Tag = c.Tag + 1
            ElseIf c.Tag = Exp.Tag Then
                c.Tag = c.Tag - 1
            End If
        Next

        Exp.Tag = Exp.Tag - 1 '自分自身の画像のタグから1引く

        Call SortPic() 'すべての画像をタグに従って画像の重なりを並べ替え表示する

        Call ReConstructタグに従ってコレクションの再構成()
        '↓の二つはReConstructタグに従ってコレクションの再構成にまとめた
        ''コレクションを更新する、これを実行しないと実際のピクチャーボックスとコレクションの中身が一致しない
        'myPicAr.Clear()
        'For Each c As Control In Me.Panel2.Controls
        '    If c.GetType Is GetType(ExPictureBox) Then
        '        If c.Tag > 0 Then
        '            myPicAr.Add(c)
        '        End If

        '    End If
        'Next
        ''myPicAr.AddRange(Me.Panel2.Controls)'旧式

        'Call TagSortCollection()


        '透過処理
        Call Transparent4()

        '半リアルタイムにチェックが入っていたら透過する
        'If Me.CheckBoxTransparent.Checked Then
        '    'Call TransparentForMove() '動かした画像のみ再描画
        '    Call Transparent() 'すべての画像を再描画
        'End If

        Me.NumNowPic.Value = Exp.Tag
        Call UpdateThumbnail()
    End Sub
    '自身の画像を一つ上げるためにタグの調整をするメソッド
    Private Overloads Sub FocusPictureUp()
        If myPicAr.Count = 0 Then
            Exit Sub
        End If
        Dim Exp As ExPictureBox = ActExPic
        Call FocusPictureUp(Exp)

        ''一番上の画像のタグが1、数字が大きいほど下になる
        'If ActExPic.Tag = 1 Or myPicAr.Count = 0 Then '一番上の画像か画像がなければ何もしないで終わる
        '    Return
        'End If
        ''一個上の画像のタグに1を足す
        'For Each c As ExPictureBox In myPicAr
        '    If c.Tag = ActExPic.Tag - 1 Then
        '        c.Tag = c.Tag + 1
        '        Exit For
        '    End If
        'Next

        ''一個上の画像のタグに1を足す(クローン用)
        'For Each c As ExPictureBox In myPicArClone
        '    If c.Tag = ActExPic.Tag - 1 Then
        '        c.Tag = c.Tag + 1
        '    ElseIf c.Tag = ActExPic.Tag Then
        '        c.Tag = c.Tag - 1
        '    End If
        'Next

        ''一個上の画像のタグに1を足す(backup用)
        'For Each c As ExPictureBox In myPicArBackup
        '    If c.Tag = ActExPic.Tag - 1 Then
        '        c.Tag = c.Tag + 1
        '    ElseIf c.Tag = ActExPic.Tag Then
        '        c.Tag = c.Tag - 1
        '    End If
        'Next

        'ActExPic.Tag = ActExPic.Tag - 1 '自分自身の画像のタグから1引く

        'Call SortPic() 'すべての画像をタグに従って画像の重なりを並べ替え表示する

        'Call ReConstructタグに従ってコレクションの再構成()
        ''↓の二つはReConstructタグに従ってコレクションの再構成にまとめた
        ' ''コレクションを更新する、これを実行しないと実際のピクチャーボックスとコレクションの中身が一致しない
        ''myPicAr.Clear()
        ''For Each c As Control In Me.Panel2.Controls
        ''    If c.GetType Is GetType(ExPictureBox) Then
        ''        If c.Tag > 0 Then
        ''            myPicAr.Add(c)
        ''        End If

        ''    End If
        ''Next
        ' ''myPicAr.AddRange(Me.Panel2.Controls)'旧式

        ''Call TagSortCollection()


        ''透過処理
        'Call Transparent4()

        ''半リアルタイムにチェックが入っていたら透過する
        ''If Me.CheckBoxTransparent.Checked Then
        ''    'Call TransparentForMove() '動かした画像のみ再描画
        ''    Call Transparent() 'すべての画像を再描画
        ''End If

        'Me.NumNowPic.Value = ActExPic.Tag
        'Call UpdateThumbnail()

    End Sub

    '自身の画像を一つ下げるげるためにタグの調整をするメソッド
    Private Sub FocusPictureDown()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        If ActExPic.Tag = myPicAr.Count Or myPicAr.Count = 0 Then '一番下の画像か画像がなければ何もしないで終わる
            Exit Sub
        End If

        For Each c As ExPictureBox In myPicAr '一個上の画像のタグから1引く
            If c.Tag = ActExPic.Tag + 1 Then
                c.Tag = c.Tag - 1
                Exit For
            End If
        Next
        For Each c As ExPictureBox In myPicArClone '一個上の画像のタグから1引く(クローン用)
            If c.Tag = ActExPic.Tag + 1 Then
                c.Tag = c.Tag - 1
            ElseIf c.Tag = ActExPic.Tag Then
                c.Tag = c.Tag + 1
            End If
        Next
        For Each c As ExPictureBox In myPicArBackup '一個上の画像のタグから1引く(backup用)
            If c.Tag = ActExPic.Tag + 1 Then
                c.Tag = c.Tag - 1
            ElseIf c.Tag = ActExPic.Tag Then
                c.Tag = c.Tag + 1
            End If
        Next






        ActExPic.Tag = ActExPic.Tag + 1 '自分自身の画像のタグから1足す

        Call SortPic() 'すべての画像をタグに従って画像の重なりを並べ替え表示する

        ''コレクションを更新する、これを実行しないと実際のピクチャーボックスとコレクションの中身が一致しない
        'myPicAr.Clear()
        'For Each c As Control In Me.Panel2.Controls
        '    If c.GetType() = GetType(ExPictureBox) Then
        '        If c.Tag > 0 Then
        '            myPicAr.Add(c)
        '        End If
        '    End If
        'Next

        'Call TagSortCollection()
        '上の二つはまとめた↓
        Call ReConstructタグに従ってコレクションの再構成()


        '透過処理
        'Call Transparent2()
        Call Transparent4()

        '半リアルタイムにチェックが入っていたら透過する



        'If Me.CheckBoxTransparent.Checked Then
        '    'Call TransparentForMove() '動かした画像のみ再描画
        '    Call Transparent() 'すべての画像を再描画
        'End If

        Me.NumNowPic.Value = ActExPic.Tag
        Call UpdateThumbnail()

        ''範囲選択画像ならマーカーを非表示にする→これはないほうがいいかな
        'If ActExPic.Equals(selectPicbox範囲選択画像) Then
        '    Call RSLabel選択範囲画像ラベルの非表示()
        'End If
    End Sub

    Private Overloads Sub DelPic()

        Call DelPic(ActExPic)

    End Sub

    '選択中の画像を消去するメソッド
    Private Overloads Sub DelPic(EXP As ExPictureBox)
        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        '編集中のものがあれば終了、階層順番を入れ替えているのでこれが先
        Call CloseEdit編集終了()
        Call MouseEndDrawマウスで描画終了処理()

        '範囲選択画像ならポイントも消す、selectPicbox範囲選択画像も空にする
        If EXP.Name = "範囲選択_T" Then
            For Each lp As Label In LabelPoint範囲選択画像
                'Me.Controls.Remove(lp)
                Me.Panel2.Controls.Remove(lp)
            Next
            LabelPoint範囲選択画像.Clear()
            selectPicbox範囲選択画像 = Nothing

        End If

        '図形の編集中なら頂点のラベルも消去
        If LSサイズ変更ラベル.Count > 0 Then
            For Each l As Label In LSサイズ変更ラベル
                Me.Panel2.Controls.Remove(l)
                l.Dispose()
            Next
            LSサイズ変更ラベル.Clear()
        End If

        '図形の枠を表示していたら消去
        Call PicBoderLineLabel画像に付けた枠を消去()

        ZaoraruPic = DirectCast(myPicAr(EXP.Tag - 1), ExPictureBox)
        CloneBackup = DirectCast(myPicArClone(EXP.Tag - 1), ExPictureBox) '復活用
        BackupPicBox = DirectCast(myPicArBackup(EXP.Tag - 1), ExPictureBox) '復活用
        FlagZaoraru = True

        For i = 0 To myPicAr.Count - 1 '削除する画像をコレクションから削除
            If myPicAr(i).tag = EXP.Tag Then
                myPicAr.RemoveAt(i)
                Exit For
            End If
        Next

        For i = 0 To myPicArClone.Count - 1 '削除する画像をクローンのコレクションから削除
            If myPicArClone(i).tag = EXP.Tag Then
                myPicArClone.RemoveAt(i)
                Exit For
            End If
        Next

        For i = 0 To myPicArBackup.Count - 1 '削除する画像をBackupののコレクションから削除
            If myPicArBackup(i).tag = EXP.Tag Then
                myPicArBackup.RemoveAt(i)
                Exit For
            End If
        Next

        For Each c As ExPictureBox In myPicAr '削除した画像よりタグの大きい物は-1
            If c.Tag > EXP.Tag Then
                c.Tag = c.Tag - 1
            End If
        Next

        For Each c As ExPictureBox In myPicArClone '削除した画像よりタグの大きい物は-1(クローン用)
            If c.Tag > EXP.Tag Then
                c.Tag = c.Tag - 1
            End If
        Next
        myPicArCloneR = myPicArClone.Clone

        For Each c As ExPictureBox In myPicArBackup '削除した画像よりタグの大きい物は-1(Backup用)
            If c.Tag > EXP.Tag Then
                c.Tag = c.Tag - 1
            End If
        Next


        Me.Controls.Remove(EXP) '画像削除


        'FocusPic.Image.Dispose() '意味ないかも→画像の復活ができなくなるから使わない
        EXP.Dispose() '

        If myPicAr.Count <> 0 Then
            '削除した画像が一番下(タグの最大値)の画像ではないなら一個上だった画像をfocuspicにする
            If EXP.Tag <> myPicAr.Count + 1 Then
                For Each c As ExPictureBox In myPicAr
                    If c.Tag = EXP.Tag Then
                        EXP = c
                        Exit For
                    End If
                Next
            End If


            '透過処理
            '半リアルタイムにチェックが入っていたら透過する
            If Me.CheckBoxTransparent.Checked Then
                'Call TransparentForMove() '動かした画像のみ再描画
                Call Transparent() 'すべての画像を再描画
            End If

            '表示の更新
            Me.NumNowPic.Maximum = myPicAr.Count '最大値設定
            Me.TextBox1.Text = myPicAr.Count '画像数カウント表示
            'Me.CurrentPic.Image = FocusPic.Image 'サムネイル画像の更新、加工された後の画像
            'Me.CurrentPic.Image = DirectCast(myPicArBackup(FocusPic.Tag - 1), ExPictureBox).Image '加工される前の画像
            'Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(FocusPic.Tag - 1), ExPictureBox).Image '加工される前の画像
            Call UpdateThumbnail()
            Call MoveAfter()
            Call Transparent2()

        ElseIf myPicAr.Count = 0 Then
            Call UpdateThumbnail()

        End If

        If myPicAr.Count <> 0 Then
            ActExPic = EXP ' myPicAr(0)
        End If


    End Sub

    'Form3を閉じるときに範囲選択画像があれば消去する
    Friend Sub SelectRangeDel()
        For Each c As ExPictureBox In myPicAr

            If c.Name = "範囲選択_T" Then
                ActExPic = c
                Call DelPic()
                Exit For

            End If

        Next

    End Sub
    'タグに従ってコレクションの中を並べ替えるメソッド
    Private Sub TagSortCollection()

        'クローンのコレクションmyPicArCloneRの更新(並べ替えの準備)
        myPicArCloneR.Clear()
        For i As Integer = 1 To myPicArClone.Count
            For Each c As ExPictureBox In myPicArClone
                If c.Tag = i Then
                    myPicArCloneR.Add(c)
                    Exit For
                End If
            Next
        Next i
        'クローンのコレクションの更新(並べ替え完了)
        myPicArClone.Clear()
        myPicArClone = myPicArCloneR.Clone


        'Backupの並べ替え準備
        Dim myPicBackupR As New ArrayList
        For i As Integer = 1 To myPicArBackup.Count
            For Each c As ExPictureBox In myPicArBackup
                If c.Tag = i Then
                    myPicBackupR.Add(c)
                    Exit For
                End If
            Next
        Next i
        'Backupのコレクションの更新
        myPicArBackup.Clear()
        myPicArBackup = myPicBackupR.Clone

    End Sub


    '画像をタグに従って上下の順番を考慮して表示し直すメソッド
    Private Sub SortPic()
        Dim k As Integer
        For k = 1 To myPicAr.Count 'すべての画像をタグに従って画像の重なりを並べ替える
            For Each c As Control In myPicAr
                If c.Tag = k + 1 Then
                    c.SendToBack()
                    Exit For

                End If
            Next
        Next k

        '図形の編集中ならダミー画像を改めて最背面に移動
        If isDrawEditNow Then
            dummyExPicBox.SendToBack()
        End If

    End Sub

    '画像の保存前にコレクションの並び替えをするメソッド
    Private Sub SortForSave()
        Dim j As Integer
        myPicArR.Clear()
        myPicArCloneR.Clear()

        For j = myPicAr.Count To 1 Step -1
            For Each c As Control In myPicAr
                If c.Tag = j Then
                    myPicArR.Add(c)
                    Exit For

                End If
            Next
        Next

        For j = myPicArClone.Count To 1 Step -1
            For Each c As ExPictureBox In myPicArClone

                If c.Tag = j Then
                    myPicArCloneR.Add(c)
                    Exit For

                End If

            Next
        Next
        For i = 0 To myPicAr.Count - 1
            DirectCast(myPicArCloneR(i), ExPictureBox).Location = DirectCast(myPicArR(i), ExPictureBox).Location

        Next
    End Sub

    '保存用の画像の作成してピクチャーボックスを返す関数
    Private Function SavePicCreate() As ExPictureBox

        Call SortForSave() '保存用画像の並べ替え


        FullSize = RigthDownPoint()
        'Dim BitFull As Bitmap = New Bitmap(FullSize.X, FullSize.Y, System.Drawing.Imaging.PixelFormat.Alpha)
        Dim BitFull As Bitmap = New Bitmap(FullSize.X, FullSize.Y)
        BitFull.MakeTransparent()

        Dim FullPicBox As New ExPictureBox
        FullPicBox.Location = New Point(0, 0)
        FullPicBox.Image = BitFull

        'BitFull.MakeTransparent(Drawing.Color.Aqua)
        Dim FullGra As Graphics = Graphics.FromImage(FullPicBox.Image)
        ''FullGra.FillRectangle(Brushes.Aqua, FullGra.VisibleClipBounds)

        '背景色も保存するとき
        If Me.CheckBoxBackColor.Checked = False Then
            'BitFull.MakeTransparent(Me.myBackColor.BackColor)
            Dim myBrusheColor As Color = Me.Panel2.BackColor '背景色を取得
            'Dim myBrusheColor As Color = Me.myBackColor.BackColor '背景色を取得
            Dim myBrushe As New SolidBrush(myBrusheColor) '取得した背景色でブラシ作成
            FullGra.FillRectangle(myBrushe, FullGra.VisibleClipBounds) '作成したブラシで塗りつぶす
        End If

        For Each c As ExPictureBox In myPicArCloneR '画像配置して、保存用画像作成
            FullGra.DrawImage(c.Image, AbsolutePoint(c))
        Next
        'For Each c As ExPictureBox In myPicArR '画像配置して、保存用画像作成
        '    FullGra.DrawImage(c.Image, AbsolutePoint(c))
        'Next
        'Exif情報の書き込み
        Dim SoftName As String = "Pixtack" '書き込むソフトウェアネーム
        Dim Comment As String = "ゆっくりメテオさん★" 'コメント

        'Dim ResourceBmp As New Bitmap(My.Resources.ゆっくりメテオさん520x450_Exif付き) 'Exif情報を読み込む画像
        'My.Resources.ゆっくりメテオさん520x450_Exif付きではPropertyItemsは取得できない
        'VB Report No023
        'http://kilala.mydns.jp/def/VBTIPS/026/
        'ここの情報で何とかなった
        'ソリューションエクスプローラー→画像のプロパティ→ビルドアクションを埋め込まれたリソースに変更

        Dim Asm As System.Reflection.Assembly = System.Reflection.Assembly.GetExecutingAssembly()
        'Dim ResourceBmp As New Bitmap(Asm.GetManifestResourceStream("WindowsApplication4.ゆっくりメテオさん520x450_Exif付き.png")) 'Exif情報を読み込む画像
        Dim ResourceBmp As New Bitmap(Asm.GetManifestResourceStream("Pixtack.ゆっくりメテオさん520x450_Exif付き.png")) 'Exif情報を読み込む画像
        Dim iList() As Integer = ResourceBmp.PropertyIdList
        Dim iIndex As Integer = Array.IndexOf(iList, 305) '書き込むソフトウェアネーム用
        Dim iProp As System.Drawing.Imaging.PropertyItem = ResourceBmp.PropertyItems(iIndex)
        Dim iProp2 As System.Drawing.Imaging.PropertyItem = ResourceBmp.PropertyItems(0)

        'ソフトウェア名称書き込み
        iProp.Value = System.Text.Encoding.ASCII.GetBytes(SoftName)
        iProp.Len = iProp.Value.Length
        BitFull.SetPropertyItem(iProp)
        ''-------------------------ここからコメントを書き込む動作---------------------
        ''iProp2.Id = &H9286
        'iProp2.Id = 37510
        'iProp2.Type = 7

        ''Coment = New String(ControlChars.NullChar, 8) + Coment
        'iProp2.Value = System.Text.Encoding.GetEncoding("shift_jis").GetBytes(Comment)
        ''iProp2.Value = System.Text.Encoding.GetEncoding("UNICODE").GetBytes(Coment)
        'iProp2.Len = iProp2.Value.Length
        'BitFull.SetPropertyItem(iProp2)
        ''--------------------ここまでコメントを書き込む動作--------------------------


        Return FullPicBox
    End Function



    '追加画像の初期位置を返す関数
    Private Function AddPicPoint() As Point
        '画像を追加する初期座標の決定
        Dim APPoint As New Point(0, 0)
        'スクロールバーの移動位置を取得
        Dim xDiff As Integer = Math.Abs(Me.Panel2.AutoScrollPosition.X)
        Dim yDiff As Integer = Math.Abs(Me.Panel2.AutoScrollPosition.Y)
        'ラジオボタンのチェック
        If Me.CheckBoxDropPoint.Checked Then 'ドロップした位置に追加、Gridに合わせるようにした
            Dim Grid As Integer = Me.NumericUpDownGrid.Value
            APPoint = New Point(Me.Panel2.PointToClient(Cursor.Position))
            Dim x As Integer = APPoint.X - ((APPoint.X + xDiff) Mod Grid)
            Dim y As Integer = APPoint.Y - ((APPoint.Y + yDiff) Mod Grid)
            APPoint = New Point(x, y)

        ElseIf Me.CheckBoxSelectPicPoint.Checked Then '選択画像が基準の場合
            If myPicAr.Count = 0 Then
                APPoint = New Point(0, 0)
            Else
                Dim x As Integer = Me.ActExPic.Location.X
                Dim y As Integer = Me.ActExPic.Location.Y
                Dim xSlide As Integer = Me.NumericXslide.Value
                Dim ySlide As Integer = Me.NumericYslide.Value
                If Me.RadioButtonStack.Checked Then
                    APPoint = New Point(x + xSlide, y + ySlide)
                ElseIf Me.RadioButtonUnstack.Checked OrElse Me.RadioButtonAbsoluteUnstack.Checked Then
                    If Me.RadioButtonDown.Checked Then
                        APPoint = New Point(x, y + Me.ActExPic.Height)
                    ElseIf Me.RadioButtonRight.Checked Then
                        APPoint = New Point(x + Me.ActExPic.Width, y)
                    ElseIf Me.RadioButtonRightDown.Checked Then
                        APPoint = New Point(x + Me.ActExPic.Width, y + Me.ActExPic.Height)
                    End If
                End If

            End If
        ElseIf Me.RadioButtonDown.Checked Then
            APPoint = New Point(-xDiff, RigthDownPoint().Y - yDiff) '(0，Y座標の最大値)になるはず
        ElseIf Me.RadioButtonRight.Checked Then
            APPoint = New Point(RigthDownPoint().X - xDiff, -yDiff) '(X座標の最大値, 0)になるはず
        ElseIf Me.RadioButtonRightDown.Checked Then
            APPoint = New Point(RigthDownPoint().X - xDiff, RigthDownPoint().Y - yDiff) '(X, Y)の最大値になるはず

        End If



        Return APPoint
    End Function

    '透過色取得開始
    Private Sub TransparentPictureBox_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TransparentPictureBox.MouseDown
        If FlagColor Then
            FlagColor = False 'キャンセルの場合カーソルを戻す
            FlagGetColor = False
            Me.Cursor = Cursors.Default
        ElseIf FlagColor = False Then
            Me.Cursor = Cursors.Cross 'カーソルを十字型にする
            FlagColor = True
            FlagGetColor = True
        End If


    End Sub




    '背景色の設定の色のダイアログを表示する、背景色の設定
    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        If Me.ColorDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            'Me.myBackColor.BackColor = Me.ColorDialog1.Color
            Me.Panel2.BackColor = Me.ColorDialog1.Color
            Me.PictureBox1.BackColor = Me.ColorDialog1.Color
            Me.PictureBox2.BackColor = Me.ColorDialog1.Color
            Me.PictureBox3.BackColor = Me.ColorDialog1.Color
            myForm3.PictureBoxTextSample.BackColor = Me.ColorDialog1.Color
            myForm3.PictureBoxSquareSample.BackColor = Me.ColorDialog1.Color

        End If
    End Sub
    '背景色をリセット
    Private Sub 背景色をリセット_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonBackColorReset.Click
        'Dim bc As Color = Color.AliceBlue
        'bc = Color.FromArgb(2, 2, 2)
        Dim bc As Color = Color.FromKnownColor(KnownColor.Control)


        Me.Panel2.BackColor = bc
        Me.PictureBox1.BackColor = bc
        Me.PictureBox2.BackColor = bc
        Me.PictureBox3.BackColor = bc
        myForm3.PictureBoxTextSample.BackColor = bc
        myForm3.PictureBoxSquareSample.BackColor = bc

    End Sub


    'サムネイル画像更新メソッド、UpperとLowerのサムネイル画像だけ
    Private Sub UpdateThumbnail() '画像のZオーダー変更、削除、追加の時に使う


        If myPicAr.Count = 0 Then '1枚も画像がないとき
            Me.PictureBoxUpper.Image = Nothing 'Current画像が一番上なら上の画像のサムネイルは空白に
            Me.PictureBoxLower.Image = Nothing 'Current画像が一番下の時のサムネイル画像更新
            Me.CurrentPic.Image = Nothing
            Me.PictureBoxBackup.Image = Nothing



        ElseIf myPicAr.Count = 1 Then

            Me.PictureBoxUpper.Image = Nothing 'Current画像が一番上なら上の画像のサムネイルは空白に
            Me.PictureBoxLower.Image = Nothing 'Current画像が一番下の時のサムネイル画像更新
            'Me.CurrentPic.Image = FocusPic.Image
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
            'Me.PictureBoxBackup.Image = backupPic.Image
            '2枚以上あって最下層の時
        ElseIf myPicAr.Count >= 2 And ActExPic.Tag = myPicAr.Count Then

            Me.PictureBoxLower.Image = Nothing 'Current画像が一番下の時のサムネイル画像更新
            'Me.PictureBoxUpper.Image = myPicAr(FocusPic.Tag - 2).image'加工された画像
            'Me.PictureBoxUpper.Image = myPicArBackup(FocusPic.Tag - 2).image '加工される前の画像
            Me.PictureBoxUpper.Image = DirectCast(myPicArClone(ActExPic.Tag - 2), ExPictureBox).Image '加工された後の画像
            'Me.CurrentPic.Image = FocusPic.Image
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
            '斜めのサムネ実験
            'Dim UImg As ExPictureBox = myPicAr(FocusPic.Tag - 2)
            'Dim bmp As New Bitmap(UImg.Width, UImg.Height)
            'Dim g As Graphics = Graphics.FromImage(bmp)
            'Dim img As Image = UImg.Image
            'Dim dPoint() As Point
            'dPoint = New Point() {New Point(0, 10), New Point(256 - 30, 10), New Point(30, 224 - 10)}
            'g.DrawImage(img, dPoint)
            'Me.PictureBoxUpper.Image = bmp

            '2枚以上あって最上層のとき
        ElseIf myPicAr.Count >= 2 And ActExPic.Tag = 1 Then

            Me.PictureBoxUpper.Image = Nothing 'Current画像が一番上なら上の画像のサムネイルは空白に
            'Me.PictureBoxLower.Image = myPicAr(FocusPic.Tag).image
            Me.PictureBoxLower.Image = myPicArBackup(ActExPic.Tag).image
            'Me.CurrentPic.Image = FocusPic.Image
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
            'それ以外のとき
        Else 'If FocusPic.Tag = myPicAr.Count And myPicAr.Count > 1 Then

            'Me.PictureBoxUpper.Image = myPicArBackup(FocusPic.Tag - 2).image 'Currentの上の画像のサムネイル更新
            Me.PictureBoxUpper.Image = DirectCast(myPicArClone(ActExPic.Tag - 2), ExPictureBox).Image '加工された後の画像
            'Me.PictureBoxLower.Image = myPicArBackup(FocusPic.Tag).image
            Me.PictureBoxLower.Image = DirectCast(myPicArClone(ActExPic.Tag), ExPictureBox).Image '加工された後の画像
            'Me.CurrentPic.Image = FocusPic.Image
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
        End If
    End Sub

    '選択画像のサムネだけ更新
    Friend Sub UpdateThumbnailOne()

        'If myPicAr.Count = 0 Then
        '    Exit Sub

        'End If
        'Dim i As Integer = FocusPic.Tag - 1
        'Dim img As Image = DirectCast(myPicAr(i), ExPictureBox).Image
        'Dim backupImg As Image = DirectCast(myPicArBackup(i), ExPictureBox).Image

        'Me.CurrentPic.Image = img
        'Me.PictureBoxBackup.Image = backupImg

    End Sub

    '斜めのサムネの実験サムネイル画像更新メソッド、UpperとLowerのサムネイル画像だけ
    Private Sub UpdateThumbnail2() '画像のZオーダー変更、削除、追加の時に使う
        ''Dim UDPoint() As Point
        ''UDPoint = New Point() {New Point(70, 0), New Point(256, 0)}
        'Dim NanameValue As Double = 0.3 '傾き加減


        'If myPicAr.Count = 0 Then '1枚も画像がないとき
        '    Me.PictureBoxU.Image = Nothing 'Current画像が一番上なら上の画像のサムネイルは空白に
        '    Me.PictureBoxL.Image = Nothing 'Current画像が一番下の時のサムネイル画像更新
        '    Me.PictureBoxM.Image = Nothing
        'ElseIf myPicAr.Count = 1 Then
        '    Me.PictureBoxU.Image = Nothing 'Current画像が一番上なら上の画像のサムネイルは空白に
        '    Me.PictureBoxL.Image = Nothing 'Current画像が一番下の時のサムネイル画像更新
        '    '2枚以上あって最下層の時
        'ElseIf myPicAr.Count >= 2 And FocusPic.Tag = myPicAr.Count Then
        '    Me.PictureBoxL.Image = Nothing 'Current画像が一番下の時のサムネイル画像更新
        '    'Me.PictureBoxUpper.Image = myPicAr(FocusPic.Tag - 2).image
        '    Dim UImg As ExPictureBox = myPicAr(FocusPic.Tag - 2)
        '    Dim katamuki As Integer = UImg.Width * NanameValue
        '    Dim bmp As New Bitmap(UImg.Width, UImg.Height)
        '    Dim g As Graphics = Graphics.FromImage(bmp)
        '    Dim img As Image = UImg.Image
        '    Dim dPoint() As Point
        '    dPoint = New Point() {New Point(katamuki, 0), New Point(UImg.Width, 0), New Point(0, UImg.Height)}
        '    g.DrawImage(img, dPoint)
        '    Me.PictureBoxU.Image = bmp
        '    '2枚以上あって最上層のとき
        'ElseIf myPicAr.Count >= 2 And FocusPic.Tag = 1 Then
        '    Me.PictureBoxU.Image = Nothing 'Current画像が一番上なら上の画像のサムネイルは空白に
        '    Me.PictureBoxL.Image = myPicAr(FocusPic.Tag).image
        '    Dim LImg As ExPictureBox = myPicAr(FocusPic.Tag)
        '    Dim katamuki As Integer = LImg.Width * NanameValue
        '    Dim bmp As New Bitmap(LImg.Width, LImg.Height)
        '    Dim g As Graphics = Graphics.FromImage(bmp)
        '    Dim img As Image = LImg.Image
        '    Dim dPoint() As Point
        '    dPoint = New Point() {New Point(katamuki, 0), New Point(LImg.Width, 0), New Point(0, LImg.Height)}
        '    g.DrawImage(img, dPoint)
        '    Me.PictureBoxL.Image = bmp
        '    'それ以外のとき
        'Else 'If FocusPic.Tag = myPicAr.Count And myPicAr.Count > 1 Then
        '    'Me.PictureBoxU.Image = myPicAr(FocusPic.Tag - 2).image 'Currentの上の画像のサムネイル更新
        '    'Me.PictureBoxL.Image = myPicAr(FocusPic.Tag).image
        '    Dim UImg As ExPictureBox = myPicAr(FocusPic.Tag - 2)
        '    Dim bmp As New Bitmap(UImg.Width, UImg.Height)
        '    Dim katamuki As Integer = UImg.Width * NanameValue
        '    Dim g As Graphics = Graphics.FromImage(bmp)
        '    Dim img As Image = UImg.Image
        '    Dim dPoint() As Point
        '    dPoint = New Point() {New Point(katamuki, 0), New Point(UImg.Width, 0), New Point(0, UImg.Height)}
        '    g.DrawImage(img, dPoint)
        '    Me.PictureBoxU.Image = bmp


        '    Me.PictureBoxL.Image = myPicAr(FocusPic.Tag).image
        '    Dim LImg As ExPictureBox = myPicAr(FocusPic.Tag)
        '    Dim katamukiL As Integer = LImg.Width * NanameValue
        '    Dim Lbmp As New Bitmap(LImg.Width, LImg.Height)
        '    Dim gL As Graphics = Graphics.FromImage(Lbmp)
        '    Dim imgL As Image = LImg.Image
        '    Dim dPointL() As Point
        '    dPointL = New Point() {New Point(katamukiL, 0), New Point(LImg.Width, 0), New Point(0, LImg.Height)}
        '    gL.DrawImage(imgL, dPointL)
        '    Me.PictureBoxL.Image = Lbmp
        'End If
    End Sub
    'サムネイル画像の横の数字を変えるとサムネイル画像も変わる(旧式)
    'Private Sub NumNowPic_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumNowPic.ValueChanged
    '    For Each c As ExPictureBox In myPicAr
    '        If Me.NumNowPic.Value = c.Tag Then
    '            'NowControl = c
    '            FocusPic = c
    '            Me.CurrentPic.Image = c.Image 'サムネイル画像の表示

    '            'Dim katamuki As Integer = c.Width * 0.3
    '            'Dim Img As ExPictureBox = c
    '            'Dim bmp As New Bitmap(c.Width, c.Height)
    '            'Dim g As Graphics = Graphics.FromImage(bmp)
    '            'Dim gimg As Image = c.Image
    '            'Dim dPoint() As Point
    '            'dPoint = New Point() {New Point(katamuki, 0), New Point(c.Width, 0), New Point(0, c.Height)}
    '            'g.DrawImage(gimg, dPoint)
    '            'Me.PictureBoxM.Image = bmp

    '            Call ChangeFocusT() 'ステータス表示更新
    '            Call UpdateThumbnail() 'サブサムネイル画像の更新
    '            'Call UpdateThumbnail2() '実験

    '            FocusPic.Focus() 'これで一旦ピクチャーボックスにフォーカスしないとすっ飛んでいくことがある、
    '           
    '            Exit For

    '        End If
    '    Next
    'End Sub

    'サムネイル画像の横の数字を変えるとサムネイル画像も変わる(改)
    Private Sub NumNowPic_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumNowPic.ValueChanged
        If myPicAr.Count <> 0 Then
            isLayerChange = True '編集中にレイヤー変更の有無判定用
            Dim i As Integer = Me.NumNowPic.Value - 1
            ActExPic = DirectCast(myPicAr(i), ExPictureBox)
            Me.CurrentPic.Image = DirectCast(myPicArClone(i), ExPictureBox).Image
            Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
            'Call ChangeFocusTステータス表示更新()’これが余計だった
            Call UpdateThumbnail()
            'FocusPic.Focus() 
            'Me.ListBox1.Focus()
        End If
    End Sub
    'マウスホイールで数字を1づつ動かそうとしたけど5づつ動く
    '下記で調整してもうまく動かんので諦め
    Private Sub NumNowPic_MouseWheel(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles NumNowPic.MouseWheel
        'Debug.Print(e.Delta.ToString) '120と-120が上下でそれぞれ返ってくる

        'If e.Delta > 0 Then
        '    If Me.NumNowPic.Value = Me.NumNowPic.Maximum Then
        '        Return
        '    End If
        '    Me.NumNowPic.Value = Me.NumNowPic.Value + e.Delta / 120
        'End If

        'If e.Delta < 0 Then
        '    If Me.NumNowPic.Value = 1 Then
        '        Return
        '    End If
        '    Me.NumNowPic.Value = Me.NumNowPic.Value + e.Delta / 120
        'End If


        'If e.Delta > 0 Then
        '    If Me.NumNowPic.Value = Me.NumNowPic.Maximum Then
        '        Return
        '    End If
        '    Me.NumNowPic.Value = Me.NumNowPic.Value + 1
        'End If

        'If e.Delta < 0 Then
        '    If Me.NumNowPic.Value = 1 Then
        '        Return
        '    End If
        '    Me.NumNowPic.Value = Me.NumNowPic.Value - 1
        'End If

    End Sub

    Private Sub Grid4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Grid4.Click
        If Me.NumericUpDownGrid.Value >= Me.NumericUpDownGrid.Minimum + 2 Then
            Me.NumericUpDownGrid.Value = Me.NumericUpDownGrid.Value - 2
        End If
    End Sub

    Private Sub Grid8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Grid8.Click
        Me.NumericUpDownGrid.Value = 8

    End Sub

    Private Sub Grid16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Grid16.Click
        If Me.NumericUpDownGrid.Value + 8 <= Me.NumericUpDownGrid.Maximum Then
            Me.NumericUpDownGrid.Value = Me.NumericUpDownGrid.Value + 8
        End If
    End Sub

    Private Sub Grid32_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Grid32.Click
        If Me.NumericUpDownGrid.Value >= Me.NumericUpDownGrid.Minimum + 8 Then
            Me.NumericUpDownGrid.Value = Me.NumericUpDownGrid.Value - 8
        End If
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Me.NumericUpDownGrid.Value = 1
    End Sub

    'サムネイル画像をクリックしたときその画像にフォーカスを一瞬移して画面中央に表示する
    Private Sub CurrentPic_MouseUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles CurrentPic.MouseUp
        If myPicAr.Count > 0 Then
            If isDrawEditNow Then '編集中の画像があれば編集終了
                Call CloseEdit編集終了()
            End If
            If isMouseDeDrawNow Then
                Call MouseEndDrawマウスで描画終了処理()
            End If

            Call PicBoderLineLabel画像に枠を作成表示(ActExPic) '枠の表示

            ActExPic.Focus() 'あまり使いたくない、右下にすっ飛んでいく
            'Me.Select() 'これがうまく動かない、フォーカスをExPictureから外したい
            Me.ListBox1.Select() 'フォーカス外しはこれでいい
            'Me.ListBox1.Focus()
            Me.TextBox2.Text = ActExPic.Name
            Call ChangeFocusTステータス表示更新()
        End If
    End Sub
    '中央のサムネの上でマウスホイールを動かしたとき
    Private Sub CurrentPic_MouseWheel(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles CurrentPic.MouseWheel
        ''その画像の上下の順番を入れ替えるバージョン
        'If e.Delta > 0 Then
        '    Call FocusPictureUp()
        'End If
        'If e.Delta < 0 Then
        '    Call FocusPictureDown()
        'End If
        If myPicAr.Count = 0 Or myPicAr.Count = 1 Then
            'If Me.Panel2.Controls.Count = 0 Or Me.Panel2.Controls.Count = 1 Then
            Exit Sub
        End If

        '図形の編集中なら終了させる
        Call CloseEdit編集終了()
        Call MouseEndDrawマウスで描画終了処理()


        '表示の切替バージョン
        If e.Delta > 0 And NumNowPic.Value <> 1 Then
            NumNowPic.Value = ActExPic.Tag - 1
        ElseIf e.Delta < 0 And NumNowPic.Value < NumNowPic.Maximum Then
            NumNowPic.Value = ActExPic.Tag + 1
        End If
    End Sub



    '上層サムネをクリックしたとき
    Private Sub PictureBoxUpper_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBoxUpper.MouseDown
        If myPicAr.Count = 0 OrElse Me.NumNowPic.Value = 1 Then
            'If Me.Panel2.Controls.Count = 0 OrElse Me.NumNowPic.Value = 1 Then
            Exit Sub
        End If
        '図形の編集中なら終了させる
        Call CloseEdit編集終了()
        Call MouseEndDrawマウスで描画終了処理()

        If myPicAr.Count > 1 Then
            Me.NumNowPic.Value = Me.NumNowPic.Value - 1

        End If

    End Sub

    '下層サムネをクリックしたとき
    Private Sub PictureBoxLower_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBoxLower.MouseDown
        If myPicAr.Count = 0 Then
            Exit Sub
        End If
        Call CloseEdit編集終了() '図形の編集中なら終了させる
        Call MouseEndDrawマウスで描画終了処理()

        If myPicAr.Count > 1 And ActExPic.Tag <> myPicAr.Count Then
            Me.NumNowPic.Value = Me.NumNowPic.Value + 1
            'Call ChangeFocusT()
        End If


    End Sub

    'クリップボードからの画像追加-----------ここから-------------
    Private Sub ButtonClipBitmap_Click(sender As System.Object, e As System.EventArgs) Handles ButtonClipBitmap.Click
        Call CloseEdit編集終了() '編集中のものがあれば編集終了する
        Call MouseEndDrawマウスで描画終了処理()

        If Me.CheckBoxDropPoint.Checked Then
            MsgBox("「ドロップした位置」のチェックを外してから実行してください")
            Exit Sub
        End If

        Dim Data1 As IDataObject = Clipboard.GetDataObject()
        If Data1.GetDataPresent("Bitmap") Then
            Dim img As Image = DirectCast(Data1.GetData("Bitmap"), Image)
            Call クリップボードからの画像追加(img)
        Else
            MsgBox("クリップボードにBitmap形式の画像はありませんでした")
            Exit Sub
        End If

        'クリップボードのデータの形式を取得する: .NET(Tips) : C#, VB.NET
        'http://dobon.net/vb/dotnet/system/clipboardformats.html

        ''クリップボードのデータ形式をすべて列挙
        'If Not Data1 Is Nothing Then
        '    For Each fmt As String In Data1.GetFormats
        '        Console.WriteLine(fmt)
        '    Next
        'End If


        'Dim img As Image
        'Dim ms As System.IO.MemoryStream
        ''ms = DirectCast(Clipboard.GetData("Office Drawing Shape Format"), System.IO.MemoryStream)
        'ms = DirectCast(Clipboard.GetData("PNG+Office Art"), IO.MemoryStream)
        'ms = DirectCast(Data1.GetData("MetaFilePict"), IO.MemoryStream)
        ''ms = DirectCast(Data1.GetData("Office Drawing Shape Format"), IO.MemoryStream)
        'img = Image.FromStream(ms)
        'Call クリップボードからの画像追加(img)
        'Dim pi() As System.Drawing.Imaging.PropertyItem = img.PropertyItems


    End Sub
    Private Sub ButtonClipMeta_Click(sender As System.Object, e As System.EventArgs) Handles ButtonClipMeta.Click
        Call CloseEdit編集終了() '編集中のものがあれば編集終了する
        If Me.CheckBoxDropPoint.Checked Then
            MsgBox("「ドロップした位置」のチェックを外してから実行してください")
            Exit Sub
        End If

        Dim ClipImage As Image
        ClipImage = GetEnhMetafileOnClipboard(Me.Handle)
        If ClipImage Is Nothing Then
            MsgBox("クリップボードにメタファイル画像はありませんでした")
            Exit Sub
        End If
        Call クリップボードからの画像追加(ClipImage)

    End Sub
    ''' <summary>
    ''' クリップボードのメタファイル形式のデータを取得する
    ''' </summary>
    ''' <param name="hWnd">ウィンドウのハンドル</param>
    ''' <returns>取得したデータ</returns>
    Public Shared Function GetEnhMetafileOnClipboard(ByVal hWnd As IntPtr) As System.Drawing.Imaging.Metafile
        Dim meta As System.Drawing.Imaging.Metafile = Nothing
        If OpenClipboard(hWnd) Then
            Try
                If IsClipboardFormatAvailable(CF_ENHMETAFILE) <> 0 Then
                    Dim hmeta As IntPtr = GetClipboardData(CF_ENHMETAFILE)
                    meta = New System.Drawing.Imaging.Metafile(hmeta, True)
                End If
            Finally
                CloseClipboard()
                'MsgBox("クリップボードにメタファイル画像はありませんでした")
            End Try
        End If

        Return meta
    End Function
    Private Sub ButtonClipB_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonClipB.Click
        Call AddPicFromClipBクリップボードから追加()

        ''        Microsoft(Officeでコピーした図をExPictureBoxに表示する) : .NET(Tips) : C#, VB.NET
        ''http://dobon.net/vb/dotnet/graphics/getclipboardmetafile.html

        'Call CloseEdit編集終了() '編集中のものがあれば編集終了する
        'Call MouseEndDrawマウスで描画終了処理()

        'If Me.CheckBoxDropPoint.Checked Then
        '    MsgBox("「ドロップした位置」のチェックを外してから実行してください")
        '    Exit Sub
        'End If

        'Try
        '    Dim data As IDataObject = Clipboard.GetDataObject()
        '    Dim ms As System.IO.MemoryStream = DirectCast(data.GetData("PNG"), System.IO.MemoryStream)
        '    Dim ClipImage As Image


        '    If Not ms Is Nothing Then 'エクセルのクリップボードから画像として取り出せたら
        '        ClipImage = Image.FromStream(ms)

        '    ElseIf Clipboard.ContainsImage Then 'クリップボードの中が画像なら

        '        ClipImage = Clipboard.GetImage
        '        'Dim ClipImage As Image = Clipboard.GetImage
        '    Else

        '        MsgBox("クリップボードに画像はありませんでした")
        '        Exit Sub
        '    End If

        '    Call クリップボードからの画像追加(ClipImage)


        'Catch ex As Exception

        'End Try
    End Sub
    Private Sub AddPicFromClipBクリップボードから追加()
        '        Microsoft(Officeでコピーした図をExPictureBoxに表示する) : .NET(Tips) : C#, VB.NET
        'http://dobon.net/vb/dotnet/graphics/getclipboardmetafile.html

        Call CloseEdit編集終了() '編集中のものがあれば編集終了する
        Call MouseEndDrawマウスで描画終了処理()

        If Me.CheckBoxDropPoint.Checked Then
            MsgBox("「ドロップした位置」のチェックを外してから実行してください")
            Exit Sub
        End If

        Try
            Dim data As IDataObject = Clipboard.GetDataObject()
            Dim ms As System.IO.MemoryStream = DirectCast(data.GetData("PNG"), System.IO.MemoryStream)
            Dim ClipImage As Image


            If Not ms Is Nothing Then 'エクセルのクリップボードから画像として取り出せたら
                ClipImage = Image.FromStream(ms)

            ElseIf Clipboard.ContainsImage Then 'クリップボードの中が画像なら

                ClipImage = Clipboard.GetImage
                'Dim ClipImage As Image = Clipboard.GetImage
            Else

                MsgBox("クリップボードに画像はありませんでした")
                Exit Sub
            End If

            Call クリップボードからの画像追加(ClipImage)


        Catch ex As Exception

        End Try
    End Sub
    'クリップボードから画像の追加
    Private Sub クリップボードからの画像追加(ClipImage As Image)
        Call CloseEdit編集終了() '編集中のものがあれば編集終了
        Call MouseEndDrawマウスで描画終了処理()

        Try

            tagCount = myPicAr.Count 'タグの初期値にピクチャーボックスの総数を入れる
            Dim myBmp As New Bitmap(ClipImage)
            Dim myPic As Image = myBmp
            Dim myPicBox As New ExPictureBox
            'myPicBox.Name = "myPic1"
            myPicBox.SizeMode = PictureBoxSizeMode.AutoSize '必須

            '画像の初期位置
            myPicBox.Location = AddPicPoint()

            myPicBox.Image = myPic '画像をピクチャーボックスに割り当て

            'タグの管理
            If Me.RadioButtonLower.Checked Then
                tagCount = tagCount + 1
            ElseIf Me.RadioButtonUpper.Checked Then
                For Each c As ExPictureBox In myPicAr
                    c.Tag = c.Tag + 1
                Next
                'クローン用
                For Each c As ExPictureBox In myPicArClone
                    c.Tag = c.Tag + 1
                Next
                'バックアップ用
                For Each c As ExPictureBox In myPicArBackup
                    c.Tag = c.Tag + 1
                Next

                tagCount = 1
            End If


            myPicBox.Tag = tagCount
            ClipCount = ClipCount + 1
            myPicBox.Name = "fromClipImage_" & ClipCount
            'ファイル名を取得してピクチャーボックスの名前にする

            '右クリックメニュー追加、2014/12/13
            myPicBox.ContextMenuStrip = Me.ContextMenuStrip1


            'myPicAr.Add(myPicBox)
            '完成したピクチャーボックスをコレクションに追加
            If Me.RadioButtonLower.Checked Then
                myPicAr.Add(myPicBox)
            ElseIf Me.RadioButtonUpper.Checked Then
                myPicAr.Insert(0, myPicBox)
            End If



            'クローンのピクチャーボックスをクローン用のコレクションに追加
            Dim myPicBox2 As New ExPictureBox
            With myPicBox2
                .Location = myPicBox.Location
                .SizeMode = myPicBox.SizeMode
                .Name = "クローン_fromClipImage_" & tagCount
                .Image = myPicBox.Image
                .Tag = myPicBox.Tag
            End With
            'クローン用のコレクションに追加
            If Me.RadioButtonLower.Checked Then
                myPicArClone.Add(myPicBox2)
            ElseIf Me.RadioButtonUpper.Checked Then
                myPicArClone.Insert(0, myPicBox2)
            End If

            'バックアップ用のピクチャーボックス作成
            Dim myPicBackup As New ExPictureBox
            With myPicBackup
                .Location = myPicBox.Location
                .SizeMode = PictureBoxSizeMode.Normal
                .Name = "fromClipImage_" & ClipCount
                .Image = myPicBox.Image
                .Tag = myPicBox.Tag
            End With
            'バックアップ用のコレクションに追加
            If Me.RadioButtonLower.Checked Then
                myPicArBackup.Add(myPicBackup)
            ElseIf Me.RadioButtonUpper.Checked Then
                myPicArBackup.Insert(0, myPicBackup)
            End If


            'myPicArClone.Add(myPicBox)

            Me.Panel2.Controls.Add(myPicBox) 'ピクチャーボックスを表示
            'イベントに関連付ける
            AddHandler myPicBox.MouseEnter, AddressOf PictureBox1_MouseEnter
            AddHandler myPicBox.MouseUp, AddressOf PictureBox1_MouseUp
            AddHandler myPicBox.MouseLeave, AddressOf PictureBox1_MouseLeave
            AddHandler myPicBox.MouseMove, AddressOf PictureBox1_MouseMove '大幅変更箇所
            AddHandler myPicBox.MouseDown, AddressOf PictureBox1_MouseDown '大幅変更箇所
            Call SortPic()

            Me.TextBox1.Text = myPicAr.Count
            FullSize = RigthDownPoint()
            Me.TextBox9.Text = FullSize.ToString
            Me.NumNowPic.Maximum = myPicAr.Count '最大値設定
            Me.NumNowPic.Value = NumNowPic.Maximum

            For Each c As ExPictureBox In myPicAr '追加した画像をnowcontrolとfocuspicにする
                If c.Tag = tagCount Then
                    'NowControl = c
                    ActExPic = c
                End If
            Next
            'NowControl.Focus()
            'ActExPic.Focus()
            'Me.ListBox1.Focus()
            'Me.CurrentPic.Image = FocusPic.Image
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image

            Call ChangeFocusTステータス表示更新()
            Call MoveAfter()
            Call Transparent2()
        Catch ex As Exception

        End Try
    End Sub



    '全画像消去ボタン
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonAllDel.Click
        '範囲選択画像用
        If LabelPoint範囲選択画像.Count <> 0 Then
            For Each lp As Label In LabelPoint範囲選択画像
                Me.Panel2.Controls.Remove(lp)
            Next
            LabelPoint範囲選択画像.Clear()
        End If


        For Each c As ExPictureBox In myPicAr
            Panel2.Controls.Remove(c)
            c.Dispose()
        Next
        myPicAr.Clear()
        myPicArR.Clear()
        myPicArClone.Clear()
        myPicArCloneR.Clear()
        myPicArBackup.Clear()

        'Me.NumNowPic.Maximum = Me.Panel2.Controls.Count '最大値設定
        Me.TextBox1.Text = myPicAr.Count '画像数カウント表示
        Call UpdateThumbnail()

        Call CloseEdit編集終了()
        Call MouseEndDrawマウスで描画終了処理()

    End Sub

    '重絶対否のラジオボタンをチェックしたらスライドのラジオボタンを無効にする
    Private Sub RadioButtonAbsoluteUnstack_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButtonAbsoluteUnstack.CheckedChanged
        If Me.RadioButtonAbsoluteUnstack.Checked Then
            Me.NumericXslide.Enabled = False
            Me.NumericYslide.Enabled = False
        ElseIf Me.RadioButtonAbsoluteUnstack.Checked = False Then
            Me.NumericXslide.Enabled = True
            Me.NumericYslide.Enabled = True
        End If

    End Sub

    'ドロップした位置に追加のチェックボックス
    'チェックしたらいくつかのラジオボタンを無効にする、外れたら有効にする
    Private Sub CheckBoxDragPoint_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxDropPoint.CheckedChanged

        If Me.CheckBoxDropPoint.Checked Then
            'Me.RadioButtonDown.Enabled = False
            'Me.RadioButtonRight.Enabled = False
            'Me.RadioButtonRightDown.Enabled = False
            'Me.RadioButtonStack.Enabled = False
            'Me.RadioButtonUnstack.Enabled = False
            'Me.RadioButtonAbsoluteUnstack.Enabled = False
            Me.CheckBoxSelectPicPoint.Enabled = False
        ElseIf Me.CheckBoxDropPoint.Checked = False Then
            'Me.RadioButtonDown.Enabled = True
            'Me.RadioButtonRight.Enabled = True
            'Me.RadioButtonRightDown.Enabled = True
            'Me.RadioButtonStack.Enabled = True
            'Me.RadioButtonUnstack.Enabled = True
            'Me.RadioButtonAbsoluteUnstack.Enabled = True
            Me.CheckBoxSelectPicPoint.Enabled = True
        End If

    End Sub
    Private Sub CheckBoxSelectPicPoint_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxSelectPicPoint.CheckedChanged
        If Me.CheckBoxSelectPicPoint.Checked Then
            'Me.RadioButtonDown.Enabled = False
            'Me.RadioButtonRight.Enabled = False
            'Me.RadioButtonRightDown.Enabled = False
            'Me.RadioButtonStack.Enabled = False
            'Me.RadioButtonUnstack.Enabled = False
            'Me.RadioButtonAbsoluteUnstack.Enabled = False
            Me.CheckBoxDropPoint.Enabled = False
        ElseIf Me.CheckBoxSelectPicPoint.Checked = False Then
            'Me.RadioButtonDown.Enabled = True
            'Me.RadioButtonRight.Enabled = True
            'Me.RadioButtonRightDown.Enabled = True
            'Me.RadioButtonStack.Enabled = True
            'Me.RadioButtonUnstack.Enabled = True
            'Me.RadioButtonAbsoluteUnstack.Enabled = True
            Me.CheckBoxDropPoint.Enabled = True
        End If
    End Sub

    '保存ボタン押したとき
    'SavePicというメソッドも作ったけどこれはこれでそのまま
    Private Sub ButtonSavePng_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonSavePng.Click
        If myPicAr.Count = 0 Then '画像が一個もなければ何もしない
            Return
        End If


        If SaveFile.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Dim SavePic As ExPictureBox = SavePicCreate() '保存用の画像の作成
            ''画像フォーマットを選んで保存
            'If SaveFile.FilterIndex = 1 Then 'フィルタインデックスが1ならpng形式で保存
            '    SavePic.Image.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Png)

            '    ''pngもjpegのようにパラメータを渡せば反映されるとおもいきや無視される
            '    'Dim JQuality As Long = Me.NumericUpDownJpeg.Value
            '    'Dim encParam As New EncoderParameter(Encoder.ColorDepth, Convert.ToByte(24))
            '    'Debug.Print(Convert.ToByte(24).ToString)
            '    ''Dim encParam As New EncoderParameter(Encoder.Compression, JQuality)
            '    'Dim encParams As New EncoderParameters(1)
            '    'encParams.Param(0) = encParam
            '    'SavePic.Image.Save(SaveFile.FileName, GetEncoderInfo("image/png"), encParams)

            'ElseIf SaveFile.FilterIndex = 2 Then 'Jpeg
            '    'Imports System.Drawing.Imagingをクラスの外側に付けないとうまくいかない
            '    '品質を指定してJPEG画像を保存するには？ － ＠IT
            '    'http://www.atmarkit.co.jp/fdotnet/dotnettips/533jpgquality/jpgquality.html
            '    'Visual Basic .NET を使用して、JPEG 品質要因を決定します。
            '    'http://support.microsoft.com/kb/324788/ja

            '    Dim eps As EncoderParameters = New EncoderParameters(1) '1は格納できるオブジェクトの数らしい…

            '    Dim JQuality As Long = Me.NumericUpDownJpeg.Value
            '    Dim encParam As New EncoderParameter(Encoder.Quality, JQuality)
            '    Dim encParams As New EncoderParameters(1)
            '    encParams.Param(0) = encParam
            '    'encParams.Param(1) = New EncoderParameter(Encoder.ColorDepth, Convert.ToByte(8))

            '    'eps.Param(0) = New EncoderParameters(Encoder.Quality, JQuality)
            '    SavePic.Image.Save(SaveFile.FileName, GetEncoderInfo("image/jpeg"), encParams)


            '    'SavePic.Image.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Jpeg)
            'ElseIf SaveFile.FilterIndex = 3 Then 'Bitmap
            '    SavePic.Image.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Bmp)
            'ElseIf SaveFile.FilterIndex = 4 Then
            '    SavePic.Image.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Gif)
            'End If



            Dim bmp As New Bitmap(SavePic.Image)
            Call PicToFileSave(bmp, SaveFile.FilterIndex)
            bmp.Dispose()
            SavePic.Dispose()
        End If

    End Sub

    '選択範囲の保存
    Friend Sub SelectRangeSave()
        '画像がない時と範囲選択画像が一番下の時はなにもしないで終了
        If myPicAr.Count < 2 OrElse selectPicbox範囲選択画像.Tag = myPicAr.Count Then
            Exit Sub
        End If

        For Each c As ExPictureBox In myPicAr

            If c.Name = "範囲選択_T" Then
                Dim bmp As New Bitmap(SelectRangeBitmapAdd(c))

                If SaveFile.ShowDialog() = Windows.Forms.DialogResult.OK Then

                    Call PicToFileSave(bmp, SaveFile.FilterIndex)
                    bmp.Dispose()

                End If


                'バックアップ画像には半透明画像があるのでこれを利用して表示用画像に戻す
                Dim img As Image = DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image
                'Dim img As Image = DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image
                c.Image = img
                DirectCast(myPicAr(c.Tag - 1), ExPictureBox).Image = img
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = img
                Call Transparent2()
                'img.Dispose()'これはエラーになる

                Call UpdateThumbnail()

                Exit Sub


            End If

        Next


    End Sub
    '画像の保存
    Friend Sub SavePic()
        If myPicAr.Count = 0 Then '画像が一個もなければ何もしない
            Return
        End If

        If SaveFile.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Dim SavePic As ExPictureBox = SavePicCreate() '保存用の画像の作成
            ''画像フォーマットを選んで保存
            'If SaveFile.FilterIndex = 1 Then 'フィルタインデックスが1ならpng形式で保存
            '    SavePic.Image.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Png)

            'ElseIf SaveFile.FilterIndex = 2 Then 'Jpeg
            '    Dim eps As EncoderParameters = New EncoderParameters(1) '1は格納できるオブジェクトの数らしい…
            '    Dim JQuality As Long = Me.NumericUpDownJpeg.Value
            '    Dim encParam As New EncoderParameter(Encoder.Quality, JQuality)
            '    Dim encParams As New EncoderParameters(1)
            '    encParams.Param(0) = encParam
            '    SavePic.Image.Save(SaveFile.FileName, GetEncoderInfo("image/jpeg"), encParams)

            'ElseIf SaveFile.FilterIndex = 3 Then 'Bitmap
            '    SavePic.Image.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Bmp)
            'ElseIf SaveFile.FilterIndex = 4 Then
            '    SavePic.Image.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Gif)

            'End If


            Dim bmp As New Bitmap(SavePic.Image)
            Call PicToFileSave(bmp, SaveFile.FilterIndex)
            bmp.Dispose()
            SavePic.Dispose()
        End If
    End Sub


    'jpegの品質を指定して保存で使う関数(ネットからほぼコピペ)
    'エンコーダーを返す
    '渡されたMimeのエンコーダーを返す、jpegが欲しいならGetEncoderInfo("image/jpeg")
    'ほかは"image/png""image/bmp""image/gif""image/tiff"
    'Imports System.Drawing.Imagingをクラスの外側に付けないとうまくいかない
    Private Function GetEncoderInfo(ByVal mimeType As String) As ImageCodecInfo
        Dim j As Integer
        Dim encoders As ImageCodecInfo()
        encoders = ImageCodecInfo.GetImageEncoders() '5種類しかないみたい
        For j = 0 To encoders.Length
            If encoders(j).MimeType = mimeType Then
                Return encoders(j)
            End If
        Next j
        Return Nothing

    End Function





    'Slide量のボタンをおした時
    Private Sub ButtonXRSlide1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonXRSlide1.MouseDown
        Me.NumericXslide.Value = Me.NumericXslide.Value + Me.ButtonXRSlide1.Text

    End Sub

    Private Sub ButtonXRSlide2_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonXRSlide2.MouseDown
        Me.NumericXslide.Value = Me.NumericXslide.Value + Me.ButtonXRSlide2.Text
    End Sub

    Private Sub ButtonXRSlide3_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonXRSlide3.MouseDown
        Me.NumericXslide.Value = Me.NumericXslide.Value + Me.ButtonXRSlide3.Text
    End Sub

    Private Sub ButtonXRSlide4_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonXRSlide4.MouseDown
        Me.NumericXslide.Value = Me.NumericXslide.Value + Me.ButtonXRSlide4.Text
    End Sub

    Private Sub ButtonXRSlide5_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonXRSlide5.MouseDown
        Me.NumericXslide.Value = Me.NumericXslide.Value + Me.ButtonXRSlide5.Text
    End Sub

    Private Sub ButtonDYSlide1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonDYSlide1.MouseDown
        Me.NumericYslide.Value = Me.NumericYslide.Value + Me.ButtonDYSlide1.Text
    End Sub

    Private Sub ButtonDYSlide2_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonDYSlide2.MouseDown
        Me.NumericYslide.Value = Me.NumericYslide.Value + Me.ButtonDYSlide2.Text
    End Sub

    Private Sub ButtonDYSlide3_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonDYSlide3.MouseDown
        Me.NumericYslide.Value = Me.NumericYslide.Value + Me.ButtonDYSlide3.Text
    End Sub

    Private Sub ButtonDYSlide4_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonDYSlide4.MouseDown
        Me.NumericYslide.Value = Me.NumericYslide.Value + Me.ButtonDYSlide4.Text
    End Sub

    Private Sub ButtonDYSlide5_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonDYSlide5.MouseDown
        Me.NumericYslide.Value = Me.NumericYslide.Value + Me.ButtonDYSlide5.Text
    End Sub

    '透明にする
    '指定した色を透明にするボタン
    Private Sub ButtonTranceparent_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTranceparent.Click
        ''Dim test(0) As Imaging.ColorMap
        ''test(0) = New Imaging.ColorMap
        ''test(0).OldColor = Color.White
        ''test(0).NewColor = Color.Red
        ''Dim attr As New Imaging.ImageAttributes
        ''attr.SetRemapTable(test)

        ''透過色が指定されていなければ何もしない
        'If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
        '    Exit Sub
        'End If

        ''全体を再描画する透過処理
        'Call Transparent()
        Call TransparentOnePic()
        Call Transparent3()
        Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image

    End Sub

    '色を元に戻す
    '透過色を無効にする、透過色を表示する
    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click

        If myPicAr.Count <> 0 Then
            'Dim myTempBmp As Bitmap
            'Dim i As Integer
            ''myPicAr(0).image = myPicArClone(3).image '確認用
            ''For Each c As ExPictureBox In myPicAr
            ''    c.Image = Nothing
            ''Next

            ''For Each c As ExPictureBox In myPicAr
            ''    myTempBmp = myPicArClone(8).Image

            ''Next

            ''クローンからコピー
            ''For i = 0 To myPicAr.Count - 1
            ''    myTempBmp = myPicArCloneR(i).image
            ''    myPicAr(i).image = myTempBmp
            ''Next

            'For i = 0 To myPicAr.Count - 1
            '    myTempBmp = myPicArClone(i).image
            '    myPicAr(i).image = myTempBmp
            'Next

            Call PicRestor()
            '-------------------------------------------------------------
            'Dim i As Integer = Me.FocusPic.Tag - 1
            'Dim FPic As ExPictureBox = DirectCast(myPicAr(i), ExPictureBox)
            'Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            'FPic.Image = DirectCast(myPicArBackup(i), ExPictureBox).Image
            ''Me.FocusPic.Image = DirectCast(myPicArBackup(i), ExPictureBox).Image
            'ClonePic.Image = FPic.Image
            ''DirectCast(myPicArClone(i), ExPictureBox).Image = Me.FocusPic.Image

            ''ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            'If Me.FocusPic.Name.EndsWith("_T") Then '名前の末尾が_Tなら_Tを削除
            '    Dim newName As String = FPic.Name.Substring(0, FPic.Name.Length - 2)
            '    FPic.Name = newName
            '    Dim newCName As String = ClonePic.Name.Substring(0, ClonePic.Name.Length - 2)
            '    ClonePic.Name = newCName
            'End If


            '-----------------------------------------------------------
            ''For i = 0 To myPicAr.Count - 1
            ''    myPicAr(i) = myPicArClone(i)
            ''Next


            ''For i = 0 To myPicAr.Count - 1
            ''    myTempBmp = myPicArClone(i).image
            ''    myPicAr(i).image = myTempBmp
            ''Next

            ''Dim myTempBmp As Bitmap
            ''For Each c As ExPictureBox In myPicArClone
            ''For Each c As Image In myPicArClone
            ''    myTempBmp = New Bitmap(c)
            ''    c = myTempBmp
            ''Next

            ''Dim myTempBmp As Bitmap
            ''For Each c As ExPictureBox In myPicArClone
            ''For Each c As Image In myPicArClone
            ''    myTempBmp = New Bitmap(c)
            ''    c = myTempBmp
            ''Next
        End If
    End Sub

    '画像を元に戻す
    Friend Sub PicRestor()
        If myPicAr.Count <> 0 Then
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim FPic As ExPictureBox = DirectCast(myPicAr(i), ExPictureBox)
            Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            Dim Backup As ExPictureBox = DirectCast(myPicArBackup(i), ExPictureBox)

            '画像をBackupから戻す
            FPic.Image = Backup.Image
            FPic.Name = Backup.Name '名前をBackupから戻す
            ClonePic.Image = FPic.Image

            Call Transparent4()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            'ピクチャーボックスの名前を戻す
            'Backupから戻すようにした
            'If Me.FocusPic.Name.EndsWith("_T") Then '名前の末尾が_Tなら_Tを削除
            '    Dim newName As String = FPic.Name.Substring(0, FPic.Name.Length - 2)
            '    FPic.Name = newName
            '    Dim newCName As String = ClonePic.Name.Substring(0, ClonePic.Name.Length - 2)
            '    ClonePic.Name = newCName
            'End If
        End If
    End Sub
    '全体を再描画する透過処理、超重い、ムリ
    Private Sub Transparent()
        '画像がなければ何もしない
        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        '透過色が指定されていなければ何もしない
        If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
            Exit Sub
        End If


        '一旦透明にした色を戻す、これを実行しないと前回の画像が残る、残像みたいになる
        'デメリットはこれを使うと次々に色を透明にすることができない
        Dim myTempBmp As Bitmap
        Dim i As Integer
        For i = 0 To myPicAr.Count - 1
            myTempBmp = myPicArClone(i).image
            myPicAr(i).image = myTempBmp
        Next


        '透明にする
        Dim myTempBmp2 As Bitmap
        'myPicArClone = myPicAr.Clone
        For Each c As ExPictureBox In myPicAr
            myTempBmp2 = New Bitmap(c.Image)
            myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
            c.Image = myTempBmp2
        Next


        '下の画像を映す、見えるようにする

        Call SortForSave()
        Dim k As Integer
        For k = 0 To myPicAr.Count - 1 ' Step -1
            Dim x As Integer = myPicArR(k).width
            Dim y As Integer = myPicArR(k).height
            Dim bmp As Bitmap = New Bitmap(x, y)
            Dim gra As Graphics = Graphics.FromImage(bmp)
            Dim xLocate As Integer = myPicArR(k).location.x
            Dim yLocate As Integer = myPicArR(k).location.y

            For j As Integer = 0 To k
                Dim img As Image = myPicArCloneR(j).image
                'Dim img As Image = myPicArR(j).image
                'Dim xDiff As Integer = xLocate - myPicArR(j).location.x
                'Dim yDiff As Integer = yLocate - myPicArR(j).location.y

                gra.DrawImage(myPicArCloneR(j).image, myPicArR(j).location.x - xLocate, myPicArR(j).location.y - yLocate)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xLocate, myPicArR(j).location.y - yLocate)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xLocate - xDiff, myPicArR(j).location.y - yLocate - yDiff)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xDiff, myPicArR(j).location.y - yDiff)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location)

            Next j
            myPicArR(k).image = bmp
        Next

    End Sub
    'サブフォーム用、全体を再描画する透過処理、超重い、ムリ
    Friend Sub Transparent2()
        '画像がなければ何もしない
        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        '透過色が指定されていなければ何もしない
        'If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
        '    Exit Sub
        'End If


        '一旦透明にした色を戻す、これを実行しないと前回の画像が残る、残像みたいになる
        'デメリットはこれを使うと次々に色を透明にすることができない
        'Dim myTempBmp As Bitmap
        Dim i As Integer
        For i = 0 To myPicAr.Count - 1
            'myTempBmp = myPicArClone(i).image
            'myPicAr(i).image = myTempBmp
            myPicAr(i).image = myPicArClone(i).image
        Next


        '透明にする
        Dim myTempBmp2 As Bitmap
        'myPicArClone = myPicAr.Clone
        For Each c As ExPictureBox In myPicAr
            myTempBmp2 = New Bitmap(c.Image)
            myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
            c.Image = myTempBmp2
        Next


        '下の画像を映す、見えるようにする

        Call SortForSave()
        'Call SortForSavetime()
        'Dim fPic As ExPictureBox = Me.FocusPic
        Dim k As Integer
        For k = 0 To myPicAr.Count - 1 ' Step -1
            Dim pic As ExPictureBox = DirectCast(myPicArR(k), ExPictureBox)


            '移動画像に重なっていないものは処理しないようにしたいけどうまくいかないけど
            'もう少しな気がする
            Dim wPick As Integer = pic.Width
            Dim hPick As Integer = pic.Height
            Dim xPick As Integer = pic.Location.X
            Dim yPick As Integer = pic.Location.Y


            Dim bmp As New Bitmap(pic.Width, pic.Height)
            Dim gra As Graphics = Graphics.FromImage(bmp)

            For j As Integer = 0 To k

                Dim PicJ As ExPictureBox = DirectCast(myPicArCloneR(j), ExPictureBox)
                'Dim PicJ As ExPictureBox = DirectCast(myPicArR(j), ExPictureBox)

                Dim xPicJ As Integer = PicJ.Location.X
                Dim yPicJ As Integer = PicJ.Location.Y
                Dim wPicJ As Integer = PicJ.Width
                Dim hPicJ As Integer = PicJ.Height

                '全部描画
                gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)


                '全部描画しないで重なりがない画像は弾くように判定をつけてみたけど
                '速度的にはほとんど変わらなかった
                'Dim drawFlag As Boolean
                'If Not xPicJ > pic.Location.X + pic.Width _
                '    OrElse Not yPicJ > pic.Location.Y + pic.Height _
                '    OrElse Not xPicJ + wPicJ < pic.Location.X _
                '    OrElse Not yPicJ + hPicJ < pic.Location.Y Then
                '    drawFlag = True
                'End If
                'If drawFlag Then
                '    gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                '    drawFlag = False
                'End If
                '↑と↓どちらもほとんど同じ速度
                'If xPicJ < pic.Location.X + pic.Width _
                '    And yPicJ < pic.Location.Y + pic.Height _
                '    And xPicJ + wPicJ > pic.Location.X _
                '    And yPicJ + hPicJ > pic.Location.Y Then
                '    gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                'End If

            Next j


            gra.Dispose() '意味ないかも、弊害あるかも
            myPicArR(k).image = bmp


            'End If

        Next



    End Sub
    Friend Sub Transparent1_2() 'Transparentの改変
        'Dim testarreso As Single = DirectCast(myPicAr(0), ExPictureBox).Image.HorizontalResolution



        '画像がなければ何もしない
        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        '一旦透明にした色を戻す、これを実行しないと前回の画像が残る、残像みたいになる
        'デメリットはこれを使うと次々に色を透明にすることができない
        'Dim myTempBmp As Bitmap
        Dim i As Integer
        For i = 0 To myPicAr.Count - 1
            myPicAr(i).image = myPicArClone(i).image
        Next



        ''透明にする()
        Dim myTempBmp2 As Bitmap
        'myPicArClone = myPicAr.Clone
        For Each c As ExPictureBox In myPicAr
            myTempBmp2 = New Bitmap(c.Image)

            myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
            'MakeTransparentを設定した後にレゾリューションを設定しないと強制的に96になってしまう
            '→画像の読み込みの時に正しくなるように修正したので不要
            'myTempBmp2.SetResolution(c.Image.HorizontalResolution, c.Image.VerticalResolution)
            c.Image = myTempBmp2
        Next


        '下の画像を映す、見えるようにする
        Call SortForSave()


        Dim k As Integer
        For k = 0 To myPicAr.Count - 1 ' Step -1
            Dim picB As ExPictureBox = DirectCast(myPicArR(k), ExPictureBox)
            Dim bmp As Bitmap = New Bitmap(picB.Width, picB.Height)
            'bmp.SetResolution(picB.Image.HorizontalResolution, picB.Image.VerticalResolution)

            Dim gra As Graphics = Graphics.FromImage(bmp)
            Dim xLocate As Integer = picB.Location.X 'myPicArR(k).location.x
            Dim yLocate As Integer = picB.Location.Y 'myPicArR(k).location.y

            For j As Integer = 0 To k
                Dim img As Image = myPicArCloneR(j).image

                gra.DrawImage(img, myPicArR(j).location.x - xLocate, myPicArR(j).location.y - yLocate)

            Next j
            myPicArR(k).image = bmp
        Next


    End Sub
    '移動画像に重なっていない画像は処理しないようにしたから
    '少し軽くなった
    Friend Sub Transparent3()
        '画像がなければ何もしない
        If myPicAr.Count = 0 Then
            Exit Sub
        End If


        '一旦透明にした色を戻す、これを実行しないと前回の画像が残る、残像みたいになる
        'デメリットはこれを使うと次々に色を透明にすることができない
        '本当は透過色のある画像だけ処理したい
        'Dim i As Integer

        'For i = 0 To myPicAr.Count - 1
        '    Dim PicAr As ExPictureBox = DirectCast(myPicAr(i), ExPictureBox)

        '    If PicAr.Name.EndsWith("_T") Then
        '        PicAr.Image = DirectCast(myPicArClone(i), ExPictureBox).Image
        '        '                myPicAr(i).image = myPicArClone(i).image
        '    End If

        'Next
        'Dim PicAr As ExPictureBox = DirectCast(myPicAr(i), ExPictureBox)
        'PicAr.Image = DirectCast(myPicArClone(i), ExPictureBox).Image

        '下の画像を映す、見えるようにする
        Call SortForSave()
        'Call SortForSavetime()
        Dim fPic As ExPictureBox = Me.ActExPic
        Dim k As Integer
        For k = 0 To myPicAr.Count - 1 ' Step -1
            Dim pic As ExPictureBox = DirectCast(myPicArR(k), ExPictureBox)
            Dim Clone As ExPictureBox = DirectCast(myPicArCloneR(k), ExPictureBox)

            '移動画像に重なっていないものと透過色が設定されていないものは処理しない
            Dim wPick As Integer = pic.Width
            Dim hPick As Integer = pic.Height
            Dim xPick As Integer = pic.Location.X
            Dim yPick As Integer = pic.Location.Y
            If xPick < fPic.Location.X + fPic.Width _
            AndAlso yPick < fPic.Location.Y + fPic.Height _
            AndAlso xPick + wPick > fPic.Location.X _
            AndAlso yPick + hPick > fPic.Location.Y _
            AndAlso pic.Name.EndsWith("_T") Then '名前の末尾に_Tが付いているものだけ処理する

                pic.Image = Clone.Image 'これがないと残像が出る

                Dim bmp As New Bitmap(pic.Width, pic.Height)

                Dim gra As Graphics = Graphics.FromImage(bmp)

                'Dim Sw As New System.Diagnostics.Stopwatch
                'Sw.Start()
                For j As Integer = 0 To k
                    Dim PicJ As ExPictureBox = DirectCast(myPicArCloneR(j), ExPictureBox)
                    'Dim PicJ As ExPictureBox = DirectCast(myPicArR(j), ExPictureBox)

                    Dim xPicJ As Integer = PicJ.Location.X
                    Dim yPicJ As Integer = PicJ.Location.Y
                    Dim wPicJ As Integer = PicJ.Width
                    Dim hPicJ As Integer = PicJ.Height


                    '全部描画
                    'gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)

                    '全部描画しないで重なりがない画像は弾くように判定をつけてみたけど
                    '速度的にはほとんど変わらなかった
                    'Dim drawFlag As Boolean
                    'If Not xPicJ > pic.Location.X + pic.Width _
                    '    OrElse Not yPicJ > pic.Location.Y + pic.Height _
                    '    OrElse Not xPicJ + wPicJ < pic.Location.X _
                    '    OrElse Not yPicJ + hPicJ < pic.Location.Y Then
                    '    drawFlag = True
                    'End If
                    'If drawFlag Then
                    '    gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                    '    drawFlag = False
                    'End If
                    '↑と↓どちらもほとんど同じ速度

                    If xPicJ < pic.Location.X + pic.Width _
                                         AndAlso yPicJ < pic.Location.Y + pic.Height _
                                         AndAlso xPicJ + wPicJ > pic.Location.X _
                                         AndAlso yPicJ + hPicJ > pic.Location.Y Then
                        gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                    End If

                Next j

                gra.Dispose() '意味ないかも、弊害あるかも
                myPicArR(k).image = bmp

                'Sw.Stop()
                'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)
            End If


        Next
        'fPic.Dispose()'これはエラーの原因になるかも


    End Sub

    'Transparent3の改変で上下を入れ替えたとき用
    '画像が移動しないとき用
    Friend Sub Transparent4()
        '画像がなければ何もしない
        If myPicAr.Count = 0 Then
            Exit Sub
        End If


        '下の画像を映す、見えるようにする
        Call SortForSave()
        'Call SortForSavetime()
        Dim fPic As ExPictureBox = Me.ActExPic
        Dim k As Integer
        For k = 0 To myPicAr.Count - 1 ' Step -1
            Dim pic As ExPictureBox = DirectCast(myPicArR(k), ExPictureBox)
            Dim Clone As ExPictureBox = DirectCast(myPicArCloneR(k), ExPictureBox)

            '移動画像に重なっていないものは処理しない
            Dim wPick As Integer = pic.Width
            Dim hPick As Integer = pic.Height
            Dim xPick As Integer = pic.Location.X
            Dim yPick As Integer = pic.Location.Y
            If xPick < fPic.Location.X + fPic.Width _
            AndAlso yPick < fPic.Location.Y + fPic.Height _
            AndAlso xPick + wPick > fPic.Location.X _
            AndAlso yPick + hPick > fPic.Location.Y Then '_
                'AndAlso pic.Name.EndsWith("_T") Then '名前の末尾に_Tが付いているものだけ処理する

                pic.Image = Clone.Image 'これがないと残像が出る

                Dim bmp As New Bitmap(pic.Width, pic.Height)
                bmp.SetResolution(pic.Image.HorizontalResolution, pic.Image.VerticalResolution)

                Dim gra As Graphics = Graphics.FromImage(bmp)

                'Dim Sw As New System.Diagnostics.Stopwatch
                'Sw.Start()
                For j As Integer = 0 To k
                    Dim PicJ As ExPictureBox = DirectCast(myPicArCloneR(j), ExPictureBox)
                    'Dim PicJ As ExPictureBox = DirectCast(myPicArR(j), ExPictureBox)

                    Dim xPicJ As Integer = PicJ.Location.X
                    Dim yPicJ As Integer = PicJ.Location.Y
                    Dim wPicJ As Integer = PicJ.Width
                    Dim hPicJ As Integer = PicJ.Height


                    '全部描画
                    'gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)

                    '全部描画しないで重なりがない画像は弾くように判定をつけてみたけど
                    '速度的にはほとんど変わらなかった
                    'Dim drawFlag As Boolean
                    'If Not xPicJ > pic.Location.X + pic.Width _
                    '    OrElse Not yPicJ > pic.Location.Y + pic.Height _
                    '    OrElse Not xPicJ + wPicJ < pic.Location.X _
                    '    OrElse Not yPicJ + hPicJ < pic.Location.Y Then
                    '    drawFlag = True
                    'End If
                    'If drawFlag Then
                    '    gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                    '    drawFlag = False
                    'End If
                    '↑と↓どちらもほとんど同じ速度

                    If xPicJ < pic.Location.X + pic.Width _
                                         AndAlso yPicJ < pic.Location.Y + pic.Height _
                                         AndAlso xPicJ + wPicJ > pic.Location.X _
                                         AndAlso yPicJ + hPicJ > pic.Location.Y Then
                        gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                    End If

                Next j

                gra.Dispose() '意味ないかも、弊害あるかも
                myPicArR(k).image = bmp

                'Sw.Stop()
                'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)
            End If


        Next
        'fPic.Dispose()'これはエラーの原因になるかも


    End Sub

    'Transparent4の改変でカーソルキーで移動用
    '
    Friend Sub Transparent5()
        '画像がなければ何もしない
        If myPicAr.Count = 0 Then
            Exit Sub
        End If


        '下の画像を映す、見えるようにする
        Call SortForSave()
        'Call SortForSavetime()
        Dim grid As Integer = Me.NumericUpDownGrid.Value
        Dim fPic As ExPictureBox = Me.ActExPic
        Dim k As Integer
        For k = 0 To myPicAr.Count - 1 ' Step -1
            Dim pic As ExPictureBox = DirectCast(myPicArR(k), ExPictureBox)
            Dim Clone As ExPictureBox = DirectCast(myPicArCloneR(k), ExPictureBox)

            '移動画像に重なっていないものは処理しない
            Dim wPick As Integer = pic.Width
            Dim hPick As Integer = pic.Height
            Dim xPick As Integer = pic.Location.X
            Dim yPick As Integer = pic.Location.Y
            '移動中の画像の大きさ＋グリッドの数値の範囲にあるものだけ処理
            If xPick < fPic.Location.X + fPic.Width + grid _
            AndAlso yPick < fPic.Location.Y + fPic.Height + grid _
            AndAlso xPick + wPick > fPic.Location.X - grid _
            AndAlso yPick + hPick > fPic.Location.Y - grid Then '_
                'AndAlso pic.Name.EndsWith("_T") Then '名前の末尾に_Tが付いているものだけ処理する

                pic.Image = Clone.Image 'これがないと残像が出る

                Dim bmp As New Bitmap(pic.Width, pic.Height)

                Dim gra As Graphics = Graphics.FromImage(bmp)

                'Dim Sw As New System.Diagnostics.Stopwatch
                'Sw.Start()
                For j As Integer = 0 To k
                    Dim PicJ As ExPictureBox = DirectCast(myPicArCloneR(j), ExPictureBox)
                    'Dim PicJ As ExPictureBox = DirectCast(myPicArR(j), ExPictureBox)

                    Dim xPicJ As Integer = PicJ.Location.X
                    Dim yPicJ As Integer = PicJ.Location.Y
                    Dim wPicJ As Integer = PicJ.Width
                    Dim hPicJ As Integer = PicJ.Height


                    '全部描画
                    'gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)

                    '全部描画しないで重なりがない画像は弾くように判定をつけてみたけど
                    '速度的にはほとんど変わらなかった
                    'Dim drawFlag As Boolean
                    'If Not xPicJ > pic.Location.X + pic.Width _
                    '    OrElse Not yPicJ > pic.Location.Y + pic.Height _
                    '    OrElse Not xPicJ + wPicJ < pic.Location.X _
                    '    OrElse Not yPicJ + hPicJ < pic.Location.Y Then
                    '    drawFlag = True
                    'End If
                    'If drawFlag Then
                    '    gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                    '    drawFlag = False
                    'End If
                    '↑と↓どちらもほとんど同じ速度

                    If xPicJ < pic.Location.X + pic.Width _
                                         AndAlso yPicJ < pic.Location.Y + pic.Height _
                                         AndAlso xPicJ + wPicJ > pic.Location.X _
                                         AndAlso yPicJ + hPicJ > pic.Location.Y Then
                        gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                    End If

                Next j

                gra.Dispose() '意味ないかも、弊害あるかも
                myPicArR(k).image = bmp

                'Sw.Stop()
                'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)
            End If


        Next
        'fPic.Dispose()'これはエラーの原因になるかも


    End Sub

    'Transparent4の改変でマウスで動かした後用
    Friend Sub Transparent6()
        '画像がなければ何もしない
        If myPicAr.Count = 0 Then
            Exit Sub
        End If


        '下の画像を映す、見えるようにする
        Call SortForSave()
        'Call SortForSavetime()
        Dim fPic As ExPictureBox = Me.ActExPic

        Dim k As Integer
        For k = 0 To myPicAr.Count - 1 ' Step -1
            Dim pic As ExPictureBox = DirectCast(myPicArR(k), ExPictureBox)
            Dim Clone As ExPictureBox = DirectCast(myPicArCloneR(k), ExPictureBox)

            '移動画像の移動前と移動後に重なっているものだけ処理するために中へ
            Dim wPick As Integer = pic.Width
            Dim hPick As Integer = pic.Height
            Dim xPick As Integer = pic.Location.X
            Dim yPick As Integer = pic.Location.Y
            If (xPick < fPic.Location.X + fPic.Width _
            AndAlso yPick < fPic.Location.Y + fPic.Height _
            AndAlso xPick + wPick > fPic.Location.X _
            AndAlso yPick + hPick > fPic.Location.Y) Or _
            (xPick < oldPicLocate.X + fPic.Width _
            AndAlso yPick < oldPicLocate.Y + fPic.Height _
            AndAlso xPick + wPick > oldPicLocate.X _
            AndAlso yPick + hPick > oldPicLocate.Y) Then '_
                'AndAlso pic.Name.EndsWith("_T") Then '名前の末尾に_Tが付いているものだけ処理する

                pic.Image = Clone.Image 'これがないと残像が出る

                Dim bmp As New Bitmap(pic.Width, pic.Height)


                Dim gra As Graphics = Graphics.FromImage(bmp)

                'Dim Sw As New System.Diagnostics.Stopwatch
                'Sw.Start()
                For j As Integer = 0 To k
                    'myPicArCloneRのクローン画像を重ねて描画していくmyPicArRだと半透明で色が濃くなる
                    Dim PicJ As ExPictureBox = DirectCast(myPicArCloneR(j), ExPictureBox)
                    'Dim PicJ As ExPictureBox = DirectCast(myPicArR(j), ExPictureBox)

                    Dim xPicJ As Integer = PicJ.Location.X
                    Dim yPicJ As Integer = PicJ.Location.Y
                    Dim wPicJ As Integer = PicJ.Width
                    Dim hPicJ As Integer = PicJ.Height


                    '全部描画
                    'gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)

                    '全部描画しないで重なりがない画像は弾くように判定をつけてみたけど
                    '速度的にはほとんど変わらなかった
                    'Dim drawFlag As Boolean
                    'If Not xPicJ > pic.Location.X + pic.Width _
                    '    OrElse Not yPicJ > pic.Location.Y + pic.Height _
                    '    OrElse Not xPicJ + wPicJ < pic.Location.X _
                    '    OrElse Not yPicJ + hPicJ < pic.Location.Y Then
                    '    drawFlag = True
                    'End If
                    'If drawFlag Then
                    '    gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                    '    drawFlag = False
                    'End If
                    '↑と↓どちらもほとんど同じ速度

                    If xPicJ < pic.Location.X + pic.Width _
                                         AndAlso yPicJ < pic.Location.Y + pic.Height _
                                         AndAlso xPicJ + wPicJ > pic.Location.X _
                                         AndAlso yPicJ + hPicJ > pic.Location.Y Then
                        gra.DrawImage(PicJ.Image, xPicJ - xPick, yPicJ - yPick)
                    End If

                Next j

                gra.Dispose() '意味ないかも、弊害あるかも
                myPicArR(k).image = bmp

                'Sw.Stop()
                'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)
            End If


        Next
        'fPic.Dispose()'これはエラーの原因になるかも


    End Sub

    '移動し終わった時に使う透過処理、1枚だけ処理するTransparent()より軽い
    Private Sub TransparentForMove()
        '移動している画像用に少し書き換えてみた
        '動かしている画像のみ再描画するようにしてみたけど負荷がかかりすぎで実用的ではない
        '移動し終わった時に使うのが良さそう

        '画像がなければ何もしない
        If myPicAr.Count = 0 Then
            Exit Sub
        End If
        '透過色が指定されていなければ何もしない
        If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
            Exit Sub
        End If
        '一旦透明にした色を戻す、これを実行しないと前回の画像が残る、残像みたいになる
        'デメリットはこれを使うと次々に色を透明にすることができない
        '↑リアルタイム透過処理じゃなければ意味が無いみたい
        'For Each c As ExPictureBox In myPicArClone
        '    If FocusPic.Tag = c.Tag Then
        '        FocusPic.Image = c.Image
        '        Exit For
        '    End If
        'Next



        '透明にする
        Dim myTempBmp2 As Bitmap
        'myPicArClone = myPicAr.Clone
        For Each c As ExPictureBox In myPicAr
            If ActExPic.Tag = c.Tag Then
                myTempBmp2 = New Bitmap(c.Image)
                myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
                c.Image = myTempBmp2
                Exit For
            End If
        Next


        '下の画像を映す、見えるようにする
        Call SortForSave()
        Dim count As Integer = 0
        For Each c As Control In myPicArR '逆順コレクションからFocusPicと同じ画像のIDを探す、これは効率が悪い
            count = count + 1
            If c.Tag = ActExPic.Tag Then
                Exit For
            End If
        Next

        Dim k As Integer = count - 1
        Dim x As Integer = myPicArR(k).width
        Dim y As Integer = myPicArR(k).height
        Dim bmp As Bitmap = New Bitmap(x, y)
        Dim gra As Graphics = Graphics.FromImage(bmp)
        Dim xLocate As Integer = myPicArR(k).location.x
        Dim yLocate As Integer = myPicArR(k).location.y

        For j As Integer = 0 To k '自分より下にある画像をすべて描画
            Dim img As Image = myPicArR(j).image
            gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xLocate, myPicArR(j).location.y - yLocate)
        Next j
        myPicArR(k).image = bmp

    End Sub


    Private Sub TransparentForMove2()
        '透過色が指定されていなければ何もしない
        If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
            Exit Sub
        End If

        '一旦透明にした色を戻す、これを実行しないと前回の画像が残る、残像みたいになる
        'デメリットはこれを使うと次々に色を透明にすることができない
        Dim myTempBmp As Bitmap
        Dim i As Integer
        For i = 0 To myPicAr.Count - 1
            myTempBmp = myPicArClone(i).image
            myPicAr(i).image = myTempBmp
        Next



        '透明にする
        Dim myTempBmp2 As Bitmap
        'myPicArClone = myPicAr.Clone
        For Each c As ExPictureBox In myPicAr
            myTempBmp2 = New Bitmap(c.Image)
            myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
            c.Image = myTempBmp2
        Next
        ''自分だけ透明にする
        'Dim myTempBmp2 As Bitmap
        ''myPicArClone = myPicAr.Clone
        'For Each c As ExPictureBox In myPicAr
        '    If FocusPic.Tag = c.Tag Then
        '        myTempBmp2 = New Bitmap(c.Image)
        '        myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
        '        c.Image = myTempBmp2
        '        Exit For
        '    End If
        'Next


        '下の画像を映す、見えるようにする
        '自分より上の画像すべてを再描画
        Call SortForSave()
        Dim k As Integer
        For k = ActExPic.Tag To myPicAr.Count - 1 ' Step -1
            Dim x As Integer = myPicArR(k).width
            Dim y As Integer = myPicArR(k).height
            Dim bmp As Bitmap = New Bitmap(x, y)
            Dim gra As Graphics = Graphics.FromImage(bmp)
            Dim xLocate As Integer = myPicArR(k).location.x
            Dim yLocate As Integer = myPicArR(k).location.y

            For j As Integer = 0 To myPicAr.Count - 1
                Dim img As Image = myPicArR(j).image
                'Dim xDiff As Integer = xLocate - myPicArR(j).location.x
                'Dim yDiff As Integer = yLocate - myPicArR(j).location.y

                gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xLocate, myPicArR(j).location.y - yLocate)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xLocate - xDiff, myPicArR(j).location.y - yLocate - yDiff)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xDiff, myPicArR(j).location.y - yDiff)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location)

            Next j
            myPicArR(k).image = bmp
        Next
    End Sub

    '少しは軽くしてみたつもり
    Private Sub TransparentForMove3()
        '透過色が指定されていなければ何もしない
        If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
            Exit Sub
        End If

        '一旦透明にした色を戻す、これを実行しないと前回の画像が残る、残像みたいになる
        'デメリットはこれを使うと次々に色を透明にすることができない
        'Dim myTempBmp As Bitmap
        'Dim i As Integer
        'For i = 0 To myPicAr.Count - 1
        '    myTempBmp = myPicArClone(i).image
        '    myPicAr(i).image = myTempBmp
        'Next



        '自分だけ色を戻す
        'Me.FocusPic.Image = myPicArClone(Me.FocusPic.Tag - 1).image

        '自分とその上だけ色を戻す
        For i As Integer = Me.ActExPic.Tag - 1 To 0 Step -1
            myPicAr(i).image = myPicArClone(i).image

        Next


        '自分とその上だけ透過処理
        Dim b As Bitmap

        For i As Integer = Me.ActExPic.Tag - 1 To 0 Step -1
            b = New Bitmap(CType(myPicAr(i), ExPictureBox).Image)
            b.MakeTransparent(Me.TransparentPictureBox.BackColor)
            myPicAr(i).image = b
        Next



        '下の画像を映す、見えるようにする
        '自分より上の画像すべてを再描画
        'Call SortForSave()’これはいらないかも？
        Dim k As Integer
        For k = ActExPic.Tag - 1 To 0 Step -1
            Dim x As Integer = myPicAr(k).width
            Dim y As Integer = myPicAr(k).height
            Dim bmp As Bitmap = New Bitmap(x, y)
            Dim gra As Graphics = Graphics.FromImage(bmp)
            Dim xLocate As Integer = myPicAr(k).location.x
            Dim yLocate As Integer = myPicAr(k).location.y

            For j As Integer = myPicAr.Count - 1 To k Step -1
                Dim img As Image = myPicAr(j).image
                'Dim xDiff As Integer = xLocate - myPicArR(j).location.x
                'Dim yDiff As Integer = yLocate - myPicArR(j).location.y

                gra.DrawImage(myPicAr(j).image, myPicAr(j).location.x - xLocate, myPicAr(j).location.y - yLocate)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xLocate - xDiff, myPicArR(j).location.y - yLocate - yDiff)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location.x - xDiff, myPicArR(j).location.y - yDiff)
                'gra.DrawImage(myPicArR(j).image, myPicArR(j).location)

            Next j
            myPicAr(k).image = bmp
        Next
    End Sub

    'カラーダイアログから色を取得
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If Me.ColorDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Me.TransparentPictureBox.BackColor = Me.ColorDialog1.Color

        End If
    End Sub

    '透過色を取得する
    Private Sub ButtonTColorSelect_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTColorSelect.Click
        'Dim Btext As String = Me.ButtonTColorSelect.Text

        If FlagColor Then
            FlagColor = False 'キャンセルの場合カーソルを戻す
            FlagGetColor = False
            Me.Cursor = Cursors.Default
            'Me.ButtonTColorSelect.Text = Btext
        ElseIf FlagColor = False Then
            'Me.ButtonTColorSelect.Text = "決まったらここ"
            Me.Cursor = Cursors.Cross 'カーソルを十字型にする
            FlagColor = True
            FlagGetColor = True
        End If
    End Sub

    '選択画像を左右反転
    Private Sub Button6_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        If myPicAr.Count <> 0 Then
            Me.ActExPic.Image.RotateFlip(RotateFlipType.RotateNoneFlipX)
            Me.ActExPic.Refresh()
        End If
        'If myPicAr.Count <> 0 Then
        '    Me.FocusPic.SizeMode = PictureBoxSizeMode.Zoom
        '    Me.FocusPic.Size = New Size(100, 100)
        '    Me.FocusPic.Refresh()
        'End If
        '拡大(デフォルトはBilinear)
        'If myPicAr.Count <> 0 Then
        '    Dim bmp As New Bitmap(Me.FocusPic.Width * 2, Me.FocusPic.Height * 2)
        '    Dim g As Graphics = Graphics.FromImage(bmp)
        '    Dim img As Image = Me.FocusPic.Image
        '    g.DrawImage(img, 0, 0, img.Width * 2, img.Height * 2)
        '    img.Dispose()
        '    g.Dispose()
        '    FocusPic.Image = bmp
        'End If

        ''拡大、ニアレストネイバー法NearestNeighbor
        'If myPicAr.Count <> 0 Then
        '    Dim bmp As New Bitmap(Me.FocusPic.Width * 2, Me.FocusPic.Height * 2)
        '    Dim g As Graphics = Graphics.FromImage(bmp)
        '    Dim img As Image = Me.FocusPic.Image
        '    'MsgBox(g.InterpolationMode.ToString)
        '    g.InterpolationMode = Drawing2D.InterpolationMode.NearestNeighbor
        '    g.DrawImage(img, 0, 0, img.Width * 2, img.Height * 2)
        '    img.Dispose()
        '    g.Dispose()
        '    FocusPic.Image = bmp
        'End If

        ''縮小、HighQualityBicubic
        'Dim W As Integer = Me.FocusPic.Width / 2
        'Dim H As Integer = Me.FocusPic.Height / 2
        'If myPicAr.Count <> 0 Then
        '    Dim bmp As New Bitmap(W, H)
        '    Dim g As Graphics = Graphics.FromImage(bmp)
        '    Dim img As Image = Me.FocusPic.Image
        '    'MsgBox(g.InterpolationMode.ToString)
        '    g.InterpolationMode = Drawing2D.InterpolationMode.HighQualityBicubic
        '    g.DrawImage(img, 0, 0, W, H)
        '    img.Dispose()
        '    g.Dispose()
        '    FocusPic.Image = bmp
        'End If
    End Sub

    'アプリケーションのバージョン情報表示
    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        Dim VDialog As New Form2()
        VDialog.ShowDialog(Me)
        VDialog.Dispose()

    End Sub

    'Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
    '    If Me.Panel2.Controls.Count <> 0 Then
    '        Me.ToolStripStatusLabel5.Text = "now" & NowControl.Name
    '        Me.ToolStripStatusLabel6.Text = "focus" & FocusPic.Name

    '    End If
    'End Sub

    '選択中の画像を上げるボタンをクリック
    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonUpper.Click
        Call FocusPictureUp()

    End Sub

    '選択中の画像を下げるボタン
    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonLower.Click
        Call FocusPictureDown()

    End Sub
    '選択中の画像を上げるボタンの上でホイール
    Private Sub ButtonUpper_MouseWheel(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonUpper.MouseWheel
        If e.Delta > 0 Then
            Call FocusPictureUp()
        ElseIf e.Delta < 0 Then
            Call FocusPictureDown()
        End If
    End Sub
    '選択中の画像を下げるボタンの上でホイール
    Private Sub ButtonLower_MouseWheel(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles ButtonLower.MouseWheel
        If e.Delta > 0 Then
            Call FocusPictureUp()
        ElseIf e.Delta < 0 Then
            Call FocusPictureDown()
        End If
    End Sub

    '選択中の画像を消去ボタンをおした時
    Private Sub ButtonDelPic_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonDelPic.Click
        Call DelPic()

    End Sub

    '透過色のReset
    Private Sub ButtonColReset_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonColReset.Click
        Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control)
    End Sub

    '選択画像を半透明のメソッド、元の画像からの透明度
    Friend Sub AbsColMatrixAlpha()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        '画像全体を半透明
        Dim i As Integer = ActExPic.Tag - 1
        '一旦透過処理を無しにするためにバックアップから描画する
        'FocusPic.Image = myPicArBackup(FocusPic.Tag - 1).image
        ActExPic.Image = DirectCast(myPicArBackup(i), ExPictureBox).Image

        Dim imgbmp = New Bitmap(Me.ActExPic.Image)
        Dim img As Image = imgbmp
        'Dim img As Image = Me.FocusPic.Image

        'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
        'Dim i As Integer = Me.FocusPic.Tag - 1
        'Dim bmp As New Bitmap(Me.FocusPic.Width, Me.FocusPic.Height)
        'Dim img As Image = DirectCast(myPicArClone(i), ExPictureBox).Image

        Dim bmp As New Bitmap(Me.ActExPic.Width, Me.ActExPic.Height)
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim cm As New System.Drawing.Imaging.ColorMatrix()
        Dim f As Single = myForm3.NumericUpDownAbsTra.Value / 100

        cm.Matrix00 = 1
        cm.Matrix11 = 1
        cm.Matrix22 = 1
        'cm.Matrix33 = myForm3.NumericUpDownAbsTra.Value / 100
        'cm.Matrix33 = 0.5F
        cm.Matrix33 = f
        cm.Matrix44 = 1
        Dim ia As New System.Drawing.Imaging.ImageAttributes()
        ia.SetColorMatrix(cm)

        g.DrawImage(img, New Rectangle(0, 0, bmp.Width, bmp.Height), 0, 0, bmp.Width, bmp.Height, GraphicsUnit.Pixel, ia)
        'Me.FocusPic.Image = bmp'下でも同じかなあ
        DirectCast(myPicAr(i), ExPictureBox).Image = bmp
        Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
        ClonePic.Image = bmp '半透明状態をクローンに記録
        'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
        If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
            Me.ActExPic.Name = Me.ActExPic.Name & "_T"
            ClonePic.Name = ClonePic.Name & "_T"
        End If

        'Call Transparent()
        Call Transparent2()
        Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
    End Sub
    '選択画像を半透明のメソッド加算、実行するほど透明になる→
    Friend Sub AddColMatrixAlpha()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        '画像全体を半透明

        '一旦透過処理を無しにするためにバックアップから描画する
        'FocusPic.Image = myPicArBackup(FocusPic.Tag - 1).image
        'Dim bmp As New Bitmap(Me.FocusPic.Width, Me.FocusPic.Height)
        'Dim img As Image = Me.FocusPic.Image

        'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
        Dim i As Integer = Me.ActExPic.Tag - 1
        Dim bmp As New Bitmap(Me.ActExPic.Width, Me.ActExPic.Height)

        'Dim img As Image = DirectCast(myPicArClone(i), ExPictureBox).Image
        '以前は↑でimgを作っていたけどファイルストリームでファイルを開くようにしたら
        'g.Drawimageの所でメモリが不足していますのエラーが出るようになった
        'これは32bitで書き込むのにimgが24bitだったから

        'ImageAttributeを使ったDrawImageでメモリが不足しています: DOBON.NETプログラミング掲示板過去ログ()
        'http://dobon.net/vb/bbs/log3-38/23266.html

        'なので一旦bmpにピクチャーボックスのイメージを読み込んで32bitにするようにしたのが↓
        Dim imgbmp = New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
        Dim img As Image = imgbmp


        Dim alpha As Single = myForm3.NumericUpDownAddTra.Value / 100
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim cm As New System.Drawing.Imaging.ColorMatrix()

        cm.Matrix00 = 1
        cm.Matrix11 = 1
        cm.Matrix22 = 1
        cm.Matrix33 = alpha 'myForm3.NumericUpDownAddTra.Value / 100
        'cm.Matrix33 = 0.5F
        cm.Matrix44 = 1
        Dim ia As New System.Drawing.Imaging.ImageAttributes()
        ia.SetColorMatrix(cm)

        g.DrawImage(img, New Rectangle(0, 0, bmp.Width, bmp.Height), 0, 0, bmp.Width, bmp.Height, GraphicsUnit.Pixel, ia)
        Me.ActExPic.Image = bmp
        myPicArClone(ActExPic.Tag - 1).image = bmp '半透明状態をクローンに記録

        'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
        If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
            Dim tempClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            Me.ActExPic.Name = Me.ActExPic.Name & "_T"
            tempClonePic.Name = tempClonePic.Name & "_T"
        End If


        'Call Transparent()
        Call Transparent2()
        Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image

    End Sub
    '指定色を透明度を指定して透過メソッド
    Friend Sub AlphaforOneColor()
        If myPicAr.Count = 0 Then
            Exit Sub
        End If


        Me.Cursor = Cursors.WaitCursor 'マウスカーソルを砂時計に
        '一旦透過処理を無しにするためにバックアップから描画する
        'FocusPic.Image = myPicArBackup(FocusPic.Tag - 1).image
        'Dim bmp As New Bitmap(Me.FocusPic.Image)

        'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
        Dim i As Integer = Me.ActExPic.Tag - 1
        Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)

        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim x As Integer
        Dim y As Integer
        Dim Tcol As Color = myForm3.PictureBoxTranspCol.BackColor '塗り替える色の取得
        Dim tValue As Integer = myForm3.NumericUpDownTCol.Value
        '特定の色を半透明にする,1pixelづつ判定して塗り替える
        For x = 0 To bmp.Width - 1
            For y = 0 To bmp.Height - 1
                Dim OldColor = bmp.GetPixel(x, y)
                If OldColor = Tcol Then
                    Dim newColor As Color = Color.FromArgb(tValue, Tcol)
                    '多分下と同じかな
                    'If OldColor.R = Tcol.R And OldColor.G = Tcol.G And OldColor.B = Tcol.B Then
                    '    Dim newColor As Color = Color.FromArgb(tValue, Tcol.R, Tcol.G, Tcol.B)
                    bmp.SetPixel(x, y, newColor)
                End If
            Next
        Next
        g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height)) '出来上がったbmpをドローイング
        'Me.FocusPic.Image = bmp '表示
        DirectCast(myPicAr(i), ExPictureBox).Image = bmp
        'DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録
        Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
        ClonePic.Image = bmp '半透明状態をクローンに記録

        'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
        If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
            Me.ActExPic.Name = Me.ActExPic.Name & "_T"
            ClonePic.Name = ClonePic.Name & "_T"
        End If



        'Call Transparent() '透過表示
        Call Transparent2()
        Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
        Me.Cursor = Cursors.Default 'マウスカーソルを元に戻す
    End Sub

    '選択画像に透過色を設定
    Private Sub TransparentOnePic()

        If myPicAr.Count <> 0 Then

            '透過色が指定されていなければ何もしない
            If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
                Exit Sub
            End If

            Dim i As Integer = Me.ActExPic.Tag - 1
            '透明にする
            Dim myTempBmp2 As Bitmap
            Dim tempClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)

            myTempBmp2 = New Bitmap(tempClonePic.Image)
            myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
            Me.ActExPic.Image = myTempBmp2

            'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
                'If Microsoft.VisualBasic.Right(Me.FocusPic.Name, 2) <> "_T" Then'こっちでも同じ
                Me.ActExPic.Name = Me.ActExPic.Name & "_T"
                tempClonePic.Name = tempClonePic.Name & "_T"
            End If
            'DirectCast(myPicArClone(i), ExPictureBox).Image = myTempBmp2
            tempClonePic.Image = myTempBmp2

            Call Transparent2()
            'Call Transparent6()


        End If

    End Sub

    Friend Sub Bilinear()
        '縮小(デフォルトはBilinear)
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            Dim x As Integer = ActExPic.Width * myForm3.NumericUpDownRatio.Value
            Dim y As Integer = ActExPic.Height * myForm3.NumericUpDownRatio.Value
            If x = 0 Or y = 0 Then '縮小画像の大きさが0になったら何もしない
                Exit Sub
            End If

            Dim bmp As New Bitmap(x, y)
            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim PicImg As ExPictureBox = DirectCast(myPicArClone(Me.ActExPic.Tag - 1), ExPictureBox)
            Dim img As Image = PicImg.Image
            'g.PixelOffsetMode = PixelOffsetMode.Default
            g.PixelOffsetMode = PixelOffsetMode.Half
            'g.PixelOffsetMode = PixelOffsetMode.HighQuality
            'g.PixelOffsetMode = PixelOffsetMode.HighSpeed
            'g.PixelOffsetMode = PixelOffsetMode.None

            g.DrawImage(img, 0, 0, x, y) 'Bitmapに書き込み
            'img.Dispose()'これをつけると他のところでCollectionにアクセする時にエラーになる
            'g.Dispose()
            ActExPic.Image = bmp '表示
            PicImg.Image = bmp 'クローンのコレクションを更新
            Call MoveAfter()
            Me.Cursor = Cursors.Default

        End If
    End Sub

    Friend Sub NearestNeighbor()
        '拡大、ニアレストネイバー法NearestNeighbor
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            Dim x As Integer = ActExPic.Width * myForm3.NumericUpDownRatio.Value
            Dim y As Integer = ActExPic.Height * myForm3.NumericUpDownRatio.Value
            If x = 0 Or y = 0 Then '縮小画像の大きさが0になったら何もしない
                Exit Sub
            End If

            Dim bmp As New Bitmap(x, y)
            Dim g As Graphics = Graphics.FromImage(bmp)
            '縮小する画像をクローンからセット
            Dim PicImg As ExPictureBox = DirectCast(myPicArClone(Me.ActExPic.Tag - 1), ExPictureBox)
            Dim img As Image = PicImg.Image

            g.InterpolationMode = Drawing2D.InterpolationMode.NearestNeighbor
            g.PixelOffsetMode = PixelOffsetMode.Half '重要、これがないとずれる

            'g.PixelOffsetMode = PixelOffsetMode.None
            Dim val = g.PixelOffsetMode

            g.DrawImage(img, 0, 0, x, y)
            ActExPic.Image = bmp
            'DirectCast(myPicArClone(FocusPic.Tag - 1), ExPictureBox).Image = bmp 'クローンのコレクションを更新
            PicImg.Image = bmp 'クローンのコレクションを更新
            Call MoveAfter()
            Me.Cursor = Cursors.Default

        End If
    End Sub

    Friend Sub HighQualityBicubic()
        'ハイクオリティバイキュービック法で拡大縮小
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            Dim x As Integer = ActExPic.Width * myForm3.NumericUpDownRatio.Value
            Dim y As Integer = ActExPic.Height * myForm3.NumericUpDownRatio.Value

            If x = 0 Or y = 0 Then '縮小画像の大きさが0になったら何もしない
                Exit Sub
            End If

            Dim bmp As New Bitmap(x, y)
            Dim g As Graphics = Graphics.FromImage(bmp)
            '縮小する画像をクローンからセット
            Dim PicImg As ExPictureBox = DirectCast(myPicArClone(Me.ActExPic.Tag - 1), ExPictureBox)
            Dim img As Image = PicImg.Image

            g.InterpolationMode = Drawing2D.InterpolationMode.HighQualityBicubic
            'g.PixelOffsetMode = PixelOffsetMode.Default
            g.PixelOffsetMode = PixelOffsetMode.Half
            'g.PixelOffsetMode = PixelOffsetMode.HighQuality
            'g.PixelOffsetMode = PixelOffsetMode.HighSpeed
            'g.PixelOffsetMode = PixelOffsetMode.None

            g.DrawImage(img, 0, 0, x, y)
            ActExPic.Image = bmp
            PicImg.Image = bmp 'クローンのコレクションを更新
            Call MoveAfter()
            Me.Cursor = Cursors.Default

        End If
    End Sub

    Friend Sub GrayScale()
        'GrayScaleグレースケール
        If myPicAr.Count <> 0 Then


            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(Me.ActExPic.Width, Me.ActExPic.Height)
            Dim imgbmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim img As Image = imgbmp

            Dim g As Graphics = Graphics.FromImage(bmp)

            Dim cm As New Imaging.ColorMatrix(New Single()() {New Single() {0.299F, 0.299F, 0.299F, 0, 0}, _
                                                               New Single() {0.587F, 0.587F, 0.587F, 0, 0}, _
                                                               New Single() {0.114F, 0.114F, 0.114F, 0, 0}, _
                                                               New Single() {0, 0, 0, 1, 0}, _
                                                               New Single() {0, 0, 0, 0, 1}})
            Dim ia As New System.Drawing.Imaging.ImageAttributes()
            ia.SetColorMatrix(cm)


            g.DrawImage(img, New Rectangle(0, 0, bmp.Width, bmp.Height), 0, 0, bmp.Width, bmp.Height, GraphicsUnit.Pixel, ia)


            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'グレースケール状態ををクローンに記録

            'Call Transparent()
            Call Transparent2()

        End If
    End Sub
    Friend Sub GrayScaleNTSC()

        If myPicAr.Count = 0 Then
            Exit Sub

        End If
        Dim r As Single
        Dim g As Single
        Dim b As Single
        Dim ntsc As Integer

        Dim i As Integer = ActExPic.Tag - 1
        Dim bmp As New Bitmap(ActExPic.Image)
        Dim rw As Integer = bmp.Width
        Dim rh As Integer = bmp.Height
        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)

        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * rh - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)


        For x = 0 To rw - 1
            For y = 0 To rh - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4
                r = pixels(pos + 2) * 0.298912
                g = pixels(pos + 1) * 0.586611
                b = pixels(pos) * 0.114478
                ntsc = r + g + b
                pixels(pos + 2) = ntsc
                pixels(pos + 1) = ntsc
                pixels(pos) = ntsc
            Next

        Next

        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)

        ActExPic.Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp


    End Sub

    Friend Sub GrayScaleHDTV()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim r As Single
        Dim g As Single
        Dim b As Single
        Dim ntsc As Integer
        Dim gamma As Single = myForm3.TrackBarGrayScaleGamma.Value / 10
        Dim gamma2 As Single = myForm3.TrackBarGrayScaleGamma2.Value / 10


        Dim i As Integer = ActExPic.Tag - 1
        Dim bmp As New Bitmap(ActExPic.Image)
        Dim rw As Integer = bmp.Width
        Dim rh As Integer = bmp.Height
        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)

        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * rh - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        For x = 0 To rw - 1
            For y = 0 To rh - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4
                r = ((pixels(pos + 2) / 255) ^ gamma2) * 0.222015
                g = ((pixels(pos + 1) / 255) ^ gamma2) * 0.706655
                b = ((pixels(pos) / 255) ^ gamma2) * 0.07133
                ntsc = ((r + g + b) ^ (1 / gamma)) * 255

                pixels(pos + 2) = CByte(ntsc)
                pixels(pos + 1) = CByte(ntsc)
                pixels(pos) = CByte(ntsc)
            Next

        Next

        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)
        ActExPic.Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp

    End Sub

    Friend Sub GrayScaleCenter()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim r As Single
        Dim g As Single
        Dim b As Single
        Dim bri As Integer
        Dim gamma As Single = myForm3.TrackBarGrayScaleGamma.Value / 10
        Dim gamma2 As Single = myForm3.TrackBarGrayScaleGamma2.Value / 10

        Dim i As Integer = ActExPic.Tag - 1
        Dim bmp As New Bitmap(ActExPic.Image)
        Dim rw As Integer = bmp.Width
        Dim rh As Integer = bmp.Height
        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)

        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * rh - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        For x = 0 To rw - 1
            For y = 0 To rh - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4
                r = pixels(pos + 2)
                g = pixels(pos + 1)
                b = pixels(pos)
                bri = ((Math.Max(Math.Max(r, g), b)) + (Math.Min(Math.Min(r, g), b))) / 2

                pixels(pos + 2) = CByte(bri)
                pixels(pos + 1) = CByte(bri)
                pixels(pos) = CByte(bri)
            Next

        Next

        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)
        ActExPic.Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp

    End Sub

    Friend Sub GrayScaleAverage()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim r As Single
        Dim g As Single
        Dim b As Single
        Dim bri As Integer
        Dim gamma As Single = myForm3.TrackBarGrayScaleGamma.Value / 10
        Dim gamma2 As Single = myForm3.TrackBarGrayScaleGamma2.Value / 10

        Dim i As Integer = ActExPic.Tag - 1
        Dim bmp As New Bitmap(ActExPic.Image)
        Dim rw As Integer = bmp.Width
        Dim rh As Integer = bmp.Height
        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)

        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * rh - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        For x = 0 To rw - 1
            For y = 0 To rh - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4
                r = pixels(pos + 2)
                g = pixels(pos + 1)
                b = pixels(pos)
                bri = (r + g + b) / 3

                pixels(pos + 2) = CByte(bri)
                pixels(pos + 1) = CByte(bri)
                pixels(pos) = CByte(bri)
            Next

        Next

        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)
        ActExPic.Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp

    End Sub

    '四辺からのグラデ(遅い方)
    Friend Sub Transparent4sides()
        'If myPicAr.Count <> 0 Then
        '    Me.Cursor = Cursors.WaitCursor
        '    'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
        '    Dim i As Integer = Me.FocusPic.Tag - 1
        '    'Dim bmp As New Bitmap(Me.FocusPic.Image, Me.FocusPic.Width, Me.FocusPic.Height)
        '    Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)

        '    'Dim img As Image = DirectCast(myPicArClone(i), ExPictureBox).Image
        '    Dim g As Graphics = Graphics.FromImage(bmp)
        '    Dim x As Integer
        '    Dim y As Integer
        '    Dim wBmp As Integer = bmp.Width - 1
        '    Dim hBmp As Integer = bmp.Height - 1
        '    Dim keisuu As Integer = 8

        '    For x = 0 To bmp.Width / 2
        '        For y = 0 To bmp.Height / 2
        '            Dim oldC1 As Color = bmp.GetPixel(x, y)
        '            Dim oldC2 As Color = bmp.GetPixel(wBmp - x, y)
        '            Dim oldC3 As Color = bmp.GetPixel(x, hBmp - y)
        '            Dim oldC4 As Color = bmp.GetPixel(wBmp - x, hBmp - y)
        '            Dim a As Integer

        '            If x >= y Then
        '                a = (y + 1 / (x + 1)) * keisuu
        '            End If

        '            If a >= 255 Then
        '                a = 255
        '            End If
        '            Dim newC1 As Color = Color.FromArgb(a, oldC1)
        '            Dim newC2 As Color = Color.FromArgb(a, oldC2)
        '            Dim newC3 As Color = Color.FromArgb(a, oldC3)
        '            Dim newC4 As Color = Color.FromArgb(a, oldC4)

        '            bmp.SetPixel(x, y, newC1)
        '            bmp.SetPixel(wBmp - x, y, newC2)
        '            bmp.SetPixel(x, hBmp - y, newC3)
        '            bmp.SetPixel(wBmp - x, hBmp - y, newC4)
        '        Next
        '    Next

        '    g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
        '    Me.FocusPic.Image = bmp
        '    myPicArClone(FocusPic.Tag - 1).image = bmp 'グレースケール状態ををクローンに記録
        '    Call Transparent2()
        '    Me.Cursor = Cursors.Default
        'End If
    End Sub

    'グラデをかける範囲を指定して更に高速化した四辺からのグラデ
    Friend Sub Transparent4sides2()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            'Dim bmp As New Bitmap(Me.FocusPic.Image, Me.FocusPic.Width, Me.FocusPic.Height)
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)

            'Dim img As Image = DirectCast(myPicArClone(i), ExPictureBox).Image
            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim val As Decimal = myForm3.NumericUpDown4sides.Value / 100
            Dim range As Integer = bmp.Width * val

            If range <= 0 Then
                range = 1
            End If


            For x = 0 To bmp.Width / 2
                For y = 0 To bmp.Height / 2
                    Dim oldC1 As Color = bmp.GetPixel(x, y)
                    Dim oldC2 As Color = bmp.GetPixel(wBmp - x, y)
                    Dim oldC3 As Color = bmp.GetPixel(x, hBmp - y)
                    Dim oldC4 As Color = bmp.GetPixel(wBmp - x, hBmp - y)
                    Dim a As Integer

                    If x >= y Then
                        a = y * (255 / range)
                    End If

                    If a >= 255 Then
                        a = 255
                        Exit For 'だんだん濃くなって255を超えたら同じ列は変化しないから次の行(y)へ
                    End If

                    bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
                    bmp.SetPixel(wBmp - x, y, Color.FromArgb(a, oldC2))
                    bmp.SetPixel(x, hBmp - y, Color.FromArgb(a, oldC3))
                    bmp.SetPixel(wBmp - x, hBmp - y, Color.FromArgb(a, oldC4))
                Next
            Next

            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Me.ActExPic.Image = bmp

            Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            ClonePic.Image = bmp '半透明状態をクローンに記録

            'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
                Me.ActExPic.Name = Me.ActExPic.Name & "_T"
                ClonePic.Name = ClonePic.Name & "_T"
            End If


            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '透明グラデーション左が透明で右が不透明
    Friend Sub TransparentGradation()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim gRange As Single = myForm3.TrackBarTranspGradation.Value
            gRange = Math.Abs(gRange - 11) / 10
            For x = 0 To wBmp
                Dim a As Integer = (x * (255 / (bmp.Width * gRange))) + 1
                For y = 0 To hBmp
                    Dim oldC1 As Color = bmp.GetPixel(x, y)

                    If oldC1.A <> 0 Then 'もともと透明のところはそのまま
                        If a >= 255 Then
                            a = 255
                        End If

                        bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
                    End If

                Next
            Next

            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Me.ActExPic.Image = bmp

            Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            ClonePic.Image = bmp '半透明状態をクローンに記録

            'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
                Me.ActExPic.Name = Me.ActExPic.Name & "_T"
                ClonePic.Name = ClonePic.Name & "_T"
            End If

            Call Transparent4()
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '透明グラデーション左が透明で右が不透明TransparentGradationの改変
    '現在のアルファ値を考慮して新しいアルファ値を決める、重ねがけが有効
    Friend Sub TransparentGradation2()
        If myPicAr.Count <> 0 Then
            'Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim gRange As Single = myForm3.TrackBarTranspGradation.Value
            gRange = Math.Abs(gRange - 11) / 10
            Dim wBmpRange As Integer = bmp.Width * gRange - 1
            'Dim wBmpRange As Integer = wBmp * gRange - 1
            Dim tStrength As Single = myForm3.TrackBarTransparentStrength.Value
            tStrength = Math.Abs(tStrength - 11) / 10
            Dim allRange As Integer = Math.Round(bmp.Width * gRange, MidpointRounding.AwayFromZero)

            Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
            Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdata.Scan0
            Dim data As Integer = bmpdata.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
            'Dim val As Integer = wBmp * gRange - 1

            For x = 0 To wBmpRange 'wBmp * gRange - 1
                For y = 0 To hBmp
                    Dim pos As Integer = bmpdata.Stride * y + x * 4 + 3
                    If pixels(pos) <> 0 Then
                        'pixles(pos) = x * (oldC1.A / (bmp.Width * gRange))
                        'pixles(pos) = CByte(x * (pixles(pos) / (bmp.Width * gRange)))
                        'pixles(pos) = CByte(pixles(pos) * 0.0) + (pixles(pos) * (1 - 0.0) * x / (bmp.Width * gRange))
                        'If CByte(pixels(pos) * (1 - tStrength)) + (pixels(pos) * tStrength * x / (wBmp * gRange)) _
                        '    > CByte(255) Then
                        '    pixels(pos) = CByte(255)
                        'Else
                        '    pixels(pos) = CByte(pixels(pos) * (1 - tStrength)) + (pixels(pos) * tStrength * x / (bmp.Width * gRange))
                        'End If

                        'pixels(pos) = CByte(pixels(pos) * (1 - tStrength)) + (pixels(pos) * tStrength * x / (bmp.Width * gRange))
                        'pixels(pos) = CByte(pixels(pos) * (1 - tStrength)) + (pixels(pos) * tStrength * x / (wBmp * gRange))

                        pixels(pos) = CByte(pixels(pos) * (1 - tStrength)) + (pixels(pos) * tStrength * x / allRange)
                        If pixels(pos) = 0 Then
                            pixels(pos) = 1
                        End If
                    End If

                Next
            Next

            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            bmp.UnlockBits(bmpdata)


            'For x = 0 To wBmp * gRange
            '    'Dim a As Integer = (x * (255 / bmp.Width)) + 1
            '    For y = 0 To hBmp

            '        Dim oldC1 As Color = bmp.GetPixel(x, y)
            '        If oldC1.A <> 0 Then '元から0以外は0にしない
            '            Dim a As Integer = x * (oldC1.A / (bmp.Width * gRange))

            '            If a = 0 Then
            '                a = 1
            '            End If
            '            If a >= 255 Then
            '                a = 255
            '            End If

            '            bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
            '        End If
            '    Next
            'Next

            'g.DrawImage(bmp, rect)
            Me.ActExPic.Image = bmp

            Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            ClonePic.Image = bmp '半透明状態をクローンに記録

            'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
                Me.ActExPic.Name = Me.ActExPic.Name & "_T"
                ClonePic.Name = ClonePic.Name & "_T"
            End If

            Call Transparent4()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            'Me.Cursor = Cursors.Default
        End If
    End Sub
    '透明グラデーション左が透明で右が不透明TransparentGradationの改変
    '現在のアルファ値を考慮して新しいアルファ値を決める、重ねがけが有効
    Friend Function TransparentGradation2kai(ByVal bmp As Bitmap, Optional ByVal gRange As Single = 1, Optional ByVal tStrength As Single = 1) As Bitmap
        If myPicAr.Count <> 0 Then
            'Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            'Dim i As Integer = Me.FocusPic.Tag - 1
            'Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            'Dim gRange As Single = myForm3.TrackBarTranspGradation.Value
            'gRange = Math.Abs(gRange - 11) / 10
            Dim wBmpRange As Integer = bmp.Width * gRange - 1
            'Dim wBmpRange As Integer = wBmp * gRange - 1
            'Dim tStrength As Single = myForm3.TrackBarTransparentStrength.Value
            'tStrength = Math.Abs(tStrength - 11) / 10
            Dim allRange As Integer = Math.Round(bmp.Width * gRange, MidpointRounding.AwayFromZero)

            Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
            Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdata.Scan0
            Dim data As Integer = bmpdata.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            For x = 0 To wBmpRange 'wBmp * gRange - 1
                For y = 0 To hBmp
                    Dim pos As Integer = bmpdata.Stride * y + x * 4 + 3
                    If pixels(pos) <> 0 Then

                        pixels(pos) = CByte(pixels(pos) * (1 - tStrength)) + (pixels(pos) * tStrength * x / allRange)
                        If pixels(pos) = 0 Then
                            pixels(pos) = 1
                        End If
                    End If

                Next
            Next

            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            bmp.UnlockBits(bmpdata)


            'Me.FocusPic.Image = bmp

            'Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            'ClonePic.Image = bmp '半透明状態をクローンに記録

            ''ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            'If Me.FocusPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
            '    Me.FocusPic.Name = Me.FocusPic.Name & "_T"
            '    ClonePic.Name = ClonePic.Name & "_T"
            'End If

            'Call Transparent4()
            'Me.Cursor = Cursors.Default
        End If
        Return bmp

    End Function
    '左右からの透明Gradation
    '透明グラデーション左が透明で右が不透明TransparentGradationの改変
    '現在のアルファ値を考慮して新しいアルファ値を決める、重ねがけが有効
    Friend Sub TransparentGradation3()
        If myPicAr.Count <> 0 Then

            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim gRange As Single = myForm3.TrackBarTranspGradation.Value
            gRange = Math.Abs(gRange - 11) / 10
            Dim wBmpRange As Integer = wBmp / 2 * gRange + 1
            Dim leftRange As Integer = Math.Round((bmp.Width / 2) * gRange, MidpointRounding.AwayFromZero) '四捨五入'(wBmp / 2) * gRange - 1
            Dim rightRange As Integer = leftRange ' = wBmp - ((wBmp * gRange) / 2) + 1

            'If bmp.Width Mod 2 = 1 Then
            '    rightRange -= 1
            'End If

            Dim tStrength As Single = myForm3.TrackBarTransparentStrength.Value
            tStrength = Math.Abs(tStrength - 11) / 10
            Dim allRange As Integer = leftRange * gRange

            'If allRange <= 0 Then
            '    Exit Sub
            'End If

            Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
            Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdata.Scan0
            Dim data As Integer = bmpdata.Stride * bmp.Height - 1
            Dim pixles(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixles, 0, pixles.Length)

            'Dim Sw As New System.Diagnostics.Stopwatch
            'Sw.Start()

            For x = 0 To wBmp
                For y = 0 To hBmp
                    If x < leftRange Then '(wBmp / 2) * gRange - 1 Then
                        Dim pos As Integer = bmpdata.Stride * y + x * 4 + 3
                        If pixles(pos) <> 0 Then
                            Dim alphaLow As Integer = pixles(pos) * (1 - tStrength)
                            Dim alphaEach As Integer = (pixles(pos) - alphaLow) * (x / (leftRange)) ' (bmp.Width / leftRange)
                            pixles(pos) = alphaLow + alphaEach
                            If pixles(pos) = 0 Then
                                pixles(pos) = 1
                            End If
                        End If
                    ElseIf x >= bmp.Width - rightRange Then ' wBmp - ((wBmp * gRange) / 2) + 1 Then
                        Dim pos As Integer = bmpdata.Stride * y + x * 4 + 3
                        If pixles(pos) <> 0 Then
                            Dim alphaLow As Integer = pixles(pos) * (1 - tStrength)
                            Dim alphaEach As Integer = (pixles(pos) - alphaLow) * ((bmp.Width - 1 - x) / (leftRange)) ' (bmp.Width / leftRange)
                            pixles(pos) = CByte(alphaLow + alphaEach)

                            'pixles(pos) _
                            '= CByte(pixles(pos) * (1 - tStrength)) _
                            '+ (pixles(pos) * tStrength * (wBmp - x) / allRange)
                            'pixles(pos) = CByte(pixles(pos) * (1 - tStrength)) + (pixles(pos) * tStrength * (wBmp - x) / (wBmp / 2 * gRange))
                            If pixles(pos) = 0 Then
                                pixles(pos) = 1
                            End If
                        End If
                    End If
                Next
            Next
            '1000x1000で1.00秒

            'Sw.Stop()
            'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)


            System.Runtime.InteropServices.Marshal.Copy(pixles, 0, ptr, pixles.Length)
            bmp.UnlockBits(bmpdata)

            'For x = 0 To wBmp
            '    'Dim a As Integer = (x * (255 / bmp.Width)) + 1
            '    For y = 0 To hBmp

            '        Dim oldC1 As Color = bmp.GetPixel(x, y)
            '        If oldC1.A <> 0 Then '元から0以外は0にしない

            '            If x < (bmp.Width / 2) * gRange Then '左半分
            '                Dim a As Integer = x * (oldC1.A / ((bmp.Width / 2) * gRange))

            '                If a <= 0 Then
            '                    a = 1
            '                End If
            '                If a >= 255 Then
            '                    a = 255
            '                End If
            '                bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
            '            ElseIf x > bmp.Width - ((bmp.Width * gRange) / 2) Then

            '                '右半分
            '                Dim a As Integer = (bmp.Width - x) * (oldC1.A / ((bmp.Width / 2) * gRange))
            '                If a <= 0 Then
            '                    a = 1
            '                End If
            '                If a >= 255 Then
            '                    a = 255
            '                End If
            '                bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
            '            Else
            '                Dim a As Integer = oldC1.A
            '                bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
            '            End If
            '        End If
            '    Next
            'Next

            'g.DrawImage(bmp, rect)
            Me.ActExPic.Image = bmp

            Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            ClonePic.Image = bmp '半透明状態をクローンに記録

            'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
                Me.ActExPic.Name = Me.ActExPic.Name & "_T"
                ClonePic.Name = ClonePic.Name & "_T"
            End If

            Call Transparent4()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image

        End If
    End Sub
    '左右からの透明Gradation
    '透明グラデーション左が透明で右が不透明TransparentGradationの改変
    '現在のアルファ値を考慮して新しいアルファ値を決める、重ねがけが有効
    Friend Function TransparentGradation3kai(ByVal bmp As Bitmap, Optional ByVal gRange As Single = 1, Optional ByVal tStrength As Single = 1) As Bitmap
        If myPicAr.Count <> 0 Then

            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            'Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            'Dim gRange As Single = myForm3.TrackBarTranspGradation.Value
            'gRange = Math.Abs(gRange - 11) / 10
            Dim wBmpRange As Integer = wBmp / 2 * gRange + 1
            Dim leftRange As Integer = Math.Round((bmp.Width / 2) * gRange, MidpointRounding.AwayFromZero) '四捨五入'(wBmp / 2) * gRange - 1
            Dim rightRange As Integer = leftRange ' = wBmp - ((wBmp * gRange) / 2) + 1

            'If bmp.Width Mod 2 = 1 Then
            '    rightRange -= 1
            'End If

            'Dim tStrength As Single = myForm3.TrackBarTransparentStrength.Value
            'tStrength = Math.Abs(tStrength - 11) / 10
            Dim allRange As Integer = leftRange * gRange

            'If allRange <= 0 Then
            '    Exit Sub
            'End If

            Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
            Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdata.Scan0
            Dim data As Integer = bmpdata.Stride * bmp.Height - 1
            Dim pixles(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixles, 0, pixles.Length)

            'Dim Sw As New System.Diagnostics.Stopwatch
            'Sw.Start()

            For x = 0 To wBmp
                For y = 0 To hBmp
                    If x < leftRange Then '(wBmp / 2) * gRange - 1 Then
                        Dim pos As Integer = bmpdata.Stride * y + x * 4 + 3
                        If pixles(pos) <> 0 Then
                            Dim alphaLow As Integer = pixles(pos) * (1 - tStrength)
                            Dim alphaEach As Integer = (pixles(pos) - alphaLow) * (x / (leftRange)) ' (bmp.Width / leftRange)
                            pixles(pos) = alphaLow + alphaEach
                            If pixles(pos) = 0 Then
                                pixles(pos) = 1
                            End If
                        End If
                    ElseIf x >= bmp.Width - rightRange Then ' wBmp - ((wBmp * gRange) / 2) + 1 Then
                        Dim pos As Integer = bmpdata.Stride * y + x * 4 + 3
                        If pixles(pos) <> 0 Then 'かなり適当で255以上になる場合がある、わけわからん
                            Dim alphaLow As Integer = pixles(pos) * (1 - tStrength)
                            Dim alphaEach As Integer = (pixles(pos) - alphaLow) * ((bmp.Width - 1 - x) / (leftRange)) ' (bmp.Width / leftRange)
                            pixles(pos) = CByte(alphaLow + alphaEach)

                            'pixles(pos) _
                            '= CByte(pixles(pos) * (1 - tStrength)) _
                            '+ (pixles(pos) * tStrength * (wBmp - x) / allRange)
                            'pixles(pos) = CByte(pixles(pos) * (1 - tStrength)) + (pixles(pos) * tStrength * (wBmp - x) / (wBmp / 2 * gRange))
                            If pixles(pos) = 0 Then
                                pixles(pos) = 1
                            End If
                        End If
                    End If
                Next
            Next
            '1000x1000で1.00秒

            'Sw.Stop()
            'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)


            System.Runtime.InteropServices.Marshal.Copy(pixles, 0, ptr, pixles.Length)
            bmp.UnlockBits(bmpdata)

            'For x = 0 To wBmp
            '    'Dim a As Integer = (x * (255 / bmp.Width)) + 1
            '    For y = 0 To hBmp

            '        Dim oldC1 As Color = bmp.GetPixel(x, y)
            '        If oldC1.A <> 0 Then '元から0以外は0にしない

            '            If x < (bmp.Width / 2) * gRange Then '左半分
            '                Dim a As Integer = x * (oldC1.A / ((bmp.Width / 2) * gRange))

            '                If a <= 0 Then
            '                    a = 1
            '                End If
            '                If a >= 255 Then
            '                    a = 255
            '                End If
            '                bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
            '            ElseIf x > bmp.Width - ((bmp.Width * gRange) / 2) Then

            '                '右半分
            '                Dim a As Integer = (bmp.Width - x) * (oldC1.A / ((bmp.Width / 2) * gRange))
            '                If a <= 0 Then
            '                    a = 1
            '                End If
            '                If a >= 255 Then
            '                    a = 255
            '                End If
            '                bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
            '            Else
            '                Dim a As Integer = oldC1.A
            '                bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))
            '            End If
            '        End If
            '    Next
            'Next

            ''g.DrawImage(bmp, rect)
            'Me.FocusPic.Image = bmp

            'Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            'ClonePic.Image = bmp '半透明状態をクローンに記録

            ''ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            'If Me.FocusPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
            '    Me.FocusPic.Name = Me.FocusPic.Name & "_T"
            '    ClonePic.Name = ClonePic.Name & "_T"
            'End If

            'Call Transparent4()

        End If
        Return bmp
    End Function
    '左上からの斜めの透明Gradation
    '透明グラデーション左が透明で右が不透明TransparentGradationの改変
    '現在のアルファ値を考慮して新しいアルファ値を決める、重ねがけが有効
    Friend Sub TransparentGradation4()
        If myPicAr.Count <> 0 Then
            Dim Sw As New System.Diagnostics.Stopwatch
            Sw.Start()

            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim gradationRange As Single = myForm3.TrackBarTranspGradation.Value
            gradationRange = Math.Abs(gradationRange - 11) / 10

            For x = 0 To wBmp
                'Dim a As Integer = (x * (255 / bmp.Width)) + 1
                For y = 0 To hBmp

                    Dim oldC1 As Color = bmp.GetPixel(x, y)
                    '元から0とグラデの範囲以外は無視する
                    If oldC1.A <> 0 And x + y < (bmp.Height + bmp.Width) * gradationRange Then

                        Dim a As Integer
                        a = (x + y) * (oldC1.A / ((bmp.Width + bmp.Height - 2) * gradationRange))
                        If a = 0 Then
                            a = 1
                        End If
                        If a >= 255 Then
                            a = 255
                        End If
                        bmp.SetPixel(x, y, Color.FromArgb(a, oldC1))

                    End If
                Next
            Next

            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Sw.Stop()
            Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)
            Me.ActExPic.Image = bmp

            Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            ClonePic.Image = bmp '半透明状態をクローンに記録

            'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
                Me.ActExPic.Name = Me.ActExPic.Name & "_T"
                ClonePic.Name = ClonePic.Name & "_T"
            End If

            Call Transparent4()
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '左上からの斜めの透明Gradation、LockBitsバージョン、はやい(確信)
    '透明グラデーション左が透明で右が不透明TransparentGradationの改変
    '現在のアルファ値を考慮して新しいアルファ値を決める、重ねがけが有効
    'BitmapData.Stride プロパティ (System.Drawing.Imaging)
    'http://msdn.microsoft.com/ja-jp/library/system.drawing.imaging.bitmapdata.stride.aspx

    Friend Sub TransparentGradation4LockBit()
        If myPicAr.Count <> 0 Then

            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する

            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
            'Bitmapのロック
            Dim bmpDate As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim pixelSize As Integer = 4 'ピクセルサイズ決め打ち、32bppしか扱わないと思う
            Dim ptr As IntPtr = bmpDate.Scan0
            Dim byt As Integer = bmpDate.Stride * bmpDate.Height - 1
            Dim pixels(byt) As Byte
            'Dim pixels As Byte() = New Byte(bmpDate.Stride * bmp.Height - 1) {}'一行にまとめるとこう
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            Dim x As Integer = 0
            Dim y As Integer = 0
            Dim width As Integer = bmp.Width
            Dim height As Integer = bmp.Height
            Dim gradationRange As Single = myForm3.TrackBarTranspGradation.Value
            gradationRange = Math.Abs(gradationRange - 11) / 10
            Dim tStrength As Single = myForm3.TrackBarTransparentStrength.Value
            tStrength = Math.Abs(tStrength - 11) / 10
            Dim allRange As Integer = (width + height - 1) * gradationRange '全体距離

            Dim Sw As New System.Diagnostics.Stopwatch
            Sw.Start()

            Dim mycount As Integer = 0
            For y = 0 To height - 1

                For x = 0 To width - 1
                    Dim pos As Integer = (y * bmpDate.Stride + x * pixelSize) + 3 '＋3はアルファ値は4番目だから

                    If pixels(pos) <> 0 AndAlso x + y < (height + width - 1) * gradationRange Then

                        Dim minAlpha As Integer = (pixels(pos) * (1 - tStrength)) '最低値
                        Dim alphaEach As Single = (pixels(pos) - minAlpha) * ((x + y) / allRange) '距離ごとの変化値

                        '最低値＋ 距離ごとの変化値
                        '＝最低値＋ (現在値-最低値) * ((x+y)/全体距離)
                        'pixels(pos) = minAlpha + alphaEach
                        pixels(pos) = CByte(minAlpha + alphaEach)
                        If pixels(pos) = 0 Then
                            pixels(pos) = 1
                        End If

                        mycount += 1
                    End If

                Next
            Next
            Debug.WriteLine("処理ピクセル数" & mycount)
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            bmp.UnlockBits(bmpDate)

            Sw.Stop()
            Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)

            'g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Me.ActExPic.Image = bmp

            Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            ClonePic.Image = bmp '半透明状態をクローンに記録

            'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
                Me.ActExPic.Name = Me.ActExPic.Name & "_T"
                ClonePic.Name = ClonePic.Name & "_T"
            End If

            Call Transparent4()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image

            Me.Cursor = Cursors.Default
        End If
    End Sub
    '左上からの斜めの透明Gradation、LockBitsバージョン、はやい(確信)
    '透明グラデーション左が透明で右が不透明TransparentGradationの改変
    '現在のアルファ値を考慮して新しいアルファ値を決める、重ねがけが有効
    'BitmapData.Stride プロパティ (System.Drawing.Imaging)
    'http://msdn.microsoft.com/ja-jp/library/system.drawing.imaging.bitmapdata.stride.aspx

    Friend Function TransparentGradation4LockBitkai(ByVal bmp As Bitmap, Optional ByVal gRange As Single = 1, Optional ByVal tStrength As Single = 1) As Bitmap
        If myPicAr.Count <> 0 Then

            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する

            'Dim i As Integer = Me.FocusPic.Tag - 1
            'Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
            'Bitmapのロック
            Dim bmpDate As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim pixelSize As Integer = 4 'ピクセルサイズ決め打ち、32bppしか扱わないと思う
            Dim ptr As IntPtr = bmpDate.Scan0
            Dim byt As Integer = bmpDate.Stride * bmpDate.Height - 1
            Dim pixels(byt) As Byte
            'Dim pixels As Byte() = New Byte(bmpDate.Stride * bmp.Height - 1) {}'一行にまとめるとこう
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            Dim x As Integer = 0
            Dim y As Integer = 0
            Dim width As Integer = bmp.Width
            Dim height As Integer = bmp.Height
            Dim gradationRange = gRange
            'gradationRange = Math.Abs(gradationRange - 11) / 10
            Dim allRange As Integer = (width + height - 1) * gradationRange '全体距離
            'Dim tStrength As Single = myForm3.TrackBarTransparentStrength.Value
            'tStrength = Math.Abs(tStrength - 11) / 10

            Dim Sw As New System.Diagnostics.Stopwatch
            Sw.Start()

            Dim mycount As Integer = 0
            For y = 0 To height - 1

                For x = 0 To width - 1
                    Dim pos As Integer = (y * bmpDate.Stride + x * pixelSize) + 3 '＋3はアルファ値は4番目だから

                    If pixels(pos) <> 0 AndAlso x + y < (height + width - 1) * gradationRange Then

                        Dim minAlpha As Integer = (pixels(pos) * (1 - tStrength)) '最低値
                        Dim alphaEach As Single = (pixels(pos) - minAlpha) * ((x + y) / allRange) '距離ごとの変化値

                        '最低値＋ 距離ごとの変化値
                        '＝最低値＋ (現在値-最低値) * ((x+y)/全体距離)
                        'pixels(pos) = minAlpha + alphaEach
                        pixels(pos) = CByte(minAlpha + alphaEach)
                        If pixels(pos) = 0 Then
                            pixels(pos) = 1
                        End If

                        mycount += 1
                    End If

                Next
            Next
            Debug.WriteLine("処理ピクセル数" & mycount)
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            bmp.UnlockBits(bmpDate)

            Sw.Stop()
            Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)

            ''g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            'Me.FocusPic.Image = bmp

            'Dim ClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            'ClonePic.Image = bmp '半透明状態をクローンに記録

            ''ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
            'If Me.FocusPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
            '    Me.FocusPic.Name = Me.FocusPic.Name & "_T"
            '    ClonePic.Name = ClonePic.Name & "_T"
            'End If

            'Call Transparent4()
            'Me.Cursor = Cursors.Default
        End If
        Return bmp

    End Function
    'ガンマ補正
    Friend Sub Gamma()
        If myPicAr.Count <> 0 Then

            Dim myGamma As Single = myForm3.NumericUpDownGamma.Value

            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim g As Graphics = Graphics.FromImage(bmp)

            Dim ia As New ImageAttributes()
            ia.SetGamma(myGamma) 'ガンマ値をセット
            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height), 0, 0, bmp.Width, bmp.Height, GraphicsUnit.Pixel, ia)

            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'ガンマ補正をクローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image

        End If
    End Sub

    'LockBitsバージョンのガンマ値変更
    Friend Sub GammaLockBit()
        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim r As Single
        Dim g As Single
        Dim b As Single
        'Dim ntsc As Integer
        Dim gamma As Single = myForm3.NumericUpDownGammaLockBits.Value
        'Dim gamma2 As Single = myForm3.TrackBarGrayScaleGamma2.Value / 10


        Dim i As Integer = ActExPic.Tag - 1
        Dim bmp As New Bitmap(ActExPic.Image)
        Dim rw As Integer = bmp.Width
        Dim rh As Integer = bmp.Height
        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)

        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * rh - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        For x = 0 To rw - 1
            For y = 0 To rh - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4
                r = ((pixels(pos + 2) / 255) ^ (1 / gamma)) * 255
                g = ((pixels(pos + 1) / 255) ^ (1 / gamma)) * 255
                b = ((pixels(pos) / 255) ^ (1 / gamma)) * 255


                pixels(pos + 2) = CByte(r)
                pixels(pos + 1) = CByte(g)
                pixels(pos) = CByte(b)
            Next

        Next

        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)
        ActExPic.Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp
    End Sub

    '鮮やかさ
    Friend Sub SaturationLockBit()
        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim r As Single
        Dim g As Single
        Dim b As Single
        'Dim ntsc As Integer
        Dim sa As Single = myForm3.NumericUpDownSaturation.Value
        Dim dsa As Single = (1 - sa) / 2
        'Dim gamma2 As Single = myForm3.TrackBarGrayScaleGamma2.Value / 10


        Dim i As Integer = ActExPic.Tag - 1
        Dim bmp As New Bitmap(ActExPic.Image)
        Dim rw As Integer = bmp.Width
        Dim rh As Integer = bmp.Height
        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)

        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * rh - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        For x = 0 To rw - 1
            For y = 0 To rh - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4
                Dim red As Integer = pixels(pos + 2)
                Dim gre As Integer = pixels(pos + 1)
                Dim blu As Integer = pixels(pos)

                r = pixels(pos + 2) / 255
                g = pixels(pos + 1) / 255
                b = pixels(pos) / 255
                Dim valr As Single = ((r * sa + g * dsa + b * dsa)) * 255
                Dim valg As Single = ((r * dsa + g * sa + b * dsa)) * 255
                Dim valb As Single = ((r * dsa + g * dsa + b * sa)) * 255
                Dim valr2 As Single = (r * sa + g * dsa + b * dsa) * 255
                'Dim valr3 As Byte = CByte((r * sa + g * dsa + b * dsa) * 255)
                'Dim valr4 As Byte = CByte((r * sa + g * dsa + b * dsa)) * 255
                Dim r1 As Integer = (r * sa + g * dsa + b * dsa) * 255
                Dim g1 As Integer = (r * dsa + g * sa + b * dsa) * 255
                Dim b1 As Integer = (r * dsa + g * dsa + (b * sa)) * 255

                If r1 > 255 Then
                    r1 = 255
                ElseIf r1 < 0 Then
                    r1 = 0
                End If

                If g1 > 255 Then
                    g1 = 255
                ElseIf g1 < 0 Then
                    g1 = 0
                End If

                If b1 > 255 Then
                    b1 = 255
                ElseIf b1 < 0 Then
                    b1 = 0
                End If

                pixels(pos + 2) = CByte(r1)
                pixels(pos + 1) = g1
                pixels(pos) = b1


                'pixels(pos + 2) = CByte((r * sa + g * dsa + b * dsa) * 255)
                'pixels(pos + 1) = CByte((r * dsa + g * sa + b * dsa) * 255)
                'pixels(pos) = CByte((r * dsa + g * dsa + (b * sa)) * 255)

                'pixels(pos + 2) = CByte((r * sa + g * dsa + b * dsa)) * 255
                'pixels(pos + 1) = CByte((r * dsa + g * sa + b * dsa)) * 255
                'pixels(pos) = CByte((r * dsa + g * dsa + b * sa)) * 255
            Next

        Next

        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)
        ActExPic.Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp
    End Sub
    '白黒、ブライトネスを使ったもの
    Friend Sub Brightness1bpp()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            Dim Brightness As Single = myForm3.NumericUpDownBrightness1bpp.Value

            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1

            Dim pic As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            'Dim img As Image = DirectCast(myPicArClone(i), ExPictureBox).Image

            Dim bmp As New Bitmap(pic.Width, pic.Height, PixelFormat.Format1bppIndexed)
            Dim bmpimg As New Bitmap(pic)
            'Bitmapをロックする、らしい
            Dim bmpData As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.WriteOnly, bmp.PixelFormat)
            'ピクセルデータの作成、らしい
            Dim pixels As Byte() = New Byte(bmpData.Stride * bmpData.Height - 1) {}
            For y As Integer = 0 To bmpData.Height - 1
                For x As Integer = 0 To bmpData.Width - 1
                    '明るさが0.5以上の時は白くする
                    If Brightness <= bmpimg.GetPixel(x, y).GetBrightness() Then
                        Dim pos As Integer = (x >> 3) + bmpData.Stride * y      'ピクセルデータの位置
                        pixels(pos) = pixels(pos) Or CByte(&H80 >> (x And &H7)) '白くする
                    End If
                Next
            Next
            '作成したピクセルデータをコピーする、らしい
            Dim ptr As IntPtr = bmpData.Scan0
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            'ロックを解除する
            bmp.UnlockBits(bmpData)

            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'ガンマ補正をクローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '白黒、ランダムのブライトネスを使ったもの
    Friend Sub BrightnessRandom1bpp()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            Dim randomB As New Random()


            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1

            Dim pic As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            'Dim img As Image = DirectCast(myPicArClone(i), ExPictureBox).Image

            Dim bmp As New Bitmap(pic.Width, pic.Height, PixelFormat.Format1bppIndexed)
            Dim bmpimg As New Bitmap(pic)
            'Bitmapをロックする、らしい
            Dim bmpData As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.WriteOnly, bmp.PixelFormat)
            'ピクセルデータの作成、らしい
            Dim pixels As Byte() = New Byte(bmpData.Stride * bmpData.Height - 1) {}
            For y As Integer = 0 To bmpData.Height - 1
                For x As Integer = 0 To bmpData.Width - 1
                    '明るさが0.5以上の時は白くする
                    If randomB.NextDouble() <= bmpimg.GetPixel(x, y).GetBrightness() Then
                        Dim pos As Integer = (x >> 3) + bmpData.Stride * y      'ピクセルデータの位置
                        pixels(pos) = pixels(pos) Or CByte(&H80 >> (x And &H7)) '白くする
                    End If
                Next
            Next
            '作成したピクセルデータをコピーする、らしい
            Dim ptr As IntPtr = bmpData.Scan0
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            'ロックを解除する
            bmp.UnlockBits(bmpData)

            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'ガンマ補正をクローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '右に90度回転
    Friend Sub RotateRight90()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            bmp.RotateFlip(RotateFlipType.Rotate90FlipNone) '右に90度回転

            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'クローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image

            Me.Cursor = Cursors.Default
        End If
    End Sub

    '左に90度回転
    Friend Sub RotateLeft90()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            bmp.RotateFlip(RotateFlipType.Rotate270FlipNone) '右に90度回転

            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'クローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '左右反転
    Friend Sub RotateFlipX()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            bmp.RotateFlip(RotateFlipType.RotateNoneFlipX) '右に90度回転

            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'クローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '回転、画像を回転させると大きさも変えないとはみ出してしまうので失敗したけど
    '書きなおした
    Friend Sub RotateAngle()
        If myPicAr.Count <> 0 Then


            ''Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            'Dim i As Integer = Me.FocusPic.Tag - 1
            'Dim bmp As New Bitmap(FocusPic.Width, FocusPic.Height)
            'Dim g As Graphics = Graphics.FromImage(bmp)
            'Dim img As Image = DirectCast(myPicArClone(i), ExPictureBox).Image
            'g.RotateTransform(30.0F)

            'g.DrawImage(img, New Rectangle(0, 0, img.Width, img.Height))
            'Me.FocusPic.Image = bmp
            'myPicArClone(FocusPic.Tag - 1).image = bmp 'クローンに記録

            '回転
            Dim sAngle As Single = -myForm3.NumericUpDownPicAngle.Value

            If sAngle = 0 OrElse sAngle = 360 Then
                Exit Sub

            End If



            Dim i As Integer = ActExPic.Tag - 1
            'Dim bmp As New Bitmap(FocusPic.Image)
            Dim bmp As New Bitmap(DirectCast(myPicArBackup(i), ExPictureBox).Image)
            'Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            'Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
            If sAngle < -90 And sAngle >= -180 Then
                bmp.RotateFlip(RotateFlipType.Rotate270FlipNone)
                sAngle = sAngle + 90
            ElseIf sAngle < -180 And sAngle >= -270 Then
                bmp.RotateFlip(RotateFlipType.Rotate180FlipNone)
                sAngle = sAngle + 180
            ElseIf sAngle < -270 Then
                bmp.RotateFlip(RotateFlipType.Rotate90FlipNone)
                sAngle = sAngle + 270
            ElseIf sAngle > 90 And sAngle <= 180 Then
                bmp.RotateFlip(RotateFlipType.Rotate90FlipNone)
                sAngle -= 90
            ElseIf sAngle > 180 And sAngle <= 270 Then
                bmp.RotateFlip(RotateFlipType.Rotate180FlipNone)
                sAngle -= 180
            ElseIf sAngle > 270 Then
                bmp.RotateFlip(RotateFlipType.Rotate270FlipNone)
                sAngle -= 270
            End If
            Dim width As Integer = bmp.Width
            Dim height As Integer = bmp.Height

            Dim d As Double = sAngle / (180 / Math.PI)
            Dim xx As Single
            Dim yy As Single
            Dim x1 As Single = width * CSng(Math.Cos(d))
            Dim y1 As Single = width * CSng(Math.Sin(d))
            'Dim x2 As Single = xx + bmp.Height * CSng(Math.Sin(d))
            Dim x2 As Single = -height * CSng(Math.Sin(d))
            Dim y2 As Single = height * CSng(Math.Cos(d))

            If y1 <= 0 Then
                y1 = Math.Abs(y1)
            End If
            Dim destnationPoint As Point() '画像を描画する座標3点
            Dim canvas As Bitmap '空のBitmapで大きさを決めてこれに上で書いた文字を回転して描画する
            If sAngle < 0 Then '角度がマイナスの場合
                destnationPoint = {New Point(xx, y1), New Point(x1, yy), New Point(x2, y1 + y2)} '左上、右上、左下
                canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))
            Else
                destnationPoint = {New Point(-x2, yy), New Point(x1 + Math.Abs(x2), y1), New Point(xx, y2)}
                canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))
            End If

            Dim g2 As Graphics = Graphics.FromImage(canvas)
            'g2.SmoothingMode = SmoothingMode.HighQuality 'アンチエイリアスは意味ないみたい
            'g2.InterpolationMode = InterpolationMode.NearestNeighbor
            g2.PixelOffsetMode = PixelOffsetMode.Half

            Dim mode = g2.InterpolationMode

            g2.DrawImage(bmp, destnationPoint)

            Dim name As String = ActExPic.Name
            'Call PicBoxAdd(name, bmp)

            DirectCast(myPicAr(i), ExPictureBox).Image = canvas
            DirectCast(myPicArClone(i), ExPictureBox).Image = canvas
            'DirectCast(myPicArBackup(i), ExPictureBox).Image = bmp

            Call Transparent4()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image

        End If
    End Sub


    '文字列の描画
    Friend Sub StringDraw4()

        ''文字が入力されていなかったら何もしない
        'Dim bmp As New Bitmap(10, 10) '適当なbitmap

        'Dim mySt As String = myForm3.ComboBoxString.Text
        'Dim name As String = "文字_" & myForm3.ComboBoxString.Text
        'If mySt = "" Then
        '    Exit Sub
        'End If

        'bmp = StringDraw5(mySt)

        'Call PicBoxAdd(name, bmp)
     
    End Sub
    '文字列の描画 Function！！！！！
    'StringDraw4の改変
    'Friend Function StringDraw5(ByVal bmp As Bitmap, ByVal mySt As String) As Bitmap
    '未使用、複数行列に移行した
    'Friend Function StringDraw5(ByVal mySt As String) As Bitmap
        'Dim bmp As New Bitmap(10, 10) '適当なbitmap
        'Dim g As Graphics = Graphics.FromImage(bmp)
        ''Dim mySt As String = myForm3.ComboBoxString.Text
        ''Dim name As String = "文字_" & myForm3.ComboBoxString.Text
        'Dim myFSize As Integer = myForm3.NumericUpDownFontSize.Value
        ''Dim myBrush As New SolidBrush(myForm3.ButtonFontColor.ForeColor)
        'Dim myBrush As New SolidBrush(myForm3.PictureBoxTextColor1.BackColor)
        'Dim sf As New StringFormat
        'Dim shadow As Boolean
        'Dim shadowB As New SolidBrush(myForm3.ButtonStringShadowColor.ForeColor) '影の色
        'Dim fringeB As New SolidBrush(myForm3.ButtonStringFringeColor2Kako.ForeColor) '縁取りの色
        ''影の有無
        'If myForm3.CheckBoxStringShadow.Checked Then
        '    shadow = True
        'Else
        '    shadow = False
        'End If


        ' ''文字が入力されていなかったら何もしない
        ''If mySt = "" Then
        ''    Exit Function
        ''End If


        'If myForm3.RadioButtonStringV.Checked Then '縦書のとき
        '    sf.FormatFlags = StringFormatFlags.DirectionVertical
        'End If

        'Try

        '    Dim fName As New FontFamily(myForm3.ComboBoxAllFonts.SelectedItem.ToString)
        '    Dim myFont As New Font(fName, myFSize)
        '    'フォント名とサイズを指定してフォントオブジェクト作成
        '    If myForm3.CheckBoxTextItalic.Checked AndAlso myForm3.CheckBoxTextBold.Checked Then
        '        myFont = New Font(fName, myFSize, FontStyle.Italic Or FontStyle.Bold) '太字＆斜体
        '    ElseIf myForm3.CheckBoxTextBold.Checked Then
        '        myFont = New Font(fName, myFSize, FontStyle.Bold) '太字

        '    ElseIf myForm3.CheckBoxTextItalic.Checked Then
        '        myFont = New Font(fName, myFSize, FontStyle.Italic) '斜体

        '    End If


        '    g.DrawString(mySt, myFont, myBrush, 0, 0, sf) '縦横幅を図るために一度描画

        '    '縦横幅測定、3000ドットはとりあえずでこれを超える文字幅だと切り捨てられる
        '    Dim strinSize As SizeF = g.MeasureString(mySt, myFont, 3000, sf)
        '    bmp = New Bitmap(CType(strinSize.Width, Integer), CType(strinSize.Height, Integer)) '測定した値でBitmap作成
        '    Dim rw As Integer = bmp.Width
        '    Dim rh As Integer = bmp.Height

        '    g = Graphics.FromImage(bmp)


        '    '背景色を塗りつぶし
        '    If myForm3.CheckBoxTextBackColor背景色の有無.Checked Then

        '        Dim backTransparent As Integer = myForm3.NumericUpDownTextTransparent1.Value
        '        Dim bCol As Color = Color.FromArgb(backTransparent, myForm3.ButtonTextBackColor1.ForeColor)
        '        Dim bCol2 As Color = Color.FromArgb(backTransparent, myForm3.ButtonTextBackColor2.ForeColor)

        '        'アンチエイリアスは効果が出ない、なんで？
        '        If myForm3.NumericUpDownStringAngle.Value = 0 AndAlso myForm3.RadioButtonStringBackSquare.Checked Then
        '            g.SmoothingMode = SmoothingMode.None
        '        Else
        '            'g.PixelOffsetMode = PixelOffsetMode.Half
        '            g.SmoothingMode = SmoothingMode.AntiAlias
        '            'g.InterpolationMode = InterpolationMode.HighQualityBicubic
        '        End If


        '        '背景グラデーション無し
        '        If myForm3.CheckBoxTextBackC_Gradation.Checked = False Then

        '            Dim backBrush As New SolidBrush(bCol)

        '            If myForm3.RadioButtonStringBackSquare.Checked Then
        '                '背景四角
        '                g.FillRectangle(backBrush, 0, 0, rw, rh)
        '            ElseIf myForm3.RadioButtonStringBackEllipse.Checked Then
        '                '背景楕円
        '                g.FillEllipse(backBrush, 0, 0, rw, rh)
        '            ElseIf myForm3.RadioButtonStringBackRoundrect.Checked Then
        '                '背景角丸

        '                Dim diameter As Single = myForm3.TrackBarStringBackRoundrect.Value
        '                diameter = (Math.Min(rw, rh) - 1) * (diameter / 100)
        '                Dim gp As New GraphicsPath
        '                Dim xOff As Single = rw - 1 - diameter
        '                Dim yOff As Single = rh - 1 - diameter
        '                Dim dRect As New RectangleF(0, 0, diameter, diameter)
        '                gp.AddArc(0, 0, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        '                gp.AddArc(xOff, 0, diameter, diameter, 270, 90)
        '                gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
        '                gp.AddArc(0, yOff, diameter, diameter, 90, 90)
        '                gp.CloseFigure()
        '                g.FillPath(backBrush, gp)
        '            ElseIf myForm3.RadioButtonStringBackFrameRoundRect.Checked Then
        '                '角丸枠
        '                Dim diameter As Single = myForm3.TrackBarStringBackRoundrect.Value
        '                Dim col As Color = myForm3.ButtonTextBackColor1.ForeColor
        '                Dim penW As Integer = 2
        '                'Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value'回転はこっちでやるから0
        '                bmp = FrameRoundRectAdd2(rw, rh, diameter, col, col, col, penW, 0, False)
        '                g = Graphics.FromImage(bmp)
        '                'g.DrawImage(bmp, 0, 0)
        '            End If

        '            backBrush.Dispose()

        '            '背景グラデーション有り
        '        ElseIf myForm3.CheckBoxTextBackC_Gradation.Checked Then
        '            Dim backColor1 As Color = bCol
        '            Dim backColor2 As Color = bCol2
        '            Dim angle As Integer = myForm3.NumericUpDownStringBackGradAngle文字背景グラデ角度.Value
        '            'Dim gradationBrush As LinearGradientBrush
        '            Dim rect As New Rectangle(0, 0, rw, rh)
        '            Dim gradationBrush As New LinearGradientBrush(rect, backColor1, backColor2, angle)
        '            rect = New Rectangle(0, 0, rw, rh)

        '            'Dim gradationBrush As New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.Horizontal)
        '            '背景グラデーション方向
        '            'If myForm3.RadioButtonTextBackC_Horizontal.Checked Then
        '            '    gradationBrush = New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.Horizontal)
        '            '    'g.FillRectangle(gradationBrush, rect)
        '            'ElseIf myForm3.RadioButtonTextBackC_Vertical.Checked Then
        '            '    gradationBrush = New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.Vertical)
        '            '    'g.FillRectangle(gradationBrush, rect)
        '            'ElseIf myForm3.RadioButtonTextBackC_LeftUp.Checked Then
        '            '    gradationBrush = New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.ForwardDiagonal)
        '            '    'g.FillRectangle(gradationBrush, rect)
        '            'ElseIf myForm3.RadioButtonTextBackC_RightUp.Checked Then
        '            '    gradationBrush = New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.BackwardDiagonal)
        '            '    'g.FillRectangle(gradationBrush, rect)
        '            'End If

        '            'ガンマ補正
        '            If myForm3.CheckBoxTextBackC_Gamma.Checked Then
        '                gradationBrush.GammaCorrection = True
        '            Else
        '                gradationBrush.GammaCorrection = False
        '            End If
        '            '塗りつぶし

        '            If myForm3.RadioButtonStringBackSquare.Checked Then
        '                g.FillRectangle(gradationBrush, rect)
        '            ElseIf myForm3.RadioButtonStringBackEllipse.Checked Then
        '                g.FillEllipse(gradationBrush, rect)
        '            ElseIf myForm3.RadioButtonStringBackRoundrect.Checked Then
        '                '背景角丸
        '                'Dim rw As Integer = bmp.Width
        '                'Dim rh As Integer = bmp.Height
        '                Dim diameter As Single = myForm3.TrackBarStringBackRoundrect.Value
        '                diameter = (Math.Min(rw, rh) - 1) * (diameter / 100)
        '                Dim gp As New GraphicsPath
        '                Dim xOff As Single = rw - 1 - diameter
        '                Dim yOff As Single = rh - 1 - diameter
        '                Dim dRect As New RectangleF(0, 0, diameter, diameter)
        '                gp.AddArc(0, 0, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        '                gp.AddArc(xOff, 0, diameter, diameter, 270, 90)
        '                gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
        '                gp.AddArc(0, yOff, diameter, diameter, 90, 90)
        '                gp.CloseFigure()
        '                g.FillPath(gradationBrush, gp)
        '            ElseIf myForm3.RadioButtonStringBackFrameRoundRect.Checked Then
        '                '角丸枠
        '                Dim diameter As Single = myForm3.TrackBarStringBackRoundrect.Value
        '                Dim col As Color = myForm3.ButtonTextBackColor1.ForeColor
        '                Dim col2 As Color = myForm3.ButtonTextBackColor2.ForeColor
        '                Dim penW As Integer = 2
        '                'Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value'回転はこっちでやるから0
        '                bmp = FrameRoundRectAdd2(rw, rh, diameter, col, col2, col, penW, 0, True)
        '                g = Graphics.FromImage(bmp)
        '            End If
        '            gradationBrush.Dispose()

        '        End If



        '    End If



        '    If myForm3.CheckBoxStringAntiAlias.Checked Then 'アンチエイリアス
        '        g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        '    Else
        '        g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        '    End If

        '    '縁取り
        '    If myForm3.NumericUpDownStringFringe2.Value > 0 Then
        '        Dim i As Integer
        '        Dim j As Integer
        '        Dim bold As Integer = myForm3.NumericUpDownStringFringe2.Value
        '        '横書き
        '        If myForm3.RadioButtonStringH.Checked Then
        '            For i = -bold To bold
        '                For j = -bold To bold
        '                    g.DrawString(mySt, myFont, fringeB, i, (myFSize / 10) + j, sf)
        '                Next
        '            Next
        '        End If

        '        '縦書
        '        If myForm3.RadioButtonStringV.Checked Then
        '            For i = -bold To bold
        '                For j = -bold To bold
        '                    g.DrawString(mySt, myFont, fringeB, -(myFSize / 10) + i, j, sf)
        '                Next
        '            Next
        '        End If
        '    End If


        '    '文字の2色グラデーション方向設定
        '    If myForm3.CheckBoxTextGradation.Checked Then
        '        'Dim color1 As Color = myForm3.ButtonFontColor.ForeColor
        '        'Dim color2 As Color = myForm3.ButtonFontColor2.ForeColor
        '        Dim color1 As Color = myForm3.PictureBoxTextColor1.BackColor
        '        Dim color2 As Color = myForm3.PictureBoxTextColor2.BackColor

        '        Dim angle As Integer = myForm3.NumericUpDownStringGradientAngle.Value
        '        Dim gradationBrush = New LinearGradientBrush(New Rectangle(0, 0, bmp.Width, bmp.Height), color1, color2, angle)

        '        '文字のグラデーションガンマ補正
        '        If myForm3.CheckBoxStringGamma.Checked Then
        '            gradationBrush.GammaCorrection = True
        '        End If

        '        '縁取り



        '        '本番の文字列描画,グラデーションあり
        '        'Dim gradationBrush As New LinearGradientBrush(New Point(0, 0), New Point(bmp.Width, 0), color1, color2)
        '        If myForm3.RadioButtonStringV.Checked Then '縦書
        '            If shadow Then '影あり
        '                g.DrawString(mySt, myFont, shadowB, -(myFSize / 10) + 1, 1, sf)
        '            End If
        '            g.DrawString(mySt, myFont, gradationBrush, -(myFSize / 10), 0, sf)
        '        ElseIf myForm3.RadioButtonStringH.Checked Then
        '            If shadow Then '影あり
        '                g.DrawString(mySt, myFont, shadowB, 1, (myFSize / 10) + 1, sf)
        '            End If
        '            g.DrawString(mySt, myFont, gradationBrush, 0, (myFSize / 10), sf)
        '        End If
        '    Else
        '        '本番の文字列描画グラデーションなし
        '        If myForm3.RadioButtonStringV.Checked Then '縦書
        '            If shadow Then '影あり
        '                g.DrawString(mySt, myFont, shadowB, -(myFSize / 10) + 1, 1, sf)
        '            End If
        '            g.DrawString(mySt, myFont, myBrush, -(myFSize / 10), 0, sf)
        '        ElseIf myForm3.RadioButtonStringH.Checked Then '横書き

        '            If shadow Then '影あり
        '                g.DrawString(mySt, myFont, shadowB, 1, (myFSize / 10) + 1, sf)
        '            End If

        '            g.DrawString(mySt, myFont, myBrush, 0, (myFSize / 10), sf)

        '        End If
        '    End If
        '    shadowB.Dispose()
        '    fringeB.Dispose()

        '    'Call PicBoxAdd(name, bmp)

        'Catch ex As Exception

        'End Try

        ''回転

        'If myForm3.CheckBoxStringAngle.Checked Then

        '    Dim rAngle As Single = -myForm3.NumericUpDownStringAngle.Value
        '    bmp = PicAngle(bmp, rAngle)
        'End If

        'Return bmp

        ''フォント名が非共有メンバを参照するには、オブジェクト参照が必要ですになるけど動く
        'End Function
    Friend Function StringDraw6(ByVal iString As String, ByVal iFont As Font, ByVal sf As StringFormat) As Bitmap
        Dim bmp As New Bitmap(1, 1)
        Dim g As Graphics = Graphics.FromImage(bmp)
        'g.DrawString(iString, iFont, Brushes.Red, 0, 0, sf)
        Dim iSize As SizeF = g.MeasureString(iString, iFont, New PointF(0, 0), sf)
        Dim isize2 As SizeF = g.MeasureString(iString, iFont, New Point(0, 30), sf)

        bmp = New Bitmap(CInt(iSize.Width), CInt(iSize.Height))

        g.Dispose()

        'g = Graphics.FromImage(bmp)
        'g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        'g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel 'アンチエイリアスなし

        'g.DrawRectangle(Pens.Red, 0, 0, bmp.Width - 1, bmp.Height - 1)
        'g.DrawString(iString, iFont, Brushes.Black, 0, 7, sf)

        Return bmp
    End Function
    '文字の影
    Friend Function StringShadow(ByVal bmp As Bitmap, ByVal iString As String, ByVal iFont As Font, ByVal sf As StringFormat, _
                                 ByVal shadowB As SolidBrush, ByVal offS As Integer, ByVal sx As Integer, ByVal sy As Integer, ByVal antiA As Boolean, ByVal stringV As Boolean)
        Dim g As Graphics = Graphics.FromImage(bmp)

        'アンチエイリアス判定
        If antiA Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        End If

        '縦書判定して影の描画
        If stringV = False Then
            g.DrawString(iString, iFont, shadowB, sx, sy + offS, sf)
        Else
            g.DrawString(iString, iFont, shadowB, sx - offS, sy, sf)
        End If

        Return bmp

    End Function

    '
    Friend Function StringDim(ByVal bmp As Bitmap, ByVal iString As String, ByVal iFont As Font, ByVal sf As StringFormat, _
                                 ByVal dimB As Brush, ByVal offS As Integer, ByVal d As Integer, _
                                 ByVal antiA As Boolean, ByVal stringV As Boolean)
        Dim g As Graphics = Graphics.FromImage(bmp)

        'アンチエイリアス判定
        If antiA Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        End If

        '縦書判定して影の描画
        If stringV = False Then

            'For i As Integer = -2 To 2
            '    For j As Integer = -2 To 2
            '        g.DrawString(iString, iFont, dimB, i, j + offS, sf)

            '    Next
            'Next

            For i As Integer = -d To d
                For j As Integer = -d To d
                    g.DrawString(iString, iFont, dimB, i, j + offS, sf)

                Next
            Next

        Else
            For i As Integer = -d To d
                For j As Integer = -d To d
                    g.DrawString(iString, iFont, dimB, i - offS, j, sf)

                Next
            Next

        End If

        Return bmp

    End Function

    Friend Function StringFringe4(ByVal bmp As Bitmap, ByVal iString As String, ByVal iFont As Font, ByVal sf As StringFormat, _
                                 ByVal fringeB As SolidBrush, ByVal offS As Integer, ByVal bold As Integer, _
                                 ByVal antiA As Boolean, ByVal stringV As Boolean)
        Dim g As Graphics = Graphics.FromImage(bmp)
        'アンチエイリアス判定
        If antiA Then
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Else
            g.TextRenderingHint = Drawing.Text.TextRenderingHint.SingleBitPerPixel
        End If

        Dim i As Integer
        Dim j As Integer

        '縦書判定
        If stringV = False Then
            For i = -bold To bold
                For j = -bold To bold
                    g.DrawString(iString, iFont, fringeB, i, j + offS, sf)
                Next
            Next

        Else
            For i = -bold To bold
                For j = -bold To bold
                    g.DrawString(iString, iFont, fringeB, -offS + i, j, sf)
                Next
            Next

        End If

        Return bmp

    End Function


    Friend Function DrawStringLocate(ByVal iFont As Font) As Integer
        'イマイチ、失敗、未使用決定
        '文字の描画の縦の位置を考えてみたけど今までどおりフォントサイズの１/10ずらしたほうが楽+
        'MeiryoUIは名前で判定してずらさないようにする
        Dim ff As FontFamily = iFont.FontFamily
        Dim fStyle As FontStyle = iFont.Style

        Dim emH As Integer = ff.GetEmHeight(fStyle)
        Dim Ase As Integer = ff.GetCellAscent(fStyle)
        Dim Dese As Integer = ff.GetCellDescent(fStyle)
        Dim Spa As Integer = ff.GetLineSpacing(fStyle)
        Dim fSize As Integer = iFont.SizeInPoints
        Dim fHight As Integer = iFont.Height
        Dim val As Single
        val = 1 - (Ase / emH)
        If val < 0 Then
            val = 0
            Return val
            Exit Function
        End If
        'val = fHight * val
        val *= fSize

        Return val


    End Function
    '選択画像の文字の書式だけ入れ替える
    Friend Sub StringDrawShift()
        '選択画像が文字以外なら何もしない
        'If myPicAr.Count = 0 OrElse Not ActExPic.Name.StartsWith("文字_") Then
        '    Exit Sub
        'End If
        If myPicAr.Count = 0 Then Exit Sub
        If ActExPic.IsDrawString = False Then Exit Sub

        Dim str As String = ActExPic.DrawString描画文字
        If str = "" Then Exit Sub

        Dim bmp As Bitmap = myForm3.GetDrawStringAll文字と背景画像作成(str, myForm3.CreateFontフォント作成())

        Dim i As Integer = ActExPic.Tag - 1
        DirectCast(myPicAr(i), ExPictureBox).Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp
        DirectCast(myPicArBackup(i), ExPictureBox).Image = bmp 'これが迷う、有効にすれば決定画像になり元に戻らなくなる
        Call Transparent4()
        'Me.CurrentPic.Image = DirectCast(myPicArClone(i), ExPictureBox).Image
        Call UpdateThumbnail()

        'Catch ex As Exception

        'End Try
    End Sub
    '選択画像の文字の書式だけ入れ替える
    Friend Sub StringDrawShift2()
        '選択画像が文字以外なら何もしない
        If myPicAr.Count = 0 OrElse Not ActExPic.Name.StartsWith("文字_") Then
            Exit Sub
        End If

        Dim bmp As New Bitmap(10, 10) '適当なbitmap
        Dim mySt As String

        mySt = ActExPic.Name.Substring(3, ActExPic.Name.Length - 5)

        bmp = myFormText.DrawString(mySt)

        Dim i As Integer = ActExPic.Tag - 1
        DirectCast(myPicAr(i), ExPictureBox).Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp
        DirectCast(myPicArBackup(i), ExPictureBox).Image = bmp 'これが迷う、有効にすれば決定画像になり元に戻らなくなる
        Call Transparent4()
        'Me.CurrentPic.Image = DirectCast(myPicArClone(i), ExPictureBox).Image
        Call UpdateThumbnail()

    End Sub
    Friend Sub TextSample()
        If IsFontSettingNow = True Then Exit Sub '設定ファイルから設定中は更新しない
        Dim str As String = "見本"
        Dim myFont As Font
        Dim fName As String = myForm3.ComboBoxAllFonts.Text
        Dim fSize As Integer = myForm3.NumericUpDownFontSize.Value

        Dim fStyle As FontStyle
        If myForm3.CheckBoxTextItalic.Checked AndAlso myForm3.CheckBoxTextBold.Checked Then
            fStyle = FontStyle.Bold Or FontStyle.Italic Or FontStyle.Bold
        ElseIf myForm3.CheckBoxTextItalic.Checked Then
            fStyle = FontStyle.Italic
        ElseIf myForm3.CheckBoxTextBold.Checked Then
            fStyle = FontStyle.Bold
        End If
        myFont = New Font(fName, fSize, fStyle)

        Dim bmp As Bitmap = myForm3.GetDrawStringAll文字と背景画像作成(str, myFont)
        myForm3.PictureBoxTextSample.Image = bmp 'Form3に見本の表示

        Dim w As Integer = bmp.Width
        Dim h As Integer = bmp.Height

        If myForm3.RadioButtonStringH.Checked Then
            myForm3.LabelDrawStringSize.Text = "高さ" & h
        ElseIf myForm3.RadioButtonStringV.Checked Then
            myForm3.LabelDrawStringSize.Text = "幅" & w
        End If



        '文字の描画ウィンドウ用
        'Dim tBmp As New Bitmap(10, 10) '適当なbitmap
        'Dim iSt As String = "見本"

        'Dim iName As String = "見本" 
        'tBmp = StringDraw5(iSt)
        'fName = myFormText.ComboBoxAllFonts.Text
        'fSize = myFormText.NumericUpDownFontSize.Value
        'If myFormText.CheckBoxTextItalic.Checked And myFormText.CheckBoxTextBold.Checked Then
        '    fStyle = FontStyle.Bold Or FontStyle.Italic

        'End If
        'myFont = New Font(fName, fSize, fStyle)
        'tBmp = StringDraw6(iSt, myFont, fStyle)

        'myFormText.PictureBoxTextSample.Image = tBmp

        'Dim tw As Integer = tBmp.Width
        'Dim th As Integer = tBmp.Height
        'If myForm3.RadioButtonStringH.Checked Then
        '    myFormText.LabelDrawStringSize.Text = "高さ" & th
        'ElseIf myForm3.RadioButtonStringV.Checked Then
        '    myFormText.LabelDrawStringSize.Text = "幅" & tw
        'End If



    End Sub
    Friend Sub TextSample2()

        Dim bmp As New Bitmap(10, 10) '適当なbitmap

        Dim mySt As String ' = myForm3.ComboBoxString.Text
        Dim name As String ' = myForm3.ComboBoxString.Text

        mySt = "見本."
        name = mySt

        Dim w As Integer '= bmp.Width
        Dim h As Integer '= bmp.Height
        Dim sf As New StringFormat
        '縦横判定
        'Dim stringV As Boolean = False
        If Me.myFormText.RadioButtonStringV.Checked Then
            sf.FormatFlags = StringFormatFlags.DirectionVertical '縦書
            'stringV = True
        End If

        Dim fStyle As FontStyle '斜体、太字
        If Me.myFormText.CheckBoxTextItalic.Checked AndAlso Me.myFormText.CheckBoxTextBold.Checked Then
            fStyle = FontStyle.Italic Or FontStyle.Bold
        ElseIf Me.myFormText.CheckBoxTextBold.Checked Then
            fStyle = FontStyle.Bold
        ElseIf Me.myFormText.CheckBoxTextItalic.Checked Then
            fStyle = FontStyle.Italic
        End If

        Dim iFont As Font
        iFont = New Font(Me.myFormText.ComboBoxAllFonts.Text, Me.myFormText.NumericUpDownFontSize.Value, fStyle)

        w = StringDraw6(mySt, iFont, sf).Width
        h = StringDraw6(mySt, iFont, sf).Height

        If myFormText.RadioButtonStringH.Checked Then
            myFormText.LabelDrawStringSize.Text = "高さ" & h
        ElseIf myFormText.RadioButtonStringV.Checked Then
            myFormText.LabelDrawStringSize.Text = "幅" & w
        End If



        bmp = myFormText.DrawString(mySt) '(bmp, mySt, name)
        myFormText.PictureBoxTextSample.Image = bmp

        sf.Dispose()
        iFont.Dispose()

    End Sub
    '文字列の描画、アンチエイリアス、StringAntiAliasDrawのコピペでComboBox用
    '→StringDrawに一本化したので不要
    Friend Sub StringAntiAliasDraw2()

        Dim bmp As New Bitmap(10, 10) '適当なbitmap
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim mySt As String = myForm3.ComboBoxString.Text
        Dim name As String = myForm3.ComboBoxString.Text
        Dim myFSize As Integer = myForm3.NumericUpDownFontSize.Value
        Dim myBrush As New SolidBrush(myForm3.ButtonFontColor.ForeColor)
        Dim sf As New StringFormat

        '文字が入力されていなかったら何もしない
        If mySt = "" Then
            Exit Sub
        End If


        If myForm3.RadioButtonStringV.Checked Then '縦書のとき
            sf.FormatFlags = StringFormatFlags.DirectionVertical
        End If


        'フォント名が非共有メンバを参照するには、オブジェクト参照が必要ですになるけど動く
        Dim fName As New FontFamily(myForm3.ComboBoxAllFonts.SelectedItem.ToString)
        Dim myFont As New Font(fName, myFSize) 'フォント名とサイズを指定してフォントオブジェクト作成
        g.DrawString(mySt, myFont, myBrush, 0, 0, sf) '縦横幅を図るために一度描画

        '縦横幅測定、3000ドットはとりあえずでこれを超える文字幅だと切り捨てられる
        Dim strinSize As SizeF = g.MeasureString(mySt, myFont, 3000, sf)
        bmp = New Bitmap(CType(strinSize.Width, Integer), CType(strinSize.Height, Integer)) '測定した値でBitmap作成
        g = Graphics.FromImage(bmp)

        '本番の文字列描画
        g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias 'アンチエイリアス
        'g.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAliasGridFit 'アンチエイリアス
        'g.TextRenderingHint = Drawing.Text.TextRenderingHint.ClearTypeGridFit 'アンチエイリアス、これはうまくいかない
        'g.TextContrast = 4'アンチエイリアスのガンマ値、規定値は4、0-12の間、あんまり変わらない

        If myForm3.RadioButtonStringV.Checked Then
            g.DrawString(mySt, myFont, myBrush, -(myFSize / 10), 0, sf)
        ElseIf myForm3.RadioButtonStringH.Checked Then
            g.DrawString(mySt, myFont, myBrush, 0, (myFSize / 10), sf)

        End If
        Call PicBoxAdd(name, bmp)
    End Sub

    '文字列の描画、クリアタイプアンチエイリアス、画像にするには適さないから不使用
    Friend Sub StringAntiAliasDrawClearType()

        Dim bmp As New Bitmap(10, 10) '適当なbitmap
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim mySt As String = myForm3.ComboBoxString.Text
        Dim name As String = myForm3.ComboBoxString.Text
        Dim myFSize As Integer = myForm3.NumericUpDownFontSize.Value
        Dim myBrush As New SolidBrush(myForm3.ButtonFontColor.ForeColor)
        Dim sf As New StringFormat

        '文字が入力されていなかったら何もしない
        If mySt = "" Then
            Exit Sub
        End If


        If myForm3.RadioButtonStringV.Checked Then '縦書のとき
            sf.FormatFlags = StringFormatFlags.DirectionVertical
        End If


        'フォント名が非共有メンバを参照するには、オブジェクト参照が必要ですになるけど動く
        Dim fName As New FontFamily(myForm3.ComboBoxAllFonts.SelectedItem.ToString)
        Dim myFont As New Font(fName, myFSize) 'フォント名とサイズを指定してフォントオブジェクト作成
        g.DrawString(mySt, myFont, myBrush, 0, 0, sf) '縦横幅を図るために一度描画

        '縦横幅測定、3000ドットはとりあえずでこれを超える文字幅だと切り捨てられる
        Dim strinSize As SizeF = g.MeasureString(mySt, myFont, 3000, sf)
        bmp = New Bitmap(CType(strinSize.Width, Integer), CType(strinSize.Height, Integer)) '測定した値でBitmap作成
        g = Graphics.FromImage(bmp)

        '本番の文字列描画
        g.TextRenderingHint = Drawing.Text.TextRenderingHint.ClearTypeGridFit 'アンチエイリアス、これはうまくいかない
        'g.TextContrast = 1 'アンチエイリアスのガンマ値、規定値は4、0-12の間、あんまり変わらない

        If myForm3.RadioButtonStringV.Checked Then
            g.DrawString(mySt, myFont, myBrush, -(myFSize / 10), 0, sf)
        ElseIf myForm3.RadioButtonStringH.Checked Then
            g.DrawString(mySt, myFont, myBrush, 0, (myFSize / 10), sf)

        End If
        Call PicBoxAdd(name, bmp)
    End Sub

    'ピクチャーボックスを追加するとき、文字列、枠
    Friend Sub PicBoxAdd(ByVal name As String, ByVal bmp As Bitmap,
                         Optional isEdit As Boolean = False,
                         Optional isClone As Boolean = False, Optional isMouseDraw As Boolean = False,
                         Optional drawString As String = "")
        'nameはピクチャーボックスの名前
        'bmpは描画する絵、Image、Draw
        '半透明や透明を持つ画像になるもの専用
        'OptionalのisEditは図形2なのかどうか図形2ならTrue
        Call CloseEdit編集終了()
        If isMouseDraw = False Then
            Call MouseEndDrawマウスで描画終了処理()

        End If

        If Me.CheckBoxDropPoint.Checked Then
            MsgBox("「ドロップした位置」のチェックを外してから実行してください")
            bmp.Dispose()
            Exit Sub
        End If

        Dim newName As String

        If name.EndsWith("_T") Then
            newName = name
        Else
            newName = name & "_T"
        End If

        'ピクチャーボックス作成
        Dim myPicBox As New ExPictureBox
        Dim newLocate As Point 'ピクチャーボックスの位置
        If isMouseDraw Then 'マウスクリックで線を描画なら０，０
            newLocate = New Point(0, 0)
        Else
            newLocate = AddPicPoint()
        End If
        With myPicBox
            .SizeMode = PictureBoxSizeMode.AutoSize
            .Location = newLocate
            .Image = bmp
            .Name = newName
            '.BackColor = Color.AliceBlue
            .BackColor = Color.Transparent
            .DrawString描画文字 = drawString '文字画像用、改行を含む文字列を記録
            If drawString <> "" Then
                .IsDrawString = True '文字画像ならTrue
            Else
                .IsDrawString = False
            End If
            'If name = "範囲選択" Then '右クリックメニュー追加
            '    .ContextMenuStrip = Me.ContextMenuStrip1
            'End If
            .ContextMenuStrip = Me.ContextMenuStrip1 '2014/12/13

            '図形2で作成する画像なら頂点情報とかいろいろ設定する
            If isEdit Then
                .IsEdit = True

            End If

        End With


        'タグの管理
        tagCount = myPicAr.Count
        If Me.RadioButtonLower.Checked Then
            tagCount = tagCount + 1
        ElseIf Me.RadioButtonUpper.Checked Then
            For Each c As ExPictureBox In myPicAr
                c.Tag = c.Tag + 1
            Next
            'クローン用
            For Each c As ExPictureBox In myPicArClone
                c.Tag = c.Tag + 1
            Next
            'バックアップ用
            For Each c As ExPictureBox In myPicArBackup
                c.Tag = c.Tag + 1
            Next
            tagCount = 1
        End If
        myPicBox.Tag = tagCount

        '右クリックメニュー追加、2014/12/13
        myPicBox.ContextMenuStrip = Me.ContextMenuStrip1


        '2015/01/06、myPicArだけに頂点情報とか書き込んでみた、クローンやバックアップにも入れたほうがいい？
        If isEdit And isClone = False Then
            'With myPicBox

            '    .PathPoints = ActExPic.PathPoints

            'End With
            '        myPicBox = ExPicBoxSetting図形の設定書き込み(myPicBox)
            Call ExPicBoxSetting図形の設定書き込み(myPicBox)

        End If
        'コピペの時
        If isClone Then
            Call CloneExPic図形の設定の複製(ActExPic, myPicBox)
            '
        End If

        'マウスで描画の時
        If isMouseDraw Then
            Call ExPicBoxSetting図形の設定書き込み(myPicBox, True)

        End If

        '完成したピクチャーボックスをコレクションに追加
        If Me.RadioButtonLower.Checked Then
            myPicAr.Add(myPicBox)
        ElseIf Me.RadioButtonUpper.Checked Then
            myPicAr.Insert(0, myPicBox)
        End If

        'クローンのピクチャーボックスをクローン用のコレクションに追加
        Dim myPicBox2 As New ExPictureBox
        With myPicBox2
            .Location = myPicBox.Location
            .SizeMode = myPicBox.SizeMode
            '.Name = "クローン_fromClipImage_" & tagCount
            .Name = newName
            .Image = myPicBox.Image
            .Tag = myPicBox.Tag
            .BackColor = Color.Transparent
        End With
        'クローン用のコレクションに追加
        If Me.RadioButtonLower.Checked Then
            myPicArClone.Add(myPicBox2)
        ElseIf Me.RadioButtonUpper.Checked Then
            myPicArClone.Insert(0, myPicBox2)
        End If

        'バックアップ用のピクチャーボックス作成
        Dim myPicBackup As New ExPictureBox
        With myPicBackup
            .Location = myPicBox.Location
            .SizeMode = PictureBoxSizeMode.Normal
            .Name = newName
            '.Name = "バックアップ_fromClipImage_" & tagCount
            .Image = myPicBox.Image
            .Tag = myPicBox.Tag
            .BackColor = Color.Transparent
        End With
        'バックアップ用のコレクションに追加
        If Me.RadioButtonLower.Checked Then
            myPicArBackup.Add(myPicBackup)
        ElseIf Me.RadioButtonUpper.Checked Then
            myPicArBackup.Insert(0, myPicBackup)
        End If


        Me.Panel2.Controls.Add(myPicBox) 'ピクチャーボックスを表示
        'イベントに関連付ける
        AddHandler myPicBox.MouseEnter, AddressOf PictureBox1_MouseEnter
        AddHandler myPicBox.MouseUp, AddressOf PictureBox1_MouseUp
        AddHandler myPicBox.MouseLeave, AddressOf PictureBox1_MouseLeave
        AddHandler myPicBox.MouseMove, AddressOf PictureBox1_MouseMove '大幅変更箇所
        AddHandler myPicBox.MouseDown, AddressOf PictureBox1_MouseDown '大幅変更箇所
        AddHandler myPicBox.MouseDoubleClick, AddressOf PictureBox1_MouseDoubleClickダブルクリック '2015/01/21


        Call SortPic()

        Me.TextBox1.Text = myPicAr.Count
        FullSize = RigthDownPoint()
        Me.TextBox9.Text = FullSize.ToString
        Me.NumNowPic.Maximum = myPicAr.Count '最大値設定
        Me.NumNowPic.Value = NumNowPic.Maximum

        '追加した画像をnowcontrolとfocuspicにする
        For Each c As ExPictureBox In myPicAr
            If c.Tag = tagCount Then
                'NowControl = c
                ActExPic = c
            End If
        Next
        'NowControl.Focus()
        'ActExPic.Focus()
        'Me.ListBox1.Focus()
        'Me.CurrentPic.Image = FocusPic.Image
        Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
        Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
        Call ChangeFocusTステータス表示更新()
        Call MoveAfter()
        'Call 

        myPicBox.Focus()
        Me.ListBox1.Select()

        'Me.FocusPic.Image = bmp
        'myPicArClone(FocusPic.Tag - 1).image = bmp 'クローンに記録

        Call Transparent4()
        Me.Cursor = Cursors.Default
        'End If
    End Sub

    '消去した画像を復活(ButtonCopyPaste_Clickの改変)復活画像は指定しているレイヤーになるバージョン
    Private Sub ZaoraruPicBoxLowOrUpp() 'ByVal name As String, ByVal bmp As Bitmap)
        If FlagZaoraru Then
            'If Me.CheckBoxDropPoint.Checked Then
            '    MsgBox("「ドロップした位置」のチェックを外してから実行してください")
            '    Exit Sub
            'End If

            tagCount = myPicAr.Count 'タグの初期値にピクチャーボックスの総数を入れる

            Dim myPicBox As New ExPictureBox
            myPicBox.SizeMode = PictureBoxSizeMode.AutoSize '必須
            myPicBox.Location = ZaoraruPic.Location '画像の初期位置
            myPicBox.Image = ZaoraruPic.Image '画像をピクチャーボックスに割り当て
            myPicBox.Name = ZaoraruPic.Name

            'タグの管理
            If Me.RadioButtonLower.Checked Then
                tagCount = tagCount + 1
            ElseIf Me.RadioButtonUpper.Checked Then
                For Each c As ExPictureBox In myPicAr
                    c.Tag = c.Tag + 1
                Next

                For Each c As ExPictureBox In myPicArClone 'クローン用
                    c.Tag = c.Tag + 1
                Next

                For Each c As ExPictureBox In myPicArBackup 'バックアップ用
                    c.Tag = c.Tag + 1
                Next
                tagCount = 1
            End If


            myPicBox.Tag = tagCount
            'ClipCount = ClipCount + 1
            If Me.RadioButtonLower.Checked Then
                myPicAr.Add(myPicBox)
            ElseIf Me.RadioButtonUpper.Checked Then
                myPicAr.Insert(0, myPicBox)
            End If

            '右クリックメニュー追加、2014/12/13
            myPicBox.ContextMenuStrip = Me.ContextMenuStrip1


            'クローンのピクチャーボックスをクローン用のコレクションに追加
            Dim myPicBox2 As New ExPictureBox
            With myPicBox2
                .Location = myPicBox.Location
                .SizeMode = myPicBox.SizeMode
                .Name = CloneBackup.Name
                .Image = CloneBackup.Image
                .Tag = myPicBox.Tag
            End With
            'クローン用のコレクションに追加
            If Me.RadioButtonLower.Checked Then
                myPicArClone.Add(myPicBox2)
            ElseIf Me.RadioButtonUpper.Checked Then
                myPicArClone.Insert(0, myPicBox2)
            End If

            'バックアップ用のピクチャーボックス作成
            Dim myPicBackup As New ExPictureBox
            With myPicBackup
                .Location = myPicBox.Location
                .SizeMode = PictureBoxSizeMode.Normal
                '.Name = "fromClipImage_" & DirectCast(myPicArClone(Me.FocusPic.Tag - 1), ExPictureBox).Name
                .Name = BackupPicBox.Name
                .Image = BackupPicBox.Image
                .Tag = myPicBox.Tag
            End With
            'バックアップ用のコレクションに追加
            If Me.RadioButtonLower.Checked Then
                myPicArBackup.Add(myPicBackup)
            ElseIf Me.RadioButtonUpper.Checked Then
                myPicArBackup.Insert(0, myPicBackup)
            End If


            Me.Panel2.Controls.Add(myPicBox) 'ピクチャーボックスを表示
            'イベントに関連付ける
            AddHandler myPicBox.MouseEnter, AddressOf PictureBox1_MouseEnter
            AddHandler myPicBox.MouseUp, AddressOf PictureBox1_MouseUp
            AddHandler myPicBox.MouseLeave, AddressOf PictureBox1_MouseLeave
            AddHandler myPicBox.MouseMove, AddressOf PictureBox1_MouseMove '大幅変更箇所
            AddHandler myPicBox.MouseDown, AddressOf PictureBox1_MouseDown '大幅変更箇所
            Call SortPic()

            Me.TextBox1.Text = myPicAr.Count
            FullSize = RigthDownPoint()
            Me.TextBox9.Text = FullSize.ToString
            Me.NumNowPic.Maximum = Me.myPicAr.Count '最大値設定
            Me.NumNowPic.Value = NumNowPic.Maximum

            ActExPic = myPicBox '追加した画像をnowcontrolとfocuspicにする

            'ActExPic.Focus()
            'Me.ListBox1.Focus()
            'Me.CurrentPic.Image = FocusPic.Image
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
            Call ChangeFocusTステータス表示更新()
            Call MoveAfter()
            Call Transparent2()
            FlagZaoraru = False

        End If
    End Sub

    '消去した画像を復活(ButtonCopyPaste_Clickの改変)復活画像は一番上になるバージョン
    Private Sub ZaoraruPicBox() 'ByVal name As String, ByVal bmp As Bitmap)
        If FlagZaoraru Then

            tagCount = myPicAr.Count 'タグの初期値にピクチャーボックスの総数を入れる

            Dim myPicBox As New ExPictureBox
            myPicBox.SizeMode = PictureBoxSizeMode.AutoSize '必須
            myPicBox.Location = ZaoraruPic.Location '画像の初期位置
            myPicBox.Image = ZaoraruPic.Image '画像をピクチャーボックスに割り当て
            myPicBox.Name = ZaoraruPic.Name

            'タグの管理

            For Each c As ExPictureBox In myPicAr
                c.Tag = c.Tag + 1
            Next

            For Each c As ExPictureBox In myPicArClone 'クローン用
                c.Tag = c.Tag + 1
            Next

            For Each c As ExPictureBox In myPicArBackup 'バックアップ用
                c.Tag = c.Tag + 1
            Next
            tagCount = 1
            myPicBox.Tag = tagCount

            '右クリックメニュー追加、2014/12/13
            myPicBox.ContextMenuStrip = Me.ContextMenuStrip1


            myPicAr.Insert(0, myPicBox)


            'クローンのピクチャーボックスをクローン用のコレクションに追加
            Dim myPicBox2 As New ExPictureBox
            With myPicBox2
                .Location = myPicBox.Location
                .SizeMode = myPicBox.SizeMode
                .Name = CloneBackup.Name
                .Image = CloneBackup.Image
                .Tag = myPicBox.Tag
            End With

            myPicArClone.Insert(0, myPicBox2)
            'バックアップ用のピクチャーボックス作成
            Dim myPicBackup As New ExPictureBox
            With myPicBackup
                .Location = myPicBox.Location
                .SizeMode = PictureBoxSizeMode.Normal
                '.Name = "fromClipImage_" & DirectCast(myPicArClone(Me.FocusPic.Tag - 1), ExPictureBox).Name
                .Name = BackupPicBox.Name
                .Image = BackupPicBox.Image
                .Tag = myPicBox.Tag
            End With

            myPicArBackup.Insert(0, myPicBackup)

            Me.Panel2.Controls.Add(myPicBox) 'ピクチャーボックスを表示
            'イベントに関連付ける
            AddHandler myPicBox.MouseEnter, AddressOf PictureBox1_MouseEnter
            AddHandler myPicBox.MouseUp, AddressOf PictureBox1_MouseUp
            AddHandler myPicBox.MouseLeave, AddressOf PictureBox1_MouseLeave
            AddHandler myPicBox.MouseMove, AddressOf PictureBox1_MouseMove '大幅変更箇所
            AddHandler myPicBox.MouseDown, AddressOf PictureBox1_MouseDown '大幅変更箇所
            Call SortPic()

            Me.TextBox1.Text = myPicAr.Count
            FullSize = RigthDownPoint()
            Me.TextBox9.Text = FullSize.ToString
            Me.NumNowPic.Maximum = Me.myPicAr.Count '最大値設定
            Me.NumNowPic.Value = NumNowPic.Maximum

            ActExPic = myPicBox '追加した画像をnowcontrolとfocuspicにする

            'ActExPic.Focus()
            'Me.ListBox1.Focus()
            'Me.CurrentPic.Image = FocusPic.Image
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
            Call ChangeFocusTステータス表示更新()
            Call MoveAfter()
            Call Transparent2()

            FlagZaoraru = False

        End If
    End Sub
    '縁取り文字
    Friend Sub StringFringe()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)

            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim range As Integer = bmp.Width * (myForm3.NumericUpDown4sides.Value / 100)
            Dim fringeColor As Color = myForm3.ButtonFringeColor.ForeColor 'Color.White
            Dim bgColor As Color = Color.FromArgb(0, 0, 0, 0)

            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1

                    If bmp.GetPixel(x, y) <> bgColor And x - 1 >= 0 Then
                        bmp.SetPixel(x - 1, y, fringeColor)
                    End If

                Next
            Next

            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'グレースケール状態ををクローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '縁取り文字2
    Friend Sub StringFringe2()
        'Dim Sw As New System.Diagnostics.Stopwatch
        'Sw.Start()



        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)

            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim fringeColor As Color = myForm3.ButtonFringeColor.ForeColor 'Color.White
            Dim fAColor As Integer = myForm3.ButtonFringeColor.ForeColor.A
            Dim fRColor As Integer = myForm3.ButtonFringeColor.ForeColor.R
            Dim fGColor As Integer = myForm3.ButtonFringeColor.ForeColor.G
            Dim fBColor As Integer = myForm3.ButtonFringeColor.ForeColor.B
            Dim bgColor As Color = Color.FromArgb(0, 0, 0, 0)


            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim bmpcol As Color = bmp.GetPixel(x, y)
                    'Getpixelだと色の名前まで一致しないと同じ色とみなされないので
                    'わざわざARGBで比較している
                    'If bmp.GetPixel(x, y) <> fringeColor AndAlso bmp.GetPixel(x, y) <> bgColor AndAlso x - 1 >= 0 Then
                    If bmpcol.A <> fAColor OrElse bmpcol.R <> fRColor OrElse bmpcol.G <> fGColor OrElse bmpcol.B <> fBColor Then
                        If bmp.GetPixel(x, y) <> bgColor AndAlso x - 1 >= 0 Then

                            For x2 As Integer = -1 To 1
                                For y2 As Integer = -1 To 1

                                    If x + x2 >= 0 AndAlso y + y2 >= 0 AndAlso x + x2 <= wBmp AndAlso y + y2 <= hBmp Then
                                        If bmp.GetPixel(x + x2, y + y2) = bgColor Then
                                            bmp.SetPixel(x + x2, y + y2, fringeColor)
                                        End If
                                    End If

                                Next
                            Next

                        End If
                    End If
                Next
            Next

            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'グレースケール状態ををクローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If

        'Sw.Stop()
        'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)
    End Sub


    '縁取り文字3
    Friend Sub StringFringe3()
        Dim Sw As New System.Diagnostics.Stopwatch
        Sw.Start()



        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)

            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim fringeColor As Color = myForm3.ButtonFringeColor.ForeColor 'Color.White
            Dim fAColor As Integer = myForm3.ButtonFringeColor.ForeColor.A
            Dim fRColor As Integer = myForm3.ButtonFringeColor.ForeColor.R
            Dim fGColor As Integer = myForm3.ButtonFringeColor.ForeColor.G
            Dim fBColor As Integer = myForm3.ButtonFringeColor.ForeColor.B
            Dim bgColor As Color = Color.FromArgb(0, 0, 0, 0)


            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim bmpcol As Color = bmp.GetPixel(x, y)
                    'Getpixelだと色の名前まで一致しないと同じ色とみなされないので
                    'わざわざARGBで比較している
                    'If bmp.GetPixel(x, y) <> fringeColor AndAlso bmp.GetPixel(x, y) <> bgColor AndAlso x - 1 >= 0 Then
                    If bmpcol.A <> fAColor OrElse bmpcol.R <> fRColor OrElse bmpcol.G <> fGColor OrElse bmpcol.B <> fBColor Then
                        If bmp.GetPixel(x, y) <> bgColor AndAlso x - 1 >= 0 Then

                            For x2 As Integer = -2 To 2
                                For y2 As Integer = -2 To 2

                                    If x + x2 >= 0 AndAlso y + y2 >= 0 AndAlso x + x2 <= wBmp AndAlso y + y2 <= hBmp Then
                                        If bmp.GetPixel(x + x2, y + y2) = bgColor Then
                                            bmp.SetPixel(x + x2, y + y2, fringeColor)
                                        End If
                                    End If

                                Next
                            Next

                        End If
                    End If
                Next
            Next

            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'グレースケール状態ををクローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If

        Sw.Stop()
        Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)
    End Sub

    '縁取り文字、アンチエイリアス用に作ったけどいまいち
    Friend Sub StringFringeAntiAlias()

        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)

            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim fringeColor As Color = myForm3.ButtonFringeColor.ForeColor 'Color.White
            Dim fAColor As Integer = myForm3.ButtonFringeColor.ForeColor.A
            Dim fRColor As Integer = myForm3.ButtonFringeColor.ForeColor.R
            Dim fGColor As Integer = myForm3.ButtonFringeColor.ForeColor.G
            Dim fBColor As Integer = myForm3.ButtonFringeColor.ForeColor.B
            Dim bgColor As Color = Color.FromArgb(0, 0, 0, 0)


            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim bmpCol As Color = bmp.GetPixel(x, y)
                    'Getpixelだと色の名前まで一致しないと同じ色とみなされないので
                    'わざわざARGBで比較している
                    'If bmp.GetPixel(x, y) <> fringeColor AndAlso bmp.GetPixel(x, y) <> bgColor AndAlso x - 1 >= 0 Then
                    'getpixelの色と縁取りの色が数値以上違うなら中へ、数値は大きくすると似たような色は弾かれる
                    If Math.Abs(bmpCol.R - fRColor) > 10 _
                        OrElse Math.Abs(bmpCol.G - fGColor) > 10 _
                        OrElse Math.Abs(bmpCol.B - fBColor) > 10 Then

                        If bmp.GetPixel(x, y) <> bgColor AndAlso x - 1 >= 0 Then 'GetPixelの色が完全透明ではないなら

                            For x2 As Integer = -2 To 2
                                For y2 As Integer = -2 To 2

                                    If x + x2 >= 0 AndAlso y + y2 >= 0 AndAlso x + x2 <= wBmp AndAlso y + y2 <= hBmp Then
                                        If bmp.GetPixel(x + x2, y + y2) = bgColor Then
                                            'Dim fA As Integer = Math.Abs(bmp.GetPixel(x + x2, y + y2).A) - bmpCol.A
                                            ''If Math.Abs(bmp.GetPixel(x + x2, y + y2).A - bmpCol.A) > 10 Then
                                            'If fA < 2 Then
                                            bmp.SetPixel(x + x2, y + y2, Color.FromArgb(bmpCol.A, fringeColor))
                                        End If
                                    End If

                                Next
                            Next

                        End If
                    End If
                Next
            Next

            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'グレースケール状態ををクローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If

    End Sub

    '縁取り文字、アンチエイリアス用に作ったけどいまいち
    Friend Sub StringFringeAntiAlias2()

        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor
            'Backupからではなく前回の続きからにするためクローンから呼び出して描画する
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)

            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim wBmp As Integer = bmp.Width - 1
            Dim hBmp As Integer = bmp.Height - 1
            Dim fringeColor As Color = myForm3.ButtonFringeColor.ForeColor 'Color.White
            Dim fAColor As Integer = myForm3.ButtonFringeColor.ForeColor.A
            Dim fRColor As Integer = myForm3.ButtonFringeColor.ForeColor.R
            Dim fGColor As Integer = myForm3.ButtonFringeColor.ForeColor.G
            Dim fBColor As Integer = myForm3.ButtonFringeColor.ForeColor.B
            Dim bgColor As Color = Color.FromArgb(0, 0, 0, 0)


            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim bmpCol As Color = bmp.GetPixel(x, y)

                    If bmpCol.A > 0 AndAlso bmpCol.A < 255 Then
                        'bmp.SetPixel(x, y, fringeColor)
                        bmp.SetPixel(x, y, Color.FromArgb(bmpCol.A, fringeColor))
                    End If

                Next
            Next

            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
            Me.ActExPic.Image = bmp
            myPicArClone(ActExPic.Tag - 1).image = bmp 'グレースケール状態ををクローンに記録
            Call Transparent2()
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
            Me.Cursor = Cursors.Default
        End If

    End Sub

    'GraphicsPathでの縁取りテスト
    Friend Sub StringFringeGPath()
        Dim bmp As New Bitmap(10, 10) '適当なbitmap
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim gp As New System.Drawing.Drawing2D.GraphicsPath()

        Dim mySt As String = myForm3.ComboBoxString.Text
        Dim name As String = "文字_" & myForm3.ComboBoxString.Text
        Dim myFSize As Integer = myForm3.NumericUpDownFontSize.Value
        Dim myBrush As New SolidBrush(myForm3.ButtonFontColor.ForeColor)
        Dim sf As New StringFormat
        Dim pColor As Color = myForm3.ButtonStringFringeColor.ForeColor
        Dim myPen As New Pen(pColor, myForm3.NumericUpDownStringFringeBold.Value)

        'bmp = StringDraw5(bmp, mySt, name)

        'Call PicBoxAdd(name, bmp)
        '文字が入力されていなかったら何もしない
        If mySt = "" Then
            Exit Sub
        End If


        If myForm3.RadioButtonStringV.Checked Then '縦書のとき
            sf.FormatFlags = StringFormatFlags.DirectionVertical
        End If

        Try
            Dim fName As New FontFamily(myForm3.ComboBoxAllFonts.SelectedItem.ToString)
            Dim myFont As New Font(fName, myFSize)
            Dim fBold As Integer
            Dim fItalyc As Integer
            'フォント名とサイズを指定してフォントオブジェクト作成
            If myForm3.CheckBoxTextItalic.Checked AndAlso myForm3.CheckBoxTextBold.Checked Then
                myFont = New Font(fName, myFSize, FontStyle.Italic Or FontStyle.Bold) '太字＆斜体
                fBold = 1
                fItalyc = 2
            ElseIf myForm3.CheckBoxTextBold.Checked Then
                myFont = New Font(fName, myFSize, FontStyle.Bold) '太字
                fBold = 1
                fItalyc = 0
            ElseIf myForm3.CheckBoxTextItalic.Checked Then
                myFont = New Font(fName, myFSize, FontStyle.Italic) '斜体
                fItalyc = 2
                fBold = 0
            End If
            'Dim myFontF As New FontFamily(myFont)

            'gp.AddString(mySt, fName, 0, 50, New Point(0, 0), sf)

            g.DrawString(mySt, myFont, myBrush, 0, 0, sf) '縦横幅を図るために一度描画

            '縦横幅測定、3000ドットはとりあえずでこれを超える文字幅だと切り捨てられる
            Dim strinSize As SizeF = g.MeasureString(mySt, myFont, 3000, sf)
            Dim stringHeigt As Integer = strinSize.Height '描画した時の四角の高さを取得
            Dim fontHeight As Single = myFont.GetHeight(g) '描画した時のフォントの高さを取得
            'Dim val3 As Single = myFont.GetHeight()

            bmp = New Bitmap(CType(strinSize.Width, Integer), CType(strinSize.Height, Integer)) '測定した値でBitmap作成
            gp.AddString(mySt, fName, 0, fontHeight, New Point(0, 0), sf) '取得したフォントの高さでAddString
            'Dim val As RectangleF = gp.GetBounds 'gpの大きさを取得

            'bmp = New Bitmap(CType(val.Width, Integer), CType(val.Height, Integer)) '測定した値でBitmap再作成
            g = Graphics.FromImage(bmp)
            'gp.ClearMarkers()
            'gp.Reverse()
            gp.Reset() 'リセットしないと塗りつぶしが無視される、前回の描画が残っているから？


            '背景色を塗りつぶし
            If myForm3.CheckBoxTextBackColor背景色の有無.Checked Then

                'グラデーション無し
                If myForm3.CheckBoxTextBackC_Gradation.Checked = False Then
                    Dim backColor As Color = myForm3.ButtonTextBackColor1.ForeColor
                    Dim backBrush As New SolidBrush(backColor)
                    g.FillRectangle(backBrush, 0, 0, bmp.Width, bmp.Height)

                    'グラデーション有り
                ElseIf myForm3.CheckBoxTextBackC_Gradation.Checked Then
                    'Dim backColor1 As Color = myForm3.ButtonTextBackColor1.ForeColor
                    'Dim backColor2 As Color = myForm3.ButtonTextBackColor2.ForeColor
                    Dim backColor1 As Color = myForm3.PictureBoxStringBGColor1.BackColor
                    Dim backColor2 As Color = myForm3.PictureBoxStringBGColor2.BackColor
                    Dim angle As Integer = myForm3.NumericUpDownStringBackGradAngle文字背景グラデ角度.Value


                    Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
                    Dim gradationBrush As New LinearGradientBrush(rect, backColor1, backColor2, angle)

                    'If myForm3.RadioButtonTextBackC_Horizontal.Checked Then
                    '    gradationBrush = New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.Horizontal)
                    '    g.FillRectangle(gradationBrush, rect)
                    'ElseIf myForm3.RadioButtonTextBackC_Vertical.Checked Then
                    '    gradationBrush = New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.Vertical)
                    '    g.FillRectangle(gradationBrush, rect)
                    'ElseIf myForm3.RadioButtonTextBackC_LeftUp.Checked Then
                    '    gradationBrush = New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.ForwardDiagonal)
                    '    g.FillRectangle(gradationBrush, rect)
                    'ElseIf myForm3.RadioButtonTextBackC_RightUp.Checked Then
                    '    gradationBrush = New LinearGradientBrush(rect, backColor1, backColor2, LinearGradientMode.BackwardDiagonal)
                    '    g.FillRectangle(gradationBrush, rect)
                    'End If

                End If

                '背景の透明度
                Dim bmpdata As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadWrite, bmp.PixelFormat)
                Dim ptr As IntPtr = bmpdata.Scan0
                Dim data As Integer = bmpdata.Stride * bmp.Height - 1
                Dim pixels(data) As Byte
                System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

                Dim transprent As Integer = myForm3.NumericUpDownTextTransparent1.Value
                Dim x As Integer = 0
                Dim y As Integer = 0
                For x = 0 To bmp.Width - 1
                    For y = 0 To bmp.Height - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4 + 3
                        pixels(pos) = CByte(transprent)
                    Next
                Next
                System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
                bmp.UnlockBits(bmpdata)
            End If




            If myForm3.CheckBoxStringAntiAlias.Checked Then 'アンチエイリアス
                g.SmoothingMode = SmoothingMode.AntiAlias
            End If

            '2色グラデーション方向設定
            If myForm3.CheckBoxTextGradation.Checked Then
                'Dim color1 As Color = myForm3.ButtonFontColor.ForeColor
                'Dim color2 As Color = myForm3.ButtonFontColor2.ForeColor
                Dim color1 As Color = myForm3.PictureBoxTextColor1.BackColor
                Dim color2 As Color = myForm3.PictureBoxTextColor2.BackColor

                Dim angle As Integer = myForm3.NumericUpDownStringGradientAngle.Value
                Dim gradationBrush As New LinearGradientBrush( _
                    New Rectangle(0, 0, bmp.Width, bmp.Height), color1, color2, angle)


                '本番の文字列描画
                'Dim gradationBrush As New LinearGradientBrush(New Point(0, 0), New Point(bmp.Width, 0), color1, color2)
                If myForm3.RadioButtonStringV.Checked Then '縦書
                    'g.DrawString(mySt, myFont, gradationBrush, -(myFSize / 10), 0, sf)
                    gp.AddString(mySt, fName, fBold Or fItalyc, fontHeight, New Point(0, myFSize / 10), sf)
                    g.FillPath(gradationBrush, gp) '文字の中を塗りつぶし
                    g.DrawPath(myPen, gp) '縁取り
                ElseIf myForm3.RadioButtonStringH.Checked Then
                    'g.DrawString(mySt, myFont, gradationBrush, 0, (myFSize / 10), sf)
                    gp.AddString(mySt, fName, fBold Or fItalyc, fontHeight, New Point(0, myFSize / 10), sf) '文字の追加、本番、縦の位置調整
                    g.FillPath(gradationBrush, gp) '文字の中を塗りつぶし
                    g.DrawPath(myPen, gp) '縁取り
                End If
            Else
                '本番の文字列描画
                If myForm3.RadioButtonStringV.Checked Then '縦書
                    'g.DrawString(mySt, myFont, myBrush, -(myFSize / 10), 0, sf)
                    gp.AddString(mySt, fName, fBold Or fItalyc, fontHeight, New Point(0, myFSize / 10), sf)
                    g.FillPath(myBrush, gp) '文字の中を塗りつぶし
                    g.DrawPath(myPen, gp) '縁取り
                ElseIf myForm3.RadioButtonStringH.Checked Then
                    gp.AddString(mySt, fName, fBold Or fItalyc, fontHeight, New Point(0, myFSize / 10), sf)
                    g.FillPath(myBrush, gp) '文字の中を塗りつぶし
                    g.DrawPath(myPen, gp) '縁取り
                    'g.DrawString(mySt, myFont, myBrush, 0, (myFSize / 10), sf)
                End If
            End If
            'g.FillPath(myBrush, gp) '文字の中を塗りつぶし
            'g.DrawPath(Pens.White, gp) '縁取り

            g.Dispose()


            Call PicBoxAdd(name, bmp)

        Catch ex As Exception

        End Try

    End Sub

    Private Sub Button10_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        '図形2チェック用
        Dim pic2 As ExPictureBox = EditNowPic
        Dim pic3 As ExPictureBox = ActExPic
        Dim ar1 = myPicAr
        Dim ar2 = myPicArBackup
        Dim ar3 = myPicArClone

        Dim pic2name = pic2.Name



        '図形2チェック用ここまで

        '画像全体を半透明

        '一旦透過処理を無しにするためにバックアップから描画する
        ActExPic.Image = myPicArBackup(ActExPic.Tag - 1).image


        Dim bmp As New Bitmap(Me.ActExPic.Width, Me.ActExPic.Height)
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim img As Image = Me.ActExPic.Image
        Dim cm As New System.Drawing.Imaging.ColorMatrix()
        cm.Matrix00 = 1
        cm.Matrix11 = 1
        cm.Matrix22 = 1
        cm.Matrix33 = 0.3F
        'cm.Matrix33 = 0.5F
        cm.Matrix44 = 1
        Dim ia As New System.Drawing.Imaging.ImageAttributes()
        ia.SetColorMatrix(cm)

        g.DrawImage(img, New Rectangle(0, 0, bmp.Width, bmp.Height), 0, 0, bmp.Width, bmp.Height, GraphicsUnit.Pixel, ia)
        Me.ActExPic.Image = bmp
        myPicArClone(ActExPic.Tag - 1).image = bmp '半透明状態をクローンに記録

        'Call Transparent()








        ''色の入れ替えで色を残したまま透明度だけを0にして透明にしようとしたけど
        '透明度を指定すると色の入れ替えはできないみたい
        'Dim bmp As New Bitmap(Me.FocusPic.Width, Me.FocusPic.Height)
        'Dim bmp2 As New Bitmap(Me.FocusPic.Width, Me.FocusPic.Height)

        'Dim img As Image = Me.FocusPic.Image
        'Dim g As Graphics = Graphics.FromImage(bmp)
        'Dim g2 As Graphics = Graphics.FromImage(bmp2)

        'g2.FillRectangle(Brushes.White, Me.FocusPic.ClientRectangle)
        ''g2.FillRectangle(Brushes.Blue, Me.FocusPic.ClientRectangle)
        'Dim ia As New Imaging.ImageAttributes
        'Dim map(0) As Imaging.ColorMap


        ''Dim Tc = Me.TransparentPictureBox.BackColor.ToArgb
        'Dim Tcol = Me.TransparentPictureBox.BackColor

        'str = String.Format("{0}_{1}_{2}_{3}", Tcol.A, Tcol.R, Tcol.G, Tcol.B)
        'MsgBox(str)
        'Dim zeroA As Color = Color.FromArgb(0, Tcol.R, Tcol.G, Tcol.B)
        'Dim zeroA As Color = Color.FromArgb(0, Tcol)
        'Dim zeroA As Color = Color.FromArgb(0, Color.Aqua)

        'map(0) = New Imaging.ColorMap
        ''map(0).OldColor = Color.White

        ''map(0).NewColor = Color.Black
        ''map(0).OldColor = Me.TransparentPictureBox.BackColor
        'map(0).OldColor = Me.TransparentPictureBox.BackColor
        'map(0).NewColor = zeroA
        'ia.SetRemapTable(map)


        'g.DrawImage(img, New Rectangle(0, 0, img.Width, img.Height), 0, 0, img.Width, img.Height, GraphicsUnit.Pixel, ia)
        'Me.FocusPic.Image = img
        'Str = String.Format("{0}_{1}_{2}_{3}", Tcol.A, Tcol.R, Tcol.G, Tcol.B)
        'MsgBox(Str)

        'Dim Ia2 As New Imaging.ImageAttributes
        ''Ia2.SetColorKey(Tcol, Tcol)
        'Ia2.SetColorKey(Color.White, Color.White)
        ''Ia2.SetColorKey(Color.Blue, Color.Blue)
        'g2.DrawImage(img, New Rectangle(0, 0, img.Width, img.Height), 0, 0, img.Width, img.Height, GraphicsUnit.Pixel, Ia2)

        'Me.FocusPic.Image = bmp2

        ''Me.FocusPic.Refresh()

    End Sub

    Private Sub ButtonTransparent_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTransparent.Click
        '透過色が指定されていなければ何もしない
        If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
            Exit Sub
        End If

        Call TransparentOnePic()
        Call Transparent2()

        'ピクチャーボックスの名前の末尾に_Tを付けて透過色が設定されている事を示す
        If Me.ActExPic.Name.EndsWith("_T") = False Then '名前の末尾に_Tがなければ
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim tempClonePic As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            Me.ActExPic.Name = Me.ActExPic.Name & "_T"
            tempClonePic.Name = tempClonePic.Name & "_T"
        End If

        '全体を再描画する透過処理

        'Call Transparent()
    End Sub

    'もう一つのフォームを表示
    Private Sub Button4_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        'Form3がすでに開かれていたら何もしない
        For Each f As Form In Me.OwnedForms
            'For Each f As Form In Application.OpenForms
            If f.Name = "Form3" Then
                Exit Sub
            End If
        Next

        'Dim omake As New Form3
        'omake.Show(Me)
        'Dim myForm3 As New Form3 'Form3にアクセスするの


        Me.Cursor = Cursors.WaitCursor
        'Dim myform3 As Form3
        'myForm3 = Me.Owner
        myForm3 = New Form3 'これを付けないと一度閉じたサブフォームを開こうとするとリソースがないと言われる
        myForm3.ShowInTaskbar = False 'タスクバーに表示しない

        myForm3.Show(Me)
        Call SquareSample()

        'omake.Dispose()
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub Button11_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click

        If myPicAr.Count <> 0 Then

            '透過色が指定されていなければ何もしない
            If Me.TransparentPictureBox.BackColor = Color.FromKnownColor(KnownColor.Control) Then
                Exit Sub
            End If
            '一旦透明にした色を戻す、これを実行しないと前回の画像が残る、残像みたいになる
            'デメリットはこれを使うと次々に色を透明にすることができない
            '↑リアルタイム透過処理じゃなければ意味が無いみたい
            'For Each c As ExPictureBox In myPicArClone
            '    If FocusPic.Tag = c.Tag Then
            '        FocusPic.Image = c.Image
            '        Exit For
            '    End If
            'Next



            '透明にする
            Dim myTempBmp2 As Bitmap
            ''myPicArClone = myPicAr.Clone
            'For Each c As ExPictureBox In myPicAr
            '    If FocusPic.Tag = c.Tag Then
            '        myTempBmp2 = New Bitmap(c.Image)
            '        myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
            '        c.Image = myTempBmp2
            '        Exit For
            '    End If
            'Next
            myTempBmp2 = New Bitmap(Me.ActExPic.Image)
            myTempBmp2.MakeTransparent(Me.TransparentPictureBox.BackColor)
            Me.ActExPic.Image = myTempBmp2

            Dim i As Integer = Me.ActExPic.Tag - 1
            DirectCast(myPicArClone(i), ExPictureBox).Image = myTempBmp2

            'Call Transparent()
            Call Transparent2()
        End If
    End Sub
    'すべての画像を初期状態に戻す
    Private Sub ButtonAllReset_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonAllReset.Click
        'Dim myTempBmp As Bitmap
        Dim i As Integer

        For i = 0 To myPicAr.Count - 1
            'myTempBmp = myPicArBackup(i).image
            'myPicAr(i).image = myTempBmp
            Dim PicAr As ExPictureBox = DirectCast(myPicAr(i), ExPictureBox)
            Dim Backup As ExPictureBox = DirectCast(myPicArBackup(i), ExPictureBox)
            Dim Clone As ExPictureBox = DirectCast(myPicArClone(i), ExPictureBox)
            PicAr.Image = Backup.Image
            Clone.Image = Backup.Image
            PicAr.Name = Backup.Name 'Backupから名前を戻す

            ''名前の末尾に_Tがあればそれを削除
            '→Backupから戻すようにした
            'If PicAr.Name.EndsWith("_T") Then
            '    PicAr.Name = PicAr.Name.Substring(0, PicAr.Name.Length - 2)
            '    Clone.Name = Clone.Name.Substring(0, Clone.Name.Length - 2)
            'End If

        Next

        'For i = 0 To myPicAr.Count - 1
        'myTempBmp = myPicArBackup(i).image
        'myPicArClone(i).image = myTempBmp
        'Next
    End Sub

    ''選択画像だけ初期状態に戻す→PicRestorに統合
    'Friend Sub PicReset()
    '    '画像がなければ何もしない
    '    If myPicAr.Count = 0 Then
    '        Exit Sub
    '    End If
    '    Dim i As Integer = FocusPic.Tag - 1

    '    Dim myTempBmp As New Bitmap(DirectCast(myPicArBackup(i), ExPictureBox).Image)

    '    'Dim myBackPic As ExPictureBox = DirectCast(myPicArBackup(i), ExPictureBox)
    '    Dim myPic As ExPictureBox = DirectCast(myPicAr(i), ExPictureBox)

    '    'myTempBmp = myBackPic.Image
    '    myPic.Image = myTempBmp
    '    DirectCast(myPicAr(i), ExPictureBox).Image = myTempBmp
    '    DirectCast(myPicArClone(i), ExPictureBox).Image = myTempBmp
    'End Sub

    'カラーマトリックスで遊ぶ
    Friend Sub ColorMatrix()
        If myPicAr.Count <> 0 Then
            Dim rScale As Single = 1
            Dim gScale As Single = 1
            Dim bScale As Single = 1
            Dim cm4 As Single = myForm3.NumericUpDownCM40.Value
            Dim cm3 As Single = myForm3.NumericUpDownCM30.Value
            Dim cm2 As Single = myForm3.NumericUpDownCM20.Value
            Dim cm1 As Single = myForm3.NumericUpDownCM10.Value
            Dim cm0 As Single = myForm3.NumericUpDownCM00.Value
            Dim cm10 As Single = myForm3.NumericUpDownCM01.Value
            Dim cm11 As Single = myForm3.NumericUpDownCM11.Value
            Dim cm12 As Single = myForm3.NumericUpDownCM21.Value
            Dim cm13 As Single = myForm3.NumericUpDownCM31.Value
            Dim cm14 As Single = myForm3.NumericUpDownCM41.Value
            Dim cm20 As Single = myForm3.NumericUpDownCM02.Value
            Dim cm21 As Single = myForm3.NumericUpDownCM12.Value
            Dim cm22 As Single = myForm3.NumericUpDownCM22.Value
            Dim cm23 As Single = myForm3.NumericUpDownCM32.Value
            Dim cm24 As Single = myForm3.NumericUpDownCM42.Value
            Dim cm As New Imaging.ColorMatrix(New Single()() {New Single() {cm0, cm10, cm20, 0, 0}, _
                                                              New Single() {cm1, cm11, cm21, 0, 0}, _
                                                              New Single() {cm2, cm12, cm22, 0, 0}, _
                                                              New Single() {cm3, cm13, cm23, 1, 0}, _
                                                              New Single() {cm4, cm14, cm24, 0, 1}})
            Dim ia As New Imaging.ImageAttributes()
            ia.SetColorMatrix(cm)

            'Dim img As New Bitmap(FocusPic.Image, FocusPic.Width, FocusPic.Height)
            'Dim g As Graphics = Graphics.FromImage(img)
            'g.DrawImage(img, New Rectangle(0, 0, img.Width, img.Height), 0, 0, img.Width, img.Height, GraphicsUnit.Pixel, ia)
            'FocusPic.Image = img
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(ActExPic.Width, ActExPic.Height)
            'Dim img As Image = DirectCast(FocusPic, ExPictureBox).Image
            Dim imgbmp = New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            Dim img As Image = imgbmp


            Dim g As Graphics = Graphics.FromImage(bmp)
            g.DrawImage(img, New Rectangle(0, 0, img.Width, img.Height), 0, 0, img.Width, img.Height, GraphicsUnit.Pixel, ia)
            ActExPic.Image = bmp
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録
            Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image

        End If
    End Sub

    '赤以外を白黒にする
    Friend Sub RedGrayScale()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            '特定の色を半透明にする,1pixelづつ判定して塗り替える
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)

            'Dim bmp As New Bitmap(Me.FocusPic.Image)
            Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim rRange As Integer = myForm3.NumericUpDownRed.Value '赤の数値
            Dim gbRange As Integer = myForm3.NumericUpDownGrBl.Value '緑と青の数値

            'Dim tValue As Integer = myForm3.NumericUpDownTCol.Value
            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim r As Integer = bmp.GetPixel(x, y).R
                    Dim green As Integer = bmp.GetPixel(x, y).G
                    Dim b As Integer = bmp.GetPixel(x, y).B
                    If r < rRange Then '赤の数値が設定値より小さければ白黒
                        Dim ave As Integer = (r + green + b) / 3
                        Dim newColor = Color.FromArgb(ave, ave, ave)
                        bmp.SetPixel(x, y, newColor)
                    ElseIf green > gbRange OrElse b > gbRange Then '赤の数値が設定値より大きくても緑か青が大きければ白黒
                        Dim ave As Integer = (r + green + b) / 3
                        Dim newColor = Color.FromArgb(ave, ave, ave)
                        bmp.SetPixel(x, y, newColor)
                    End If
                Next
            Next
            g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height)) '出来上がったbmpをドローイング
            Me.ActExPic.Image = bmp '表示
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録

            Call Transparent() '透過表示
            Me.Cursor = Cursors.Default
        End If

    End Sub

    '赤以外を白黒にするLockBitsバージョン
    Friend Sub RedGrayScaleLockBits()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            '特定の色を半透明にする,1pixelづつ判定して塗り替える
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
            Dim bmpdate As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdate.Scan0
            Dim data As Integer = bmpdate.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            'Dim g As Graphics = Graphics.FromImage(bmp)
            Dim x As Integer
            Dim y As Integer
            Dim rRange As Integer = myForm3.NumericUpDownRed.Value '赤の数値
            Dim gbRange As Integer = myForm3.NumericUpDownGrBl.Value '緑と青の数値

            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim pos As Integer = (y * bmpdate.Stride + x * 4)
                    Dim r As Integer = pixels(pos + 2)
                    Dim green As Integer = pixels(pos + 1)
                    Dim b As Integer = pixels(pos)
                    'Dim r As Integer = bmp.GetPixel(x, y).R
                    'Dim green As Integer = bmp.GetPixel(x, y).G
                    'Dim b As Integer = bmp.GetPixel(x, y).B
                    If r < rRange Then
                        Dim ave As Integer = (r + green + b) / 3
                        pixels(pos + 2) = CByte(ave)
                        pixels(pos + 1) = CByte(ave)
                        pixels(pos) = CByte(ave)
                    ElseIf green > gbRange OrElse b > gbRange Then
                        Dim ave As Integer = (r + green + b) / 3
                        pixels(pos + 2) = ave 'Cbyteで変換しなくてもいいみたい？
                        pixels(pos + 1) = ave
                        pixels(pos) = ave
                    End If

                    'If r < rRange Then '赤の数値が設定値より小さければ白黒
                    '    Dim ave As Integer = (r + green + b) / 3
                    '    Dim newColor = Color.FromArgb(ave, ave, ave)
                    '    bmp.SetPixel(x, y, newColor)
                    'ElseIf green > gbRange OrElse b > gbRange Then '赤の数値が設定値より大きくても緑か青が大きければ白黒
                    '    Dim ave As Integer = (r + green + b) / 3
                    '    Dim newColor = Color.FromArgb(ave, ave, ave)
                    '    bmp.SetPixel(x, y, newColor)
                    'End If
                Next
            Next
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            bmp.UnlockBits(bmpdate)

            'g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height)) '出来上がったbmpをドローイング
            Me.ActExPic.Image = bmp '表示
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録

            Call Transparent() '透過表示
            Me.Cursor = Cursors.Default
        End If

    End Sub
    '赤以外白黒2
    Friend Sub RedGrayScale2()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            '特定の色を半透明にする,1pixelづつ判定して塗り替える
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
            Dim bmpdate As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdate.Scan0
            Dim data As Integer = bmpdate.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            Dim x As Integer
            Dim y As Integer
            Dim gRange As Single = myForm3.NumericUpDownGreen2.Value '赤の数値
            Dim rRange As Single = myForm3.NumericUpDownRed2.Value '緑と青の数値
            Dim bRange As Single = myForm3.NumericUpDownBlue2.Value

            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim pos As Integer = (y * bmpdate.Stride + x * 4)
                    Dim r As Integer = pixels(pos + 2)
                    Dim g As Integer = pixels(pos + 1)
                    Dim b As Integer = pixels(pos)

                    If r * gRange < g Or r * bRange < b Then
                        Dim ave As Integer = (r + g + b) / 3
                        pixels(pos + 2) = ave
                        pixels(pos + 1) = ave
                        pixels(pos) = ave
                    End If
                Next
            Next
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            bmp.UnlockBits(bmpdate)

            'g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height)) '出来上がったbmpをドローイング
            Me.ActExPic.Image = bmp '表示
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録

            Call Transparent4() '透過表示
            Me.Cursor = Cursors.Default
        End If
    End Sub
    '赤以外白黒3
    Friend Sub RGBGrayScale()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor


            'Dim Sw As New System.Diagnostics.Stopwatch
            'Sw.Start()
            '特定の色を半透明にする,1pixelづつ判定して塗り替える
            Dim i As Integer = Me.ActExPic.Tag - 1
            'Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
            Dim bmp As New Bitmap(DirectCast(myPicArBackup(i), ExPictureBox).Image)
            ActExPic.Image = bmp
            Dim bmpdate As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdate.Scan0
            Dim data As Integer = bmpdate.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            Dim x As Integer
            Dim y As Integer
            Dim gRange As Single = myForm3.TrackBarGreen.Value / 100
            Dim rRange As Single = myForm3.TrackBarRed.Value / 100
            Dim bRange As Single = myForm3.TrackBarBlue.Value / 100


            'If myForm3.RadioButtonRed.Checked Then
            '    For x = 0 To bmp.Width - 1
            '        For y = 0 To bmp.Height - 1
            '            Dim pos As Integer = (y * bmpdate.Stride + x * 4)
            '            Dim r As Integer = pixels(pos + 2)
            '            Dim g As Integer = pixels(pos + 1)
            '            Dim b As Integer = pixels(pos)

            '            If r * gRange < g Or r * bRange < b Then
            '                Dim ave As Integer = (r + g + b) / 3
            '                pixels(pos + 2) = ave
            '                pixels(pos + 1) = ave
            '                pixels(pos) = ave
            '            End If
            '        Next
            '    Next
            '    System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            '    bmp.UnlockBits(bmpdate)

            'ElseIf myForm3.RadioButtonGreen.Checked Then
            '    For x = 0 To bmp.Width - 1
            '        For y = 0 To bmp.Height - 1
            '            Dim pos As Integer = (y * bmpdate.Stride + x * 4)
            '            Dim r As Integer = pixels(pos + 2)
            '            Dim g As Integer = pixels(pos + 1)
            '            Dim b As Integer = pixels(pos)

            '            If g * rRange < r Or g * bRange < b Then
            '                Dim ave As Integer = (r + g + b) / 3
            '                pixels(pos + 2) = ave
            '                pixels(pos + 1) = ave
            '                pixels(pos) = ave
            '            End If
            '        Next
            '    Next
            '    System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            '    bmp.UnlockBits(bmpdate)
            'ElseIf myForm3.RadioButtonBlue.Checked Then
            '    For x = 0 To bmp.Width - 1
            '        For y = 0 To bmp.Height - 1
            '            Dim pos As Integer = (y * bmpdate.Stride + x * 4)
            '            Dim r As Integer = pixels(pos + 2)
            '            Dim g As Integer = pixels(pos + 1)
            '            Dim b As Integer = pixels(pos)

            '            If b * gRange < g Or b * rRange < r Then
            '                Dim ave As Integer = (r + g + b) / 3
            '                pixels(pos + 2) = ave
            '                pixels(pos + 1) = ave
            '                pixels(pos) = ave
            '            End If
            '        Next
            '    Next
            '    System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            '    bmp.UnlockBits(bmpdate)
            'End If

            '↑のloopより4倍くらい速い
            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim pos As Integer = (y * bmpdate.Stride + x * 4)
                    Dim r As Integer = pixels(pos + 2)
                    Dim g As Integer = pixels(pos + 1)
                    Dim b As Integer = pixels(pos)

                    If myForm3.RadioButtonRed.Checked Then

                        If r * gRange < g OrElse r * bRange < b Then
                            Dim ave As Integer = (r + g + b) / 3
                            pixels(pos + 2) = ave
                            pixels(pos + 1) = ave
                            pixels(pos) = ave
                        End If
                    End If

                    If myForm3.RadioButtonGreen.Checked Then
                        If g * rRange < r OrElse g * bRange < b Then
                            Dim ave As Integer = (r + g + b) / 3
                            pixels(pos + 2) = ave
                            pixels(pos + 1) = ave
                            pixels(pos) = ave
                        End If
                    End If

                    If myForm3.RadioButtonBlue.Checked Then
                        If b * gRange < g OrElse b * rRange < r Then
                            Dim ave As Integer = (r + g + b) / 3
                            pixels(pos + 2) = ave
                            pixels(pos + 1) = ave
                            pixels(pos) = ave
                        End If
                    End If

                    If myForm3.RadioButtonYellow.Checked Then

                        If (r + g) / 2 * gRange < (g + b) / 2 OrElse (r + g) * rRange < (r + b) Then
                            Dim ave As Integer = (r + g + b) / 3
                            pixels(pos + 2) = ave
                            pixels(pos + 1) = ave
                            pixels(pos) = ave
                        End If
                    End If
                    If myForm3.RadioButtonAqua.Checked Then

                        If (g + b) / 2 * gRange < (g + r) / 2 OrElse (g + b) * bRange < (b + r) Then
                            Dim ave As Integer = (r + g + b) / 3
                            pixels(pos + 2) = ave
                            pixels(pos + 1) = ave
                            pixels(pos) = ave
                        End If
                    End If
                    If myForm3.RadioButtonMagenta.Checked Then

                        If (r + b) / 2 * rRange < (r + g) / 2 OrElse (r + b) * bRange < (b + g) Then
                            Dim ave As Integer = (r + g + b) / 3
                            pixels(pos + 2) = ave
                            pixels(pos + 1) = ave
                            pixels(pos) = ave
                        End If
                    End If
                Next
            Next
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            bmp.UnlockBits(bmpdate)


            Me.ActExPic.Image = bmp '表示
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録
            'Sw.Stop()
            'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)



            Call Transparent4() '透過表示
            Me.Cursor = Cursors.Default
        End If
    End Sub


    '緑以外白黒
    Friend Sub GreenGrayScale()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            '特定の色を半透明にする,1pixelづつ判定して塗り替える
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
            Dim bmpdate As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdate.Scan0
            Dim data As Integer = bmpdate.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            Dim x As Integer
            Dim y As Integer
            Dim gRange As Integer = myForm3.NumericUpDownGreen.Value '赤の数値
            Dim rbRange As Integer = myForm3.NumericUpDownRedBlue.Value '緑と青の数値
            Dim bRange As Integer = myForm3.NumericUpDownBlue2.Value

            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim pos As Integer = (y * bmpdate.Stride + x * 4)
                    Dim r As Integer = pixels(pos + 2)
                    Dim green As Integer = pixels(pos + 1)
                    Dim b As Integer = pixels(pos)
                    'If green < gRange Then
                    '    Dim ave As Integer = (r + green + b) / 3
                    '    pixels(pos + 2) = CByte(ave)
                    '    pixels(pos + 1) = CByte(ave)
                    '    pixels(pos) = CByte(ave)
                    'ElseIf r > rbRange OrElse b > rbRange Then
                    '    Dim ave As Integer = (r + green + b) / 3
                    '    pixels(pos + 2) = ave 'Cbyteで変換しなくてもいいみたい？
                    '    pixels(pos + 1) = ave
                    '    pixels(pos) = ave
                    'End If

                    'If green < gRange OrElse r > rbRange OrElse b > bRange Then
                    '    Dim ave As Integer = (r + green + b) / 3
                    '    pixels(pos + 2) = ave
                    '    pixels(pos + 1) = ave
                    '    pixels(pos) = ave
                    'End If

                    'If green < r Then
                    '    Dim ave As Integer = (r + green + b) / 3
                    '    pixels(pos + 2) = ave
                    '    pixels(pos + 1) = ave
                    '    pixels(pos) = ave
                    'End If
                    If green * 1.1 < r Or green * 1.0 < b Then
                        Dim ave As Integer = (r + green + b) / 3
                        pixels(pos + 2) = ave
                        pixels(pos + 1) = ave
                        pixels(pos) = ave
                    End If
                Next
            Next
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            bmp.UnlockBits(bmpdate)

            'g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height)) '出来上がったbmpをドローイング
            Me.ActExPic.Image = bmp '表示
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録

            Call Transparent4() '透過表示
            Me.Cursor = Cursors.Default
        End If
    End Sub

    '緑以外白黒
    Friend Sub GreenGrayScale2()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            '特定の色を半透明にする,1pixelづつ判定して塗り替える
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
            Dim bmpdate As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdate.Scan0
            Dim data As Integer = bmpdate.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            Dim x As Integer
            Dim y As Integer
            Dim gRange As Integer = myForm3.NumericUpDownGreen2.Value '赤の数値
            Dim rRange As Integer = myForm3.NumericUpDownRed2.Value '緑と青の数値
            Dim bRange As Integer = myForm3.NumericUpDownBlue2.Value

            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim pos As Integer = (y * bmpdate.Stride + x * 4)
                    Dim r As Integer = pixels(pos + 2)
                    Dim g As Integer = pixels(pos + 1)
                    Dim b As Integer = pixels(pos)

                    If g * rRange < r Or g * bRange < b Then
                        Dim ave As Integer = (r + g + b) / 3
                        pixels(pos + 2) = ave
                        pixels(pos + 1) = ave
                        pixels(pos) = ave
                    End If
                Next
            Next
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            bmp.UnlockBits(bmpdate)

            'g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height)) '出来上がったbmpをドローイング
            Me.ActExPic.Image = bmp '表示
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録

            Call Transparent4() '透過表示
            Me.Cursor = Cursors.Default
        End If
    End Sub
    '青以外白黒
    Friend Sub BlueGrayScale()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            '特定の色を半透明にする,1pixelづつ判定して塗り替える
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
            Dim bmpdate As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdate.Scan0
            Dim data As Integer = bmpdate.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            Dim x As Integer
            Dim y As Integer
            Dim bRange As Integer = myForm3.NumericUpDownBlue.Value '赤の数値
            Dim rgRange As Integer = myForm3.NumericUpDownRedGreen.Value '緑と青の数値

            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim pos As Integer = (y * bmpdate.Stride + x * 4)
                    Dim r As Integer = pixels(pos + 2)
                    Dim green As Integer = pixels(pos + 1)
                    Dim b As Integer = pixels(pos)
                    If b < bRange Then
                        Dim ave As Integer = (r + green + b) / 3
                        pixels(pos + 2) = CByte(ave)
                        pixels(pos + 1) = CByte(ave)
                        pixels(pos) = CByte(ave)
                    ElseIf r > rgRange OrElse green > rgRange Then
                        Dim ave As Integer = (r + green + b) / 3
                        pixels(pos + 2) = ave 'Cbyteで変換しなくてもいいみたい？
                        pixels(pos + 1) = ave
                        pixels(pos) = ave
                    End If

                Next
            Next
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            bmp.UnlockBits(bmpdate)

            'g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height)) '出来上がったbmpをドローイング
            Me.ActExPic.Image = bmp '表示
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録

            Call Transparent() '透過表示
            Me.Cursor = Cursors.Default
        End If
    End Sub
    '青以外白黒2
    Friend Sub BlueGrayScale2()
        If myPicAr.Count <> 0 Then
            Me.Cursor = Cursors.WaitCursor

            '特定の色を半透明にする,1pixelづつ判定して塗り替える
            Dim i As Integer = Me.ActExPic.Tag - 1
            Dim bmp As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
            Dim bmpdate As BitmapData = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdate.Scan0
            Dim data As Integer = bmpdate.Stride * bmp.Height - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            Dim x As Integer
            Dim y As Integer
            Dim gRange As Single = myForm3.NumericUpDownGreen2.Value '赤の数値
            Dim rRange As Single = myForm3.NumericUpDownRed2.Value '緑と青の数値
            Dim bRange As Single = myForm3.NumericUpDownBlue2.Value

            For x = 0 To bmp.Width - 1
                For y = 0 To bmp.Height - 1
                    Dim pos As Integer = (y * bmpdate.Stride + x * 4)
                    Dim r As Integer = pixels(pos + 2)
                    Dim g As Integer = pixels(pos + 1)
                    Dim b As Integer = pixels(pos)

                    If b * gRange < g Or b * rRange < r Then
                        Dim ave As Integer = (r + g + b) / 3
                        pixels(pos + 2) = ave
                        pixels(pos + 1) = ave
                        pixels(pos) = ave
                    End If
                Next
            Next
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, data)
            bmp.UnlockBits(bmpdate)

            'g.DrawImage(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height)) '出来上がったbmpをドローイング
            Me.ActExPic.Image = bmp '表示
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録

            Call Transparent4() '透過表示
            Me.Cursor = Cursors.Default
        End If
    End Sub
    Private Sub CopyImage()
        If myPicAr.Count <> 0 Then

            Clipboard.SetImage(Me.ActExPic.Image)
            'My.Computer.Clipboard.SetImage(myPicArClone(0).Image)
            'My.Computer.Clipboard.SetImage(New Bitmap(FocusPic.Image))

            'Dim i As Integer = Me.FocusPic.Tag - 1
            'Clipboard.SetImage(DirectCast(myPicArClone(i), ExPictureBox).Image)
            'Dim bit As New Bitmap(DirectCast(myPicArClone(i), ExPictureBox).Image)
            'Clipboard.SetImage(bit)

            'Dim clonePic As ExPictureBox = myPicArClone(i)
            'Dim clonePic As ExPictureBox = myPicAr(i)
            'Dim bmp As New Bitmap(clonePic.Width, clonePic.Height)
            'clonePic.DrawToBitmap(bmp, New Rectangle(0, 0, clonePic.Width, clonePic.Height))

            'Clipboard.SetImage(bmp)
            'Clipboard.SetDataObject(Me.FocusPic.Image, True)

            'どうやっても透明色のAlphaの値は無視される、不透明な色になる


        End If
    End Sub

    Private Sub ButtonCopyImage_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCopyImage.Click
        Call CopyImage()

    End Sub
    Private Sub CopyPasteExPictureのコピペ()
        Call CloseEdit編集終了()
        Call MouseEndDrawマウスで描画終了処理()

        If myPicAr.Count = 0 Then Exit Sub
        If Me.CheckBoxDropPoint.Checked Then
            MsgBox("「ドロップした位置」のチェックを外してから実行してください")
            Exit Sub
        End If

        'ActExPicのImageは透過処理されていてるので送るのは透過処理されていないmypicarcloneのImage
        Dim bmp As Bitmap = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
        Dim nName As String = ActExPic.Name
        If nName.StartsWith("fromClipImage") OrElse nName.StartsWith("文字") Then
        Else
            nName = "fromClipImage" & nName
        End If

        '編集可能な図形2ならIsEditとisCloneがTrueでPicBoxAddに送る
        '文字の画像もisCloneはTrueで送るのでこう？
        Call PicBoxAdd(nName, bmp, ActExPic.IsEdit, True)
        

    End Sub
    '画像のコピペ
    Private Sub ButtonCopyPaste_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCopyPaste.Click
        If isDrawEditNow Then Exit Sub '図形の編集中なら何もしないで終了

        Call CopyPasteExPictureのコピペ()
        'Dim cExp As ExPictureBox
        'cExp = ActExPic.Clone

        'cExp.ExPenWidth = 20
        'Dim ooo = cExp.ExPen.Width
        'ooo = ActExPic.ExPen.Width
        'Dim oooo = cExp.ExPen.EndCap
        'oooo = ActExPic.ExPen.EndCap
        'Dim oio = ActExPic

        'If myPicAr.Count <> 0 Then
        '    If Me.CheckBoxDropPoint.Checked Then
        '        MsgBox("「ドロップした位置」のチェックを外してから実行してください")
        '        Exit Sub
        '    End If


        '    tagCount = myPicAr.Count 'タグの初期値にピクチャーボックスの総数を入れる

        '    Dim myPicBox As New ExPictureBox
        '    myPicBox.SizeMode = PictureBoxSizeMode.AutoSize '必須

        '    '画像の初期位置
        '    myPicBox.Location = AddPicPoint()
        '    '画像をピクチャーボックスに割り当て
        '    myPicBox.Image = DirectCast(myPicArClone(Me.ActExPic.Tag - 1), ExPictureBox).Image
        '    'タグの管理の前に名前を取得しないと上層に追加の場合インデックスエラーになる
        '    Dim PicName As String = DirectCast(myPicArClone(Me.ActExPic.Tag - 1), ExPictureBox).Name


        '    'タグの管理
        '    If Me.RadioButtonLower.Checked Then
        '        tagCount = tagCount + 1
        '    ElseIf Me.RadioButtonUpper.Checked Then
        '        For Each c As ExPictureBox In myPicAr
        '            c.Tag = c.Tag + 1
        '        Next
        '        'クローン用
        '        For Each c As ExPictureBox In myPicArClone
        '            c.Tag = c.Tag + 1
        '        Next
        '        'バックアップ用
        '        For Each c As ExPictureBox In myPicArBackup
        '            c.Tag = c.Tag + 1
        '        Next

        '        tagCount = 1
        '    End If

        '    '名前を決める
        '    Dim newName As String
        '    If PicName.StartsWith("fromClipImage") OrElse PicName.StartsWith("文字_") Then
        '        newName = PicName
        '    Else
        '        newName = "fromClipImage" & PicName
        '    End If


        '    myPicBox.Tag = tagCount
        '    ClipCount = ClipCount + 1
        '    'myPicBox.Name = "fromClipImage_" & DirectCast(myPicArClone(Me.FocusPic.Tag - 1), ExPictureBox).Name
        '    myPicBox.Name = newName
        '    'ファイル名を取得してピクチャーボックスの名前にする

        '    '右クリックメニュー追加、2014/12/13
        '    myPicBox.ContextMenuStrip = Me.ContextMenuStrip1


        '    'myPicAr.Add(myPicBox)
        '    '完成したピクチャーボックスをコレクションに追加
        '    If Me.RadioButtonLower.Checked Then
        '        myPicAr.Add(myPicBox)
        '    ElseIf Me.RadioButtonUpper.Checked Then
        '        myPicAr.Insert(0, myPicBox)
        '    End If



        '    'クローンのピクチャーボックスをクローン用のコレクションに追加
        '    Dim myPicBox2 As New ExPictureBox
        '    With myPicBox2
        '        .Location = myPicBox.Location
        '        .SizeMode = myPicBox.SizeMode
        '        '.Name = "クローン_fromClipImage_" & tagCount
        '        '.Name = "fromClipImage_" & DirectCast(myPicArClone(Me.FocusPic.Tag - 1), ExPictureBox).Name
        '        .Name = newName
        '        .Image = myPicBox.Image
        '        .Tag = myPicBox.Tag
        '    End With
        '    'クローン用のコレクションに追加
        '    If Me.RadioButtonLower.Checked Then
        '        myPicArClone.Add(myPicBox2)
        '    ElseIf Me.RadioButtonUpper.Checked Then
        '        myPicArClone.Insert(0, myPicBox2)
        '    End If

        '    'バックアップ用のピクチャーボックス作成
        '    Dim myPicBackup As New ExPictureBox
        '    With myPicBackup
        '        .Location = myPicBox.Location
        '        .SizeMode = PictureBoxSizeMode.Normal
        '        '.Name = "fromClipImage_" & DirectCast(myPicArClone(Me.FocusPic.Tag - 1), ExPictureBox).Name
        '        .Name = newName
        '        .Image = myPicBox.Image
        '        .Tag = myPicBox.Tag
        '    End With
        '    'バックアップ用のコレクションに追加
        '    If Me.RadioButtonLower.Checked Then
        '        myPicArBackup.Add(myPicBackup)
        '    ElseIf Me.RadioButtonUpper.Checked Then
        '        myPicArBackup.Insert(0, myPicBackup)
        '    End If


        '    'myPicArClone.Add(myPicBox)

        '    Me.Panel2.Controls.Add(myPicBox) 'ピクチャーボックスを表示
        '    'イベントに関連付ける
        '    AddHandler myPicBox.MouseEnter, AddressOf PictureBox1_MouseEnter
        '    AddHandler myPicBox.MouseUp, AddressOf PictureBox1_MouseUp
        '    AddHandler myPicBox.MouseLeave, AddressOf PictureBox1_MouseLeave
        '    AddHandler myPicBox.MouseMove, AddressOf PictureBox1_MouseMove '大幅変更箇所
        '    AddHandler myPicBox.MouseDown, AddressOf PictureBox1_MouseDown '大幅変更箇所
        '    Call SortPic()

        '    Me.TextBox1.Text = myPicAr.Count
        '    FullSize = RigthDownPoint()
        '    Me.TextBox9.Text = FullSize.ToString
        '    Me.NumNowPic.Maximum = myPicAr.Count '最大値設定
        '    Me.NumNowPic.Value = NumNowPic.Maximum

        '    For Each c As ExPictureBox In myPicAr '追加した画像をnowcontrolとfocuspicにする
        '        If c.Tag = tagCount Then
        '            'NowControl = c
        '            ActExPic = c
        '        End If
        '    Next
        '    'NowControl.Focus()
        '    'ActExPic.Focus()
        '    'Me.ListBox1.Focus()
        '    'Me.CurrentPic.Image = FocusPic.Image
        '    Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
        '    Me.PictureBoxBackup.Image = DirectCast(myPicArBackup(ActExPic.Tag - 1), ExPictureBox).Image
        '    Call ChangeFocusTステータス表示更新()
        '    Call MoveAfter()
        '    Call Transparent2()

        'End If

    End Sub
    Friend Sub CopyPsteTransparent()
        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        If Me.CheckBoxDropPoint.Checked Then
            MsgBox("「ドロップした位置」のチェックを外してから実行してください")
            Exit Sub
        End If
        Call CloseEdit編集終了()
        Call MouseEndDrawマウスで描画終了処理()

        Dim name As String = ActExPic.Name & "_copy_T"
        Dim bmp As New Bitmap(DirectCast(myPicAr(ActExPic.Tag - 1), ExPictureBox).Image)

        Call PicBoxAdd(name, bmp)

    End Sub

    '画像の保存前にコレクションの並び替えをするメソッド、タイム計測付き
    Private Sub SortForSavetime()
        'Dim Sw As New System.Diagnostics.Stopwatch
        'Sw.Start()

        Dim j As Integer
        myPicArR.Clear()

        'For j = myPicAr.Count To 1 Step -1
        '    For Each c As Control In myPicAr
        '        If c.Tag = j Then
        '            myPicArR.Add(c)
        '        End If
        '    Next
        'Next

        '↑より↓のほうが3～4倍速いけどどうなんだろう
        For j = myPicAr.Count - 1 To 0 Step -1
            myPicArR.Add(myPicAr(j))
        Next

        'Sw.Stop()
        'Debug.WriteLine("処理時間" & Sw.Elapsed.ToString)

    End Sub

    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click
        'SortForSavetime()
        Call ZaoraruPicBox()
    End Sub

    ''半自動再描画のチェック
    'Private Sub CheckBoxHalfRealtimeRendering_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxHalfRealtimeRendering.CheckedChanged
    '    If Me.CheckBoxHalfRealtimeRendering.Checked Then
    '        HalfRealtimeRendering = True
    '    ElseIf Me.CheckBoxHalfRealtimeRendering.Checked = False Then
    '        HalfRealtimeRendering = False
    '    End If
    'End Sub

    '枠の画像を作る、2014年3月23日大幅変更
    Friend Function FlameAdd(ByVal rw As Integer, ByVal rh As Integer, ByVal pw As Integer, ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color) As Bitmap

        'Dim rw As Integer = myForm3.NumericUpDownRectWidth.Value '枠の幅
        'Dim rh As Integer = myForm3.NumericUpDownRectHeight.Value '枠の高さ
        'Dim pw As Integer = myForm3.NumericUpDownPenWidth.Value '枠の太さ

        'Dim col As Color = Color.FromArgb(myForm3.NumericUpDownSquareTransparent.Value, myForm3.ButtonSquareColor1.ForeColor)
        'Dim col2 As Color = Color.FromArgb(myForm3.NumericUpDownSquareTransparent2.Value, myForm3.ButtonSquareColor2.ForeColor)
        'Dim col3 As Color = Color.FromArgb(myForm3.NumericUpDownSquareTransparent3.Value, myForm3.ButtonSquareColor3.ForeColor)

        Dim x As Integer = Math.Floor(pw / 2) '切り捨て
        'Dim x As Integer = pw / 2
        Dim y As Integer = Math.Floor(pw / 2)
        Dim bmp As New Bitmap(rw, rh)
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim rPen As New Pen(col, pw) '枠の色と太さ指定
        Dim name As String = "枠" 'ピクチャーボックスの名前

        '単色塗り
        g.DrawRectangle(rPen, x, y, rw - pw, rh - pw)

        'グラデーション塗り
        If myForm3.CheckBoxSquareGradation.Checked Then
            bmp = GradationGammaBitmapAdd(bmp, col, col2, col3)
        End If
        g.Dispose()
        Return bmp

        'Call PicBoxAdd(name, bmp)

    End Function



    '枠の画像を作る
    Friend Sub RectangleAdd()
        Dim rw As Integer = myForm3.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = myForm3.NumericUpDownRectHeight.Value '枠の高さ
      
        Dim bmp As New Bitmap(rw, rh)
        Dim rect As New Rectangle(0, 0, rw, rh)
        Dim g As Graphics = Graphics.FromImage(bmp)
        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(col)


        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then
            Dim gCol1 As Color = myForm3.ButtonSquareColor1.ForeColor
            Dim gCol2 As Color = myForm3.ButtonSquareColor2.ForeColor

            If myForm3.RadioButtonSquareGradaH.Checked Then
                Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
                g.FillRectangle(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
                g.FillRectangle(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                g.FillRectangle(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                g.FillRectangle(gradationB, rect)
            End If


            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
        Else
            'g.DrawRectangle(Pens.AliceBlue, 0, 0, rw, rh)
            g.FillRectangle(bru, 0, 0, rw, rh)
            'g.FillRectangle(bur, 0, 0, rw, rh)
        End If

        '透明度
        Dim x As Integer
        Dim y As Integer
        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * bmp.Height - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
        Dim transparent As Integer = myForm3.NumericUpDownSquareTransparent.Value

        For x = 0 To rw - 1
            For y = 0 To rh - 1
                Dim pos As Integer = y * bmpdata.Stride + x * 4 + 3
                pixels(pos) = CByte(transparent)
            Next
        Next
        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)


        'Dim bur As Brush = Brushes.AliceBlue
        'Dim name As String = "四角形" 'ピクチャーボックスの名前
        '透明度が255より小さければ名前の末尾に_Tをつける
        If transparent < 255 Then
            name = name & "_T"

        End If
        
        Call PicBoxAdd(name, bmp)
        'Me.FocusPic.Image = bmp
    End Sub

    '四角形の画像を作る subの改変
    Friend Function RectangleAdd(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color) As Bitmap
        'Dim rw as Integer = myForm3.NumericUpDownRectWidth.Value '枠の幅
        'Dim rh As Integer = myForm3.NumericUpDownRectHeight.Value '枠の高さ

        Dim bmp As New Bitmap(rw, rh)
        Dim rect As New Rectangle(0, 0, rw, rh)
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh)
        'Dim rectF As New RectangleF(0, 0, rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)
        If myForm3.NumericUpDownRectangleAngle.Value = 0 Then
            g.SmoothingMode = SmoothingMode.None 'アンチエイリアス効果なし
        Else
            g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス効果あり
        End If


        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(col)
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then
            Dim r1 As Integer = col.R
            Dim g1 As Integer = col.G
            Dim b1 As Integer = col.B
            Dim a1 As Integer = col.A
            Dim r2 As Integer = col2.R
            Dim g2 As Integer = col2.G
            Dim b2 As Integer = col2.B
            Dim a2 As Integer = col2.A
            Dim r3 As Integer = col3.R
            Dim g3 As Integer = col3.G
            Dim b3 As Integer = col3.B
            Dim a3 As Integer = col3.A

            'Dim rAdd As Single = (col2.R - col1.R) / (rw - 1)
            'Dim rAdd As Double = CInt(col2.R - col1.R) / (rw - 1)
            Dim rAdd As Single
            Dim gAdd As Single
            Dim bAdd As Single
            Dim aAdd As Single
            Dim rAdd2 As Single '3色グラデーション
            Dim gAdd2 As Single
            Dim bAdd2 As Single
            Dim aAdd2 As Single

            Dim rDiff As Integer = r1 - r2
            Dim gDiff As Integer = g1 - g2
            Dim bDiff As Integer = b1 - b2
            Dim rDiff2 As Integer = r2 - r3 '3色グラデーション
            Dim gDiff2 As Integer = g2 - g3
            Dim bDiff2 As Integer = b2 - b3

            '1ピクセルごとに変化する値
            If myForm3.RadioButtonSquareGradaH.Checked And rw > 1 Then

                If myForm3.RadioButtonShape2ColorGrada.Checked Then
                    '2色グラデーション横
                    rAdd = (r2 - r1) / (rw - 1)
                    'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
                    gAdd = (g2 - g1) / (rw - 1)
                    bAdd = (b2 - b1) / (rw - 1)
                    aAdd = (a2 - a1) / (rw - 1)
                Else '3色グラデーション横
                    Dim val As Integer = Math.Floor((rw - 1) / 2) '切り捨て
                    rAdd = (r2 - r1) / val
                    'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
                    gAdd = (g2 - g1) / val
                    bAdd = (b2 - b1) / val
                    aAdd = (a2 - a1) / val
                    rAdd2 = (r3 - r2) / val
                    gAdd2 = (g3 - g2) / val
                    bAdd2 = (b3 - b2) / val
                    aAdd2 = (a3 - a2) / val

                End If

            ElseIf myForm3.RadioButtonSquareGradaV.Checked And rh > 1 Then
                If myForm3.RadioButtonShape2ColorGrada.Checked Then
                    rAdd = (r2 - r1) / (rh - 1)
                    gAdd = (g2 - g1) / (rh - 1)
                    bAdd = (b2 - b1) / (rh - 1)
                    aAdd = (a2 - a1) / (rh - 1)
                Else
                    Dim val As Integer = Math.Floor((rh - 1) / 2) '切り捨て
                    rAdd = (r2 - r1) / val
                    gAdd = (g2 - g1) / val
                    bAdd = (b2 - b1) / val
                    aAdd = (a2 - a1) / val
                    rAdd2 = (r3 - r2) / val
                    gAdd2 = (g3 - g2) / val
                    bAdd2 = (b3 - b2) / val
                    aAdd2 = (a3 - a2) / val

                End If

            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked OrElse myForm3.RadioButtonSquareGradaLUp.Checked Then
                If rw + rh > 2 Then '右上
                    If myForm3.RadioButtonShape2ColorGrada.Checked Then
                        rAdd = (r2 - r1) / (rw + rh - 2)
                        gAdd = (g2 - g1) / (rw + rh - 2)
                        bAdd = (b2 - b1) / (rw + rh - 2)
                        aAdd = (a2 - a1) / (rw + rh - 2)
                    Else
                        Dim val As Integer = Math.Floor((rw + rh - 2) / 2) '切り捨て
                        rAdd = (r2 - r1) / val
                        gAdd = (g2 - g1) / val
                        bAdd = (b2 - b1) / val
                        aAdd = (a2 - a1) / val
                        rAdd2 = (r3 - r2) / val
                        gAdd2 = (g3 - g2) / val
                        bAdd2 = (b3 - b2) / val
                        aAdd2 = (a3 - a2) / val

                    End If
                End If


                'ElseIf myForm3.RadioButtonSquareGradaLUp.Checked And rw + rh > 2 Then
                '    rAdd = (r2 - r1) / (rw + rh - 2)
                '    gAdd = (g2 - g1) / (rw + rh - 2)
                '    bAdd = (b2 - b1) / (rw + rh - 2)
                '    aAdd = (a2 - a1) / (rw + rh - 2)
            ElseIf myForm3.RadioButtonSquareGradaLR.Checked And rw > 2 Then '左右からのグラデーション
                rAdd = (r2 - r1) / Math.Floor((rw - 1) / 2) '切り捨て
                gAdd = (g2 - g1) / Math.Floor((rw - 1) / 2)
                bAdd = (b2 - b1) / Math.Floor((rw - 1) / 2)
                aAdd = (a2 - a1) / Math.Floor((rw - 1) / 2)
            ElseIf myForm3.RadioButtonSquareGradaUD.Checked And rh > 2 Then '上下からのグラデーション
                rAdd = (r2 - r1) / Math.Floor((rh - 1) / 2) '切り捨て
                gAdd = (g2 - g1) / Math.Floor((rh - 1) / 2)
                bAdd = (b2 - b1) / Math.Floor((rh - 1) / 2)
                aAdd = (a2 - a1) / Math.Floor((rh - 1) / 2)
                '斜めのグラデーション、2方向
            ElseIf myForm3.RadioButtonSquareGradaLU2.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                If rw + rh > 3 Then
                    rAdd = (r2 - r1) / Math.Floor((rw + rh - 2) / 2)
                    gAdd = (g2 - g1) / Math.Floor((rw + rh - 2) / 2)
                    bAdd = (b2 - b1) / Math.Floor((rw + rh - 2) / 2)
                    aAdd = (a2 - a1) / Math.Floor((rw + rh - 2) / 2)

                End If
            Else
                rAdd = (r2 - r1)
                gAdd = (g2 - g1)
                bAdd = (b2 - b1)
                aAdd = (a2 - a1)

            End If

            'ガンマ補正用変数ここから---------------------------
            'Const myGamma As Single = 2.2
            Dim myGamma As Single = myForm3.TrackBarShapeGamma.Value / 10
            Dim redGamma As Single = myForm3.TrackBarShapeGammaR.Value / 10
            Dim greenGamma As Single = myForm3.TrackBarShapeGammaG.Value / 10
            Dim blueGamma As Single = myForm3.TrackBarShapeGammaB.Value / 10

            '色1と色2のガンマ補正の差分(変化総量)DiffGamma
            Dim r1Gamma As Single
            Dim r2Gamma As Single
            Dim r3Gamma As Single
            Dim rDiffGamma As Single
            Dim rDiffGamma2 As Single
            Dim g1Gamma As Single
            Dim g2Gamma As Single
            Dim g3Gamma As Single
            Dim gDiffGamma As Single
            Dim gDiffGamma2 As Single
            Dim b1Gamma As Single
            Dim b2Gamma As Single
            Dim b3Gamma As Single
            Dim bDiffGamma As Single
            Dim bDiffGamma2 As Single

            If myForm3.CheckBoxShapeGammaRGB.Checked Then

                r1Gamma = (r1 / 255) ^ (1 / redGamma)
                r2Gamma = (r2 / 255) ^ (1 / redGamma)
                r3Gamma = (r3 / 255) ^ (1 / redGamma)
                rDiffGamma = r1Gamma - r2Gamma
                rDiffGamma2 = r2Gamma - r3Gamma
                g1Gamma = (g1 / 255) ^ (1 / greenGamma)
                g2Gamma = (g2 / 255) ^ (1 / greenGamma)
                g3Gamma = (g3 / 255) ^ (1 / greenGamma)
                gDiffGamma = g1Gamma - g2Gamma
                gDiffGamma2 = g2Gamma - g3Gamma
                b1Gamma = (b1 / 255) ^ (1 / blueGamma)
                b2Gamma = (b2 / 255) ^ (1 / blueGamma)
                b3Gamma = (b3 / 255) ^ (1 / blueGamma)
                bDiffGamma = b1Gamma - b2Gamma
                bDiffGamma2 = b2Gamma - b3Gamma

            Else
                redGamma = myGamma
                greenGamma = myGamma
                blueGamma = myGamma

                r1Gamma = (r1 / 255) ^ (1 / myGamma)
                r2Gamma = (r2 / 255) ^ (1 / myGamma)
                r3Gamma = (r3 / 255) ^ (1 / myGamma)
                rDiffGamma = r1Gamma - r2Gamma
                rDiffGamma2 = r2Gamma - r3Gamma
                g1Gamma = (g1 / 255) ^ (1 / myGamma)
                g2Gamma = (g2 / 255) ^ (1 / myGamma)
                g3Gamma = (g3 / 255) ^ (1 / myGamma)
                gDiffGamma = g1Gamma - g2Gamma
                gDiffGamma2 = g2Gamma - g3Gamma
                b1Gamma = (b1 / 255) ^ (1 / myGamma)
                b2Gamma = (b2 / 255) ^ (1 / myGamma)
                b3Gamma = (b3 / 255) ^ (1 / myGamma)
                bDiffGamma = b1Gamma - b2Gamma
                bDiffGamma2 = b2Gamma - b3Gamma

            End If

            '3色グラデーション用
            Dim colCenter As Integer
            Dim colCenter2 As Integer
            If myForm3.RadioButtonShape3ColorGrada.Checked Then
                colCenter = Math.Floor((rw - 1) / 2)
                colCenter2 = Math.Round(colCenter, MidpointRounding.AwayFromZero)

            End If
            'ガンマ補正用変数ここまで------------------------------



            Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdata.Scan0
            Dim data As Integer = bmpdata.Stride * rh - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            If myForm3.RadioButtonSquareGradaH.Checked Then

                If myForm3.RadioButtonShape2ColorGrada.Checked Then
                    '2色グラデーション横
                    For x = 0 To rw - 1
                        For y = 0 To rh - 1
                            Dim pos As Integer = y * bmpdata.Stride + x * 4

                            pixels(pos) = CByte((bAdd * x) + b1)
                            pixels(pos + 1) = CByte((gAdd * x) + g1)
                            pixels(pos + 2) = CByte((rAdd * x) + r1)
                            pixels(pos + 3) = CByte((aAdd * x) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    'pixels(pos + 2) = CByte((((pixels(pos + 2) / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2) 'これ
                                End If

                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    'pixels(pos + 1) = CByte((((pixels(pos + 1) / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2) 'これ
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    'pixels(pos) = CByte((((pixels(pos) / 255) ^ (1 / blueGamma) - b2Gamma) / bDiffGamma) * bDiff + b2) 'これ
                                End If
                            End If

                        Next
                    Next

                    '3色グラデーション横
                Else
                    Dim val As Integer = Math.Floor((rw - 1) / 2) '中間地点
                    'Dim val2 As Integer = val + 1 '中間地点の次の地点
                    Dim val3 As Integer = Math.Round((rw - 1) / 2, MidpointRounding.AwayFromZero) '中間地点を四捨五入

                    For x As Integer = 0 To rw - 1
                        If x <= val Then '左半分
                            For y = 0 To rh - 1
                                Dim pos As Integer = y * bmpdata.Stride + x * 4

                                pixels(pos) = CByte((bAdd * x) + b1)
                                pixels(pos + 1) = CByte((gAdd * x) + g1)
                                pixels(pos + 2) = CByte((rAdd * x) + r1)
                                pixels(pos + 3) = CByte((aAdd * x) + a1)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    End If
                                    If gDiffGamma <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    End If
                                    If bDiffGamma <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    End If
                                End If
                            Next

                        ElseIf x > val Then '右半分

                            For y = 0 To rh - 1
                                Dim pos As Integer = y * bmpdata.Stride + x * 4

                                pixels(pos) = CByte((bAdd2 * (x - val3)) + b2)
                                pixels(pos + 1) = CByte((gAdd2 * (x - val3)) + g2)
                                pixels(pos + 2) = CByte((rAdd2 * (x - val3)) + r2)
                                pixels(pos + 3) = CByte((aAdd2 * (x - val3)) + a2)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma2 <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                        Dim valr As Byte = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                    End If
                                    If gDiffGamma2 <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                        Dim valg As Byte = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                    End If
                                    If bDiffGamma2 <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                        Dim balb As Byte = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                    End If
                                End If
                            Next

                        End If

                    Next
                End If

            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then

                If myForm3.RadioButtonShape2ColorGrada.Checked Then
                    For x = 0 To rw - 1
                        For y = 0 To rh - 1
                            Dim pos As Integer = y * bmpdata.Stride + x * 4

                            pixels(pos) = CByte((bAdd * y) + b1)
                            pixels(pos + 1) = CByte((gAdd * y) + g1)
                            pixels(pos + 2) = CByte((rAdd * y) + r1)
                            pixels(pos + 3) = CByte((aAdd * y) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If
                        Next
                    Next

                    '3色グラデーション
                Else
                    Dim val As Integer = Math.Floor((rh - 1) / 2) '中間地点
                    'Dim val2 As Integer = val + 1 '中間地点の次の地点
                    Dim val3 As Integer = Math.Round((rh - 1) / 2, MidpointRounding.AwayFromZero) '中間地点を四捨五入

                    For x As Integer = 0 To rw - 1
                        For y = 0 To rh - 1
                            If y <= val Then '上半分
                                Dim pos As Integer = y * bmpdata.Stride + x * 4

                                pixels(pos) = CByte((bAdd * y) + b1)
                                pixels(pos + 1) = CByte((gAdd * y) + g1)
                                pixels(pos + 2) = CByte((rAdd * y) + r1)
                                pixels(pos + 3) = CByte((aAdd * y) + a1)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    End If
                                    If gDiffGamma <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    End If
                                    If bDiffGamma <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    End If
                                End If

                            ElseIf y > val Then '下半分
                                Dim pos As Integer = y * bmpdata.Stride + x * 4

                                pixels(pos) = CByte((bAdd2 * (y - val3)) + b2)
                                pixels(pos + 1) = CByte((gAdd2 * (y - val3)) + g2)
                                pixels(pos + 2) = CByte((rAdd2 * (y - val3)) + r2)
                                pixels(pos + 3) = CByte((aAdd2 * (y - val3)) + a2)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma2 <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                        Dim valr As Byte = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                    End If
                                    If gDiffGamma2 <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                        Dim valg As Byte = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                    End If
                                    If bDiffGamma2 <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                        Dim balb As Byte = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                    End If
                                End If
                            End If

                        Next

                    Next
                End If


            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                '2色グラデーション右上

                If myForm3.RadioButtonShape2ColorGrada.Checked Then
                    For x = 0 To rw - 1
                        For y = 0 To rh - 1
                            Dim pos As Integer = y * bmpdata.Stride + x * 4

                            pixels(pos) = CByte((bAdd * (rw - 1 - x + y)) + b1)
                            pixels(pos + 1) = CByte((gAdd * (rw - 1 - x + y)) + g1)
                            pixels(pos + 2) = CByte((rAdd * (rw - 1 - x + y)) + r1)
                            'pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                            pixels(pos + 3) = CByte((aAdd * (rw - 1 - x + y)) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If

                        Next
                    Next
                Else
                    '3色グラデーション右上
                    Dim val As Integer = Math.Floor((rw + rh - 2) / 2) '中間地点
                    'Dim val2 As Integer = val + 1 '中間地点の次の地点
                    Dim val3 As Integer = Math.Round((rw + rh - 2) / 2, MidpointRounding.AwayFromZero) '中間地点を四捨五入
                    For x As Integer = 0 To rw - 1
                        For y = 0 To rh - 1
                            If Math.Abs(x - (rw - 1)) + y <= val Then '左上半分
                                Dim pos As Integer = y * bmpdata.Stride + x * 4

                                pixels(pos) = CByte((bAdd * (Math.Abs(x - (rw - 1)) + y)) + b1)
                                pixels(pos + 1) = CByte((gAdd * (Math.Abs(x - (rw - 1)) + y)) + g1)
                                pixels(pos + 2) = CByte((rAdd * (Math.Abs(x - (rw - 1)) + y)) + r1)
                                pixels(pos + 3) = CByte((aAdd * (Math.Abs(x - (rw - 1)) + y)) + a1)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    End If
                                    If gDiffGamma <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    End If
                                    If bDiffGamma <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    End If
                                End If

                            ElseIf (Math.Abs(x - (rw - 1)) + y) > val Then '右下半分
                                Dim pos As Integer = y * bmpdata.Stride + x * 4

                                pixels(pos) = CByte((bAdd2 * ((Math.Abs(x - (rw - 1)) + y) - val3)) + b2)
                                pixels(pos + 1) = CByte((gAdd2 * ((Math.Abs(x - (rw - 1)) + y) - val3)) + g2)
                                pixels(pos + 2) = CByte((rAdd2 * ((Math.Abs(x - (rw - 1)) + y) - val3)) + r2)
                                pixels(pos + 3) = CByte((aAdd2 * ((Math.Abs(x - (rw - 1)) + y) - val3)) + a2)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma2 <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                        Dim valr As Byte = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                    End If
                                    If gDiffGamma2 <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                        Dim valg As Byte = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                    End If
                                    If bDiffGamma2 <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                        Dim balb As Byte = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                    End If
                                End If

                            End If

                        Next

                    Next
                End If


            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                '2色グラデーション左上
                If myForm3.RadioButtonShape2ColorGrada.Checked Then
                    For x = 0 To rw - 1
                        For y = 0 To rh - 1
                            Dim pos As Integer = y * bmpdata.Stride + x * 4

                            pixels(pos) = CByte((bAdd * (x + y)) + b1)
                            pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
                            pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                            pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If

                        Next
                    Next
                Else
                    '3色グラデーション左上
                    Dim val As Integer = Math.Floor((rw + rh - 2) / 2) '中間地点
                    'Dim val2 As Integer = val + 1 '中間地点の次の地点
                    Dim val3 As Integer = Math.Round((rw + rh - 2) / 2, MidpointRounding.AwayFromZero) '中間地点を四捨五入
                    For x As Integer = 0 To rw - 1
                        For y = 0 To rh - 1
                            If (x + y) <= val Then '左上半分
                                Dim pos As Integer = y * bmpdata.Stride + x * 4

                                pixels(pos) = CByte((bAdd * (x + y)) + b1)
                                pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
                                pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                                pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    End If
                                    If gDiffGamma <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    End If
                                    If bDiffGamma <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    End If
                                End If

                            ElseIf (x + y) > val Then '右下半分
                                Dim pos As Integer = y * bmpdata.Stride + x * 4

                                pixels(pos) = CByte((bAdd2 * ((x + y) - val3)) + b2)
                                pixels(pos + 1) = CByte((gAdd2 * ((x + y) - val3)) + g2)
                                pixels(pos + 2) = CByte((rAdd2 * ((x + y) - val3)) + r2)
                                pixels(pos + 3) = CByte((aAdd2 * ((x + y) - val3)) + a2)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma2 <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                        Dim valr As Byte = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                    End If
                                    If gDiffGamma2 <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                        Dim valg As Byte = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                    End If
                                    If bDiffGamma2 <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                        Dim balb As Byte = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                    End If
                                End If

                            End If

                        Next

                    Next
                End If


            ElseIf myForm3.RadioButtonSquareGradaLR.Checked Then '左右からのグラデーション
                For x As Integer = 0 To Math.Floor((rw - 1) / 2) '小数点切り捨て
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        Dim pos2 As Integer = y * bmpdata.Stride + (rw - 1 - x) * 4 '反対側のピクセル

                        pixels(pos) = CByte((bAdd * x) + b1)
                        pixels(pos2) = CByte((bAdd * x) + b1)
                        pixels(pos + 1) = CByte((gAdd * x) + g1)
                        pixels(pos2 + 1) = CByte((gAdd * x) + g1)
                        pixels(pos + 2) = CByte((rAdd * x) + r1)
                        pixels(pos2 + 2) = CByte((rAdd * x) + r1)
                        pixels(pos + 3) = CByte((aAdd * x) + a1)
                        pixels(pos2 + 3) = CByte((aAdd * x) + a1)

                        If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                            If rDiffGamma <> 0 Then
                                pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                pixels(pos2 + 2) = GradationGamma(pixels(pos2 + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                            End If
                            If gDiffGamma <> 0 Then
                                pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                pixels(pos2 + 1) = GradationGamma(pixels(pos2 + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                            End If
                            If bDiffGamma <> 0 Then
                                pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                pixels(pos2) = GradationGamma(pixels(pos2), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                            End If
                        End If

                    Next
                Next
            ElseIf myForm3.RadioButtonSquareGradaUD.Checked Then '上下からのグラデーション
                For x As Integer = 0 To rw - 1
                    For y = 0 To Math.Floor((rh - 1) / 2) '小数点切り捨て
                        'Dim val As Integer = Math.Floor((rh - 1) / 2) '小数点切り捨て
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        Dim pos2 As Integer = (rh - 1 - y) * bmpdata.Stride + x * 4 '反対側のピクセル

                        pixels(pos) = CByte((bAdd * y) + b1)
                        pixels(pos2) = CByte((bAdd * y) + b1)
                        pixels(pos + 1) = CByte((gAdd * y) + g1)
                        pixels(pos2 + 1) = CByte((gAdd * y) + g1)
                        pixels(pos + 2) = CByte((rAdd * y) + r1)
                        pixels(pos2 + 2) = CByte((rAdd * y) + r1)
                        'Dim val1 As Byte = CByte((rAdd * y) + r1)
                        pixels(pos + 3) = CByte((aAdd * y) + a1)
                        pixels(pos2 + 3) = CByte((aAdd * y) + a1)

                        If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                            If rDiffGamma <> 0 Then
                                pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                pixels(pos2 + 2) = GradationGamma(pixels(pos2 + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                            End If
                            If gDiffGamma <> 0 Then
                                pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                pixels(pos2 + 1) = GradationGamma(pixels(pos2 + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                            End If
                            If bDiffGamma <> 0 Then
                                pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                pixels(pos2) = GradationGamma(pixels(pos2), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                            End If
                        End If
                    Next
                Next
            ElseIf myForm3.RadioButtonSquareGradaLU2.Checked Then '斜めのグラデーション
                Dim range As Integer = rw - 1 + rh - 1 '総距離
                Dim hRange As Integer = Math.Floor(range / 2) '切り返しの中間地点、小数点切り捨て

                For x As Integer = 0 To rw - 1
                    For y = 0 To rh - 1

                        If x + y <= hRange Then '前半
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            pixels(pos) = CByte((bAdd * (x + y)) + b1)
                            'pixels(pos2) = CByte((bAdd * y) + b1)
                            pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
                            'pixels(pos2 + 1) = CByte((gAdd * y) + g1)
                            pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                            'pixels(pos2 + 2) = CByte((rAdd * y) + r1)
                            'Dim val1 As Byte = CByte((rAdd * y) + r1)
                            pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)
                            'pixels(pos2 + 3) = CByte((aAdd * y) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If
                        Else '後半
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            pixels(pos) = CByte((bAdd * (range - (x + y))) + b1)
                            pixels(pos + 1) = CByte((gAdd * (range - (x + y))) + g1)
                            pixels(pos + 2) = CByte((rAdd * (range - (x + y))) + r1)
                            pixels(pos + 3) = CByte((aAdd * (range - (x + y))) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If
                        End If

                    Next
                Next

            ElseIf myForm3.RadioButtonSquareGradaRU2.Checked Then '斜めのグラデーション右上
                Dim range As Integer = rw - 1 + rh - 1 '総距離
                Dim hRange As Integer = Math.Floor(range / 2) '切り返しの中間地点、小数点切り捨て

                For x As Integer = rw - 1 To 0 Step -1
                    'For x As Integer = 0 To rw - 1
                    For y = 0 To rh - 1

                        If Math.Abs(x - (rw - 1)) + y <= hRange Then '前半
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            'Dim val2 As Single = ((rAdd * (Math.Abs(x - (rw - 1)) + y)) + r1)
                            'Dim val As Single = ((bAdd * (Math.Abs(x - rw) + y)) + b1)
                            pixels(pos) = CByte((bAdd * (Math.Abs(x - (rw - 1)) + y)) + b1)
                            'pixels(pos) = CByte((bAdd * (Math.Abs(x - rw) + y)) + b2)
                            pixels(pos + 1) = CByte((gAdd * (Math.Abs(x - (rw - 1)) + y)) + g1)
                            pixels(pos + 2) = CByte((rAdd * (Math.Abs(x - (rw - 1)) + y)) + r1)
                            pixels(pos + 3) = CByte((aAdd * (Math.Abs(x - (rw - 1)) + y)) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If

                        Else '後半
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            pixels(pos) = CByte((bAdd * (range - (Math.Abs(x - (rw - 1)) + y))) + b1)
                            pixels(pos + 1) = CByte((gAdd * (range - (Math.Abs(x - (rw - 1)) + y))) + g1)
                            pixels(pos + 2) = CByte((rAdd * (range - (Math.Abs(x - (rw - 1)) + y))) + r1)
                            pixels(pos + 3) = CByte((aAdd * (range - (Math.Abs(x - (rw - 1)) + y))) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If
                           
                        End If

                    Next
                Next
            End If

           
            'ガンマ補正
            'If myForm3.CheckBoxRectGradationGamma.Checked Then


            '    If myForm3.RadioButtonShape2ColorGrada.Checked Then
            '        For x = 0 To rw - 1
            '            For y = 0 To rh - 1

            '                Dim pos As Integer = y * bmpdata.Stride + x * 4


            'If rDiffGamma <> 0 Then

            '    Dim r As Integer = pixels(pos + 2)
            '    Dim val As Single = r / 255
            '    Dim val2 As Single = val ^ (1 / redGamma)
            '    'Dim val2 As Single = val ^ (1 / 2.2)
            '    Dim val3 As Single = (val2 - r2Gamma) / rDiffGamma
            '    Dim val4 As Single = val3 * rDiff
            '    Dim val5 As Single = val4 + r2
            '    'pixels(pos + 2) = CByte(val5)
            '    Dim val6 As Byte = GradationGamma(r, redGamma, r2Gamma, rDiffGamma, rDiff, r2) '四捨五入になっている

            '    If val5 < 0 Then
            '        val5 = 0
            '    ElseIf val5 > 255 Then
            '        val5 = 255
            '    End If
            '    'pixels(pos + 2) = CByte(val5)
            '    pixels(pos + 2) = val6

            '    'If myForm3.RadioButtonShape2ColorGrada.Checked Then
            '    '    pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)
            '    'Else
            '    '    pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff2 + r2)
            '    'End If

            '    'pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)'これ
            '    ''pixels(pos + 2) = CByte((((r / 255) ^ (1 / myGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)

            'End If

            'If gDiffGamma <> 0 Then

            '    Dim gr As Integer = pixels(pos + 1)
            '    Dim val As Single = ((((gr / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)

            '    If val < 0 Then
            '        val = 0
            '    ElseIf val > 255 Then
            '        val = 255
            '    End If
            '    pixels(pos + 1) = CByte(val)
            '    'pixels(pos + 1) = CByte((((gr / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)

            'End If

            'If bDiffGamma <> 0 Then
            '    Dim b As Integer = pixels(pos)
            '    Dim val As Single = b / 255 '今の色の位置
            '    Dim val2 As Single = val ^ (1 / blueGamma) '今の色のガンマ補正の位置
            '    Dim val3 As Single = (val2 - b2Gamma) / bDiffGamma '

            '    Dim val4 As Single = val3 * bDiff
            '    Dim val5 As Single = val4 + b2
            '    If val5 < 0 Then
            '        val5 = 0
            '    ElseIf val5 > 255 Then
            '        val5 = 255
            '    End If
            '    pixels(pos) = CByte(val5)

            '    'pixels(pos) = CByte((((b / 255) ^ (1 / blueGamma) - b2Gamma) / bDiffGamma) * bDiff + b2)

            '    'Dim val11 As Single = b / 255
            '    'Dim val21 As Single = val11 ^ (1 / blueGamma) '(b/255)^(1/bluegamma)
            '    'Dim val22 As Single = (b / 255) ^ (1 / blueGamma)
            '    'Dim val31 As Single = (b1Gamma - val21) / bDiffGamma
            '    'Dim val33 As Single = (b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma
            '    'Dim val41 As Single = val31 * bDiff
            '    'Dim val42 As Single = ((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff
            '    'Dim val51 As Single = val41 + b1
            '    'Dim val61 As Byte = CByte(((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff + b1)
            '    'Dim val7 As Integer = val61

            '    'pixels(pos) = CByte(((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff + b1)
            'End If

            '    Next y

            'Next x
            '3色グラデーション用
            'ElseIf myForm3.RadioButtonShape3ColorGrada.Checked Then
            'For x = 0 To rw - 1
            '    If x <= colCenter Then
            '        For y = 0 To rh - 1

            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If rDiffGamma <> 0 Then

            '                Dim r As Integer = pixels(pos + 2)
            '                Dim val As Single = r / 255
            '                Dim val2 As Single = val ^ (1 / redGamma)
            '                'Dim val2 As Single = val ^ (1 / 2.2)
            '                Dim val3 As Single = (val2 - r2Gamma) / rDiffGamma
            '                Dim val4 As Single = val3 * rDiff
            '                Dim val5 As Single = val4 + r2
            '                'pixels(pos + 2) = CByte(val5)
            '                Dim val6 As Byte = GradationGamma(r, redGamma, r2Gamma, rDiffGamma, rDiff, r2) '四捨五入になっている

            '                'If val5 < 0 Then
            '                '    val5 = 0
            '                'ElseIf val5 > 255 Then
            '                '    val5 = 255
            '                'End If
            '                'pixels(pos + 2) = CByte(val5)
            '                pixels(pos + 2) = val6

            '                'If myForm3.RadioButtonShape2ColorGrada.Checked Then
            '                '    pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)
            '                'Else
            '                '    pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff2 + r2)
            '                'End If

            '                'pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)'これ
            '                ''pixels(pos + 2) = CByte((((r / 255) ^ (1 / myGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)

            '            End If

            '            If gDiffGamma <> 0 Then

            '                Dim gr As Integer = pixels(pos + 1)
            '                Dim val As Single = ((((gr / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)

            '                If val < 0 Then
            '                    val = 0
            '                ElseIf val > 255 Then
            '                    val = 255
            '                End If
            '                pixels(pos + 1) = CByte(val)
            '                'pixels(pos + 1) = CByte((((gr / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)

            '            End If

            '            If bDiffGamma <> 0 Then
            '                Dim b As Integer = pixels(pos)
            '                Dim val As Single = b / 255 '今の色の位置
            '                Dim val2 As Single = val ^ (1 / blueGamma) '今の色のガンマ補正の位置
            '                Dim val3 As Single = (val2 - b2Gamma) / bDiffGamma '

            '                Dim val4 As Single = val3 * bDiff
            '                Dim val5 As Single = val4 + b2
            '                If val5 < 0 Then
            '                    val5 = 0
            '                ElseIf val5 > 255 Then
            '                    val5 = 255
            '                End If
            '                pixels(pos) = CByte(val5)

            '                'pixels(pos) = CByte((((b / 255) ^ (1 / blueGamma) - b2Gamma) / bDiffGamma) * bDiff + b2)

            '                'Dim val11 As Single = b / 255
            '                'Dim val21 As Single = val11 ^ (1 / blueGamma) '(b/255)^(1/bluegamma)
            '                'Dim val22 As Single = (b / 255) ^ (1 / blueGamma)
            '                'Dim val31 As Single = (b1Gamma - val21) / bDiffGamma
            '                'Dim val33 As Single = (b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma
            '                'Dim val41 As Single = val31 * bDiff
            '                'Dim val42 As Single = ((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff
            '                'Dim val51 As Single = val41 + b1
            '                'Dim val61 As Byte = CByte(((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff + b1)
            '                'Dim val7 As Integer = val61

            '                'pixels(pos) = CByte(((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff + b1)
            '            End If

            '        Next y
            '    ElseIf x > colCenter Then
            '        For y = 0 To rh - 1

            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If rDiffGamma2 <> 0 Then

            '                Dim r As Integer = pixels(pos + 2)
            '                Dim val As Single = r / 255
            '                Dim val2 As Single = val ^ (1 / redGamma)
            '                'Dim val2 As Single = val ^ (1 / 2.2)
            '                Dim val3 As Single = (val2 - r3Gamma) / rDiffGamma2
            '                Dim val4 As Single = val3 * rDiff2
            '                Dim val5 As Single = val4 + r3
            '                pixels(pos + 2) = CByte(val5)
            '                'Dim val6 As Byte = GradationGamma(r, redGamma, r2Gamma, rDiffGamma, rDiff, r2) '四捨五入になっている
            '                'pixels(pos + 2) = val6

            '                'pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2) 'これ

            '            End If

            '            If gDiffGamma2 <> 0 Then

            '                Dim gr As Integer = pixels(pos + 1)
            '                Dim val As Single = ((((gr / 255) ^ (1 / greenGamma) - g3Gamma) / gDiffGamma2) * gDiff2 + g3)

            '                pixels(pos + 1) = CByte(val)

            '            End If

            '            If bDiffGamma2 <> 0 Then
            '                Dim b As Integer = pixels(pos)
            '                'Dim val As Single = b / 255 '今の色の位置
            '                'Dim val2 As Single = val ^ (1 / blueGamma) '今の色のガンマ補正の位置
            '                'Dim val3 As Single = (val2 - b2Gamma) / bDiffGamma '

            '                'Dim val4 As Single = val3 * bDiff
            '                'Dim val5 As Single = val4 + b2

            '                pixels(pos) = CByte((((b / 255) ^ (1 / blueGamma) - b3Gamma) / bDiffGamma2) * bDiff2 + b3)

            '            End If

            '        Next y
            '    End If
            'Next x
            'End If

            'End If



            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            bmp.UnlockBits(bmpdata)

            'Dim ia As New ImageAttributes()
            'ia.SetGamma(1 / 2.2)
            'g = Graphics.FromImage(bmp)

            'g.DrawImage(bmp, rect, 0, 0, bmp.Width, bmp.Height, GraphicsUnit.Pixel, ia)



            ''以前のものLinearGradientBrushを使ったもの
            'Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            'Dim gCol2 As Color = Color.FromArgb(transparent2, myForm3.ButtonSquareColor2.ForeColor)
            'Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)

            ''グラデーションブラシ設定
            'If myForm3.RadioButtonSquareGradaH.Checked Then
            '    'gradationB = New LinearGradientBrush(rectF, gCol1, gCol2, LinearGradientMode.Horizontal)
            '    gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.Horizontal)
            '    'gradationB = New LinearGradientBrush(rect, gCol1, gCol2, 20, True)
            '    'gradationB = New LinearGradientBrush(g.VisibleClipBounds, gCol1, gCol2, 10, True)
            'ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
            '    gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)

            'ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
            '    gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)

            'ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
            '    gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)

            'End If
            ''ガンマ補正ありなら
            'If myForm3.CheckBoxRectGradationGamma.Checked Then
            '    gradationB.GammaCorrection = True
            'Else
            '    gradationB.GammaCorrection = False
            'End If
            ''グラデーション塗り

            'Dim blend1 As New Blend()
            ''blend1.Factorsはcolor1の強さみたいなもので
            ''blend1.Positionsはその場所指定みたいなもの
            ''2つはSingleの配列で要素数を同じにしないとエラーになる、初期値はどちらも1.0
            ''0.0から1.0の間で指定する

            ''blend1.Factors = New Single() {0.0F, 0.1F, 0.225F, 0.375F, 0.475F, 0.5F, 0.525F, 0.625F, 0.775F, 0.9F, 1.0F}
            ' ''blend1.Factors = New Single() {0.0F, 0.1F, 0.2F, 0.3F, 0.4F, 0.5F, 0.6F, 0.7F, 0.8F, 0.9F, 1.0F}
            ' ''blend1.Factors = New Single() {0.0F, 0.2F, 0.5F, 0.7F, 1.0F, 0.7F, 0.5F, 0.2F, 1.0F}
            ' '' Set the positions.
            ''blend1.Positions = New Single() {0.0F, 0.1F, 0.2F, 0.3F, 0.4F, 0.5F, 0.6F, 0.7F, 0.8F, 0.9F, 1.0F}

            ''gradationB.Blend = blend1


            ''gradationB.Blend = brend

            ''g.SmoothingMode = SmoothingMode.AntiAlias
            'g.FillRectangle(gradationB, rect)
            ''g.FillRectangle(gradationB, g.VisibleClipBounds)

            ''Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            ''Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            ''gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)


        Else
            'g.FillRectangle(bru, 0, 0, rw, rh)
            g.FillRectangle(bru, rect)

        End If

        '透明度
        'If transparent < 255 Then
        '    Dim x As Integer
        '    Dim y As Integer
        '    Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        '    Dim ptr As IntPtr = bmpdata.Scan0
        '    Dim data As Integer = bmpdata.Stride * bmp.Height - 1
        '    Dim pixels(data) As Byte
        '    System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
        '    'Dim transparent As Integer = myForm3.NumericUpDownSquareTransparent.Value

        '    For x = 0 To rw - 1
        '        For y = 0 To rh - 1
        '            Dim pos As Integer = y * bmpdata.Stride + x * 4 + 3
        '            pixels(pos) = CByte(transparent)
        '        Next
        '    Next
        '    System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        '    bmp.UnlockBits(bmpdata)
        'End If


        '回転



        Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        bmp = PicAngle(bmp, rAngle)

        'If rAngle <> 0 Then
        '    Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        '    Dim d As Double = rAngle / (180 / Math.PI)
        '    Dim xx As Single
        '    Dim yy As Single
        '    Dim x1 As Single = xx + bmp.Width * CSng(Math.Cos(d))
        '    Dim y1 As Single = yy + bmp.Width * CSng(Math.Sin(d))
        '    'Dim x2 As Single = xx + bmp.Height * CSng(Math.Sin(d))
        '    Dim x2 As Single = xx - bmp.Height * CSng(Math.Sin(d))
        '    Dim y2 As Single = yy + bmp.Height * CSng(Math.Cos(d))
        '    'テスト確認用
        '    Dim x3 As Single = CSng(Math.Sin(d)) '90度のとき1.0、45度のとき0.7
        '    Dim y3 As Single = CSng(Math.Cos(d)) '90度のとき6.123、45度のとき0.7


        '    If y1 <= 0 Then
        '        y1 = Math.Abs(y1)
        '    End If
        '    Dim destnationPoint As Point() '画像を描画する座標3点
        '    Dim canvas As Bitmap '空のBitmapで大きさを決めてこれに上で書いた文字を回転して描画する
        '    If rAngle < 0 Then '角度がマイナスの場合
        '        destnationPoint = {New Point(xx, y1), New Point(x1, yy), New Point(x2, y1 + y2)} '左上、右上、左下
        '        'destnationPoint = {New Point(xx, y1), New Point(x1 - x2, yy), New Point(x2, y1 + y2)} '左上、右上、左下
        '        'destnationPoint = {New Point(xx, y1 - y2), New Point(x2 - bmp.Height, yy), New Point(x2, y1)}
        '        'canvas = New Bitmap(CInt(x1 + Math.Abs(x2) * 2), CInt(Math.Abs(y1) + y2))
        '        canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))
        '    Else
        '        destnationPoint = {New Point(-x2, yy), New Point(x1 + Math.Abs(x2), y1), New Point(xx, y2)}
        '        canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))

        '        'canvas = New Bitmap(CInt(x1), CInt(Math.Abs(y1) + y2))
        '    End If


        '    Dim img As New Bitmap(bmp) '文字画像
        '    Dim g2 As Graphics = Graphics.FromImage(canvas)
        '    'g2.SmoothingMode = SmoothingMode.HighQuality 'アンチエイリアスは意味ないみたい

        '    g2.DrawImage(img, destnationPoint)

        '    Return canvas
        'End If


        'Dim bur As Brush = Brushes.AliceBlue
        'Dim name As String = "四角形" 'ピクチャーボックスの名前
        ''透明度が255より小さければ名前の末尾に_Tをつける
        'If transparent < 255 Then
        '    name = name & "_T"

        'End If
        Return bmp

        'Call PicBoxAdd(name, bmp)
        'Me.FocusPic.Image = bmp
    End Function
    Friend Function GradationGammaBitmapAdd(ByVal bmp As Bitmap, ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color) As Bitmap

        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
        Dim rh As Integer = bmp.Height
        Dim rw As Integer = bmp.Width

        Dim r1 As Integer = col.R
        Dim g1 As Integer = col.G
        Dim b1 As Integer = col.B
        Dim a1 As Integer = col.A
        Dim r2 As Integer = col2.R
        Dim g2 As Integer = col2.G
        Dim b2 As Integer = col2.B
        Dim a2 As Integer = col2.A
        Dim r3 As Integer = col3.R
        Dim g3 As Integer = col3.G
        Dim b3 As Integer = col3.B
        Dim a3 As Integer = col3.A

        'Dim rAdd As Single = (col2.R - col1.R) / (rw - 1)
        'Dim rAdd As Double = CInt(col2.R - col1.R) / (rw - 1)
        Dim rAdd As Single
        Dim gAdd As Single
        Dim bAdd As Single
        Dim aAdd As Single
        Dim rAdd2 As Single '3色グラデーション
        Dim gAdd2 As Single
        Dim bAdd2 As Single
        Dim aAdd2 As Single

        Dim rDiff As Integer = r1 - r2
        Dim gDiff As Integer = g1 - g2
        Dim bDiff As Integer = b1 - b2
        Dim rDiff2 As Integer = r2 - r3 '3色グラデーション
        Dim gDiff2 As Integer = g2 - g3
        Dim bDiff2 As Integer = b2 - b3

        '1ピクセルごとに変化する値
        If myForm3.RadioButtonSquareGradaH.Checked And rw > 1 Then

            If myForm3.RadioButtonShape2ColorGrada.Checked Then
                '2色グラデーション横
                rAdd = (r2 - r1) / (rw - 1)
                'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
                gAdd = (g2 - g1) / (rw - 1)
                bAdd = (b2 - b1) / (rw - 1)
                aAdd = (a2 - a1) / (rw - 1)
            Else '3色グラデーション横
                Dim val As Integer = Math.Floor((rw - 1) / 2) '切り捨て
                rAdd = (r2 - r1) / val
                'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
                gAdd = (g2 - g1) / val
                bAdd = (b2 - b1) / val
                aAdd = (a2 - a1) / val
                rAdd2 = (r3 - r2) / val
                gAdd2 = (g3 - g2) / val
                bAdd2 = (b3 - b2) / val
                aAdd2 = (a3 - a2) / val

            End If

        ElseIf myForm3.RadioButtonSquareGradaV.Checked And rh > 1 Then
            If myForm3.RadioButtonShape2ColorGrada.Checked Then
                rAdd = (r2 - r1) / (rh - 1)
                gAdd = (g2 - g1) / (rh - 1)
                bAdd = (b2 - b1) / (rh - 1)
                aAdd = (a2 - a1) / (rh - 1)
            Else
                Dim val As Integer = Math.Floor((rh - 1) / 2) '切り捨て
                rAdd = (r2 - r1) / val
                gAdd = (g2 - g1) / val
                bAdd = (b2 - b1) / val
                aAdd = (a2 - a1) / val
                rAdd2 = (r3 - r2) / val
                gAdd2 = (g3 - g2) / val
                bAdd2 = (b3 - b2) / val
                aAdd2 = (a3 - a2) / val

            End If

        ElseIf myForm3.RadioButtonSquareGradaRUp.Checked OrElse myForm3.RadioButtonSquareGradaLUp.Checked Then
            If rw + rh > 2 Then '右上
                If myForm3.RadioButtonShape2ColorGrada.Checked Then
                    rAdd = (r2 - r1) / (rw + rh - 2)
                    gAdd = (g2 - g1) / (rw + rh - 2)
                    bAdd = (b2 - b1) / (rw + rh - 2)
                    aAdd = (a2 - a1) / (rw + rh - 2)
                Else
                    Dim val As Integer = Math.Floor((rw + rh - 2) / 2) '切り捨て
                    rAdd = (r2 - r1) / val
                    gAdd = (g2 - g1) / val
                    bAdd = (b2 - b1) / val
                    aAdd = (a2 - a1) / val
                    rAdd2 = (r3 - r2) / val
                    gAdd2 = (g3 - g2) / val
                    bAdd2 = (b3 - b2) / val
                    aAdd2 = (a3 - a2) / val

                End If
            End If


            'ElseIf myForm3.RadioButtonSquareGradaLUp.Checked And rw + rh > 2 Then
            '    rAdd = (r2 - r1) / (rw + rh - 2)
            '    gAdd = (g2 - g1) / (rw + rh - 2)
            '    bAdd = (b2 - b1) / (rw + rh - 2)
            '    aAdd = (a2 - a1) / (rw + rh - 2)
        ElseIf myForm3.RadioButtonSquareGradaLR.Checked And rw > 2 Then '左右からのグラデーション
            rAdd = (r2 - r1) / Math.Floor((rw - 1) / 2) '切り捨て
            gAdd = (g2 - g1) / Math.Floor((rw - 1) / 2)
            bAdd = (b2 - b1) / Math.Floor((rw - 1) / 2)
            aAdd = (a2 - a1) / Math.Floor((rw - 1) / 2)
        ElseIf myForm3.RadioButtonSquareGradaUD.Checked And rh > 2 Then '上下からのグラデーション
            rAdd = (r2 - r1) / Math.Floor((rh - 1) / 2) '切り捨て
            gAdd = (g2 - g1) / Math.Floor((rh - 1) / 2)
            bAdd = (b2 - b1) / Math.Floor((rh - 1) / 2)
            aAdd = (a2 - a1) / Math.Floor((rh - 1) / 2)
            '斜めのグラデーション、2方向
        ElseIf myForm3.RadioButtonSquareGradaLU2.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
            If rw + rh > 3 Then
                rAdd = (r2 - r1) / Math.Floor((rw + rh - 2) / 2)
                gAdd = (g2 - g1) / Math.Floor((rw + rh - 2) / 2)
                bAdd = (b2 - b1) / Math.Floor((rw + rh - 2) / 2)
                aAdd = (a2 - a1) / Math.Floor((rw + rh - 2) / 2)

            End If
        Else
            rAdd = (r2 - r1)
            gAdd = (g2 - g1)
            bAdd = (b2 - b1)
            aAdd = (a2 - a1)

        End If

        'ガンマ補正用変数ここから---------------------------
        'Const myGamma As Single = 2.2
        Dim myGamma As Single = myForm3.TrackBarShapeGamma.Value / 10
        Dim redGamma As Single = myForm3.TrackBarShapeGammaR.Value / 10
        Dim greenGamma As Single = myForm3.TrackBarShapeGammaG.Value / 10
        Dim blueGamma As Single = myForm3.TrackBarShapeGammaB.Value / 10

        '色1と色2のガンマ補正の差分(変化総量)DiffGamma
        Dim r1Gamma As Single
        Dim r2Gamma As Single
        Dim r3Gamma As Single
        Dim rDiffGamma As Single
        Dim rDiffGamma2 As Single
        Dim g1Gamma As Single
        Dim g2Gamma As Single
        Dim g3Gamma As Single
        Dim gDiffGamma As Single
        Dim gDiffGamma2 As Single
        Dim b1Gamma As Single
        Dim b2Gamma As Single
        Dim b3Gamma As Single
        Dim bDiffGamma As Single
        Dim bDiffGamma2 As Single

        If myForm3.CheckBoxShapeGammaRGB.Checked Then

            r1Gamma = (r1 / 255) ^ (1 / redGamma)
            r2Gamma = (r2 / 255) ^ (1 / redGamma)
            r3Gamma = (r3 / 255) ^ (1 / redGamma)
            rDiffGamma = r1Gamma - r2Gamma
            rDiffGamma2 = r2Gamma - r3Gamma
            g1Gamma = (g1 / 255) ^ (1 / greenGamma)
            g2Gamma = (g2 / 255) ^ (1 / greenGamma)
            g3Gamma = (g3 / 255) ^ (1 / greenGamma)
            gDiffGamma = g1Gamma - g2Gamma
            gDiffGamma2 = g2Gamma - g3Gamma
            b1Gamma = (b1 / 255) ^ (1 / blueGamma)
            b2Gamma = (b2 / 255) ^ (1 / blueGamma)
            b3Gamma = (b3 / 255) ^ (1 / blueGamma)
            bDiffGamma = b1Gamma - b2Gamma
            bDiffGamma2 = b2Gamma - b3Gamma

        Else
            redGamma = myGamma
            greenGamma = myGamma
            blueGamma = myGamma

            r1Gamma = (r1 / 255) ^ (1 / myGamma)
            r2Gamma = (r2 / 255) ^ (1 / myGamma)
            r3Gamma = (r3 / 255) ^ (1 / myGamma)
            rDiffGamma = r1Gamma - r2Gamma
            rDiffGamma2 = r2Gamma - r3Gamma
            g1Gamma = (g1 / 255) ^ (1 / myGamma)
            g2Gamma = (g2 / 255) ^ (1 / myGamma)
            g3Gamma = (g3 / 255) ^ (1 / myGamma)
            gDiffGamma = g1Gamma - g2Gamma
            gDiffGamma2 = g2Gamma - g3Gamma
            b1Gamma = (b1 / 255) ^ (1 / myGamma)
            b2Gamma = (b2 / 255) ^ (1 / myGamma)
            b3Gamma = (b3 / 255) ^ (1 / myGamma)
            bDiffGamma = b1Gamma - b2Gamma
            bDiffGamma2 = b2Gamma - b3Gamma

        End If

        '3色グラデーション用
        Dim colCenter As Integer
        Dim colCenter2 As Integer
        If myForm3.RadioButtonShape3ColorGrada.Checked Then
            colCenter = Math.Floor((rw - 1) / 2)
            colCenter2 = Math.Round(colCenter, MidpointRounding.AwayFromZero)

        End If
        'ガンマ補正用変数ここまで------------------------------


        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * rh - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        If myForm3.RadioButtonSquareGradaH.Checked Then

            If myForm3.RadioButtonShape2ColorGrada.Checked Then
                '2色グラデーション横
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4

                        If pixels(pos + 3) <> 0 Then
                            pixels(pos) = CByte((bAdd * x) + b1)
                            pixels(pos + 1) = CByte((gAdd * x) + g1)
                            pixels(pos + 2) = CByte((rAdd * x) + r1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    'pixels(pos + 2) = CByte((((pixels(pos + 2) / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2) 'これ
                                End If

                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    'pixels(pos + 1) = CByte((((pixels(pos + 1) / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2) 'これ
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    'pixels(pos) = CByte((((pixels(pos) / 255) ^ (1 / blueGamma) - b2Gamma) / bDiffGamma) * bDiff + b2) 'これ
                                End If
                            End If

                        End If

                    Next
                Next

                '3色グラデーション横
            Else
                Dim val As Integer = Math.Floor((rw - 1) / 2) '中間地点
                'Dim val2 As Integer = val + 1 '中間地点の次の地点
                Dim val3 As Integer = Math.Round((rw - 1) / 2, MidpointRounding.AwayFromZero) '中間地点を四捨五入

                For x As Integer = 0 To rw - 1
                    If x <= val Then '左半分
                        For y = 0 To rh - 1
                            Dim pos As Integer = y * bmpdata.Stride + x * 4

                            If pixels(pos + 3) <> 0 Then
                                pixels(pos) = CByte((bAdd * x) + b1)
                                pixels(pos + 1) = CByte((gAdd * x) + g1)
                                pixels(pos + 2) = CByte((rAdd * x) + r1)
                                'pixels(pos + 3) = CByte((aAdd * x) + a1)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    End If
                                    If gDiffGamma <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    End If
                                    If bDiffGamma <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    End If
                                End If
                            End If


                        Next

                    ElseIf x > val Then '右半分

                        For y = 0 To rh - 1
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            If pixels(pos + 3) <> 0 Then
                                pixels(pos) = CByte((bAdd2 * (x - val3)) + b2)
                                pixels(pos + 1) = CByte((gAdd2 * (x - val3)) + g2)
                                pixels(pos + 2) = CByte((rAdd2 * (x - val3)) + r2)
                                'pixels(pos + 3) = CByte((aAdd2 * (x - val3)) + a2)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma2 <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                        Dim valr As Byte = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                    End If
                                    If gDiffGamma2 <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                        Dim valg As Byte = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                    End If
                                    If bDiffGamma2 <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                        Dim balb As Byte = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                    End If
                                End If
                            End If

                        Next

                    End If

                Next
            End If

        ElseIf myForm3.RadioButtonSquareGradaV.Checked Then

            If myForm3.RadioButtonShape2ColorGrada.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        If pixels(pos + 3) <> 0 Then
                            pixels(pos) = CByte((bAdd * y) + b1)
                            pixels(pos + 1) = CByte((gAdd * y) + g1)
                            pixels(pos + 2) = CByte((rAdd * y) + r1)
                            'pixels(pos + 3) = CByte((aAdd * y) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If
                        End If

                    Next
                Next

                '3色グラデーション
            Else
                Dim val As Integer = Math.Floor((rh - 1) / 2) '中間地点
                'Dim val2 As Integer = val + 1 '中間地点の次の地点
                Dim val3 As Integer = Math.Round((rh - 1) / 2, MidpointRounding.AwayFromZero) '中間地点を四捨五入

                For x As Integer = 0 To rw - 1
                    For y = 0 To rh - 1
                        If y <= val Then '上半分
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            If pixels(pos + 3) <> 0 Then
                                pixels(pos) = CByte((bAdd * y) + b1)
                                pixels(pos + 1) = CByte((gAdd * y) + g1)
                                pixels(pos + 2) = CByte((rAdd * y) + r1)
                                'pixels(pos + 3) = CByte((aAdd * y) + a1)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    End If
                                    If gDiffGamma <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    End If
                                    If bDiffGamma <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    End If
                                End If
                            End If

                        ElseIf y > val Then '下半分
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            If pixels(pos + 3) <> 0 Then
                                pixels(pos) = CByte((bAdd2 * (y - val3)) + b2)
                                pixels(pos + 1) = CByte((gAdd2 * (y - val3)) + g2)
                                pixels(pos + 2) = CByte((rAdd2 * (y - val3)) + r2)
                                'pixels(pos + 3) = CByte((aAdd2 * (y - val3)) + a2)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma2 <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                        Dim valr As Byte = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                    End If
                                    If gDiffGamma2 <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                        Dim valg As Byte = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                    End If
                                    If bDiffGamma2 <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                        Dim balb As Byte = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                    End If
                                End If
                            End If

                        End If

                    Next

                Next
            End If


        ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
            '2色グラデーション右上

            If myForm3.RadioButtonShape2ColorGrada.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        If pixels(pos + 3) <> 0 Then
                            pixels(pos) = CByte((bAdd * (rw - 1 - x + y)) + b1)
                            pixels(pos + 1) = CByte((gAdd * (rw - 1 - x + y)) + g1)
                            pixels(pos + 2) = CByte((rAdd * (rw - 1 - x + y)) + r1)
                            'pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                            'pixels(pos + 3) = CByte((aAdd * (rw - 1 - x + y)) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If
                        End If

                    Next
                Next
            Else
                '3色グラデーション右上
                Dim val As Integer = Math.Floor((rw + rh - 2) / 2) '中間地点
                'Dim val2 As Integer = val + 1 '中間地点の次の地点
                Dim val3 As Integer = Math.Round((rw + rh - 2) / 2, MidpointRounding.AwayFromZero) '中間地点を四捨五入
                For x As Integer = 0 To rw - 1
                    For y = 0 To rh - 1
                        If Math.Abs(x - (rw - 1)) + y <= val Then '左上半分
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            If pixels(pos + 3) <> 0 Then
                                pixels(pos) = CByte((bAdd * (Math.Abs(x - (rw - 1)) + y)) + b1)
                                pixels(pos + 1) = CByte((gAdd * (Math.Abs(x - (rw - 1)) + y)) + g1)
                                pixels(pos + 2) = CByte((rAdd * (Math.Abs(x - (rw - 1)) + y)) + r1)
                                'pixels(pos + 3) = CByte((aAdd * (Math.Abs(x - (rw - 1)) + y)) + a1)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    End If
                                    If gDiffGamma <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    End If
                                    If bDiffGamma <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    End If
                                End If
                            End If

                        ElseIf (Math.Abs(x - (rw - 1)) + y) > val Then '右下半分
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            If pixels(pos + 3) <> 0 Then
                                pixels(pos) = CByte((bAdd2 * ((Math.Abs(x - (rw - 1)) + y) - val3)) + b2)
                                pixels(pos + 1) = CByte((gAdd2 * ((Math.Abs(x - (rw - 1)) + y) - val3)) + g2)
                                pixels(pos + 2) = CByte((rAdd2 * ((Math.Abs(x - (rw - 1)) + y) - val3)) + r2)
                                'pixels(pos + 3) = CByte((aAdd2 * ((Math.Abs(x - (rw - 1)) + y) - val3)) + a2)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma2 <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                        Dim valr As Byte = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                    End If
                                    If gDiffGamma2 <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                        Dim valg As Byte = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                    End If
                                    If bDiffGamma2 <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                        Dim balb As Byte = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                    End If
                                End If
                            End If

                        End If

                    Next

                Next
            End If


        ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
            '2色グラデーション左上
            If myForm3.RadioButtonShape2ColorGrada.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        If pixels(pos + 3) <> 0 Then
                            pixels(pos) = CByte((bAdd * (x + y)) + b1)
                            pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
                            pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                            'pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)

                            If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                If rDiffGamma <> 0 Then
                                    pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                End If
                                If gDiffGamma <> 0 Then
                                    pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                End If
                                If bDiffGamma <> 0 Then
                                    pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                End If
                            End If
                        End If

                    Next
                Next
            Else
                '3色グラデーション左上
                Dim val As Integer = Math.Floor((rw + rh - 2) / 2) '中間地点
                'Dim val2 As Integer = val + 1 '中間地点の次の地点
                Dim val3 As Integer = Math.Round((rw + rh - 2) / 2, MidpointRounding.AwayFromZero) '中間地点を四捨五入
                For x As Integer = 0 To rw - 1
                    For y = 0 To rh - 1
                        If (x + y) <= val Then '左上半分
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            If pixels(pos + 3) <> 0 Then
                                pixels(pos) = CByte((bAdd * (x + y)) + b1)
                                pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
                                pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                                'pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                                    End If
                                    If gDiffGamma <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                                    End If
                                    If bDiffGamma <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                                    End If
                                End If
                            End If

                        ElseIf (x + y) > val Then '右下半分
                            Dim pos As Integer = y * bmpdata.Stride + x * 4
                            If pixels(pos + 3) <> 0 Then
                                pixels(pos) = CByte((bAdd2 * ((x + y) - val3)) + b2)
                                pixels(pos + 1) = CByte((gAdd2 * ((x + y) - val3)) + g2)
                                pixels(pos + 2) = CByte((rAdd2 * ((x + y) - val3)) + r2)
                                'pixels(pos + 3) = CByte((aAdd2 * ((x + y) - val3)) + a2)

                                If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                                    If rDiffGamma2 <> 0 Then
                                        pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                        Dim valr As Byte = GradationGamma(pixels(pos + 2), redGamma, r3Gamma, rDiffGamma2, rDiff2, r3)
                                    End If
                                    If gDiffGamma2 <> 0 Then
                                        pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                        Dim valg As Byte = GradationGamma(pixels(pos + 1), greenGamma, g3Gamma, gDiffGamma2, gDiff2, g3)
                                    End If
                                    If bDiffGamma2 <> 0 Then
                                        pixels(pos) = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                        Dim balb As Byte = GradationGamma(pixels(pos), blueGamma, b3Gamma, bDiffGamma2, bDiff2, b3)
                                    End If
                                End If
                            End If

                        End If

                    Next

                Next
            End If


        ElseIf myForm3.RadioButtonSquareGradaLR.Checked Then '左右からのグラデーション
            For x As Integer = 0 To Math.Floor((rw - 1) / 2) '小数点切り捨て
                For y = 0 To rh - 1
                    Dim pos As Integer = y * bmpdata.Stride + x * 4
                    Dim pos2 As Integer = y * bmpdata.Stride + (rw - 1 - x) * 4 '反対側のピクセル

                    pixels(pos) = CByte((bAdd * x) + b1)
                    pixels(pos2) = CByte((bAdd * x) + b1)
                    pixels(pos + 1) = CByte((gAdd * x) + g1)
                    pixels(pos2 + 1) = CByte((gAdd * x) + g1)
                    pixels(pos + 2) = CByte((rAdd * x) + r1)
                    pixels(pos2 + 2) = CByte((rAdd * x) + r1)

                    'If pixels(pos + 3) <> 0 And pixels(pos + 3) > col.A Then
                    '    ''pixels(pos + 3) = pixels(pos + 3)
                    '    ''pixels(pos2 + 3) = pixels(pos2 + 3)
                    '    'pixels(pos + 3) = CByte((aAdd * x) + a1)
                    '    'pixels(pos2 + 3) = CByte((aAdd * x) + a1)
                    'ElseIf pixels(pos + 3) <> 0 Then
                    '    'pixels(pos2 + 3) = pixels(pos + 3)

                    'End If



                    If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                        If rDiffGamma <> 0 Then
                            pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                            pixels(pos2 + 2) = GradationGamma(pixels(pos2 + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                        End If
                        If gDiffGamma <> 0 Then
                            pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                            pixels(pos2 + 1) = GradationGamma(pixels(pos2 + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                        End If
                        If bDiffGamma <> 0 Then
                            pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                            pixels(pos2) = GradationGamma(pixels(pos2), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                        End If
                    End If

                Next
            Next
        ElseIf myForm3.RadioButtonSquareGradaUD.Checked Then '上下からのグラデーション
            For x As Integer = 0 To rw - 1
                For y = 0 To Math.Floor((rh - 1) / 2) '小数点切り捨て
                    'Dim val As Integer = Math.Floor((rh - 1) / 2) '小数点切り捨て
                    Dim pos As Integer = y * bmpdata.Stride + x * 4
                    Dim pos2 As Integer = (rh - 1 - y) * bmpdata.Stride + x * 4 '反対側のピクセル

                    pixels(pos) = CByte((bAdd * y) + b1)
                    pixels(pos2) = CByte((bAdd * y) + b1)
                    pixels(pos + 1) = CByte((gAdd * y) + g1)
                    pixels(pos2 + 1) = CByte((gAdd * y) + g1)
                    pixels(pos + 2) = CByte((rAdd * y) + r1)
                    pixels(pos2 + 2) = CByte((rAdd * y) + r1)
                    'Dim val1 As Byte = CByte((rAdd * y) + r1)
                    'pixels(pos + 3) = CByte((aAdd * y) + a1)
                    'pixels(pos2 + 3) = CByte((aAdd * y) + a1)

                    If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                        If rDiffGamma <> 0 Then
                            pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                            pixels(pos2 + 2) = GradationGamma(pixels(pos2 + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                        End If
                        If gDiffGamma <> 0 Then
                            pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                            pixels(pos2 + 1) = GradationGamma(pixels(pos2 + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                        End If
                        If bDiffGamma <> 0 Then
                            pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                            pixels(pos2) = GradationGamma(pixels(pos2), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                        End If
                    End If
                Next
            Next
        ElseIf myForm3.RadioButtonSquareGradaLU2.Checked Then '斜めのグラデーション
            Dim range As Integer = rw - 1 + rh - 1 '総距離
            Dim hRange As Integer = Math.Floor(range / 2) '切り返しの中間地点、小数点切り捨て

            For x As Integer = 0 To rw - 1
                For y = 0 To rh - 1

                    If x + y <= hRange Then '前半
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        pixels(pos) = CByte((bAdd * (x + y)) + b1)
                        pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
                        pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                        'pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)

                        If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                            If rDiffGamma <> 0 Then
                                pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                            End If
                            If gDiffGamma <> 0 Then
                                pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                            End If
                            If bDiffGamma <> 0 Then
                                pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                            End If
                        End If
                    Else '後半
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        pixels(pos) = CByte((bAdd * (range - (x + y))) + b1)
                        pixels(pos + 1) = CByte((gAdd * (range - (x + y))) + g1)
                        pixels(pos + 2) = CByte((rAdd * (range - (x + y))) + r1)
                        'pixels(pos + 3) = CByte((aAdd * (range - (x + y))) + a1)

                        If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                            If rDiffGamma <> 0 Then
                                pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                            End If
                            If gDiffGamma <> 0 Then
                                pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                            End If
                            If bDiffGamma <> 0 Then
                                pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                            End If
                        End If
                    End If

                Next
            Next

        ElseIf myForm3.RadioButtonSquareGradaRU2.Checked Then '斜めのグラデーション右上
            Dim range As Integer = rw - 1 + rh - 1 '総距離
            Dim hRange As Integer = Math.Floor(range / 2) '切り返しの中間地点、小数点切り捨て

            For x As Integer = rw - 1 To 0 Step -1
                'For x As Integer = 0 To rw - 1
                For y = 0 To rh - 1

                    If Math.Abs(x - (rw - 1)) + y <= hRange Then '前半
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        pixels(pos) = CByte((bAdd * (Math.Abs(x - (rw - 1)) + y)) + b1)
                        pixels(pos + 1) = CByte((gAdd * (Math.Abs(x - (rw - 1)) + y)) + g1)
                        pixels(pos + 2) = CByte((rAdd * (Math.Abs(x - (rw - 1)) + y)) + r1)
                        'pixels(pos + 3) = CByte((aAdd * (Math.Abs(x - (rw - 1)) + y)) + a1)

                        If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                            If rDiffGamma <> 0 Then
                                pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                            End If
                            If gDiffGamma <> 0 Then
                                pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                            End If
                            If bDiffGamma <> 0 Then
                                pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                            End If
                        End If

                    Else '後半
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        pixels(pos) = CByte((bAdd * (range - (Math.Abs(x - (rw - 1)) + y))) + b1)
                        pixels(pos + 1) = CByte((gAdd * (range - (Math.Abs(x - (rw - 1)) + y))) + g1)
                        pixels(pos + 2) = CByte((rAdd * (range - (Math.Abs(x - (rw - 1)) + y))) + r1)
                        'pixels(pos + 3) = CByte((aAdd * (range - (Math.Abs(x - (rw - 1)) + y))) + a1)

                        If myForm3.CheckBoxRectGradationGamma.Checked Then 'ガンマ補正あり
                            If rDiffGamma <> 0 Then
                                pixels(pos + 2) = GradationGamma(pixels(pos + 2), redGamma, r2Gamma, rDiffGamma, rDiff, r2)
                            End If
                            If gDiffGamma <> 0 Then
                                pixels(pos + 1) = GradationGamma(pixels(pos + 1), greenGamma, g2Gamma, gDiffGamma, gDiff, g2)
                            End If
                            If bDiffGamma <> 0 Then
                                pixels(pos) = GradationGamma(pixels(pos), blueGamma, b2Gamma, bDiffGamma, bDiff, b2)
                            End If
                        End If

                    End If

                Next
            Next
        End If


        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)


        Return bmp

    End Function


    Friend Function GradationGamma(ByVal col As Integer, ByVal colGamma As Single, ByVal colGamma2 As Single, _
                                   ByVal diffG As Single, ByVal coldiff As Single, ByVal col2 As Integer) As Byte
        Dim val As Byte
        val = CByte((((((col / 255) ^ (1 / colGamma)) - colGamma2) / diffG) * coldiff) + col2)
        Return val

    End Function


    'RectangleAddのバックアップ

    Friend Function RectangleAddBackup(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal transparent As Integer, ByVal transparent2 As Integer) As Bitmap
        'Dim rw as Integer = myForm3.NumericUpDownRectWidth.Value '枠の幅
        'Dim rh As Integer = myForm3.NumericUpDownRectHeight.Value '枠の高さ

        Dim bmp As New Bitmap(rw, rh)
        Dim rect As New Rectangle(0, 0, rw, rh)
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh)
        'Dim rectF As New RectangleF(0, 0, rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)
        If myForm3.NumericUpDownRectangleAngle.Value = 0 Then
            g.SmoothingMode = SmoothingMode.None 'アンチエイリアス効果なし
        Else
            g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス効果あり
        End If


        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(Color.FromArgb(transparent, col))
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then
            Dim r1 As Integer = col.R
            Dim g1 As Integer = col.G
            Dim b1 As Integer = col.B
            Dim a1 As Integer = transparent
            Dim r2 As Integer = col2.R
            Dim g2 As Integer = col2.G
            Dim b2 As Integer = col2.B
            Dim a2 As Integer = transparent2
            'Dim rAdd As Single = (col2.R - col1.R) / (rw - 1)
            'Dim rAdd As Double = CInt(col2.R - col1.R) / (rw - 1)
            Dim rAdd As Single
            Dim gAdd As Single
            Dim bAdd As Single
            Dim aAdd As Single


            Dim rDiff As Integer = r1 - r2
            Dim gDiff As Integer = g1 - g2
            Dim bDiff As Integer = b1 - b2


            If myForm3.RadioButtonSquareGradaH.Checked AndAlso rw > 1 Then
                rAdd = (r2 - r1) / (rw - 1)
                'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
                gAdd = (g2 - g1) / (rw - 1)
                bAdd = (b2 - b1) / (rw - 1)
                aAdd = (a2 - a1) / (rw - 1)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked AndAlso rh > 1 Then
                rAdd = (r2 - r1) / (rh - 1)
                gAdd = (g2 - g1) / (rh - 1)
                bAdd = (b2 - b1) / (rh - 1)
                aAdd = (a2 - a1) / (rh - 1)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked AndAlso rw + rh > 2 Then
                rAdd = (r2 - r1) / (rw + rh - 2)
                gAdd = (g2 - g1) / (rw + rh - 2)
                bAdd = (b2 - b1) / (rw + rh - 2)
                aAdd = (a2 - a1) / (rw + rh - 2)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked AndAlso rw + rh > 2 Then
                rAdd = (r2 - r1) / (rw + rh - 2)
                gAdd = (g2 - g1) / (rw + rh - 2)
                bAdd = (b2 - b1) / (rw + rh - 2)
                aAdd = (a2 - a1) / (rw + rh - 2)
            Else
                rAdd = (r2 - r1)
                gAdd = (g2 - g1)
                bAdd = (b2 - b1)
                aAdd = (a2 - a1)

            End If

            Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdata.Scan0
            Dim data As Integer = bmpdata.Stride * rh - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            If myForm3.RadioButtonSquareGradaH.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4

                        pixels(pos) = CByte((bAdd * x) + b1)
                        pixels(pos + 1) = CByte((gAdd * x) + g1)
                        pixels(pos + 2) = CByte((rAdd * x) + r1)

                        'pixels(pos + 2) = CByte((rAdd * x ^ (1 / 2.2)) + r1)
                        'pixels(pos + 2) = CByte(rDiff * ((x / (rw - 1)) ^ (1 / 2.2)) + r1)

                        'pixels(pos + 2) = CByte(rDiff * ((x / (rw - 1)) ^ (2.2)) + r1)
                        'pixels(pos + 2) = CByte(rDiff * ((x / (rw - 1)) ^ (1.2)) + r1)
                        pixels(pos + 3) = CByte((aAdd * x) + a1)
                    Next
                Next
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4

                        pixels(pos) = CByte((bAdd * y) + b1)
                        pixels(pos + 1) = CByte((gAdd * y) + g1)
                        pixels(pos + 2) = CByte((rAdd * y) + r1)
                        pixels(pos + 3) = CByte((aAdd * y) + a1)
                    Next
                Next
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4

                        pixels(pos) = CByte((bAdd * (rw - 1 - x + y)) + b1)
                        pixels(pos + 1) = CByte((gAdd * (rw - 1 - x + y)) + g1)
                        pixels(pos + 2) = CByte((rAdd * (rw - 1 - x + y)) + r1)
                        'pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                        pixels(pos + 3) = CByte((aAdd * (rw - 1 - x + y)) + a1)
                    Next
                Next
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4

                        pixels(pos) = CByte((bAdd * (x + y)) + b1)
                        pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
                        pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                        pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)
                    Next
                Next
            End If

            'ガンマ補正
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                'Const myGamma As Single = 2.2
                Dim myGamma As Single = myForm3.TrackBarShapeGamma.Value / 10
                Dim redGamma As Single = myForm3.TrackBarShapeGammaR.Value / 10
                Dim greenGamma As Single = myForm3.TrackBarShapeGammaG.Value / 10
                Dim blueGamma As Single = myForm3.TrackBarShapeGammaB.Value / 10

                '色1と色2のガンマ補正の差分(変化総量)DiffGamma
                Dim r1Gamma As Single
                Dim r2Gamma As Single
                Dim rDiffGamma As Single
                Dim g1Gamma As Single
                Dim g2Gamma As Single
                Dim gDiffGamma As Single
                Dim b1Gamma As Single
                Dim b2Gamma As Single
                Dim bDiffGamma As Single
                If myForm3.CheckBoxShapeGammaRGB.Checked Then

                    r1Gamma = (r1 / 255) ^ (1 / redGamma)
                    r2Gamma = (r2 / 255) ^ (1 / redGamma)
                    rDiffGamma = r1Gamma - r2Gamma
                    g1Gamma = (g1 / 255) ^ (1 / greenGamma)
                    g2Gamma = (g2 / 255) ^ (1 / greenGamma)
                    gDiffGamma = g1Gamma - g2Gamma
                    b1Gamma = (b1 / 255) ^ (1 / blueGamma)
                    b2Gamma = (b2 / 255) ^ (1 / blueGamma)
                    bDiffGamma = b1Gamma - b2Gamma
                Else
                    redGamma = myGamma
                    greenGamma = myGamma
                    blueGamma = myGamma

                    r1Gamma = (r1 / 255) ^ (1 / myGamma)
                    r2Gamma = (r2 / 255) ^ (1 / myGamma)
                    rDiffGamma = r1Gamma - r2Gamma
                    g1Gamma = (g1 / 255) ^ (1 / myGamma)
                    g2Gamma = (g2 / 255) ^ (1 / myGamma)
                    gDiffGamma = g1Gamma - g2Gamma
                    b1Gamma = (b1 / 255) ^ (1 / myGamma)
                    b2Gamma = (b2 / 255) ^ (1 / myGamma)
                    bDiffGamma = b1Gamma - b2Gamma

                End If

                For x = 0 To rw - 1
                    For y = 0 To rh - 1

                        Dim pos As Integer = y * bmpdata.Stride + x * 4

                        If rDiffGamma <> 0 Then
                            Dim r As Integer = pixels(pos + 2)
                            Dim val As Single = r / 255
                            Dim val2 As Single = val ^ (1 / redGamma)
                            'Dim val2 As Single = val ^ (1 / 2.2)
                            Dim val3 As Single = (val2 - r2Gamma) / rDiffGamma
                            Dim val4 As Single = val3 * rDiff
                            Dim val5 As Single = val4 + r2
                            'pixels(pos + 2) = CByte(val5)

                            pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)
                            'pixels(pos + 2) = CByte((((r / 255) ^ (1 / myGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)
                        End If

                        If gDiffGamma <> 0 Then
                            Dim gr As Integer = pixels(pos + 1)
                            pixels(pos + 1) = CByte((((gr / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)

                        End If

                        If bDiffGamma <> 0 Then
                            Dim b As Integer = pixels(pos)
                            Dim val As Single = b / 255 '今の色の位置
                            Dim val2 As Single = val ^ (1 / blueGamma) '今の色のガンマ補正の位置
                            Dim val3 As Single = (val2 - b2Gamma) / bDiffGamma '

                            Dim val4 As Single = val3 * bDiff
                            Dim val5 As Single = val4 + b2
                            pixels(pos) = CByte((((b / 255) ^ (1 / blueGamma) - b2Gamma) / bDiffGamma) * bDiff + b2)

                            'Dim val11 As Single = b / 255
                            'Dim val21 As Single = val11 ^ (1 / blueGamma) '(b/255)^(1/bluegamma)
                            'Dim val22 As Single = (b / 255) ^ (1 / blueGamma)
                            'Dim val31 As Single = (b1Gamma - val21) / bDiffGamma
                            'Dim val33 As Single = (b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma
                            'Dim val41 As Single = val31 * bDiff
                            'Dim val42 As Single = ((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff
                            'Dim val51 As Single = val41 + b1
                            'Dim val61 As Byte = CByte(((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff + b1)
                            'Dim val7 As Integer = val61

                            'pixels(pos) = CByte(((b1Gamma - (b / 255) ^ (1 / blueGamma)) / -bDiffGamma) * bDiff + b1)
                        End If

                        'pixels(pos) = CByte((bAdd * x) + b1)
                        'pixels(pos + 1) = CByte((gAdd * x) + g1)
                        'pixels(pos) = CByte(val5)




                        'pixels(pos + 3) = CByte((aAdd * x) + a1)
                    Next
                Next

            End If
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            bmp.UnlockBits(bmpdata)

            'Dim ia As New ImageAttributes()
            'ia.SetGamma(1 / 2.2)
            'g = Graphics.FromImage(bmp)

            'g.DrawImage(bmp, rect, 0, 0, bmp.Width, bmp.Height, GraphicsUnit.Pixel, ia)



            ''以前のものLinearGradientBrushを使ったもの
            'Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            'Dim gCol2 As Color = Color.FromArgb(transparent2, myForm3.ButtonSquareColor2.ForeColor)
            'Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)

            ''グラデーションブラシ設定
            'If myForm3.RadioButtonSquareGradaH.Checked Then
            '    'gradationB = New LinearGradientBrush(rectF, gCol1, gCol2, LinearGradientMode.Horizontal)
            '    gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.Horizontal)
            '    'gradationB = New LinearGradientBrush(rect, gCol1, gCol2, 20, True)
            '    'gradationB = New LinearGradientBrush(g.VisibleClipBounds, gCol1, gCol2, 10, True)
            'ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
            '    gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)

            'ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
            '    gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)

            'ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
            '    gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)

            'End If
            ''ガンマ補正ありなら
            'If myForm3.CheckBoxRectGradationGamma.Checked Then
            '    gradationB.GammaCorrection = True
            'Else
            '    gradationB.GammaCorrection = False
            'End If
            ''グラデーション塗り

            'Dim blend1 As New Blend()
            ''blend1.Factorsはcolor1の強さみたいなもので
            ''blend1.Positionsはその場所指定みたいなもの
            ''2つはSingleの配列で要素数を同じにしないとエラーになる、初期値はどちらも1.0
            ''0.0から1.0の間で指定する

            ''blend1.Factors = New Single() {0.0F, 0.1F, 0.225F, 0.375F, 0.475F, 0.5F, 0.525F, 0.625F, 0.775F, 0.9F, 1.0F}
            ' ''blend1.Factors = New Single() {0.0F, 0.1F, 0.2F, 0.3F, 0.4F, 0.5F, 0.6F, 0.7F, 0.8F, 0.9F, 1.0F}
            ' ''blend1.Factors = New Single() {0.0F, 0.2F, 0.5F, 0.7F, 1.0F, 0.7F, 0.5F, 0.2F, 1.0F}
            ' '' Set the positions.
            ''blend1.Positions = New Single() {0.0F, 0.1F, 0.2F, 0.3F, 0.4F, 0.5F, 0.6F, 0.7F, 0.8F, 0.9F, 1.0F}

            ''gradationB.Blend = blend1


            ''gradationB.Blend = brend

            ''g.SmoothingMode = SmoothingMode.AntiAlias
            'g.FillRectangle(gradationB, rect)
            ''g.FillRectangle(gradationB, g.VisibleClipBounds)

            ''Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            ''Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            ''gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
        Else
            'g.FillRectangle(bru, 0, 0, rw, rh)
            g.FillRectangle(bru, rect)

        End If

        '透明度
        'If transparent < 255 Then
        '    Dim x As Integer
        '    Dim y As Integer
        '    Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        '    Dim ptr As IntPtr = bmpdata.Scan0
        '    Dim data As Integer = bmpdata.Stride * bmp.Height - 1
        '    Dim pixels(data) As Byte
        '    System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
        '    'Dim transparent As Integer = myForm3.NumericUpDownSquareTransparent.Value

        '    For x = 0 To rw - 1
        '        For y = 0 To rh - 1
        '            Dim pos As Integer = y * bmpdata.Stride + x * 4 + 3
        '            pixels(pos) = CByte(transparent)
        '        Next
        '    Next
        '    System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        '    bmp.UnlockBits(bmpdata)
        'End If


        '回転
        Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        bmp = PicAngle(bmp, rAngle)

        'If rAngle <> 0 Then
        '    Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        '    Dim d As Double = rAngle / (180 / Math.PI)
        '    Dim xx As Single
        '    Dim yy As Single
        '    Dim x1 As Single = xx + bmp.Width * CSng(Math.Cos(d))
        '    Dim y1 As Single = yy + bmp.Width * CSng(Math.Sin(d))
        '    'Dim x2 As Single = xx + bmp.Height * CSng(Math.Sin(d))
        '    Dim x2 As Single = xx - bmp.Height * CSng(Math.Sin(d))
        '    Dim y2 As Single = yy + bmp.Height * CSng(Math.Cos(d))
        '    'テスト確認用
        '    Dim x3 As Single = CSng(Math.Sin(d)) '90度のとき1.0、45度のとき0.7
        '    Dim y3 As Single = CSng(Math.Cos(d)) '90度のとき6.123、45度のとき0.7


        '    If y1 <= 0 Then
        '        y1 = Math.Abs(y1)
        '    End If
        '    Dim destnationPoint As Point() '画像を描画する座標3点
        '    Dim canvas As Bitmap '空のBitmapで大きさを決めてこれに上で書いた文字を回転して描画する
        '    If rAngle < 0 Then '角度がマイナスの場合
        '        destnationPoint = {New Point(xx, y1), New Point(x1, yy), New Point(x2, y1 + y2)} '左上、右上、左下
        '        'destnationPoint = {New Point(xx, y1), New Point(x1 - x2, yy), New Point(x2, y1 + y2)} '左上、右上、左下
        '        'destnationPoint = {New Point(xx, y1 - y2), New Point(x2 - bmp.Height, yy), New Point(x2, y1)}
        '        'canvas = New Bitmap(CInt(x1 + Math.Abs(x2) * 2), CInt(Math.Abs(y1) + y2))
        '        canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))
        '    Else
        '        destnationPoint = {New Point(-x2, yy), New Point(x1 + Math.Abs(x2), y1), New Point(xx, y2)}
        '        canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))

        '        'canvas = New Bitmap(CInt(x1), CInt(Math.Abs(y1) + y2))
        '    End If


        '    Dim img As New Bitmap(bmp) '文字画像
        '    Dim g2 As Graphics = Graphics.FromImage(canvas)
        '    'g2.SmoothingMode = SmoothingMode.HighQuality 'アンチエイリアスは意味ないみたい

        '    g2.DrawImage(img, destnationPoint)

        '    Return canvas
        'End If


        'Dim bur As Brush = Brushes.AliceBlue
        'Dim name As String = "四角形" 'ピクチャーボックスの名前
        ''透明度が255より小さければ名前の末尾に_Tをつける
        'If transparent < 255 Then
        '    name = name & "_T"

        'End If
        Return bmp

        'Call PicBoxAdd(name, bmp)
        'Me.FocusPic.Image = bmp
    End Function
    '四角形の画像を作る subの改変
    Friend Function RectangleAddHantaiColor(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal transparent As Integer, ByVal transparent2 As Integer) As Bitmap
        'Dim rw as Integer = myForm3.NumericUpDownRectWidth.Value '枠の幅
        'Dim rh As Integer = myForm3.NumericUpDownRectHeight.Value '枠の高さ

        Dim bmp As New Bitmap(rw, rh)
        Dim rect As New Rectangle(0, 0, rw, rh)
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh)
        Dim rect3 As New Rectangle(0, 0, rw, rh - 1)


        'Dim rectF As New RectangleF(0, 0, rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)

        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(Color.FromArgb(transparent, col))
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then ' AndAlso rw > 1 AndAlso rh > 1 Then


            '以前のものLinearGradientBrushを使ったもの
            Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            Dim gCol2 As Color = Color.FromArgb(transparent2, myForm3.ButtonSquareColor2.ForeColor)
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)

            'グラデーションブラシ設定
            If myForm3.RadioButtonSquareGradaH.Checked AndAlso rw > 1 Then
                'gradationB = New LinearGradientBrush(rectF, gCol1, gCol2, LinearGradientMode.Horizontal)
                gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.Horizontal)
                'gradationB = New LinearGradientBrush(rect, gCol1, gCol2, 20, True)
                'gradationB = New LinearGradientBrush(g.VisibleClipBounds, gCol1, gCol2, 10, True)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked AndAlso rh > 1 Then
                gradationB = New LinearGradientBrush(rect3, gCol1, gCol2, LinearGradientMode.Vertical)

            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked AndAlso rw > 1 AndAlso rh > 1 Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)

            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked AndAlso rw > 1 AndAlso rh > 1 Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)

            End If
            'ガンマ補正ありなら
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                gradationB.GammaCorrection = True
            Else
                gradationB.GammaCorrection = False
            End If
            'グラデーション塗り

            'Dim blend1 As New Blend()
            'blend1.Factorsはcolor1の強さみたいなもので
            'blend1.Positionsはその場所指定みたいなもの
            '2つはSingleの配列で要素数を同じにしないとエラーになる、初期値はどちらも1.0
            '0.0から1.0の間で指定する

            'blend1.Factors = New Single() {0.0F, 0.1F, 0.225F, 0.375F, 0.475F, 0.5F, 0.525F, 0.625F, 0.775F, 0.9F, 1.0F}
            ''blend1.Factors = New Single() {0.0F, 0.1F, 0.2F, 0.3F, 0.4F, 0.5F, 0.6F, 0.7F, 0.8F, 0.9F, 1.0F}
            ''blend1.Factors = New Single() {0.0F, 0.2F, 0.5F, 0.7F, 1.0F, 0.7F, 0.5F, 0.2F, 1.0F}
            '' Set the positions.
            'blend1.Positions = New Single() {0.0F, 0.1F, 0.2F, 0.3F, 0.4F, 0.5F, 0.6F, 0.7F, 0.8F, 0.9F, 1.0F}

            'gradationB.Blend = blend1


            'gradationB.Blend = brend

            'g.SmoothingMode = SmoothingMode.AntiAlias
            'g.PixelOffsetMode = PixelOffsetMode.Half
            'g.InterpolationMode = InterpolationMode.NearestNeighbor
            'gradationB.SetBlendTriangularShape(0.5)
            'gradationB.SetSigmaBellShape(0.5)
            'gradationB.ScaleTransform(0.5, 0.1)
            'gradationB.SetBlendTriangularShape(0.1)
            'gradationB.RotateTransform(30)
            'gradationB.TranslateTransform(2, 1)


            g.FillRectangle(gradationB, rect)
            'g.FillRectangle(gradationB, g.VisibleClipBounds)

            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
        Else
            'g.FillRectangle(bru, 0, 0, rw, rh)
            g.FillRectangle(bru, rect)

        End If



        '回転
        Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        bmp = PicAngle(bmp, rAngle)


        Return bmp

        'Call PicBoxAdd(name, bmp)
        'Me.FocusPic.Image = bmp
    End Function
    '四角形の画像を作る subの改変
    Friend Function RectangleAddGomakasi(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal transparent As Integer, ByVal transparent2 As Integer) As Bitmap


        Dim bmp As New Bitmap(rw, rh)
        Dim rect As New Rectangle(0, 0, rw, rh)
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh)
        'Dim rectF As New RectangleF(0, 0, rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)


        Dim bru As New SolidBrush(Color.FromArgb(transparent, col))
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked AndAlso rw > 1 AndAlso rh > 1 Then

            '以前のものLinearGradientBrushを使ったもの
            Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            Dim gCol2 As Color = Color.FromArgb(transparent2, myForm3.ButtonSquareColor2.ForeColor)
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)

            'グラデーションブラシ設定
            If myForm3.RadioButtonSquareGradaH.Checked Then
                'gradationB = New LinearGradientBrush(rectF, gCol1, gCol2, LinearGradientMode.Horizontal)
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
                'gradationB = New LinearGradientBrush(rect, gCol1, gCol2, 20, True)
                'gradationB = New LinearGradientBrush(g.VisibleClipBounds, gCol1, gCol2, 10, True)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)

            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)

            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)

            End If
            'ガンマ補正ありなら
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                gradationB.GammaCorrection = True
            Else
                gradationB.GammaCorrection = False
            End If
            'グラデーション塗り
            g.FillRectangle(gradationB, rect)

        Else
            g.FillRectangle(bru, rect)

        End If


        Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        bmp = PicAngle(bmp, rAngle)

        Return bmp

        'Call PicBoxAdd(name, bmp)
        'Me.FocusPic.Image = bmp
    End Function

    '四角形の画像を作る subの改変
    Friend Function RectangleAddBlend(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal transparent As Integer, ByVal transparent2 As Integer) As Bitmap
        'Dim rw as Integer = myForm3.NumericUpDownRectWidth.Value '枠の幅
        'Dim rh As Integer = myForm3.NumericUpDownRectHeight.Value '枠の高さ

        Dim bmp As New Bitmap(rw, rh)
        Dim rect As New Rectangle(0, 0, rw, rh)
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh)
        'Dim rectF As New RectangleF(0, 0, rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)

        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(Color.FromArgb(transparent, col))
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked AndAlso rw > 1 AndAlso rh > 1 Then

            '以前のものLinearGradientBrushを使ったもの
            Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            Dim gCol2 As Color = Color.FromArgb(transparent2, myForm3.ButtonSquareColor2.ForeColor)
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)

            'グラデーションブラシ設定
            If myForm3.RadioButtonSquareGradaH.Checked Then
                'gradationB = New LinearGradientBrush(rectF, gCol1, gCol2, LinearGradientMode.Horizontal)
                gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.Horizontal)
                'gradationB = New LinearGradientBrush(rect, gCol1, gCol2, 20, True)
                'gradationB = New LinearGradientBrush(g.VisibleClipBounds, gCol1, gCol2, 10, True)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)

            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)

            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)

            End If
            'ガンマ補正ありなら
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                gradationB.GammaCorrection = True
            Else
                gradationB.GammaCorrection = False
            End If
            'グラデーション塗り

            Dim blend1 As New Blend()
            'blend1.Factorsはcolor1の強さみたいなもので()
            'blend1.Positionsはその場所指定みたいなもの()
            '2つはSingleの配列で要素数を同じにしないとエラーになる、初期値はどちらも1.0
            '0.0から1.0の間で指定する

            blend1.Factors = New Single() {0.0F, 0.1F, 0.225F, 0.375F, 0.475F, 0.5F, 0.525F, 0.625F, 0.775F, 0.9F, 1.0F}
            'blend1.Factors = New Single() {0.0F, 0.1F, 0.2F, 0.3F, 0.4F, 0.5F, 0.6F, 0.7F, 0.8F, 0.9F, 1.0F}
            'blend1.Factors = New Single() {0.0F, 0.2F, 0.5F, 0.7F, 1.0F, 0.7F, 0.5F, 0.2F, 1.0F}

            blend1.Positions = New Single() {0.0F, 0.1F, 0.2F, 0.3F, 0.4F, 0.5F, 0.6F, 0.7F, 0.8F, 0.9F, 1.0F}

            gradationB.Blend = blend1



            'g.SmoothingMode = SmoothingMode.AntiAlias
            g.FillRectangle(gradationB, rect)
        Else
            'g.FillRectangle(bru, 0, 0, rw, rh)
            g.FillRectangle(bru, rect)

        End If




        '回転
        Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        bmp = PicAngle(bmp, rAngle)

        Return bmp

    End Function
    Friend Sub GradationAdd()
        Dim bmp As New Bitmap(1000, 1000, PixelFormat.Format32bppArgb)
        Dim g As Graphics = Graphics.FromImage(bmp)
        'Dim col1 As Color = Color.Red
        'Dim col2 As Color = Color.Blue
        Dim col1 As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim col2 As Color = myForm3.ButtonSquareColor2.ForeColor

        Dim x As Integer = 0
        Dim y As Integer = 0
        Dim rw As Integer = bmp.Width
        Dim rh As Integer = bmp.Height
        Dim rect As New Rectangle(0, 0, rw, rh)
        Dim col1R As Integer = col1.R
        Dim col1G As Integer = col1.G
        Dim col1B As Integer = col1.B
        Dim col2R As Integer = col2.R
        Dim col2G As Integer = col2.G
        Dim col2B As Integer = col2.B
        'Dim rAdd As Single = (col2.R - col1.R) / (rw - 1)
        'Dim rAdd As Double = CInt(col2.R - col1.R) / (rw - 1)
        Dim rAdd As Double = (col2R - col1R) / (rw - 1)
        'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
        Dim gAdd As Single = (col2.G - col1.G) / (rw - 1)
        Dim bAdd As Single = (col2.B - col1.B) / (rw - 1)



        Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * rh - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        For x = 0 To rw - 1
            For y = 0 To rh - 1
                Dim pos As Integer = y * bmpdata.Stride + x * 4

                pixels(pos) = CByte((bAdd * x) + col1.B)
                pixels(pos + 1) = CByte((gAdd * x) + col1.G)
                pixels(pos + 2) = CByte((rAdd * x) + col1.R)
                pixels(pos + 3) = CByte(255)
            Next
        Next
        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)


        Call PicBoxAdd("test", bmp)



    End Sub
    '四角形書式適用
    Friend Sub RectangleShift()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim name As String = Me.ActExPic.Name
        Dim shape As Integer = 0
        If name.EndsWith("四角形_T") Then
            shape = 1
        ElseIf name.EndsWith("楕円_T") Then
            shape = 2
        ElseIf name.EndsWith("三角_T") Then
            shape = 3
        ElseIf name.EndsWith("角丸四角_T") Then
            shape = 4
        ElseIf name.EndsWith("角丸枠_T") Then
            shape = 5
        ElseIf name.EndsWith("角丸枠2_T") Then
            shape = 6
        ElseIf name.EndsWith("楕円枠_T") Then
            shape = 7
        ElseIf name.EndsWith("楕円枠2_T") Then
            shape = 8
        ElseIf name.EndsWith("枠_T") Then
            shape = 9
        End If


        If myPicAr.Count = 0 OrElse shape = 0 Then
            Exit Sub
        End If

        Dim rw As Integer = myForm3.NumericUpDownRectWidth.Value '枠の幅
        Dim rh As Integer = myForm3.NumericUpDownRectHeight.Value '枠の高さ
        Dim transparent As Integer = myForm3.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = myForm3.NumericUpDownSquareTransparent2.Value
        Dim transparent3 As Integer = myForm3.NumericUpDownSquareTransparent3.Value
        Dim col As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
        Dim col2 As Color = Color.FromArgb(transparent2, myForm3.ButtonSquareColor2.ForeColor)
        Dim col3 As Color = Color.FromArgb(transparent3, myForm3.ButtonSquareColor3.ForeColor)
        Dim penW As Integer = myForm3.NumericUpDownPenWidth.Value
        Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        Dim diameter As Single = myForm3.TrackBarRoundRect.Value
        Dim gradation As Boolean

        If myForm3.CheckBoxSquareGradation.Checked Then
            gradation = True
        Else
            gradation = False
        End If

        Dim bmp As New Bitmap(1, 1)

        If shape = 1 Then
            bmp = New Bitmap(RectangleAdd(rw, rh, col, col2, col3))
        ElseIf shape = 2 Then
            bmp = New Bitmap(EllipseAdd4(rw, rh, col, col2, col3))
        ElseIf shape = 3 Then
            bmp = New Bitmap(TriangleAdd(rw, rh, col, col2, col3))
        ElseIf shape = 4 Then
            'Dim diameter As Single = myForm3.TrackBarRoundRect.Value
            bmp = New Bitmap(RoundRectAdd(rw, rh, diameter, col, col2, col3))
        ElseIf shape = 5 Then
            'Dim diameter As Single = myForm3.TrackBarRoundRect.Value
            bmp = New Bitmap(FrameRoundRectAdd(rw, rh, diameter, col, col2, col3, penW, rAngle, gradation))
        ElseIf shape = 6 Then
            bmp = New Bitmap(FrameRoundRectAdd2(rw, rh, diameter, col, col2, col3, penW, rAngle, gradation))
        ElseIf shape = 7 Then
            bmp = New Bitmap(EllipseFrameAdd(rw, rh, col, col2, col3, penW))
        ElseIf shape = 8 Then
            bmp = New Bitmap(EllipseFrameAdd2(rw, rh, col, col2, col3, penW))
        ElseIf shape = 9 Then

            bmp = New Bitmap(FlameAdd(rw, rh, penW, col, col2, col3))

        End If

        Dim i As Integer = ActExPic.Tag - 1
        DirectCast(myPicAr(i), ExPictureBox).Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp
        DirectCast(myPicArBackup(i), ExPictureBox).Image = bmp
        Call Transparent4()
        Me.CurrentPic.Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
        Call UpdateThumbnail()

    End Sub


    '楕円作成、RectangleAddの改変
    Friend Function EllipseAdd(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        '楕円は1ピクセル小さくしないとはみ出る→PixelOffsetMode.halfではみでなくなった
        'Dim rect2 As New Rectangle(0, 0, rw - 1, rh - 1)

        Dim rect As New Rectangle(0, 0, rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)
        g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス

        'g.PixelOffsetMode = PixelOffsetMode.Default
        g.PixelOffsetMode = PixelOffsetMode.Half
        'g.PixelOffsetMode = PixelOffsetMode.HighQuality
        'g.PixelOffsetMode = PixelOffsetMode.HighSpeed
        'g.PixelOffsetMode = PixelOffsetMode.None

        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(col)
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then

            Dim gCol1 As Color = col
            Dim gCol2 As Color = col2
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If
            'ガンマ補正ありなら
            'If myForm3.CheckBoxRectGradationGamma.Checked Then
            '    gradationB.GammaCorrection = True
            'Else
            '    gradationB.GammaCorrection = False
            'End If
            'グラデーション塗り、グラデーションは必要なけど塗りが必要、塗ったとろこに後からグラデーションかける
            g.FillEllipse(gradationB, rect)
            'g.FillEllipse(gradationB, 0, 0, rw, rh)

            'g.FillEllipse(gradationB, rect2)

            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
            gradationB.Dispose()




            'グラデーション
            Dim r1 As Integer = col.R
            Dim g1 As Integer = col.G
            Dim b1 As Integer = col.B
            Dim a1 As Integer = col.A
            Dim r2 As Integer = col2.R
            Dim g2 As Integer = col2.G
            Dim b2 As Integer = col2.B
            Dim a2 As Integer = col.A


            'Dim rAdd As Single = (col2.R - col1.R) / (rw - 1)
            'Dim rAdd As Double = CInt(col2.R - col1.R) / (rw - 1)
            Dim rAdd As Single
            Dim gAdd As Single
            Dim bAdd As Single
            Dim aAdd As Single


            Dim rDiff As Integer = r1 - r2
            Dim gDiff As Integer = g1 - g2
            Dim bDiff As Integer = b1 - b2

            '1ピクセルごとに変化する値
            If myForm3.RadioButtonSquareGradaH.Checked AndAlso rw > 1 Then
                rAdd = (r2 - r1) / (rw - 1)
                'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
                gAdd = (g2 - g1) / (rw - 1)
                bAdd = (b2 - b1) / (rw - 1)
                aAdd = (a2 - a1) / (rw - 1)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked AndAlso rh > 1 Then
                rAdd = (r2 - r1) / (rh - 1)
                gAdd = (g2 - g1) / (rh - 1)
                bAdd = (b2 - b1) / (rh - 1)
                aAdd = (a2 - a1) / (rh - 1)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked AndAlso rw + rh > 2 Then
                rAdd = (r2 - r1) / (rw + rh - 2)
                gAdd = (g2 - g1) / (rw + rh - 2)
                bAdd = (b2 - b1) / (rw + rh - 2)
                aAdd = (a2 - a1) / (rw + rh - 2)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked AndAlso rw + rh > 2 Then
                rAdd = (r2 - r1) / (rw + rh - 2)
                gAdd = (g2 - g1) / (rw + rh - 2)
                bAdd = (b2 - b1) / (rw + rh - 2)
                aAdd = (a2 - a1) / (rw + rh - 2)
            Else
                rAdd = (r2 - r1)
                gAdd = (g2 - g1)
                bAdd = (b2 - b1)
                aAdd = (a2 - a1)

            End If

            Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            Dim ptr As IntPtr = bmpdata.Scan0
            Dim data As Integer = bmpdata.Stride * rh - 1
            Dim pixels(data) As Byte
            System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            If myForm3.RadioButtonSquareGradaH.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1

                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        If pixels(pos + 3) <> 0 Then
                            pixels(pos) = CByte((bAdd * x) + b1)
                            pixels(pos + 1) = CByte((gAdd * x) + g1)
                            pixels(pos + 2) = CByte((rAdd * x) + r1)
                            'pixels(pos + 3) = CByte((aAdd * x) + a1)
                        End If
                    Next
                Next
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        If pixels(pos + 3) <> 0 Then
                            pixels(pos) = CByte((bAdd * y) + b1)
                            pixels(pos + 1) = CByte((gAdd * y) + g1)
                            pixels(pos + 2) = CByte((rAdd * y) + r1)
                            'pixels(pos + 3) = CByte((aAdd * y) + a1)
                        End If
                    Next
                Next
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        If pixels(pos + 3) <> 0 Then
                            pixels(pos) = CByte((bAdd * (rw - 1 - x + y)) + b1)
                            pixels(pos + 1) = CByte((gAdd * (rw - 1 - x + y)) + g1)
                            pixels(pos + 2) = CByte((rAdd * (rw - 1 - x + y)) + r1)
                            'pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                            'pixels(pos + 3) = CByte((aAdd * (rw - 1 - x + y)) + a1)
                        End If

                    Next
                Next
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                For x = 0 To rw - 1
                    For y = 0 To rh - 1
                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        If pixels(pos + 3) <> 0 Then
                            pixels(pos) = CByte((bAdd * (x + y)) + b1)
                            pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
                            pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
                            'pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)
                        End If

                    Next
                Next
            End If

            'ガンマ補正
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                'Const myGamma As Single = 2.2
                Dim myGamma As Single = myForm3.TrackBarShapeGamma.Value / 10
                Dim redGamma As Single = myForm3.TrackBarShapeGammaR.Value / 10
                Dim greenGamma As Single = myForm3.TrackBarShapeGammaG.Value / 10
                Dim blueGamma As Single = myForm3.TrackBarShapeGammaB.Value / 10

                '色1と色2のガンマ補正の差分(変化総量)DiffGamma
                Dim r1Gamma As Single
                Dim r2Gamma As Single
                Dim rDiffGamma As Single
                Dim g1Gamma As Single
                Dim g2Gamma As Single
                Dim gDiffGamma As Single
                Dim b1Gamma As Single
                Dim b2Gamma As Single
                Dim bDiffGamma As Single

                If myForm3.CheckBoxShapeGammaRGB.Checked Then

                    r1Gamma = (r1 / 255) ^ (1 / redGamma)
                    r2Gamma = (r2 / 255) ^ (1 / redGamma)
                    rDiffGamma = r1Gamma - r2Gamma
                    g1Gamma = (g1 / 255) ^ (1 / greenGamma)
                    g2Gamma = (g2 / 255) ^ (1 / greenGamma)
                    gDiffGamma = g1Gamma - g2Gamma
                    b1Gamma = (b1 / 255) ^ (1 / blueGamma)
                    b2Gamma = (b2 / 255) ^ (1 / blueGamma)
                    bDiffGamma = b1Gamma - b2Gamma
                Else
                    redGamma = myGamma
                    greenGamma = myGamma
                    blueGamma = myGamma

                    r1Gamma = (r1 / 255) ^ (1 / myGamma)
                    r2Gamma = (r2 / 255) ^ (1 / myGamma)
                    rDiffGamma = r1Gamma - r2Gamma
                    g1Gamma = (g1 / 255) ^ (1 / myGamma)
                    g2Gamma = (g2 / 255) ^ (1 / myGamma)
                    gDiffGamma = g1Gamma - g2Gamma
                    b1Gamma = (b1 / 255) ^ (1 / myGamma)
                    b2Gamma = (b2 / 255) ^ (1 / myGamma)
                    bDiffGamma = b1Gamma - b2Gamma

                End If

                'Dim r1Gamma As Single = (r1 / 255) ^ (1 / myGamma)
                'Dim r2Gamma As Single = (r2 / 255) ^ (1 / myGamma)
                'Dim rDiffGamma As Single = r1Gamma - r2Gamma
                'Dim g1Gamma As Single = (g1 / 255) ^ (1 / myGamma)
                'Dim g2Gamma As Single = (g2 / 255) ^ (1 / myGamma)
                'Dim gDiffGamma As Single = g1Gamma - g2Gamma
                'Dim b1Gamma As Single = (b1 / 255) ^ (1 / myGamma)
                'Dim b2Gamma As Single = (b2 / 255) ^ (1 / myGamma)
                'Dim bDiffGamma As Single = b1Gamma - b2Gamma


                For x = 0 To rw - 1
                    For y = 0 To rh - 1

                        Dim pos As Integer = y * bmpdata.Stride + x * 4
                        If pixels(pos + 3) <> 0 Then

                            If rDiffGamma <> 0 Then
                                Dim r As Integer = pixels(pos + 2)
                                pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)
                                'pixels(pos + 2) = CByte((((r / 255) ^ (1 / myGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)

                            End If

                            If gDiffGamma <> 0 Then
                                Dim gr As Integer = pixels(pos + 1)
                                pixels(pos + 1) = CByte((((gr / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)
                                'pixels(pos + 1) = CByte((((gr / 255) ^ (1 / myGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)

                            End If

                            If bDiffGamma <> 0 Then
                                Dim b As Integer = pixels(pos)
                                pixels(pos) = CByte((((b / 255) ^ (1 / blueGamma) - b2Gamma) / bDiffGamma) * bDiff + b2)
                                'pixels(pos) = CByte((((b / 255) ^ (1 / myGamma) - b2Gamma) / bDiffGamma) * bDiff + b2)

                            End If
                        End If
                    Next
                Next

            End If
            System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            bmp.UnlockBits(bmpdata)
        Else
            'g.FillEllipse(bru, rect2)
            g.FillEllipse(bru, rect)
            'g.FillRectangle(bur, 0, 0, rw, rh)
        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then
            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            '画像を回転する関数に渡す
            bmp = PicAngle(bmp, rAngle)
            'すぐ渡す
            Return bmp

            '以下は上の関数にした
            'Dim d As Double = rAngle / (180 / Math.PI)
            'Dim xx As Single
            'Dim yy As Single
            'Dim x1 As Single = xx + bmp.Width * CSng(Math.Cos(d))
            'Dim y1 As Single = yy + bmp.Width * CSng(Math.Sin(d))
            ''Dim x2 As Single = xx + bmp.Height * CSng(Math.Sin(d))
            'Dim x2 As Single = xx - bmp.Height * CSng(Math.Sin(d))
            'Dim y2 As Single = yy + bmp.Height * CSng(Math.Cos(d))


            'If y1 <= 0 Then
            '    y1 = Math.Abs(y1)
            'End If
            'Dim destnationPoint As Point() '画像を描画する座標3点
            'Dim canvas As Bitmap '空のBitmapで大きさを決めてこれに上で書いた文字を回転して描画する
            'If rAngle < 0 Then '角度がマイナスの場合
            '    destnationPoint = {New Point(xx, y1), New Point(x1, yy), New Point(x2, y1 + y2)} '左上、右上、左下

            '    canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))
            'Else
            '    destnationPoint = {New Point(-x2, yy), New Point(x1 + Math.Abs(x2), y1), New Point(xx, y2)}
            '    canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))

            'End If


            'Dim img As New Bitmap(bmp)
            'Dim g2 As Graphics = Graphics.FromImage(canvas)

            'g2.DrawImage(img, destnationPoint)
            'g.Dispose()
            'g2.Dispose()
            'bmp.Dispose()

            'Return canvas
        End If

        Return bmp

    End Function


    '楕円作成、EllipseAddの改変、グラデーションガンマ補正をメソッドにして外に出した
    Friend Function EllipseAdd4(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        '楕円は1ピクセル小さくしないとはみ出る→PixelOffsetMode.halfではみでなくなった
        'Dim rect2 As New Rectangle(0, 0, rw - 1, rh - 1)

        Dim rect As New Rectangle(0, 0, rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)
        g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス

        'g.PixelOffsetMode = PixelOffsetMode.Default
        g.PixelOffsetMode = PixelOffsetMode.Half
        'g.PixelOffsetMode = PixelOffsetMode.HighQuality
        'g.PixelOffsetMode = PixelOffsetMode.HighSpeed
        'g.PixelOffsetMode = PixelOffsetMode.None

        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(col)
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then

            Dim gCol1 As Color = col
            Dim gCol2 As Color = col2
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked OrElse myForm3.RadioButtonSquareGradaLR.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If
            'ガンマ補正ありなら
            'If myForm3.CheckBoxRectGradationGamma.Checked Then
            '    gradationB.GammaCorrection = True
            'Else
            '    gradationB.GammaCorrection = False
            'End If
            '2方向からのグラデーション
            If myForm3.RadioButtonSquareGradaLR.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                gradationB.SetBlendTriangularShape(0.5)

            End If


            'グラデーション塗り、グラデーションは必要なけど塗りが必要、塗ったとろこに後からグラデーションかける
            g.FillEllipse(gradationB, rect)
            'g.FillEllipse(gradationB, 0, 0, rw, rh)

            'g.FillEllipse(gradationB, rect2)

            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
            gradationB.Dispose()




            'グラデーション
            Dim r1 As Integer = col.R
            Dim g1 As Integer = col.G
            Dim b1 As Integer = col.B
            Dim a1 As Integer = col.A
            Dim r2 As Integer = col2.R
            Dim g2 As Integer = col2.G
            Dim b2 As Integer = col2.B
            Dim a2 As Integer = col.A


            'Dim rAdd As Single = (col2.R - col1.R) / (rw - 1)
            'Dim rAdd As Double = CInt(col2.R - col1.R) / (rw - 1)
            Dim rAdd As Single
            Dim gAdd As Single
            Dim bAdd As Single
            Dim aAdd As Single


            Dim rDiff As Integer = r1 - r2
            Dim gDiff As Integer = g1 - g2
            Dim bDiff As Integer = b1 - b2

            '1ピクセルごとに変化する値
            If myForm3.RadioButtonSquareGradaH.Checked AndAlso rw > 1 Then
                rAdd = (r2 - r1) / (rw - 1)
                'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
                gAdd = (g2 - g1) / (rw - 1)
                bAdd = (b2 - b1) / (rw - 1)
                aAdd = (a2 - a1) / (rw - 1)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked AndAlso rh > 1 Then
                rAdd = (r2 - r1) / (rh - 1)
                gAdd = (g2 - g1) / (rh - 1)
                bAdd = (b2 - b1) / (rh - 1)
                aAdd = (a2 - a1) / (rh - 1)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked AndAlso rw + rh > 2 Then
                rAdd = (r2 - r1) / (rw + rh - 2)
                gAdd = (g2 - g1) / (rw + rh - 2)
                bAdd = (b2 - b1) / (rw + rh - 2)
                aAdd = (a2 - a1) / (rw + rh - 2)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked AndAlso rw + rh > 2 Then
                rAdd = (r2 - r1) / (rw + rh - 2)
                gAdd = (g2 - g1) / (rw + rh - 2)
                bAdd = (b2 - b1) / (rw + rh - 2)
                aAdd = (a2 - a1) / (rw + rh - 2)
            Else
                rAdd = (r2 - r1)
                gAdd = (g2 - g1)
                bAdd = (b2 - b1)
                aAdd = (a2 - a1)

            End If

            bmp = GradationGammaBitmapAdd(bmp, col, col2, col3)

            '上をメソッドにして外に出した

            'Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            'Dim ptr As IntPtr = bmpdata.Scan0
            'Dim data As Integer = bmpdata.Stride * rh - 1
            'Dim pixels(data) As Byte
            'System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            'If myForm3.RadioButtonSquareGradaH.Checked Then
            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1

            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then
            '                pixels(pos) = CByte((bAdd * x) + b1)
            '                pixels(pos + 1) = CByte((gAdd * x) + g1)
            '                pixels(pos + 2) = CByte((rAdd * x) + r1)
            '                'pixels(pos + 3) = CByte((aAdd * x) + a1)
            '            End If
            '        Next
            '    Next
            'ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1
            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then
            '                pixels(pos) = CByte((bAdd * y) + b1)
            '                pixels(pos + 1) = CByte((gAdd * y) + g1)
            '                pixels(pos + 2) = CByte((rAdd * y) + r1)
            '                'pixels(pos + 3) = CByte((aAdd * y) + a1)
            '            End If
            '        Next
            '    Next
            'ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1
            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then
            '                pixels(pos) = CByte((bAdd * (rw - 1 - x + y)) + b1)
            '                pixels(pos + 1) = CByte((gAdd * (rw - 1 - x + y)) + g1)
            '                pixels(pos + 2) = CByte((rAdd * (rw - 1 - x + y)) + r1)
            '                'pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
            '                'pixels(pos + 3) = CByte((aAdd * (rw - 1 - x + y)) + a1)
            '            End If

            '        Next
            '    Next
            'ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1
            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then
            '                pixels(pos) = CByte((bAdd * (x + y)) + b1)
            '                pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
            '                pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
            '                'pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)
            '            End If

            '        Next
            '    Next
            'End If

            ''ガンマ補正
            'If myForm3.CheckBoxRectGradationGamma.Checked Then
            '    'Const myGamma As Single = 2.2
            '    Dim myGamma As Single = myForm3.TrackBarShapeGamma.Value / 10
            '    Dim redGamma As Single = myForm3.TrackBarShapeGammaR.Value / 10
            '    Dim greenGamma As Single = myForm3.TrackBarShapeGammaG.Value / 10
            '    Dim blueGamma As Single = myForm3.TrackBarShapeGammaB.Value / 10

            '    '色1と色2のガンマ補正の差分(変化総量)DiffGamma
            '    Dim r1Gamma As Single
            '    Dim r2Gamma As Single
            '    Dim rDiffGamma As Single
            '    Dim g1Gamma As Single
            '    Dim g2Gamma As Single
            '    Dim gDiffGamma As Single
            '    Dim b1Gamma As Single
            '    Dim b2Gamma As Single
            '    Dim bDiffGamma As Single

            '    If myForm3.CheckBoxShapeGammaRGB.Checked Then

            '        r1Gamma = (r1 / 255) ^ (1 / redGamma)
            '        r2Gamma = (r2 / 255) ^ (1 / redGamma)
            '        rDiffGamma = r1Gamma - r2Gamma
            '        g1Gamma = (g1 / 255) ^ (1 / greenGamma)
            '        g2Gamma = (g2 / 255) ^ (1 / greenGamma)
            '        gDiffGamma = g1Gamma - g2Gamma
            '        b1Gamma = (b1 / 255) ^ (1 / blueGamma)
            '        b2Gamma = (b2 / 255) ^ (1 / blueGamma)
            '        bDiffGamma = b1Gamma - b2Gamma
            '    Else
            '        redGamma = myGamma
            '        greenGamma = myGamma
            '        blueGamma = myGamma

            '        r1Gamma = (r1 / 255) ^ (1 / myGamma)
            '        r2Gamma = (r2 / 255) ^ (1 / myGamma)
            '        rDiffGamma = r1Gamma - r2Gamma
            '        g1Gamma = (g1 / 255) ^ (1 / myGamma)
            '        g2Gamma = (g2 / 255) ^ (1 / myGamma)
            '        gDiffGamma = g1Gamma - g2Gamma
            '        b1Gamma = (b1 / 255) ^ (1 / myGamma)
            '        b2Gamma = (b2 / 255) ^ (1 / myGamma)
            '        bDiffGamma = b1Gamma - b2Gamma

            '    End If

            '    'Dim r1Gamma As Single = (r1 / 255) ^ (1 / myGamma)
            '    'Dim r2Gamma As Single = (r2 / 255) ^ (1 / myGamma)
            '    'Dim rDiffGamma As Single = r1Gamma - r2Gamma
            '    'Dim g1Gamma As Single = (g1 / 255) ^ (1 / myGamma)
            '    'Dim g2Gamma As Single = (g2 / 255) ^ (1 / myGamma)
            '    'Dim gDiffGamma As Single = g1Gamma - g2Gamma
            '    'Dim b1Gamma As Single = (b1 / 255) ^ (1 / myGamma)
            '    'Dim b2Gamma As Single = (b2 / 255) ^ (1 / myGamma)
            '    'Dim bDiffGamma As Single = b1Gamma - b2Gamma


            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1

            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then

            '                If rDiffGamma <> 0 Then
            '                    Dim r As Integer = pixels(pos + 2)
            '                    pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)
            '                    'pixels(pos + 2) = CByte((((r / 255) ^ (1 / myGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)

            '                End If

            '                If gDiffGamma <> 0 Then
            '                    Dim gr As Integer = pixels(pos + 1)
            '                    pixels(pos + 1) = CByte((((gr / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)
            '                    'pixels(pos + 1) = CByte((((gr / 255) ^ (1 / myGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)

            '                End If

            '                If bDiffGamma <> 0 Then
            '                    Dim b As Integer = pixels(pos)
            '                    pixels(pos) = CByte((((b / 255) ^ (1 / blueGamma) - b2Gamma) / bDiffGamma) * bDiff + b2)
            '                    'pixels(pos) = CByte((((b / 255) ^ (1 / myGamma) - b2Gamma) / bDiffGamma) * bDiff + b2)

            '                End If
            '            End If
            '        Next
            '    Next

            'End If
            'System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            'bmp.UnlockBits(bmpdata)
        Else
            'g.FillEllipse(bru, rect2)
            g.FillEllipse(bru, rect)
            'g.FillRectangle(bur, 0, 0, rw, rh)
        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then
            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            '画像を回転する関数に渡す
            bmp = PicAngle(bmp, rAngle)
            'すぐ渡す
            Return bmp

            '以下は上の関数にした
            'Dim d As Double = rAngle / (180 / Math.PI)
            'Dim xx As Single
            'Dim yy As Single
            'Dim x1 As Single = xx + bmp.Width * CSng(Math.Cos(d))
            'Dim y1 As Single = yy + bmp.Width * CSng(Math.Sin(d))
            ''Dim x2 As Single = xx + bmp.Height * CSng(Math.Sin(d))
            'Dim x2 As Single = xx - bmp.Height * CSng(Math.Sin(d))
            'Dim y2 As Single = yy + bmp.Height * CSng(Math.Cos(d))


            'If y1 <= 0 Then
            '    y1 = Math.Abs(y1)
            'End If
            'Dim destnationPoint As Point() '画像を描画する座標3点
            'Dim canvas As Bitmap '空のBitmapで大きさを決めてこれに上で書いた文字を回転して描画する
            'If rAngle < 0 Then '角度がマイナスの場合
            '    destnationPoint = {New Point(xx, y1), New Point(x1, yy), New Point(x2, y1 + y2)} '左上、右上、左下

            '    canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))
            'Else
            '    destnationPoint = {New Point(-x2, yy), New Point(x1 + Math.Abs(x2), y1), New Point(xx, y2)}
            '    canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))

            'End If


            'Dim img As New Bitmap(bmp)
            'Dim g2 As Graphics = Graphics.FromImage(canvas)

            'g2.DrawImage(img, destnationPoint)
            'g.Dispose()
            'g2.Dispose()
            'bmp.Dispose()

            'Return canvas
        End If

        Return bmp

    End Function
    'EllipseAdd4の改変、楕円枠作成
    Friend Function EllipseFrameAdd(ByVal rw As Integer, ByVal rh As Integer, _
                              ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color, ByVal penW As Integer) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        '楕円は1ピクセル小さくしないとはみ出る→PixelOffsetMode.halfではみでなくなった
        Dim rect2 As New Rectangle(1, 1, rw - 2, rh - 2)

        Dim rect As New Rectangle(0, 0, rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim gp As New GraphicsPath
        gp.AddEllipse(rect2)

        g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス

        'g.PixelOffsetMode = PixelOffsetMode.Default
        g.PixelOffsetMode = PixelOffsetMode.Half
        'g.PixelOffsetMode = PixelOffsetMode.HighQuality
        'g.PixelOffsetMode = PixelOffsetMode.HighSpeed
        'g.PixelOffsetMode = PixelOffsetMode.None

        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(col)
        Dim myPen As New Pen(bru, penW)

        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then

            Dim gCol1 As Color = col
            Dim gCol2 As Color = col2
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked OrElse myForm3.RadioButtonSquareGradaLR.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If
            'ガンマ補正ありなら
            'If myForm3.CheckBoxRectGradationGamma.Checked Then
            '    gradationB.GammaCorrection = True
            'Else
            '    gradationB.GammaCorrection = False
            'End If
            '2方向からのグラデーション
            If myForm3.RadioButtonSquareGradaLR.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                gradationB.SetBlendTriangularShape(0.5)

            End If


            'グラデーション塗り、グラデーションは必要なけど塗りが必要、塗ったとろこに後からグラデーションかける
            myPen = New Pen(gradationB, penW)
            myPen.Alignment = PenAlignment.Inset '枠の幅が太い時は内側に太くする
            'myPen.Alignment = PenAlignment.Left
            'myPen.Alignment = PenAlignment.Outset
            'myPen.Alignment = PenAlignment.Right
            g.DrawPath(myPen, gp)

            'g.DrawEllipse(myPen, rect)
            'g.FillEllipse(gradationB, rect)

            bmp = GradationGammaBitmapAdd(bmp, col, col2, col3)

        Else
            myPen = New Pen(col, penW)
            myPen.Alignment = PenAlignment.Inset

            g.DrawPath(myPen, gp)
            'g.DrawEllipse(myPen, rect2)

        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then
            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            '画像を回転する関数に渡す
            bmp = PicAngle(bmp, rAngle)
            'すぐ渡す
            Return bmp

        End If

        Return bmp

    End Function
    'EllipseAdd4の改変、楕円枠作成
    Friend Function EllipseFrameAdd2(ByVal rw As Integer, ByVal rh As Integer, _
                              ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color, ByVal penW As Integer) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        '楕円は1ピクセル小さくしないとはみ出る→PixelOffsetMode.halfではみでなくなった
        '大小2つの楕円を描画してその差分を残して枠に見立てる
        'Dim rect2 As New Rectangle(1, 1, rw - 2, rh - 2)
        Dim rect3 As New RectangleF(penW / 2, penW / 2, rw - penW, rh - penW) '内側の小さい楕円用

        Dim rect As New Rectangle(0, 0, rw, rh) '外側の大きい楕円用

        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim gp As New GraphicsPath
        'gp.FillMode = FillMode.Alternate'指定した方がいい？

        gp.AddEllipse(rect) '大きい楕円
        gp.AddEllipse(rect3) '小さい楕円

        'gp.AddEllipse(1.0F, 1.0F, 126, 126)

        g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス

        'g.PixelOffsetMode = PixelOffsetMode.Default
        g.PixelOffsetMode = PixelOffsetMode.Half
        Dim bru As New SolidBrush(col)

        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then

            Dim gCol1 As Color = col
            Dim gCol2 As Color = col2
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked OrElse myForm3.RadioButtonSquareGradaLR.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If

            '2方向からのグラデーション
            If myForm3.RadioButtonSquareGradaLR.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                gradationB.SetBlendTriangularShape(0.5)

            End If


            'グラデーション塗り、グラデーションは必要なけど塗りが必要、塗ったとろこに後からグラデーションかける

            g.FillPath(gradationB, gp) '楕円描画

            bmp = GradationGammaBitmapAdd(bmp, col, col2, col3)
            gradationB.Dispose()

        Else

            g.FillPath(bru, gp)
            bru.Dispose()
        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then
            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            '画像を回転する関数に渡す
            bmp = PicAngle(bmp, rAngle)
            'すぐ渡す
            Return bmp

        End If

        Return bmp

    End Function
    '楕円作成、RectangleAddの改変、グラデーションブラシ反対側の色
    Friend Function EllipseAdd2(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal transparent As Integer) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh) '楕円は1ピクセル小さくしないとはみ出る
        Dim rect3 As New Rectangle(0, 0, rw, rh - 1) '楕円は1ピクセル小さくしないとはみ出る

        Dim rect As New Rectangle(0, 0, rw, rh) '楕円は1ピクセル小さくしないとはみ出るけどそのままだと2色グラデーションで反対側の色が出てしまう

        Dim g As Graphics = Graphics.FromImage(bmp)
        g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス

        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(col)
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then
            Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            Dim gCol2 As Color = Color.FromArgb(myForm3.NumericUpDownSquareTransparent2.Value, myForm3.ButtonSquareColor2.ForeColor)
            'Dim gCol2 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor2.ForeColor)
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked AndAlso rw > 1 Then
                gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked AndAlso rh > 1 Then
                gradationB = New LinearGradientBrush(rect3, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked AndAlso rw > 1 AndAlso rh > 1 Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked AndAlso rw > 1 AndAlso rh > 1 Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If
            'ガンマ補正ありなら
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                gradationB.GammaCorrection = True
            Else
                gradationB.GammaCorrection = False
            End If
            'グラデーション塗り
            g.FillEllipse(gradationB, rect2)

            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
            gradationB.Dispose()

        Else
            'g.DrawRectangle(Pens.AliceBlue, 0, 0, rw, rh)
            g.FillEllipse(bru, rect2)
            'g.FillRectangle(bur, 0, 0, rw, rh)
        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then
            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            '画像を回転する関数に渡す
            bmp = PicAngle(bmp, rAngle)
            'すぐ渡す
            Return bmp

        End If

        Return bmp

    End Function

    '楕円作成、RectangleAddの改変、グラデーションブラシごまかし
    Friend Function EllipseAdd3(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal transparent As Integer) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh - 1) '楕円は1ピクセル小さくしないとはみ出る

        Dim rect As New Rectangle(0, 0, rw, rh) '楕円は1ピクセル小さくしないとはみ出るけどそのままだと2色グラデーションで反対側の色が出てしまう

        Dim g As Graphics = Graphics.FromImage(bmp)
        g.SmoothingMode = SmoothingMode.AntiAlias 'アンチエイリアス

        'Dim rPen As New Pen(myForm3.ButtonFlameColor.ForeColor, 1) '枠の色と太さ指定
        'Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim bru As New SolidBrush(col)
        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked Then
            Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            Dim gCol2 As Color = Color.FromArgb(myForm3.NumericUpDownSquareTransparent2.Value, myForm3.ButtonSquareColor2.ForeColor)
            'Dim gCol2 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor2.ForeColor)
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If
            'ガンマ補正ありなら
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                gradationB.GammaCorrection = True
            Else
                gradationB.GammaCorrection = False
            End If
            'グラデーション塗り
            g.FillEllipse(gradationB, rect2)

            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
            gradationB.Dispose()

        Else
            'g.DrawRectangle(Pens.AliceBlue, 0, 0, rw, rh)
            g.FillEllipse(bru, rect2)
            'g.FillRectangle(bur, 0, 0, rw, rh)
        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then
            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            '画像を回転する関数に渡す
            bmp = PicAngle(bmp, rAngle)
            'すぐ渡す
            Return bmp

        End If

        Return bmp

    End Function

    '画像の回転
    Friend Function PicAngle(ByVal bmp As Bitmap, ByVal sAngle As Integer) As Bitmap
        'Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        If sAngle = 0 OrElse sAngle = 360 OrElse sAngle = -360 Then
            Return bmp

        End If

        If sAngle < -90 And sAngle >= -180 Then
            bmp.RotateFlip(RotateFlipType.Rotate270FlipNone)
            sAngle = sAngle + 90
        ElseIf sAngle < -180 And sAngle >= -270 Then
            bmp.RotateFlip(RotateFlipType.Rotate180FlipNone)
            sAngle = sAngle + 180
        ElseIf sAngle < -270 Then
            bmp.RotateFlip(RotateFlipType.Rotate90FlipNone)
            sAngle = sAngle + 270
        ElseIf sAngle > 90 And sAngle <= 180 Then
            bmp.RotateFlip(RotateFlipType.Rotate90FlipNone)
            sAngle -= 90
        ElseIf sAngle > 180 And sAngle <= 270 Then
            bmp.RotateFlip(RotateFlipType.Rotate180FlipNone)
            sAngle -= 180
        ElseIf sAngle > 270 Then
            bmp.RotateFlip(RotateFlipType.Rotate270FlipNone)
            sAngle -= 270
        End If
        'Dim width As Integer = bmp.Width
        'Dim height As Integer = bmp.Height



        Dim d As Double = sAngle / (180 / Math.PI)
        Dim xx As Single
        Dim yy As Single
        Dim x1 As Single = xx + bmp.Width * CSng(Math.Cos(d))
        Dim y1 As Single = yy + bmp.Width * CSng(Math.Sin(d))
        'Dim x2 As Single = xx + bmp.Height * CSng(Math.Sin(d))
        Dim x2 As Single = xx - bmp.Height * CSng(Math.Sin(d))
        Dim y2 As Single = yy + bmp.Height * CSng(Math.Cos(d))

        If y1 <= 0 Then
            y1 = Math.Abs(y1)
        End If
        Dim destnationPoint As Point() '画像を描画する座標3点
        Dim destnationPointF As PointF() '画像を描画する座標3点
        Dim canvas As Bitmap '空のBitmapで大きさを決めてこれに上で書いた文字を回転して描画する
        If sAngle < 0 Then '角度がマイナスの場合
            destnationPoint = {New Point(xx, y1), New Point(x1, yy), New Point(x2, y1 + y2)} '左上、右上、左下
            destnationPointF = {New PointF(xx, y1), New PointF(x1, yy), New PointF(x2, y1 + y2)} '左上、右上、左下
            'canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))
            '小数点切り上げ
            canvas = New Bitmap(CInt(Math.Ceiling(x1 + Math.Abs(x2))), CInt(Math.Ceiling(Math.Abs(y1) + y2)))
        Else
            destnationPoint = {New Point(-x2, yy), New Point(x1 + Math.Abs(x2), y1), New Point(xx, y2)}
            destnationPointF = {New PointF(xx, y1), New PointF(x1, yy), New PointF(x2, y1 + y2)} '左上、右上、左下
            canvas = New Bitmap(CInt(x1 + Math.Abs(x2)), CInt(Math.Abs(y1) + y2))

        End If


        Dim img As New Bitmap(bmp)
        Dim g2 As Graphics = Graphics.FromImage(canvas)
        'g2.PixelOffsetMode = PixelOffsetMode.Default
        'g2.PixelOffsetMode = PixelOffsetMode.Half
        'g2.PixelOffsetMode = PixelOffsetMode.HighQuality
        'g2.PixelOffsetMode = PixelOffsetMode.HighSpeed
        'g2.PixelOffsetMode = PixelOffsetMode.None
        'g2.InterpolationMode = InterpolationMode.Bilinear
        g2.InterpolationMode = InterpolationMode.HighQualityBicubic
        'g2.InterpolationMode = InterpolationMode.NearestNeighbor

        'g2.SmoothingMode = SmoothingMode.AntiAlias
        'g2.TransformPoints(CoordinateSpace.World, CoordinateSpace.World, destnationPoint)

        'g2.DrawImage(img, destnationPoint)
        g2.DrawImage(img, destnationPointF)

        'g2.RotateTransform(10.0F)
        'g2.DrawImage(img, 0, 0, canvas.Width, canvas.Height)
        'g2.DrawImage(img, 2, 0, bmp.Width, bmp.Height)


        'g.Dispose()
        g2.Dispose()
        bmp.Dispose()

        Return canvas
        'Return bmp

    End Function

    '楕円作成、RectangleAddの改変
    '三角作成
    Friend Function TriangleAdd(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        Dim rect As New Rectangle(0, 0, rw - 1, rh - 1) '楕円は1ピクセル小さくしないとはみ出る
        Dim rect2 As New Rectangle(0, 0, rw, rh)
        Dim ue As Single = (rw - 1) / 2
        Dim ue2 As Single = rw / 2
        Dim points() As PointF '3点
        points = {New PointF(ue, 0), New Point(0, rh - 1), New Point(rw - 1, rh - 1)}
        Dim points2 As PointF() = {New PointF(ue2, 0), New Point(0, rh), New Point(rw, rh)}
        Dim points3 As PointF() = {New PointF(ue, 0), New Point(0, rh), New Point(rw, rh)}
        Dim points4 As PointF() = {New PointF(ue, 0), New Point(0, rh - 1), New Point(rw - 1, rh - 1)}


        Dim g As Graphics = Graphics.FromImage(bmp)

        'アンチエイリアス
        g.SmoothingMode = SmoothingMode.AntiAlias

        'g.PixelOffsetMode = PixelOffsetMode.Default
        'g.PixelOffsetMode = PixelOffsetMode.Half
        'g.PixelOffsetMode = PixelOffsetMode.HighQuality
        'g.PixelOffsetMode = PixelOffsetMode.HighSpeed
        'g.PixelOffsetMode = PixelOffsetMode.None



        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked AndAlso rw > 1 AndAlso rh > 1 Then
            Dim gCol1 As Color = col
            'Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            Dim gCol2 As Color = col2
            'Dim gCol2 As Color = Color.FromArgb(myForm3.NumericUpDownSquareTransparent2.Value, myForm3.ButtonSquareColor2.ForeColor)
            'Dim gCol2 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor2.ForeColor)
            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked OrElse myForm3.RadioButtonSquareGradaLR.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If

            'ガンマ補正ありなら
            'If myForm3.CheckBoxRectGradationGamma.Checked Then
            '    gradationB.GammaCorrection = True
            'Else
            '    gradationB.GammaCorrection = False
            'End If

            '2方向からのグラデーション
            If myForm3.RadioButtonSquareGradaLR.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
                gradationB.SetBlendTriangularShape(0.5)

            End If

            'Dim val As Color() = gradationB.LinearColors

            'gradationB.MultiplyTransform(Matrix)


            'g.PixelOffsetMode = PixelOffsetMode.Half
            'g.InterpolationMode = InterpolationMode.Bicubic

            g.FillPolygon(gradationB, points)
            'g.FillPolygon(gradationB, points2)
            'g.FillPolygon(gradationB, points3)
            'g.FillPolygon(gradationB, points4)
            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
            gradationB.Dispose()




            'グラデーション
            Dim r1 As Integer = col.R
            Dim g1 As Integer = col.G
            Dim b1 As Integer = col.B
            Dim a1 As Integer = col.A
            Dim r2 As Integer = col2.R
            Dim g2 As Integer = col2.G
            Dim b2 As Integer = col2.B
            Dim a2 As Integer = col.A


            'Dim rAdd As Single = (col2.R - col1.R) / (rw - 1)
            'Dim rAdd As Double = CInt(col2.R - col1.R) / (rw - 1)
            Dim rAdd As Single
            Dim gAdd As Single
            Dim bAdd As Single
            Dim aAdd As Single


            Dim rDiff As Integer = r1 - r2
            Dim gDiff As Integer = g1 - g2
            Dim bDiff As Integer = b1 - b2

            '1ピクセルごとに変化する値
            If myForm3.RadioButtonSquareGradaH.Checked AndAlso rw > 1 Then
                rAdd = (r2 - r1) / (rw - 1)
                'Dim rAdd As Double = (CInt(col2.R) - CInt(col1.R)) / (rw - 1)
                gAdd = (g2 - g1) / (rw - 1)
                bAdd = (b2 - b1) / (rw - 1)
                aAdd = (a2 - a1) / (rw - 1)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked AndAlso rh > 1 Then
                rAdd = (r2 - r1) / (rh - 1)
                gAdd = (g2 - g1) / (rh - 1)
                bAdd = (b2 - b1) / (rh - 1)
                aAdd = (a2 - a1) / (rh - 1)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked AndAlso rw + rh > 2 Then
                rAdd = (r2 - r1) / (rw + rh - 2)
                gAdd = (g2 - g1) / (rw + rh - 2)
                bAdd = (b2 - b1) / (rw + rh - 2)
                aAdd = (a2 - a1) / (rw + rh - 2)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked AndAlso rw + rh > 2 Then
                rAdd = (r2 - r1) / (rw + rh - 2)
                gAdd = (g2 - g1) / (rw + rh - 2)
                bAdd = (b2 - b1) / (rw + rh - 2)
                aAdd = (a2 - a1) / (rw + rh - 2)
            Else
                rAdd = (r2 - r1)
                gAdd = (g2 - g1)
                bAdd = (b2 - b1)
                aAdd = (a2 - a1)

            End If

            bmp = GradationGammaBitmapAdd(bmp, col, col2, col3)


            'Dim bmpdata As BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, bmp.PixelFormat)
            'Dim ptr As IntPtr = bmpdata.Scan0
            'Dim data As Integer = bmpdata.Stride * rh - 1
            'Dim pixels(data) As Byte
            'System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

            'If myForm3.RadioButtonSquareGradaH.Checked Then
            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1

            '            Dim pos As Integer = y * bmpdata.Stride + x * 4

            '            If pixels(pos + 3) <> 0 Then
            '                pixels(pos) = CByte((bAdd * x) + b1)
            '                pixels(pos + 1) = CByte((gAdd * x) + g1)
            '                pixels(pos + 2) = CByte((rAdd * x) + r1)
            '                'pixels(pos + 3) = CByte((aAdd * x) + a1)
            '            End If
            '        Next
            '    Next
            'ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1
            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then
            '                pixels(pos) = CByte((bAdd * y) + b1)
            '                pixels(pos + 1) = CByte((gAdd * y) + g1)
            '                pixels(pos + 2) = CByte((rAdd * y) + r1)
            '                'pixels(pos + 3) = CByte((aAdd * y) + a1)
            '            End If
            '        Next
            '    Next
            'ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1
            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then
            '                pixels(pos) = CByte((bAdd * (rw - 1 - x + y)) + b1)
            '                pixels(pos + 1) = CByte((gAdd * (rw - 1 - x + y)) + g1)
            '                pixels(pos + 2) = CByte((rAdd * (rw - 1 - x + y)) + r1)
            '                'pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
            '                'pixels(pos + 3) = CByte((aAdd * (rw - 1 - x + y)) + a1)
            '            End If

            '        Next
            '    Next
            'ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1
            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then
            '                pixels(pos) = CByte((bAdd * (x + y)) + b1)
            '                pixels(pos + 1) = CByte((gAdd * (x + y)) + g1)
            '                pixels(pos + 2) = CByte((rAdd * (x + y)) + r1)
            '                'pixels(pos + 3) = CByte((aAdd * (x + y)) + a1)
            '            End If

            '        Next
            '    Next
            'End If

            ''ガンマ補正
            'If myForm3.CheckBoxRectGradationGamma.Checked Then
            '    'Const myGamma As Single = 2.2
            '    Dim myGamma As Single = myForm3.TrackBarShapeGamma.Value / 10
            '    Dim redGamma As Single = myForm3.TrackBarShapeGammaR.Value / 10
            '    Dim greenGamma As Single = myForm3.TrackBarShapeGammaG.Value / 10
            '    Dim blueGamma As Single = myForm3.TrackBarShapeGammaB.Value / 10

            '    '色1と色2のガンマ補正の差分(変化総量)DiffGamma
            '    Dim r1Gamma As Single
            '    Dim r2Gamma As Single
            '    Dim rDiffGamma As Single
            '    Dim g1Gamma As Single
            '    Dim g2Gamma As Single
            '    Dim gDiffGamma As Single
            '    Dim b1Gamma As Single
            '    Dim b2Gamma As Single
            '    Dim bDiffGamma As Single

            '    If myForm3.CheckBoxShapeGammaRGB.Checked Then

            '        r1Gamma = (r1 / 255) ^ (1 / redGamma)
            '        r2Gamma = (r2 / 255) ^ (1 / redGamma)
            '        rDiffGamma = r1Gamma - r2Gamma
            '        g1Gamma = (g1 / 255) ^ (1 / greenGamma)
            '        g2Gamma = (g2 / 255) ^ (1 / greenGamma)
            '        gDiffGamma = g1Gamma - g2Gamma
            '        b1Gamma = (b1 / 255) ^ (1 / blueGamma)
            '        b2Gamma = (b2 / 255) ^ (1 / blueGamma)
            '        bDiffGamma = b1Gamma - b2Gamma
            '    Else
            '        redGamma = myGamma
            '        greenGamma = myGamma
            '        blueGamma = myGamma

            '        r1Gamma = (r1 / 255) ^ (1 / myGamma)
            '        r2Gamma = (r2 / 255) ^ (1 / myGamma)
            '        rDiffGamma = r1Gamma - r2Gamma
            '        g1Gamma = (g1 / 255) ^ (1 / myGamma)
            '        g2Gamma = (g2 / 255) ^ (1 / myGamma)
            '        gDiffGamma = g1Gamma - g2Gamma
            '        b1Gamma = (b1 / 255) ^ (1 / myGamma)
            '        b2Gamma = (b2 / 255) ^ (1 / myGamma)
            '        bDiffGamma = b1Gamma - b2Gamma

            '    End If

            '    'Dim r1Gamma As Single = (r1 / 255) ^ (1 / myGamma)
            '    'Dim r2Gamma As Single = (r2 / 255) ^ (1 / myGamma)
            '    'Dim rDiffGamma As Single = r1Gamma - r2Gamma
            '    'Dim g1Gamma As Single = (g1 / 255) ^ (1 / myGamma)
            '    'Dim g2Gamma As Single = (g2 / 255) ^ (1 / myGamma)
            '    'Dim gDiffGamma As Single = g1Gamma - g2Gamma
            '    'Dim b1Gamma As Single = (b1 / 255) ^ (1 / myGamma)
            '    'Dim b2Gamma As Single = (b2 / 255) ^ (1 / myGamma)
            '    'Dim bDiffGamma As Single = b1Gamma - b2Gamma


            '    For x = 0 To rw - 1
            '        For y = 0 To rh - 1

            '            Dim pos As Integer = y * bmpdata.Stride + x * 4
            '            If pixels(pos + 3) <> 0 Then
            '                If rDiffGamma <> 0 Then
            '                    Dim r As Integer = pixels(pos + 2)
            '                    pixels(pos + 2) = CByte((((r / 255) ^ (1 / redGamma) - r2Gamma) / rDiffGamma) * rDiff + r2)

            '                End If

            '                If gDiffGamma <> 0 Then
            '                    Dim gr As Integer = pixels(pos + 1)
            '                    pixels(pos + 1) = CByte((((gr / 255) ^ (1 / greenGamma) - g2Gamma) / gDiffGamma) * gDiff + g2)

            '                End If

            '                If bDiffGamma <> 0 Then
            '                    Dim b As Integer = pixels(pos)
            '                    pixels(pos) = CByte((((b / 255) ^ (1 / blueGamma) - b2Gamma) / bDiffGamma) * bDiff + b2)

            '                End If
            '            End If

            '        Next
            '    Next

            'End If
            'System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
            'bmp.UnlockBits(bmpdata)


        Else '1色塗り
            Dim bru As New SolidBrush(col)
            g.FillPolygon(bru, points)
            bru.Dispose()

        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then

            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            bmp = PicAngle(bmp, rAngle)
            Return bmp

        End If

        Return bmp

    End Function

    '三角作成
    Friend Function TriangleAdd2(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal transparent As Integer) As Bitmap

        Dim bmp As New Bitmap(rw, rh)

        Dim rect As New Rectangle(0, 0, rw - 1, rh - 1) '楕円は1ピクセル小さくしないとはみ出る


        Dim ue As Single = (rw - 1) / 2
        Dim points() As PointF '3点
        points = {New PointF(ue, 0), New Point(0, rh - 1), New Point(rw - 1, rh - 1)}
        'Dim points2() As PointF = {New PointF(ue, 0), New Point(0, rh), New Point(rw, rh)}


        Dim g As Graphics = Graphics.FromImage(bmp)

        'アンチエイリアス
        g.SmoothingMode = SmoothingMode.AntiAlias
        'g.PixelOffsetMode = PixelOffsetMode.Half




        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked AndAlso rw > 1 AndAlso rh > 1 Then
            Dim gCol1 As Color = col
            'Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            Dim gCol2 As Color = col2

            Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked Then
                'gradationB = New LinearGradientBrush(New Point(0, 0), New Point(rw, 0), col, col2)
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If

            'ガンマ補正ありなら
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                gradationB.GammaCorrection = True
            Else
                gradationB.GammaCorrection = False
            End If

            g.FillPolygon(gradationB, points)
            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
            gradationB.Dispose()

        Else '1色塗り
            Dim bru As New SolidBrush(col)
            'g.DrawRectangle(Pens.AliceBlue, 0, 0, rw, rh)
            'g.FillEllipse(bru, rect)
            g.FillPolygon(bru, points)
            bru.Dispose()

            'g.FillRectangle(bur, 0, 0, rw, rh)
        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then
            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            bmp = PicAngle(bmp, rAngle)
            Return bmp

        End If

        Return bmp

    End Function

    '三角作成
    Friend Function TriangleAdd3(ByVal rw As Integer, ByVal rh As Integer, _
                                 ByVal col As Color, ByVal col2 As Color, ByVal transparent As Integer) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        Dim rect2 As New Rectangle(0, 0, rw, rh) 'グラデーションブラシ用
        Dim rect As New Rectangle(0, 0, rw - 1, rh - 1) '楕円は1ピクセル小さくしないとはみ出る
        'Dim rect3 As New Rectangle(0, 0, rw / 2, rh)
        Dim ue As Single = (rw - 1) / 2
        Dim points() As PointF '3点
        points = {New PointF(ue, 0), New Point(0, rh - 1), New Point(rw - 1, rh - 1)}

        Dim g As Graphics = Graphics.FromImage(bmp)

        'アンチエイリアス
        g.SmoothingMode = SmoothingMode.AntiAlias





        '2色グラデーション
        If myForm3.CheckBoxSquareGradation.Checked AndAlso rw > 1 AndAlso rh > 1 Then
            Dim gCol1 As Color = col
            'Dim gCol1 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor1.ForeColor)
            Dim gCol2 As Color = col2
            'Dim gCol2 As Color = Color.FromArgb(myForm3.NumericUpDownSquareTransparent2.Value, myForm3.ButtonSquareColor2.ForeColor)
            'Dim gCol2 As Color = Color.FromArgb(transparent, myForm3.ButtonSquareColor2.ForeColor)
            Dim gradationB As New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.Horizontal)
            'グラデーションブラシ設定、作成
            If myForm3.RadioButtonSquareGradaH.Checked Then
                gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.Horizontal)
                'gradationB = New LinearGradientBrush(, gCol1, gCol2, LinearGradientMode.Horizontal)
            ElseIf myForm3.RadioButtonSquareGradaV.Checked Then
                gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.Vertical)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaLUp.Checked Then
                gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            ElseIf myForm3.RadioButtonSquareGradaRUp.Checked Then
                gradationB = New LinearGradientBrush(rect2, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
                'g.FillEllipse(gradationB, rect)
            End If

            'ガンマ補正ありなら
            If myForm3.CheckBoxRectGradationGamma.Checked Then
                gradationB.GammaCorrection = True
            Else
                gradationB.GammaCorrection = False
            End If

            g.FillPolygon(gradationB, points)
            'Dim angle As Integer = myForm3.ComboBoxSquareGradation.SelectedItem
            'Dim gradationB As New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, angle)
            'gradationB = New LinearGradientBrush(rect, Color.AliceBlue, Color.Aqua, LinearGradientMode.Horizontal)
            gradationB.Dispose()

        Else '1色塗り
            Dim bru As New SolidBrush(col)
            'g.DrawRectangle(Pens.AliceBlue, 0, 0, rw, rh)
            'g.FillEllipse(bru, rect)
            g.FillPolygon(bru, points)
            bru.Dispose()

            'g.FillRectangle(bur, 0, 0, rw, rh)
        End If



        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then
            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            bmp = PicAngle(bmp, rAngle)
            Return bmp

        End If

        Return bmp

    End Function
    Friend Function RoundRectAdd(ByVal rw As Integer, ByVal rh As Integer, ByVal diameter As Single, _
                            ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color) As Bitmap

        Dim bmp As New Bitmap(rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim gp As New GraphicsPath
        'Dim myPen As New Pen(Brushes.AliceBlue, 2)

        diameter = (Math.Min(rw, rh) - 1) * (diameter / 100) '角丸の直径、最低値は2、最高値は縦横の小さい方の値
        'diameter = (Math.Min(rw, rh)) * (diameter / 100) '角丸の直径、最低値は2、最高値は縦横の小さい方の値

        Dim xOff As Single = rw - 1 - diameter
        Dim yOff As Single = rh - 1 - diameter

        'Dim dRect As New RectangleF(0, 0, diameter, diameter)

        gp.AddArc(0, 0, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        gp.AddArc(xOff, 0, diameter, diameter, 270, 90)
        gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
        gp.AddArc(0, yOff, diameter, diameter, 90, 90)
        gp.CloseFigure()
        'gp.FillMode = FillMode.Winding

        g.SmoothingMode = SmoothingMode.AntiAlias
        'g.PixelOffsetMode = PixelOffsetMode.Half


        If myForm3.CheckBoxSquareGradation.Checked = False Then
            'グラデーションなし
            g.FillPath(New SolidBrush(col), gp)
        Else
            'グラデーションあり
            Dim gBrush As LinearGradientBrush = GradationBrushAdd(rw, rh, col, col2)
            If rw = 1 OrElse rh = 1 Then
                g.FillRectangle(gBrush, 0, 0, rw, rh)
                Return bmp

            End If

            g.FillPath(gBrush, gp)
            'g.DrawPath(Pens.AliceBlue, gp)’枠
            'g.DrawRectangle(New Pen(Brushes.AliceBlue, 2), rect4)
            bmp = GradationGammaBitmapAdd(bmp, col, col2, col3)
            gBrush.Dispose()

        End If
        g.Dispose()
        gp.Dispose()


        '回転
        If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then

            Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
            bmp = PicAngle(bmp, rAngle)
            Return bmp

        End If

        Return bmp

        'Call PicBoxAdd("角丸四角", bmp)

    End Function
    'Friend Function RoundRectAdd2(ByVal g As Graphics, ByVal gp As GraphicsPath, ByVal rect As Rectangle, _
    '                              ByVal brush As Brush, ByVal diameter As Single) As Graphics
    '    '角丸四角描画、文字の背景用
    '    'Dim gp As New GraphicsPath
    '    diameter = (Math.Min(rect.Width, rect.Height) * (diameter / 100))
    '    Dim xOff As Single = rect.Width - diameter
    '    Dim yOff As Single = rect.Height - diameter

    '    gp.AddArc(0, 0, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
    '    gp.AddArc(xOff, 0, diameter, diameter, 270, 90)
    '    gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
    '    gp.AddArc(0, yOff, diameter, diameter, 90, 90)
    '    gp.CloseFigure()

    '    g.SmoothingMode = SmoothingMode.AntiAlias
    '    g.FillPath(brush, gp)

    '    Return g
    'End Function
    '角丸四角用のグラフィックスパスを返す
    Friend Function RoundRectGPath(ByVal gp As GraphicsPath, ByVal rect As RectangleF, ByVal diameter As Single) As GraphicsPath
        diameter = (Math.Min(rect.Width, rect.Height) * (diameter / 100))
        Dim xOff As Single = rect.Width - diameter
        Dim yOff As Single = rect.Height - diameter

        gp.AddArc(0, 0, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        gp.AddArc(xOff, 0, diameter, diameter, 270, 90)
        gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
        gp.AddArc(0, yOff, diameter, diameter, 90, 90)
        gp.CloseFigure()

        Return gp
    End Function
    Friend Function RoundRectInSideGPath(gp As GraphicsPath, rect As Rectangle, diameter As Single, inSide As Single) As GraphicsPath
        '内側の角丸四角のパスを作成、文字背景の枠の内側を塗るときに使用
        diameter = (Math.Min(rect.Width, rect.Height)) * (diameter / 100)
        Dim diameter2 As Single = diameter - (inSide * 2)
        If diameter2 < 0 Then
            diameter2 = diameter
        End If

        Dim xOff2 As Single = rect.Width - (inSide + (diameter2))
        Dim yOff2 As Single = rect.Height - (inSide + (diameter2))
        gp.AddArc(inSide, inSide, diameter2, diameter2, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        gp.AddArc(xOff2, inSide, diameter2, diameter2, 270, 90)
        gp.AddArc(xOff2, yOff2, diameter2, diameter2, 0, 90)
        gp.AddArc(inSide, yOff2, diameter2, diameter2, 90, 90)
        gp.CloseFigure()

        Return gp
    End Function

    '角丸四角枠、角丸四角の改変
    Friend Function FrameRoundRectAdd(ByVal rw As Integer, ByVal rh As Integer, ByVal diameter As Single, _
                          ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color, ByVal penW As Integer, _
                          ByVal rAngle As Integer, ByVal gradation As Boolean) As Bitmap

        Dim bmp As New Bitmap(rw, rh)
        'Dim penW As Integer = myForm3.NumericUpDownPenWidth.Value '枠を書くペンの太さ
        Dim lineW As Single = (penW - 1) / 2 'ペンが太くなるとBitmapからはみ出るので補正用
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim gp As New GraphicsPath

        diameter = (Math.Min(rw, rh) - 1) * (diameter / 100) '角丸の直径、最低値は2、最高値は縦横の小さい方の値

        Dim xOff As Single = rw - 1 - diameter - lineW
        Dim yOff As Single = rh - 1 - diameter - lineW
        'Dim dRect As New RectangleF(0, 0, diameter, diameter)

        gp.AddArc(0 + lineW, 0 + lineW, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        gp.AddArc(xOff, 0 + lineW, diameter, diameter, 270, 90)
        gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
        gp.AddArc(0 + lineW, yOff, diameter, diameter, 90, 90)
        gp.CloseFigure()
        'gp.FillMode = FillMode.Winding

        g.SmoothingMode = SmoothingMode.AntiAlias
        'g.PixelOffsetMode = PixelOffsetMode.Half


        If gradation = False Then
            'If myForm3.CheckBoxSquareGradation.Checked = False Then
            'グラデーションなし
            'g.FillPath(New SolidBrush(col), gp)
            Dim gPen As New Pen(New SolidBrush(col), penW)
            'gPen.Alignment = PenAlignment.Inset'デフォルトはCenter
            'デフォルトはMiter
            'gPen.LineJoin = LineJoin.Miter
            'gPen.LineJoin = LineJoin.Bevel
            'gPen.LineJoin = LineJoin.MiterClipped
            gPen.LineJoin = LineJoin.Round
            Dim val = gPen.LineJoin

            g.DrawPath(gPen, gp)

        Else
            'グラデーションあり
            Dim gBrush As LinearGradientBrush = GradationBrushAdd(rw, rh, col, col2)
            Dim gPen As New Pen(gBrush, penW)
            gPen.LineJoin = LineJoin.Round

            If rw = 1 OrElse rh = 1 Then
                g.FillRectangle(gBrush, 0, 0, rw, rh)
                Return bmp

            End If

            'g.FillPath(gBrush, gp)
            'g.DrawPath(Pens.AliceBlue, gp)’枠
            g.DrawPath(gPen, gp)
            'g.DrawRectangle(New Pen(Brushes.AliceBlue, 2), rect4)
            bmp = GradationGammaBitmapAdd(bmp, col, col2, col3)
            gPen.Dispose()
            gBrush.Dispose()

        End If
        g.Dispose()
        gp.Dispose()


        '回転
        'If myForm3.NumericUpDownRectangleAngle.Value <> 0 Then

        '    Dim rAngle As Single = -myForm3.NumericUpDownRectangleAngle.Value
        '    bmp = PicAngle(bmp, rAngle)
        '    Return bmp

        'End If

        If rAngle <> 0 Then
            bmp = PicAngle(bmp, rAngle)

        End If

        Return bmp

        'Call PicBoxAdd("角丸四角", bmp)

    End Function

    Friend Function FrameRoundRectAdd2(ByVal rw As Integer, ByVal rh As Integer, ByVal diameter As Single, _
                          ByVal col As Color, ByVal col2 As Color, ByVal col3 As Color, ByVal penW As Integer, _
                          ByVal rAngle As Integer, ByVal gradation As Boolean) As Bitmap

        Dim bmp As New Bitmap(rw, rh)

        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim gp As New GraphicsPath
        'Dim penW As Integer = myForm3.NumericUpDownPenWidth.Value '枠を書くペンの太さ
        Dim lineW As Single = (penW)  'ペンが太くなるとBitmapからはみ出るので補正用

        If penW = 1 Then '枠の太さが1ならframeroundrectaddに丸投げ
            'bmp = FrameRoundRectAdd(rw, rh, diameter, col, col2, col3, penW, 0)
        Else
            diameter = (Math.Min(rw, rh) - 1) * (diameter / 100) '角丸の直径、最低値は2、最高値は縦横の小さい方の値

            Dim xOff As Single = rw - 1 - diameter
            Dim yOff As Single = rh - 1 - diameter
            Dim xOff2 As Single = rw - 1 - diameter - lineW
            Dim yOff2 As Single = rh - 1 - diameter - lineW
            'Dim dRect As New RectangleF(0, 0, diameter, diameter)

            gp.AddArc(0, 0, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
            gp.AddArc(xOff, 0, diameter, diameter, 270, 90)
            gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
            gp.AddArc(0, yOff, diameter, diameter, 90, 90)
            gp.CloseFigure()

            If penW > 1 Then
                gp.AddArc(0 + lineW, 0 + lineW, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
                gp.AddArc(xOff2, 0 + lineW, diameter, diameter, 270, 90)
                gp.AddArc(xOff2, yOff2, diameter, diameter, 0, 90)
                gp.AddArc(0 + lineW, yOff2, diameter, diameter, 90, 90)
                gp.CloseFigure()
            End If



            'gp.FillMode = FillMode.Winding
            g.SmoothingMode = SmoothingMode.AntiAlias
            'g.PixelOffsetMode = PixelOffsetMode.Half


            If gradation = False Then
                'If myForm3.CheckBoxSquareGradation.Checked = False Then
                'グラデーションなし
                g.FillPath(New SolidBrush(col), gp)
            Else
                'グラデーションあり
                Dim gBrush As LinearGradientBrush = GradationBrushAdd(rw, rh, col, col2)
                If rw = 1 OrElse rh = 1 Then
                    g.FillRectangle(gBrush, 0, 0, rw, rh)
                    Return bmp

                End If

                g.FillPath(gBrush, gp)
                'g.DrawPath(Pens.AliceBlue, gp) '枠
                'g.DrawRectangle(New Pen(Brushes.AliceBlue, 2), dRect)
                'g.FillRectangle(Brushes.AliceBlue, 0, 0, diameter, diameter)
                bmp = GradationGammaBitmapAdd(bmp, col, col2, col3)
                gBrush.Dispose()

            End If
            g.Dispose()
            gp.Dispose()

        End If

        '回転
        If rAngle <> 0 Then
            bmp = PicAngle(bmp, rAngle)
            Return bmp

        End If

        Return bmp
    End Function
    'Friend Function FrameRoundRectAdd3(ByVal g As Graphics, ByVal rect As Rectangle, ByVal bold As Single, _
    '                                   ByVal fBrush As Brush, ByVal diameter As Single) As Graphics
    '    Dim gp As New GraphicsPath
    '    diameter = (Math.Min(rect.Width, rect.Height)) * (diameter / 100) '角丸の直径、最低値は2、最高値は縦横の小さい方の値
    '    Dim xOff As Single = rect.Width - diameter
    '    Dim yOff As Single = rect.Height - diameter
    '    Dim xOff2 As Single = rect.Width - diameter - bold
    '    Dim yOff2 As Single = rect.Height - diameter - bold

    '    gp.AddArc(0, 0, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
    '    gp.AddArc(xOff, 0, diameter, diameter, 270, 90)
    '    gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
    '    gp.AddArc(0, yOff, diameter, diameter, 90, 90)
    '    gp.CloseFigure()


    '    gp.AddArc(0 + bold, 0 + bold, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
    '    gp.AddArc(xOff2, 0 + bold, diameter, diameter, 270, 90)
    '    gp.AddArc(xOff2, yOff2, diameter, diameter, 0, 90)
    '    gp.AddArc(0 + bold, yOff2, diameter, diameter, 90, 90)
    '    gp.CloseFigure()

    '    g.SmoothingMode = SmoothingMode.AntiAlias
    '    'g.PixelOffsetMode = PixelOffsetMode.Half
    '    g.FillPath(fBrush, gp)

    '    Return g
    '    gp.Dispose()
    'End Function


    '文字の枠と塗りつぶし用
    'グラフィックスパスを返す
    Friend Function FrameRoundRectGPath(ByVal gp As GraphicsPath, ByVal rect As Rectangle, ByVal bold As Single, _
                                   ByVal fBrush As Brush, ByVal diameter As Single) As GraphicsPath
        Dim rw As Integer = rect.Width
        Dim rh As Integer = rect.Height
        diameter = (Math.Min(rw, rh) - 1) * (diameter / 100) '角丸の直径、最低値は2、最高値は縦横の小さい方の値
        Dim xOff As Single = rw - diameter
        Dim yOff As Single = rh - diameter
        'Dim xOff2 As Single = rw - diameter - bold
        'Dim yOff2 As Single = rh - diameter - bold

        '外側
        gp.AddArc(0, 0, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        gp.AddArc(xOff, 0, diameter, diameter, 270, 90)
        gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
        gp.AddArc(0, yOff, diameter, diameter, 90, 90)
        gp.CloseFigure()

        '内側
        'Dim xOff2 As Single = rect.Width - diameter - bold
        'Dim yOff2 As Single = rect.Height - diameter - bold
        'gp.AddArc(0 + bold, 0 + bold, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        'gp.AddArc(xOff2, 0 + bold, diameter, diameter, 270, 90)
        'gp.AddArc(xOff2, yOff2, diameter, diameter, 0, 90)
        'gp.AddArc(0 + bold, yOff2, diameter, diameter, 90, 90)
        'gp.CloseFigure()

        '内側、↑よりきれいな四隅になる
        Dim diameter2 As Single = diameter - (bold * 2)
        If diameter2 < 0 Then
            diameter2 = diameter
        End If

        Dim xOff2 As Single = rw - (bold + (diameter2))
        Dim yOff2 As Single = rh - (bold + (diameter2))
        gp.AddArc(bold, bold, diameter2, diameter2, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        gp.AddArc(xOff2, bold, diameter2, diameter2, 270, 90)
        gp.AddArc(xOff2, yOff2, diameter2, diameter2, 0, 90)
        gp.AddArc(bold, yOff2, diameter2, diameter2, 90, 90)
        gp.CloseFigure()


        Return gp

    End Function

    '角丸四角枠のDrawpathのpen用
    '背景色と組み合わせるとうまくいかないので保留
    Friend Function FrameRoundRectGPath2(ByVal gp As GraphicsPath, ByVal rect As Rectangle, ByVal bold As Single, _
                                   ByVal fBrush As Brush, ByVal diameter As Single) As GraphicsPath

        diameter = (Math.Min(rect.Width, rect.Height)) * (diameter / 100) '角丸の直径、最低値は2、最高値は縦横の小さい方の値
        Dim LineW As Single = bold / 2
        Dim xOff As Single = rect.Width - diameter - LineW
        Dim yOff As Single = rect.Height - diameter - LineW

        gp.AddArc(0 + LineW, 0 + LineW, diameter, diameter, 180, 90) '追加する順番が重要、一筆書きにすれば直線は書かなくても塗りつぶせる
        gp.AddArc(xOff, 0 + LineW, diameter, diameter, 270, 90)
        gp.AddArc(xOff, yOff, diameter, diameter, 0, 90)
        gp.AddArc(0 + LineW, yOff, diameter, diameter, 90, 90)
        gp.CloseFigure()

        Return gp

    End Function


    Friend Function GradationBrushAdd(ByVal rw As Integer, ByVal rh As Integer, ByVal col As Color, ByVal col2 As Color) As LinearGradientBrush

        Dim rect As New Rectangle(0, 0, 1, 1)
        If rw = 1 Or rh = 1 Then
            rect = New Rectangle(0, 0, rw, rh) '-1しないほうがいいのかどうか
        Else
            rect = New Rectangle(0, 0, rw - 1, rh - 1) '-1しないほうがいいのかどうか
        End If

        'Dim rect As New Rectangle(0, 0, rw - 1, rh - 1) '-1しないほうがいいのかどうか
        'Dim rect As New Rectangle(0, 0, rw, rh) '-1しないほうがいいのかどうか
        Dim gCol1 As Color = col
        Dim gCol2 As Color = col2

        Dim gradationB As New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
        'グラデーションブラシ設定、作成
        If myForm3.RadioButtonSquareGradaH.Checked OrElse myForm3.RadioButtonSquareGradaLR.Checked Then
            gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Horizontal)
        ElseIf myForm3.RadioButtonSquareGradaV.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked Then
            gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.Vertical)
            'g.FillEllipse(gradationB, rect)
        ElseIf myForm3.RadioButtonSquareGradaLUp.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked Then
            gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.ForwardDiagonal)
            'g.FillEllipse(gradationB, rect)
        ElseIf myForm3.RadioButtonSquareGradaRUp.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
            gradationB = New LinearGradientBrush(rect, gCol1, gCol2, LinearGradientMode.BackwardDiagonal)
            'g.FillEllipse(gradationB, rect)
        End If

        ''ガンマ補正ありなら
        ''If myForm3.CheckBoxRectGradationGamma.Checked Then
        ''    gradationB.GammaCorrection = True
        ''Else
        ''    gradationB.GammaCorrection = False
        ''End If

        '2方向からのグラデーション
        If myForm3.RadioButtonSquareGradaLR.Checked OrElse myForm3.RadioButtonSquareGradaUD.Checked OrElse myForm3.RadioButtonSquareGradaLU2.Checked OrElse myForm3.RadioButtonSquareGradaRU2.Checked Then
            gradationB.SetBlendTriangularShape(0.5)

        End If
        Return gradationB

    End Function
    '四角形のサンプル見本
    Friend Sub SquareSample()
        'For Each f As Form In Me.OwnedForms
        '    If f.Name = "Form3" Then
        '        Dim rw As Integer = myForm3.PictureBoxSquareSample.Width '枠の幅
        '        Dim rh As Integer = myForm3.PictureBoxSquareSample.Height '枠の高さ
        '        Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        '        Dim transparent As Integer = myForm3.NumericUpDownSquareTransparent.Value
        '        Dim bmp = New Bitmap(RectangleAdd(rw, rh, col, transparent))
        '        myForm3.PictureBoxSquareSample.Image = bmp

        '    End If

        'Next
        Dim rw As Integer = myForm3.PictureBoxSquareSample.Width '枠の幅
        Dim rh As Integer = myForm3.PictureBoxSquareSample.Height '枠の高さ
        Dim col As Color = myForm3.ButtonSquareColor1.ForeColor
        Dim col2 As Color = myForm3.ButtonSquareColor2.ForeColor
        Dim col3 As Color = myForm3.ButtonSquareColor3.ForeColor
        Dim transparent As Integer = myForm3.NumericUpDownSquareTransparent.Value
        Dim transparent2 As Integer = myForm3.NumericUpDownSquareTransparent2.Value

        Dim bmp = New Bitmap(RectangleAdd(rw, rh, col, col2, col3))
        myForm3.PictureBoxSquareSample.Image = bmp


    End Sub

    '選択画像を30倍に拡大してピクセルごとのアルファ値を表示
    '0ならアルファ値、1が赤、2が緑、3が青、引数なしで9で表示なし
    Friend Sub ZoomPicAdd(Optional ByVal ARGB As Integer = 9)

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim fBmp As New Bitmap(DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image)

        Dim picW As Integer = fBmp.Width
        Dim picH As Integer = fBmp.Height

        If picW * picH > 400 Then
            MsgBox("選択画像が大きすぎます、縦ｘ横＝400ピクセルまで")
            fBmp.Dispose()
            Exit Sub
        End If

        Dim x As Integer
        Dim y As Integer
        Dim a As String
        Dim col As Color
        'Dim fColor As Color = myForm3.ButtonZoomFontColor.ForeColor
        'Dim fBrushe As New SolidBrush(fColor) 'フォントカラー

        Const zoomSize As Integer = 30
        Dim bmp As New Bitmap(zoomSize * picW, zoomSize * picH)

        Dim g As Graphics = Graphics.FromImage(bmp)

        If ARGB = 0 Then
            For x = 0 To picW - 1
                For y = 0 To picH - 1
                    col = fBmp.GetPixel(x, y)
                    a = col.A
                    Dim brush As New SolidBrush(col)
                    g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)
                    g.DrawString("a" & a, New Font("MS ゴシック", 9), Brushes.Gray, x * zoomSize + 1, y * zoomSize + 1)
                    g.DrawString("a" & a, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, y * zoomSize)
                Next
            Next
        ElseIf ARGB = 1 Then
            For x = 0 To picW - 1
                For y = 0 To picH - 1
                    col = fBmp.GetPixel(x, y)
                    a = col.R
                    Dim brush As New SolidBrush(col)
                    g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)
                    'g.DrawString("r" & a, New Font("MS ゴシック", 9), fBrushe, x * zoomSize, y * zoomSize)
                    g.DrawString("r" & a, New Font("MS ゴシック", 9), Brushes.Gray, x * zoomSize + 1, y * zoomSize + 1)
                    g.DrawString("r" & a, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, y * zoomSize)
                Next
            Next
        ElseIf ARGB = 2 Then
            For x = 0 To picW - 1
                For y = 0 To picH - 1
                    col = fBmp.GetPixel(x, y)
                    a = col.G
                    'Dim fontColor As Integer = col.R + col.G + col.B
                    Dim brush As New SolidBrush(col)
                    g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)
                    'g.DrawString("g" & a, New Font("MS ゴシック", 9), fBrushe, x * zoomSize, y * zoomSize)
                    g.DrawString("g" & a, New Font("MS ゴシック", 9), Brushes.Gray, x * zoomSize + 1, y * zoomSize + 1)
                    g.DrawString("g" & a, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, y * zoomSize)
                Next
            Next
        ElseIf ARGB = 3 Then
            For x = 0 To picW - 1
                For y = 0 To picH - 1
                    col = fBmp.GetPixel(x, y)
                    a = col.B
                    Dim brush As New SolidBrush(col)
                    g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)
                    'g.DrawString("b" & a, New Font("MS ゴシック", 9), fBrushe, x * zoomSize, y * zoomSize)
                    g.DrawString("b" & a, New Font("MS ゴシック", 9), Brushes.Gray, x * zoomSize + 1, y * zoomSize + 1)
                    g.DrawString("b" & a, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, y * zoomSize)
                Next
            Next
        ElseIf ARGB = 9 Then
            For x = 0 To picW - 1
                For y = 0 To picH - 1
                    col = fBmp.GetPixel(x, y)
                    a = col.B
                    Dim brush As New SolidBrush(col)
                    g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)

                Next
            Next

        End If


        Dim name As String = ActExPic.Name '"四角形" 'ピクチャーボックスの名前
        'g.FillRegion(Brushes.AliceBlue, bmp)

        Call PicBoxAdd(name, bmp)
        'Me.FocusPic.Image = bmp
    End Sub

    '選択画像を30倍に拡大してピクセルごとのアルファ値を表示
    '0ならアルファ値、1が赤、2が緑、3が青、引数なしで9で表示なし
    Friend Sub ZoomPicAllAdd(Optional ByVal ARGB As Integer = 9)

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim fBmp As New Bitmap(DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image)

        Dim picW As Integer = fBmp.Width
        Dim picH As Integer = fBmp.Height

        If picW * picH > 400 Then
            MsgBox("選択画像が大きすぎます、縦ｘ横＝400ピクセルまで")
            fBmp.Dispose()
            Exit Sub
        End If

        Dim x As Integer
        Dim y As Integer
        Dim a As String
        Dim col As Color
        'Dim fColor As Color = myForm3.ButtonZoomFontColor.ForeColor
        'Dim fBrushe As New SolidBrush(fColor)

        Dim name As String
        Dim red As Integer
        Dim green As Integer
        Dim blue As Integer
        Const zoomSize As Integer = 30
        Dim bmp As New Bitmap(1, 1)

        If fBmp.Width - fBmp.Height <= 0 Then
            bmp = New Bitmap(zoomSize * picW * 4, zoomSize * picH)

            Dim g As Graphics = Graphics.FromImage(bmp)

            For x = 0 To picW - 1
                For y = 0 To picH - 1
                    col = fBmp.GetPixel(x, y)
                    a = col.A
                    red = col.R
                    green = col.G
                    blue = col.B
                    Dim brush As New SolidBrush(col)
                    g.FillRectangle(brush, (4 * x) * zoomSize, y * zoomSize, zoomSize * 4, zoomSize)
                    g.DrawString("a" & a, New Font("MS ゴシック", 9), Brushes.Gray, x * 4 * zoomSize + 1, y * zoomSize + 1)
                    g.DrawString("a" & a, New Font("MS ゴシック", 9), Brushes.White, x * 4 * zoomSize, y * zoomSize)
                    g.DrawString("r" & red, New Font("MS ゴシック", 9), Brushes.Gray, ((x * 4 + 1) * zoomSize) + 1, y * zoomSize + 1)
                    g.DrawString("r" & red, New Font("MS ゴシック", 9), Brushes.White, ((x * 4 + 1) * zoomSize), y * zoomSize)
                    g.DrawString("g" & green, New Font("MS ゴシック", 9), Brushes.Gray, ((x * 4 + 2) * zoomSize) + 1, y * zoomSize + 1)
                    g.DrawString("g" & green, New Font("MS ゴシック", 9), Brushes.White, ((x * 4 + 2) * zoomSize), y * zoomSize)
                    g.DrawString("b" & blue, New Font("MS ゴシック", 9), Brushes.Gray, ((x * 4 + 3) * zoomSize) + 1, y * zoomSize + 1)
                    g.DrawString("b" & blue, New Font("MS ゴシック", 9), Brushes.White, ((x * 4 + 3) * zoomSize), y * zoomSize)
                    'g.DrawString("g" & green, New Font("MS ゴシック", 9), fBrushe, x * 2 * zoomSize, (y * zoomSize) + 15)
                    'g.DrawString("b" & blue, New Font("MS ゴシック", 9), fBrushe, (x * 2 * zoomSize) + 30, y * zoomSize + 15)
                Next
            Next
            name = ActExPic.Name & "拡大" '"四角形" 'ピクチャーボックスの名前
            Call PicBoxAdd(name, bmp)
        Else
            bmp = New Bitmap(zoomSize * picW, zoomSize * picH * 3)
            Dim g As Graphics = Graphics.FromImage(bmp)

            For x = 0 To picW - 1
                For y = 0 To picH - 1
                    col = fBmp.GetPixel(x, y)
                    a = col.A
                    red = col.R
                    green = col.G
                    blue = col.B

                    Dim brush As New SolidBrush(col)
                    g.FillRectangle(brush, x * zoomSize, (3 * y) * zoomSize, zoomSize, zoomSize * 3)
                    g.DrawString("a" & a, New Font("MS ゴシック", 9), Brushes.Gray, x * zoomSize + 1, (3 * y) * zoomSize + 1)
                    g.DrawString("a" & a, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, (3 * y) * zoomSize)
                    g.DrawString("r" & red, New Font("MS ゴシック", 9), Brushes.Gray, x * zoomSize + 1, (3 * y + 0.5) * zoomSize + 1)
                    g.DrawString("r" & red, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, (3 * y + 0.5) * zoomSize)
                    g.DrawString("g" & green, New Font("MS ゴシック", 9), Brushes.Gray, x * zoomSize + 1, (3 * y + 1) * zoomSize + 1)
                    g.DrawString("g" & green, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, (3 * y + 1) * zoomSize)
                    g.DrawString("b" & blue, New Font("MS ゴシック", 9), Brushes.Gray, x * zoomSize + 1, (3 * y + 1.5) * zoomSize + 1)
                    g.DrawString("b" & blue, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, (3 * y + 1.5) * zoomSize)
                Next
            Next
            name = ActExPic.Name & "拡大" '"四角形" 'ピクチャーボックスの名前
            Call PicBoxAdd(name, bmp)

        End If




        'For x = 0 To picW - 1
        '    For y = 0 To picH - 1
        '        col = fBmp.GetPixel(x, y)

        '        Dim brush As New SolidBrush(col)
        '        g.FillRectangle(brush, x * zoomSize, (2 * y + 1) * zoomSize, zoomSize, zoomSize)
        '    Next
        'Next







        'For x = 0 To picW - 1
        '    For y = 0 To picH - 1
        '        col = fBmp.GetPixel(x, y)
        '        a = col.G
        '        'Dim fontColor As Integer = col.R + col.G + col.B
        '        Dim brush As New SolidBrush(col)
        '        g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)
        '        g.DrawString("g" & a, New Font("MS ゴシック", 9), fBrushe, x * zoomSize, y * zoomSize)
        '    Next
        'Next
        'name = FocusPic.Name & "_g" '"四角形" 'ピクチャーボックスの名前
        'Call PicBoxAdd(name, bmp)

        'For x = 0 To picW - 1
        '    For y = 0 To picH - 1
        '        col = fBmp.GetPixel(x, y)
        '        a = col.B
        '        Dim brush As New SolidBrush(col)
        '        g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)
        '        g.DrawString("b" & a, New Font("MS ゴシック", 9), fBrushe, x * zoomSize, y * zoomSize)
        '    Next
        'Next
        'g.Dispose()

        'name = FocusPic.Name & "_b" '"四角形" 'ピクチャーボックスの名前
        'Call PicBoxAdd(name, bmp)

        'For x = 0 To picW - 1
        '    For y = 0 To picH - 1
        '        col = fBmp.GetPixel(x, y)
        '        a = col.B
        '        Dim brush As New SolidBrush(col)
        '        g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)
        '    Next
        'Next
        'g.Dispose()

        'name = FocusPic.Name & "_拡大" '"四角形" 'ピクチャーボックスの名前
        'Call PicBoxAdd(name, bmp)





    End Sub

    '選択画像を30倍に拡大してピクセルごとのアルファ値を表示
    Friend Sub ZoomPicAddEachRange()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If

        Dim range As Integer
        Dim gRange As Single
        Dim tStrength As Single

        Dim tag As Integer = ActExPic.Tag

        For range = 1 To 10


            Dim fbmp As New Bitmap(DirectCast(myPicArBackup(tag - 1), ExPictureBox).Image)
            Dim picW As Integer = fbmp.Width
            Dim picH As Integer = fbmp.Height

            If picW * picH > 200 Then
                MsgBox("選択画像が大きすぎます、200ピクセルまで")
                fbmp.Dispose()
                Exit Sub
            End If


            gRange = range / 10
            tStrength = range / 10

            Dim bmp As Bitmap
            Select Case True
                Case myForm3.RadioButtonTransprentGradationLeft.Checked
                    'bmp = New Bitmap(TransparentGradation2kai(fbmp, , tStrength))'範囲固定で強さ変化
                    bmp = New Bitmap(TransparentGradation2kai(fbmp, gRange)) '強さ固定で範囲変化
                Case myForm3.RadioButtonTransprentGradationLeftRight.Checked
                    'bmp = New Bitmap(TransparentGradation3kai(fbmp, , tStrength))
                    bmp = New Bitmap(TransparentGradation3kai(fbmp, gRange))
                Case myForm3.RadioButtonTransprentGradationLeftUp.Checked
                    'bmp = New Bitmap(TransparentGradation4LockBitkai(fbmp, , tStrength))
                    bmp = New Bitmap(TransparentGradation4LockBitkai(fbmp, gRange))

            End Select


            Dim x As Integer
            Dim y As Integer
            Dim a As String
            Dim col As Color
            Const zoomSize As Integer = 30
            Dim bmpAll As New Bitmap(zoomSize * picW, zoomSize * picH)

            Dim g As Graphics = Graphics.FromImage(bmpAll)

            For x = 0 To picW - 1
                For y = 0 To picH - 1

                    col = fbmp.GetPixel(x, y)
                    a = col.A
                    Dim brush As New SolidBrush(col)
                    g.FillRectangle(brush, x * zoomSize, y * zoomSize, zoomSize, zoomSize)
                    'g.DrawString(a, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, y * zoomSize)
                    g.DrawString("α" & a, New Font("MS ゴシック", 9), Brushes.Black, x * zoomSize + 1, y * zoomSize + 1)
                    g.DrawString("α" & a, New Font("MS ゴシック", 9), Brushes.White, x * zoomSize, y * zoomSize)
                Next
            Next

            Dim name As String = "枠" 'ピクチャーボックスの名前

            Call PicBoxAdd(name, bmpAll)
            If Me.RadioButtonUpper.Checked Then
                tag += 1
            End If

            'FocusPic.Image = DirectCast(myPicArBackup(FocusPic.Tag - 1), ExPictureBox).Image
        Next range
    End Sub
    '選択画像のサイズを取得して四角形作成の大きさにセット
    Friend Sub RectSize()

        If myPicAr.Count <> 0 Then
            If Me.ActExPic.Width > myForm3.NumericUpDownRectWidth.Maximum OrElse Me.ActExPic.Height > myForm3.NumericUpDownRectHeight.Maximum Then
                MsgBox("選択画像が大きすぎてサイズ取得できません")
                Exit Sub

            End If
            myForm3.NumericUpDownRectWidth.Value = Me.ActExPic.Width
            myForm3.NumericUpDownRectHeight.Value = Me.ActExPic.Height
        End If

    End Sub
    'myPicArBackupに現在の画像を上書き
    Friend Sub BackupOverWrite()

        If myPicAr.Count = 0 Then
            Exit Sub
        End If
        Dim i As Integer = ActExPic.Tag - 1
        Dim fPic As New Bitmap(DirectCast(myPicAr(i), ExPictureBox).Image)
        DirectCast(myPicArBackup(i), ExPictureBox).Image = fPic
        DirectCast(myPicArClone(i), ExPictureBox).Image = fPic


    End Sub

    Friend Function RGBtoHLS(ByVal col As Color) As String
        Dim hsv As String
        Dim r As Single = CSng(col.R / 255)
        Dim g As Single = CSng(col.G / 255)
        Dim b As Single = CSng(col.B / 255)
        Dim h As Single = 0
        Dim l As Single = 0
        Dim s As Single = 0


        Dim max As Single = Math.Max(r, Math.Max(g, b))
        Dim min As Single = Math.Min(r, Math.Min(g, b))

        l = (max + min) / 2

        If max = 0 Then
            hsv = "000 000 000"
            Return hsv
        ElseIf max + min = 2 Then
            l = l * 240 '色相240の場合
            'l = l * 360 '色相360の場合
            hsv = CInt(l).ToString("D3") & " " & "000 000"
            Return hsv
        End If


        If l <= 0.5 Then
            s = (max - min) / (max + min)
        Else
            s = (max - min) / (2 - max - min)
        End If

        '色相240の場合
        'If max - min <> 0 Then
        '    If r = max Then
        '        h = 40 * (g - b) / (max - min)
        '    ElseIf g = max Then
        '        h = 40 * (b - r) / (max - min) + 80
        '    ElseIf b = max Then
        '        h = 40 * (g - b) / (max - min) + 160
        '    End If
        'End If

        'If h < 0 Then
        '    h = h + 240
        'End If

        's = s * 240
        'l = l * 240


        '色相360の場合
        If max - min <> 0 Then
            If r = max Then
                h = 60 * (g - b) / (max - min)
            ElseIf g = max Then
                h = 60 * (b - r) / (max - min) + 120
            ElseIf b = max Then
                h = 60 * (g - b) / (max - min) + 240
            End If
        End If
        If h < 0 Then
            h = h + 360
        End If
        s = s * 360
        l = l * 360


        'hsv = String.Format("D3", h.ToString) ' & Strings.Format("d3", s.ToString & v.ToString)
        Dim h1 As Integer = CInt(h)
        Dim s1 As Integer = CInt(s)
        Dim l1 As Integer = CInt(l)
        Dim rgb As String

        'toRGB
        h = h1 / 60
        Dim i As Integer = CInt(Math.Floor(h))
        Dim f As Single = h - i
        Dim c As Single
        l = l1 / 360
        s = s1 / 360

        If s = 0 Then
            Dim allL As String = CInt(l).ToString("d3")
            rgb = allL & " " & allL & " " & allL
        Else

            If l < 0.5 Then
                c = 2 * s * l
            Else
                c = 2 * s * (1 - l)
            End If

            Dim m As Single = l - c / 2
            Dim p As Single = c + m
            Dim q As Single

            If i Mod 2 = 0 Then
                q = l + c * (f - 0.5)
            Else
                q = l - c * (f - 0.5)
            End If

            Select Case i
                Case 0
                    r = p
                    g = q
                    b = m
                    Exit Select
                Case 1
                    r = q
                    g = p
                    b = m
                    Exit Select
                Case 2
                    r = m
                    g = p
                    b = q
                    Exit Select
                Case 3
                    r = m
                    g = q
                    b = p
                    Exit Select
                Case 4
                    r = q
                    g = m
                    b = p
                    Exit Select
                Case 5
                    r = p
                    g = m
                    b = q
                    Exit Select
            End Select
            rgb = CInt(r * 255).ToString("D3") & " " & CInt(g * 255).ToString("d3") & " " & CInt(b * 255).ToString("d3")
            myForm3.LabelHSLtoRGB.Text = rgb
        End If




        'Dim m1 As Single
        'Dim m2 As Single

        'If l <= 0.5 Then
        '    m1 = l * (1 + s)
        '    m2 = 2 * l - m1
        'Else
        '    m2 = l + s - l * s
        '    m1 = 2 * l - m2
        'End If


        'If h1 > 360 Then
        '    h1 = h1 - 360
        'ElseIf h1 < 0 Then
        '    h1 = h1 + 360
        'End If
        'If h1 < 60 Then
        '    r = m1 + (m2 - m1) * (h1 + 120) / 60
        '    g = m1 + (m2 - m1) * h1 / 60
        '    b = m1 + (m2 - m1) * (h1 - 120) / 60

        'ElseIf h1 < 180 Then
        '    r = m2
        '    g = m2
        '    b = m2

        'ElseIf h1 < 2400 Then
        '    r = m1 + (m2 - m1) * (240 - h1 + 120) / 60
        '    g = m1 + (m2 - m1) * (240 - h1) / 60
        '    b = m1 + (m2 - m1) * (240 - h1 - 120) / 60
        'End If

        'myForm3.LabelHSLtoRGB.Text = RGB(CInt(r * 255), CInt(g * 255), CInt(b * 255))




        hsv = h1.ToString("d3") & " " & s1.ToString("d3") & " " & l1.ToString("d3")

        Return hsv

    End Function


    '透過コピペ
    Private Sub ButtonCopyPasteTransparent_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCopyPasteTransparent.Click
        Call CopyPsteTransparent()

    End Sub
    '-----------------------右クリックで範囲選択テストここから----------------------------------
    Private Sub Panel2_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel2.MouseDown

        If e.Button = Windows.Forms.MouseButtons.Right Then
            Me.Panel2.Refresh()
            FlagRightDragging = True
            'CurPoint = Me.Panel2.PointToClient(Cursor.Position)
            DiffPoint = New Point(e.X, e.Y)
        End If

    End Sub


    Private Sub Panel2_MouseMove(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel2.MouseMove

        If FlagRightDragging Then

            Dim newPoint As New Point(e.X, e.Y)
            Dim g As Graphics = Me.Panel2.CreateGraphics()
            Me.Panel2.Refresh()

            'g.Clear(Color.Transparent)


            g.DrawLine(Pens.Red, DiffPoint, newPoint)
            'g.DrawRectangle(Pens.AliceBlue, DiffPoint.X, DiffPoint.Y, DiffPoint.X - e.X, DiffPoint.Y - e.Y)
            'g.DrawRectangle(Pens.AliceBlue, DiffPoint.X, DiffPoint.Y, e.X - DiffPoint.X, e.Y - DiffPoint.Y)

            'If e.X - DiffPoint.X < 0 Then
            '    g.DrawRectangle(Pens.AliceBlue, e.X, DiffPoint.Y, DiffPoint.X - e.X, e.Y - DiffPoint.Y)

            'End If
            Select Case True
                Case e.X - DiffPoint.X >= 0 And e.Y - DiffPoint.Y >= 0
                    g.DrawRectangle(Pens.AliceBlue, DiffPoint.X, DiffPoint.Y, e.X - DiffPoint.X, e.Y - DiffPoint.Y)
                Case e.X - DiffPoint.X < 0 And e.Y - DiffPoint.Y < 0
                    g.DrawRectangle(Pens.AliceBlue, e.X, e.Y, DiffPoint.X - e.X, DiffPoint.Y - e.Y)
                Case e.X - DiffPoint.X < 0
                    g.DrawRectangle(Pens.AliceBlue, e.X, DiffPoint.Y, DiffPoint.X - e.X, e.Y - DiffPoint.Y)
                Case e.Y - DiffPoint.Y < 0
                    g.DrawRectangle(Pens.AliceBlue, DiffPoint.X, e.Y, e.X - DiffPoint.X, DiffPoint.Y - e.Y)
            End Select

        End If

    End Sub

    Private Sub Panel2_MouseUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel2.MouseUp
        If e.Button = Windows.Forms.MouseButtons.Right Then
            FlagRightDragging = False

        End If
    End Sub
    '-----------------------右クリックで範囲選択テストここまで----------------------------------

    '■作成で範囲選択
    Friend Sub RectRangeSelect範囲選択用画像表示()
        Dim col As Color = Color.FromArgb(128, myForm3.PictureBoxRangeSelectColor.BackColor)
        'Dim col2 As Color

        If col = Color.White Then
            col = Color.FromArgb(64, Color.White)
            'col2 = Color.FromArgb(64, Color.White)
        End If
        Dim rw As Integer = myForm3.NumericUpDownRectRangeH.Value
        Dim rh As Integer = myForm3.NumericUpDownRectRangeV.Value
        Dim bmp As New Bitmap(rw, rh)
        'Dim rect As New Rectangle(0, 0, rw - 1, rh - 1) 'Pen用、'g.DrawRectangle(myPen, rect)
        Dim rect As New Rectangle(0, 0, rw, rh) '塗りつぶし用(g.FillRectangle(myBrush, rect))
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh - 1) '楕円用
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim myPen As New Pen(Color.FromArgb(128, Color.AliceBlue))
        Dim myBrush As New SolidBrush(col)
        'Dim myBrush2 As New SolidBrush(col2)

        'Dim myBrush As New SolidBrush(Color.FromArgb(128, Color.AliceBlue))
        'Dim myBrush As New SolidBrush(Color.FromArgb(128, Color.Black))

        '選択範囲の図形がすでにあれば画像だけ入れ替える
        For Each c As ExPictureBox In myPicAr
            If c.Name = "範囲選択_T" Then
                'Call RectRangeSelectRealTime()

                c.Focus()
                ActExPic = c

                '図形判定
                If myForm3.RadioButtonRactRange.Checked Then '■のとき
                    'g.DrawRectangle(myPen, rect)
                    g.FillRectangle(myBrush, rect)
                    'g.DrawEllipse(myPen, rect2)
                    'g.FillEllipse(myBrush2, rect2)
                ElseIf myForm3.RadioButtonEllipseRange.Checked Then '●のとき
                    bmp = New Bitmap(c.Image.Width, c.Image.Height)
                    g = Graphics.FromImage(bmp)
                    rect = New Rectangle(0, 0, c.Image.Width, c.Image.Height)
                    'g.FillRectangle(myBrush, rect)

                    Dim gp As New GraphicsPath
                    gp.AddRectangle(rect)
                    'gp.AddEllipse(rect)
                    'gp.AddEllipse(-1, -1, bmp.Width + 1, bmp.Height + 1) '枠いっぱい
                    gp.AddEllipse(0, 0, bmp.Width - 1, bmp.Height - 1)

                    Dim rgn As New Region(gp)

                    g.Clip = rgn
                    'g.DrawImage(c.Image, rect)

                    Dim img As Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
                    g.DrawImage(img, rect)
                End If



                c.Image = bmp
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp
                DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image = c.Image
                g.Dispose()

                Call Transparent6()
                Call UpdateThumbnail()

                'ポイントの削除
                For Each lp As Label In LabelPoint範囲選択画像
                    Panel2.Controls.Remove(lp)
                    lp.Dispose()
                Next
                LabelPoint範囲選択画像.Clear()

                'ポイントの作成
                'Call SelectRangePoint()
                Call SelectRangePoint初期化()
                Exit Sub

            End If

        Next
        g.FillRectangle(myBrush, rect)
        g.Dispose()

        'ExPicture作成
        Call PicBoxAdd("範囲選択", bmp)
        selectPicbox範囲選択画像 = ActExPic 'selectPicbox範囲選択画像に指定


        'ポイントの削除
        For Each lp As Label In LabelPoint範囲選択画像
            Panel2.Controls.Remove(lp)
            lp.Dispose()
        Next
        LabelPoint範囲選択画像.Clear()

        'ポイントの作成
        'Call SelectRangePoint()
        Call SelectRangePoint初期化()

    End Sub

    Friend Sub RectRangeSelectRealTime(ByVal rw As Integer, ByVal rh As Integer)
        If myPicAr.Count <= 1 Then Exit Sub ' And ActExPic.Name = "範囲選択_T" Then Exit Sub

        Dim col As Color = Color.FromArgb(128, myForm3.PictureBoxRangeSelectColor.BackColor)


        For Each c As ExPictureBox In myPicAr
            If c.Name = "範囲選択_T" Then
                'c.Focus()
                ActExPic = c

                Dim bmp As New Bitmap(rw, rh)

                Dim rect As New Rectangle(0, 0, rw, rh) '塗りつぶし用(g.FillRectangle(myBrush, rect))
                Dim g As Graphics = Graphics.FromImage(bmp)
                'Dim myPen As New Pen(Color.FromArgb(128, Color.AliceBlue))
                Dim myBrush As New SolidBrush(col)
                Dim clonePic As ExPictureBox = DirectCast(myPicArClone(c.Tag - 1), ExPictureBox)

                '

                If myForm3.RadioButtonRactRange.Checked Then '■のとき
                    g.FillRectangle(myBrush, rect)
                ElseIf myForm3.RadioButtonEllipseRange.Checked Then '●の時

                    '一旦新しいBitmapを作り半透明塗り、これを使ってクリップする

                    Dim canvas As New Bitmap(rw, rh)
                    Dim g2 As Graphics = Graphics.FromImage(canvas)
                    g2.FillRectangle(myBrush, rect)

                    Dim gp As New GraphicsPath

                    '逆範囲の判定
                    If myForm3.CheckBoxReverseRange.Checked Then '順範囲のとき
                        g.FillEllipse(myBrush, 0, 0, bmp.Width - 1, bmp.Height - 1)
                        g.DrawRectangle(Pens.AliceBlue, 0, 0, rect.Width - 1, rect.Height - 1)

                    Else                                        '逆範囲のとき
                        gp.AddRectangle(rect)
                        'gp.AddEllipse(rect)
                        'gp.AddEllipse(-1, -1, bmp.Width + 1, bmp.Height + 1) '枠いっぱい
                        gp.AddEllipse(0, 0, bmp.Width - 1, bmp.Height - 1)

                        Dim rgn As New Region(gp)
                        'rgn.Exclude(gp)
                        'rgn.Union(gp)
                        'rgn.Intersect(gp)
                        'rgn.Xor(gp)
                        g.Clip = rgn

                        'g.DrawRectangle(Pens.AliceBlue, rect)
                        g.DrawImage(canvas, rect)
                    End If

                End If



                c.Image = bmp
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp
                DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image = c.Image
                g.Dispose()

                Call Transparent6()


                Call RSLabel選択範囲画像ラベルの移動()
                Exit For

            End If

            'Exit For

        Next
    End Sub

    '選択範囲をコピペ、未使用、SelectRangeCopyPasteとSelectRangeBitmapAddに分けた
    Friend Sub RectRangeCopyPaste()
        If myPicAr.Count < 2 Then
            Exit Sub

        End If
        'Dim rw As Integer
        'Dim rh As Integer
        'Dim canvas As New Bitmap(1, 1)


        '一旦選択範囲を透明で塗りつぶす
        For Each c As ExPictureBox In myPicAr
            If c.Name = "範囲選択_T" Then
                ActExPic = c
                Dim rect As New Rectangle(0, 0, c.Image.Width, c.Image.Height)
                Dim bmp As New Bitmap(c.Image.Width, c.Image.Height)
                Dim g As Graphics = Graphics.FromImage(bmp)
                'Dim myPen As New Pen(Brushes.AliceBlue)
                'g.DrawRectangle(myPen, rect)
                Dim myBrush As New SolidBrush(Color.Transparent)
                g.FillRectangle(myBrush, rect)

                c.Image = bmp
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp
                'DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image = bmp

                Call Transparent6()

                '図形判定

                If myForm3.RadioButtonEllipseRange.Checked Then '●の時
                    Dim gp As New GraphicsPath()

                    If myForm3.CheckBoxReverseRange.Checked Then
                        gp.AddRectangle(rect) '逆範囲コピペで使う
                    End If


                    'gp.AddEllipse(-1, -1, bmp.Width + 1, bmp.Height + 1)’枠いっぱい
                    'gp.AddEllipse(0, 0, bmp.Width, bmp.Height)’これは中途半端な大きさ
                    gp.AddEllipse(0, 0, bmp.Width - 1, bmp.Height - 1) '1ピクセル小さい

                    Dim rgn As New Region(gp) 'レギオン
                    'rgn.Union(gp)
                    g.Clip = rgn '差分をClip


                    'g.SetClip(gp)
                    g.DrawImage(c.Image, g.VisibleClipBounds) '描画

                    c.Image = bmp
                    DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp
                    'DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image = bmp
                    'g.Dispose()
                    'myBrush.Dispose()
                    gp.Dispose()
                End If

                g.Dispose()
                myBrush.Dispose()
                'Call SelectRangeSave(c.Image)

                Dim name As String = "RangeCopyPaste"


                Call PicBoxAdd(name, c.Image) '切り抜き画像作成



                'バックアップ画像には半透明画像があるのでこれを利用して表示用画像に戻す
                Dim img As Image = DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image
                'Dim img As Image = DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image
                c.Image = img
                DirectCast(myPicAr(c.Tag - 1), ExPictureBox).Image = img
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = img
                Call Transparent2()
                'img.Dispose()'これはエラーになる

                Call UpdateThumbnail()
                Exit For

            End If

        Next

        '透明で塗りつぶした下に写っている画像で新規画像作成
        'For Each c As ExPictureBox In myPicAr
        '    If c.Name = "範囲選択_T" Then
        '        rw = c.Image.Width
        '        rh = c.Image.Height

        '        'canvas = New Bitmap(rw - 1, rh - 1)
        '        Dim bmp As New Bitmap(c.Image)

        '        'Dim g As Graphics = Graphics.FromImage(canvas)
        '        'g.DrawImage(c.Image, 0, 0, rw, rh)

        '        Call PicBoxAdd(name, bmp)
        '        Exit For
        '    End If
        'Next
        'Call RectRangeSelect() '透明で塗りつぶしたのを半透明に戻す

    End Sub
    '選択範囲のコピペ
    Friend Sub SelectRangeCopyPaste()
        '画像がない時と範囲選択画像が一番下の時はなにもしないで終了
        If myPicAr.Count < 2 OrElse selectPicbox範囲選択画像.Tag = myPicAr.Count Then
            Exit Sub

        End If

        For Each c As ExPictureBox In myPicAr

            If c.Name = "範囲選択_T" Then
                Dim bmp As New Bitmap(SelectRangeBitmapAdd(c))
                Dim name As String = "RangeCopyPaste"


                Call PicBoxAdd(name, c.Image) '切り抜き画像作成



                'バックアップ画像には半透明画像があるのでこれを利用して表示用画像に戻す
                Dim img As Image = DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image
                'Dim img As Image = DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image
                c.Image = img
                DirectCast(myPicAr(c.Tag - 1), ExPictureBox).Image = img
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = img
                Call Transparent2()
                'img.Dispose()'これはエラーになる

                Call UpdateThumbnail()
                Exit Sub

            End If

        Next

    End Sub
    '選択範囲の画像作成
    Friend Function SelectRangeBitmapAdd(ByVal c As ExPictureBox) As Bitmap
        'If myPicAr.Count < 2 Then
        '    Exit Function

        'End If
        'Dim rw As Integer
        'Dim rh As Integer
        'Dim canvas As New Bitmap(1, 1)


        '一旦選択範囲を透明で塗りつぶす
        'For Each c As ExPictureBox In myPicAr
        '    If c.Name = "範囲選択_T" Then
        ActExPic = c
        Dim rect As New Rectangle(0, 0, c.Image.Width, c.Image.Height)
        Dim bmp As New Bitmap(c.Image.Width, c.Image.Height)
        Dim g As Graphics = Graphics.FromImage(bmp)
        'Dim myPen As New Pen(Brushes.AliceBlue)
        'g.DrawRectangle(myPen, rect)
        Dim myBrush As New SolidBrush(Color.Transparent)
        g.FillRectangle(myBrush, rect)
        Dim col As Color = myForm3.PictureBoxRangeSelectColor.BackColor
        myBrush = New SolidBrush(Color.FromArgb(64, col))

        c.Image = bmp
        DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp
        'DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image = bmp

        Call Transparent6()

        '図形判定

        If myForm3.RadioButtonEllipseRange.Checked Then '●の時
            Dim gp As New GraphicsPath()

            If myForm3.CheckBoxReverseRange.Checked Then
                gp.AddRectangle(rect) '逆範囲コピペで使う
            End If


            'gp.AddEllipse(-1, -1, bmp.Width + 1, bmp.Height + 1)’枠いっぱい
            'gp.AddEllipse(0, 0, bmp.Width, bmp.Height)’これは中途半端な大きさ
            gp.AddEllipse(0, 0, bmp.Width - 1, bmp.Height - 1) '1ピクセル小さい

            Dim rgn As New Region(gp) 'レギオン
            'rgn.Union(gp)
            g.Clip = rgn '差分をClip


            'g.SetClip(gp)
            g.DrawImage(c.Image, g.VisibleClipBounds) '描画

            c.Image = bmp
            DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp
            'DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image = bmp
            'g.Dispose()
            'myBrush.Dispose()
            gp.Dispose()
        ElseIf myForm3.RadioButtonRactRange.Checked Then

            g.DrawImage(c.Image, rect)


        End If

        g.Dispose()
        myBrush.Dispose()
        Return bmp

        'Call SelectRangeSave(c.Image)

        'Dim name As String = "RangeCopyPaste"


        'Call PicBoxAdd(name, c.Image) '切り抜き画像作成



        ''バックアップ画像には半透明画像があるのでこれを利用して表示用画像に戻す
        'Dim img As Image = DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image
        ''Dim img As Image = DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image
        'c.Image = img
        'DirectCast(myPicAr(c.Tag - 1), ExPictureBox).Image = img
        'DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = img
        'Call Transparent2()
        ''img.Dispose()'これはエラーになる

        'Call UpdateThumbnail()
        'Exit For

        'End If

        'Next

    End Function
    Friend Function GetSize() As Size
        If myPicAr.Count = 0 Then
            Exit Function

        End If
        Dim rectSize As Size = ActExPic.Size
        Return rectSize

    End Function
    '●楕円で範囲選択テスト、未使用
    Friend Sub EllipseRangeSelect()
        Dim col As Color = myForm3.PictureBoxRangeSelectColor.BackColor

        If col = Color.White Then
            col = Color.FromArgb(64, Color.White)

        End If
        Dim rw As Integer = myForm3.NumericUpDownRectRangeH.Value
        Dim rh As Integer = myForm3.NumericUpDownRectRangeV.Value
        Dim bmp As New Bitmap(rw, rh)
        'Dim rect As New Rectangle(0, 0, rw - 1, rh - 1) 'Pen用、'g.DrawRectangle(myPen, rect)
        Dim rect As New Rectangle(0, 0, rw, rh) '塗りつぶし用(g.FillRectangle(myBrush, rect))
        Dim rect2 As New Rectangle(0, 0, rw - 1, rh - 1) '楕円用
        Dim g As Graphics = Graphics.FromImage(bmp)
        'Dim myPen As New Pen(Color.FromArgb(64, Color.White))
        Dim myBrush As New SolidBrush(col)


        '選択範囲の図形がすでにあれば画像だけ入れ替える
        For Each c As ExPictureBox In myPicAr
            If c.Name = "範囲選択_T" Then
                c.Focus()
                ActExPic = c



                bmp = New Bitmap(c.Image.Width, c.Image.Height)
                g = Graphics.FromImage(bmp)
                rect = New Rectangle(0, 0, c.Image.Width, c.Image.Height)
                'g.FillRectangle(myBrush, rect)

                Dim gp As New GraphicsPath
                gp.AddRectangle(rect)
                'gp.AddEllipse(rect)
                'gp.AddEllipse(-1, -1, bmp.Width + 1, bmp.Height + 1) '枠いっぱい
                gp.AddEllipse(0, 0, bmp.Width - 1, bmp.Height - 1)

                Dim rgn As New Region(gp)

                g.Clip = rgn
                'g.DrawImage(c.Image, rect)

                Dim img As Image = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image
                g.DrawImage(img, rect)






                ''g.DrawRectangle(myPen, rect)
                'g.FillRectangle(myBrush, rect) '■を半透明で塗りつぶす
                ''g.DrawEllipse(myPen, rect2)
                'g.FillEllipse(Brushes.AliceBlue, -1, -1, rw + 1, rh + 1) '●を不透明で塗りつぶす





                ''LockBits
                'Dim lockW As Integer = c.Image.Width
                'Dim lockH As Integer = c.Image.Height
                'Dim lockRect As New Rectangle(0, 0, lockW, lockH)
                'bmp = New Bitmap(lockW, lockH)
                'Dim bmpdata As BitmapData = bmp.LockBits(lockRect, ImageLockMode.ReadWrite, bmp.PixelFormat)
                'Dim ptr As IntPtr = bmpdata.Scan0
                'Dim data As Integer = bmpdata.Stride * lockH - 1
                'Dim pixels(data) As Byte
                'System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
                'For x = 0 To lockW - 1
                '    For y = 0 To lockH - 1
                '        Dim pos As Integer = bmpdata.Stride * y + x * 4

                '        If pixels(pos + 3) = 255 Then '不透明部分は●なので透明に変換する
                '            pixels(pos) = 0
                '            pixels(pos + 1) = 0
                '            pixels(pos + 2) = 0
                '            pixels(pos + 3) = 0
                '        End If

                '    Next
                'Next
                'System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
                'bmp.UnlockBits(bmpdata)


                c.Image = bmp
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp
                DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image = c.Image
                g.Dispose()

                Call Transparent6()
                Call UpdateThumbnail()

                Exit Sub

            End If

        Next
        g.FillRectangle(myBrush, rect)

        'g.DrawRectangle(myPen, rect)

        g.Dispose()
        Call PicBoxAdd("範囲選択", bmp)

    End Sub


    '選択範囲をコピペ楕円テスト、未使用
    Friend Sub EllipseRangeCopyPaste()
        If myPicAr.Count < 2 Then
            Exit Sub

        End If
        'Dim rw As Integer
        'Dim rh As Integer
        'Dim canvas As New Bitmap(1, 1)
        Dim name As String = "RangeCopyPaste"



        '一旦選択範囲を透明で塗りつぶす
        For Each c As ExPictureBox In myPicAr
            If c.Name = "範囲選択_T" Then
                '一旦透明で描画して下の画像を映してその画像を楕円と合成して差分の画像を作る

                ActExPic = c
                Dim rect As New Rectangle(0, 0, c.Image.Width, c.Image.Height)
                Dim bmp As New Bitmap(c.Image.Width, c.Image.Height)
                Dim g As Graphics = Graphics.FromImage(bmp)
                Dim myBrush As New SolidBrush(Color.Transparent)
                g.FillRectangle(myBrush, rect)
                c.Image = bmp
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp 'クローンにも透明画像を入れる
                Call Transparent6() '透過処理
                'g.SmoothingMode = SmoothingMode.None '意味ないみたい

                Dim gp As New GraphicsPath()
                'gp.AddRectangle(rect)
                'gp.AddEllipse(-1, -1, bmp.Width + 1, bmp.Height + 1)’枠いっぱい
                'gp.AddEllipse(0, 0, bmp.Width, bmp.Height)’これは中途半端な大きさ
                gp.AddEllipse(0, 0, bmp.Width - 1, bmp.Height - 1) '1ピクセル小さい

                Dim rgn As New Region(gp) 'レギオン
                'rgn.Union(gp)
                g.Clip = rgn '差分をClip


                'g.SetClip(gp)
                g.DrawImage(c.Image, g.VisibleClipBounds) '描画

                c.Image = bmp
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = bmp
                'DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image = bmp
                g.Dispose()
                myBrush.Dispose()
                gp.Dispose()

                'Call Transparent6()
                Call PicBoxAdd(name, bmp) '切り抜き画像作成


                'バックアップ画像には半透明画像があるのでこれを利用して表示用画像に戻す
                Dim img As Image = DirectCast(myPicArBackup(c.Tag - 1), ExPictureBox).Image
                c.Image = img
                DirectCast(myPicAr(c.Tag - 1), ExPictureBox).Image = img
                DirectCast(myPicArClone(c.Tag - 1), ExPictureBox).Image = img
                Call Transparent2()
                'img.Dispose()'これはエラーになる

                Call UpdateThumbnail()
                Exit For

            End If

        Next


    End Sub



    '----------------ここから範囲選択画像表示-------------------
    Friend Sub SelectRangePoint初期化()
        If myPicAr.Count = 0 Then Exit Sub

        'ラベルが残っていたら全て消去
        If LabelPoint範囲選択画像.Count <> 0 Then
            For Each lp As Label In LabelPoint範囲選択画像
                Me.Panel2.Controls.Remove(lp)
            Next
            LabelPoint範囲選択画像.Clear()
        End If

        Dim ps() = サイズ変更ラベルの位置配列作成(selectPicbox範囲選択画像.Bounds) 'ExPictureのサイズからラベルの座標の配列作成
        Dim lName() As String = {"RS_LU", "RS_LD", "RS_RU", "RS_RD", "RS_U", "RS_D", "RS_L", "RS_R"} 'ラベルの名前
        Dim bmp As New Bitmap(LABEL_PIC_SIZE, LABEL_PIC_SIZE)
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim b As New SolidBrush(Color.Red)
        Dim p As New Pen(Brushes.White)
        g.FillRectangle(b, 0, 0, bmp.Width, bmp.Height)
        g.DrawRectangle(p, 1, 1, bmp.Width - 3, bmp.Height - 3)
        g.Dispose()
        b.Dispose()
        p.Dispose()

        For i As Integer = 0 To UBound(ps)
            Dim L As New Label
            With L
                .SetBounds(ps(i).X, ps(i).Y, LABEL_PIC_SIZE, LABEL_PIC_SIZE)
                '.BackColor = Color.FromArgb(255, Color.LimeGreen)
                .Cursor = Cursors.Hand
                .Name = lName(i)
                .Image = bmp

            End With
            Me.Panel2.Controls.Add(L) 'ラベルをPanel2に追加して表示
            LabelPoint範囲選択画像.Add(L) '管理リストに追加
            L.BringToFront() '最前面に表示


            AddHandler L.MouseDown, AddressOf MouseDownRSLabel選択範囲画像
            AddHandler L.MouseMove, AddressOf MouseMoveRSLabel選択範囲画像
            AddHandler L.MouseUp, AddressOf MouseUpRSLabel選択範囲画像

        Next
        Me.Panel2.Controls("RS_RD").BringToFront() '右下のラベルを一番上に表示する

        'すでに選択範囲画像が作成されていたらそれを選択する
        If Me.Panel2.Controls.Contains(selectPicbox範囲選択画像) = False Then
            For Each c As ExPictureBox In myPicAr

                If c.Name = "範囲選択_T" Then
                    selectPicbox範囲選択画像 = c
                    Exit For
                End If

            Next
        End If
    End Sub
    Private Sub MouseDownRSLabel選択範囲画像(sender As Label, e As MouseEventArgs)
        If e.Button <> Windows.Forms.MouseButtons.Left Then Exit Sub

        Xp範囲選択画像 = e.X
        Yp範囲選択画像 = e.Y

        'グリッドからずれていたらサイズと位置を合わせる
        Dim xsb As Integer = Me.Panel2.AutoScrollPosition.X
        Dim ysb As Integer = Me.Panel2.AutoScrollPosition.Y

        Dim np As New Point(AbsolutePoint(selectPicbox範囲選択画像))
        Dim gv As Integer = Me.NumericUpDownGrid.Value
        Dim xMod As Integer = np.X Mod gv
        Dim yMod As Integer = np.Y Mod gv
        np = New Point(np.X - xMod + xsb, np.Y - yMod + ysb)
        selectPicbox範囲選択画像.Location = np '座標決定

        Dim w As Integer = selectPicbox範囲選択画像.Width
        Dim h As Integer = selectPicbox範囲選択画像.Height
        'グリッドの左上に一度合わせて
        Dim nw As Integer = w - (w Mod gv)
        Dim nh As Integer = h - (h Mod gv)
        'MODが0ならサイズ変更なし、それ以外なら右下に大きくする
        If w Mod gv <> 0 Then
            nw = nw + gv
        End If
        If h Mod gv <> 0 Then
            nh = nh + gv
        End If
        'サイズ変更、グリッドの倍数に合わせる

        'selectPicbox範囲選択画像.Width = nw
        'selectPicbox範囲選択画像.Height = nh
        myForm3.NumericUpDownRectRangeH.Value = nw
        myForm3.NumericUpDownRectRangeV.Value = nh



        '文鎮用のダミー画像設置、これ大事
        Me.Panel2.Controls.Add(DummyPicBox)

    End Sub
    Private Sub MouseMoveRSLabel選択範囲画像(sender As Label, e As MouseEventArgs)
        If e.Button <> Windows.Forms.MouseButtons.Left Then Exit Sub

        MMRSXマウスの移動距離 = e.X - Xp範囲選択画像
        MMRSYマウスの移動距離 = e.Y - Yp範囲選択画像
        Call RSPSizeSet選択範囲画像サイズ変更(sender)

    End Sub
    Private Sub MouseUpRSLabel選択範囲画像(sender As Label, e As MouseEventArgs)
        '文鎮用のダミー画像のBoundsをセット
        DummyPicBox.Bounds = selectPicbox範囲選択画像.Bounds

    End Sub
    Private Sub RSPSizeSet選択範囲画像サイズ変更(L As Label)
        'ヌメリックに数値指定して選択範囲画像の大きさを変更する
        Dim offSize As New Point(0, 0)
        Dim offPoint As New Point(0, 0)
        Dim picSize As New Point(selectPicbox範囲選択画像.Size)
        Dim picLocate As New Point(selectPicbox範囲選択画像.Location)
        Dim gridValue As Integer = Me.NumericUpDownGrid.Value
        Dim gXグリッド補正値 As Integer = MMRSXマウスの移動距離 Mod gridValue
        Dim gYグリッド補正値 As Integer = MMRSYマウスの移動距離 Mod gridValue

        Select Case L.Name
            Case "RS_U"
                offSize = New Point(0, -MMRSYマウスの移動距離 + gYグリッド補正値)
                offPoint = New Point(0, MMRSYマウスの移動距離 - gYグリッド補正値)
            Case "RS_D"
                offSize = New Point(0, MMRSYマウスの移動距離 - gYグリッド補正値)
                offPoint = New Point(0, 0)
            Case "RS_L"
                offSize = New Point(-MMRSXマウスの移動距離 + gXグリッド補正値, 0)
                offPoint = New Point(MMRSXマウスの移動距離 - gXグリッド補正値, 0)
            Case "RS_R"
                offSize = New Point(MMRSXマウスの移動距離 - gXグリッド補正値, 0)
                'offSize = New Point(-gXグリッド補正値, 0)
                offPoint = New Point(0, 0)
            Case "RS_LU"
                offSize = New Point(-MMRSXマウスの移動距離 + gXグリッド補正値, -MMRSYマウスの移動距離 + gYグリッド補正値)
                offPoint = New Point(MMRSXマウスの移動距離 - gXグリッド補正値, MMRSYマウスの移動距離 - gYグリッド補正値)
            Case "RS_RU"
                offSize = New Point(MMRSXマウスの移動距離 - gXグリッド補正値, -MMRSYマウスの移動距離 + gYグリッド補正値)
                offPoint = New Point(0, MMRSYマウスの移動距離 - gYグリッド補正値)
            Case "RS_RD"
                offSize = New Point(MMRSXマウスの移動距離 - gXグリッド補正値, MMRSYマウスの移動距離 - gYグリッド補正値)
                offPoint = New Point(0, 0)
            Case "RS_LD"
                offSize = New Point(-MMRSXマウスの移動距離 + gXグリッド補正値, MMRSYマウスの移動距離 - gYグリッド補正値)
                offPoint = New Point(MMRSXマウスの移動距離 - gXグリッド補正値, 0)
        End Select

        'ExPictureの大きさを制限値以下にはしないように
        Dim lx As Integer = myForm3.NumericUpDownRectRangeH.Minimum
        Dim ly As Integer = myForm3.NumericUpDownRectRangeV.Minimum
        If picSize.X + offSize.X < lx Or picSize.Y + offSize.Y < ly Then
            offSize = New Point(0, 0)
            offPoint = New Point(0, 0)
        End If

        'ExPictureのサイズと位置をセット
        picLocate.Offset(offPoint)
        picSize.Offset(offSize)

        selectPicbox範囲選択画像.Location = picLocate
        myForm3.NumericUpDownRectRangeH.Value = picSize.X ' += offSize.X
        myForm3.NumericUpDownRectRangeV.Value = picSize.Y ' += offSize.Y

        'ラベルの移動
        Dim ps() = サイズ変更ラベルの位置配列作成(selectPicbox範囲選択画像.Bounds) 'ExPictureのサイズからラベルの座標の配列作成
        Dim lName() As String = {"RS_LU", "RS_LD", "RS_RU", "RS_RD", "RS_U", "RS_D", "RS_L", "RS_R"} 'ラベルの名前
        For i As Integer = 0 To LabelPoint範囲選択画像.Count - 1
            LabelPoint範囲選択画像(i).Location = ps(i)

        Next


    End Sub
    Private Sub RSLabel選択範囲画像ラベルの移動()
        Dim ps() = サイズ変更ラベルの位置配列作成(selectPicbox範囲選択画像.Bounds) 'ExPictureのサイズからラベルの座標の配列作成
        'Dim lName() As String = {"RS_LU", "RS_LD", "RS_RU", "RS_RD", "RS_U", "RS_D", "RS_L", "RS_R"} 'ラベルの名前
        
        For i As Integer = 0 To LabelPoint範囲選択画像.Count - 1
            LabelPoint範囲選択画像(i).Location = ps(i)
            LabelPoint範囲選択画像(i).Visible = True
        Next
        
    End Sub
    Private Sub RSLabel選択範囲画像ラベルの非表示() '2015/05/16今は未使用
        '選択範囲画像はあるけど他の画像の下にある時はラベルは非表示にする
        'If Me.Panel2.Controls.Contains(selectPicbox範囲選択画像) = False Then Exit Sub

        For i As Integer = 0 To LabelPoint範囲選択画像.Count - 1
            LabelPoint範囲選択画像(i).Visible = False
        Next
    End Sub
    Friend Sub SelectRangePoint()

        'If myPicAr.Count = 0 Then
        '    Exit Sub
        'End If

        '    If LabelPoint範囲選択画像.Count <> 0 Then
        '        For Each lp As Label In LabelPoint範囲選択画像
        '            Me.Panel2.Controls.Remove(lp)

        '        Next
        '        LabelPoint範囲選択画像.Clear()

        '    End If

        '    Dim pR As New Label
        '    Dim pb As ExPictureBox = DirectCast(ActExPic, ExPictureBox)
        '    Dim x As Integer = pb.Location.X
        '    Dim y As Integer = pb.Location.Y

        '    ''右
        '    'With pR
        '    '    .Name = "migi"
        '    '    .Text = ""
        '    '    .Size = New Size(4, 4)
        '    '    .BackColor = Color.Red
        '    '    .Cursor = Cursors.SizeWE
        '    '    .Location = New Point(x + pb.Width - 2, y + (pb.Height / 2) - 2)

        '    'End With
        '    'Me.Panel2.Controls.Add(pR)
        '    'AddHandler pR.MouseMove, AddressOf RightMove_MouseMove
        '    'LabelPoint.Add(pR)

        '    ''下
        '    'Dim pD As New Label
        '    'With pD
        '    '    .Text = ""
        '    '    .Size = New Size(4, 4)
        '    '    .BackColor = Color.Red
        '    '    .Cursor = Cursors.SizeNS
        '    '    .Location = New Point(x + (pb.Width / 2) - 2, y + pb.Height - 2)
        '    'End With

        '    'Me.Panel2.Controls.Add(pD)
        '    'LabelPoint.Add(pD)
        '    'AddHandler pD.MouseMove, AddressOf DownMove_MouseMove



        '    '右下
        '    Dim pRD As New Label
        '    With pRD
        '        .Name = "RD"
        '        .Text = ""
        '        .Size = New Size(6, 6)
        '        .BackColor = Color.Red
        '        .Cursor = Cursors.SizeNWSE
        '        .Location = New Point(x + pb.Width - 3, y + pb.Height - 3)
        '        '.BringToFront()
        '    End With
        '    Me.Panel2.Controls.Add(pRD)
        '    LabelPoint範囲選択画像.Add(pRD)
        '    AddHandler pRD.MouseMove, AddressOf DownMove_MouseMove
        '    AddHandler pRD.MouseMove, AddressOf RightMove_MouseMove



        '    For Each lp As Label In LabelPoint範囲選択画像
        '        AddHandler lp.MouseDown, AddressOf RightMove_MouseDown
        '        'AddHandler lp.MouseMove, AddressOf RightMove_MouseMove
        '        lp.BringToFront()
        '    Next


        '    If Me.Panel2.Controls.Contains(selectPicbox範囲選択画像) = False Then
        '        For Each c As ExPictureBox In myPicAr

        '            If c.Name = "範囲選択_T" Then
        '                selectPicbox範囲選択画像 = c
        '                Exit For
        '            End If

        '        Next
        '        'selectPicbox.SizeMode = PictureBoxSizeMode.StretchImage '必要、なくすと縮まなくなる
        '        'widthSelect = selectPicbox.Width
        '        'heightSelect = selectPicbox.Height
        '    End If

    End Sub
    Friend Sub RightMove_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)

        If e.Button = Windows.Forms.MouseButtons.Left Then
            Xp範囲選択画像 = e.X
            Yp範囲選択画像 = e.Y
        End If

    End Sub
    Friend Sub RightMove_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        'Dim X1 As Integer
        'Dim myLabel As Label = DirectCast(sender, Label)
        'Dim rw As Integer = selectPicbox範囲選択画像.Width
        'Dim rh As Integer = selectPicbox範囲選択画像.Height

        'X1 = Xp範囲選択画像 - e.X
        'If e.Button = Windows.Forms.MouseButtons.Left Then

        '    If selectPicbox範囲選択画像.Width - X1 < 3 Then
        '        Exit Sub
        '    End If

        '    myLabel.Left = myLabel.Left - X1
        '    rw -= X1

        '    'selectPicbox.Width -= X1
        '    'Call RectRangeSelectRealTime(rw, rh)
        '    myForm3.NumericUpDownRectRangeH.Value = rw
        '    myForm3.NumericUpDownRectRangeV.Value = rh

        'End If

    End Sub
    Friend Sub DownMove_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        'Dim Y1 As Integer
        'Dim myLabel As Label = DirectCast(sender, Label)
        'Dim rw As Integer = selectPicbox範囲選択画像.Width
        'Dim rh As Integer = selectPicbox範囲選択画像.Height

        'Y1 = Xp範囲選択画像 - e.Y
        'If e.Button = Windows.Forms.MouseButtons.Left Then

        '    If selectPicbox範囲選択画像.Height - Y1 < 3 Then
        '        Exit Sub
        '    End If

        '    myLabel.Top = myLabel.Top - Y1
        '    rh -= Y1

        '    myForm3.NumericUpDownRectRangeH.Value = rw
        '    myForm3.NumericUpDownRectRangeV.Value = rh

        'End If

    End Sub
    '----------------ここまで範囲選択画像表示-------------------


    '画像に影をつける
    Friend Sub ShadowAdd(ByVal sH As Integer, ByVal sV As Integer, ByVal sType As Integer, ByVal shade As Single, ByVal A1 As Integer, ByVal R1 As Integer, ByVal G1 As Integer, ByVal B1 As Integer)
        'sH 影の横位置
        'sV 影の縦位置
        'sType  影の濃淡、Trueなら絶対値、Falseなら相対値
        'shade  影の不透明度
        'A1からB1 ARGB

        If myPicAr.Count = 0 Then
            Exit Sub

        End If
        Dim name As String = ActExPic.Name

        'If FocusPic.Name.EndsWith("_影付き_T") Then
        '    name = FocusPic.Name
        'Else
        '    name = FocusPic.Name & "_影付き"
        'End If


        'LockBits
        Dim bmp As New Bitmap(DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image)
        Dim lockW As Integer = bmp.Width
        Dim lockH As Integer = bmp.Height
        Dim lockRect As New Rectangle(0, 0, lockW, lockH)
        'bmp = New Bitmap(lockW, lockH)
        Dim bmpdata As BitmapData = bmp.LockBits(lockRect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * lockH - 1
        Dim pixels(data) As Byte
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        If sType = 3 Then
            shade = shade / 100
            For x = 0 To lockW - 1
                For y = 0 To lockH - 1
                    Dim pos As Integer = bmpdata.Stride * y + x * 4
                    If pixels(pos + 3) <> 0 Then
                        pixels(pos + 3) = pixels(pos + 3) * shade

                    End If

                Next
            Next
        ElseIf sType = 4 Then
            'shade = shade / 100
            For x = 0 To lockW - 1
                For y = 0 To lockH - 1
                    Dim pos As Integer = bmpdata.Stride * y + x * 4

                    If pixels(pos + 3) <> 0 Then
                        'pixels(pos) = shade
                        'pixels(pos + 1) = shade
                        'pixels(pos + 2) = shade
                        pixels(pos + 3) = shade
                        'pixels(pos + 3) = pixels(pos + 3) * (shade / 100) '透明度は元の半分にしてみた
                    End If

                Next
            Next
        ElseIf sType = 1 Then '影の不透明度相対値
            shade = shade / 100
            For x = 0 To lockW - 1
                For y = 0 To lockH - 1
                    Dim pos As Integer = bmpdata.Stride * y + x * 4

                    If pixels(pos + 3) <> 0 Then
                        pixels(pos) = B1
                        pixels(pos + 1) = G1
                        pixels(pos + 2) = R1
                        pixels(pos + 3) = pixels(pos + 3) * shade
                        'pixels(pos + 3) = pixels(pos + 3) * (shade / 100) '透明度は元の半分にしてみた
                    End If

                Next
            Next
        ElseIf sType = 2 Then            '影の不透明度絶対値
            For x = 0 To lockW - 1
                For y = 0 To lockH - 1
                    Dim pos As Integer = bmpdata.Stride * y + x * 4

                    If pixels(pos + 3) <> 0 Then
                        pixels(pos) = B1
                        pixels(pos + 1) = G1
                        pixels(pos + 2) = R1
                        pixels(pos + 3) = shade
                        'pixels(pos + 3) = pixels(pos + 3) / 2 '透明度は元の半分にしてみた
                    End If

                Next
            Next
        End If


        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)

        Dim canvas As New Bitmap(bmp.Width + Math.Abs(sH), bmp.Height + Math.Abs(sV))
        Dim g As Graphics = Graphics.FromImage(canvas)

        '影の位置調整、影が左側なら影のxは０で画像が右にずれる
        Dim sRight As Integer = 0
        Dim sDown As Integer = 0
        If sH < 0 Then
            sRight = Math.Abs(sH)
            sH = 0
        End If

        If sV < 0 Then
            sDown = Math.Abs(sV)
            sV = 0
        End If

        g.DrawImage(bmp, sH, sV) '影の描画

        '画像の描画
        g.DrawImage(DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox).Image, sRight, sDown)
        'Call PicBoxAdd(name, shadow)
        Call PicBoxAdd(name, canvas)
        g.Dispose()

    End Sub

    '文字の描画のフォームを表示する
    Private Sub ButtonShowFormText_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonShowFormText.Click
        'Form3がすでに開かれていたら何もしない
        For Each f As Form In Me.OwnedForms
            'For Each f As Form In Application.OpenForms
            If f.Name = "FormText" Then
                Exit Sub
            End If
        Next

        'Dim omake As New Form3
        'omake.Show(Me)
        'Dim myForm3 As New Form3 'Form3にアクセスするの


        Me.Cursor = Cursors.WaitCursor
        'Dim myform3 As Form3
        'myForm3 = Me.Owner
        myFormText = New FormText 'これを付けないと一度閉じたサブフォームを開こうとするとリソースがないと言われる
        myFormText.ShowInTaskbar = False 'タスクバーに表示しない

        myFormText.Show(Me)
        Call SquareSample()
        Call TextFileRead() '設定ファイルの読み込み

        'omake.Dispose()
        Me.Cursor = Cursors.Default
    End Sub

    '文字の描画の設定ファイルの読み込み
    Private Sub TextFileRead()
        Try
            Dim iPath As String = My.Application.Info.DirectoryPath 'Pixtack.exeがあるパスの取得
            iPath = iPath & "\Pixtack.txt" '設定ファイルのパス
            Dim iRead As New IO.StreamReader(iPath) 'ファイルの読み込み
            'Dim iRead As New IO.StreamReader("C:\Users\waten\Documents\Visual Studio 2010\Projects\テスト05-2パネル追加\テスト05-2パネル追加\bin\Release\Pixtack.txt", System.Text.Encoding.GetEncoding("Shift-JIS"))
            Dim rgb() As String

            Dim ft As String = "[ButtonTextBackColor1]" '検索文字、文字の背景の色の取得

            Do Until iRead.ReadLine = ft '検索文字まで改行

            Loop

            rgb = iRead.ReadLine.Split(",")
            Dim r As Integer = rgb(0)
            Dim icol As Color = Color.FromArgb(rgb(0), rgb(1), rgb(2))

            myFormText.ButtonTextBackColor1.ForeColor = icol

            iRead.Close()
        Catch ex As Exception

        End Try


    End Sub
    Friend Sub HistogramShow()


        'Form3がすでに開かれていたら何もしない
        For Each f As Form In Me.OwnedForms
            'For Each f As Form In Application.OpenForms
            If f.Name = "FormHistogram" Then
                Exit Sub
            End If
        Next


        Me.Cursor = Cursors.WaitCursor

        myFormHistgram = New FormHistogram 'これを付けないと一度閉じたサブフォームを開こうとするとリソースがないと言われる
        myFormHistgram.ShowInTaskbar = False 'タスクバーに表示しない

        'フォーム表示
        myFormHistgram.Show(Me)

        'ヒストグラム表示更新

        If myPicAr.Count <> 0 Then
            Call Histogram()

        End If


        Me.Cursor = Cursors.Default


    End Sub
    Friend Sub Histogram()
        'Dim Sw As New System.Diagnostics.Stopwatch
        'Sw.Start()

        Me.Cursor = Cursors.WaitCursor

        Dim bmp As New Bitmap(ActExPic.Image)

        'Dim bmp As New Bitmap("F:\png\guriguri1.png")

        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
        Dim bmpdata As System.Drawing.Imaging.BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb)
        Dim ptr As IntPtr = bmpdata.Scan0

        Dim data As Integer = bmpdata.Stride * bmp.Height - 1
        Dim pixels(data) As Byte


        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)

        '別バージョン1、RGBそれぞれで100分率
        'Dim redBmp As Bitmap = HistogramSub(bmp, bmpdata, pixels, 2)
        'Dim greenBmp As Bitmap = HistogramSub(bmp, bmpdata, pixels, 1)
        'Dim blueBmp As Bitmap = HistogramSub(bmp, bmpdata, pixels, 0)
        'Dim yBmp As Bitmap = HistogramSubY(bmp, bmpdata, pixels)

        'Dim canvas As New Bitmap(256, 400)
        'Dim g As Graphics = Graphics.FromImage(canvas)
        'g.DrawImage(redBmp, 0, 0)
        'g.DrawImage(greenBmp, 0, 100)
        'g.DrawImage(blueBmp, 0, 200)
        'g.DrawImage(yBmp, 0, 300)


        '別バージョン2、RGB全体の中で100分率
        'Dim canvas As New Bitmap(256, 400)
        'Dim g As Graphics = Graphics.FromImage(canvas)
        'Dim histBmp As Bitmap = HistogramSub2(bmp, bmpdata, pixels)
        'Dim yBmp As Bitmap = HistogramSubY(bmp, bmpdata, pixels)
        'g.DrawImage(histBmp, 0, 0)
        'g.DrawImage(yBmp, 0, 300)


        '別バージョン3、LockBitsバージョン、RGBそれぞれで100分率
        'Dim redBmp As Bitmap = HistogramSubLockBits(bmp, bmpdata, pixels, 2)
        'Dim greenBmp As Bitmap = HistogramSubLockBits(bmp, bmpdata, pixels, 1)
        'Dim blueBmp As Bitmap = HistogramSubLockBits(bmp, bmpdata, pixels, 0)
        'Dim yBmp As Bitmap = HistogramSubY(bmp, bmpdata, pixels)
        'Dim canvas As New Bitmap(256, 400)
        'Dim g As Graphics = Graphics.FromImage(canvas)
        'g.DrawImage(redBmp, 0, 0)
        'g.DrawImage(greenBmp, 0, 100)
        'g.DrawImage(blueBmp, 0, 200)
        'g.DrawImage(yBmp, 0, 300)


        '別バージョン3、LockBitsバージョン、RGBそれぞれで100分率
        'Dim RGBbmp As Bitmap = HistogramSubLockBitsRGB(bmp, bmpdata, pixels)
        'Dim yBmp As Bitmap = HistogramSubY(bmp, bmpdata, pixels)
        'Dim canvas As New Bitmap(256, 400)
        'Dim g As Graphics = Graphics.FromImage(canvas)
        'g.DrawImage(RGBbmp, 0, 0)
        'g.DrawImage(yBmp, 0, 300)


        '別バージョン4、LockBitsバージョン、RGBそれぞれで100分率,間引き
        Dim RGBbmp As Bitmap = HistogramSubLockBitsRGB2(bmp, bmpdata, pixels)
        Dim yBmp As Bitmap = HistogramSubY(bmp, bmpdata, pixels)
        Dim canvas As New Bitmap(256, 400)
        Dim g As Graphics = Graphics.FromImage(canvas)
        g.DrawImage(RGBbmp, 0, 0)
        g.DrawImage(yBmp, 0, 300)



        myFormHistgram.PictureBoxHistogram.Image = canvas

        'bmpのアンロック
        bmp.UnlockBits(bmpdata)
        Me.Cursor = Cursors.Default
        'Sw.Stop()
        'Debug.WriteLine("処理時間 " & Sw.Elapsed.ToString & "  処理ピクセル数 " & bmp.Width * bmp.Height)

        'PicBoxAdd("histogram", canvas)
    End Sub
    Friend Function HistogramSub(ByVal bmp As Bitmap, ByVal bmpdata As BitmapData, ByVal pixels() As Byte, ByVal col As Integer) As Bitmap
        'colは０なら青、１は緑、２は赤

        Dim data(255) As Single
        Dim val As Integer

        For x = 0 To bmp.Width - 1
            For y = 0 To bmp.Height - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4

                If pixels(pos + 3) <> 0 Then
                    val = pixels(pos + col)
                    data(val) = data(val) + 1

                End If

                'val = pixels(pos + col)
                'data(val) = data(val) + 1
            Next

        Next

        '最大値
        Dim cMax As Integer = 0
        For i = 0 To 255
            If cMax < data(i) Then
                cMax = data(i)

            End If
        Next

        '100分率
        For i = 0 To 255
            data(i) = (data(i) / cMax) * 100

        Next

        '塗りつぶし
        Dim histBmp As New Bitmap(256, 300)

        'Histogram塗りグラデーション
        Select Case col
            Case 2 '赤
                For x As Integer = 0 To 255
                    For y As Integer = 0 To 99

                        If data(x) > 99 - y Then
                            histBmp.SetPixel(x, y, Color.FromArgb(255, x, 0, 0))

                        End If

                    Next
                Next
            Case 1 '緑
                For x As Integer = 0 To 255
                    For y As Integer = 0 To 99

                        If data(x) > 99 - y Then
                            histBmp.SetPixel(x, y, Color.FromArgb(255, 0, x, 0))

                        End If

                    Next
                Next
            Case 0 '青
                For x As Integer = 0 To 255
                    For y As Integer = 0 To 99

                        If data(x) > 99 - y Then
                            histBmp.SetPixel(x, y, Color.FromArgb(255, 0, 0, x))

                        End If

                    Next
                Next
        End Select


        '単色塗り
        'Dim fill As Color
        'Select Case col
        '    Case 2
        '        fill = Color.Red
        '    Case 1
        '        fill = Color.Green
        '    Case 0
        '        fill = Color.Blue

        'End Select

        ''Histogram塗り
        'For x As Integer = 0 To 255
        '    For y As Integer = 0 To 99

        '        If data(x) > 99 - y Then
        '            histBmp.SetPixel(x, y, fill)

        '        End If

        '    Next
        'Next
        Return histBmp

    End Function


    Friend Function HistogramSub2(ByVal bmp As Bitmap, ByVal bmpdata As BitmapData, ByVal pixels() As Byte) As Bitmap
        '100分率をそれぞれの色ではなく3色全体の中で最多を１００にしてみた
        '返すBitmapの大きさは256x300

        '0から255のカウントを配列に入れる
        Dim dataR(255) As Single
        Dim valR As Integer
        Dim dataG(255) As Single
        Dim valG As Integer
        Dim dataB(255) As Single
        Dim valB As Integer

        '色のカウント
        For x = 0 To bmp.Width - 1
            For y = 0 To bmp.Height - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4

                If pixels(pos + 3) <> 0 Then
                    valR = pixels(pos + 2)
                    dataR(valR) = dataR(valR) + 1
                    valG = pixels(pos + 1)
                    dataG(valG) += 1
                    valB = pixels(pos)
                    dataB(valB) += 1

                End If
            Next
        Next

        '最大値
        Dim cMax As Integer = 0
        Dim tMax As Integer = 0

        For i = 0 To 255
            tMax = Math.Max(Math.Max(dataR(i), dataG(i)), dataB(i))

            If cMax < tMax Then
                cMax = tMax

            End If
        Next

        '100分率
        For i = 0 To 255
            dataR(i) = (dataR(i) / cMax) * 100
            dataG(i) = (dataG(i) / cMax) * 100
            dataB(i) = (dataB(i) / cMax) * 100
        Next

        Dim histBmp As New Bitmap(256, 300)

        '塗りつぶしの色
        'Histogram塗りグラデーション

        '赤
        For x As Integer = 0 To 255
            For y As Integer = 0 To 99

                If dataR(x) > 99 - y Then
                    histBmp.SetPixel(x, y, Color.FromArgb(255, x, 0, 0))

                End If

            Next
        Next
        '緑
        For x As Integer = 0 To 255
            For y As Integer = 100 To 199

                If dataG(x) > 199 - y Then
                    histBmp.SetPixel(x, y, Color.FromArgb(255, 0, x, 0))

                End If

            Next
        Next
        '青
        For x As Integer = 0 To 255
            For y As Integer = 200 To 299

                If dataB(x) > 299 - y Then
                    histBmp.SetPixel(x, y, Color.FromArgb(255, 0, 0, x))

                End If

            Next
        Next




        Return histBmp

    End Function
    Friend Function HistogramSubY(ByVal bmp As Bitmap, ByVal bmpdata As BitmapData, ByVal pixels() As Byte) As Bitmap
        'Y(輝度)のヒストグラム

        Dim yData(255) As Single
        Dim val As Integer
        Dim mabiki As Integer = 2 'カウント間引き、間引かないときは1にする

        For x = 0 To bmp.Width - 1 Step mabiki
            For y = 0 To bmp.Height - 1 Step mabiki
                Dim pos As Integer = bmpdata.Stride * y + x * 4

                If pixels(pos + 3) <> 0 Then '完全透明はカウントしない
                    val = (pixels(pos + 2) * 0.299) + (pixels(pos + 1) * 0.587) + (pixels(pos) * 0.114)
                    yData(val) = yData(val) + 1

                End If

            Next

        Next

        '最大個数
        Dim yMax As Integer = 0
        For i = 0 To 255
            If yMax < yData(i) Then
                yMax = yData(i)

            End If
        Next

        '100分率
        For i = 0 To 255
            yData(i) = (yData(i) / yMax) * 100

        Next

        '塗りつぶし
        Dim yBmp As New Bitmap(256, 100)

        'Histogram塗り
        For x As Integer = 0 To 255
            For y As Integer = 0 To 99

                If yData(x) > 99 - y Then
                    yBmp.SetPixel(x, y, Color.Black)

                End If

            Next
        Next

        Return yBmp

    End Function
    Friend Function HistogramSubLockBits(ByVal bmp As Bitmap, ByVal bmpdata As BitmapData, ByVal pixels() As Byte, ByVal col As Integer) As Bitmap
        'colは０なら青、１は緑、２は赤


        Dim data(255) As Single
        Dim val As Integer

        For x = 0 To bmp.Width - 1
            For y = 0 To bmp.Height - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4

                If pixels(pos + 3) <> 0 Then
                    val = pixels(pos + col)
                    data(val) = data(val) + 1

                End If

            Next

        Next

        '最大値
        Dim cMax As Integer = 0
        For i = 0 To 255
            If cMax < data(i) Then
                cMax = data(i)

            End If
        Next

        '100分率
        For i = 0 To 255
            data(i) = (data(i) / cMax) * 100

        Next

        '塗りつぶし
        Dim histBmp As New Bitmap(256, 100)
        Dim hRect As New Rectangle(0, 0, histBmp.Width, histBmp.Height)
        Dim hBmpdata As BitmapData = histBmp.LockBits(hRect, ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb)
        Dim hPtr As IntPtr = hBmpdata.Scan0
        Dim hData As Integer = hBmpdata.Stride * hBmpdata.Height - 1
        Dim hPixels(hData) As Byte

        'ヒストグラム画像の左上から右へ順番に見ていく
        '現在の縦の位置より100分率の数値が小さければ色をつける
        For j = 0 To 99
            For i = 0 To 255
                If data(i) > 99 - j Then
                    'hPixels(i * 4 + col + (j * hBmpdata.Stride)) = 255 'RGB原色で塗り
                    hPixels(i * 4 + col + (j * hBmpdata.Stride)) = i 'グラデーション塗り
                    hPixels(i * 4 + 3 + (j * hBmpdata.Stride)) = 255 '透明度は必ず不透明255

                End If

            Next
        Next

        System.Runtime.InteropServices.Marshal.Copy(hPixels, 0, hPtr, hPixels.Length)

        'ロック解除
        histBmp.UnlockBits(hBmpdata)


        Return histBmp

    End Function

    Friend Function HistogramSubLockBitsRGB(ByVal bmp As Bitmap, ByVal bmpdata As BitmapData, ByVal pixels() As Byte) As Bitmap
        'rgbまとめてカウントしてまとめてヒストグラム作成、1.5倍くらい速くなった


        Dim rdata(255) As Single
        Dim gdata(255) As Single
        Dim bdata(255) As Single

        Dim val As Integer

        For x = 0 To bmp.Width - 1
            For y = 0 To bmp.Height - 1
                Dim pos As Integer = bmpdata.Stride * y + x * 4

                If pixels(pos + 3) <> 0 Then
                    val = pixels(pos + 0)
                    bdata(val) = bdata(val) + 1
                    val = pixels(pos + 1)
                    gdata(val) = gdata(val) + 1
                    val = pixels(pos + 2)
                    rdata(val) += 1

                End If

            Next

        Next

        '最大値
        'Dim cMax As Integer = 0
        'cMax = Math.Max(Math.Max(ArrayMax(rdata), ArrayMax(gdata)), ArrayMax(bdata))
        ''100分率
        'For i = 0 To 255
        '    rdata(i) = (rdata(i) / cMax) * 100
        '    gdata(i) = (gdata(i) / cMax) * 100
        '    bdata(i) = (bdata(i) / cMax) * 100
        'Next

        'RGBそれぞれの最大値
        Dim rMax As Integer = ArrayMax(rdata)
        Dim gMax As Integer = ArrayMax(gdata)
        Dim bMax As Integer = ArrayMax(bdata)
        'RGBそれぞれの100分率
        For i = 0 To 255
            rdata(i) = (rdata(i) / rMax) * 100
            gdata(i) = (gdata(i) / gMax) * 100
            bdata(i) = (bdata(i) / bMax) * 100
        Next


        '塗りつぶし
        Dim histBmp As New Bitmap(256, 300)
        Dim hRect As New Rectangle(0, 0, histBmp.Width, histBmp.Height)
        Dim hBmpdata As BitmapData = histBmp.LockBits(hRect, ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb)
        Dim hPtr As IntPtr = hBmpdata.Scan0
        Dim hData As Integer = hBmpdata.Stride * hBmpdata.Height - 1
        Dim hPixels(hData) As Byte

        'ヒストグラム画像の左上から右へ順番に見ていく
        '現在の縦の位置より100分率の数値が小さければ色をつける
        For j = 0 To 99
            For i = 0 To 255
                If rdata(i) > 99 - j Then
                    'hPixels(i * 4 + col + (j * hBmpdata.Stride)) = 255 'RGB原色で塗り
                    hPixels(i * 4 + 2 + (j * hBmpdata.Stride)) = i 'グラデーション塗り
                    hPixels(i * 4 + 3 + (j * hBmpdata.Stride)) = 255 '透明度は必ず不透明255

                End If

            Next
        Next

        For j = 100 To 199
            For i = 0 To 255
                If gdata(i) > 199 - j Then
                    'hPixels(i * 4 + col + (j * hBmpdata.Stride)) = 255 'RGB原色で塗り
                    hPixels(i * 4 + 1 + (j * hBmpdata.Stride)) = i 'グラデーション塗り
                    hPixels(i * 4 + 3 + (j * hBmpdata.Stride)) = 255 '透明度は必ず不透明255

                End If

            Next
        Next
        For j = 20 To 299
            For i = 0 To 255
                If bdata(i) > 299 - j Then
                    'hPixels(i * 4 + col + (j * hBmpdata.Stride)) = 255 'RGB原色で塗り
                    hPixels(i * 4 + 0 + (j * hBmpdata.Stride)) = i 'グラデーション塗り
                    hPixels(i * 4 + 3 + (j * hBmpdata.Stride)) = 255 '透明度は必ず不透明255

                End If

            Next
        Next


        System.Runtime.InteropServices.Marshal.Copy(hPixels, 0, hPtr, hPixels.Length)

        'ロック解除
        histBmp.UnlockBits(hBmpdata)


        Return histBmp

    End Function

    Friend Function ArrayMax(ByVal data) As Integer
        '配列の中の最大値を返す
        Dim iMax As Integer
        For i = 0 To UBound(data)

            If iMax < data(i) Then
                iMax = data(i)
            End If

        Next
        Return iMax
    End Function

    Friend Function HistogramSubLockBitsRGB2(ByVal bmp As Bitmap, ByVal bmpdata As BitmapData, ByVal pixels() As Byte) As Bitmap
        'rgbまとめてカウントしてまとめてヒストグラム作成、1.5倍くらい速くなった
        '間引き
        Dim mabiki As Integer = 2 '間引く間隔
        Dim rdata(255) As Single
        Dim gdata(255) As Single
        Dim bdata(255) As Single

        Dim val As Integer

        For x = 0 To bmp.Width - 1 Step mabiki
            For y = 0 To bmp.Height - 1 Step mabiki
                Dim pos As Integer = bmpdata.Stride * y + x * 4

                If pixels(pos + 3) <> 0 Then
                    val = pixels(pos + 0)
                    bdata(val) = bdata(val) + 1
                    val = pixels(pos + 1)
                    gdata(val) = gdata(val) + 1
                    val = pixels(pos + 2)
                    rdata(val) += 1

                End If

            Next

        Next

        '最大値
        'Dim cMax As Integer = 0
        'cMax = Math.Max(Math.Max(ArrayMax(rdata), ArrayMax(gdata)), ArrayMax(bdata))
        ''100分率
        'For i = 0 To 255
        '    rdata(i) = (rdata(i) / cMax) * 100
        '    gdata(i) = (gdata(i) / cMax) * 100
        '    bdata(i) = (bdata(i) / cMax) * 100
        'Next

        'RGBそれぞれの最大値
        Dim rMax As Integer = ArrayMax(rdata)
        Dim gMax As Integer = ArrayMax(gdata)
        Dim bMax As Integer = ArrayMax(bdata)
        'RGBそれぞれの100分率
        For i = 0 To 255
            rdata(i) = (rdata(i) / rMax) * 100
            gdata(i) = (gdata(i) / gMax) * 100
            bdata(i) = (bdata(i) / bMax) * 100
        Next


        '塗りつぶし
        Dim histBmp As New Bitmap(256, 300)
        Dim hRect As New Rectangle(0, 0, histBmp.Width, histBmp.Height)
        Dim hBmpdata As BitmapData = histBmp.LockBits(hRect, ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb)
        Dim hPtr As IntPtr = hBmpdata.Scan0
        Dim hData As Integer = hBmpdata.Stride * hBmpdata.Height - 1
        Dim hPixels(hData) As Byte

        'ヒストグラム画像の左上から右へ順番に見ていく
        '現在の縦の位置より100分率の数値が小さければ色をつける
        For j = 0 To 99
            For i = 0 To 255
                If rdata(i) > 99 - j Then
                    'hPixels(i * 4 + col + (j * hBmpdata.Stride)) = 255 'RGB原色で塗り
                    hPixels(i * 4 + 2 + (j * hBmpdata.Stride)) = i 'グラデーション塗り
                    hPixels(i * 4 + 3 + (j * hBmpdata.Stride)) = 255 '透明度は必ず不透明255

                End If

            Next
        Next

        For j = 100 To 199
            For i = 0 To 255
                If gdata(i) > 199 - j Then
                    'hPixels(i * 4 + col + (j * hBmpdata.Stride)) = 255 'RGB原色で塗り
                    hPixels(i * 4 + 1 + (j * hBmpdata.Stride)) = i 'グラデーション塗り
                    hPixels(i * 4 + 3 + (j * hBmpdata.Stride)) = 255 '透明度は必ず不透明255

                End If

            Next
        Next
        For j = 20 To 299
            For i = 0 To 255
                If bdata(i) > 299 - j Then
                    'hPixels(i * 4 + col + (j * hBmpdata.Stride)) = 255 'RGB原色で塗り
                    hPixels(i * 4 + 0 + (j * hBmpdata.Stride)) = i 'グラデーション塗り
                    hPixels(i * 4 + 3 + (j * hBmpdata.Stride)) = 255 '透明度は必ず不透明255

                End If

            Next
        Next


        System.Runtime.InteropServices.Marshal.Copy(hPixels, 0, hPtr, hPixels.Length)

        'ロック解除
        histBmp.UnlockBits(hBmpdata)


        Return histBmp

    End Function

    Friend Sub HistogramSubYZ()
        'Y(輝度)のヒストグラムの伸張

        Dim bmp As New Bitmap(ActExPic.Image)

        'Dim bmp As New Bitmap("F:\png\guriguri1.png")

        Dim rect As New Rectangle(0, 0, bmp.Width, bmp.Height)
        Dim bmpdata As System.Drawing.Imaging.BitmapData = bmp.LockBits(rect, ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb)
        Dim ptr As IntPtr = bmpdata.Scan0

        Dim data As Integer = bmpdata.Stride * bmp.Height - 1
        Dim pixels(data) As Byte


        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)


        Dim yData(255) As Single
        Dim val As Integer
        Dim mabiki As Integer = 1 'カウント間引き、間引かないときは1にする

        For x = 0 To bmp.Width - 1 Step mabiki
            For y = 0 To bmp.Height - 1 Step mabiki
                Dim pos As Integer = bmpdata.Stride * y + x * 4

                If pixels(pos + 3) <> 0 Then '完全透明はカウントしない
                    val = (pixels(pos + 2) * 0.299) + (pixels(pos + 1) * 0.587) + (pixels(pos) * 0.114)
                    yData(val) = yData(val) + 1

                End If

            Next

        Next

        Dim y2Max As Integer = 0
        Dim y2Min As Integer = 0

        For i = 0 To 255
            If yData(i) > 0 Then
                y2Min = i
                Exit For
            End If
        Next

        For i = 255 To 0 Step -1
            If yData(i) > 0 Then
                y2Max = i
                Exit For
            End If
        Next

        'Dim Z As Integer
        For x = 0 To bmp.Width - 1 Step mabiki
            For y = 0 To bmp.Height - 1 Step mabiki
                Dim pos As Integer = bmpdata.Stride * y + x * 4

                If pixels(pos + 3) <> 0 Then '完全透明はカウントしない
                    val = (pixels(pos + 2) * 0.299) + (pixels(pos + 1) * 0.587) + (pixels(pos) * 0.114)
                    yData(val) = yData(val) + 1

                    If 0 <= val And val < y2Min Then
                        val = 0
                    ElseIf y2Min <= val <= y2Max Then
                        val = 255 * ((val - y2Min) / (y2Max - y2Min))
                    ElseIf 0 < val <= 255 Then
                        val = 255

                    End If

                    pixels(pos) = val
                    pixels(pos + 1) = val
                    pixels(pos + 2) = val
                    pixels(pos + 3) = pixels(pos + 3)
                End If

            Next

        Next



        Dim bmp2 As New Bitmap(bmp.Width, bmp.Height)
        Dim rect2 As New Rectangle(0, 0, bmp2.Width, bmp2.Height)
        Dim bmpdata2 As BitmapData = bmp2.LockBits(rect2, ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb)
        Dim ptr2 As Integer = bmpdata2.Scan0
        Dim data2 As Integer = bmpdata.Stride * bmp.Height - 1
        Dim pixels2(data2) As Byte
        'System.Runtime.InteropServices.Marshal.Copy(pixels2, 0, ptr2, pixels2.Length)
        System.Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)
        bmp2.UnlockBits(bmpdata2)

        Dim p As Integer = ActExPic.Tag - 1

        myPicArClone(p).image = bmp

        ActExPic.Image = bmp



    End Sub

    '選択画像だけ保存(自他透過)
    Private Sub ToolStripMenuItemSaveT_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemSaveT.Click
        If myPicAr.Count = 0 Then '画像が一個もなければ何もしない
            Return
        End If

        If SaveFile.ShowDialog() = Windows.Forms.DialogResult.OK Then

            Dim bmp As New Bitmap(ActExPic.Image)
            Call PicToFileSave(bmp, SaveFile.FilterIndex)
            bmp.Dispose()

        End If
    End Sub
    '選択画像だけ保存(自透過)
    Private Sub ToolStripMenuItemSave_Click_1(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemSave.Click
        If myPicAr.Count = 0 Then '画像が一個もなければ何もしない
            Return
        End If

        If SaveFile.ShowDialog() = Windows.Forms.DialogResult.OK Then

            Dim SavePic As ExPictureBox = DirectCast(myPicArClone(ActExPic.Tag - 1), ExPictureBox) '保存用の画像の作成
            Dim bmp As New Bitmap(SavePic.Image)
            Call PicToFileSave(bmp, SaveFile.FilterIndex)
        End If


    End Sub

    '画像として保存
    Public Sub PicToFileSave(bmp As Bitmap, SaveFileFilterIndex As Integer)
        'ソフトウェア名記入
        Dim SoftName As String = "Pixtack Azisai" '最後に空白を入れないと文字化けすることがある、なんで？あと日本語も文字化けする
        Dim Asm As System.Reflection.Assembly = System.Reflection.Assembly.GetExecutingAssembly()
        Dim ResourceBmp As New Bitmap(Asm.GetManifestResourceStream("Pixtack.ゆっくりメテオさん520x450_Exif付き.png"))
        Dim iList() As Integer = ResourceBmp.PropertyIdList
        Dim iIndex As Integer = Array.IndexOf(iList, 305)
        Dim iProp As PropertyItem = ResourceBmp.PropertyItems(iIndex)
        iProp.Value = System.Text.Encoding.ASCII.GetBytes(SoftName + ControlChars.NullChar) 'これだと日本語が文字化け、日本語は使えない
        '        テキスト変換・フォーマット変換 - Programming/.NET Framework - 総武ソフトウェア推進所
        'http://smdn.jp/programming/netfx/text_format_conversion/

        'Dim e As System.Text.Encoding = System.Text.Encoding.GetEncoding("shift_jis") 'ShiftJISなら日本語でも大丈夫…じゃなかったたまに文字化けする
        'iProp.Value = e.GetBytes(SoftName)
        'iProp.Value = System.Text.Encoding.Unicode.GetBytes(SoftName)'Unicodeでは文字化けになる
        iProp.Len = iProp.Value.Length
        bmp.SetPropertyItem(iProp)


        '画像フォーマットを選んで保存
        If SaveFileFilterIndex = 1 Then 'フィルタインデックスが1ならpng形式で保存
            bmp = OnePixelTransparent(bmp) 'ツイッターに投稿する画像用
            bmp.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Png)
        ElseIf SaveFileFilterIndex = 2 Then 'Jpeg
            Dim eps As EncoderParameters = New EncoderParameters(1) '1は格納できるオブジェクトの数らしい…
            Dim JQuality As Long = Me.NumericUpDownJpeg.Value
            Dim encParam As New EncoderParameter(Encoder.Quality, JQuality)
            Dim encParams As New EncoderParameters(1)
            encParams.Param(0) = encParam
            bmp.Save(SaveFile.FileName, GetEncoderInfo("image/jpeg"), encParams)

        ElseIf SaveFileFilterIndex = 3 Then 'Bitmap
            bmp.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Bmp)
        ElseIf SaveFileFilterIndex = 4 Then
            bmp.Save(SaveFile.FileName, System.Drawing.Imaging.ImageFormat.Gif)

        End If
    End Sub
    '画像の左上1ピクセルのAlphaが255ならAlphaを254にする
    Function OnePixelTransparent(bmp As Bitmap) As Bitmap
        'ツイッターに投稿する画像用、左上1ピクセルのAlphaを254にする
        '透過ピクセルがあると投稿時にpng画像がjpegに変換されない
        If CheckBox_SaveForTwitter.Checked Then
            Dim c As Color
            c = bmp.GetPixel(0, 0)
            If c.A = 255 Then
                c = Color.FromArgb(254, c)
                bmp.SetPixel(0, 0, c)
                c = bmp.GetPixel(0, 0)
            End If
        End If
        Return bmp
    End Function
    Friend Overloads Sub ore加工した画像を各アレイリストに書き込む(EXP As ExPictureBox, bmp As Bitmap)
        Dim i As Long = EXP.Tag - 1
        DirectCast(myPicAr(i), ExPictureBox).Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp
        DirectCast(myPicArBackup(i), ExPictureBox).Image = bmp
        'Call Transparent4() '編集後はこっち、これだとマウスクリックで線の描画終了時には足りなかった
        Call Transparent2()
        'Call TransparentForMove3() '編集後はこれじゃない、透過処理はいっぱい作りすぎてどれがいいのかわからない

        Call UpdateThumbnailOne()
    End Sub
    'Friend Overloads Sub ore加工した画像を各アレイリストに書き込む(bmp As Bitmap)

    '    Dim exp As ExPictureBox = ActExPic
    '    Call ore加工した画像を各アレイリストに書き込む(exp, bmp)
    'End Sub
  

    Friend Function LineCapFromラインキャップ作成(cName As ExPictureBox.PenCap) As Drawing2D.LineCap
        Dim lc As Drawing2D.LineCap
        Select Case True
            Case cName = ExPictureBox.PenCap.平坦
                lc = LineCap.Flat
            Case cName = ExPictureBox.PenCap.矢印
                lc = LineCap.ArrowAnchor
            Case cName = ExPictureBox.PenCap.丸
                lc = LineCap.Round
            Case cName = ExPictureBox.PenCap.四角
                lc = LineCap.Square
            Case cName = ExPictureBox.PenCap.三角
                lc = LineCap.Triangle
            Case cName = ExPictureBox.PenCap.アンカーマスク
                lc = LineCap.AnchorMask
            Case cName = ExPictureBox.PenCap.丸アンカー
                lc = LineCap.RoundAnchor
            Case cName = ExPictureBox.PenCap.四角アンカー
                lc = LineCap.SquareAnchor
            Case cName = ExPictureBox.PenCap.ダイヤアンカー
                lc = LineCap.DiamondAnchor
            Case cName = ExPictureBox.PenCap.カスタム
                lc = LineCap.Custom
                'Case cName = ExPictureBox.PenCap.矢印アンカー
                '    lc = LineCap.ArrowAnchor
        End Select
        Return lc
    End Function
    Private Sub メモリの開放()
        '.NET アプリケーションのパフォーマンスとスケーラビリティの向上 - 第 5 章 「マネージ コ ード パフォーマンスの向上」
        'http://msdn.microsoft.com/ja-jp/library/ms998547.aspx

        ' Re[13]: 『「GC.Collect」メソッド』について
        'http://bbs.wankuma.com/index.cgi?mode=al2&namber=35188&KLOG=61

        System.GC.Collect() 'メモリの開放、かなり効果があるけどいいのかな？
        GC.WaitForPendingFinalizers() 'これと
        GC.Collect() 'ここでもう一回、こうした方がいいらしい

        'もしくはこのセットも有り？
        'System.GC.Collect() 'メモリの開放、かなり効果があるけどいいのかな？
        'System.Threading.Thread.Sleep(1) '処理にウェイトをかける、動きがカクカクするようになる

    End Sub
    Friend Sub ReConstructタグに従ってコレクションの再構成()
        'タグに従ってコレクションの再構成、並べ替え
        Dim allAr() As ArrayList = {myPicAr, myPicArClone, myPicArBackup}
        Dim j, k As Integer
        For j = 0 To UBound(allAr)
            Dim tempAr As New ArrayList

            For k = 1 To allAr(j).Count
                For Each p As ExPictureBox In allAr(j)
                    If p.Tag = k Then
                        tempAr.Add(p)
                        Exit For

                    End If
                Next p
            Next k

            'allAr(j).Clear()
            allAr(j) = tempAr.Clone 'これだけだと元のコレクションに反映されない、なんで？

        Next j
        '反映されないので改めて指定
        myPicAr = allAr(0).Clone
        myPicArClone = allAr(1).Clone
        myPicArBackup = allAr(2).Clone

    End Sub



    '------------------------ここから図形2用--------------------------------



    Friend Sub ToOriginLayerExPictureを元のレイヤーに戻す()

        If isDrawEditNow = False Then Exit Sub
        If EditNowPic.Tag <> 1 Then Exit Sub

        'If isLayerChange = True Then Exit Sub '編集中にレイヤー変更していたら何もしないで終了

        If myPicAr.Count <> 0 Then
            Dim allAr() As ArrayList = {myPicAr, myPicArClone, myPicArBackup}
            Dim i As Integer
            'ExPictureのタグを変更→コレクションを再作成

            'タグの変更、元のタグの数値に戻す
            For i = 0 To UBound(allAr)
                For Each p As ExPictureBox In allAr(i)
                    If p.Tag = 1 Then
                        p.Tag = LayerExPic
                    ElseIf p.Tag <= LayerExPic Then
                        p.Tag = p.Tag - 1

                    End If
                Next
            Next

            'タグに従ってコレクションの再作成
            Call ReConstructタグに従ってコレクションの再構成()

            Call SortPic() 'ExPictureのZオーダー指定

            Me.NumNowPic.Value = EditNowPic.Tag
            'Call UpdateThumbnail()


        End If


    End Sub
    Friend Sub ToFront指定のExPictureを最前面にする(ex As ExPictureBox)
        If isDrawEditNow = False Then Exit Sub
        If ex.Tag = 1 Then
            LayerExPic = 1
            Exit Sub
        End If


        LayerExPic = ex.Tag '元の階層を記憶
        isLayerChange = False '編集中にレイヤー変更の有無

        Dim allAr() As ArrayList = {myPicAr, myPicArClone, myPicArBackup}
        Dim i As Integer


        'ExPictureのタグを変更→コレクションを再作成

        'tagの調整、自分より上の画像を1づつ下げて自分は最前面にする
        For i = 0 To UBound(allAr)
            For Each p As ExPictureBox In allAr(i)
                If p.Tag < LayerExPic Then
                    p.Tag = p.Tag + 1
                ElseIf p.Tag = LayerExPic Then
                    p.Tag = 1

                End If
            Next
        Next

        'タグに従ってコレクションの再作成
        Call ReConstructタグに従ってコレクションの再構成()

        Call SortPic() 'ExPictureのZオーダー指定

        Me.NumNowPic.Value = EditNowPic.Tag
        Call UpdateThumbnail()


    End Sub
    Private Sub CloneExPic図形の設定の複製(OriExp As ExPictureBox, NewExp As ExPictureBox)
        '参照型のものは一度変数に入れてから指定しないとコピー元とコピー先が同期してしまう
        Dim pp As New List(Of PointF)(OriExp.PathPoints)
        Dim pen1 As Pen
        Dim b1 As Brush
        'Dim col As Color

        With OriExp
            b1 = New SolidBrush(.ExPenColor)
            pen1 = New Pen(b1, .ExPenWidth)


        End With
        With NewExp
            .IsEdit = OriExp.IsEdit
            .PathPoints = pp 'OriExp.PathPoints
            .ExPen = pen1 ' OriExp.ExPen
            .ExPenStartCap = OriExp.ExPenStartCap
            .ExPenEndCap = OriExp.ExPenEndCap
            .ExPenWidth = OriExp.ExPenWidth
            .ExPenColor = OriExp.ExPenColor
            .ExSolidBrush = b1 'OriExp.ExSolidBrush
            .ExLineJoin = OriExp.ExLineJoin
            .ShadowColor = OriExp.ShadowColor

            .GraphicDrawType = OriExp.GraphicDrawType
            .CurveTension = OriExp.CurveTension
            .ExFillType = OriExp.ExFillType
            .CloseLine = OriExp.CloseLine
            .isFill = OriExp.isFill
            .isAntiAlias = OriExp.isAntiAlias
            .isGrid = OriExp.isGrid
            .isShadow = OriExp.isShadow
            'ここまで図形2

            '文字の描画関係
            .DrawString描画文字 = OriExp.DrawString描画文字 '改行を含む、描画している文字

        End With
    End Sub
    Private Sub ExPicBoxSetting図形の設定書き込み(Exp As ExPictureBox, Optional PathPoint As Boolean = False) 'As ExPictureBox
        '使っている場所はPicBoxAddだけ


        Call CloseEdit編集終了()
        'Call MouseEndDrawマウスで描画終了処理()

        Dim pPoint As New Generic.List(Of PointF) '({New PointF(50, 50), New PointF(150, 50), New PointF(150, 150), New PointF(50, 150)})

        If PathPoint = False Then
            pPoint = New List(Of PointF)({New PointF(50, 50), New PointF(150, 50), New PointF(150, 150), New PointF(50, 150)})
        End If
        
        Dim P1 = New Pen(myForm3.PictureBoxPenの色.BackColor, myForm3.NumericUpDownPathPenの太さ.Value)



        With Exp
            .IsEdit = True
            .PathPoints = pPoint
            .ExPen = P1
            .ExPenStartCap = myForm3.ComboBoxStartCap.SelectedIndex
            .ExPenEndCap = myForm3.ComboBoxEndCap.SelectedIndex
            .ExPenWidth = myForm3.NumericUpDownPathPenの太さ.Value
            .ExPenColor = myForm3.PictureBoxPenの色.BackColor
            .ExSolidBrush = New SolidBrush(myForm3.PictureBoxPenの色.BackColor) '単色ブラシ
            .ExLineJoin = myForm3.ComboBoxPenLineJoin角の形状.SelectedIndex '角の形状
            .ShadowColor = myForm3.PictureBoxShape2影の色.BackColor '影の色

            .GraphicDrawType = myForm3.ComboBoxLineType線の種類.SelectedIndex
            .CurveTension = myForm3.NumericUpDown曲線のテンション.Value
            .ExFillType = myForm3.ComboBoxFillMode塗りモード.SelectedIndex
            .CloseLine = myForm3.CheckBoxLineClose線を閉じる.Checked
            .isFill = myForm3.CheckBoxFill塗りつぶす.Checked
            .isAntiAlias = myForm3.CheckBoxAntiAliasアンチエイリアス.Checked
            .isGrid = myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked
            .isShadow = myForm3.CheckBoxShape2Shadow図形2影.Checked



        End With

        Dim bmp As Bitmap
        If PathPoint = False Then
            bmp = New Bitmap(200, 200)
            bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp, True)
        Else
            'マウスで描画中は
            bmp = New Bitmap(Exp.Width, Exp.Height)
            Dim g As Graphics = Graphics.FromImage(bmp)
            g.DrawRectangle(Pens.Green, 0, 0, bmp.Width - 1, bmp.Height - 1)
            g.Dispose()

        End If
        Exp.Image = bmp


        'Return Exp

    End Sub
    Private Sub ExPicBoxSetting図形の設定書き込み2(Exp As ExPictureBox) 'As ExPictureBox'未使用
        '使っている場所はPicBoxAddだけ

        Call CloseEdit編集終了()
        Call MouseEndDrawマウスで描画終了処理()

        Dim pPoint As New Generic.List(Of PointF)({New PointF(50, 50), New PointF(150, 50), New PointF(150, 150), New PointF(50, 150)})
        Dim P1 = New Pen(myForm3.PictureBoxPenの色.BackColor, myForm3.NumericUpDownPathPenの太さ.Value)



        With Exp
            .IsEdit = True
            .PathPoints = pPoint
            .ExPen = P1
            .ExPenStartCap = myForm3.ComboBoxStartCap.SelectedIndex
            .ExPenEndCap = myForm3.ComboBoxEndCap.SelectedIndex
            .ExPenWidth = myForm3.NumericUpDownPathPenの太さ.Value
            .ExPenColor = myForm3.PictureBoxPenの色.BackColor
            .ExSolidBrush = New SolidBrush(myForm3.PictureBoxPenの色.BackColor) '単色ブラシ
            .ExLineJoin = myForm3.ComboBoxPenLineJoin角の形状.SelectedIndex '角の形状

            .GraphicDrawType = myForm3.ComboBoxLineType線の種類.SelectedIndex
            .CurveTension = myForm3.NumericUpDown曲線のテンション.Value
            .ExFillType = myForm3.ComboBoxFillMode塗りモード.SelectedIndex
            .CloseLine = myForm3.CheckBoxLineClose線を閉じる.Checked
            .isFill = myForm3.CheckBoxFill塗りつぶす.Checked
            .isAntiAlias = myForm3.CheckBoxAntiAliasアンチエイリアス.Checked

        End With

        Dim bmp As New Bitmap(200, 200)
        bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp, True)
        Exp.Image = bmp

        'Return Exp

    End Sub
    Friend Sub Shape2Add図形2追加その2()
        Call CloseEdit編集終了()
        'Call MouseEndDrawマウスで描画終了処理()

        Dim bmp As New Bitmap(200, 200)
        'ExPictureBox追加！！！
        Call PicBoxAdd(myForm3.ComboBoxLineType線の種類.SelectedItem, bmp, True)

    End Sub
  

    Friend Function oreDraw図形2描画ドラッグ用() As Bitmap

        Dim pt() As PointF = DirectCast(EditNowPic.PathPoints.ToArray, PointF())
        '画像の大きさ設定、頂点から
        Dim i As Integer
        'Dim Mマージン As Integer = EditNowPic.DrawMargin
        'Dim Mオフセット As New Point(Mマージン, Mマージン)
        'For i = 0 To UBound(pt)
        '    pt(i).Offset(Mオフセット)
        'Next

        'Dim xMin, yMin As Integer '頂点座標xyそれぞれの最小値、マイナスなら？
        'xMin = pt(0).X
        'yMin = pt(0).Y
        'For i = 1 To UBound(pt)
        '    If pt(i).X < xMin Then
        '        xMin = pt(i).X
        '    End If
        '    If pt(i).Y < yMin Then
        '        yMin = pt(i).Y
        '    End If
        'Next

        ''座標のオフセット
        'Dim offX As Integer = 0 - xMin
        'Dim offY As Integer = 0 - yMin

        'For i = 0 To UBound(pt)
        '    pt(i).X += offX
        '    pt(i).Y += offY
        'Next


        'ExPicture表示位置のオフセット


        'Dim offsetP As New Point(0, 0)
        'Dim exP As Point = EditNowPic.Location
        'If xMin < 0 And yMin < 0 Then
        '    offsetP = New Point(Math.Abs(xMin), Math.Abs(yMin))
        '    exP.Offset(xMin, yMin)

        'ElseIf xMin < 0 Then
        '    offsetP = New Point(Math.Abs(xMin), 0)
        '    exP.Offset(xMin, 0)
        'ElseIf yMin < 0 Then
        '    offsetP = New Point(0, Math.Abs(yMin))
        '    exP.Offset(0, yMin)

        'End If

        'EditNowPic.Location = exP


        'For i = 0 To UBound(pt)
        '    'pt(i).Offset(offsetP)
        '    If xMin < 0 Or yMin < 0 Then
        '        pt(i).Offset(offsetP)

        '        'pt(i).X += Math.Abs(xMin)
        '        'pt(i).Y += Math.Abs(yMin)

        '    End If

        'Next


        Dim xMax, yMax As Integer '頂点座標xyそれぞれの最大値、画像の大きさになる
        xMax = pt(0).X
        yMax = pt(0).Y
        For i = 1 To UBound(pt)
            If pt(i).X > xMax Then
                xMax = pt(i).X
            End If
            If pt(i).Y > yMax Then
                yMax = pt(i).Y
            End If
        Next


        Dim bmp As Bitmap
        'bmp = New Bitmap(xMax - xMin + 1, yMax - yMin + 1) 'これが正解！！Bitmapは1から数える、Pointは0から数えるからその差が＋１
        bmp = New Bitmap(xMax + 1, yMax + 1)

        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim P1 As Pen = ActExPic.ExPen
        Dim tN As Single = EditNowPic.CurveTension
        'Dim DrawMargin As Integer = EditNowPic.DrawMargin

        Select Case ActExPic.GraphicDrawType
            Case ExPictureBox.DrawType.DrawBeziers

            Case ExPictureBox.DrawType.DrawClosedCurve

            Case ExPictureBox.DrawType.DrawCurve
                g.DrawCurve(P1, pt, tN)
            Case ExPictureBox.DrawType.DrawEllipse

            Case ExPictureBox.DrawType.DrawLines
                g.DrawLines(P1, pt)
            Case ExPictureBox.DrawType.DrawPath

            Case ExPictureBox.DrawType.DrawPolygon

            Case ExPictureBox.DrawType.DrawRectangle

            Case ExPictureBox.DrawType.FillClosedCurve

            Case ExPictureBox.DrawType.FillEllipse

            Case ExPictureBox.DrawType.FillPath

            Case ExPictureBox.DrawType.FillPolygon

            Case ExPictureBox.DrawType.FillRectangle

        End Select
        g.Dispose()

        Return bmp


    End Function
    Friend Overloads Function DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp As ExPictureBox,
                                                            PP() As PointF,
                                                Optional EditEnd As Boolean = False) As Bitmap
        Dim pSize As New Size(Exp.Width, Exp.Height)
        Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp, pSize, PP, EditEnd)
        Return bmp

    End Function

    Friend Overloads Function DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp As ExPictureBox, pSize As Size,
                                                Optional EditEnd As Boolean = False) As Bitmap
        Dim PP() As PointF = DirectCast(Exp.PathPoints.ToArray, PointF())
        Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp, pSize, PP, EditEnd)
        Return bmp

    End Function
    Friend Overloads Function DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp As ExPictureBox,
                                              Optional EditEnd As Boolean = False) As Bitmap
        Dim PP() As PointF = DirectCast(Exp.PathPoints.ToArray, PointF())
        Dim pSize As New Size(Exp.Width, Exp.Height)
        Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp, pSize, PP, EditEnd)
        Return bmp

    End Function

    Friend Overloads Function DrawShape2図形2描画ドラッグ用サイズ変更なし版(Exp As ExPictureBox, pSize As Size,
                                                           PathPoint() As PointF,
                                              Optional EditEnd As Boolean = False) As Bitmap

        'Dim pt() As PointF = DirectCast(Exp.PathPoints.ToArray, PointF())
        Dim pt() As PointF
        'If PathPoint.Length >= 2 Then
        '    pt = PathPoint
        'Else
        '    pt = DirectCast(Exp.PathPoints.ToArray, PointF())
        'End If
        pt = PathPoint

        '影のPathPoint作成
        Dim shadowLocate As Integer = 10 '影の位置
        Dim sPP() As PointF = pt.Clone '影のパスポイント
        Dim offs As New Point(shadowLocate, shadowLocate) '影のオフセット距離
        For i = 0 To UBound(sPP)
            sPP(i) += offs
        Next
        Dim gp2 As New GraphicsPath '影用


        '画像の大きさ設定、ExPictureから
        Dim bmp As New Bitmap(pSize.Width, pSize.Height)


        Dim g As Graphics = Graphics.FromImage(bmp)
        If Exp.isAntiAlias Then
            g.SmoothingMode = SmoothingMode.AntiAlias
        Else
            g.SmoothingMode = SmoothingMode.Default
        End If

        g.PixelOffsetMode = PixelOffsetMode.Half 'これはないほうがいいみたい、やっぱりあったほうがいい、ないとアンチエイリアスの時直線でぼやける
        '        g.TransformPoints(CoordinateSpace.World,,

        Dim gp As New GraphicsPath


        'Dim ooo As New AdjustableArrowCap(2, 4)



        'g.Flush(FlushIntention.Flush) 'グラフィックス スタックのコマンドをすぐに終了 (フラッシュ) するか、またはできる限り早く実行するかを指定します。


        Dim P1 As Pen = Exp.ExPen
        'P1.Width = EditNowPic.ExPenWidth
        'P1.CustomEndCap = ooo
        '角の形、線と線の繋ぎ目の形、初期値はMiterかな
        'Bevelは斜めでMiterのMiterLimit＝0の時と同じ
        'MiterLimitの初期値は10
        'P1.LineJoin = LineJoin.Miter
        'P1.MiterLimit = 10

        Dim tN As Single = Exp.CurveTension
        'Dim DrawMargin As Integer = EditNowPic.DrawMargin
        Dim fm As Drawing2D.FillMode = Exp.ExFillType ' myForm3.ComboBoxFillMode塗りモード.SelectedIndex

        Select Case Exp.GraphicDrawType
            Case ExPictureBox.DrawType.直線


                If (Exp.CloseLine Or Exp.isFill) And pt.Length > 2 Then
                    'g.DrawPolygon(P1, pt) '閉じた直線
                    gp.AddPolygon(pt)
                    gp2.AddPolygon(sPP)
                Else
                    'g.DrawLines(P1, pt) '開いた直線
                    gp.AddLines(pt)
                    gp2.AddLines(sPP)
                    'g.DrawPath(P1, gp) '開いた直線

                End If

            Case ExPictureBox.DrawType.曲線
                If Exp.isFill And pt.Length > 2 Then
                    'g.FillClosedCurve(Exp.ExSolidBrush, pt, fm, tN)
                    gp.AddClosedCurve(pt, tN)
                    gp2.AddClosedCurve(sPP, tN)
                    Exit Select
                End If

                If Exp.CloseLine And pt.Length >= 3 Then '閉じた曲線は頂点数が2以下だとエラーになる
                    'g.DrawClosedCurve(P1, pt, tN, fm)
                    gp.AddClosedCurve(pt, tN)
                    gp2.AddClosedCurve(sPP, tN)
                Else
                    'g.DrawCurve(P1, pt, tN)
                    gp.AddCurve(pt, tN)
                    gp2.AddCurve(sPP, tN)
                    'g.DrawPath(P1, gp)

                End If

            Case ExPictureBox.DrawType.ベジェ曲線
                If pt.Length >= 4 And pt.Length Mod 3 = 1 Then
                    'g.DrawBeziers(P1, pt)

                    '閉じたベジェ曲線はDrawBeziersでは描けないっぽいので
                    'GrahicsPathを使ってDrawPathで描く
                    gp.AddBeziers(pt)
                    gp2.AddBeziers(sPP)

                    '線を閉じる
                    If Exp.CloseLine Then
                        'gp.CloseAllFigures()
                        gp.CloseFigure() '違いがわからん
                        gp2.CloseFigure()

                    End If

                End If


            Case ExPictureBox.DrawType.四角枠
                Dim rect As Rectangle = Exp.ClientRectangle
                rect.Inflate(New Size(-shadowLocate, -shadowLocate))
                rect.Offset(New Point(-(shadowLocate / 2), -(shadowLocate / 2)))

                Dim rect2 As New Rectangle
                rect2 = rect
                rect2.Offset(New Point(shadowLocate, shadowLocate))


                gp.AddRectangle(rect)
                gp2.AddRectangle(rect2)

            Case ExPictureBox.DrawType.DrawBeziers

            Case ExPictureBox.DrawType.DrawClosedCurve

            Case ExPictureBox.DrawType.DrawCurve
                g.DrawCurve(P1, pt, tN)
            Case ExPictureBox.DrawType.DrawEllipse

            Case ExPictureBox.DrawType.DrawLines
                g.DrawLines(P1, pt)
            Case ExPictureBox.DrawType.DrawPath

            Case ExPictureBox.DrawType.DrawPolygon

            Case ExPictureBox.DrawType.DrawRectangle

            Case ExPictureBox.DrawType.FillClosedCurve

            Case ExPictureBox.DrawType.FillEllipse

            Case ExPictureBox.DrawType.FillPath

            Case ExPictureBox.DrawType.FillPolygon

            Case ExPictureBox.DrawType.FillRectangle

        End Select


        '描画
        If Exp.isShadow Then
            '影あり
            Dim sPen As Pen = P1.Clone
            sPen.Color = Color.FromArgb(128, Exp.ShadowColor)
            If Exp.isFill Then
                Dim sb As New SolidBrush(Color.FromArgb(128, Exp.ShadowColor))
                gp2.FillMode = fm
                g.FillPath(sb, gp2)
                gp.FillMode = fm
                g.FillPath(Exp.ExSolidBrush, gp)

            Else

                g.DrawPath(sPen, gp2)
                g.DrawPath(P1, gp)

            End If
            sPen.Dispose()

        Else
            '影なし
            If Exp.isFill Then
                gp.FillMode = fm
                g.FillPath(Exp.ExSolidBrush, gp)

            Else
                g.DrawPath(P1, gp)

            End If

        End If





        'ベジェ曲線の補助線、編集終了時は描画しない
        If EditEnd = False And Exp.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
            Dim p2 As New Pen(Brushes.DarkRed, 1)
            p2.DashStyle = DashStyle.Dot '点線
            g.DrawLine(Pens.White, pt(0), pt(1)) '白を塗って
            g.DrawLine(p2, pt(0), pt(1)) '赤の点線で上書き

            For i As Integer = 1 To pt.Length - 2
                If (i - 1) Mod 3 <> 0 Then
                    g.DrawLine(Pens.White, pt(i), pt(i + 1))
                    g.DrawLine(p2, pt(i), pt(i + 1))
                End If
            Next
        End If

        gp.Dispose()
        gp2.Dispose()
        g.Dispose()
        Return bmp



    End Function
    Friend Function DrawShape2図形2描画ドラッグ用サイズ変更なし版区間指定版(Exp As ExPictureBox, offset As Integer, segment As Integer) As Bitmap
        'CPU負荷が低くなるかと思ったけどそうでもないみたい、なので使っていない
        Dim pt() As PointF = DirectCast(Exp.PathPoints.ToArray, PointF())
        'Dim pt() As Point = DirectCast(PP.ToArray, Point())
        '画像の大きさ設定、ExPictureから


        Dim bmp = New Bitmap(Exp.Width, Exp.Height)
        Dim g As Graphics = Graphics.FromImage(bmp)
        g.SmoothingMode = SmoothingMode.AntiAlias
        g.PixelOffsetMode = PixelOffsetMode.Half
        Dim gp As New GraphicsPath
        'gp.AddCurve(pt)
        'gp.AddBeziers(pt)

        'g.DrawPath(Pens.Red, gp)

        'Dim ooo As New AdjustableArrowCap(2, 4)



        'g.Flush(FlushIntention.Flush) 'グラフィックス スタックのコマンドをすぐに終了 (フラッシュ) するか、またはできる限り早く実行するかを指定します。


        Dim P1 As Pen = Exp.ExPen
        'P1.Width = EditNowPic.ExPenWidth
        'P1.CustomEndCap = ooo
        'P1.LineJoin = LineJoin.Bevel


        Dim tN As Single = Exp.CurveTension
        'Dim DrawMargin As Integer = EditNowPic.DrawMargin
        Dim fm As Drawing2D.FillMode = Exp.ExFillType ' myForm3.ComboBoxFillMode塗りモード.SelectedIndex

        Select Case Exp.GraphicDrawType
            Case ExPictureBox.DrawType.直線
                If Exp.isFill Then
                    g.FillPolygon(Exp.ExSolidBrush, pt, fm)
                    Exit Select
                End If

                If Exp.CloseLine Then
                    g.DrawPolygon(P1, pt) '閉じた直線
                Else
                    g.DrawLines(P1, pt) '開いた直線

                End If

            Case ExPictureBox.DrawType.曲線
                If Exp.isFill Then
                    g.FillClosedCurve(Exp.ExSolidBrush, pt, fm, tN)
                    Exit Select
                End If

                If Exp.CloseLine And pt.Length >= 3 Then '閉じた曲線は頂点数が2以下だとエラーになる
                    g.DrawClosedCurve(P1, pt, tN, fm)

                Else
                    'g.DrawCurve(P1, pt, tN)
                    g.DrawCurve(P1, pt, offset, segment, tN)
                End If

            Case ExPictureBox.DrawType.ベジェ曲線
                If pt.Length >= 4 And pt.Length Mod 3 = 1 Then
                    g.DrawBeziers(P1, pt)
                    ''補助線
                    'g.DrawLine(Pens.DarkRed, pt(0), pt(1))
                    'For i As Integer = 1 To pt.Length - 2
                    '    If (i - 1) Mod 3 <> 0 Then
                    '        g.DrawLine(Pens.DarkRed, pt(i), pt(i + 1))

                    '    End If
                    'Next
                End If

                'Case ExPictureBox.DrawType.閉じた曲線

            Case ExPictureBox.DrawType.DrawBeziers

            Case ExPictureBox.DrawType.DrawClosedCurve

            Case ExPictureBox.DrawType.DrawCurve
                g.DrawCurve(P1, pt, tN)
            Case ExPictureBox.DrawType.DrawEllipse

            Case ExPictureBox.DrawType.DrawLines
                g.DrawLines(P1, pt)
            Case ExPictureBox.DrawType.DrawPath

            Case ExPictureBox.DrawType.DrawPolygon

            Case ExPictureBox.DrawType.DrawRectangle

            Case ExPictureBox.DrawType.FillClosedCurve

            Case ExPictureBox.DrawType.FillEllipse

            Case ExPictureBox.DrawType.FillPath

            Case ExPictureBox.DrawType.FillPolygon

            Case ExPictureBox.DrawType.FillRectangle

        End Select

        g.Dispose()
        Return bmp



    End Function
    Friend Function oreDraw図形2描画ドラッグ用2背景付き描画テスト用() As Bitmap

        Dim pt() As PointF = DirectCast(ActExPic.PathPoints.ToArray, PointF())
        '画像の大きさ設定、頂点から
        'Dim i As Integer


        'Dim xMax, yMax As Integer '頂点座標xyそれぞれの最大値、画像の大きさになる
        'xMax = pt(0).X
        'yMax = pt(0).Y
        'For i = 1 To UBound(pt)
        '    If pt(i).X > xMax Then
        '        xMax = pt(i).X
        '    End If
        '    If pt(i).Y > yMax Then
        '        yMax = pt(i).Y
        '    End If
        'Next


        Dim bmp As Bitmap
        ''bmp = New Bitmap(xMax - xMin + 1, yMax - yMin + 1) 'これが正解！！Bitmapは1から数える、Pointは0から数えるからその差が＋１
        'bmp = New Bitmap(xMax + 1, yMax + 1)
        ''bmp = New Bitmap(xMax, yMax)
        bmp = New Bitmap(EditNowPic.Image.Width, EditNowPic.Image.Height)


        'Dim bmp As New Bitmap(ActExPic.Width, ActExPic.Height)
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim P1 As Pen = ActExPic.ExPen

        Select Case ActExPic.GraphicDrawType
            Case ExPictureBox.DrawType.DrawBeziers

            Case ExPictureBox.DrawType.DrawClosedCurve

            Case ExPictureBox.DrawType.DrawCurve

            Case ExPictureBox.DrawType.DrawEllipse

            Case ExPictureBox.DrawType.DrawLines
                g.DrawLines(P1, pt)
            Case ExPictureBox.DrawType.DrawPath

            Case ExPictureBox.DrawType.DrawPolygon

            Case ExPictureBox.DrawType.DrawRectangle

            Case ExPictureBox.DrawType.FillClosedCurve

            Case ExPictureBox.DrawType.FillEllipse

            Case ExPictureBox.DrawType.FillPath

            Case ExPictureBox.DrawType.FillPolygon

            Case ExPictureBox.DrawType.FillRectangle

        End Select
        g.Dispose()

        Return bmp


    End Function
    Friend Sub 頂点の初期化(exp) '編集開始2
        If myPicAr.Count = 0 Then Exit Sub
        'If ActExPic Is Nothing Then Exit Sub
        'If ActExPic.IsEdit = False Then Exit Sub
        If exp.IsEdit = False Then Exit Sub
        If isDrawEditNow = True Then Exit Sub

        isDrawEditNow = True '編集中フラグ
        If isMouseDeDrawNow Then 'マウスで描画中ならマウス描画を終了
            Call MouseEndDrawマウスで描画終了処理()
        End If
        'EditNowPic = ActExPic
        EditNowPic = exp
        EditNowPic.ContextMenuStrip = Me.ContextMenuStrip画像編集用 '右クリックメニューの切り替え
        'EditNowPic.BorderStyle = BorderStyle.FixedSingle 'ExPictureに枠を表示←これはキケン、無限に枠が広がっていく、なんで？
        '→たぶん枠を表示した分ExPictureの縦横が1づつ増えたことになってそれを元に新たに大きさを指定しているからループになっている


        '枠の表示
        Call PicBoderLineLabel画像に枠を作成表示(EditNowPic)


        '背景テスト
        Dim BGPic As New Bitmap(BGImage背景画像作成ExPictureBox用(EditNowPic))
        With EditNowPic
            .BackgroundImage = BGPic
            .BackgroundImageLayout = ImageLayout.None
            .SizeMode = PictureBoxSizeMode.Normal '一時的に変更
            '.Size = New Size(BGPic.Width, BGPic.Height)

            '.Location = New Point(0, 0)

        End With

        '頂点ラベルの作成
        Call LabelAdd_頂点ラベルの全部作成(exp)

        'サイズ変更用ラベル作成表示
        Call ExPicture大きさ変更用ラベル表示()

        'Form3に書き込み
        With myForm3
            .NumericUpDown曲線のテンション.Value = EditNowPic.CurveTension

            .NumericUpDownPathPenの太さ.Value = EditNowPic.ExPenWidth
            .PictureBoxPenの色.BackColor = EditNowPic.ExPenColor
            .ComboBoxLineType線の種類.SelectedIndex = EditNowPic.GraphicDrawType

            .ComboBoxEndCap.SelectedIndex = EditNowPic.ExPenEndCap
            .ComboBoxStartCap.SelectedIndex = EditNowPic.ExPenStartCap
            .ComboBoxFillMode塗りモード.SelectedIndex = EditNowPic.FillMode
            .ComboBoxPenLineJoin角の形状.SelectedIndex = EditNowPic.ExLineJoin
            .PictureBoxShape2影の色.BackColor = EditNowPic.ShadowColor
            .CheckBoxShape2Shadow図形2影.Checked = EditNowPic.isShadow

            .CheckBoxLineClose線を閉じる.Checked = EditNowPic.CloseLine
            .CheckBoxFill塗りつぶす.Checked = EditNowPic.isFill
            .CheckBoxAntiAliasアンチエイリアス.Checked = EditNowPic.isAntiAlias
            .CheckBoxGridFitShape図形をグリッドに合わせる.Checked = EditNowPic.isGrid
            .CheckBoxGridFitShape図形をグリッドに合わせる.Enabled = True
        End With


        'スクロール調整用のダミー画像を作成、panel2の一番右下に置く、編集が終わったら消す
        dummyExPicBox = New ExPictureBox
        Dim sp As New Point(EditNowPic.Size)
        Dim LabelSize As New Point(LABEL_PIC_SIZE, LABEL_PIC_SIZE)
        sp.Offset(LabelSize) 'サイズ変更用のラベルの大きさ分を足す
        With dummyExPicBox
            .Location = EditNowPic.Location
            .Size = sp
            .Name = "ExDummy"
            '.BackColor=Color.Transparent
        End With
        Me.Panel2.Controls.Add(dummyExPicBox)
        dummyExPicBox.SendToBack()


        ''変更後にスクロールバーを移動させる→廃止、いろいろずれる元になる
        ''Me.Panel2.Controls("RD").Select()'これだとスクロールバーが移動しない
        'Me.Panel2.Controls("RD").Focus() '正解はこっち、Focusの方が強力なのかな、MSはSelect推奨していたけど…
        'Me.ListBox1.Select()

        '編集中のExPictureを一時的に最前面にする、終わったら元の位置に戻す
        '編集中にレイヤー変更した時は終わっても元に戻さないでそのまま
        Call ToFront指定のExPictureを最前面にする(EditNowPic)

        '画像作成、描画
        Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(EditNowPic)
        EditNowPic.Image = bmp
        Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)


    End Sub
    Private Sub LabelMoveOne_頂点ラベルを個別に移動(L As Label)
        'ラベルの位置更新、ドラッグしている一個だけ           
        Dim np As New Point(EditNowPic.PathPoints(L.Tag).X, EditNowPic.PathPoints(L.Tag).Y) ' Point(EditNowPic.PathPoints(L.Tag))
        np -= PPLabelOffset
        L.Location = np '移動

        '表示情報の更新
        If myForm3.CheckBoxVisiblePointNumber頂点番号表示.Checked Or myForm3.CheckBoxVisiblePointLocate頂点座標表示.Checked Then
            Call VisiblePointNumber頂点番号表示テキスト更新だけ(L)
        End If


        'ベジェ曲線で連動移動なら制御点の位置更新
        If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then

        End If
    End Sub
    Friend Sub LabelAdd_頂点ラベルの全部作成(exp As ExPictureBox)
        '使っている場所→頂点の初期化、背景付き描画テスト、頂点移動をグリッドに合わせる、頂点の追加、削除時

        ActExPic.Controls.Clear() 'すべての頂点を削除
        For Each L As Label In PPL描画頂点リスト
            EditNowPic.Controls.Remove(L)
            L.Dispose()
        Next
        PPL描画頂点リスト.Clear()

        'ラベルの大きさ選択
        Dim labelSize As Integer
        If isLabelSizeSmall Then
            labelSize = LABEL_SIZE_S
        Else
            labelSize = LABEL_SIZE
        End If

        PPLabelOffset = New Size((labelSize - 1) / 2, (labelSize - 1) / 2) 'ラベル表示位置に調整

        Dim ps() As PointF = DirectCast(EditNowPic.PathPoints.ToArray, PointF())
        Dim newLocate As New Point
        Dim iLabel As Label
        Dim i As Long
        Dim myCursor As Cursor
        myCursor = New Cursor("E:\オレ\アイコン\マウスカーソル\白黒点3.cur")
        'oc = New Cursor("E:\オレ\アイコン\マウスカーソル\矢印枠4.cur")


        For i = 0 To UBound(ps)
            'newLocate = New PointF(ps(i)) - PPLabelOffset 
            newLocate = New Point(ps(i).X, ps(i).Y) - PPLabelOffset 'ラベルの位置オフセット、Cintを付けたほうがいいかな

            iLabel = New Label

            With iLabel
                .Width = labelSize ' LABEL_SIZE
                .Height = labelSize ' LABEL_SIZE
                .Tag = i
                .Name = i
                .Location = newLocate
                .Cursor = myCursor
                .ContextMenuStrip = ContextMenuStrip画像編集用 '右クリックメニューの追加

                '番号表示用
                Call VisiblePointState頂点番号と座標表示(iLabel)

            End With

            EditNowPic.Controls.Add(iLabel)


            AddHandler iLabel.MouseDown, AddressOf MouseDownラベルドラッグ用
            AddHandler iLabel.MouseMove, AddressOf MouseMove頂点ラベル
            AddHandler iLabel.MouseUp, AddressOf MouseUp頂点用

            PPL描画頂点リスト.Add(iLabel) 'ラベルをリストに追加

        Next

    End Sub

    Friend Sub 頂点ラベルの再作成(exp As ExPictureBox)
        If isDrawEditNow = False Then Exit Sub
        '頂点ラベルの再作成
        Call LabelAdd_頂点ラベルの全部作成(exp)

    End Sub
    Friend Sub 背景付き描画テスト() '編集開始3
        'If ActExPic Is Nothing Then Exit Sub
        'If ActExPic.IsEdit = False Then Exit Sub
        ''→たぶん枠を表示した分ExPictureの縦横が1づつ増えたことになってそれを元に新たに大きさを指定しているからループになっている
        'isDrawEditNow = True '編集中フラグ
        'Dim pts As New Generic.List(Of Point)(ActExPic.PathPoints)
        'Dim P1 As Pen = ActExPic.ExPen



        ''Dim canvas As New Bitmap(RigthDownPoint.X, RigthDownPoint.Y)
        'Dim canvas As New Bitmap(Me.Panel2.Width, Me.Panel2.Height)
        'Dim g As Graphics = Graphics.FromImage(canvas)

        'For Each c As ExPictureBox In myPicArR '背景画像
        '    '自分自身は除く
        '    If c.Tag <> EditNowPic.Tag Then
        '        g.DrawImage(c.Image, AbsolutePoint(c))
        '    End If
        'Next

        ''Call PicBoxAdd("編集用ExPicture", canvas, True)


        'EditNowPic = ActExPic
        'EditNowPic.ContextMenuStrip = Me.ContextMenuStrip画像編集用 '右クリックメニューの切り替え
        'EditNowPic.BorderStyle = BorderStyle.FixedSingle 'ExPictureに枠を表示←これはキケン、無限に枠が広がっていく、なんで？


        'With EditNowPic
        '    .BackgroundImage = canvas
        '    .BackgroundImageLayout = ImageLayout.None
        '    .Size = New Size(canvas.Width, canvas.Height)
        '    .SizeMode = PictureBoxSizeMode.Normal '一時的に変更

        '    .Location = New Point(0, 0)

        'End With


        'Call LabelAdd_頂点ラベルの全部作成(exp)

    End Sub


    Friend Sub CloseEdit編集終了()
        If isDrawEditNow <> True Then Exit Sub
        If PPL描画頂点リスト.Count = 0 Then Exit Sub



        'グリッドの設定書き込み
        'EditNowPic.isGrid = myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked
        'myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Enabled = False
        With myForm3.CheckBoxGridFitShape図形をグリッドに合わせる
            EditNowPic.isGrid = .Checked
            .Enabled = False
            .Checked = False

        End With
        '最終描画
        If Not PastActExPic Is Nothing Then
            Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(PastActExPic, True)
            PastActExPic.Image = bmp
            'Call ore加工した画像を各アレイリストに書き込む(bmp)


            Dim i As Long = PastActExPic.Tag - 1
            DirectCast(myPicAr(i), ExPictureBox).Image = bmp
            DirectCast(myPicArClone(i), ExPictureBox).Image = bmp
            DirectCast(myPicArBackup(i), ExPictureBox).Image = bmp
            ''Call Transparent4() '編集後はこっち

            'Call UpdateThumbnailOne()
            PastActExPic = Nothing
        ElseIf myPicAr.Count <> 0 Then

            Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(EditNowPic, True)
            EditNowPic.Image = bmp
            Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)

        End If

        '編集中の画像があれば編集終了
        With EditNowPic
            .ContextMenuStrip = Me.ContextMenuStrip1 '右クリックメニューの切り替え
            .Controls.Clear() '頂点の消去
            .BorderStyle = BorderStyle.None 'ExPictureの枠を非表示に戻す
            .SizeMode = PictureBoxSizeMode.AutoSize '表示モードを元に戻す

            .BackgroundImage = Nothing '背景画像消去
            .BackgroundImageLayout = ImageLayout.None '背景画像の位置、消すから無意味かも、初期値はtile

        End With

        '描画頂点用のラベルの削除
        For Each L As Label In PPL描画頂点リスト
            EditNowPic.Controls.Remove(L)
            L.Dispose()
        Next
        PPL描画頂点リスト.Clear() 'ラベルのリストを空にする

        'スクロールバー固定用ダミー画像を消去
        Me.Panel2.Controls.Remove(dummyExPicBox)
        dummyExPicBox.Dispose()

        '大きさ変更用のラベル消去
        Call ExPicture大きさ変更用ラベルと枠を消去()

        '画像の階層を元に戻す
        Call ToOriginLayerExPictureを元のレイヤーに戻す()


        isDrawEditNow = False '編集中フラグ

        'Call Transparent4() 'これだと足りない、選択画像に重なっていないものが無視される
        Call Transparent2() '全体の再表示、これでOK

        EditNowPic = Nothing


    End Sub
    '頂点の右クリックメニューとか

    Private Sub MouseDownラベルドラッグ用(sender As Label, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then
            isLabelMoveNow = True '頂点ラベル移動中フラグ
            ActLabel = sender 'どのラベルを右クリックしたのか記憶
            '左クリックの時
            sLPoint = e.Location
            XClickPP = e.X 'クリック初期位置記憶
            YClickPP = e.Y

            ActExPic = sender.Parent 'ActExPicにラベルを所有する画像を指定

            'グリッドに移動、座標がグリッドに合っていなければ
            'If myForm3.CheckBoxDrawPointGridFit.Checked OrElse My.Computer.Keyboard.AltKeyDown Then
            If myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked OrElse My.Computer.Keyboard.AltKeyDown Then
                Dim gv As Integer = NumericUpDownGrid.Value
                'Dim PP() As PointF = {New PointF(EditNowPic.PathPoints.Item(sender.Tag))}
                Dim PP As New List(Of PointF)(EditNowPic.PathPoints)

                If PP(0).X Mod gv <> 0 OrElse PP(0).Y Mod gv <> 0 Then

                    Call Points2NearGrid頂点を一番近いグリッドに移動(PP, sender.Tag)

                End If
            End If

        ElseIf e.Button = Windows.Forms.MouseButtons.Right Then
            '右クリックの時
            '頂点の削除の項目の有効無効の切り替え
            Dim DT As ExPictureBox.DrawType = EditNowPic.GraphicDrawType
            Dim LC As Integer = PPL描画頂点リスト.Count
            Select Case True
                Case DT = ExPictureBox.DrawType.ベジェ曲線
                    'ベジェ曲線の制御点も削除不可にする、制御点はtagの数値が1,2,4,5,7,8で3で割り切れない数字
                    If LC >= 6 And sender.Tag Mod 3 = 0 Then
                        ToolStripMenuItem頂点の削除.Enabled = True
                    Else
                        ToolStripMenuItem頂点の削除.Enabled = False
                    End If
                Case DT = ExPictureBox.DrawType.直線
                    If LC >= 4 Then
                        ToolStripMenuItem頂点の削除.Enabled = True
                    ElseIf LC = 3 And EditNowPic.CloseLine = False Then
                        '頂点数が3でも開いた直線なら有効に
                        ToolStripMenuItem頂点の削除.Enabled = True
                    Else
                        ToolStripMenuItem頂点の削除.Enabled = False
                    End If
                Case DT = ExPictureBox.DrawType.曲線
                    If LC >= 4 Then
                        ToolStripMenuItem頂点の削除.Enabled = True
                    Else
                        ToolStripMenuItem頂点の削除.Enabled = False
                    End If
            End Select



            ToolStripMenuItem頂点の追加.Enabled = False
            ActLabel = sender 'どのラベルを右クリックしたのか記憶
        End If
    End Sub

    Private Sub MouseMove頂点ラベル(sender As Label, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left And isLabelMoveNow Then

            'メモリの開放
            MemoryFreeCount += 1
            If MemoryFreeCount = MEMORY_FREE_LIMIT Then
                Call メモリの開放()
                MemoryFreeCount = 0
            End If


            XPPM頂点移動距離 = e.X - XClickPP
            YPPM頂点移動距離 = e.Y - YClickPP
            '線の描画用の座標
            Call 頂点座標の計算(sender)

            '画像作成
            Dim bmp As Bitmap
            bmp = Me.DrawShape2図形2描画ドラッグ用サイズ変更なし版(EditNowPic)
            'bmp = Me.oreDraw図形2描画ドラッグ用サイズ変更なし版区間指定版(EditNowPic, 1, 1)


            'ラベルの情報表示更新
            If myForm3.CheckBoxVisiblePointLocate頂点座標表示.Checked Or myForm3.CheckBoxVisiblePointNumber頂点番号表示.Checked Then
                Call VisiblePointNumber頂点番号表示テキスト更新だけ(sender)
            End If

            '画像表示はどちらにするか
            ActExPic.Image = bmp '透過処理されない、軽い

        End If

    End Sub
    Private Sub MouseUp頂点用(sender As Label, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then
            isLabelMoveNow = False '頂点ラベル移動中フラグ

            Me.ListBox1.Select() 'ラベルのActiveを外す
            Dim bmp As Bitmap
            bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(EditNowPic)
            Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)
            Call 編集画像の背景更新()

        Else
            'Me.Select()
            Me.ListBox1.Select() 'ラベルのActiveを外す
            Dim bmp As Bitmap
            bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(EditNowPic)
            Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)
            Call 編集画像の背景更新()
        End If
    End Sub
    Private Sub 頂点座標の計算(L As Label)
        'ドラッグしている頂点だけの計算
        Dim pMove As New Point(XPPM頂点移動距離, YPPM頂点移動距離)

        Dim LTag As Integer = L.Tag
        'Dim p As New PointF(EditNowPic.PathPoints.Item(LTag)) 'これだとエラーになる
        Dim p = New PointF(EditNowPic.PathPoints(LTag).X, EditNowPic.PathPoints(LTag).Y)

        'グリッド、これもスクロールバー関係無かった…
        'グリッド移動にチェックありかAltキーを押しながらでグリッド移動
        'If myForm3.CheckBoxDrawPointGridFit.Checked = True Or My.Computer.Keyboard.AltKeyDown Then
        If myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked OrElse My.Computer.Keyboard.AltKeyDown Then


            Dim gv As Integer = NumericUpDownGrid.Value
            Dim glidValue As Integer = Me.NumericUpDownGrid.Value
            Dim xOffset As Integer = (XPPM頂点移動距離 Mod glidValue)
            Dim yOffset As Integer = (YPPM頂点移動距離 Mod glidValue)
            pMove = New Point(XPPM頂点移動距離 - xOffset, YPPM頂点移動距離 - yOffset)

        End If

        If p.X + pMove.X < 0 And p.Y + pMove.Y < 0 Then
            p = New Point(0, 0)

        End If

        If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 And
            myForm3.CheckBoxAnchorAngle角度固定.Checked And LTag Mod 3 <> 0 Then
            'ベジェ曲線の制御点で角度固定にチェックありの場合
            Select Case LTag Mod 3
                Case 1 'アンカーポイントの後ろの制御点
                    Call AnchorAngle角度固定(pMove, LTag - 1, LTag)
                Case 2 'アンカーポイントの前の制御点
                    Call AnchorAngle角度固定(pMove, LTag + 1, LTag)
            End Select
        Else
            '普通の頂点の場合
            'p.Offset(pMove) '新しい頂点座標決定
            p += pMove '新しい頂点座標決定

            EditNowPic.PathPoints.Item(LTag) = p '新しい頂点座標書き込み
            Call LabelMoveOne_頂点ラベルを個別に移動(L)

        End If



        'ベジェ曲線でアンカーポイントの移動で連動移動にチェックありかShiftキー押していたら制御点も移動
        If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then

            If LTag Mod 3 = 0 Then 'アンカーポイントの時
                If myForm3.CheckBoxLinkedMove連動移動.Checked Or My.Computer.Keyboard.ShiftKeyDown Then
                    'アンカーポイントと制御点の連動移動
                    Dim fcp As New PointF
                    Dim bcp As New PointF
                    If LTag = 0 Then
                        '始点をドラッグ中の時
                        'bcp = New PointF(EditNowPic.PathPoints.Item(LTag + 1).X, EditNowPic.PathPoints(LTag + 1).Y) '始点の制御点座標
                        bcp = EditNowPic.PathPoints(1)
                        bcp += pMove
                        'bcp.Offset(pMove)
                        EditNowPic.PathPoints.Item(LTag + 1) = bcp
                        Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(LTag + 1))
                        If EditNowPic.CloseLine Then
                            '線を閉じている場合は終点とその制御点も移動
                            Dim sp As New PointF
                            sp = (EditNowPic.PathPoints.Item(0)) '始点の座標
                            Dim epi As Integer = PPL描画頂点リスト.Count - 1 '終点のid
                            fcp = EditNowPic.PathPoints.Item(epi - 1) '終点の制御点座標
                            fcp += pMove 'オフセット
                            'fcp.Offset(pMove)
                            EditNowPic.PathPoints.Item(epi - 1) = fcp
                            EditNowPic.PathPoints.Item(epi) = sp '終点の座標を始点と同じにする
                            Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(epi - 1))
                            Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(epi))
                        End If
                    ElseIf LTag = PPL描画頂点リスト.Count - 1 Then
                        '終点をドラッグ中
                        fcp = EditNowPic.PathPoints.Item(LTag - 1)
                        fcp += pMove

                        'fcp.Offset(pMove)
                        EditNowPic.PathPoints.Item(LTag - 1) = fcp
                        Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(LTag - 1))
                    Else
                        'bcp = New Point(EditNowPic.PathPoints.Item(LTag + 1))
                        'fcp = New Point(EditNowPic.PathPoints.Item(LTag - 1))
                        bcp = EditNowPic.PathPoints(LTag + 1)
                        fcp = EditNowPic.PathPoints(LTag - 1)
                        bcp += pMove
                        fcp += pMove

                        'bcp.Offset(pMove)
                        'fcp.Offset(pMove)
                        EditNowPic.PathPoints.Item(LTag + 1) = bcp
                        EditNowPic.PathPoints.Item(LTag - 1) = fcp
                        Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(LTag + 1))
                        Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(LTag - 1))
                    End If

                End If

            ElseIf myForm3.CheckBoxLinkedControlPoint制御点連動.Checked Or My.Computer.Keyboard.CtrlKeyDown Then
                '制御点の時、コントロールキーを押しているかチェックありで
                'ベジェ曲線で制御点を移動中で制御点連動にチェックありなら反対側の制御点も動かす
                Dim PP As List(Of PointF) = EditNowPic.PathPoints
                If EditNowPic.CloseLine And LTag = 1 Then
                    '線を閉じていてドラッグ中なのが始点の制御点の場合
                    Call Linked制御点の連動(LTag, PP.Count - 2, 0)
                ElseIf EditNowPic.CloseLine And LTag = PP.Count - 2 Then
                    ''線を閉じていてドラッグ中なのが終点の制御点の場合
                    'tmpP = New Point(PP.Item(PP.Count - 1)) '終点座標、これは始点でも同じか
                    Call Linked制御点の連動(LTag, 1, PP.Count - 1)
                ElseIf LTag Mod 3 = 1 And LTag <> 1 Then
                    ''idが1,4,7,10などのとき(アンカーポイントの前の制御点)
                    Call Linked制御点の連動(LTag, LTag - 2, LTag - 1)
                ElseIf LTag Mod 3 = 2 And LTag <> PP.Count - 2 Then
                    '自分のidが2,5,8,11のとき(アンカーポイントの後ろの制御点)
                    Call Linked制御点の連動(LTag, LTag + 2, LTag + 1)
                End If

            ElseIf myForm3.CheckBoxLinkedAngle角度連動.Checked Or My.Computer.Keyboard.ShiftKeyDown Then
                'Shiftキーを押しているかチェックありで
                '反対側の制御点の角度だけ連動
                Dim PP As List(Of PointF) = EditNowPic.PathPoints
                If EditNowPic.CloseLine And LTag = 1 Then
                    '線を閉じていてドラッグ中なのが始点の制御点の場合
                    Call Linked制御点角度だけ連動(LTag, PP.Count - 2, 0)
                ElseIf EditNowPic.CloseLine And LTag = PP.Count - 2 Then
                    ''線を閉じていてドラッグ中なのが終点の制御点の場合
                    'tmpP = New Point(PP.Item(PP.Count - 1)) '終点座標、これは始点でも同じか
                    Call Linked制御点角度だけ連動(LTag, 1, PP.Count - 1)
                ElseIf LTag Mod 3 = 1 And LTag <> 1 Then
                    ''idが1,4,7,10などのとき(アンカーポイントの前の制御点)
                    Call Linked制御点角度だけ連動(LTag, LTag - 2, LTag - 1)
                ElseIf LTag Mod 3 = 2 And LTag <> PP.Count - 2 Then
                    '自分のidが2,5,8,11のとき(アンカーポイントの後ろの制御点)
                    Call Linked制御点角度だけ連動(LTag, LTag + 2, LTag + 1)
                End If

            End If

        End If


    End Sub
    Private Sub Linked制御点の連動(myID As Integer, uID As Integer, apID As Integer)
        'Private Sub 頂点座標の計算(L As Label)用
        'myIDがドラッグ中のラベル、uIDが反対側の制御点、apIDがアンカーポイント
        Dim x, y As Integer
        Dim PP As List(Of PointF) = EditNowPic.PathPoints
        Dim tempP As New PointF
        tempP = (PP.Item(apID)) '中心になるアンカーポイント
        Dim iP As New PointF
        iP = (PP.Item(myID)) 'ドラッグ中の制御点
        x = tempP.X - iP.X
        y = tempP.Y - iP.Y
        Dim op As New SizeF(x, y) 'オフセット値
        tempP += op
        'tempP.Offset(op)
        EditNowPic.PathPoints.Item(uID) = tempP '反対側の制御点の座標セット
        Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(uID))
        Call VisiblePointNumber頂点番号表示テキスト更新だけ(PPL描画頂点リスト.Item(uID))

    End Sub
    Private Sub Linked制御点角度だけ連動(myID As Integer, uID As Integer, apID As Integer)
        'Private Sub 頂点座標の計算(L As Label)用
        '反対側の制御点の距離は変えずに角度だけ連動
        Dim iSyahen, iTaihen, iRinpen, uSyahen, uTaihen, uRinpen As Single
        Dim sinR, cosR As Single
        Dim PP As List(Of PointF) = EditNowPic.PathPoints
        Dim iP As New PointF '(PP.Item(myID))
        iP = PP(myID)
        Dim aP As New PointF '(PP.Item(apID))
        aP = PP(apID)
        iTaihen = iP.Y - aP.Y
        iRinpen = iP.X - aP.X
        iSyahen = Math.Sqrt(iTaihen ^ 2 + iRinpen ^ 2)

        If iSyahen <> 0 Then
            '斜辺が0以上の時だけ反対側の角度を変える
            sinR = iTaihen / iSyahen
            cosR = iRinpen / iSyahen
            Dim uP As New PointF '(PP.Item(uID))
            uP = PP(uID)
            uTaihen = uP.Y - aP.Y
            uRinpen = uP.X - aP.X
            uSyahen = Math.Sqrt(uTaihen ^ 2 + uRinpen ^ 2)
            Dim x, y As Single ' Integer
            'x = CInt(uSyahen * cosR)
            'y = CInt(uSyahen * sinR)
            x = uSyahen * cosR
            y = uSyahen * sinR
            Dim op As New SizeF(-x, -y) ' PointF(-x, -y)
            aP += op
            'aP.Offset(op)
            EditNowPic.PathPoints.Item(uID) = aP
            Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(uID))
            Call VisiblePointNumber頂点番号表示テキスト更新だけ(PPL描画頂点リスト.Item(uID))
        End If
    End Sub
    Private Sub AnchorAngle角度固定(pMove As Point, apID As Integer, iID As Integer)
        Dim PP As List(Of PointF) = EditNowPic.PathPoints
        Dim nowSyahen, nowTaihen, nowRinpen, pastSyahen, pastTaihen, pastRinpen As Single
        Dim sinR, cosR As Single
        Dim nowP As New PointF '(PP.Item(iID))
        nowP = PP(iID)
        nowP += pMove 'マウスで移動後の座標
        'nowP.Offset(pMove) 
        Dim aP As New PointF '(PP.Item(apID)) 'アンカーポイント
        aP = PP(apID)
        Dim past As New PointF '(PP.Item(iID)) 'マウスで移動前の座標
        past = PP(iID)

        pastTaihen = past.Y - aP.Y
        pastRinpen = past.X - aP.X
        pastSyahen = Math.Sqrt(pastTaihen ^ 2 + pastRinpen ^ 2)
        If pastSyahen = 0 Then 'エラー回避
            pastSyahen = 1
        End If
        sinR = pastTaihen / pastSyahen
        cosR = pastRinpen / pastSyahen

        nowTaihen = nowP.Y - aP.Y
        nowRinpen = nowP.X - aP.X
        nowSyahen = Math.Sqrt(nowTaihen ^ 2 + nowRinpen ^ 2)
        Dim x As Single = nowSyahen * cosR
        'Dim y As Integer = CInt(nowSyahen * sinR)
        Dim y As Single = nowSyahen * sinR
        Dim op As New SizeF(x, y) 'Point(x, y)
        aP += op
        'aP.Offset(op)
        EditNowPic.PathPoints.Item(iID) = aP
        Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(iID))
        Call VisiblePointNumber頂点番号表示テキスト更新だけ(PPL描画頂点リスト.Item(iID))

    End Sub
    Friend Sub AllPointsMoveGridすべての頂点をグリッドに移動()
        If isDrawEditNow = False Then Exit Sub
        'If myForm3.CheckBoxDrawPointGridFit.Checked = False And
        '   Not My.Computer.Keyboard.AltKeyDown Then Exit Sub
        If myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked = False And
    Not My.Computer.Keyboard.AltKeyDown Then Exit Sub

        'Dim PP() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
        Dim PP As New List(Of PointF)(EditNowPic.PathPoints)
        Call Points2NearGrid頂点を一番近いグリッドに移動(PP)

    End Sub
    Private Overloads Sub Points2NearGrid頂点を一番近いグリッドに移動(Points As List(Of PointF), Optional id As Integer = 0)
        'チェックボックス変更時、図形追加時、編集開始時、頂点追加時
        'すべての頂点を一番近いグリッドに移動
        '上下左右でちょうど中間にある場合は左上優先にしてみた
        'ExPictureのPathPointの情報から計算する
        If isDrawEditNow = False Then Exit Sub
        'If myForm3.CheckBoxDrawPointGridFit.Checked = False And
        '   Not My.Computer.Keyboard.AltKeyDown Then Exit Sub
        If myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked = False And
    Not My.Computer.Keyboard.AltKeyDown Then Exit Sub

        Dim PP As Generic.List(Of PointF) = EditNowPic.PathPoints
        Dim ppX, ppY, gMod, gp, i As Single
        Dim gv As Integer = Me.NumericUpDownGrid.Value
        Dim gd As Single = (gv - 1) / 2
        For i = 0 To Points.Count - 1
            ppX = Points(i).X
            ppY = Points(i).Y
            'x座標
            gMod = ppX Mod gv
            gp = ppX - gMod '補正なしグリッド移動先
            If gMod <= gd Then '左上優先移動、右下優先ならIf gMod <= gd + 0.5 Then
                ppX = gp
            Else
                ppX = gp + gv
            End If

            'y座標
            gMod = ppY Mod gv
            gp = ppY - gMod
            If gMod <= gd Then
                ppY = gp
            Else
                ppY = gp + gv

            End If


            If id = 0 Then
                'すべての頂点
                EditNowPic.PathPoints.Item(i) = New PointF(ppX, ppY)
                Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(i))

            Else
                EditNowPic.PathPoints.Item(id) = New PointF(ppX, ppY)
                Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(id))

            End If
            'Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(id))

        Next


        Call Draw再描画()
        'Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(id))
        'Call 頂点ラベルの再作成()


    End Sub
    Private Overloads Sub Points2NearGrid頂点を一番近いグリッドに移動(Points As PointF, Optional id As Integer = 0)
        'チェックボックス変更時、図形追加時、編集開始時、頂点追加時
        'すべての頂点を一番近いグリッドに移動
        '上下左右でちょうど中間にある場合は左上優先にしてみた
        'ExPictureのPathPointの情報から計算する
        If isDrawEditNow = False Then Exit Sub
        'If myForm3.CheckBoxDrawPointGridFit.Checked = False And
        '   Not My.Computer.Keyboard.AltKeyDown Then Exit Sub
        If myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked = False And
            Not My.Computer.Keyboard.AltKeyDown Then Exit Sub

        Dim PP As Generic.List(Of PointF) = EditNowPic.PathPoints
        Dim ppX, ppY, gMod, gp, i As Single
        Dim gv As Integer = Me.NumericUpDownGrid.Value
        Dim gd As Single = (gv - 1) / 2

        ppX = Points.X
        ppY = Points.Y
        'x座標
        gMod = ppX Mod gv
        gp = ppX - gMod '補正なしグリッド移動先
        If gMod <= gd Then '左上優先移動、右下優先ならIf gMod <= gd + 0.5 Then
            ppX = gp
        Else
            ppX = gp + gv
        End If

        'y座標
        gMod = ppY Mod gv
        gp = ppY - gMod
        If gMod <= gd Then
            ppY = gp
        Else
            ppY = gp + gv

        End If


        If id = 0 Then
            'すべての頂点
            EditNowPic.PathPoints.Item(i) = New PointF(ppX, ppY)
            Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(i))

        Else
            EditNowPic.PathPoints.Item(id) = New PointF(ppX, ppY)
            Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(id))

        End If
        'Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(id))




        Call Draw再描画()
        'Call LabelMoveOne_頂点ラベルを個別に移動(PPL描画頂点リスト.Item(id))
        'Call 頂点ラベルの再作成()


    End Sub


    Private Sub MouseUpラベルドラッグ用(sender As Label, e As MouseEventArgs)
        'If e.Button = Windows.Forms.MouseButtons.Left Then
        '    sender.Location -= PPLabelOffset
        '    Me.ListBox1.Select() 'ラベルのActiveを外す
        '    Dim bmp As Bitmap
        '    bmp = (oreDraw図形2描画ドラッグ用())
        '    Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)
        'Else
        '    Me.Select()
        '    Me.ListBox1.Select() 'ラベルのActiveを外す
        '    Dim bmp As Bitmap
        '    bmp = (oreDraw図形2描画ドラッグ用())
        '    Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)
        'End If
    End Sub
    '
    Friend Overloads Sub Draw再描画(EXP As ExPictureBox, pSize As Size, Optional editEnd As Boolean = False)
        'Dim bmp As Bitmap = oreDraw図形2描画ドラッグ用()
        Dim bmp As Bitmap

        If editEnd Then
            bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(EXP, pSize, True)
        Else
            bmp = DrawShape2図形2描画ドラッグ用サイズ変更なし版(EXP, pSize)
        End If

        'EditNowPic.Image = bmp
        EXP.Image = bmp
        Call ore加工した画像を各アレイリストに書き込む(EXP, bmp)

    End Sub
    Friend Overloads Sub Draw再描画()
        Dim psize As New Size(EditNowPic.Width, EditNowPic.Height)
        Call Draw再描画(EditNowPic, psize)

    End Sub
    Friend Overloads Sub Draw再描画(Exp As ExPictureBox, Optional editEnd As Boolean = False)
        Dim pSize As New Size(Exp.Width, Exp.Height)
        Call Draw再描画(Exp, pSize, editEnd)
    End Sub


#Region "頂点の追加"
    '--------------------------------頂点の追加ここから--------------------------------
    Private Function GetAngle_対辺と隣辺から角度(Taihen As Integer, Rinpen As Integer) As Single
        '対辺と隣辺を受け取って角度を返す
        '対辺＝Y軸、隣辺＝X軸の距離
        Dim kakudo As Single
        If Taihen = 0 Or Rinpen = 0 Then '対辺と隣辺のどちらかが0の時は0,90,180,270のいずれか
            Select Case True
                Case Taihen = 0 And Rinpen > 0
                    kakudo = 180
                Case Taihen = 0 And Rinpen < 0
                    kakudo = 0
                Case Taihen > 0 And Rinpen = 0
                    kakudo = 270
                Case Taihen < 0 And Rinpen = 0
                    kakudo = 90
            End Select

        Else '対辺と隣辺どちらも0以外の時
            Dim radian As Single
            radian = Math.Atan(Taihen / Rinpen) 'ラジアン取得
            kakudo = 180 / Math.PI * radian 'ラジアンを角度に変換
            Select Case True '対辺と隣辺が+-で加算する値が変わる
                Case Taihen < 0 And Rinpen > 0 '対辺マイナスで隣辺プラスの時
                    kakudo += 180
                Case Taihen < 0 And Rinpen < 0 '対辺隣辺どちらもマイナスの時
                    kakudo += 0
                Case Taihen > 0 And Rinpen < 0 '対辺がプラスで隣辺がマイナスの時
                    kakudo += 360
                Case Taihen > 0 And Rinpen > 0 '対辺隣辺どちらもプラスの時
                    kakudo += 180
            End Select
        End If

        Return kakudo
    End Function
    Private Sub ToolStripMenuItem頂点の追加延長_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItem頂点の追加延長.Click
        '始点と終点の近い方に継ぎ足す
        If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.曲線 Or
            EditNowPic.GraphicDrawType = ExPictureBox.DrawType.直線 Then

            Dim PP As List(Of PointF) = EditNowPic.PathPoints
            Dim sp As New PointF '(PP.Item(0))
            sp = PP(0)
            Dim ep As New PointF '(PP.Item(PP.Count - 1))
            ep = PP(PP.Count - 1)
            Dim sd As Single = kyori二点間の距離＿斜辺の長さ(sp, RClickPoint, True)
            Dim ed As Single = kyori二点間の距離＿斜辺の長さ(ep, RClickPoint, True)
            If sd >= ed Then
                '終点のほうが近い時
                EditNowPic.PathPoints.Insert(PP.Count, RClickPoint)
            Else
                EditNowPic.PathPoints.Insert(0, RClickPoint)
            End If

        ElseIf EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then

            'ベジェ曲線だけど線上以外なら近くの始点か終点に継ぎ足す
            '二点間の距離計測
            Dim pts() As PointF = DirectCast(EditNowPic.PathPoints.ToArray, PointF())

            Dim ub As Integer = UBound(pts)
            Dim sp As Single = kyori二点間の距離＿斜辺の長さ(pts(0), RClickPoint)
            Dim ep As Single = kyori二点間の距離＿斜辺の長さ(pts(ub), RClickPoint)

            If sp <= ep Then
                '始点の方が近い場合は始点に継ぎ足す
                '0,1,2,3に継ぎ足す時、元の頂点はそれぞれ0→3、1→4、2→5になり
                '追加した頂点が0、その制御点が1、2が元の始点の制御点になる
                '新1は新0と旧0の線上10分の1の距離、新2は旧1の対角線上にしてみる
                Dim p0 As New PointF '(pts(0)) '旧0(始点)
                p0 = pts(0)
                Dim p1 As New PointF '(pts(1)) '旧1(旧始点の制御点)
                p1 = pts(1)
                Dim x As Single = p0.X - p1.X
                Dim y As Single = p0.Y - p1.Y
                Dim offp As New SizeF(x, y) 'PointF(x, y)
                Dim n2 As New PointF '(pts(0))
                n2 = pts(0)
                n2 += offp
                'n2.Offset(offp)

                x = (p0.X - RClickPoint.X) / 10
                y = (p0.Y - RClickPoint.Y) / 10
                offp = New SizeF(x, y) ' PointF(x, y)
                Dim n1 As New PointF(RClickPoint.X, RClickPoint.Y)
                n1 += offp
                'n1.Offset(offp)

                Dim pr As New List(Of PointF)({RClickPoint, n1, n2})


                EditNowPic.PathPoints.InsertRange(0, pr)


            Else
                '終点のほうが近い時
                '0,1,2,3に継ぎ足す時4,5,6を追加6がアンカーポイントになる
                '4は2の対角線上、5は6から3への線上で距離は10分の1
                Call AddPointアンカーポイントの継ぎ足し(pts)
                '終点に継ぎ足すのは線を閉じる場合にも使うので外に出した


            End If
        End If

        Call AllPointsMoveGridすべての頂点をグリッドに移動()
        Call 頂点ラベルの再作成(EditNowPic) '頂点ラベルの再作成
        Dim bmp As New Bitmap(DrawShape2図形2描画ドラッグ用サイズ変更なし版(EditNowPic))
        Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)

    End Sub

    Private Sub ToolStripMenuItem頂点を追加_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem頂点の追加.Click
        '右クリックしたところに頂点を追加
        Dim newPoint As New Point(RClickPoint) '新しい頂点の座標、ExPictureの中の右クリックした座標
        Dim ps As Generic.List(Of PointF) = EditNowPic.PathPoints '頂点のコレクションリスト
        Dim i As Long
        Dim AddPointIndex As Single = 0 ' Integer = 0
        Dim p2pMin As Single = 0 ' Integer = 0
        'Dim AngleMin2 As Integer = 0


        '実際に描いて判定
        'Dim pts() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
        Dim pts As New List(Of PointF)(EditNowPic.PathPoints)

        Dim lc, idx2, st As Integer
        Dim isOnline As Boolean = False

        If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
            'lc = UBound(pts) - 3
            lc = pts.Count - 4
        ElseIf EditNowPic.CloseLine Then '線が閉じているか開いているかでループ回数を変化
            'lc = UBound(pts)
            lc = pts.Count - 1
        Else
            'lc = UBound(pts) - 1
            lc = pts.Count - 2
        End If
        If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
            st = 3
        Else
            st = 1
        End If
        For i = 0 To lc Step st
            Dim tempbmp As Bitmap ' = GetP2PLine二頂点間の線の画像作成(pts, i)
            If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
                'Dim bpts() As Point = {New Point(pts(i)), New Point(pts(i + 1)), New Point(pts(i + 2)), New Point(pts(i + 3))}
                Dim bpts As New List(Of PointF)({pts(i), pts(i + 1), pts(i + 2), pts(i + 3)})
                tempbmp = GetP2PLine二頂点間の線の画像作成(bpts)
            Else
                tempbmp = GetP2PLine二頂点間の線の画像作成(pts, i)

            End If
            Dim ccol As Color = tempbmp.GetPixel(RClickPoint.X, RClickPoint.Y)
            If ccol <> Color.FromArgb(0, 0, 0, 0) Then 'クリックした座標に色がついていれば
                idx2 = i
                isOnline = True
                Exit For

            End If
        Next i
        'ここまでisOnlineがFalseなら線上以外ってことになる
        '画像判定で結果が出ればそれを優先、クリックした座標に色がなければ角度での判定を優先
        If isOnline Then
            AddPointIndex = idx2

        End If



        '角度調査は追加頂点が線上になかった場合だけ
        If isOnline = False Then
            'ベジェ曲線での角度調査件数は0,3,6,9,12の頂点だけになる、直線や曲線とは違うのでリストを作る
            If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
                Dim BezierP As New List(Of PointF)
                'BezierP.Add(ps(0))
                For i = 0 To ps.Count - 1 Step 3
                    BezierP.Add(ps(i))
                Next
                AddPointIndex = Judge頂点の追加場所_各頂点との角度差から(BezierP, RClickPoint)
                AddPointIndex *= 3
            Else

                '各頂点との角度差から
                AddPointIndex = Judge頂点の追加場所_各頂点との角度差から(ps, RClickPoint)
            End If
            'AddPointIndex = AngleMin2

        End If



        '新頂点をexPictureBoxのPointNに挿入
        'If isOnline Then
        'ベジェ曲線上なら制御点も追加
        If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then

            Dim x0 As Single = pts(AddPointIndex).X '追加するアンカーポイントの前のアンカーポイント座標
            Dim x1 As Single = pts(AddPointIndex + 3).X '追加するアンカーポイントの後ろのアンカーポイント座標
            Dim y0 As Single = pts(AddPointIndex).Y
            Dim y1 As Single = pts(AddPointIndex + 3).Y
            Dim xd As Single = (x1 - x0) / 10
            Dim yd As Single = (y1 - y0) / 10

            Dim off1 As New PointF(newPoint.X - xd, newPoint.Y - yd)
            Dim off2 As New PointF(newPoint.X + xd, newPoint.Y + yd)

            With EditNowPic.PathPoints
                .Insert(AddPointIndex + 2, off1) '前
                .Insert(AddPointIndex + 3, newPoint) '自分、アンカーポイント
                .Insert(AddPointIndex + 4, off2) '後ろ
            End With
        Else
            '直線、曲線の場合
            EditNowPic.PathPoints.Insert(AddPointIndex + 1, newPoint) '線の上以外なら角度優先

        End If




        'ElseIf EditNowPic.GraphicDrawType = ExPictureBox.DrawType.曲線 Or
        '    EditNowPic.GraphicDrawType = ExPictureBox.DrawType.直線 Then

        'EditNowPic.PathPoints.Insert(AddPointIndex + 1, newPoint) '各頂点との角度差バージョン

        'End If


        Call AllPointsMoveGridすべての頂点をグリッドに移動()

        Call 頂点ラベルの再作成(EditNowPic) '頂点ラベルの再作成
        'Dim bmp As Bitmap = oreDraw図形2描画ドラッグ用()
        Dim bmp As New Bitmap(DrawShape2図形2描画ドラッグ用サイズ変更なし版(EditNowPic))
        Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)

    End Sub

    Private Sub AddPointアンカーポイントの継ぎ足し(pts As PointF(), Optional toClose As Boolean = False)
        '終点のほうが近い時、終点に継ぎ足す
        '0,1,2,3に継ぎ足す時4,5,6を追加6がアンカーポイントになる
        '4は2の対角線上、5は6から3への線上で距離は10分の1
        Dim ub As Integer = UBound(pts)
        Dim p1 As New PointF ' Point(pts(1))
        Dim p2 As New PointF ' Point(pts(ub - 1))
        Dim p3 As New PointF ' Point(pts(ub))
        p1 = pts(1)
        p2 = pts(ub - 1)
        p3 = pts(ub)
        Dim x As Single = p3.X - p2.X ' Integer = p3.X - p2.X
        Dim y As Single = p3.Y - p2.Y ' Integer = p3.Y - p2.Y
        Dim offp As New SizeF(x, y) 'New Point(x, y)
        Dim p4 As New PointF ' Point(pts(ub))
        p4 = pts(ub)
        p4 += offp
        'p4.Offset(offp)

        'アンカーポイントの追加座標
        Dim AddP As PointF = RClickPoint
        If toClose Then '線を閉じる場合は右クリックの座標ではなく始点と同じ座標
            'AddP = New Point(pts(0))
            AddP = pts(0)

            x = (AddP.X - p1.X)
            y = (AddP.Y - p1.Y)
        Else
            '線を閉じる以外の時
            x = (p3.X - AddP.X) / 10
            y = (p3.Y - AddP.Y) / 10

        End If


        offp = New SizeF(x, y) ' New Point(x, y)

        Dim p5 As New PointF(AddP.X, AddP.Y)
        p5 += offp
        'p5.Offset(offp)

        Dim pr As New List(Of PointF)({p4, p5, AddP}) 'リストにまとめて
        With EditNowPic.PathPoints '追加！！！
            .AddRange(pr)

        End With
    End Sub
    Private Function Judge頂点の追加場所_各頂点との角度差から(Points As Generic.List(Of PointF), ClickPoint As Point) As Single ' Integer
        '頂点座標のリストと右クリック座標を受け取って追加するインデックスを返す

        '---新頂点から各頂点との角度差のリスト作成---ここから
        Dim zTaihen, zRinpen As Single ' Integer
        Dim zKakudo As Single
        Dim zArray As New List(Of Single)

        For i = 0 To Points.Count - 1
            zRinpen = Points(i).X - ClickPoint.X
            zTaihen = Points(i).Y - ClickPoint.Y
            zKakudo = GetAngle_対辺と隣辺から角度(zTaihen, zRinpen)
            zArray.Add(zKakudo)

        Next

        '各角度差のリスト作成
        Dim zDiff, ztmp As New ArrayList
        For i = 0 To Points.Count - 2
            Dim tp As Single = Math.Abs(zArray(i) - zArray(i + 1))
            tp = Math.Abs(tp - 180)
            zDiff.Add(tp)
        Next

        '閉じた線なら終点と始点の角度差もリストに追加、
        If EditNowPic.CloseLine Then
            Dim tp As Single = Math.Abs(zArray.Last - zArray(0))
            tp = Math.Abs(tp - 180)
            zDiff.Add(tp)

        End If

        ztmp = zDiff.Clone      '昇順並べ替えのためにリストのクローン作成
        ztmp.Sort()             '並べ替え
        Dim idx3 = zDiff.IndexOf(ztmp(0)) '新頂点から各頂点との角度の最小値のインデックス取得

        '---新頂点から各頂点との角度差のリスト作成---ここまで
        Return idx3

    End Function

    Private Sub 頂点追加判定() 'テスト用
        '二頂点間の線の画像がほしい
        '画像サイズはExPicture
        Dim bmp As New Bitmap(200, 200)
        Dim g As Graphics = Graphics.FromImage(bmp)
        'Dim pp() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
        'Dim pt() As Point = {New Point(50, 50), New Point(150, 50), New Point(150, 150)} ', New Point(50, 150)}
        Dim pt As New List(Of PointF)({New PointF(50, 50), New PointF(150, 50), New PointF(150, 150)})
        Dim P1 = New Pen(myForm3.PictureBoxPenの色.BackColor, myForm3.NumericUpDownPathPenの太さ.Value)


        'g.DrawCurve(P1, pt, 1, 1, 1.0)
        bmp = GetP2PLine二頂点間の線の画像作成(pt, 2)

        Me.Panel2.BackgroundImage = bmp

    End Sub
    Private Function GetP2PLine二頂点間の線の画像作成(pts As List(Of PointF), Optional offset As Integer = 0) As Bitmap
        'pstは全頂点、opはどこから
        '0から5のうち1から2の区間ならopは1を指定する、3から4ならopは3を指定
        '指定できるopの範囲は最小値は0、最大値は閉じた線ならUbound(pts)で開いた線ならそれ-1

        Dim bmp = New Bitmap(EditNowPic.Width, EditNowPic.Height)
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim P1 As New Pen(Brushes.AliceBlue, EditNowPic.ExPenWidth) 'Dim P1 As Pen = EditNowPic.ExPen
        Dim tN As Single = EditNowPic.CurveTension

        'Dim bmp = New Bitmap(200, 200)
        'Dim g As Graphics = Graphics.FromImage(bmp)
        'Dim P1 = New Pen(myForm3.PictureBoxPenの色.BackColor, myForm3.NumericUpDownPathPenの太さ.Value)
        'Dim tN As Single = 1

        'opはオフセット、描画開始ポイント
        If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.直線 Then
            If EditNowPic.CloseLine And offset = pts.Count - 1 Then ' UBound(pts) Then '閉じた直線でオフセットが終点の場合
                g.DrawLine(P1, pts(offset), pts(0))
            Else
                g.DrawLine(P1, pts(offset), pts(offset + 1))
            End If

            Return bmp


        ElseIf EditNowPic.GraphicDrawType = ExPictureBox.DrawType.曲線 Then
            If EditNowPic.CloseLine = False And offset = 0 Then '開いた曲線でオフセットが0の場合
                Dim seg(2) As PointF
                seg(0) = pts(0)
                seg(1) = pts(1)
                seg(2) = pts(2)
                g.DrawCurve(P1, seg, 0, 1, tN)
                Return bmp


            ElseIf EditNowPic.CloseLine = False And offset + 1 = pts.Count - 1 Then ' UBound(pts) Then '開いた曲線でセグメントに終点が含まれて
                Dim seg(2) As PointF
                seg(0) = pts(offset - 1)
                seg(1) = pts(offset)
                seg(2) = pts(pts.Count - 1) ' pts(UBound(pts))
                g.DrawCurve(P1, seg, 1, 1, tN)
                Return bmp


            Else '上記以外
                Dim seg(3) As PointF
                If EditNowPic.CloseLine = False Then 'セグメントに始点終点が含まれていない開いた曲線
                    seg(0) = pts(offset - 1)
                    seg(1) = pts(offset)
                    seg(2) = pts(offset + 1)
                    seg(3) = pts(offset + 2)
                    g.DrawCurve(P1, seg, 1, 1, tN)
                    Return bmp

                Else 'ここから閉じた曲線
                    Select Case True
                        Case offset = 0 'セグメント開始(オフセット)が始点
                            seg(0) = pts(pts.Count - 1) ' pts(UBound(pts))
                            seg(1) = pts(offset)
                            seg(2) = pts(offset + 1)
                            seg(3) = pts(offset + 2)
                        Case offset = pts.Count - 2 ' UBound(pts) - 1 'セグメント終了(オフセット)が終点
                            seg(0) = pts(offset - 1)
                            seg(1) = pts(offset)
                            seg(2) = pts(pts.Count - 1) 'pts(UBound(pts))
                            seg(3) = pts(0)
                        Case offset = pts.Count - 1 'UBound(pts) 'セグメント開始が終点で終了が始点
                            seg(0) = pts(pts.Count - 2) ' pts(UBound(pts) - 1)
                            seg(1) = pts(offset)
                            seg(2) = pts(0)
                            seg(3) = pts(1)
                        Case Else 'それ以外
                            seg(0) = pts(offset - 1)
                            seg(1) = pts(offset)
                            seg(2) = pts(offset + 1)
                            seg(3) = pts(offset + 2)
                    End Select
                    g.DrawCurve(P1, seg, 1, 1, tN)
                    Return bmp


                End If

            End If
        Else 'If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
            Dim gp As New GraphicsPath
            'gp.AddBeziers(pts)
            Dim pF() As PointF = DirectCast(pts.ToArray, PointF())

            gp.AddBeziers(pF)
            gp.Flatten()

            g.DrawPath(P1, gp)
            gp.Dispose()

            Return bmp

        End If

    End Function
    Private Function kyori二点間の距離＿斜辺の長さ(p1 As PointF, p2 As PointF, Optional abs As Boolean = False) As Single
        Dim kyori As Single
        Dim x = (p2.X - p1.X) ^ 2
        Dim y = (p2.Y - p1.Y) ^ 2
        kyori = Math.Sqrt(x + y)
        'フラグで絶対値に変更
        If abs Then
            kyori = Math.Abs(kyori)
        End If
        Return kyori

    End Function
    '--------------------------------頂点の追加ここまで--------------------------------
#End Region

    '--------------------------------頂点の削除ここから--------------------------------
    Private Sub ToolStripMenuItem頂点の削除_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItem頂点の削除.Click
        If EditNowPic.Controls.Count > 2 Then

            Dim i As Integer = ActLabel.Tag
            'ベジェ曲線の場合、始点の時は0,1,2を削除だからi,i+1,i+2、終点の時は4,5,6だからi,i-1,i-2
            If EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
                With EditNowPic.Controls
                    If i = 0 Then                               '始点の時
                        If EditNowPic.CloseLine = True Then
                            '線を閉じている場合
                            Dim cp As New PointF '(EditNowPic.PathPoints(2)) '元の始点の制御点を記録
                            cp = EditNowPic.PathPoints(2)
                            EditNowPic.PathPoints.RemoveRange(0, 3) '始点とその制御点を削除
                            Dim ep As Integer = EditNowPic.PathPoints.Count - 1
                            EditNowPic.PathPoints(ep) = EditNowPic.PathPoints(0) ' New Point(EditNowPic.PathPoints(0)) '終点を始点の座標と同じにする
                            EditNowPic.PathPoints(ep - 1) = cp '終点の制御点をさっき記録した座標にする

                        Else
                            '線が開いていれば合計3つ
                            EditNowPic.PathPoints.RemoveRange(0, 3)

                        End If

                    ElseIf i = PPL描画頂点リスト.Count - 1 Then '終点の時
                        EditNowPic.PathPoints.RemoveRange(i - 2, 3)
                    Else
                        EditNowPic.PathPoints.RemoveRange(i - 1, 3) 'それ以外の時
                    End If

                    .Remove(PPL描画頂点リスト(i))
                End With


            Else 'ベジェ曲線意外の場合 
                EditNowPic.PathPoints.RemoveAt(i)
                EditNowPic.Controls.Remove(ActLabel)

            End If
            '直後に再作成をするからdisposeとかはここでは要らない



            Call 頂点ラベルの再作成(EditNowPic) '頂点ラベルの再作成
            'Dim bmp As Bitmap = oreDraw図形2描画ドラッグ用()
            Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(EditNowPic)
            Call ore加工した画像を各アレイリストに書き込む(EditNowPic, bmp)
            Me.ToolStripMenuItem頂点の削除.Enabled = False

            '一旦頂点の削除の項目を無効にする、逆にした方がいいかも
            ToolStripMenuItem頂点の削除.Enabled = False

        End If
    End Sub

    '--------------------------------頂点の削除ここまで--------------------------------
    Private Sub ToolStripMenuItem頂点の編集開始_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItem頂点の編集開始.Click
        Call myForm3.StartEdit編集開始()

    End Sub
    Private Sub ToolStripMenuItem頂点の編集終了_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItem頂点の編集終了.Click
        Call CloseEdit編集終了()

    End Sub
    Private Sub ToolStripMenuItemクリックで描画を終了_Click_1(sender As Object, e As EventArgs) Handles ToolStripMenuItemマウスで描画を終了.Click
        Call MouseEndDrawマウスで描画終了処理()
    End Sub

    Private Sub Button13_Click(sender As System.Object, e As System.EventArgs) Handles Button13.Click
        '列挙体のすべての数値を取得
        Dim env() As Integer = [Enum].GetValues(GetType(ExPictureBox.DrawType))
        '列挙体のすべての名前を取得
        Dim enn() As String = [Enum].GetNames(GetType(ExPictureBox.DrawType))


        'Call MouseDrawBezierベジェ曲線(New Point(0, 0))

        ''グリッド合わせ
        'Dim grid As Integer = NumericUpDownGrid.Value
        'Dim absp As Point = AbsolutePoint(ActExPic)

        'Dim px As Integer = ActExPic.Location.X
        'Dim py As Integer = ActExPic.Location.Y
        'Dim absx As Integer = px - Panel2.AutoScrollPosition.X
        'Dim absy As Integer = py - Panel2.AutoScrollPosition.Y

        'Dim xmod As Integer = absx Mod grid
        'Dim ymod As Integer = absy Mod grid

        'Dim np As New Point(px + xmod, py + ymod)
        'ActExPic.Location = np

        '図形2チェック用ここまで

    End Sub
    '#Region "図形2ExPictureサイズ変更"
    '--------------------------------図形2ExPictureBoxのサイズ変更ここから--------------------------------
    Friend Sub ExPicture大きさ変更用ラベル表示() '編集ボタン押した時に実行
        Call ExPicture大きさ変更用ラベルと枠を消去()

        If myPicAr.Count = 0 Then Exit Sub
        'If ActExPic Is Nothing Then Exit Sub
        If ActExPic.IsEdit = False Then Exit Sub


        Dim ps() = サイズ変更ラベルの位置配列作成(EditNowPic.Bounds) 'ExPictureのサイズからラベルの座標の配列作成
        Dim lName() As String = {"LU", "LD", "RU", "RD", "U", "D", "L", "R"} 'ラベルの名前
        Dim LC() As Cursor = {Cursors.SizeNWSE, Cursors.SizeNESW, Cursors.SizeNESW, Cursors.SizeNWSE,
                              Cursors.SizeNS, Cursors.SizeNS, Cursors.SizeWE, Cursors.SizeWE}
        Dim bmp As New Bitmap(LABEL_PIC_SIZE, LABEL_PIC_SIZE)
        Dim g As Graphics = Graphics.FromImage(bmp)
        Dim b As New SolidBrush(Color.LimeGreen)
        Dim p As New Pen(Brushes.White)
        g.FillRectangle(b, 0, 0, bmp.Width, bmp.Height)
        g.DrawRectangle(p, 1, 1, bmp.Width - 3, bmp.Height - 3)
        g.Dispose()
        b.Dispose()
        p.Dispose()

        For i As Integer = 0 To UBound(ps)
            Dim L As New Label
            With L

                '.Width = LABEL_PIC_SIZE
                '.Height = LABEL_PIC_SIZE
                '.Location = ps(i)
                .SetBounds(ps(i).X, ps(i).Y, LABEL_PIC_SIZE, LABEL_PIC_SIZE)
                '.BackColor = Color.FromArgb(255, Color.LimeGreen)
                .Cursor = LC(i)

                .Name = lName(i)
                .Image = bmp

            End With
            Me.Panel2.Controls.Add(L) 'ラベルをPanel2に追加して表示
            LSサイズ変更ラベル.Add(L) '管理リストに追加
            L.BringToFront() '最前面に表示


            AddHandler L.MouseDown, AddressOf mouseDown_ExPicture大きさ変更用ラベル
            AddHandler L.MouseMove, AddressOf mouseMove用_ExPicture大きさ変更
            AddHandler L.MouseUp, AddressOf MouseUP_ExPicture大きさ変更

        Next
        Me.Panel2.Controls("RD").BringToFront() '右下のラベルを一番上に表示する




    End Sub
    Friend Sub ExPicture大きさ変更用ラベルと枠を消去()
        If LSサイズ変更ラベル.Count = 0 Then Exit Sub

        'ラベルが表示されていたら消去
        For i As Integer = 0 To LSサイズ変更ラベル.Count - 1
            Me.Panel2.Controls.Remove(LSサイズ変更ラベル(i))
            LSサイズ変更ラベル.Item(i).Dispose()

        Next
        LSサイズ変更ラベル.Clear()

        '枠も消去
        Call PicBoderLineLabel画像に付けた枠を消去()

    End Sub
    Private Sub mouseDown_ExPicture大きさ変更用ラベル(sender As Object, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then
            Click_初期位置 = e.Location
            XLClick初期位置 = e.X
            YLClick初期位置 = e.Y

            MLドラッグラベル = sender
            xSizeLabelLocate = MLドラッグラベル.Location.X
            ySizeLabelLocate = MLドラッグラベル.Location.Y

            'EditNowPic.BackgroundImage = Nothing '意味が無いみたい、なんで？
            'EditNowPic.BackColor = Color.Transparent


        End If

    End Sub
    Private Sub mouseMove用_ExPicture大きさ変更(sender As Label, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then


            XMMマウスの移動距離 = e.X - XLClick初期位置
            YMMマウスの移動距離 = e.Y - YLClick初期位置

            Dim L As Label
            L = sender '動かしているラベル


            'Call サイズ変更用ラベルの位置(L) 'test
            'Call サイズ変更中用のラベルの再表示(TempRect変更中) 'test


            'Call ExPictureのサイズ変更(L)
            Call ExPictureのサイズ変更_小数点計算(L)

            Call サイズ変更用のラベルの再表示()




            ''背景画像消す
            'EditNowPic.BackgroundImage = Nothing '意味が無いみたい、なんで？
            'EditNowPic.BackColor = Color.Transparent

            'Call Draw再描画()'これは処理が重すぎるのでなしで、MouseUpで実行
            'EditNowPic.Refresh() 'これも重いし、一度描画したものしか表示されないのは当たり前か…


        End If

    End Sub
    Private Sub MouseUP_ExPicture大きさ変更(sender As Label, e As MouseEventArgs)
        '頂点座標セット、これはマウスアップでやったほうがいい？
        'Dim sBound As New Rectangle(EditNowPic.Location, EditNowPic.Size)
        'Dim eBound As Rectangle = TempRect変更中
        'Dim ox As Integer = eBound.Location.X - sBound.Location.X
        'Dim oy As Integer = eBound.Location.Y - sBound.Location.Y
        'Dim op As New Point(-ox, -oy)
        'Dim ps() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
        'For i As Integer = 0 To UBound(ps)
        '    ps(i).Offset(op)
        '    EditNowPic.PathPoints.Item(i) = ps(i)
        'Next


        'EditNowPic.Size = New Size(TempRect変更中.Size)
        'EditNowPic.Location = New Point(TempRect変更中.Location)

        Call AllPointsMoveGridすべての頂点をグリッドに移動()
        Call Draw再描画()
        Call 編集画像の背景更新()
        Call ダミー画像の更新()
        '頂点情報表示なら表示の更新
        Select Case sender.Name
            Case "U", "L", "LD", "LU", "RU"
                If myForm3.CheckBoxVisiblePointLocate頂点座標表示.Checked = False Then Exit Select
                For i As Integer = 0 To PPL描画頂点リスト.Count - 1
                    Call VisiblePointNumber頂点番号表示テキスト更新だけ(PPL描画頂点リスト(i))

                Next

        End Select

        '変更後にスクロールバーを移動させる
        sender.Select()
        Me.ListBox1.Select()

    End Sub
    Private Sub ダミー画像の更新()
        If isDrawEditNow = False Then Exit Sub

        Dim sp As New Point(EditNowPic.Size)
        Dim LabelSize As New Point(LABEL_PIC_SIZE, LABEL_PIC_SIZE)
        sp.Offset(LabelSize) 'サイズ変更用のラベルの大きさ分を足す

        With dummyExPicBox
            .Location = EditNowPic.Location
            .Size = sp
        End With
        dummyExPicBox.SendToBack()

    End Sub
    Private Sub 編集画像の背景更新()
        If isDrawEditNow = False Then Exit Sub
        Dim bmp As Bitmap = BGImage背景画像作成ExPictureBox用(EditNowPic)
        EditNowPic.BackgroundImage = bmp

    End Sub
    Private Overloads Sub サイズ変更用のラベルの再表示()
        If isDrawEditNow = False Then Exit Sub

        Dim rect As Rectangle = EditNowPic.Bounds
        Call サイズ変更用のラベルの再表示(rect)

    End Sub
    Private Overloads Sub サイズ変更用のラベルの再表示(rect As Rectangle)
        If isDrawEditNow = False Then Exit Sub

        Dim ps = サイズ変更ラベルの位置配列作成(rect) 'サイズ変更したExPictureから新たにラベルの位置作成
        Dim lName() As String = {"LU", "LD", "RU", "RD", "U", "D", "L", "R"} 'ラベルの名前

        'ラベルの位置変更
        For i As Integer = 0 To UBound(lName)
            Me.Panel2.Controls(lName(i)).Location = ps(i)
        Next


        ''枠の位置変更
        Call PicBorderMove枠の移動(rect)

    End Sub
    Friend Sub ExPictureをグリッドに移動(exp As ExPictureBox)
        '最初にExPictureをグリッドに合わせる 
        'サイズをグリッドに合わせるにチェック入れた時、チェックが入っている時に編集開始をおした時、図形追加時
        If myPicAr.Count = 0 Then Exit Sub
        If isDrawEditNow = False Then Exit Sub

        If myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked = False Then Exit Sub

        Dim xsb As Integer = Me.Panel2.AutoScrollPosition.X
        Dim ysb As Integer = Me.Panel2.AutoScrollPosition.Y

        Dim np As New Point(AbsolutePoint(exp))
        Dim gv As Integer = Me.NumericUpDownGrid.Value
        Dim xMod As Integer = np.X Mod gv
        Dim yMod As Integer = np.Y Mod gv
        np = New Point(np.X - xMod + xsb, np.Y - yMod + ysb)
        exp.Location = np '座標決定

        'サイズ変更、グリッドの倍数に合わせる
        Dim w As Integer = exp.Width
        Dim h As Integer = exp.Height
        'グリッドに左上に一度合わせて
        Dim nw As Integer = w - (w Mod gv)
        Dim nh As Integer = h - (h Mod gv)
        'MODが0ならサイズ変更なし、それ以外なら右下に大きくする
        If w Mod gv <> 0 Then
            nw = nw + gv
        End If
        If h Mod gv <> 0 Then
            nh = nh + gv
        End If

        exp.Width = nw
        exp.Height = nh


        Call サイズ変更用のラベルの再表示()
        Call 編集画像の背景更新()
    End Sub
    Private Sub ExPictureのサイズ変更_小数点計算(L As Label)
        'サイズ変更用ラベルのMouseMoveで使用

        Dim offsサイズ As New Size(0, 0)
        Dim offp画像座標 As New Size(0, 0)
        Dim pps画像サイズ As New Size(EditNowPic.Size)
        Dim pps画像元サイズ As New Size(EditNowPic.Size)
        Dim ppp画像の座標 As New Point(EditNowPic.Location)
        Dim ppp画像元座標 As New Point(EditNowPic.Location)
        Dim dpp描画offset As New Size(0, 0) '描画頂点のオフセット用
        'グリッド
        'スクロールバーの移動分は要らなかった…
        'Dim xS As Integer = Me.Panel2.AutoScrollPosition.X 'スクロールバーのぶん
        'Dim yS As Integer = Me.Panel2.AutoScrollPosition.Y
        Dim gridValue As Integer = Me.NumericUpDownGrid.Value
        'Dim gXグリッド補正値 As Integer = (XMMマウスの移動距離 Mod gridValue) + xS
        'Dim gYグリッド補正値 As Integer = (YMMマウスの移動距離 Mod gridValue) + yS
        Dim gXグリッド補正値 As Integer = (XMMマウスの移動距離 Mod gridValue)
        Dim gYグリッド補正値 As Integer = (YMMマウスの移動距離 Mod gridValue)


        'グリッドに合わせないとき

        If myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked = False Then
            gXグリッド補正値 = 0
            gYグリッド補正値 = 0

        End If

        Select Case L.Name
            Case "U"
                offsサイズ = New Size(0, -YMMマウスの移動距離 + gYグリッド補正値)
                offp画像座標 = New Size(0, YMMマウスの移動距離 - gYグリッド補正値)
                dpp描画offset = New Size(0, -YMMマウスの移動距離 + gYグリッド補正値)
            Case "D"
                offsサイズ = New Size(0, YMMマウスの移動距離 - gYグリッド補正値)
            Case "L"
                offsサイズ = New Size(-XMMマウスの移動距離 + gXグリッド補正値, 0)
                offp画像座標 = New Size(XMMマウスの移動距離 - gXグリッド補正値, 0)
                dpp描画offset = New Size(-XMMマウスの移動距離 + gXグリッド補正値, 0)
            Case "R"
                offsサイズ = New Size(XMMマウスの移動距離 - gXグリッド補正値, 0)
            Case "LU"
                offsサイズ = New Size(-XMMマウスの移動距離 + gXグリッド補正値, -YMMマウスの移動距離 + gYグリッド補正値)
                offp画像座標 = New Size(XMMマウスの移動距離 - gXグリッド補正値, YMMマウスの移動距離 - gYグリッド補正値)
                dpp描画offset = New Size(-XMMマウスの移動距離 + gXグリッド補正値, -YMMマウスの移動距離 + gYグリッド補正値)
            Case "RU"
                offsサイズ = New Size(XMMマウスの移動距離 - gXグリッド補正値, -YMMマウスの移動距離 + gYグリッド補正値)
                offp画像座標 = New Size(0, YMMマウスの移動距離 - gYグリッド補正値)
                dpp描画offset = New Size(0, -YMMマウスの移動距離 + gYグリッド補正値)
            Case "RD"
                offsサイズ = New Size(XMMマウスの移動距離 - gXグリッド補正値, YMMマウスの移動距離 - gYグリッド補正値)
            Case "LD"
                offsサイズ = New Size(-XMMマウスの移動距離 + gXグリッド補正値, YMMマウスの移動距離 - gYグリッド補正値)
                offp画像座標 = New Size(XMMマウスの移動距離 - gXグリッド補正値, 0)
                dpp描画offset = New Size(-XMMマウスの移動距離 + gXグリッド補正値, 0)
        End Select

        'ExPictureの大きさを1x1以下にはしないように
        If pps画像サイズ.Width + offsサイズ.Width <= 1 Or pps画像サイズ.Height + offsサイズ.Height <= 1 Then
            offsサイズ = New Size(0, 0)
            offp画像座標 = New Size(0, 0)
            dpp描画offset = New Size(0, 0)
        End If

        '右下、下、右のラベルでシフトキーを押しながらでアスペクト比保持の拡大縮小
        If My.Computer.Keyboard.ShiftKeyDown Then
            Dim xmag As Single = (pps画像サイズ.Width + offsサイズ.Width) / pps画像サイズ.Width
            Dim ymag As Single = (pps画像サイズ.Height + offsサイズ.Height) / pps画像サイズ.Height

            Select Case L.Name
                Case "RD"
                    If xmag >= ymag Then
                        pps画像サイズ.Height *= xmag
                        pps画像サイズ.Width += offsサイズ.Width
                    Else
                        pps画像サイズ.Width *= ymag
                        pps画像サイズ.Height += offsサイズ.Height
                    End If

                Case "D"
                    pps画像サイズ.Width *= ymag
                    pps画像サイズ.Height += offsサイズ.Height
                Case "R"
                    pps画像サイズ.Height *= xmag
                    pps画像サイズ.Width += offsサイズ.Width
                Case Else
                    pps画像サイズ += offsサイズ 'このCase Elseは大事
            End Select

        Else
            pps画像サイズ += offsサイズ

        End If
        'pps画像サイズ += offsサイズ
        ppp画像の座標 += offp画像座標


        'ExPictureのサイズと位置をセット
        EditNowPic.Size = pps画像サイズ
        EditNowPic.Location = ppp画像の座標

        '描画用の座標セット、ExPictureにPathPointに書き込み
        Dim ps() As PointF = DirectCast(EditNowPic.PathPoints.ToArray, PointF())
        If myForm3.CheckBoxLinkedPictureSize頂点と画像サイズ連動.Checked Then
            '画像サイズに合わせて描画画像も拡大縮小する
            Dim xpp As Single = pps画像サイズ.Width / pps画像元サイズ.Width
            Dim ypp As Single = pps画像サイズ.Height / pps画像元サイズ.Height
            For i = 0 To UBound(ps)
                Dim x As Single = ps(i).X * xpp
                Dim y As Single = ps(i).Y * ypp
                EditNowPic.PathPoints(i) = New PointF(x, y)

            Next
        Else
            '描画座標は画像サイズに合わせないで独立
            'マウスの動きと上下左右全てが逆になる
            For i As Integer = 0 To ps.Count - 1 ' UBound(ps)
                'ps(i).Offset(dpp描画offset)
                ps(i) += dpp描画offset
                EditNowPic.PathPoints.Item(i) = ps(i)
            Next

        End If

        '        PointF 演算子(System.Drawing)
        'http://msdn.microsoft.com/ja-jp/library/ff986570(v=vs.110).aspx

        'Dim p1 As New Point(1, 1)
        'Dim s1 As New Size(20, 20)
        'Dim sf1 As New SizeF(1.5, 0.1)
        'Dim f1 As New PointF(0.1, 0.2)
        'Dim f2 As New PointF(0.1, 0.01)
        'Dim p2 As New Point(2, 2)
        'Dim p3 As New Point(3, 30)

        'Dim addp As New Point(p1 + p2)
        'addp = New Point(p1 + s1)
        'addp = p1 + p2
        'addp = p3 + p2
        'Dim addf As PointF

        ''addf = f1 + f2 'これはエラー
        'addf = f1 + s1
        'addf = f1 + p1
        ''addf = p1 + f1 'エラー
        'addf = f1 + sf1












        '頂点ラベルの移動
        If L.Name <> "R" Or L.Name <> "RD" Or L.Name <> "D" Then
            'Call Draw再描画() '←これが重くなりメモリ食いの原因、処理に見合ったものではないので不使用
            ''メモリの開放
            'MemoryFreeCount += 1
            'If MemoryFreeCount = MEMORY_FREE_LIMIT_サイズ変更用 Then
            '    Call メモリの開放()
            '    MemoryFreeCount = 0
            'End If

            '頂点ラベルの移動
            'Dim ps() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
            For i As Integer = 0 To ps.Count - 1 ' UBound(ps)
                ps(i) -= PPLabelOffset
                Point.Round(ps(i))
                'Dim ip As New Point(ps(i).X, ps(i).Y)
                Dim ipp As New Point(Point.Round(ps(i))) '小数点を整数に丸めるPoint.Round
                'EditNowPic.Controls(i).Location = ip 'ps(i)

                EditNowPic.Controls(i).Location = ipp

            Next

        End If

        'ラベルの表示情報更新、画像サイズと図形サイズが連動していると頂点座標が変わるから書いてみたけど
        '負荷が重いようならマウスアップ時だけにした方がいいかも
        If myForm3.CheckBoxLinkedPictureSize頂点と画像サイズ連動.Checked Then
            For Each ll In PPL描画頂点リスト
                Call VisiblePointNumber頂点番号表示テキスト更新だけ(ll)
            Next

        End If



    End Sub
    Private Sub ExPictureのサイズ変更(L As Label)
        ''サイズ変更用ラベルのMouseMoveで使用

        'Dim soサイズオフセット As New Point(0, 0)
        'Dim zo座標オフセット As New Point(0, 0)
        'Dim pps画像サイズ As New Point(EditNowPic.Size)
        'Dim pp画像の座標 As New Point(EditNowPic.Location)
        'Dim dpp描画offset As New Point(0, 0) '描画頂点のオフセット用
        ''グリッド
        ''スクロールバーの移動分は要らなかった…
        ''Dim xS As Integer = Me.Panel2.AutoScrollPosition.X 'スクロールバーのぶん
        ''Dim yS As Integer = Me.Panel2.AutoScrollPosition.Y
        'Dim gridValue As Integer = Me.NumericUpDownGrid.Value
        ''Dim gXグリッド補正値 As Integer = (XMMマウスの移動距離 Mod gridValue) + xS
        ''Dim gYグリッド補正値 As Integer = (YMMマウスの移動距離 Mod gridValue) + yS
        'Dim gXグリッド補正値 As Integer = (XMMマウスの移動距離 Mod gridValue)
        'Dim gYグリッド補正値 As Integer = (YMMマウスの移動距離 Mod gridValue)


        ''グリッドに合わせないとき
        'If myForm3.CheckBoxShapeSizeGridFit.Checked = False Then
        '    gXグリッド補正値 = 0
        '    gYグリッド補正値 = 0

        'End If

        'Select Case L.Name
        '    Case "U"
        '        soサイズオフセット = New Point(0, -YMMマウスの移動距離 + gYグリッド補正値)
        '        zo座標オフセット = New Point(0, YMMマウスの移動距離 - gYグリッド補正値)
        '        dpp描画offset = New Point(0, -YMMマウスの移動距離 + gYグリッド補正値)
        '    Case "D"
        '        soサイズオフセット = New Point(0, YMMマウスの移動距離 - gYグリッド補正値)
        '        zo座標オフセット = New Point(0, 0)
        '    Case "L"
        '        soサイズオフセット = New Point(-XMMマウスの移動距離 + gXグリッド補正値, 0)
        '        zo座標オフセット = New Point(XMMマウスの移動距離 - gXグリッド補正値, 0)
        '        dpp描画offset = New Point(-XMMマウスの移動距離 + gXグリッド補正値, 0)
        '    Case "R"
        '        soサイズオフセット = New Point(XMMマウスの移動距離 - gXグリッド補正値, 0)
        '        zo座標オフセット = New Point(0, 0)
        '    Case "LU"
        '        soサイズオフセット = New Point(-XMMマウスの移動距離 + gXグリッド補正値, -YMMマウスの移動距離 + gYグリッド補正値)
        '        zo座標オフセット = New Point(XMMマウスの移動距離 - gXグリッド補正値, YMMマウスの移動距離 - gYグリッド補正値)
        '        dpp描画offset = New Point(-XMMマウスの移動距離 + gXグリッド補正値, -YMMマウスの移動距離 + gYグリッド補正値)
        '    Case "RU"
        '        soサイズオフセット = New Point(XMMマウスの移動距離 - gXグリッド補正値, -YMMマウスの移動距離 + gYグリッド補正値)
        '        zo座標オフセット = New Point(0, YMMマウスの移動距離 - gYグリッド補正値)
        '        dpp描画offset = New Point(0, -YMMマウスの移動距離 + gYグリッド補正値)
        '    Case "RD"
        '        soサイズオフセット = New Point(XMMマウスの移動距離 - gXグリッド補正値, YMMマウスの移動距離 - gYグリッド補正値)
        '        zo座標オフセット = New Point(0, 0)
        '    Case "LD"
        '        soサイズオフセット = New Point(-XMMマウスの移動距離 + gXグリッド補正値, YMMマウスの移動距離 - gYグリッド補正値)
        '        zo座標オフセット = New Point(XMMマウスの移動距離 - gXグリッド補正値, 0)
        '        dpp描画offset = New Point(-XMMマウスの移動距離 + gXグリッド補正値, 0)
        'End Select

        ''ExPictureの大きさを1x1以下にはしないように
        'If pps画像サイズ.X + soサイズオフセット.X < 1 Or pps画像サイズ.Y + soサイズオフセット.Y < 1 Then
        '    soサイズオフセット = New Point(0, 0)
        '    zo座標オフセット = New Point(0, 0)
        '    dpp描画offset = New Point(0, 0)
        'End If


        ''ExPictureのサイズと位置をセット
        'pps画像サイズ.Offset(soサイズオフセット)
        'pp画像の座標.Offset(zo座標オフセット)

        'EditNowPic.Size = pps画像サイズ
        'EditNowPic.Location = pp画像の座標

        ''
        ''Dim xos As Integer = soサイズオフセット.X
        ''Dim yos As Integer = soサイズオフセット.Y
        ''Dim xp As Single = xos / pps画像サイズ.X
        ''Dim yp As Single = yos / pps画像サイズ.Y
        ''Dim ps() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
        ''For i As Integer = 0 To UBound(ps)
        ''    Dim x As Integer = ps(i).X * xp
        ''    Dim y As Integer = ps(i).Y * yp
        ''    Dim p As New Point(x, y)
        ''    ps(i).Offset(p)
        ''    EditNowPic.PathPoints.Item(i) = ps(i)
        ''Next

        ''        PointF 演算子(System.Drawing)
        ''http://msdn.microsoft.com/ja-jp/library/ff986570(v=vs.110).aspx

        ''Dim p1 As New Point(1, 1)
        ''Dim s1 As New Size(20, 20)
        ''Dim sf1 As New SizeF(1.5, 0.1)
        ''Dim f1 As New PointF(0.1, 0.2)
        ''Dim f2 As New PointF(0.1, 0.01)
        ''Dim p2 As New Point(2, 2)
        ''Dim p3 As New Point(3, 30)

        ''Dim addp As New Point(p1 + p2)
        ''addp = New Point(p1 + s1)
        ''addp = p1 + p2
        ''addp = p3 + p2
        ''Dim addf As PointF

        ' ''addf = f1 + f2 'これはエラー
        ''addf = f1 + s1
        ''addf = f1 + p1
        ' ''addf = p1 + f1 'エラー
        ''addf = f1 + sf1











        ''描画用の座標セット、ExPictureにPathPointに書き込み
        ''マウスの動きと上下左右全てが逆になる
        'Dim ps() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
        'For i As Integer = 0 To UBound(ps)
        '    ps(i).Offset(dpp描画offset)

        '    EditNowPic.PathPoints.Item(i) = ps(i)
        'Next

        ''頂点ラベルの移動
        'If L.Name <> "R" Or L.Name <> "RD" Or L.Name <> "D" Then
        '    'Call Draw再描画() '←これが重くなりメモリ食いの原因、処理に見合ったものではないので不使用
        '    ''メモリの開放
        '    'MemoryFreeCount += 1
        '    'If MemoryFreeCount = MEMORY_FREE_LIMIT_サイズ変更用 Then
        '    '    Call メモリの開放()
        '    '    MemoryFreeCount = 0
        '    'End If

        '    '頂点ラベルの移動
        '    'Dim ps() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
        '    For i As Integer = 0 To UBound(ps)
        '        ps(i) -= PPLabelOffset
        '        EditNowPic.Controls(i).Location = ps(i)

        '    Next

        'End If


    End Sub


    Private Function サイズ変更ラベルの位置配列作成(pb As Rectangle) As Point()
        Dim picL As Integer = pb.Left
        Dim picT As Integer = pb.Top
        Dim picW As Integer = pb.Width
        Dim picH As Integer = pb.Height


        '各ラベルの初期座標の配列作成()
        Dim pLU As New Point(pb.Location)
        Dim pLD As New Point(picL, picT + picH)
        Dim pRU As New Point(picL + picW, picT)
        Dim pRD As New Point(picL + picW, picT + picH)
        Dim pU As New Point(picL + (picW / 2), picT)
        Dim pD As New Point(picL + (picW / 2), picT + picH)
        Dim pL As New Point(picL, picT + (picH / 2))
        Dim pR As New Point(picL + picW, picT + (picH / 2))
        Dim ps() As Point = {pLU, pLD, pRU, pRD, pU, pD, pL, pR}
        Return ps

        'Dim ul As Label = Me.Panel2.Controls("uLine")
        'Dim ll As Label = Me.Panel2.Controls("lLine")
        'Dim l As Integer = ul.Left
        'Dim t As Integer = ul.Top
        'Dim w As Integer = ul.Width
        'Dim h As Integer = ll.Height
        'Dim pLU As New Point(ul.Location)
        'Dim pLD As New Point(l, t + h)
        'Dim pRU As New Point(l + w, t)
        'Dim pRD As New Point(l + w, t + h)
        'Dim pU As New Point(l + (w / 2), t)
        'Dim pD As New Point(l + (w / 2), t + h)
        'Dim pL As New Point(l, t + (h / 2))
        'Dim pR As New Point(l + w, t + (h / 2))
        'Dim ps() As Point = {pLU, pLD, pRU, pRD, pU, pD, pL, pR}
        'Return ps

    End Function
    Private Sub サイズ変更用ラベルの位置(L As Label)
        Dim oSize As New Point(0, 0)
        Dim oLocate As New Point(0, 0)
        Dim oPath As New Point(0, 0)

        Dim gv As Integer = Me.NumericUpDownGrid.Value
        Dim gx As Integer = XMMマウスの移動距離 Mod gv
        Dim gy As Integer = YMMマウスの移動距離 Mod gv

        If myForm3.CheckBoxGridFitShape図形をグリッドに合わせる.Checked = False Then
            gx = 0
            gy = 0

        End If

        Select Case L.Name
            Case "U"
                oSize = New Point(0, -YMMマウスの移動距離 + gy)
                oLocate = New Point(0, YMMマウスの移動距離 - gy)
                oPath = New Point(0, -YMMマウスの移動距離 + gy)
            Case "D"
                oSize = New Point(0, YMMマウスの移動距離 - gy)
                oLocate = New Point(0, 0)
            Case "L"
                oSize = New Point(-XMMマウスの移動距離 + gx, 0)
                oLocate = New Point(XMMマウスの移動距離 - gx, 0)
                oPath = New Point(-XMMマウスの移動距離 + gx, 0)
            Case "R"
                oSize = New Point(XMMマウスの移動距離 - gx, 0)
                oLocate = New Point(0, 0)
            Case "LU"

            Case "RU"

            Case "RD"

            Case "LD"

        End Select


        Dim pSize As New Point(EditNowPic.Size)
        Dim pLocate As New Point(EditNowPic.Location)
        'ExPictureの大きさを1x1以下にはしないように
        If pSize.X + oSize.X < 1 Or pSize.Y + oSize.Y < 1 Then
            oSize = New Point(0, 0)
            oLocate = New Point(0, 0)
            oPath = New Point(0, 0)
        End If

        '元のExPictureにサイズと位置をオフセット
        pSize.Offset(oSize)
        pLocate.Offset(oLocate)
        TempRect変更中 = New Rectangle(pLocate, pSize) '仮のレクとアングル、ここからサイズや座標を取得して設定していく

        ''頂点座標セット、これはマウスアップでやったほうがいい？
        'Dim ps() As Point = DirectCast(EditNowPic.PathPoints.ToArray, Point())
        'For i As Integer = 0 To UBound(ps)
        '    ps(i).Offset(oPath)
        '    EditNowPic.PathPoints.Item(i) = ps(i)
        'Next



        Call PicBorderMove枠の移動(TempRect変更中)
        'Call サイズ変更中用のラベルの再表示(TempRect変更中)


    End Sub
    Private Sub PicBorderMove枠の移動(rect As Rectangle)
        If BorderLineLabel枠表示用ラベル.Count = 0 Then Exit Sub

        Dim w As Integer = rect.Width
        Dim h As Integer = rect.Height
        Dim t As Integer = rect.Top
        Dim l As Integer = rect.Left
        'Dim bLine() As String = {"uLine", "dLine", "lLine", "rLine"}
        Dim lSize() As Size = {New Size(w, 1), New Size(w, 1), New Size(1, h), New Size(1, h)}
        Dim lLocate() As Point = {New Point(l, t), New Point(l, t + h), New Point(l, t), New Point(l + w, t)}
        Dim linePo() As Point = {New Point(w, 0), New Point(w, 0), New Point(0, h), New Point(0, h)}

        For i As Integer = 0 To 3
            Dim canvas As New Bitmap(lSize(i).Width, lSize(i).Height)
            Dim g As Graphics = Graphics.FromImage(canvas)
            Dim d As DashStyle = DashStyle.Dot
            Dim Pen = New Pen(Brushes.Blue)
            Pen.DashStyle = d
            g.DrawLine(Pen, New Point(0, 0), linePo(i))


            With BorderLineLabel枠表示用ラベル(i)
                .Location = lLocate(i)
                .Size = lSize(i)
                .Image = canvas
            End With
            g.Dispose()
            Pen.Dispose()

        Next
    End Sub
    Private Sub 枠の表示初期化(rect As Rectangle) '未使用
        Dim w As Integer = rect.Width
        Dim h As Integer = rect.Height
        Dim t As Integer = rect.Top
        Dim l As Integer = rect.Left

        Dim bLine() As String = {"uLine", "dLine", "lLine", "rLine"}
        Dim lSize() As Size = {New Size(w, 1), New Size(w, 1), New Size(1, h), New Size(1, h)}
        Dim lLocate() As Point = {New Point(l, t), New Point(l, t + h), New Point(l, t), New Point(l + w, t)}

        For i As Integer = 0 To 3
            Dim newL As New Label
            With newL
                .Location = lLocate(i)
                .Size = lSize(i)
                .BackColor = Color.Red
                .Name = bLine(i)
            End With
            Me.Panel2.Controls.Add(newL)
            newL.BringToFront()
            BorderLineLabel枠表示用ラベル.Add(newL)

        Next

    End Sub
    Friend Sub PicBoderLineLabel画像に枠を作成表示(EXP As ExPictureBox)
        'If EXP.IsEdit = False Then Exit Sub

        If BorderLineLabel枠表示用ラベル.Count >= 0 Then
            Call PicBoderLineLabel画像に付けた枠を消去()
        End If

        Dim rect As Rectangle = EXP.Bounds

        Dim w As Integer = rect.Width
        Dim h As Integer = rect.Height
        Dim t As Integer = rect.Top
        Dim l As Integer = rect.Left

        Dim bLine() As String = {"uLine", "dLine", "lLine", "rLine"}
        Dim lSize() As Size = {New Size(w, 1), New Size(w, 1), New Size(1, h), New Size(1, h)}
        Dim lLocate() As Point = {New Point(l, t), New Point(l, t + h), New Point(l, t), New Point(l + w, t)}
        Dim linePo() As Point = {New Point(w, 0), New Point(w, 0), New Point(0, h), New Point(0, h)}

        For i As Integer = 0 To 3
            Dim canvas As New Bitmap(lSize(i).Width, lSize(i).Height)
            Dim g As Graphics = Graphics.FromImage(canvas)
            'Dim b As New SolidBrush(Color.Blue)
            Dim d As DashStyle = DashStyle.Dot
            Dim Pen = New Pen(Brushes.Blue)
            Pen.DashStyle = d
            'g.FillRectangle(b, 0, 0, canvas.Width, canvas.Height)
            g.DrawLine(Pen, New Point(0, 0), linePo(i))

            Dim newL As New Label
            With newL
                .Location = lLocate(i)
                .Size = lSize(i)
                .BackColor = Color.Cyan
                .Image = canvas
                .Name = bLine(i)
            End With
            Me.Panel2.Controls.Add(newL)
            newL.BringToFront()
            BorderLineLabel枠表示用ラベル.Add(newL)

            g.Dispose()
            Pen.Dispose()

        Next

    End Sub
    Friend Sub PicBoderLineLabel画像に付けた枠を消去()
        If BorderLineLabel枠表示用ラベル.Count = 0 Then Exit Sub

        For i = 0 To BorderLineLabel枠表示用ラベル.Count - 1
            Me.Panel2.Controls.Remove(BorderLineLabel枠表示用ラベル(i))
            BorderLineLabel枠表示用ラベル.Item(i).Dispose()

        Next
        BorderLineLabel枠表示用ラベル.Clear()

    End Sub

    '--------------------------------図形2ExPictureBoxのサイズ変更ここまで--------------------------------
    '#End Region

    ''' <summary>
    ''' 背景用画像を作成して返す、透過処理した画像
    ''' </summary>
    ''' <param name="EXP">ExPictureBox</param>
    ''' <param name="exclude">Trueで自分自身を描画する</param>
    ''' <param name="Border">Falseで枠を描画しない</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    ''' 
    Private Function BGImage背景画像作成ExPictureBox用(EXP As ExPictureBox,
                                                Optional exclude As Boolean = True,
                                                Optional Border As Boolean = False) As Bitmap


        'Dim canvas As New Bitmap(RigthDownPoint.X, RigthDownPoint.Y)
        'Dim canvas As New Bitmap(Me.Panel2.Width, Me.Panel2.Height)
        Dim canvas As New Bitmap(EXP.Width, EXP.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)

        Dim zx As Integer = AbsolutePoint(EXP).X
        Dim zy As Integer = AbsolutePoint(EXP).Y

        For Each c As ExPictureBox In myPicArR '背景画像
            Dim ax As Integer = AbsolutePoint(c).X
            Dim ay As Integer = AbsolutePoint(c).Y
            Dim dp As New Point(ax - zx, ay - zy) '自分との相対位置
            If exclude Then
                '自分自身は除く
                If c.Tag <> EXP.Tag Then
                    g.DrawImage(c.Image, dp)
                    'g.DrawImage(c.Image, AbsolutePoint(c))
                End If

            Else
                '自分自身も含める
                g.DrawImage(c.Image, dp)

            End If
        Next

        '枠の描画
        If Border Then
            'Dim p1 As New Pen(Color.FromArgb(255, 0, 0, 0))
            Dim p1 As New Pen(Color.FromArgb(255, 255, 255, 255))
            Dim p2 As New Pen(Color.FromArgb(255, 0, 255, 200))
            Dim ds As DashStyle = DashStyle.Dot
            p2.DashStyle = ds
            g.DrawRectangle(p1, 0, 0, EXP.Width - 1, EXP.Height - 1) '枠
            g.DrawRectangle(p2, 0, 0, EXP.Width - 1, EXP.Height - 1)
            p1.Dispose()
            p2.Dispose()

        End If
        g.Dispose()


        Return canvas

    End Function
    Private Sub BGImage背景画像作成Panel2用()
        Dim canvas As New Bitmap(Me.Panel2.DisplayRectangle.Width, Me.Panel2.DisplayRectangle.Height)
        Dim ooo = FullSize

        Dim g As Graphics = Graphics.FromImage(canvas)
        For Each c As ExPictureBox In myPicArR
            Dim p As New Point(AbsolutePoint(c))

            g.DrawImage(c.Image, p)

        Next
        Me.Panel2.BackgroundImage = canvas
        g.Dispose()

    End Sub
    Private Sub VisiblePointNumber頂点番号表示テキスト更新だけ(LL As Label)
        '頂点ラベルの移動専用
        Dim n As Boolean = myForm3.CheckBoxVisiblePointNumber頂点番号表示.Checked
        Dim z As Boolean = myForm3.CheckBoxVisiblePointLocate頂点座標表示.Checked

        Dim zt As String = EditNowPic.PathPoints(LL.Tag).ToString
        Dim nt As String = Format(LL.Tag, "00")
        If n And z Then
            LL.Text = nt & vbNewLine & zt
        ElseIf n Then
            LL.Text = nt

        ElseIf z Then
            LL.Text = zt

        End If

    End Sub
    Friend Sub VisiblePointState頂点番号と座標表示(LL As Label)
        '編集終了で消去
        If isDrawEditNow = False Then Exit Sub
        Dim n As Boolean = myForm3.CheckBoxVisiblePointNumber頂点番号表示.Checked
        Dim z As Boolean = myForm3.CheckBoxVisiblePointLocate頂点座標表示.Checked
        LL.ForeColor = Color.White
        LL.Font = New Font("Meiryo UI", 9)

        'ラベルの大きさ決定
        Dim LS As Integer
        If isLabelSizeSmall Then
            LS = LABEL_SIZE_S
        Else
            LS = LABEL_SIZE
        End If

        '背景画像作成
        Dim col As Color = Color.FromArgb(0, 0, 0, 0) 'ラベルの色、BackcolorとImage用
        If n Or z Then
            col = Color.FromArgb(50, 0, 0, 0)
        End If
        Dim colKuro As Color = Color.FromArgb(255, 255, 255, 255) 'ラベルの枠の色、Image専用
        Dim colbベジェ曲線 As Color = Color.FromArgb(50, 0, 0, 255) '制御点用image
        Dim canvas As New Bitmap(LS, LS)
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim mypen As New Pen(Brushes.Black) 'Pen
        Dim myb As New SolidBrush(col) 'ブラシ
        mypen.DashStyle = DashStyle.Dot

        If LL.Tag Mod 3 <> 0 And EditNowPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
            'ベジェ曲線の制御点用
            Dim bc As Color = Color.Blue
            myb = New SolidBrush(colbベジェ曲線)
            col = Color.FromArgb(50, colbベジェ曲線)
            mypen = New Pen(Brushes.Blue)

            mypen.DashStyle = DashStyle.Dot

        End If

        g.FillRectangle(myb, 0, 0, canvas.Width, canvas.Height) 'ラベル
        g.DrawRectangle(Pens.White, 0, 0, canvas.Width - 1, canvas.Height - 1) 'ラベルの枠の描画
        g.DrawRectangle(mypen, 0, 0, canvas.Width - 1, canvas.Height - 1) 'ラベルの枠の描画
        Dim img As Image = canvas
        g.Dispose()
        myb.Dispose()
        mypen.Dispose()

        Dim zt As String = EditNowPic.PathPoints(LL.Tag).ToString
        Dim nt As String = Format(LL.Tag, "00")


        '
        If n = True And z = True Then
            '番号と座標の両方表示
            LL.AutoSize = True
            LL.Text = nt & vbNewLine & zt
            LL.TextAlign = ContentAlignment.MiddleCenter
            LL.ImageAlign = ContentAlignment.TopLeft
            LL.Image = canvas
            LL.BackColor = col

        ElseIf n Then
            '番号だけ表示
            LL.AutoSize = False
            LL.Text = nt
            LL.TextAlign = ContentAlignment.MiddleRight
            LL.ImageAlign = ContentAlignment.MiddleCenter
            LL.Image = canvas
            LL.BackColor = Nothing
        ElseIf z Then
            '座標だけ表示
            LL.AutoSize = True
            LL.Text = zt ' LL.Location.ToString
            LL.Image = Nothing
            LL.BackColor = col

        Else
            LL.AutoSize = False
            LL.Text = Nothing
            LL.ImageAlign = ContentAlignment.MiddleCenter
            LL.Image = canvas
            LL.BackColor = Nothing
        End If


    End Sub
    Friend Sub VisiblePointState頂点表示用にラベルを渡す()
        For Each l As Label In PPL描画頂点リスト
            Call VisiblePointState頂点番号と座標表示(l)

        Next
    End Sub

    Friend Sub Bezierベジェ曲線の開閉()
        '開いたベジェ曲線で線を閉じるにした場合は始点と終点を重ねる、始点を終点に移動
        '制御点は連動移動にチェックありなら一緒に移動
        If EditNowPic.GraphicDrawType <> ExPictureBox.DrawType.ベジェ曲線 Then Exit Sub

        Dim PP As List(Of PointF) = EditNowPic.PathPoints
        Dim stap As New PointF ' Point(PP.Item(0)) '始点
        Dim endp As New PointF 'Point(PP.Item(PP.Count - 1)) '終点
        Dim cstap As New PointF 'Point(PP.Item(1)) '始点の制御点
        Dim cendp As New PointF 'Point(PP.Item(PP.Count - 2)) '終点の制御点
        stap = PP(0)
        endp = PP(PP.Count - 1)
        cstap = PP(1)
        cendp = PP(PP.Count - 2)

        If myForm3.CheckBoxLineClose線を閉じる.Checked Then
            'Dim offp As New Point(endp.X - stap.X, endp.Y - stap.Y) '始点と終点の差
            'cstap.Offset(offp) '制御点をオフセット

            'PP.Item(0) = endp '始点座標を書き込み
            'If myForm3.CheckBoxLinkedMove連動移動.Checked Then
            '    PP.Item(1) = cstap '制御点座標を書込
            'End If
            'Call 頂点座標の計算(PPL描画頂点リスト(1))

            Dim pts() As PointF = DirectCast(EditNowPic.PathPoints.ToArray, PointF())
            Call AddPointアンカーポイントの継ぎ足し(pts, True)

        Else '閉じた状態から開いた状態へ
            '終点の一個前のアンカーポイントと反対方向へ移動
            Dim x As Single = endp.X - cendp.X
            Dim y As Single = endp.Y - cendp.Y
            'Dim offp As New Point(x / 10, y / 10)
            Dim offp As New SizeF(x / 10, y / 10)
            x = x / 10
            y = y / 10





            'stap.Offset(offp)
            'cstap.Offset(offp)
            stap += offp
            cstap += offp

            PP.Item(0) = stap
            If myForm3.CheckBoxLinkedMove連動移動.Checked Then
                PP.Item(1) = cstap
            End If
            Call 頂点座標の計算(PPL描画頂点リスト(1))
        End If


        Call LabelAdd_頂点ラベルの全部作成(EditNowPic)

    End Sub


    Friend Sub MouseDrawマウスで描画()
        If isMouseDeDrawNow Then Exit Sub

        isMouseDeDrawNow = True
        Dim bmp As New Bitmap(Me.Panel2.Width, Me.Panel2.Height)
        'Dim g As Graphics = Graphics.FromImage(bmp)
        'g.DrawRectangle(Pens.Green, 0, 0, bmp.Width - 1, bmp.Height - 1)
        'g.Dispose()

        Call PicBoxAdd("マウスで描画", bmp, isMouseDraw:=True)
        '今の画像を背景にしたExPictureを作成、大きさは今のpanel2の大きさ
        MouseDeDrawPic = ActExPic 'マウスで描画しているExPicture、目印用
        MouseDeDrawPic.Cursor = Cursors.Cross
        ToolStripMenuItemSaveT.Visible = False
        ToolStripMenuItemSave.Visible = False
        ToolStripMenuItem頂点の編集開始.Visible = False
        ToolStripMenuItemマウスで描画を終了.Visible = True


        'グリッドに移動
        Call ExpGridMoveグリッド合わせ(MouseDeDrawPic)
        Dim bgi As Bitmap = BGImage背景画像作成ExPictureBox用(MouseDeDrawPic)
        MouseDeDrawPic.BackgroundImage = bgi
        Call Transparent4()

    End Sub


    Private Sub MouseDrawBezierベジェ曲線(MP As Point)
        'マウスクリックでベジェ曲線の描画
        '制御点の座標を計算してPathPointに書き込む
        'p0 = New PointF(90, 60)
        'p1 = New PointF(180, 60)
        'p2 = New PointF(360, 60)
        'p3 = New PointF(330, 120)
        'p4 = New PointF(300, 180)
        'p5 = New PointF(150, 210)
        'p6 = New Point(60, 210)
        'p6がマウスカーソル、p0,p3がアンカーポイントで固定座標のとき制御点のp2,p4,p5の座標を求める
        '求める順番は4,2,5、2は4の対角線上、2,4の3からの距離は0から6の距離の4分の1に設定
        '角度も0から6の角度と同じにする
        '5の6からの距離は3と6の距離の4分の1、角度は4と6の角度と同じに設定
        Dim p0, p1, p2, p3, p4, p5 As PointF
        'Dim p6 As Point
        Dim PP() As PointF = DirectCast(MouseDeDrawPic.PathPoints.ToArray, PointF())
        Dim endP As Integer = PP.Length
        Const dKyori As Integer = 4 '制御点距離、4で4分の1になる、適当

        If endP >= 6 Then

            p0 = PP(endP - 7)
            'p1 = PP(endP - 6)
            p2 = PP(endP - 5)
            p3 = PP(endP - 4)
            p4 = PP(endP - 3)
            p5 = PP(endP - 2)
            'p0とp6間の距離
            'Dim hSyahen As Single = kyori二点間の距離＿斜辺の長さ(p3, MP)
            If kyori二点間の距離＿斜辺の長さ(p3, MP) = 0 Then Exit Sub

            Dim Syahen As Single = kyori二点間の距離＿斜辺の長さ(p0, MP, True)


            Dim cKyori As Single = Syahen / dKyori

            Dim Rinpen As Single = MP.X - p0.X
            Dim Taihen As Single = MP.Y - p0.Y
            Dim cos As Single = Rinpen / Syahen
            Dim sin As Single = Taihen / Syahen

            p4.X = p3.X + (cKyori * cos)
            p4.Y = p3.Y + (cKyori * sin) '04の座標決定

            p2.X = p3.X - p4.X + p3.X
            p2.Y = p3.Y - p4.Y + p3.Y '02の座標決定

            Syahen = kyori二点間の距離＿斜辺の長さ(MP, p3, True)
            cKyori = Syahen / dKyori
            Rinpen = p4.X - MP.X
            Taihen = p4.Y - MP.Y
            cos = Rinpen / Syahen
            sin = Taihen / Syahen

            p5.X = MP.X + (cKyori * cos)
            p5.Y = MP.Y + (cKyori * sin) '05の座標決定

            'Syahen = kyori二点間の距離＿斜辺の長さ(p0, p3, True)
            'cKyori = Syahen / dKyori
            'Rinpen = p2.X - p0.X
            'Taihen = p2.Y - p0.Y
            'cos = Rinpen / Syahen
            'sin = Taihen / Syahen
            'p1.X = p0.X + (cKyori * cos)
            'p1.Y = p0.Y + (cKyori * sin)

            '書き込み
            Dim PPL As List(Of PointF) = MouseDeDrawPic.PathPoints
            PPL(endP - 3) = p4
            PPL(endP - 5) = p2
            PPL(endP - 2) = p5
            'PPL(endP - 6) = p1
            PPL(endP - 1) = MP

        Else
            '頂点数が4の時
            p0 = PP(0)
            p1 = PP(1)
            p2 = PP(2)
            Dim Syahen As Single = kyori二点間の距離＿斜辺の長さ(p0, MP)
            Dim cKyori As Single = Syahen / dKyori
            Dim cos As Single = (MP.X - p0.X) / Syahen
            Dim sin As Single = (MP.Y - p0.Y) / Syahen
            Dim dx As Single = cKyori * cos
            Dim dy As Single = cKyori * sin

            p1.X = p0.X + dx
            p1.Y = p0.Y + dy
            p2.X = MP.X - dx
            p2.Y = MP.Y - dy

            MouseDeDrawPic.PathPoints(1) = p1
            MouseDeDrawPic.PathPoints(2) = p2
            MouseDeDrawPic.PathPoints(3) = MP


        End If





    End Sub

    Friend Sub MouseEndDrawマウスで描画終了処理()
        If isMouseDeDrawNow = False Then Exit Sub

        isMouseDeDrawNow = False
        MouseDeDrawPic.Cursor = Cursors.Default
        MouseDeDrawPic.BackgroundImage = Nothing
        Dim pCount As Integer = MouseDeDrawPic.PathPoints.Count

        If MouseDeDrawPic.GraphicDrawType = ExPictureBox.DrawType.ベジェ曲線 Then
            'ベジェ曲線の時
            If pCount <= 4 Then
                '頂点数が4以下なら画像を消去
                Call DelPic(MouseDeDrawPic)
            Else
                Dim PP As List(Of PointF) = MouseDeDrawPic.PathPoints
                PP.RemoveRange(pCount - 3, 3)

                'Call Draw再描画(MouseDeDrawPic, True)
                Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic, True)
                MouseDeDrawPic.Image = bmp
                Call ore加工した画像を各アレイリストに書き込む(MouseDeDrawPic, bmp)


            End If
        Else
            'ベジェ曲線以外の時
            '頂点の数が1以下ならExPictureを削除する
            If pCount <= 1 Then
                Call DelPic(MouseDeDrawPic)
            ElseIf pCount = 2 And (MouseDeDrawPic.isFill Or MouseDeDrawPic.CloseLine) Then
                Call DelPic(MouseDeDrawPic)

            Else
                '頂点の数が2個以上なら最終描画
                'Call Draw再描画(MouseDeDrawPic)
                Dim bmp As Bitmap = DrawShape2図形2描画ドラッグ用サイズ変更なし版(MouseDeDrawPic, True)
                MouseDeDrawPic.Image = bmp
                Call ore加工した画像を各アレイリストに書き込む(MouseDeDrawPic, bmp)
            End If

        End If

        'サイズぴったり用処理開始
        Call myForm3.StartEdit編集開始(MouseDeDrawPic)
        Call DrawLineFitSizeピッタリ(MouseDeDrawPic)
        Call CloseEdit編集終了()
        'サイズぴったり用処理終了

        MouseDeDrawPic = Nothing '判定用を空にする
        MouseDeDrawPicPast = Nothing
        'Call CloseEdit編集終了()

        ToolStripMenuItemSaveT.Visible = True
        ToolStripMenuItemSave.Visible = True
        ToolStripMenuItemマウスで描画を終了.Visible = False
        ToolStripMenuItem頂点の編集開始.Visible = True

    End Sub
    Private Sub ExpGridMoveグリッド合わせ(EXP As ExPictureBox)
        'グリッド合わせ
        Dim grid As Integer = NumericUpDownGrid.Value
        Dim px As Integer = EXP.Location.X
        Dim py As Integer = EXP.Location.Y
        Dim asp As Point = Panel2.AutoScrollPosition 'スクロールバーの位置
        Dim absx As Integer = px - asp.X 'スクロールバーの位置を加味したEXPの絶対位置
        Dim absy As Integer = py - asp.Y

        Dim xmod As Integer = absx Mod grid '絶対位置をグリッドで割った余り
        Dim ymod As Integer = absy Mod grid

        Dim np As New Point(px - xmod, py - ymod)
        EXP.Location = np

    End Sub
    Friend Overloads Sub DrawLineFitSizeピッタリ()
        Dim exp As ExPictureBox
        exp = ActExPic
        Call DrawLineFitSizeピッタリ(exp)

    End Sub
    Friend Overloads Sub DrawLineFitSizeピッタリ(Exp As ExPictureBox)
        If Exp.IsEdit = False Then Exit Sub
        If isDrawEditNow = False Then Exit Sub

        Dim mar As Integer = Exp.ExPenWidth  'マージン
        Dim PP() As PointF = DirectCast(Exp.PathPoints.ToArray, PointF())
        Dim ExPP As List(Of PointF) = Exp.PathPoints
        Dim gp As New GraphicsPath

        'マージンの設定
        If Exp.isFill Then
            Select Case Exp.GraphicDrawType
                Case ExPictureBox.DrawType.直線
                    gp.AddPolygon(PP)
                    mar = 0
                Case ExPictureBox.DrawType.曲線
                    gp.AddClosedCurve(PP, Exp.CurveTension)
                    mar = 0
                Case ExPictureBox.DrawType.ベジェ曲線
                    gp.AddBeziers(PP)
                    mar = 0
            End Select
        ElseIf Exp.CloseLine Then
            Select Case Exp.GraphicDrawType
                Case ExPictureBox.DrawType.直線
                    gp.AddPolygon(PP)
                    mar *= 8
                Case ExPictureBox.DrawType.曲線
                    gp.AddClosedCurve(PP, Exp.CurveTension)

                Case ExPictureBox.DrawType.ベジェ曲線
                    gp.AddBeziers(PP)
                    mar *= 4
            End Select

        Else
            Select Case Exp.GraphicDrawType
                Case ExPictureBox.DrawType.直線
                    gp.AddLines(PP)
                    mar *= 8
                Case ExPictureBox.DrawType.曲線
                    gp.AddCurve(PP, Exp.CurveTension)
                Case ExPictureBox.DrawType.ベジェ曲線
                    gp.AddBeziers(PP)
                    mar *= 4
            End Select

        End If


        Dim grid As Integer = NumericUpDownGrid.Value

        Dim rectF As RectangleF = gp.GetBounds '頂点が収まる範囲を取得
        Dim rect = Rectangle.Round(rectF) '小数点丸め
        '画像サイズ変更、サイズ変更してから位置変更、この順番が大事、逆だと画面ががくがくする
        Dim pSize As New Size(rect.Width + mar, rect.Height + mar)
        'ActExPic.Size = pSize '画像サイズ変更

        Dim offp As New Point(rect.X, rect.Y)
        Dim expLocate As New Point(Exp.Location)
        Dim offx As Single = rectF.X - mar / 2
        Dim offy As Single = rectF.Y - mar / 2
        '画像位置
        expLocate += offp
        expLocate.X -= mar / 2
        expLocate.Y -= mar / 2
        Dim asp As New Point(Panel2.AutoScrollPosition)
        Dim gx As Integer = (expLocate.X - asp.X) Mod grid
        Dim gy As Integer = (expLocate.Y - asp.Y) Mod grid
        Dim offg As New Point(gx, gy)
        pSize = offg + pSize
        Exp.Size = pSize '画像サイズ変更

        expLocate.X -= gx
        expLocate.Y -= gy
        Exp.Location = expLocate '画像座標変更


        '頂点オフセット移動
        For i As Integer = 0 To UBound(PP)
            PP(i).X -= offx - gx
            PP(i).Y -= offy - gy
            ExPP(i) = PP(i)

        Next



        Call Draw再描画(Exp, pSize)
        Call LabelAdd_頂点ラベルの全部作成(Exp)
        Call サイズ変更用のラベルの再表示()
        Call 編集画像の背景更新()
        Call ダミー画像の更新()

    End Sub

    'ここまで図形2用--------------------------------


    Private Sub ToolStripMenuItem選択範囲を保存_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem選択範囲を保存.Click
        Call SelectRangeSave()

    End Sub

    Private Sub ToolStripMenuItem選択範囲をコピペ_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem選択範囲をコピペ.Click
        Call SelectRangeCopyPaste()
        '範囲選択画像を一番上にする
        Call SelectPicBoxFront範囲選択画像を最前面にする()

        'Dim Exp As ExPictureBox = selectPicbox範囲選択画像
        'If Exp.Tag = 2 Then 'コピペ画像ができた直後なので範囲選択画像は2番めになっているはず
        '    Call FocusPictureUp(selectPicbox範囲選択画像)

        '    Me.NumNowPic.Value = 2
        '    'Call UpdateThumbnail()
        'End If

    End Sub
    Friend Sub SelectPicBoxFront範囲選択画像を最前面にする()
        '範囲選択画像を一番上にする
        Dim Exp As ExPictureBox = selectPicbox範囲選択画像
        If Exp.Tag = 2 Then 'コピペ画像ができた直後なので範囲選択画像は2番めになっているはず
            Call FocusPictureUp(selectPicbox範囲選択画像)

            Me.NumNowPic.Value = 2
            'Call UpdateThumbnail()
        End If

    End Sub
    Friend Sub ColorRemap色の入れ替え(c1 As Color, c2 As Color)
        '色を入れ替えて画像を描画する: .NET Tips : C#, VB.NET
        'http://dobon.net/vb/dotnet/graphics/setremaptable.html

        If myPicAr.Count = 0 Then Exit Sub
        Dim i As Integer = ActExPic.Tag - 1
        Dim img As Image = DirectCast(myPicArBackup(i), ExPictureBox).Image
        Dim w As Integer = img.Width
        Dim h As Integer = img.Height
        Dim bmp As New Bitmap(w, h) '空のBitmap作成
        Dim g As Graphics = Graphics.FromImage(bmp) '空のBitmapから作成

        Dim cm(0) As ColorMap 'ColorMap作成、1個でも配列で作らないとImageAttributesでは使えないみたい
        cm(0) = New ColorMap

        cm(0).NewColor = c2 'Color.Red
        cm(0).OldColor = c1 'Color.Black
        Dim ia As New ImageAttributes
        ia.SetRemapTable(cm) 'ColorMapをImageAttributesにセット
        Dim rect As New Rectangle(New Point(0, 0), New Size(w, h))

        g.DrawImage(img, rect, 0, 0, w, h, GraphicsUnit.Pixel, ia)
        g.Dispose()

        ActExPic.Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録

    End Sub
    Friend Sub GetAverageColor平均色()
        If myPicAr.Count = 0 Then Exit Sub


        Dim bmp As Bitmap = ActExPic.Image
        Dim lockW As Integer = bmp.Width
        Dim lockH As Integer = bmp.Height
        Dim lockRect As New Rectangle(0, 0, lockW, lockH)
        Dim bmpdata As BitmapData = bmp.LockBits(lockRect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * lockH - 1
        Dim pixels(data) As Byte
        Dim pos As Integer
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
        Dim x, y, r, g, b, pixelCount As Long

        'Dim a As Long
        For y = 0 To bmp.Height - 1
            For x = 0 To bmp.Width - 1
                pos = bmpdata.Stride * y + x * 4
                If pixels(pos + 3) <> 0 Then '完全透明以外のピクセルだけ計算
                    'a += pixels(pos + 3)
                    r += pixels(pos + 2)
                    g += pixels(pos + 1)
                    b += pixels(pos)
                    pixelCount += 1
                End If

            Next
        Next

        bmp.UnlockBits(bmpdata)

        'a = CLng(a / pixelCount)
        r = CLng(r / pixelCount)
        g = CLng(g / pixelCount)
        b = CLng(b / pixelCount)

        'Form1.TransparentPictureBox.BackColor = Color.FromArgb(255, CInt(rs), CInt(gs), CInt(bs))
        myForm3.PictureBoxShapeColor1.BackColor = Color.FromArgb(255, r, g, b)
        'myForm3.NumericUpDownSquareTransparent.Value = a
    End Sub

    Friend Function HSVtoRGB(h As Single, s As Single, v As Single) As Color
        Dim r, g, b As Single

        If s = 0 Then
            r = v
            g = v
            b = v
        Else
            h = h / 60
            Dim i As Integer = CInt(Math.Floor(h))
            Dim f As Single = h - i
            Dim p As Single = v * (1.0F - s)
            Dim q As Single
            If i Mod 2 = 0 Then
                q = v * (1.0 - (1.0 - f) * s)
            Else
                q = v * (1.0 - f * s)

            End If

            Select Case i
                Case 0
                    r = v
                    g = q
                    b = p
                    Exit Select
                Case 1
                    r = q
                    g = v
                    b = p
                    Exit Select
                Case 2
                    r = p
                    g = v
                    b = q
                    Exit Select
                Case 3
                    r = p
                    g = q
                    b = v
                    Exit Select
                Case 4
                    r = q
                    g = p
                    b = v
                    Exit Select
                Case 5
                    r = v
                    g = p
                    b = q
                    Exit Select
                Case Else

            End Select
        End If
        r = CInt(Math.Round(r * 255))
        g = CInt(Math.Round(g * 255))
        b = CInt(Math.Round(b * 255))
        Return Color.FromArgb(r, g, b)

    End Function
    Friend Function HSLtoRGB(h As Single, s As Single, l As Single) As Color
        'h += 30

        Dim r, g, b As Single
        If s = 0 Then
            r = l
            g = l
            b = l
        Else
            h /= 60.0F
            Dim i As Integer = CInt(Math.Floor(h))
            Dim f As Single = h - i
            Dim c As Single
            If l < 0.5F Then
                c = 2.0F * s * l
            Else
                c = 2.0F * s * (1.0F - l)
            End If

            Dim m As Single = l - c / 2.0F
            Dim p As Single = c + m
            Dim q As Single
            If i Mod 2 = 0 Then
                q = l + c * (f - 0.5F)
            Else
                q = l - c * (f - 0.5F)
            End If

            Select Case i
                Case 0
                    r = p
                    g = q
                    b = m
                    Exit Select
                Case 1
                    r = q
                    g = p
                    b = m
                    Exit Select
                Case 2
                    r = m
                    g = p
                    b = q
                    Exit Select
                Case 3
                    r = m
                    g = q
                    b = p
                    Exit Select
                Case 4
                    r = q
                    g = m
                    b = p
                    Exit Select
                Case 5
                    r = p
                    g = m
                    b = q
                    Exit Select

            End Select

        End If

        Return Color.FromArgb(CInt(Math.Round(r * 255)),
                              CInt(Math.Round(g * 255)),
                              CInt(Math.Round(b * 255)))
    End Function
    Friend Sub ShiftHue色相移動(exp As ExPictureBox, oldHue As Integer, newHue As Integer, shiftH As Integer, oneColor As Boolean)
        If myPicAr.Count = 0 Then Exit Sub
        'Dim sw As New Stopwatch
        'sw.Start()

        Dim expTag As Integer = exp.Tag - 1
        Dim bmp As New Bitmap(DirectCast(myPicArBackup(expTag), ExPictureBox).Image) ' exp.Image
        Dim lockW As Integer = bmp.Width
        Dim lockH As Integer = bmp.Height
        Dim lockRect As New Rectangle(0, 0, lockW, lockH)
        Dim bmpdata As BitmapData = bmp.LockBits(lockRect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * lockH - 1
        Dim pixels(data) As Byte
        Dim pos As Integer
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
        Dim x, y As Integer
        Dim h, s, l As Single
        Dim col As Color
        'Dim oldHue As Integer = CInt(Me.LabelHueOld.Text) '変換の対象になる色の色相値
        'Dim newHue As Integer = CInt(Me.LabelHueNew.Text)
        Dim sft As Integer
        sft = newHue - oldHue '色相のシフト量
        'Dim shiftH As Integer = Me.NumericUpDownChangeHueWidth.Value '変換対象となる色相の幅
        Dim maxH As Single = (oldHue + shiftH) Mod 360 '変換対象となる色相の最高値
        Dim minH As Single = oldHue - shiftH '変換対象となる色相の最低値
        If minH < 0 Then '最低値がマイナスになる場合は360-最低値、
            minH = 360 + minH '-30だったら360-30=330になる
        End If
        '明度補正準備
        Dim lHosei As Single = myForm3.NumericUpDownBrightnessRevise明度補正.Value
        Dim lNew As Single = myForm3.TrackBarBrightnessNew.Value / 100
        Dim lListHosei As New List(Of Single) '補正値一覧表用
        Dim i As Integer = 0
        Dim lItem As Single
        For i = 0 To 255
            lItem = i ^ (1 / lHosei)
            If lItem < 0.00001 Then
                lItem = 0.00001
            End If
            lListHosei.Add(lItem)
        Next
        '明度範囲、上限、下限の確認
        Dim bv1 As Single = myForm3.MyTrackBarRangeBrightness明度範囲.Value1 / 100
        Dim bv2 As Single = myForm3.MyTrackBarRangeBrightness明度範囲.Value2 / 100
        Dim maxB, minB As Single
        If bv1 >= bv2 Then
            maxB = bv1
            minB = bv2
        Else
            maxB = bv2
            minB = bv1
        End If

        '彩度補正準備
        Dim sHosei As Single = myForm3.NumericUpDownSaturationRevise彩度補正.Value
        Dim sNew As Single = myForm3.TrackBarSaturationNew.Value / 100
        Dim sListHosei As New List(Of Single)
        Dim sItem As Single
        For i = 0 To 255
            sItem = i ^ (1 / sHosei)
            If sItem < 0.00001 Then
                sItem = 0.00001
            End If
            sListHosei.Add(sItem)
        Next
        '彩度範囲
        Dim sv1 As Single = myForm3.MyTrackBarRangeSaturation彩度範囲.Value1 / 100
        Dim sv2 As Single = myForm3.MyTrackBarRangeSaturation彩度範囲.Value2 / 100
        Dim maxS, minS As Single
        If sv1 >= sv2 Then
            maxS = sv1
            minS = sv2
        Else
            maxS = sv2
            minS = sv1
        End If
        For y = 0 To bmp.Height - 1
            For x = 0 To bmp.Width - 1
                pos = bmpdata.Stride * y + x * 4
                col = Color.FromArgb(pixels(pos + 3), pixels(pos + 2), pixels(pos + 1), pixels(pos))
                h = col.GetHue
                s = col.GetSaturation
                l = col.GetBrightness

                'If pixels(pos + 3) <> 0 Then '完全透明以外のピクセルだけ計算
                If pixels(pos + 3) <> 0 AndAlso s >= minS AndAlso s <= maxS AndAlso l >= minB AndAlso l <= maxB Then '完全透明以外のピクセルだけ計算
                    'col = Color.FromArgb(pixels(pos + 3), pixels(pos + 2), pixels(pos + 1), pixels(pos))
                    'h = col.GetHue
                    's = col.GetSaturation
                    'l = col.GetBrightness

                    '明度指定か明度補正
                    If myForm3.CheckBoxBrightnessNew.Checked Then
                        l = lNew
                    Else
                        l = l ^ (1 / lHosei)
                        If l < 0.00001 Then '指数で表示されるような小さな値になっているとおかしくなるので制限
                            l = 0.00001
                        End If

                    End If
                    '彩度指定か再度補正
                    If myForm3.CheckBoxSaturationNew.Checked Then
                        s = sNew
                    Else
                        s = s ^ (1 / sHosei)
                        If s < 0.00001 Then
                            s = 0.00001
                        End If
                    End If


                    If shiftH = 180 AndAlso s <> 0 Then '色相範囲が180のとき
                        If oneColor Then
                            h = newHue '変換色相は固定
                        Else
                            h = h + sft '変換色相はOffset移動
                        End If

                        If h < 0 Then
                            h += 360
                        ElseIf h >= 360 Then '>359
                            h -= 360 '359
                        End If

                        col = HSLtoRGB(h, s, l)
                        pixels(pos + 2) = col.R
                        pixels(pos + 1) = col.G
                        pixels(pos) = col.B
                    ElseIf maxH - minH >= 0 AndAlso s <> 0 Then '通常'彩度が0の時は変換しない
                        If h >= minH AndAlso h <= maxH Then
                            If oneColor Then
                                h = newHue '変換色相は固定
                            Else
                                h = h + sft '変換色相はOffset移動
                            End If

                            If h < 0 Then
                                h += 360
                            ElseIf h >= 360 Then '>359
                                h -= 360 '359
                            End If
                            col = HSLtoRGB(h, s, l)

                            pixels(pos + 2) = col.R
                            pixels(pos + 1) = col.G
                            pixels(pos) = col.B
                        End If

                    ElseIf s <> 0 Then '最低値と最高値が逆転した場合and彩度が0以外
                        If h >= minH OrElse h <= maxH Then
                            If oneColor Then
                                h = newHue '変換色相は固定
                            Else
                                h = h + sft '変換色相はOffset移動
                            End If

                            If h < 0 Then
                                h += 360
                            ElseIf h >= 360 Then '> 359 Then
                                h -= 360 '359
                            End If
                            col = HSLtoRGB(h, s, l)
                            pixels(pos + 2) = col.R
                            pixels(pos + 1) = col.G
                            pixels(pos) = col.B
                        End If
                    End If
                End If

            Next
        Next

        Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)

        'sw.Stop()
        'Debug.WriteLine(sw.Elapsed.ToString)

        exp.Image = bmp
        DirectCast(myPicArClone(expTag), ExPictureBox).Image = bmp 'クローンに記録
        Call Transparent4() '透過表示

    End Sub
    Friend Sub HueInvert色相反転(exp As ExPictureBox, sftHue As Integer)
        If myPicAr.Count = 0 Then Exit Sub

        Dim i As Integer = exp.Tag - 1
        Dim bmp As New Bitmap(DirectCast(myPicArBackup(i), ExPictureBox).Image) ' exp.Image
        Dim lockW As Integer = bmp.Width
        Dim lockH As Integer = bmp.Height
        Dim lockRect As New Rectangle(0, 0, lockW, lockH)
        Dim bmpdata As BitmapData = bmp.LockBits(lockRect, ImageLockMode.ReadWrite, bmp.PixelFormat)
        Dim ptr As IntPtr = bmpdata.Scan0
        Dim data As Integer = bmpdata.Stride * lockH - 1
        Dim pixels(data) As Byte
        Dim pos As Integer
        System.Runtime.InteropServices.Marshal.Copy(ptr, pixels, 0, pixels.Length)
        Dim x, y As Integer
        Dim oldH, s, l, h As Single
        Dim col As Color

        For y = 0 To bmp.Height - 1
            For x = 0 To bmp.Width - 1
                pos = bmpdata.Stride * y + x * 4
                If pixels(pos + 3) <> 0 Then '完全透明以外のピクセルだけ計算

                    col = Color.FromArgb(pixels(pos + 3), pixels(pos + 2), pixels(pos + 1), pixels(pos))
                    oldH = col.GetHue
                    s = col.GetSaturation
                    l = col.GetBrightness
                    If myForm3.CheckBoxBrightnessInvert明度反転.Checked Then
                        l = 1 - l
                    End If

                    If myForm3.CheckBoxSaturationInvert彩度反転.Checked Then
                        s = 1 - s
                    End If

                    If myForm3.CheckBoxHueShift色相移動.Checked Then
                        h = oldH + sftHue
                    Else
                        h = oldH
                    End If
                    If myForm3.CheckBoxBrightnessToHue明度を色相に.Checked Then
                        h = 360 * l + sftHue
                    ElseIf myForm3.CheckBoxSaturationToHue彩度を色相に.Checked Then
                        h = 360 * s + sftHue
                    End If

                    If h >= 360 Then
                        h -= 360
                    End If

                    If myForm3.CheckBoxThermographサーモグラフ.Checked Then
                        s = 1
                        l = 0.5
                    End If

                    If myForm3.CheckBoxInterlace縦.Checked AndAlso x Mod 2 = 0 Then
                        s = 0
                        l = 0

                    ElseIf myForm3.CheckBoxInterlaceインターレース横.Checked AndAlso y Mod 2 = 0 Then
                        s = 0
                        l = 0
                        'Else
                        '    s = s ^ (1 / 1.5) 'インターレースで暗くなるのを緩和
                    End If
                   
                    'If s < 0.1 OrElse l >= 0.1 Then
                    '    'h = 60
                    '    Dim ns As Single
                    '    ns = l - 0.5
                    '    ns = (l - 0.5) ^ 2

                    '    Dim ooo As Single = (0.2 - 0.5) ^ 2

                    '    ns = (Math.Abs(l - 0.4) / 0.4) ^ 3 + 0.1
                    '    If ns >= 1 Then
                    '        ns = 1
                    '    End If

                    '    If l > 0.8 Then
                    '        h = 60
                    '    Else
                    '        h = 200 - 140 * (l / 0.8)
                    '    End If
                    '    col = HSLtoRGB(h, ns, l)
                    'End If
                    col = HSLtoRGB(h, s, l)
                    pixels(pos + 2) = col.R
                    pixels(pos + 1) = col.G
                    pixels(pos) = col.B

                End If

            Next
        Next

        Runtime.InteropServices.Marshal.Copy(pixels, 0, ptr, pixels.Length)
        bmp.UnlockBits(bmpdata)

        exp.Image = bmp
        DirectCast(myPicArClone(i), ExPictureBox).Image = bmp 'クローンに記録
        Call Transparent4() '透過表示
    End Sub

End Class
