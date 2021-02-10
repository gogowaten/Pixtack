<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form6
    Inherits System.Windows.Forms.Form

    'フォームがコンポーネントの一覧をクリーンアップするために dispose をオーバーライドします。
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Windows フォーム デザイナーで必要です。
    Private components As System.ComponentModel.IContainer

    'メモ: 以下のプロシージャは Windows フォーム デザイナーで必要です。
    'Windows フォーム デザイナーを使用して変更できます。  
    'コード エディターを使って変更しないでください。
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.ButtonAddFontSetting登録 = New System.Windows.Forms.Button()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.ButtonDelete = New System.Windows.Forms.Button()
        Me.ComboBoxFontSetting = New System.Windows.Forms.ComboBox()
        Me.ButtonOverWriteFontSetting上書き = New System.Windows.Forms.Button()
        Me.DataGridView1 = New System.Windows.Forms.DataGridView()
        Me.ButtonGoUp = New System.Windows.Forms.Button()
        Me.ButtonGoDown = New System.Windows.Forms.Button()
        Me.ButtonSampleImageRefresh見本画像更新 = New System.Windows.Forms.Button()
        Me.ButtonZaoriku復活 = New System.Windows.Forms.Button()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.CheckBoxMihonListBGBlack = New System.Windows.Forms.CheckBox()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'ButtonAddFontSetting登録
        '
        Me.ButtonAddFontSetting登録.Location = New System.Drawing.Point(12, 41)
        Me.ButtonAddFontSetting登録.Name = "ButtonAddFontSetting登録"
        Me.ButtonAddFontSetting登録.Size = New System.Drawing.Size(49, 23)
        Me.ButtonAddFontSetting登録.TabIndex = 4
        Me.ButtonAddFontSetting登録.Text = "追加"
        Me.ToolTip1.SetToolTip(Me.ButtonAddFontSetting登録, "今の設定をリストに追加")
        Me.ButtonAddFontSetting登録.UseVisualStyleBackColor = True
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(12, 70)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(49, 23)
        Me.Button1.TabIndex = 5
        Me.Button1.Text = "確認"
        Me.Button1.UseVisualStyleBackColor = True
        Me.Button1.Visible = False
        '
        'ButtonDelete
        '
        Me.ButtonDelete.Location = New System.Drawing.Point(122, 41)
        Me.ButtonDelete.Name = "ButtonDelete"
        Me.ButtonDelete.Size = New System.Drawing.Size(49, 23)
        Me.ButtonDelete.TabIndex = 6
        Me.ButtonDelete.Text = "削除"
        Me.ToolTip1.SetToolTip(Me.ButtonDelete, "選択行をリストから削除")
        Me.ButtonDelete.UseVisualStyleBackColor = True
        '
        'ComboBoxFontSetting
        '
        Me.ComboBoxFontSetting.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxFontSetting.FormattingEnabled = True
        Me.ComboBoxFontSetting.Location = New System.Drawing.Point(12, 12)
        Me.ComboBoxFontSetting.Name = "ComboBoxFontSetting"
        Me.ComboBoxFontSetting.Size = New System.Drawing.Size(206, 23)
        Me.ComboBoxFontSetting.TabIndex = 7
        '
        'ButtonOverWriteFontSetting上書き
        '
        Me.ButtonOverWriteFontSetting上書き.Location = New System.Drawing.Point(67, 41)
        Me.ButtonOverWriteFontSetting上書き.Name = "ButtonOverWriteFontSetting上書き"
        Me.ButtonOverWriteFontSetting上書き.Size = New System.Drawing.Size(49, 23)
        Me.ButtonOverWriteFontSetting上書き.TabIndex = 8
        Me.ButtonOverWriteFontSetting上書き.Text = "上書"
        Me.ToolTip1.SetToolTip(Me.ButtonOverWriteFontSetting上書き, "選択行に今の設定を上書き")
        Me.ButtonOverWriteFontSetting上書き.UseVisualStyleBackColor = True
        '
        'DataGridView1
        '
        Me.DataGridView1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridView1.Location = New System.Drawing.Point(0, 108)
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.RowTemplate.Height = 21
        Me.DataGridView1.Size = New System.Drawing.Size(472, 277)
        Me.DataGridView1.TabIndex = 9
        '
        'ButtonGoUp
        '
        Me.ButtonGoUp.Location = New System.Drawing.Point(67, 70)
        Me.ButtonGoUp.Name = "ButtonGoUp"
        Me.ButtonGoUp.Size = New System.Drawing.Size(49, 23)
        Me.ButtonGoUp.TabIndex = 10
        Me.ButtonGoUp.Text = "上へ"
        Me.ToolTip1.SetToolTip(Me.ButtonGoUp, "選択行を一つ上に移動")
        Me.ButtonGoUp.UseVisualStyleBackColor = True
        '
        'ButtonGoDown
        '
        Me.ButtonGoDown.Location = New System.Drawing.Point(122, 70)
        Me.ButtonGoDown.Name = "ButtonGoDown"
        Me.ButtonGoDown.Size = New System.Drawing.Size(49, 23)
        Me.ButtonGoDown.TabIndex = 11
        Me.ButtonGoDown.Text = "下へ"
        Me.ToolTip1.SetToolTip(Me.ButtonGoDown, "選択行を一つ下に移動")
        Me.ButtonGoDown.UseVisualStyleBackColor = True
        '
        'ButtonSampleImageRefresh見本画像更新
        '
        Me.ButtonSampleImageRefresh見本画像更新.Location = New System.Drawing.Point(177, 41)
        Me.ButtonSampleImageRefresh見本画像更新.Name = "ButtonSampleImageRefresh見本画像更新"
        Me.ButtonSampleImageRefresh見本画像更新.Size = New System.Drawing.Size(49, 23)
        Me.ButtonSampleImageRefresh見本画像更新.TabIndex = 12
        Me.ButtonSampleImageRefresh見本画像更新.Text = "更新"
        Me.ToolTip1.SetToolTip(Me.ButtonSampleImageRefresh見本画像更新, "見本画像の更新")
        Me.ButtonSampleImageRefresh見本画像更新.UseVisualStyleBackColor = True
        '
        'ButtonZaoriku復活
        '
        Me.ButtonZaoriku復活.Location = New System.Drawing.Point(177, 70)
        Me.ButtonZaoriku復活.Name = "ButtonZaoriku復活"
        Me.ButtonZaoriku復活.Size = New System.Drawing.Size(49, 23)
        Me.ButtonZaoriku復活.TabIndex = 13
        Me.ButtonZaoriku復活.Text = "復活"
        Me.ToolTip1.SetToolTip(Me.ButtonZaoriku復活, "直前に削除したものをリストの最後に復活させる")
        Me.ButtonZaoriku復活.UseVisualStyleBackColor = True
        '
        'ToolTip1
        '
        Me.ToolTip1.AutomaticDelay = 1000
        Me.ToolTip1.AutoPopDelay = 20000
        Me.ToolTip1.InitialDelay = 1000
        Me.ToolTip1.ReshowDelay = 200
        '
        'CheckBoxMihonListBGBlack
        '
        Me.CheckBoxMihonListBGBlack.AutoSize = True
        Me.CheckBoxMihonListBGBlack.Location = New System.Drawing.Point(232, 45)
        Me.CheckBoxMihonListBGBlack.Name = "CheckBoxMihonListBGBlack"
        Me.CheckBoxMihonListBGBlack.Size = New System.Drawing.Size(62, 19)
        Me.CheckBoxMihonListBGBlack.TabIndex = 14
        Me.CheckBoxMihonListBGBlack.Text = "背景黒"
        Me.CheckBoxMihonListBGBlack.UseVisualStyleBackColor = True
        '
        'Form6
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(472, 397)
        Me.Controls.Add(Me.CheckBoxMihonListBGBlack)
        Me.Controls.Add(Me.ButtonZaoriku復活)
        Me.Controls.Add(Me.ButtonSampleImageRefresh見本画像更新)
        Me.Controls.Add(Me.ButtonGoDown)
        Me.Controls.Add(Me.ButtonGoUp)
        Me.Controls.Add(Me.DataGridView1)
        Me.Controls.Add(Me.ButtonOverWriteFontSetting上書き)
        Me.Controls.Add(Me.ComboBoxFontSetting)
        Me.Controls.Add(Me.ButtonDelete)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.ButtonAddFontSetting登録)
        Me.Font = New System.Drawing.Font("Meiryo UI", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(128, Byte))
        Me.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.Name = "Form6"
        Me.Text = "見本リスト"
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents ButtonAddFontSetting登録 As System.Windows.Forms.Button
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents ButtonDelete As System.Windows.Forms.Button
    Friend WithEvents ComboBoxFontSetting As System.Windows.Forms.ComboBox
    Friend WithEvents ButtonOverWriteFontSetting上書き As System.Windows.Forms.Button
    Friend WithEvents DataGridView1 As System.Windows.Forms.DataGridView
    Friend WithEvents ButtonGoUp As System.Windows.Forms.Button
    Friend WithEvents ButtonGoDown As System.Windows.Forms.Button
    Friend WithEvents ButtonSampleImageRefresh見本画像更新 As System.Windows.Forms.Button
    Friend WithEvents ButtonZaoriku復活 As System.Windows.Forms.Button
    Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Friend WithEvents CheckBoxMihonListBGBlack As System.Windows.Forms.CheckBox
End Class
