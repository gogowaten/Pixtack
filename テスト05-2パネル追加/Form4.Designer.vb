<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormHistogram
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
        Me.PictureBoxHistogram = New System.Windows.Forms.PictureBox()
        CType(Me.PictureBoxHistogram, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureBoxHistogram
        '
        Me.PictureBoxHistogram.Location = New System.Drawing.Point(5, 5)
        Me.PictureBoxHistogram.Name = "PictureBoxHistogram"
        Me.PictureBoxHistogram.Size = New System.Drawing.Size(256, 400)
        Me.PictureBoxHistogram.TabIndex = 0
        Me.PictureBoxHistogram.TabStop = False
        '
        'FormHistogram
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(266, 411)
        Me.Controls.Add(Me.PictureBoxHistogram)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormHistogram"
        Me.Text = "ヒストグラム"
        CType(Me.PictureBoxHistogram, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents PictureBoxHistogram As System.Windows.Forms.PictureBox
End Class
