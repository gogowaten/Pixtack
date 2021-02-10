Public Class Form2

    Private Sub Form2_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim myPoint As Point
        'myPoint = New Point(1000, 100)
        'myPoint = Form1.Location
        myPoint = New Point(Form1.Location.X + 32, Form1.Location.Y + 32)

        Me.Location = myPoint

        Me.Label1.Text = Application.ProductVersion
        'Me.Label2.Text = Application.ProductName
        'Me.Label3.Text = Application.CompanyName

        Dim Icon As Icon = My.Resources.紫陽花1_2_small
        Me.PictureBox1.Image = Icon.ToBitmap



    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Me.Close()

    End Sub
End Class