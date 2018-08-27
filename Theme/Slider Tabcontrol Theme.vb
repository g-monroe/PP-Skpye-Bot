Imports System.Drawing.Drawing2D

Module CustomFunctions
    Function ToBrush(ByVal A As Integer, ByVal R As Integer, ByVal G As Integer, ByVal B As Integer) As Brush
        Return New SolidBrush(Color.FromArgb(A, R, G, B))
    End Function
    Function ToBrush(ByVal R As Integer, ByVal G As Integer, ByVal B As Integer) As Brush
        Return New SolidBrush(Color.FromArgb(R, G, B))
    End Function
    Function ToBrush(ByVal A As Integer, ByVal C As Color) As Brush
        Return New SolidBrush(Color.FromArgb(A, C))
    End Function
    Function ToBrush(ByVal Pen As Pen) As Brush
        Return New SolidBrush(Pen.Color)
    End Function
    Function ToBrush(ByVal Color As Color) As Brush
        Return New SolidBrush(Color)
    End Function
    Function ToPen(ByVal A As Integer, ByVal R As Integer, ByVal G As Integer, ByVal B As Integer) As Pen
        Return New Pen(New SolidBrush(Color.FromArgb(A, R, G, B)))
    End Function
    Function ToPen(ByVal R As Integer, ByVal G As Integer, ByVal B As Integer) As Pen
        Return New Pen(New SolidBrush(Color.FromArgb(R, G, B)))
    End Function
    Function ToPen(ByVal A As Integer, ByVal C As Color) As Pen
        Return New Pen(New SolidBrush(Color.FromArgb(A, C)))
    End Function
    Function ToPen(ByVal Brush As SolidBrush) As Pen
        Return New Pen(Brush)
    End Function
    Function ToPen(ByVal Color As Color) As Pen
        Return New Pen(New SolidBrush(Color))
    End Function
    Public Function RoundRect(ByVal Rectangle As Rectangle, ByVal Curve As Integer) As GraphicsPath
        Dim P As GraphicsPath = New GraphicsPath()
        Dim ArcRectangleWidth As Integer = Curve * 2
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -180, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -90, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 0, 90)
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 90, 90)
        P.AddLine(New Point(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y), New Point(Rectangle.X, Curve + Rectangle.Y))
        P.CloseAllFigures()
        Return P
    End Function
    Public Function RoundRect(ByVal X As Integer, ByVal Y As Integer, ByVal Width As Integer, ByVal Height As Integer, ByVal Curve As Integer) As GraphicsPath
        Dim Rectangle As Rectangle = New Rectangle(X, Y, Width, Height)
        Dim P As GraphicsPath = New GraphicsPath()
        Dim ArcRectangleWidth As Integer = Curve * 2
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -180, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -90, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 0, 90)
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 90, 90)
        P.AddLine(New Point(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y), New Point(Rectangle.X, Curve + Rectangle.Y))
        P.CloseAllFigures()
        Return P
    End Function
End Module

Class PlainTabControl
    Inherits TabControl

    Sub New()
        SetStyle(ControlStyles.AllPaintingInWmPaint Or _
        ControlStyles.ResizeRedraw Or _
        ControlStyles.UserPaint Or _
        ControlStyles.DoubleBuffer, True)
    End Sub
    Protected Overrides Sub CreateHandle()
        MyBase.CreateHandle()
        SizeMode = TabSizeMode.Normal
        ItemSize = New Size(1, 1)
    End Sub
    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        Dim G As Graphics = e.Graphics
        G.Clear(Color.FromArgb(50, 50, 50))

        G.FillPath(ToBrush(50, 50, 50), RoundRect(0, 0, Width - 1, Height - 1, 2))
        G.DrawPath(ToPen(50, 50, 50), RoundRect(0, 0, Width - 1, Height - 1, 2))
    End Sub

End Class

Class SliderTab
    Inherits Control

    Private WithEvents _owner As TabControl
    Public Property Owner() As TabControl
        Get
            Return _owner
        End Get
        Set(ByVal value As TabControl)
            _owner = value
            Invalidate()
        End Set
    End Property

    Sub Hook() Handles _owner.SelectedIndexChanged, _owner.TabIndexChanged
        Invalidate()
    End Sub

    Private Offset As Integer = 10

    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)
        MyBase.OnPaint(e)
        Dim G As Graphics = e.Graphics

        Dim Roundness As Integer = Height / 3

        G.SmoothingMode = SmoothingMode.AntiAlias

        G.FillPath(New LinearGradientBrush(New Point(0, 0), New Point(0, Height), Color.FromArgb(249, 249, 249), Color.FromArgb(200, 200, 200)), RoundRect(0, 0, Width - 1, Height - 1, Roundness))
        G.DrawPath(ToPen(70, Color.Black), RoundRect(0, 0, Width - 1, Height - 1, Roundness))

        Try

            If Not Owner Is Nothing Then
                Dim TotalOffset As Integer = (10 + Offset) * Owner.TabPages.Count - Offset
                Dim FarLeft As Integer = Width / 2 - TotalOffset / 2

                For i = 0 To Owner.TabPages.Count - 1
                    Dim CurrentRectangle As Rectangle = New Rectangle(FarLeft + i * (10 + Offset), Height / 2 - 5, 10, 10)

                    G.FillEllipse(New LinearGradientBrush(CurrentRectangle.Location, New Point(CurrentRectangle.X, CurrentRectangle.Y + 10), _
                                                          Color.FromArgb(150, 150, 150), Color.FromArgb(179, 179, 179)), CurrentRectangle)
                    G.DrawEllipse(ToPen(130, 130, 130), CurrentRectangle)
                Next

                Dim SelectedRectangle As Rectangle = New Rectangle(FarLeft + Owner.SelectedIndex * (10 + Offset), Height / 2 - 5, 10, 10)

                G.FillEllipse(New LinearGradientBrush(SelectedRectangle.Location, New Point(SelectedRectangle.X, SelectedRectangle.Y + 10), _
                                      Color.FromArgb(79, 150, 200), Color.FromArgb(50, 100, 170)), SelectedRectangle)
                G.DrawEllipse(ToPen(30, Color.Black), SelectedRectangle)
            End If
        Catch ex As Exception
            'If we do run into an error, we just won't draw anything.
        End Try

    End Sub

    Protected Overrides Sub OnMouseUp(ByVal e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseUp(e)
        Dim TotalOffset As Integer = (10 + Offset) * Owner.TabPages.Count - Offset
        Dim FarLeft As Integer = Width / 2 - TotalOffset / 2

        For i = 0 To Owner.TabPages.Count - 1
            Dim CurrentRectangle As Rectangle = New Rectangle(FarLeft + i * (10 + Offset), Height / 2 - 5, 10, 10)

            If CurrentRectangle.Contains(e.Location) Then
                _owner.SelectedIndex = i
                Exit For
            End If
        Next
    End Sub
End Class