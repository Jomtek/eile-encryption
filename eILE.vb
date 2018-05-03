Public Class eILE
    Public Shared charSet As String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345678 9!()[]{}:;.,?^@|èà&éù$*%µ=/-+°ç'""²£¤§<>	" & ControlChars.Lf

    Public Shared Function generateKey() As String 'generates a key composed of a maximum of 3 digits per serials, example : 111.222.118.333.555.888
        '62 length
        Dim key As String = ""
        For i As Integer = 0 To charSet.Length
            Dim Nbaleatoire As Integer
            Randomize()
            Nbaleatoire = Int(Rnd() * 999)
            If Not key.Contains(Nbaleatoire & ".") Then
                key += Nbaleatoire & "."
            Else
                Dim Nbaleatoires As Integer
                Do
                    Randomize()
                    Nbaleatoires = Int(Rnd() * 999)
                Loop Until Not key.Contains(Nbaleatoires & ".")
                key += Nbaleatoires & "."
            End If
        Next

        If key.EndsWith(".") Then
            key = key.Substring(0, key.Length - 1)
        End If

        Return key
    End Function
    Public Shared Function encryptText(Text As String, extandedKey As String) As String 'encrypts the text with the specified encryption key
        Dim finalText As String = ""
        For Each i As String In Text
            If charSet.Contains(i) Then
                Dim getArrayNumberInCharset As Integer = charSet.IndexOf(i)
                Try

                    finalText += extandedKey.Split(".")(getArrayNumberInCharset) & "."

                Catch ex As IndexOutOfRangeException
                    MsgBox("Fatal Error : The Key Index is out of range !", MsgBoxStyle.Information, "ERROR :/")
                    Exit For
                End Try
            Else
                MsgBox("Error : Non supported characters in text : " & i & " !", MsgBoxStyle.Information, "Erreur :/")
                Exit For
            End If
        Next

        If finalText.EndsWith(".") Then
            finalText = finalText.Substring(0, finalText.Length - 1)
        End If

        Return finalText
    End Function
    Public Shared Function decryptText(text As String, extandedKey As String) As String 'decrypts the text using the specified decryption key
        Dim finalText As String = ""
        For Each i As String In text.Split(".")
            If extandedKey.Split(".").ToArray.Contains(i) Then
                '14 15 18 16 12 18 17 19 20 13. 14 15 18 16 12 18 17 19 20 13 14 15 18 16 12 18 17 19 20 13 14 15 18 16 12 18 17 19 20 13
                Dim indexExtanded = 0
                Dim getIndexExtandedKey = extandedKey.Split(".").ToArray
                Dim counter = 0
                For Each iss As String In getIndexExtandedKey
                    If iss = i Then
                        indexExtanded = counter
                    End If
                    counter += 1
                Next
                Try
                    Dim getLetterCorresponding = charSet.ToString.ToArray(indexExtanded)
                    finalText += getLetterCorresponding
                Catch ex As IndexOutOfRangeException
                    MsgBox("Fatal Error : The Key Index is out of range !", MsgBoxStyle.Information, "ERROR :/")
                    Exit For
                End Try
            Else
                If IsNumeric(i) Then
                    MsgBox("Fatal Error : Invalid Decryption Key !", MsgBoxStyle.Information, "ERROR :/")
                    Exit For
                End If
            End If
        Next
        Return finalText
    End Function
    Public Shared Function GetRandom() As Integer
        Dim random As New Random(), rndnbr As Integer
        rndnbr = random.Next(0, 99)
        Return rndnbr
    End Function
End Class