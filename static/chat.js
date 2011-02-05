
current = []

userName = "unknown"

updateChat = function(entries)
{
    if(entries.length > current.length)
    {
        newEntries = entries.slice(current.length)
        for(entry in newEntries)
        {
            $('#main').append( "<div>" + newEntries[entry] + "</div>" )
        }
        current = entries
    }
}

refresh = function()
{
    $.ajax( {url: '/entries?user=' + userName,
             dataType: 'json',
             success: updateChat} )
    window.setTimeout( refresh, 1000 )
}

setUser = function()
{
    tosay = $('#tosay')
    userName = tosay.val()

    $('#user').text(userName)
    $('#button').attr('value', 'Post')
    $('#button').attr('onClick', 'say()')
    
    tosay.val('')
}

say = function()
{
    tosay = $('#tosay')
    $.post( '/say',
            {text: tosay.val(), user: userName},
            updateChat,
            'json' )
    
    // update the page.
    tosay.val('')
}
