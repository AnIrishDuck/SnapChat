
current = []

userName = "unknown"

$(document).ready(function() {
    refresh()
    $('#tosay').autoGrowInput({
        comfortZone: 10,
        minWidth: 100,
        maxWidth: 2000
    });
})

sizeBox = function() {
    $('#tosay').width(100)
}

updateChat = function(entries)
{
    if(entries.length > current.length)
    {
        newEntries = entries.slice(current.length)
        for(entry in newEntries)
        {
            entry = newEntries[entry]
            name = entry[0]
            talk = entry[1]
            $('#main').append( "<tr><td>" + name + "</td><td>" + talk + "</td></tr>" )
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
    sizeBox()
}
