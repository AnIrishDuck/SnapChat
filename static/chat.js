
current = []
all_colors = ['#fce94f', '#fcaf3e', '#8ae234', '#729fcf', '#ad7fa8',
              '#ef2929']
system_color = '#babdb6'
system_say = '#e9b96e' 
ucolors = {}
userName = "unknown"

$(document).ready(function() {
    refresh()
    resize()
    $('#tosay').autoGrowInput({
        comfortZone: 10,
        minWidth: 100,
        maxWidth: 2000
    });
    $('#tosay').select()
    $('#tosay').keypress(function(e) {
        if(e.which == 13) {
            $("#button").click();
            return false;
        }
        else {
            return true;
        }
    })
})

resize = function(w) { 
    var nextHeight = this.innerHeight - $("#button").height() - 30 
    $('#main-container').height(nextHeight)
    scrollDown()
}

$(window).resize(resize)

scrollDown = function () {
    var el = $("#main-container")
    el.animate({ scrollTop: el.attr("scrollHeight") }, 200);
}

ucolor = function(name) {
    if(name === "system") { return system_color }
    if(ucolors[name] === undefined) { 
        // Take last color and then move it to the end of the list.
        ucolors[name] = all_colors.pop()
        all_colors.unshift(ucolors[name])
    }
    return ucolors[name] 
}

sizeBox = function() {
    $('#tosay').width(100)
}

updateChat = function(entries)
{
    if(entries.length > current.length)
    {
        var newEntries = entries.slice(current.length)
        for(entry in newEntries)
        {
            var entry = newEntries[entry]
            var name = entry[0]
            var talk = entry[1]
            var uc = ucolor(name)
            var sc = name === 'system' ? system_say : '#eeeeec'
            u = '<td style="color:' + uc + ';">' + name + '</td>' 
            t = '<td style="color:' + sc + ';">' + talk + '</td>'  
            $('#main').append( "<tr>" + u + t + "</tr>" )
        }
        current = entries
        scrollDown() 
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
    var tosay = $('#tosay')
    userName = tosay.val()

    $('#user').text(userName)
    $('#user').css('color', ucolor(userName))
    $('#button').attr('value', 'Post')
    $('#button').attr('onClick', 'say()')
    
    tosay.val('')
}

say = function()
{
    var tosay = $('#tosay')
    $.post( '/say',
            {text: tosay.val(), user: userName},
            updateChat,
            'json' )
    
    // update the page.
    tosay.val('')
    sizeBox()
}
