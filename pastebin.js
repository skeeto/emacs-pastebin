$(document).ready(function() {
    var id = location.pathname.split('/').slice(-1)[0];
    if (id) {
        $('#paste').show();
        $.get('get/' + id, function(entry) {
            $('#paste').text(entry.content);
            $('#content').val(entry.content);
        }, 'json');
    }

    $('#entry form').bind('submit', function() {
        var entry = {
            content: $('#content').val()
        };
        $.post("post", JSON.stringify(entry), function(id) {
            location = id;
        }, 'text');
        return false;
    });
});
