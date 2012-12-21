$(document).ready(function() {
    var id = location.pathname.split('/').slice(-1)[0];
    if (id) {
        $('#paste').show();
        $.get('get/' + id, function(entry) {
            $('#paste').text(entry.content);
            $('#content').val(entry.content);
        }, 'json');
    }
});
