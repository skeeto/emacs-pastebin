$(document).ready(function() {
    var id = location.pathname.split('/').slice(-1)[0];
    if (id) {
        $('#paste').show();
        $.get('get/' + id, function(content) {
            $('#paste').text(content);
            $('#content').val(content);
        }, 'text');
    }
});
