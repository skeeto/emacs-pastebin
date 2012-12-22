$(document).ready(function() {
    var id = location.pathname.split('/').slice(-1)[0];
    if (id) {
        $.get('get/' + id, function(entry) {
            $('#paste').show()
                .text(entry.content)
                .addClass(entry.language || "no-highlight")
                .each(function(i, e) {
                    hljs.highlightBlock(e);
                });
            $('#content').val(entry.content);
            $('#language').val(entry.language || "auto");
        }, 'json');
    }

    for (var language in hljs.LANGUAGES) {
        var formal = language.replace(/^./, function(first) {
            return first.toUpperCase();
        });
        $('#language').append($('<option>' + formal + '</option>').attr({
            value: language
        }));
    }

    $('#entry form').bind('submit', function() {
        var entry = {
            content: $('#content').val(),
            language: $('#language').val(),
            expiration: parseFloat($('#expiration').val())
        };
        $.post("post", JSON.stringify(entry), function(id) {
            location = id;
        }, 'text');
        return false;
    });
});
