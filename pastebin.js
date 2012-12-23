var pastebin = pastebin || {};

pastebin.ID = location.pathname.split('/').slice(-1)[0] || null;

pastebin.getPaste = function(id, callback) {
    if (id) $.get('get/' + id, callback, 'json');
};

$(document).ready(function() {
    /* Insert the current paste in the document. */
    pastebin.getPaste(pastebin.ID, function(entry) {
        $('article').show();
        $('article h2').text(entry.title || '');
        $('#paste').text(entry.content)
            .addClass(entry.language || "no-highlight")
            .each(function(i, e) {
                hljs.highlightBlock(e);
            });
        if (entry.parent) {
            $('article nav.derived').show().find('a')
                .text(entry.parent)
                .attr('href', entry.parent);
        }

        /* Fill in the editor, too. */
        $('#content').val(entry.content);
        $('#title').val(entry.title || '');
        $('#language').val(entry.language || "auto");
    });

    /* Add all the supported languages to the form. */
    var languages = Object.keys(hljs.LANGUAGES).sort();
    for (var i in languages) {
        var formal = languages[i].replace(/^./, function(first) {
            return first.toUpperCase();
        });
        $('#language').append($('<option>' + formal + '</option>').attr({
            value: languages[i]
        }));
    }

    /* Submission handling. */
    $('#entry form').bind('submit', function() {
        var entry = {
            content: $('#content').val(),
            title: $('#title').val(),
            language: $('#language').val(),
            expiration: parseFloat($('#expiration').val()),
            parent: pastebin.ID
        };
        $.post("post", JSON.stringify(entry), function(id) {
            location = id;
        }, 'text');
        return false;
    });
});
