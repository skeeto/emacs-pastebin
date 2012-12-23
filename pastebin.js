var pastebin = pastebin || {};

pastebin.ID = location.pathname.split('/').slice(-1)[0] || null;

pastebin.get = function(id, callback) {
    if (id) $.get('get/' + id, callback, 'json');
};

pastebin.diff = function(pasteA, pasteB) {
    return JsDiff.createPatch(pasteA.parent, pasteA.content, pasteB.content);
};

$(document).ready(function() {
    /* Insert the current paste in the document. */
    pastebin.get(pastebin.ID, function(entry) {
        var $paste = $('#paste').show();

        /* Set up header.  */
        $paste.find('h2').text(entry.title);
        $paste.find('.post-time').text(new Date(entry.time * 1000));

        /* Fill in the paste. */
        $paste.find('pre').text(entry.content)
            .addClass(entry.language || "no-highlight")
            .each(function(i, e) {
                hljs.highlightBlock(e);
            });

        /* Insert parent information. */
        if (entry.parent) {
            $paste.find('footer').show().find('a.parent')
                .text(entry.parent)
                .attr('href', entry.parent);
            $paste.find('footer a.diff').bind('click', function() {
                pastebin.get(entry.parent, function(parent) {
                    var diff = pastebin.diff(entry, parent);
                    var pre = $('<pre class="diff"/>').text(diff)
                            .each(function(i, e) {
                                hljs.highlightBlock(e);
                            });
                    this.append(pre).children();
                }.bind($(this).closest('article')));
                $(this).parent().remove();
                return false;
            });
        }

        /* Fill in the editor, too. */
        $('#content').val(entry.content);
        $('#title').val(entry.title);
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
    $('#edit form').bind('submit', function() {
        var entry = {
            content: $('#content').val(),
            title: $('#title').val() || 'Untitled',
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
