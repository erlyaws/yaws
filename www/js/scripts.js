$(document).ready(function() {
    $('.git-body-msg').hide();

    /* smooth scrolling sections */
    $('a[href*=#]:not([href=#])').click(function() {
        if (location.pathname.replace(/^\//,'') == this.pathname.replace(/^\//,'') && location.hostname == this.hostname) {
            var target = $(this.hash);
            if (/^#howto_/.test(target))
                return;
            target = target.length ? target : $('[name=' + this.hash.slice(1) +']');
            if (target.length) {
                $('html,body').animate({
                    scrollTop: target.offset().top - 50
                }, 1000);
            }
        }
    });

    /* Rollover images */
    $(function() {
        $('img[data-hover]').hover(function() {
            $(this).attr('tmp', $(this).attr('src')).attr('src', $(this).attr('data-hover')).attr('data-hover', $(this).attr('tmp')).removeAttr('tmp');
        }).each(function() {
            $('<img />').attr('src', $(this).attr('data-hover'));
        });;
    });

    $('nav').scrollToFixed();
    $('#leftnav').scrollToFixed({marginTop: 70});
    hljs.configure({languages: []});
    hljs.initHighlightingOnLoad();
    $('pre.code').each(function(i, block) {
        hljs.highlightBlock(block);
    });
});

 function load_modal(Title, Url) {
   $('.modal-title').html(Title);
   $('.modal-body').load(Url, function() {
     $('pre.code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  }
