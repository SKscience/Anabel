$(document).ready(function(){
	$('ul.nav.nav-pills li a').click(function() {			
    	$(this).parent().addClass('active').siblings().removeClass('active');
		var clicked = $(this).attr('id');
	});
});

$(window).on('scroll', function() {
    $('.target').each(function() {
		// Activate classes when divs are reached but not when bottom of the page is reached. Then click changeing the classes has top priority
        if(($(window).scrollTop() >= $(this).offset().top - 5) && !($(window).scrollTop() + $(window).height() == $(document).height())) {
            var id = $(this).attr('id');
            $('ul.nav.nav-pills li').removeClass('active');
            $('ul.nav.nav-pills li.' + id).addClass('active');
        }
    });
});


//function pageScroll() {
//    	window.scrollBy(0,400); // horizontal and vertical scroll increments
    	
//};

$(function() {
    $("[name=toggler]").click(function(){
            $('.toHide').hide();
$('.toHide2').hide();
            $('.toHide3').hide();

            $("#check-"+$(this).val()).show('slow');
    });
 });


$(function() {
    $("[name=toggler_two]").click(function(){
            $('.toHide2').hide();
            $('.toHide3').hide();
            $("#check-"+$(this).val()).show('slow');
    });
 });

$(function() {
    $("[name=toggler_three]").click(function(){
            $('.toHide3').hide();
            $("#check-"+$(this).val()).show('slow');
    });
 });

function toBottom(event) {
		var objControl=document.getElementById("toolkit");
		objControl.scrollTop = objControl.scrollHeight;
	};
