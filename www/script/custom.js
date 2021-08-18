$( document ).ready(function() {
    $('#select-Grandslams').click(function(){
        $('.playerspidercharts_tabset a:nth-child(1)').click()
    });
    $('#select-Masters1000').click(function(){
        $('.playerspidercharts_tabset a:nth-child(2)').click()
    });
    $('#select-Olympicmedals').click(function(){
        $('.playerspidercharts_tabset a:nth-child(3)').click()
    });    

    //Show the generate doc button only after selecting a player
    $('#select-clicks').click(function(){
        $('.button_generate_doc').show("fast")
    });
});