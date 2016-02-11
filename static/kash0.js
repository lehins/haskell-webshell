$.widget("ui.kash", {
    version: "0.1",
    options: {
	url: null
    },
    _create: function(){
	$(this).keyPress($(this)._keyDown);
    },
    _keyDown: function(event){
	console.log(event);
    }
});
