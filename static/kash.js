function Xterm(){};
Xterm.prototype = {
    caps_lock: false,
    num_lock: false,
    scroll_lock: false,
    app_mode: false,
    ignore_kc: [16, 17, 18, 20, 144, 145], // Shift, Ctrl, Alt, Caps, Num,  
    cc: { // C1 (8-Bit) Control Characters
	SS2:   String.fromCharCode(27) + "N", // Single Shift Select of G2 Character Set (esc N)
	SS3:   String.fromCharCode(27) + "O", // Single Shift Select of G3 Character Set (esc O)
	CSI:   String.fromCharCode(27) + "[" // Control Sequence Introducer (esc [)
	/*
	IND:   0x84, // Index (esc D)
	NEL:   0x85, // Next Line (esc E)
	HTS:   0x88, // Tab Set (esc H)
	RI:    0x8d, // Reverse Index (esc M)
	// SS2 and SS3 affect next character only
	SS2:   0x8e, // Single Shift Select of G2 Character Set (esc N)
	SS3:   0x8f, // Single Shift Select of G3 Character Set (esc O)
	DCS:   0x90, // Device Control String (esc P)
	SPA:   0x96, // Start of Guarded Area (esc V)
	EPA:   0x97, // End of Guarded Area (esc W)
	SOS:   0x98, // Start of String (esc X)
	DECID: 0x9a, // Return Terminal ID (esc Z)
	CSI:   0x9b, // Control Sequence Introducer (esc [)
	ST:    0x9c, // String Terminator (esc \)
	OSC:   0x9d, // Operating System Command (esc ])
	PM:    0x9e, // Privacy Message (esc ^)
	APC:   0x9f  // Application Program Control
	*/
    },
    enc: function(val){
	var cur_val = val;
	if(typeof(val) == "number") cur_val = String.fromCharCode(val);
	return escape(cur_val);
    },
    kc2seq: function(e){
	var kc = e.keyCode;
	var seq = [];
	// Ignore Shift, Ctrl, Alt and Lock Strokes
	if(this.ignore_kc.indexOf(kc) < 0){
	    // If NumPad is on
	    if(!this.num_lock){
		if      (kc==96) kc=45;  // Ins
		else if (kc==97) kc=35;  // End
		else if (kc==98) kc=40;  // Down
		else if (kc==99) kc=34;  // PgDn
		else if (kc==100) kc=37; // Left
		else if (kc==102) kc=39; // Right
		else if (kc==103) kc=36; // Home
		else if (kc==104) kc=38; // Up
		else if (kc==105) kc=33; // PgUp
		else if (kc==110) kc=46; // Del
	    }

	    if      (kc==33) seq=[this.cc.CSI, "5", "~"];       // PgUp
	    else if (kc==34) seq=[this.cc.CSI, "6", "~"];       // PgDn
	    else if (kc==35) seq=[this.cc.SS3, "4", "~"];       // End
	    else if (kc==36) seq=[this.cc.SS3, "1", "~"];       // Home
	    else if (kc==37) seq=[this.cc.CSI, "D"];            // Left
	    else if (kc==38) seq=[this.cc.CSI, "A"];            // Up
	    else if (kc==39) seq=[this.cc.CSI, "C"];            // Right
	    else if (kc==40) seq=[this.cc.CSI, "B"];            // Down
	    else if (kc==45) seq=[this.cc.CSI, "2", "~"];       // Ins
	    else if (kc==46) seq=[this.cc.CSI, "3", "~"];       // Del
	    // Convert to lower case if Caps Lock is off
	    else if(65<=kc && kc<=90 && !this.caps_lock) seq=[kc+32];
	    // Convert to numbers if Num Lock is on
	    else if (96<=kc && kc<=105 && this.num_lock) seq=[kc-48];
	    // Convert NumPad Characters
	    else if (106<=kc && kc<=111) seq=[kc-64];           // * + - . /
	    else if (kc==112) seq=[this.cc.SS3, "P"];           // F1
	    else if (kc==113) seq=[this.cc.SS3, "Q"];           // F2
	    else if (kc==114) seq=[this.cc.SS3, "R"];           // F3
	    else if (kc==115) seq=[this.cc.SS3, "S"];           // F4
	    else if (kc==116) seq=[this.cc.CSI, "1", "5", "~"]; // F5
	    else if (kc==117) seq=[this.cc.CSI, "1", "7", "~"]; // F6
	    else if (kc==118) seq=[this.cc.CSI, "1", "8", "~"]; // F7
	    else if (kc==119) seq=[this.cc.CSI, "1", "9", "~"]; // F8
	    else if (kc==120) seq=[this.cc.CSI, "2", "0", "~"]; // F9
	    else if (kc==121) seq=[this.cc.CSI, "2", "1", "~"]; // F10
	    else if (kc==122) seq=[this.cc.CSI, "2", "3", "~"]; // F11
	    else if (kc==123) seq=[this.cc.CSI, "2", "4", "~"]; // F12
	    else if (kc==186) seq=[";"];
	    else if (kc==187) seq=["="];
	    else if (kc==188) seq=[","];
	    else if (kc==189) seq=["-"];
	    else if (kc==190) seq=["."];
	    else if (kc==191) seq=["/"];
	    else if (kc==192) seq=["`"];
	    else if (kc==219) seq=["["];
	    else if (kc==220) seq=["\\"];
	    else if (kc==221) seq=["]"];
	    else if (kc==222) seq=["'"];
	    else seq=[kc];


	    if(seq.length == 1){
		if(e.shiftKey && e.altKey && e.ctrlKey){
		} else if(e.altKey && e.ctrlKey){
		} else if(e.shiftKey && e.ctrlKey){
		} else if(e.shiftKey && e.altKey){
		} else if(e.ctrlKey){
		    if(65<=kc && kc<=90 && !this.caps_lock) seq=[kc-64];
		} else if(e.altKey){
		} else if(e.shiftKey){
		    if(65<=kc && kc<=90){
			if(this.caps_lock) seq=[kc+32];
			else seq=[kc];
		    }
		    else if(kc==48) seq=[")"];    // 0
		    else if(kc==49) seq=["!"];    // 1
		    else if(kc==50) seq=["@"];    // 2
		    else if(kc==51) seq=["#"];    // 3
		    else if(kc==52) seq=["$"];    // 4
		    else if(kc==53) seq=["%"];    // 5
		    else if(kc==54) seq=["^"];    // 6
		    else if(kc==55) seq=["&"];    // 7
		    else if(kc==56) seq=["*"];    // 8
		    else if(kc==57) seq=["("];    // 9
		    else if (kc==186) seq=[":"];  // ;
		    else if (kc==187) seq=["+"];  // =
		    else if (kc==188) seq=["<"];  // ,
		    else if (kc==189) seq=["_"];  // -
		    else if (kc==190) seq=[">"];  // .
		    else if (kc==191) seq=["?"];  // /
		    else if (kc==192) seq=["~"];  // `
		    else if (kc==219) seq=["{"];  // [
		    else if (kc==220) seq=["|"];  // \
		    else if (kc==221) seq=["}"];  // ]
		    else if (kc==222) seq=["\""]; // '
		    
		}
	    } else if(seq.length > 1) {
		var mod_code = undefined;
		if(e.shiftKey && e.altKey && e.ctrlKey) mod_code = "8";
		else if(e.altKey && e.ctrlKey)          mod_code = "7"; 
		else if(e.shiftKey && e.ctrlKey)        mod_code = "6";
		else if(e.ctrlKey)                      mod_code = "5";
		else if(e.shiftKey && e.altKey)         mod_code = "4";
		else if(e.altKey)                       mod_code = "3";
		else if(e.shiftKey)                     mod_code = "2";
		if(mod_code != undefined){
		    var last = seq.pop();
		    seq = seq.concat([";", mod_code, last]);
		}
	    }
	} else{
	    if(kc == 20) this.caps_lock = !this.caps_lock;
	    else if(kc == 144) this.num_lock = !this.num_lock;
	    else if(kc == 145) this.scroll_lock = !this.scroll_lock;
	}
	return seq;
    }
};

$.widget("ui.kash", {
    version: "0.1",
    options: {
	streamPushUrl: "",
	streamPullUrl: "",
	manageUrl: "",
	pushRate: 1000,
	pullRate: 100
    },
    streamPush: "",
    xterm: null,
    _create: function(){
	this.xterm = new Xterm();
	//this.element.keypress($.proxy(this._keypress, this));
	this.element.keydown($.proxy(this._keydown, this));
	$.ajax({
            url: "/terminal",
            data: {
		action: "create"
            },
            type: "POST",
            success: $.proxy(function(data){
		this.options.tid = data;
		this._streamPush();
		this._streamPull();
		$(window).bind("beforeunload", $.proxy(this._destroy, this));
	    }, this)
	});
    },
    _destroy: function(){
	$.ajax({
            url: "/terminal",
            data: {
		tid: this.options.tid,
		action: "delete"
            },
	    async: false,
            type: "POST"
	});
    },
    _keydown: function(event) {
	console.log(event);
	var seq = this.xterm.kc2seq(event);
	if(seq.length){
	    var new_seq = $.map(seq, this.xterm.enc);
	    console.log(new_seq);
	    this._enqueue(new_seq);
	    if(seq[0] == 8)
		return true;
	}
	event.cancelBubble=true;
	if (event.stopPropagation) event.stopPropagation();
	if (event.prevententDefault)  event.prevententDefault();
	return false;
    },
    _enqueue: function(k){
	this.streamPush+= k.join('');
	if(this.pushScheduled != undefined){	
	    window.clearTimeout(this.pushScheduled);
	    this._streamPush();
	}
    },
    _dequeue: function(){
	var stream = this.streamPush;
	this.streamPush = "";
	return stream;
    },
    _streamPush: function(){
	this.pushScheduled = undefined;
	var stream = this._dequeue();
	if(stream != ""){
	    $.ajax({
		url: this.options.streamPushUrl, 
		data: {
		    "tid": this.options.tid,
		    "streamPush": stream
		}, 
		success: $.proxy(this._streamPushSuccess, this),
		type: "POST"
	    });
	} else {
	    this.pushScheduled = $.proxy(this._streamPush, this);
	    window.setTimeout(this.pushScheduled, this.options.pushRate);
	}
    },
    _streamPushSuccess: function(data){
	var stream = data;
	if(stream != ""){
	    this._processPullStream(stream);
	}
	this.pushScheduled = $.proxy(this._streamPush, this);
	window.setTimeout(this.pushScheduled, this.options.pushRate);
    },
    _streamPull: function(){
	$.ajax({
	    url: this.options.streamPullUrl, 
	    data: {tid: this.options.tid},
	    success: $.proxy(this._streamPullSuccess, this)
	});
    },
    _streamPullSuccess: function(data){
	var stream = data;
	if(stream != ""){
	    this._processPullStream(stream);
	}
	if(this.options.pullRate){
	    window.setTimeout($.proxy(this._streamPull, this), this.options.pullRate);
	} else {
	    this._streamPull();
	}
    },
    _processPullStream: function(stream){
	this.element.val(this.element.val()+stream);
	this.element.scrollTop(this.element.get(0).scrollHeight);
    }
});
