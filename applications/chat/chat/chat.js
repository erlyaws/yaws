//    -*- Fundamental -*- 
//    File:      chat.js  (/var/www/chat.js)
//    Author:    Johan Bevemyr
//    Created:   Thu Nov 18 18:25:53 2004
//    Purpose:   
 

/*
 // Uncomment this to get debug printouts in a separate window.

var _console=null;
var _console_txt = null;
var _cnt = 0;

function debug(msg)
{
    var re = new RegExp("<","gi");
    msg = msg.replace(re, "&lt;")

    if ((_console==null) || (_console.closed)) {
        _console = window.open("","console","width=600,height=300,resizable=yes,scrollbars=yes");
        _console.document.open("text/plain");
        _console.document.writeln("<pre>");
    }
    _cnt++;
    _console.document.write(_cnt + ": ");
    _console.document.writeln(msg);
}

*/

function debug(msg) {
}

function sendmsg() {
   var message = document.getElementById("msg").value;

   debug("sendmsg: sending "+message);
   xml_sender.abort();
   xml_sender.open("POST", "chat_write.yaws", false);
   xml_sender.send(message);
   debug("sendmsg: sent "+message);
   document.getElementById("msg").value = "";
}

function create_xmlhttp() {
  var xmlhttp;

  try {
    xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
  } catch (e) {
    try {
      xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    } catch (E) {
      xmlhttp = false;
    }
  }

  if (!xmlhttp && typeof XMLHttpRequest!='undefined')
    xmlhttp = new XMLHttpRequest();
  
  return xmlhttp;
}

var xml_sender = create_xmlhttp();
var xml_reader = create_xmlhttp();

function reader_init() {
  var h = function() {
    debug("reader_init(): got " + xml_reader.readyState);

    if (xml_reader.readyState == 4) {
      var msgs = document.getElementById("msgs");
      var reply = xml_reader.responseText;

      debug("reader_init(): something");

      if (reply.substring(0,2) == "ok") {
	msgs.value = msgs.value + "\n" + reply.substring(2);
	move_to_end(msgs);
	setTimeout("reader_init()", 0);
      }
      else if (reply.substring(0,7) == "timeout") {
	setTimeout("reader_init()", 0);
      }
      else {
	alert("Chat server got unsupported reply.'" + reply+"'");
      }
    }
  }

  debug("reader_init(): waiting");

  xml_reader.open("GET", "chat_read.yaws", true);

  if (window.XMLHttpRequest) {
    xml_reader.onload=h;
  }
  else {
    xml_reader.onreadystatechange=h; 
  }

  xml_reader.send(null);
  debug("reader_init: sent read request");
}

function move_to_end(element) {
    try {
        element.scrollTop = element.scrollHeight;
    } catch(E) {
    }
}

function stop_all() {
  try {
    xml_reader.abort();
  } catch (E) {
  }
}
