//    -*- Fundamental -*- 
//    File:      chat.js  (/var/www/chat.js)
//    Author:    Johan Bevemyr
//    Created:   Thu Nov 18 18:25:53 2004
//    Purpose:   
 

// Uncomment this to get debug printouts in a separate window.



/*

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
   message = expandSmilies(message);
   xml_sender.send(message);
   debug("sendmsg: sent "+message);
   document.getElementById("msg").value = "";
}


var smilies = new Array();
smilies[":-)~"] = "tongue.png";
smilies["O :-)"] = "angel.png";
smilies[":-)"] = "smile.png";
smilies[":-("] = "sad.png";
smilies[":-D"] = "bigsmile.png";
smilies[":-?"] = "burp.png";
smilies["8-)"] = "cool.png";
smilies[":-X"] = "crossedlips.png";
smilies[":,-("] = "cry.png";
smilies[":-6"] = "farted.png";
smilies[":*"] = "kiss.png";
smilies[":-$"] = "moneymouth.png";
smilies["0-)"] = "oneeye.png";
smilies[":-@"] = "scream.png";
smilies[":-/"] = "think.png";
smilies[",-)"] = "wink.png";
smilies[":O"] = "yell.png";

function smilies_toolbar() {
  var tool="";
  var smile;

  for (smile in smilies) {
    tool += ("<img src='"+smilies[smile]+"' onmousedown='return addText(\""+
             smile+"\")'></img> ");
  }

  return tool;
}

function expandSmilies(message) {
  var smile;

  for (smile in smilies) {
    var i = 0;
    i = message.indexOf(smile,i);
    while(i >= 0) {
      message = message.substring(0,i)+"<img src='"+smilies[smile]+"'>"+
                message.substring(i+smile.length);
      i = message.indexOf(smile,i);
    }
  }

  return message;
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

var intialized = false;

function reader_init0() {
  initialized = false;
  reader_init();
}


function reader_init() {
  var h = function() {
    debug("reader_init(): got " + xml_reader.readyState);

    if (xml_reader.readyState == 4) {
      var msgs = document.getElementById("msgs");
      var memb = document.getElementById("members");
      var reply = xml_reader.responseText;

      debug("reader_init(): something");

      if (reply.substring(0,2) == "ok") {
        var msg = reply.substring(2);
        var messages = "";
        var members = "";

        while(msg.length > 0) {
          var op   = msg.substring(0,1);
          var i    = msg.indexOf(":");
          var len  = parseInt(msg.substring(1,i));
          var body = msg.substring(i+1, i+len+1);

          msg = msg.substring(i+len+1);

          debug("msg = " + msg);
          debug("i = " + i);
          debug("len = " + len);
          debug("body = " + body);
          debug("op = " + op);

          switch(op) {
          case "m":
            messages += body.replace(/\n/g,"<br>")+"<br>";
            break;
          case "e":
            members = body;
            break;
          default:
          }
        }

        if (messages.length > 0)
          msgs.innerHTML = msgs.innerHTML + messages;

        if (members.length > 0)
          memb.innerHTML = members;

        move_to_end(msgs);
        setTimeout("reader_init()", 0);
      }
      else if (reply.substring(0,7) == "timeout") {
        setTimeout("reader_init()", 0);
      }
      else {
        // alert("Chat server got unsupported reply.'" + reply+"'");
      }
    }
  }

  debug("reader_init(): waiting");

  if (initialized)  {
    xml_reader.open("GET", "chat_read.yaws", true);
  } else {
    xml_reader.open("GET", "chat_read.yaws?init=true", true);
    initialized = true;
  }

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
