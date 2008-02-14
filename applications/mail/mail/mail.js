/*    -*- Java -*- 
 * 
 *    File:         mail.js
 *    Author:         Johan Bevemyr
 *    Created:         Sun Feb 15 23:40:56 2004
 *    Purpose:   support functions for webmail
 */

function setCmd(val) {
  if (val == 'delete') {
    var msg = 'Are you sure you want to delete this message?';
    if (!confirm(msg))
      return;
  }
  document.compose.cmd.value=val;
  document.compose.submit();
}

function changeActive(depth) {
  var msg = document.getElementById('msg-body:msg'+depth);
  var hdr = document.getElementById('msg-body:hdr'+depth);
  var bm = document.getElementById('msg-button:'+depth);
  var bh = document.getElementById('hdr-button:'+depth);

  if (msg.style.display == 'block') {
    msg.style.display = 'none';
    hdr.style.display = 'block';
    bm.style.display = 'none';
    bh.style.display = 'block';
  } else {
    msg.style.display = 'block';
    hdr.style.display = 'none';
    bm.style.display = 'block';
    bh.style.display = 'none';
  }
}

function setComposeCmd(val) { 
   if (document.compose.to.value.length == 0) {
       alert('The To: field must not be empty.');
       document.compose.to.focus();
       return;
   }

   try {
     document.compose.message.value = editor.getHTML();
   } catch (e) {
     var rExp;
     var text=document.compose.html_message.value;

     rExp=/&aring;/gi;
     text.replace(rExp, "å");

     rExp=/&aring;/gi;
     text.replace(rExp, "Å");

     document.compose.message.value = text;
   }

   // alert(editor.getHTML());

   if (document.compose.message.value.length == 0) {
     alert('The message field must not be empty.');
     document.compose.html_message.focus();
     return;
   }

   /* check for attachements */
   if (document.compose.file1.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file2.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file3.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file4.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file5.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file6.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file7.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file8.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file9.value.length > 0) {
     document.compose.attached.value="yes";
   } else if (document.compose.file10.value.length > 0) {
     document.compose.attached.value="yes";
   }
      
   document.compose.cmd.value=val;
   document.compose.submit();
}

function changeActiveTab(i) {
  var j;

  for( j=0; j < tabCount; ++j ) {
    if( i==j ) {
      activateTab(j);
    }
    else {
      deactivateTab(j);
    }
  }
}

function activateTab(i) {
  document.getElementById( "tab-left:"+i ).src="tab-left_active.gif";
  document.getElementById( "tab-bg:"+i ).style.background="url(tab-bg_active.gif)";
  document.getElementById( "tab-right:"+i ).src="tab-right_active.gif";
  document.getElementById( "tab-body:"+i ).style.display='block';
}

function deactivateTab(i) {
  document.getElementById( "tab-left:"+i ).src="tab-left_inactive.gif";
  document.getElementById( "tab-bg:"+i ).style.background="url(tab-bg_inactive.gif)";
  document.getElementById( "tab-right:"+i ).src="tab-right_inactive.gif";
  document.getElementById( "tab-body:"+i ).style.display='none';
}
