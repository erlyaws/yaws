-module(wiki_templates).

-export([template/4]).

%% B = normal | locked | old
template(Title,Background,Menu,Data) ->
    {html,["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<HTML> 
<HEAD> 
<TITLE>", Title, "</TITLE>
<style type=\"text/css\">
<!--
TEXTAREA{
  width : 100%;
}
-->
</style>
", Background, "
<table width='100%'>
<tr> 
<!-- first gutter to allow space for the bgimage --> 
<td ><pre>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</pre></td>
<td> 
<table width='100%'> 
<tr> 
<!-- the menu --> 
<td bgcolor='#FFFFFF' valign='top' width='90%'>\n",
Menu,
"
</td>
</tr> 
<tr> 
<td> 
<p>&nbsp;<br>
  
<!-- the generated page -->",
Data,"
</td> 
</tr> 
</table> 
</td> 
  
<!-- the right hand image --> 
<td width=10 align='right' valign='top'><img src='icon.gif'></td> 
</tr> 
</table> 
</body>
</html>"]}.









