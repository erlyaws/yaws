-module(wiki_templates).

-export([template/3]).

%% B = normal | locked | old
template(Title,Menu,Data) ->
    {html,["
<head> 
<title>", Title, "</title>
<style type=\"text/css\">
<!--
   @import url(\"WikiPreferences.files/custom.css\");
-->
</style>
</head>
<body>
<table width='100%' border=0>
<tr> 
<td> 
<table width='100%' border=0> 
<tr> 
<!-- the menu --> 
<td id=menuframe valign='top' width='90%'>\n",
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
<td width=10 align='right' valign='top'><img src='icon.gif' alt=' '></td> 
</tr> 
</table> 
</body>
"]}.









