-module(wiki_templates).

-export([template/4,    %% Page body
	 actionbar/2]). %% Action bar

%% B = normal | locked | old
template(Title,Menu,Data,Locked) ->
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
<td id=",
if Locked == true -> "lockedmenuframe" ; true -> "menuframe" end,
" valign='top' width='90%'>\n",
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
<td width=10 align='right' valign='top'><img src='WikiPreferences.files/icon.gif' alt=' '></td> 
</tr> 
</table> 
</body>
"]}.

actionbar(File, Locked) ->
    MenuId = if Locked == false -> "menu";
		true -> "lockedmenu"
	     end,
    [table(MenuId,
	   [
	    mk_image_link("showPage.yaws?node=home",
			  "WikiPreferences.files/home.gif", "Home",
			  "Go to initial page"),
	    mk_image_link("showHistory.yaws?node=" ++ wiki:str2urlencoded(File),
			  "WikiPreferences.files/history.gif",
			  "History",
			  "History of page evolution"),
	    mk_image_link("allPages.yaws",
			  "WikiPreferences.files/allpages.gif",
			  "All Pages",
			  "Lists all pages on this site"),
	    mk_image_link("lastEdited.yaws",
			  "WikiPreferences.files/lastedited.gif",
			  "Last Edited",
			  "Site editing history"),
	    mk_image_link("wikiZombies.yaws",
			  "WikiPreferences.files/zombies.gif",
			  "Zombies",
			  "Unreachable pages"),
	    mk_image_link("editPage.yaws?node=" ++ wiki:str2urlencoded(File),
			  "WikiPreferences.files/editme.gif",
			  "Edit Me",
			  "Edit this page"),
	    mk_image_link("editFiles.yaws?node=" ++ wiki:str2urlencoded(File),
			  "WikiPreferences.files/editfiles.gif",
			  "Edit Files",
			  "Edit attached files")
	   ])].

%% Various formatting functions
table(Id, X) ->
    ["<table width=\"100%\"><tr><td id=\"", Id, "\">\n",
     X,"</td></tr></table>\n"].

mk_image_link(X, Img, Alt) ->
    ["<a href=\"", X, "\"><img border=0 src='",Img, "' alt='",Alt,"'>"
     "</a>&nbsp;&nbsp;\n"].

mk_image_link(X, Img, Alt, Title) ->
    ["<a href=\"", X, "\"><img border=0 src='",Img, "' ",
     "alt='", Alt, "' "
     "title='", Title,"'></a>&nbsp;&nbsp;\n"].
