-module(wiki_templates).

-export([template/5, template2/5]).

%% B = normal | locked | old
template(Node,Root,Data,Modified,Locked) ->
    MenuId =
        if Locked == true -> "lockedmenuframe" ; 
           true -> "menuframe"
        end,

    File = Root ++ "/WikiPreferences.files/template.html",
    case wiki:file_type(File) of
        error ->
            %% template file missing, create default template
            WobFile = Root ++ "/WikiPreferences.wob",
            wiki:addFile([WobFile,"template.html"], false),
            file:write_file(File,template_file());
        _ ->
            ok
    end,

    {ssi,
     {rel_path,"WikiPreferences.files/template.html"}, "@@",
     [{"NODE", Node},
      {"ALLREFS", "allRefsToMe.yaws?node="++Node},
      {"HOME", "showPage.yaws?node=home"},
      {"HISTORY", "showHistory.yaws?node="++Node},
      {"ALL", "allPages.yaws"},
      {"LAST", "lastEdited.yaws"},
      {"ZOMBIES", "wikiZombies.yaws"},
      {"EDITPAGE", "editPage.yaws?node="++Node},
      {"EDITFILES", "editFiles.yaws?node="++Node},
      {"DATA", Data},
      {"MENUID", MenuId},
      {"MODIFIED", Modified}]}.

template2(Root,Title,Header,Data,Locked) ->
    MenuId =
        if Locked == true -> "lockedmenuframe" ; 
           true -> "menuframe"
        end,

    File = Root ++ "/WikiPreferences.files/template_info.html",
    case wiki:file_type(File) of
        error ->
            %% template file missing, create default template
            WobFile = Root ++ "/WikiPreferences.wob",
            wiki:addFile([WobFile,"template_info.html"], false),
            file:write_file(File,template_info_file());
        _ ->
            ok
    end,

    {ssi,
     {rel_path,"WikiPreferences.files/template_info.html"}, "@@",
     [{"TITLE", Title},
      {"HEADER", Header},
      {"DATA", Data},
      {"MENUID", MenuId}]}.



template_file() ->
    <<"<html>
<head> 
<title> @@NODE@@ </title>
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
<td id=\"@@MENUID@@\" valign='top' width='90%'>
<table width=\"100%\"><tr><td id=\"menu\">
<a href=\"@@HOME@@\"><img border=0 src='WikiPreferences.files/home.gif' alt='Home' title='Go to initial page'></a>&nbsp;&nbsp;
<a href=\"@@HISTORY@@\"><img border=0 src='WikiPreferences.files/history.gif' alt='History' title='History of page evolution'></a>&nbsp;&nbsp;
<a href=\"@@ALL@@\"><img border=0 src='WikiPreferences.files/allpages.gif' alt='All Pages' title='Lists all pages on this site'></a>&nbsp;&nbsp;
<a href=\"@@LAST@@\"><img border=0 src='WikiPreferences.files/lastedited.gif' alt='Last Edited' title='Site editing history'></a>&nbsp;&nbsp;
<a href=\"@@ZOMBIES@@\"><img border=0 src='WikiPreferences.files/zombies.gif' alt='Zombies' title='Unreachable pages'></a>&nbsp;&nbsp;
<a href=\"@@EDITPAGE@@\"><img border=0 src='WikiPreferences.files/editme.gif' alt='Edit Me' title='Edit this page'></a>&nbsp;&nbsp;
<a href=\"@@EDITFILES@@\"><img border=0 src='WikiPreferences.files/editfiles.gif' alt='Edit Files' title='Edit attached files'></a>&nbsp;&nbsp;
</td></tr></table>
</td>
</tr>
<tr> 
<td> 
<p>&nbsp;<br>
  
<!-- the generated page -->
<h1><a href='@@ALLREFS@@'>@@NODE@@</a></h1>
@@DATA@@
<hr><p>Last Modified: @@MODIFIED@@
</td> 
</tr> 
</table> 
</td> 
  
<!-- the right hand image --> 
<td width=10 align='right' valign='top'></td> 
</tr> 
</table> 
</body>
</html>
">>.

template_info_file() ->
    <<"<html>
<head> 
<title> @@TITLE@@ </title>
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
<td id=\"@@MENUID@@\" valign='top' width='90%'>
</td>
</tr> 
<tr> 
<td> 
<p>&nbsp;<br>
  
<!-- the generated page -->
<h1>@@HEADER@@</h1>
@@DATA@@
</td> 
</tr> 
</table> 
</td> 
  
<!-- the right hand image --> 
<td width=10 align='right' valign='top'></td> 
</tr> 
</table> 
</body>
</html>
">>.
