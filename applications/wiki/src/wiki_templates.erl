-module(wiki_templates).

-export([template/3, template/4]).

%% B = normal | locked | old
template(Node,Data,Modified,Locked) ->
    MenuId =
	if Locked == true -> "lockedmenuframe" ; 
	   true -> "menuframe"
	end,
    {ssi,
     "WikiPreferences.files/template.html", "@@",
     [{"NODE", Node},
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

template(Title,Data,Locked) ->
    MenuId =
	if Locked == true -> "lockedmenuframe" ; 
	   true -> "menuframe"
	end,
    {ssi,
     "WikiPreferences.files/template_info.html", "@@",
     [{"TITLE", Title},
      {"DATA", Data},
      {"MENUID", MenuId}]}.
