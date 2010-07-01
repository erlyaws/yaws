-module(yaws_showarg).

-export([out/1]).

-include("../include/yaws_api.hrl").

f(Fmt, Args) ->
    io_lib:format(Fmt, Args).

out(ARG) ->
    [
     {html, 
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html>
      <head>
      <style type=\"text/css\">
                                        table {border-collapse: collapse; }
      td, th { border: 1px solid #000000; vertical-align: baseline; }

      </style>
      </head>
      "},
        {ehtml,
         [
          {body,[], 
           [
            {h4,[], "ARG information"},
            {table,[], 
             [
              {thead,[], 
               [
                {tr,[], 
                 [
                  {th,[],"record field"},{th,[],"value"}
                 ]}
               ]},
              {tbody,[], 
               [
                {tr,[], 
                 [ {td,[],"clisock"},
                   {td,[],f("~p",[ARG#arg.clisock])} ]},
                {tr,[], [ {td,[],"client_ip_port"},
                          {td,[],f("~p",[ARG#arg.client_ip_port])} ]},
                {tr,[], [ {td,[],"headers"},
                          {td,[],f("(see below)",[])} ]},
                {tr,[], [ {td,[],"req"},
                          {td,[],f("~p",[ARG#arg.req])} ]},
                {tr,[], [ {td,[],"clidata"},
                          {td,[],f("~p",[ARG#arg.clidata])} ]},
                {tr,[], [ {td,[],"server_path"},
                          {td,[],f("~p",[ARG#arg.server_path])} ]},
                {tr,[], [ {td,[],"querydata"},
                          {td,[],f("~p",[ARG#arg.querydata])} ]},
                {tr,[], [ {td,[],"appmoddata (deprecated)"},
                          {td,[],f("~p",[ARG#arg.appmoddata])} ]},
                {tr,[], [ {td,[],"docroot"},
                          {td,[],f("~p",[ARG#arg.docroot])} ]},
                {tr,[], [ {td,[],"docroot_mount"},                
                          {td,[],f("~p",[ARG#arg.docroot_mount])} ]},
                {tr,[], [ {td,[],"fullpath"},                        
                          {td,[],f("~p",[ARG#arg.fullpath])} ]},
                {tr,[], [ {td,[],"cont"},                                
                          {td,[],f("~p",[ARG#arg.cont])} ]},
                {tr,[], [ {td,[],"state"},                                
                          {td,[],f("~p",[ARG#arg.state])} ]},
                {tr,[], [ {td,[],"pid"},                                
                          {td,[],f("~p",[ARG#arg.pid])} ]},
                {tr,[], [ {td,[],"opaque"},                                
                          {td,[],yaws_api:htmlize(f("~p",[ARG#arg.opaque]))} ]},
                {tr,[], [ {td,[],"appmod_prepath (deprecated)"},          
                          {td,[],f("~p",[ARG#arg.appmod_prepath])} ]},
                {tr,[], [ {td,[],"prepath"},                      
                          {td,[],f("~p",[ARG#arg.prepath])} ]},
                {tr,[], [ {td,[],"pathinfo"},                   
                          {td,[],f("~p",[ARG#arg.pathinfo])} ]}
               ]}
             ]},        
            {h4,[], "headers"},
            {pre,[], f("~p",[yaws_api:reformat_header(ARG#arg.headers)])},
            {hr,[],[]},
            {h4,[], "raw ARG"},
            {pre,[], yaws_api:htmlize(f("~p", [ARG]))},
            {hr,[],[]}
           ]}
         ]
        },
     {html,
      "
</html>
       "}
    ].

