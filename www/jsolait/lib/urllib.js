
Module("urllib","$Revision$",function(mod){
mod.NoHTTPRequestObject=Class(mod.Exception,function(publ,supr){
publ.__init__=function(trace){
supr.__init__.call(this,"Could not create an HTTP request object",trace);
};
});
mod.RequestOpenFailed=Class(mod.Exception,function(publ,supr){
publ.__init__=function(trace){
supr.__init__.call(this,"Opening of HTTP request failed.",trace);
};
});
mod.SendFailed=Class(mod.Exception,function(publ,supr){
publ.__init__=function(trace){
supr.__init__.call(this,"Sending of HTTP request failed.",trace);
};
});
var ASVRequest=Class(function(publ){
publ.__init__=function(){
if((getURL==null)||(postURL==null)){
throw "getURL and postURL are not available!";
}else{
this.readyState=0;
this.responseText="";
this.__contType="";
this.status=200;
}
};
publ.open=function(type,url,async){
if(async==false){
throw "Can only open asynchronous connections!";
}
this.__type=type;
this.__url=url;
this.readyState=0;
};
publ.setRequestHeader=function(name,value){
if(name=="Content-Type"){
this.__contType=value;
}
};
publ.send=function(data){
var self=this;
var cbh=new Object();
cbh.operationComplete=function(rsp){
self.readyState=4;
self.responseText=rsp.content;
if(this.ignoreComplete==false){
if(self.onreadystatechange){
self.onreadystatechange();
}
}
};
cbh.ignoreComplete=false;
try{
if(this.__type=="GET"){
getURL(this.__url,cbh);
}else if(this.__type=="POST"){
postURL(this.__url,data,cbh,this.__contType);
}
}catch(e){
cbh.ignoreComplete=true;
throw e;
}
};
});
var getHTTP=function(){
var obj;
try{
obj=new XMLHttpRequest();
}catch(e){
try{
obj=new ActiveXObject("Msxml2.XMLHTTP.4.0");
}catch(e){
try{
obj=new ActiveXObject("Msxml2.XMLHTTP");
}catch(e){
try{
obj=new ActiveXObject("microsoft.XMLHTTP");
}catch(e){
try{
obj=new ASVRequest();
}catch(e){
throw new mod.NoHTTPRequestObject("Neither Mozilla, IE nor ASV found. Can't do HTTP request without them.");
}
}
}
}
}
return obj;
};
mod.sendRequest=function(type,url,user,pass,data,headers,callback){
var async=false;
if(typeof arguments[arguments.length-1]=='function'){
var async=true;
callback=arguments[arguments.length-1];
}
var headindex=arguments.length-((async||arguments[arguments.length-1]==null)?2:1);
if(arguments[headindex] instanceof Array){
headers=arguments[headindex];
}else{
headers=[];
}
if(typeof user=="string"&&typeof pass=="string"){
if(typeof data!="string"){
data="";
}
}else if(typeof user=="string"){
data=user;
user=null;
pass=null;
}else{
user=null;
pass=null;
}
var xmlhttp=getHTTP();
try{
if(user!=null){
xmlhttp.open(type,url,async,user,pass);
}else{
xmlhttp.open(type,url,async);
}
}catch(e){
throw new mod.RequestOpenFailed(e);
}
for(var i=0;i<headers.length;i++){
try{
xmlhttp.setRequestHeader(headers[i][0],headers[i][1]);
}catch(e){
}
}
if(async){
xmlhttp.onreadystatechange=function(){
if(xmlhttp.readyState==4){
callback(xmlhttp);
xmlhttp=null;
}else if(xmlhttp.readyState==2){
try{
var isNetscape=netscape;
try{
var s=xmlhttp.status;
}catch(e){
callback(xmlhttp);
xmlhttp=null;
}
}catch(e){
}
}
};
}
try{
xmlhttp.send(data);
}catch(e){
if(async){
callback(xmlhttp,e);
xmlhttp=null;
}else{
throw new mod.SendFailed(e);
}
}
return xmlhttp;
};
mod.getURL=function(url,user,pass,headers,callback){
var a=["GET"];
for(var i=0;i<arguments.length;i++){
a.push(arguments[i]);
}
return mod.sendRequest.apply(this,a);
};
mod.postURL=function(url,user,pass,data,headers,callback){
var a=["POST"];
for(var i=0;i<arguments.length;i++){
a.push(arguments[i]);
}
return mod.sendRequest.apply(this,a);
};
mod.isUsable=function(){
try{
getHTTP();
return true;}catch(e){
return false;
}
};
});
