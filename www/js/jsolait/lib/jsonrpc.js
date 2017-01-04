
Module("jsonrpc","$Revision$",function(mod){
var urllib=imprt("urllib");
mod.InvalidServerResponse=Class(mod.Exception,function(publ,supr){
publ.__init__=function(status){
supr.__init__.call(this,"The server did not respond with a status 200 (OK) but with: "+status);
this.status=status;
};
publ.status;
});
mod.MalformedJSONRpc=Class(mod.Exception,function(publ,supr){
publ.__init__=function(msg,s,trace){
supr.__init__.call(this,msg,trace);
this.source=s;
};
publ.source;
});
mod.JSONRPCError=Class(mod.Exception,function(publ,supr){
publ.__init__=function(err,trace){
supr.__init__.call(this,err,trace);
};
});
mod.marshall=function(obj){
if(obj==null){
return "null";
}else if(obj.toJSON){
return obj.toJSON();
}else{
var v=[];
for(var attr in obj){
if(typeof obj[attr]!="function"){
v.push('"'+attr+'": '+mod.marshall(obj[attr]));
}
}
return "{"+v.join(", ")+"}";
}
};
mod.unmarshall=function(source){
try{
var obj;
eval("obj="+source);
return obj;
}catch(e){
throw new mod.MalformedJSONRpc("The server's response could not be parsed.",source,e);
}
};
mod.JSONRPCMethod=Class(function(publ){
var postData=function(url,user,pass,data,callback){
if(callback==null){
var rslt=urllib.postURL(url,user,pass,data,[["Content-Type","text/plain"]]);
return rslt;
}else{
return urllib.postURL(url,user,pass,data,[["Content-Type","text/plain"]],callback);
}
};
var handleResponse=function(resp){
var status=null;
try{
status=resp.status;
}catch(e){
}
if(status==200){
var respTxt="";
try{
respTxt=resp.responseText;
}catch(e){
}
if(respTxt==null||respTxt==""){
throw new mod.MalformedJSONRpc("The server responded with an empty document.","");
}else{
var rslt=mod.unmarshall(respTxt);
if(rslt.error!=null){
throw new mod.JSONRPCError(rslt.error);
}else{
return rslt.result;
}
}
}else{
throw new mod.InvalidServerResponse(status);
}
};
var jsonRequest=function(id,methodName,args){
var p=[mod.marshall(id),mod.marshall(methodName),mod.marshall(args)];
return '{"id":'+p[0]+', "method":'+p[1]+', "params":'+p[2]+"}";
};
publ.__init__=function(url,methodName,user,pass){
this.methodName=methodName;
this.url=url;
this.user=user;
this.password=pass;
};
publ.__call__=function(){
var args=new Array();
for(var i=0;i<arguments.length;i++){
args.push(arguments[i]);
}
if(typeof arguments[arguments.length-1]!='function'){
var data=jsonRequest("httpReq",this.methodName,args);
var resp=postData(this.url,this.user,this.password,data);
return handleResponse(resp);
}else{
var cb=args.pop();
var data=jsonRequest("httpReq",this.methodName,args);
return postData(this.url,this.user,this.password,data,function(resp){
var rslt=null;
var exc=null;
try{
rslt=handleResponse(resp);
}catch(e){
exc=e;
}
try{
cb(rslt,exc);
}catch(e){
}
args=null;
resp=null;
});
}
};
publ.setAuthentication=function(user,pass){
this.user=user;
this.password=pass;
};
publ.notify=function(){
var args=new Array();
for(var i=0;i<arguments.length;i++){
args.push(arguments[i]);
}
var data=jsonRequest(null,this.methodName,args);
postData(this.url,this.user,this.password,data,function(resp){});
};
publ.methodName;
publ.url;
publ.user;
publ.password;
});
mod.ServiceProxy=Class(function(publ){
publ.__init__=function(url,methodNames,user,pass){
this._url=url;
this._user=user;
this._password=pass;
this._addMethodNames(methodNames);
};
publ._addMethodNames=function(methodNames){
for(var i=0;i<methodNames.length;i++){
var obj=this;
var names=methodNames[i].split(".");
for(var n=0;n<names.length-1;n++){
var name=names[n];
if(obj[name]){
obj=obj[name];
}else{
obj[name]=new Object();
obj=obj[name];
}
}
var name=names[names.length-1];
if(obj[name]){
}else{
var mth=new mod.JSONRPCMethod(this._url,methodNames[i],this._user,this._password);
obj[name]=mth;
this._methods.push(mth);
}
}
};
publ._setAuthentication=function(user,pass){
this._user=user;
this._password=pass;
for(var i=0;i<this._methods.length;i++){
this._methods[i].setAuthentication(user,pass);
}
};
publ._url;
publ._user;
publ._password;
publ._methods=new Array();
});
mod.NotificationReceiver=Class(function(publ,supr){
publ.__init__=function(url){
this._url=url;
var req=new XMLHttpRequest();
req.multipart=true;
var self=this;
req.open('POST',url,true);
req.onload=function(evt){
self._handleData(evt.target.responseText);
};
req.send('');
};
publ._handleData=function(data){
var f=new Function('','return '+data);
var o=f();
if(this[o.method]){
this[o.method].apply(this,o.params);
}
};
});
mod.HTTPConnection=Class(function(publ,supr){
publ.__init__=function(url,datahandler){
this.url=url;
this.datahandler=datahandler;
};
publ.send=function(data){
var datahandler=this.datahandler;
urllib.postURL(this.url,data,function(req){
datahandler(req.responseText);
});
};
});
mod.ContinousHTTPConnection=Class(function(publ,supr){
publ.__init__=function(url,datahandler){
this.url=url;
this.datahandler=datahandler;
this.queue=[];
this.isWaitingForResponse=false;
this.isWaitingForServer=false;
this.processQueue();
};
publ.send=function(data){
this.queue.push(data);
this.processQueue();
};
publ.waitForServer=function(){
this.isWaitingForServer=true;
var self=this;
this.currentRequest=urllib.postURL(this.url,"",function(req){
self.isWaitingForServer=false;
self.processData(req.responseText);
});
};
publ.processData=function(data){
if(data!=''){
this.datahandler(data);
}
this.processQueue();
};
publ.sendAndWaitResponse=function(data){
this.isWaitingForResponse=true;
var self=this;
this.currReq=urllib.postURL(this.url,data,function(req){
self.isWaitingForResponse=false;
self.processData(req.responseText);
});
};
publ.processQueue=function(){
if((this.queue.length>0)&&(!this.isWaitingForResponse)){
var data=this.queue.join("");
this.queue=[];
this.sendAndWaitResponse(data);}
if((!this.isWaitingForServer)&&(!this.isWaitingForResponse)){
this.waitForServer();
}
};
});
mod.RPCMethod=Class(function(publ,supr){
publ.__init__=function(name,proxy){
this.name=name;
this.proxy=proxy;
};
publ.__call__=function(){
var args=new Array();
for(var i=0;i<arguments.length;i++){
args.push(arguments[i]);
}
if(typeof args[args.length-1]=="function"){
var callback=args.pop();
return this.proxy._sendRequest(this.name,args,callback);
}else{
return this.proxy._sendNotification(this.name,args);
}
};
});
mod.ServiceProxy2=Class(function(publ,supr){
publ.__init__=function(serviceurl,methodNames,localService){
this._url=serviceurl;var c=new mod.ContinousHTTPConnection(this._url,bind(this,this._handleData));
this._attachMethods(methodNames);
this._localService=localService==null?{}:localService;
this._pendingRequests={};
};
publ._attachMethods=function(methodNames){
for(var i=0;i<methodNames.length;i++){
var obj=this;
var names=methodNames[i].split(".");
for(var n=0;n<names.length-1;n++){
var name=names[n];
if(obj[name]){
obj=obj[name];
}else{
obj[name]=new Object();
obj=obj[name];
}
}
var name=names[names.length-1];
if(obj[name]){
}else{
var mth=new mod.RPCMethod(methodNames[i],this);
obj[name]=mth;
}
}
};
publ._handleData=function(data){
var d='return ['+data.replace(/\0/g,",")+']';
try{
f=new Function('',d);
var messages=f();
}catch(e){
throw new mod.MalformedJSONRpc("The JSON-RPC data is not parsable",data,e);
}
for(var i=0;i<messages.length;i++){
if(messages[i].method!=null&&messages[i].params!=null&&messages[i].id!=null){
this._handleInvokation(messages[i].method,messages[i].params,messages[i].id);
}else if(messages[i].method!=null&&messages[i].params!=null&&messages[i].id==null){
this._handleNotification(messages[i].result,messages[i].error);
}else if(messages[i].id!=null){
this._handleResponse(messages[i].result,messages[i].error,messages[i].id);
}else{
throw new mod.MalformedJSONRpc("The JSON-RPC message does not contain appropriate properties",d);
}
}
};
publ._handleResponse=function(result,err,id){
var r=this._pendingRequests[id];
if(r){
delete this._pendingRequests[id];
r.handleResponse(result,err);
}
};
publ._handleInvokation=function(method,params,id){
if(this._localService[method]){
var rslt=this._localService[method].apply(this._localService,params);
if(isinstanceof(rslt,mod.DelayedResponse)){
rslt.id=id;
}else{
this._sendResponse(rslt,null,id);
}
}else{
this._sendResponse(null,"Method Not Found",id);
}
};
publ._handleNotification=function(method,params){
if(this._localService[method]){
this._localService[method].apply(this._localService,params);
}
};
publ._sendData=function(data){
print(data);
};
publ._sendRequest=function(method,params,callback){
var r=new PendingRequest(callback);
this._pendingRequests[hash(r)]=r;
var data=mod.marshall({method:method,params:params,id:hash(r)});
this._sendData(data);
return r;
};
publ._sendNotification=function(method,params){
var data=mod.marshall({method:method,params:params,id:null});
this._sendData(data);
};
publ._sendResponse=function(result,error,id){
var data=mod.marshall({result:result,error:error,id:id});
this._sendData(data);
};
});
var PendingRequest=Class(function(publ,supr){
publ.__init__=function(callback){
this.callback=callback;
};
publ.handleResponse=function(result,error){
this.callback.call(null,result,error);
};
});
String.prototype.toJSON=function(){
var s='"'+this.replace(/(["\\])/g,'\\$1')+'"';
s=s.replace(/(\n)/g,"\\n");
return s;
};
Number.prototype.toJSON=function(){
return this.toString();
};
Boolean.prototype.toJSON=function(){
return this.toString();
};
Date.prototype.toJSON=function(){
var padd=function(s,p){
s=p+s;
return s.substring(s.length-p.length);
};
var y=padd(this.getUTCFullYear(),"0000");
var m=padd(this.getUTCMonth()+1,"00");
var d=padd(this.getUTCDate(),"00");
var h=padd(this.getUTCHours(),"00");
var min=padd(this.getUTCMinutes(),"00");
var s=padd(this.getUTCSeconds(),"00");
var isodate=y+m+d+"T"+h+":"+min+":"+s;
return '{"jsonclass":["sys.ISODate", ["'+isodate+'"]]}';
};
Array.prototype.toJSON=function(){
var v=[];
for(var i=0;i<this.length;i++){
v.push(mod.marshall(this[i]));
}
return "["+v.join(", ")+"]";
};
mod.__main__=function(){
print("creating ServiceProxy object using introspection for method construction...\n");
var s=new mod.ServiceProxy("http://jsolait.net/testj.py",["echo"]);
print("%s created\n".format(s));
print("creating and marshalling test data:\n");
var o=[1.234,5,{a:"Hello ' \" World",b:new Date()}];
print(mod.marshall(o));
print("\ncalling echo() on remote service...\n");
var r=s.echo(o);
print("service returned data(marshalled again):\n");
print(mod.marshall(r));
};
});
