
Class=function(name,base1,classScope){
var args=[];
for(var i=0;i<arguments.length;i++){
args[i]=arguments[i];
}
classScope=args.pop();
var classID=Class.__idcount__++;
if((args.length>0)&&(typeof args[0]=='string')){
name=args.shift();
}else{
name="anonymous"+classID;
}
var bases=args;
var __class__={__isArray__:false,
__name__:name,
__bases__:bases,
__id__:'@'+classID,
__hash__:function(){
return this.__id__;
},
__str__:function(){
return "[class %s]".format(this.__name__);
}
};
var baseProtos=[];
var proto;
if(bases.length==0){
proto={};
proto.__str__=function(){
return "[%s %s]".format(this.__class__.prototype.__call__===undefined?'object':'callable',this.__class__.__name__);
};
__class__.__bases__=[Object];
}else{
var baseProto;
for(var i=0;i<bases.length;i++){
var baseClass=bases[i];
baseProtos.push(baseClass.prototype);
if(baseClass.__createProto__!==undefined){
baseProto=baseClass.__createProto__(bases);
}else{
baseProto=new baseClass(Class);
}
__class__.__isArray__=__class__.__isArray__||baseClass.__isArray__;
if(i==0){
proto=baseProto;
}else{
for(var key in baseProto){
if(proto[key]===undefined){
proto[key]=baseProto[key];
}
}
}
for(var key in baseClass){
if((key!='prototype')&&(__class__[key]===undefined)){
__class__[key]=baseClass[key];
}
}
}
}
if(proto.__hash__===undefined){
proto.__hash__=function(){
if(this.__id__===undefined){
this.__id__='@'+(Class.__idcount__++);
}
return this.__id__;
};
}
proto.__class__=__class__;
if(classScope.length-1>baseProtos.length){
var privId='__priv__'+__class__.__id__;
classScope.apply(this,[proto,privId].concat(baseProtos));
}else{
classScope.apply(this,[proto].concat(baseProtos));
}
proto.toString=proto.__str__;
if(proto.__call__){
var NewClass=function(calledBy){
if(calledBy!==Class){
var rslt=function(){
return rslt.__call__.apply(rslt,arguments);
};
var proto=arguments.callee.prototype;
for(var n in proto){
rslt[n]=proto[n];
}
rslt.constructor=proto.__class__;
rslt.toString=proto.__str__;
if(rslt.__init__){
rslt.__init__.apply(rslt,arguments);
}
return rslt;
}
};
}else if(__class__.__isArray__){
var NewClass=function(calledBy){
if(calledBy!==Class){
rslt=[];
var proto=arguments.callee.prototype;
for(var n in proto){
rslt[n]=proto[n];
}
rslt.constructor=proto.__class__;
rslt.toString=proto.__str__;
if(rslt.__init__){
rslt.__init__.apply(rslt,arguments);
}else{
if(arguments.length==1){
rslt.length=arguments[0];
}else{
for(var i=0;i<arguments.length;i++){
rslt.push(arguments[i]);
}
}
}
return rslt;
}
};}else{
var NewClass=function(calledBy){
if(calledBy!==Class){
if(this.__init__){
this.__init__.apply(this,arguments);
}
}
};
}
proto.constructor=NewClass;
proto.__class__=NewClass;
NewClass.prototype=proto;
for(var key in __class__){
NewClass[key]=__class__[key];
}
NewClass.toString=__class__.__str__;
return NewClass;
};
Class.__idcount__=0;
Class.__str__=Class.toString=function(){return "[object Class]";};
Class.__createProto__=function(){throw "Can't use Class as a base class.";};
Function.__createProto__=function(){throw "Cannot inherit from Function. implement the callable interface instead using YourClass::__call__.";};
Array.__createProto__=function(){var r=[];r.__str__=Array.prototype.toString;return r;};
Array.__isArray__=true;
Array.__str__=Array.toString=function(){return "[class Array]";};
Object.__str__=Object.toString=function(){return "[class Object]";};
Number.__str__=Number.toString=function(){return "[class Number]";};
String.__str__=String.toString=function(){return "[class String]";};
str=String;
repr=function(obj){
if(obj==null){
return null;
}else if(obj.__repr__){
return obj.__repr__();
}else{
switch(typeof obj){
case "string":
obj=obj.replace(/\\/g,"\\\\").replace(/\"/g,"\\\"").replace(/\n/g,"\\n").replace(/\r/g,"\\r");
return '"'+obj+'"';
case "boolean":case "number":
return ""+obj;
case "object":
var out=[];
if(obj==null){
return "null";
}else if(obj instanceof Array){
for(var i=0;i<obj.length;i++){
out.push(repr(obj[i]));
}
return "["+out.join(",")+"]";
}else if(obj instanceof Object){
for(var key in obj){
out.push(repr(key)+":"+repr(obj[key]));
}
return "{"+out.join(",")+"}";
}
}
}
};
hash=function(obj,forceId){
if(obj.__id__!=null){
return obj.__id__;
}else if(obj.__hash__){
return obj.__hash__();
}else if(obj instanceof String||typeof obj=='string'){
return '$'+obj;
}else if(obj instanceof Number||typeof obj=='number'){
return '#'+obj;
}else if(forceId){
obj.__id__='@'+(Class.__idcount__++);
return obj.__id__;
}else{
throw new jsolait.Exception('Objec cannot be hashed: %s'.format(obj));
}
};
bind=function(obj,fn){
return function(){
return fn.apply(obj,arguments);
};
};
isinstance=function(obj,cls){
if(obj instanceof cls){
return true;
}else{
return issubclass(obj.constructor,cls);
}
};
issubclass=function(cls,baseclass){
if(baseclass===Object||cls===baseclass||(cls.prototype instanceof baseclass)){
return true;
}else{
var bases=cls.__bases__;
if(bases!=null){
for(var i=0;i<bases.length;i++){
if(bases[i]===baseclass){
return true;
}
}
for(var i=0;i<bases.length;i++){
if(issubclass(bases[i],baseclass)){
return true;
}
}
}
return false;
}
};
Module=function(name,version,moduleScope){
var newMod=new Module.ModuleClass(name,version,Module.currentURI);
try{
moduleScope.call(newMod,newMod);
}catch(e){
throw new Module.ModuleScopeExecFailed(newMod,e);
}
for(var n in newMod){
var obj=newMod[n];
if(typeof obj=='function'){
obj.__name__=n;
}
}
jsolait.registerModule(newMod);
return newMod;
};
Module.ModuleClass=Class(function(publ){
publ.name;
publ.version;
publ.__sourceURI__;
publ.Exception;
publ.__init__=function(name,version,sourceURI){
this.name=name;
this.version=version;
this.__sourceURI__=sourceURI;
this.Exception=Class(Module.Exception,function(){});
this.Exception.prototype.module=this;
};
publ.__str__=function(){
return "[module '%s' version: %s]".format(this.name,(this.version+'').replace(/\$Revision$/,"rev.$1"));
};
});
Module.toString=function(){
return "[object Module]";
};
Module.__createProto__=function(){
throw "Can't use Module as a base class.";
};
Module.Exception=Class("Exception",function(publ){
publ.__init__=function(msg,trace){
this.name=this.constructor.__name__;
this.message=''+msg;
this.trace=trace;
};
publ.__str__=function(){
return this.toTraceString();
};
publ.toTraceString=function(indent){
indent=indent==null?0:indent;
var s="%s in %s:\n%s".format(this.name,this.module,this.message.indent(4)).indent(indent);
if(this.trace){
if(this.trace.toTraceString){
s+=('\n\nbecause:\n'+this.trace.toTraceString(indent+4));
}else{
s+=(this.trace+'\n').indent(indent+4);
}
}
return s;
};
publ.name;
publ.message;
publ.module="jsolait";
publ.trace;
});
Module.ModuleScopeExecFailed=Class("ModuleScopeExecFailed",Module.Exception,function(publ,supr){
publ.__init__=function(module,trace){
supr.__init__.call(this,"Failed to run the module scope for %s".format(module),trace);
this.failedModule=module;
};
publ.module;
});
Module("jsolait","$Revision$",function(mod){
jsolait=mod;
mod.modules={};
mod.knownModuleURIs={"async":"%(baseURI)s/lib/async.js","codecs":"%(baseURI)s/lib/codecs.js","crypto":"%(baseURI)s/lib/crypto.js","dom":"%(baseURI)s/lib/dom.js","forms":"%(baseURI)s/lib/forms.js","iter":"%(baseURI)s/lib/iter.js","jsonrpc":"%(baseURI)s/lib/jsonrpc.js","lang":"%(baseURI)s/lib/lang.js","operators":"%(baseURI)s/lib/operators.js","sets":"%(baseURI)s/lib/sets.js","strings":"%(baseURI)s/lib/strings.js","testing":"%(baseURI)s/lib/testing.js","urllib":"%(baseURI)s/lib/urllib.js","xml":"%(baseURI)s/lib/xml.js","xmlrpc":"%(baseURI)s/lib/xmlrpc.js",".svn":"%(baseURI)s/lib/.svn/","net":"%(baseURI)s/lib/net/"};
mod.moduleSearchURIs=["."];
mod.baseURI="./jsolait";
mod.packagesURI="%(baseURI)s/packages";
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
throw new mod.Exception("Unable to get an HTTP request object.");
}
}
}
}
return obj;
};
mod.loadURI=function(uri,headers){
headers=(headers!==undefined)?headers:[];
try{
var xmlhttp=getHTTP();
xmlhttp.open("GET",uri,false);
for(var i=0;i<headers.length;i++){
xmlhttp.setRequestHeader(headers[i][0],headers[i][1]);
}
xmlhttp.send("");
}catch(e){
throw new mod.LoadURIFailed(uri,e);
}
if(xmlhttp.status==200||xmlhttp.status==0||xmlhttp.status==null||xmlhttp.status==304){
var s=new String(xmlhttp.responseText);
s.__sourceURI__=uri;
return s;
}else{
throw new mod.LoadURIFailed(uri,new mod.Exception("Server did not respond with 200"));
}
};
mod.LoadURIFailed=Class(mod.Exception,function(publ,priv,supr){
publ.__init__=function(sourceURI,trace){
supr.__init__.call(this,"Failed to load file: '%s'".format(sourceURI.indent(2)),trace);
this.sourceURI=sourceURI;
};
publ.sourceURI;
});
mod.__imprt__=function(name){
if(mod.modules[name]){
return mod.modules[name];
}else{
var src,modPath;
var searchURIs=[];
if(mod.knownModuleURIs[name]!=undefined){
searchURIs.push(mod.knownModuleURIs[name].format(mod));
}else{
name=name.split('.');
if(name.length>1){
if(mod.knownModuleURIs[name[0]]!=undefined){
var uri=mod.knownModuleURIs[name[0]].format(mod);
searchURIs.push("%s/%s.js".format(uri,name.slice(1).join('/')));
}
searchURIs.push("%s/%s.js".format(mod.packagesURI.format(mod),name.join('/')));
}
for(var i=0;i<mod.moduleSearchURIs.length;i++){
searchURIs.push("%s/%s.js".format(mod.moduleSearchURIs[i].format(mod),name.join("/")));
}
name=name.join(".");
}
var failedURIs=[];
for(var i=0;i<searchURIs.length;i++){
try{
src=mod.loadURI(searchURIs[i]);
break;
}catch(e){
failedURIs.push(e.sourceURI);
}
}
if(src==null){
throw new mod.ImportFailed(name,failedURIs);
}else{
try{
var srcURI=src.__sourceURI__;
src='Module.currentURI="%s";\n%s\nModule.currentURI=null;\n'.format(src.__sourceURI__.replace(/\\/g,'\\\\'),src);
var f=new Function("",src);
f();
}catch(e){
throw new mod.ImportFailed(name,[srcURI],e);
}
if(mod.modules[name]!=null){
return mod.modules[name];
}else{
throw new mod.ImportFailed(name,[srcURI],mod.Exception("Module did not register itself and cannot be imported. "+name));
}
}
}
};
mod.ImportFailed=Class(mod.Exception,function(publ,supr){
publ.__init__=function(moduleName,moduleURIs,trace){
supr.__init__.call(this,"Failed to import module: '%s' from:\n%s".format(moduleName,moduleURIs.join(',\n').indent(2)),trace);
this.moduleName=moduleName;
this.moduleURIs=moduleURIs;
};
publ.moduleName;
publ.moduleURIs;
});
imprt=function(name){
return mod.__imprt__(name);
};
mod.__registerModule__=function(modObj,modName){
if(modName!='jsolait'){
return mod.modules[modName]=modObj;
}
};
mod.registerModule=function(modObj,modName){
modName=modName===undefined?modObj.name:modName;
return mod.__registerModule__(modObj,modName);
};
var FormatSpecifier=function(s){
var s=s.match(/%(\(\w+\)){0,1}([ 0-]){0,1}(\+){0,1}(\d+){0,1}(\.\d+){0,1}(.)/);
if(s[1]){
this.key=s[1].slice(1,-1);
}else{
this.key=null;
}
this.paddingFlag=s[2];
if(this.paddingFlag==""){
this.paddingFlag=" ";
}
this.signed=(s[3]=="+");
this.minLength=parseInt(s[4]);
if(isNaN(this.minLength)){
this.minLength=0;
}
if(s[5]){
this.percision=parseInt(s[5].slice(1,s[5].length));
}else{
this.percision=-1;
}
this.type=s[6];
};
var pad=function(s,flag,len){
if(flag=="-"){
var c=" ";
}else{
var c=''+flag;
}
var rslt=c.mul(len-s.length);
if(flag=="-"){
rslt=s+rslt;
}else{
rslt+=s;
}
return rslt;
};
String.prototype.format=function(){
var sf=this.match(/(%(\(\w+\)){0,1}[ 0-]{0,1}(\+){0,1}(\d+){0,1}(\.\d+){0,1}[dibouxXeEfFgGcrs%])|([^%]+)/g);
if(sf){
if(sf.join("")!=this){
throw new mod.Exception("Unsupported formating string.");
}
}else{
throw new mod.Exception("Unsupported formating string.");
}
var rslt="";
var s;
var obj;
var cnt=0;
var frmt;
var sign="";
for(var i=0;i<sf.length;i++){
s=sf[i];
if(s=="%%"){
s="%";
}else if(s=="%s"){
if(cnt>=arguments.length){
throw new mod.Exception("Not enough arguments for format string.");
}else{
obj=arguments[cnt];
cnt++;
}
if(obj===null){
obj="null";
}else if(obj===undefined){
obj="undefined";
}
s=obj.toString();
}else if(s.slice(0,1)=="%"){
frmt=new FormatSpecifier(s);
if(frmt.key){
if((typeof arguments[0])=="object"&&arguments.length==1){
obj=arguments[0][frmt.key];
}else{
throw new mod.Exception("Object or associative array expected as formating value.");
}
}else{
if(cnt>=arguments.length){
throw new mod.Exception("Not enough arguments for format string.");
}else{
obj=arguments[cnt];
cnt++;
}
}
if(frmt.type=="s"){
if(obj===null){
obj="null";
}else if(obj===undefined){
obj="undefined";
}
s=pad(obj.toString(),frmt.paddingFlag,frmt.minLength);
}else if(frmt.type=="c"){
if(frmt.paddingFlag=="0"){
frmt.paddingFlag=" ";
}
if(typeof obj=="number"){
s=pad(String.fromCharCode(obj),frmt.paddingFlag,frmt.minLength);
}else if(typeof obj=="string"){
if(obj.length==1){
s=pad(obj,frmt.paddingFlag,frmt.minLength);
}else{
throw new mod.Exception("Character of length 1 required.");
}
}else{
throw new mod.Exception("Character or Byte required.");
}
}else if(typeof obj=="number"){
if(obj<0){
obj=-obj;
sign="-";
}else if(frmt.signed){
sign="+";
}else{
sign="";
}
switch(frmt.type){
case "f":
case "F":
if(frmt.percision>-1){
s=obj.toFixed(frmt.percision).toString();
}else{
s=obj.toString();
}
break;
case "E":
case "e":
if(frmt.percision>-1){
s=obj.toExponential(frmt.percision);
}else{
s=obj.toExponential();
}
s=s.replace("e",frmt.type);
break;
case "b":
s=obj.toString(2);
s=pad(s,"0",frmt.percision);
break;
case "o":
s=obj.toString(8);
s=pad(s,"0",frmt.percision);
break;
case "x":
s=obj.toString(16).toLowerCase();
s=pad(s,"0",frmt.percision);
break;
case "X":
s=obj.toString(16).toUpperCase();
s=pad(s,"0",frmt.percision);
break;
default:
s=parseInt(obj).toString();
s=pad(s,"0",frmt.percision);
break;
}
if(frmt.paddingFlag=="0"){
s=pad(s,"0",frmt.minLength-sign.length);
}
s=sign+s;
s=pad(s,frmt.paddingFlag,frmt.minLength);
}else{
throw new mod.Exception("Number required.");
}
}
rslt+=s;
}
return rslt;
};
String.prototype.pad=function(flag,len){
if(flag=="-"){
var c=" ";
}else{
var c=''+flag;
}
var s=c.mul(len-this.length);
if(flag=="-"){
s=this+s;
}else{
s+=this;
}
return s;
};
String.prototype.indent=function(indent){
var out=[];
var s=this.split('\n');
for(var i=0;i<s.length;i++){
out.push(' '.mul(indent)+s[i]);
}
return out.join('\n');
};
String.prototype.mul=function(l){
l=(l<0)?0:l;
var a=new Array(l+1);
return a.join(this);
};
});
