var tv1As = tv1As || (function(){var _scrs=document.getElementsByTagName('script'),_scr=(_scrs)?_scrs[_scrs.length-1]:null,src=(_scr)?_scr.src:'',players=[],loading=false,loaded=false;return {addPlayer:function(obj){obj.nrd = true;players.push(obj);if(this.isLoaded()){obj.init();}},removePlayer:function(obj){for(var i=this.getPlayers().length-1;i>-1;i--){if(this.getPlayers()[i].id == obj.id){this.getPlayers().splice(i,1);}}obj.unload();},getPlayers:function(){return players;},isLoading:function(){return loading;},setLoading:function(b){loading=b;},isLoaded:function(){return loaded;},setLoaded:function(b){loaded=b;},initPlayers:function(){for(var i=0;i<this.getPlayers().length;i++){if(window.console)window.console.log("ShowItAsync initPlayers "+ this.getPlayers()[i].id );this.getPlayers()[i].init();}},getSrc:function(){return src;}};})();var ShowIt=function(id,x,y,w,h,as,lid,vs,opt){this.loaded=function(){tv1As.setLoaded(true);tv1As.initPlayers();};this.init=function(){if(!this.nrd){return;}delete this.nrd;var me=this.owner || this;try {var si=new eu.tv1.ShowItAsync(me.id,me.x,me.y,me.width,me.height,me.as,me.lid,me.vs);if(!si.setWmode){if(window.console){window.console.log("ERROR: Player Object could not be created");}return;}if(me.wmode)si.setWmode(me.wmode);if(me.app)si.setApplication(me.app);if(me.adaptiveBitrates)si.setAdaptiveBitrates.apply(si,me.adaptiveBitrates);if(me.audioOnly != undefined)si.audioOnly=me.audioOnly;if(me.autoplay != undefined)si.autoplay=me.autoplay;if(me.autoplayOnLoad != undefined)si.autoplayOnLoad=me.autoplayOnLoad;if(me.bandwidth)si.bandwidth=me.bandwidth;if(me.bgcolor)si.setBgcolor(me.bgcolor);if(me.category)si.setCategory(me.category);if(me.config)si.setConfig(me.config);if(me.content)si.setContent(me.content);if(me.customParams)si.customParams=me.customParams;if(me.debug != undefined)si.debug=me.debug;if(me.disablePhaseCheck)si.disablePhaseCheck=me.disablePhaseCheck;if(me.embedType)si.setEmbedType(me.embedType);if(me.extras)si.disableExtras.apply(si,me.extras);if(me.flashPreset)si.setFlashPreset(me.flashPreset);if(me.flashSkin)si.setFlashSkin(me.flashSkin);if(me.guiLanguage)si.setGuiLanguage(me.guiLanguage);if(me.hp)si.setHomepage(me.hp);if(me.labels)si.setLanguageLabels(me.labels);if(me.language)si.language=me.language;if(me.languages)si.setLanguages(me.languages);if(me.mobileSkin)si.setMobileSkin(me.mobileSkin);if(me.mobilePreset)si.setMobilePreset(me.mobilePreset);if(me.playlist)si.setPlaylist(me.playlist);if(me.playlistTemplate)si.playlistTemplate=me.playlistTemplate;if(me.playlistResource)si.playlistResource=me.playlistResource;if(me.dvr)si.dvr=me.dvr;if(me.secureKey)si.setSecureKey(me.secureKey);if(me.startIdx)si.setStartIndex(me.startIdx);if(me.startTime)si.setStartTime(me.startTime);if(me.streamUrl)si.setStreamUrl(me.streamUrl);if(me.skin)si.setSkin(me.skin);if(me.theoVersion)si.theoVersion=me.theoVersion;if(me.tokens)si.tokens=me.tokens;if(me.webcast)si.setWebcast(me.webcast);if(me.frameMethod)si.frameMethod = me.frameMethod;if(me.loop)si.loop = me.loop;if(me.trackLogLevel > 0)si.trackLogLevel = me.trackLogLevel;if(me.vastConfig)si.vastConfig = me.vastConfig;if(me.h5Events)si.setHtml5Events(me.h5Events);if(me.frameMethod)si.frameMEthod = me.frameMethod;si.sv=me.sv;if(me.cfg){if(me.cfg.type=='ie') {si.insertIntoElement(me.cfg.id,me.cfg.w,me.cfg.h,me.cfg.kvar);}else{si.livePreviewToElement(me.cfg.id,me.cfg.w,me.cfg.h,me.cfg.kvar,me.cfg.ch);}}for(var i=0;i<me.queue.length;i++){var o=me.queue[i];if(o.a=='com'){si.comet();}else if(o.a=='loa'){si.load(o.p);}else if(o.a=='pau'){si.pause();}else if(o.a=='pla'){si.play();}else if(o.a=='pos'){si.setPosition(o.m,o.p);}else if(o.a=='idx'){si.setCurrentIndex(o.i);}else if(o.a=='em'){si.setEventManager(o.e);}else if(o.a=='adl'){si.addListener(o.t,o.c,o.s);}}me.player=si;} catch(e){if(window.console && window.console.warn)window.console.warn("Init Error "+ e);}};this.loadScript=function(){try{var me=this;var scrpt=document.createElement('script');scrpt.type='text/javascript';scrpt.async=true;(document.getElementsByTagName('head')[0] || document.body).appendChild(scrpt);scrpt.owner=me;if(scrpt.addEventListener){scrpt.addEventListener('load',me.loaded,false);}else{scrpt.onreadystatechange=function() { if (scrpt.readyState in {loaded:1,complete:1} && window.eu != undefined) {me.owner = me;me.loaded();scrpt.onreadystatechange=null;}};}var url='https://webtv.bundestag.de/player/macros/bttv/hls/showItSimple.js?async=1&rspcss=1&preset=embed_fade&theov=2.40.0';if(me.opt){if(me.opt.preset){url+='&preset='+me.opt.preset;}if(me.opt.rspcss){url+='&rspcss='+me.opt.rspcss;}if(me.opt.skin){url=url.replace(/\/_s_[^\/]*\//, "/");url=url.replace(/\/player\/macros\//,"/player/macros/_s_"+ me.opt.skin+"/");}if(me.opt.test){url=url.replace(/showItSimple\./, "showItSimple_update.");}}scrpt.src=url;} catch(e){if(window.console && window.console.warn)window.console.warn("Load Error "+ e);}};this.getCurrentIndex=function(){if(this.player){return this.player.getCurrentIndex();}};this.isPlaying=function(){if(this.player){return this.player.isPlaying();}};this.load=function(v){var force = (v) ? v : true;if(this.player){this.player.load(force);}else{this.queue.push({a:'loa',p:force});}};this.unload=function(){if(this.player){this.player.unload();}};this.pause=function(){if(this.player){this.player.pause();}else{this.queue.push({a:'pau'});}};this.play=function(){if(this.player){this.player.play();}else{this.queue.push({a:'pla'});}};this.setPosition=function(ms,play){if(this.player){this.player.setPosition(ms,play);}else{this.queue.push({a:'pos',m:ms,p:play});}};this.addListener=function(type, callback, scope){if(this.player){this.player.addListener(type, callback, scope);}else{this.queue.push({a:'adl',t:type,c:callback,s:scope});}};this.removeListener=function(type, callback, scope){if(this.player){this.player.removeListener(type, callback, scope);}};this.player=null;this.as=as;this.id=id;this.height=h;this.lid=lid;this.vs=vs;this.width=w;this.x=x;this.y=y;this.opt=opt;this.app;this.audioOnly;this.autoplay;this.autoplayOnLoad;this.bandwidth;this.bgcolor;this.category;this.config;this.content;this.customParams;this.debug=false;this.frameMethod='get';this.disablePhaseCheck;this.embedType;this.extras;this.flashPreset;this.flashSkin;this.guiLanguage;this.h5Events;this.hp;this.labels;this.language;this.languages;this.mobileSkin;this.playlist;this.playlistTemplate;this.secureKey;this.skin;this.startIdx;this.startTime;this.streamUrl;this.theoVersion;this.tokens;this.webcast;this.wmode;this.sv='A170913';this.cfg;this.queue=[];this.uparams;this.disableExtras=function(){this.extras=arguments;if(this.player){this.player.disableExtras(arguments);}};this.setApplication=function(v){this.app=v;if(this.player){this.player.setApplication(v);}};this.setAdaptiveBitrates=function(){this.adaptiveBitrates=arguments;if(this.player){this.player.setAdaptiveBitrates(arguments);}};this.setBgcolor=function(v){this.bgcolor=v;if(this.player){this.player.setBgcolor(v);}};this.setCategory=function(v){this.category=v;if(this.player){this.player.setCategory(v);}};this.setConfig=function(v){this.config=v;if(this.player){this.player.setConfig(v);}};this.setContent=function(v){this.content=v;if(this.player){this.player.setContent(v);}};this.setCurrentIndex=function(v){if(this.player){this.player.setCurrentIndex(v);}else{this.queue.push({a:'idx',i:v});}};this.setEventManager=function(v){if(this.player){this.player.setEventManager(v);}else{this.queue.push({a:'em',e:v});}};this.setFlashPreset=function(v){this.flashPreset=v;if(this.player){this.player.setFlashPreset(v);}};this.setFlashSkin=function(v){this.flashSkin=v;if(this.player){this.player.setFlashSkin(v);}};this.setGuiLanguage=function(v){this.guiLanguage=v;if(this.player){this.player.setGuiLanguage(v);}};this.setHtml5Events=function(v){this.h5Events=v;if(this.player){this.player.setHtml5Events(v);}};this.setHomepage=function(v){this.hp=v;if(this.player){this.player.setHomepage(v);}};this.setIndex=function(v){this.startIdx=v-1;if(this.player){this.player.setStartIndex(v-1);}};this.setLanguage=function(v){this.language=v;if(this.player){this.player.setLanguage(v);}};this.setLanguages=function(v){this.languages=v;if(this.player){this.player.setLanguages(v);}};this.setLanguageLabels=function(v){this.labels=v;if(this.player){this.player.setLanguageLabels(v);}};this.setMobilePreset=function(v){this.mobilePreset=v;if(this.player){this.player.setMobilePreset(v);}};this.setMobileSkin=function(v){this.mobileSkin=v;if(this.player){this.player.setMobileSkin(v);}};this.setPlaylist=function(v){this.playlist=v;if(this.player){this.player.setPlaylist(v);}};this.setSecureKey=function(v){this.secureKey=v;if(this.player){this.player.setSecureKey(v);}};this.setSkin=function(v){this.skin=v;if(this.player){this.player.setSkin(v);}};this.setStartIndex=function(v){this.startIdx=v;if(this.player){this.player.setStartIndex(v);}};this.setStartTime=function(v){this.startTime=v;if(this.player){this.player.setStartTime(v);}};this.setStreamUrl=function(v){this.streamUrl=v;if(this.player){this.player.setStreamUrl(v);}};this.setWebcast=function(v){this.webcast=v;if(this.player){this.player.setWebcast(v);}};this.setWmode=function(v){this.wmode=v;if(this.player){this.player.setWmode(v);}};this.insertIntoElement=function(id,vWidth,vHeight,kVAR){if(this.player){this.player.insertIntoElement(id,vWidth,vHeight,kVAR);}else{this.cfg={type:'ie',id:id,w:vWidth,h:vHeight,kvar:kVAR};tv1As.addPlayer(this);}};this.destroy=function(){tv1As.removePlayer(this);};this.getParam=function(q,s) {if(! this.uparams) {s=s ? s : window.location.search;var re=new RegExp("([\?|\&])([^\=]+)\=([^&]+)",'ig');var res={};s.replace(re,function (m,d,k,v) {res[k]=v || null;});this.uparams=res;}if(this.uparams && this.uparams[q]){return this.uparams[q];}return null;};if(tv1As.isLoading() === false) {tv1As.setLoading(true);this.loadScript();}};