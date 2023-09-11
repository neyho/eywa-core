"use strict";(self.webpackChunkeywa_graphiql_id=self.webpackChunkeywa_graphiql_id||[]).push([[415],{2415:function(e,n,t){t.r(n),t.d(n,{c:function(){return c}});var i=t(8457),l=Object.defineProperty,o=function(e,n){return l(e,"name",{value:n,configurable:!0})};function r(e,n){return n.forEach((function(n){n&&"string"!==typeof n&&!Array.isArray(n)&&Object.keys(n).forEach((function(t){if("default"!==t&&!(t in e)){var i=Object.getOwnPropertyDescriptor(n,t);Object.defineProperty(e,t,i.get?i:{enumerable:!0,get:function(){return n[t]}})}}))})),Object.freeze(Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}))}o(r,"_mergeNamespaces");var a={exports:{}};!function(e){var n={},t=/[^\s\u00a0]/,i=e.Pos,l=e.cmpPos;function r(e){var n=e.search(t);return-1==n?0:n}function a(e,n,t){return/\bstring\b/.test(e.getTokenTypeAt(i(n.line,0)))&&!/^[\'\"\`]/.test(t)}function c(e,n){var t=e.getMode();return!1!==t.useInnerComments&&t.innerMode?e.getModeAt(n):t}o(r,"firstNonWS"),e.commands.toggleComment=function(e){e.toggleComment()},e.defineExtension("toggleComment",(function(e){e||(e=n);for(var t=this,l=1/0,o=this.listSelections(),r=null,a=o.length-1;a>=0;a--){var c=o[a].from(),m=o[a].to();c.line>=l||(m.line>=l&&(m=i(l,0)),l=c.line,null==r?t.uncomment(c,m,e)?r="un":(t.lineComment(c,m,e),r="line"):"un"==r?t.uncomment(c,m,e):t.lineComment(c,m,e))}})),o(a,"probablyInsideString"),o(c,"getMode"),e.defineExtension("lineComment",(function(e,l,o){o||(o=n);var m=this,f=c(m,e),g=m.getLine(e.line);if(null!=g&&!a(m,e,g)){var s=o.lineComment||f.lineComment;if(s){var u=Math.min(0!=l.ch||l.line==e.line?l.line+1:l.line,m.lastLine()+1),d=null==o.padding?" ":o.padding,h=o.commentBlankLines||e.line==l.line;m.operation((function(){if(o.indent){for(var n=null,l=e.line;l<u;++l){var a=(c=m.getLine(l)).slice(0,r(c));(null==n||n.length>a.length)&&(n=a)}for(l=e.line;l<u;++l){var c=m.getLine(l),f=n.length;(h||t.test(c))&&(c.slice(0,f)!=n&&(f=r(c)),m.replaceRange(n+s+d,i(l,0),i(l,f)))}}else for(l=e.line;l<u;++l)(h||t.test(m.getLine(l)))&&m.replaceRange(s+d,i(l,0))}))}else(o.blockCommentStart||f.blockCommentStart)&&(o.fullLines=!0,m.blockComment(e,l,o))}})),e.defineExtension("blockComment",(function(e,o,r){r||(r=n);var a=this,m=c(a,e),f=r.blockCommentStart||m.blockCommentStart,g=r.blockCommentEnd||m.blockCommentEnd;if(f&&g){if(!/\bcomment\b/.test(a.getTokenTypeAt(i(e.line,0)))){var s=Math.min(o.line,a.lastLine());s!=e.line&&0==o.ch&&t.test(a.getLine(s))&&--s;var u=null==r.padding?" ":r.padding;e.line>s||a.operation((function(){if(0!=r.fullLines){var n=t.test(a.getLine(s));a.replaceRange(u+g,i(s)),a.replaceRange(f+u,i(e.line,0));var c=r.blockCommentLead||m.blockCommentLead;if(null!=c)for(var d=e.line+1;d<=s;++d)(d!=s||n)&&a.replaceRange(c+u,i(d,0))}else{var h=0==l(a.getCursor("to"),o),p=!a.somethingSelected();a.replaceRange(g,o),h&&a.setSelection(p?o:a.getCursor("from"),o),a.replaceRange(f,e)}}))}}else(r.lineComment||m.lineComment)&&0!=r.fullLines&&a.lineComment(e,o,r)})),e.defineExtension("uncomment",(function(e,l,o){o||(o=n);var r,a=this,m=c(a,e),f=Math.min(0!=l.ch||l.line==e.line?l.line:l.line-1,a.lastLine()),g=Math.min(e.line,f),s=o.lineComment||m.lineComment,u=[],d=null==o.padding?" ":o.padding;e:if(s){for(var h=g;h<=f;++h){var p=a.getLine(h),v=p.indexOf(s);if(v>-1&&!/comment/.test(a.getTokenTypeAt(i(h,v+1)))&&(v=-1),-1==v&&t.test(p))break e;if(v>-1&&t.test(p.slice(0,v)))break e;u.push(p)}if(a.operation((function(){for(var e=g;e<=f;++e){var n=u[e-g],t=n.indexOf(s),l=t+s.length;t<0||(n.slice(l,l+d.length)==d&&(l+=d.length),r=!0,a.replaceRange("",i(e,t),i(e,l)))}})),r)return!0}var b=o.blockCommentStart||m.blockCommentStart,C=o.blockCommentEnd||m.blockCommentEnd;if(!b||!C)return!1;var k=o.blockCommentLead||m.blockCommentLead,L=a.getLine(g),x=L.indexOf(b);if(-1==x)return!1;var y=f==g?L:a.getLine(f),O=y.indexOf(C,f==g?x+b.length:0),S=i(g,x+1),R=i(f,O+1);if(-1==O||!/comment/.test(a.getTokenTypeAt(S))||!/comment/.test(a.getTokenTypeAt(R))||a.getRange(S,R,"\n").indexOf(C)>-1)return!1;var T=L.lastIndexOf(b,e.ch),E=-1==T?-1:L.slice(0,e.ch).indexOf(C,T+b.length);if(-1!=T&&-1!=E&&E+C.length!=e.ch)return!1;E=y.indexOf(C,l.ch);var M=y.slice(l.ch).lastIndexOf(b,E-l.ch);return T=-1==E||-1==M?-1:l.ch+M,(-1==E||-1==T||T==l.ch)&&(a.operation((function(){a.replaceRange("",i(f,O-(d&&y.slice(O-d.length,O)==d?d.length:0)),i(f,O+C.length));var e=x+b.length;if(d&&L.slice(e,e+d.length)==d&&(e+=d.length),a.replaceRange("",i(g,x),i(g,e)),k)for(var n=g+1;n<=f;++n){var l=a.getLine(n),o=l.indexOf(k);if(-1!=o&&!t.test(l.slice(0,o))){var r=o+k.length;d&&l.slice(r,r+d.length)==d&&(r+=d.length),a.replaceRange("",i(n,o),i(n,r))}}})),!0)}))}(i.a.exports);var c=r({__proto__:null,default:a.exports},[a.exports])}}]);
//# sourceMappingURL=415.0af254be.chunk.js.map