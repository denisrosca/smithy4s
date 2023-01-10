"use strict";(self.webpackChunksmithy4s=self.webpackChunksmithy4s||[]).push([[157],{3905:function(e,t,n){n.d(t,{Zo:function(){return u},kt:function(){return m}});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=r.createContext({}),p=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},u=function(e){var t=p(e.components);return r.createElement(s.Provider,{value:t},e.children)},d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},c=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,s=e.parentName,u=o(e,["components","mdxType","originalType","parentName"]),c=p(n),m=a,h=c["".concat(s,".").concat(m)]||c[m]||d[m]||i;return n?r.createElement(h,l(l({ref:t},u),{},{components:n})):r.createElement(h,l({ref:t},u))}));function m(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,l=new Array(i);l[0]=c;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o.mdxType="string"==typeof e?e:a,l[1]=o;for(var p=2;p<i;p++)l[p]=n[p];return r.createElement.apply(null,l)}return r.createElement.apply(null,n)}c.displayName="MDXCreateElement"},9113:function(e,t,n){n.r(t),n.d(t,{assets:function(){return u},contentTitle:function(){return s},default:function(){return m},frontMatter:function(){return o},metadata:function(){return p},toc:function(){return d}});var r=n(7462),a=n(3366),i=(n(7294),n(3905)),l=["components"],o={sidebar_label:"Unwrapping",title:"New types (and unwrapping)"},s=void 0,p={unversionedId:"codegen/customisation/unwrapping",id:"codegen/customisation/unwrapping",title:"New types (and unwrapping)",description:"By default, smithy4s will wrap all standalone primitive types in a Newtype. A standalone primitive type is one that is defined like the following:",source:"@site/../docs/target/jvm-2.13/mdoc/04-codegen/01-customisation/05-unwrapping.md",sourceDirName:"04-codegen/01-customisation",slug:"/codegen/customisation/unwrapping",permalink:"/smithy4s/docs/codegen/customisation/unwrapping",draft:!1,editUrl:"https://github.com/disneystreaming/smithy4s/edit/main/modules/docs/src/04-codegen/01-customisation/05-unwrapping.md",tags:[],version:"current",sidebarPosition:5,frontMatter:{sidebar_label:"Unwrapping",title:"New types (and unwrapping)"},sidebar:"tutorialSidebar",previous:{title:"Type refinements",permalink:"/smithy4s/docs/codegen/customisation/refinements"},next:{title:"Error Unions",permalink:"/smithy4s/docs/codegen/customisation/error-unions"}},u={},d=[{value:"Default rendering",id:"default-rendering",level:2},{value:"FULL",id:"full",level:4},{value:"OPTION_ONLY",id:"option_only",level:4},{value:"NONE",id:"none",level:4}],c={toc:d};function m(e){var t=e.components,n=(0,a.Z)(e,l);return(0,i.kt)("wrapper",(0,r.Z)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("p",null,"By default, smithy4s will wrap all standalone primitive types in a Newtype. A standalone primitive type is one that is defined like the following:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-kotlin"},"string Email // standalone primitive\n\nstructure Test {\n  email: Email\n  other: String // not a standalone primitive\n}\n")),(0,i.kt)("p",null,"Given this example, smithy4s would generate something like the following:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"final case class Test(email: Email, other: String)\n")),(0,i.kt)("p",null,"This wrapping may be undesirable in some circumstances. As such, we've provided the ",(0,i.kt)("inlineCode",{parentName:"p"},"smithy4s.meta#unwrap")," trait. This trait tells the smithy4s code generation to not wrap these types in a newtype when they are used."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-kotlin"},"use smithy4s.meta#unwrap\n\n@unwrap\nstring Email\n\nstructure Test {\n  email: Email\n  other: String\n}\n")),(0,i.kt)("p",null,"This would now generate something like:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"final case class Test(email: String, other: String)\n")),(0,i.kt)("p",null,"This can be particularly useful when working with refinement types (see above for details on refinements). By default, any type that is ",(0,i.kt)("inlineCode",{parentName:"p"},"refined")," will be generated inside of a newtype. If you don't want this, you can mark the type with the ",(0,i.kt)("inlineCode",{parentName:"p"},"unwrap")," trait."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-kotlin"},'@trait(selector: "string")\nstructure emailFormat {}\n\n@emailFormat\n@unwrap\nstring Email\n')),(0,i.kt)("admonition",{type:"info"},(0,i.kt)("p",{parentName:"admonition"},"By default, smithy4s renders collection types as unwrapped EXCEPT when the collection has been refined. In this case, the collection will be rendered within a newtype by default. If you wish your refined collection be rendered unwrapped, you can accomplish this using the same ",(0,i.kt)("inlineCode",{parentName:"p"},"@unwrap")," trait annotation on it.")),(0,i.kt)("h2",{id:"default-rendering"},"Default rendering"),(0,i.kt)("p",null,"Smithy4s allows you to customize how defaults on the fields of smithy structures are rendered inside of case classes. There are three options:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"FULL")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"OPTION_ONLY")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"NONE"))),(0,i.kt)("p",null,"The default is ",(0,i.kt)("inlineCode",{parentName:"p"},"FULL"),"."),(0,i.kt)("p",null,"This value is set using metadata which means that the setting will be applied to all the rendering done by smithy4s."),(0,i.kt)("h4",{id:"full"},"FULL"),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"FULL")," means that default values are rendered for all field types. For example:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-kotlin"},'metadata smithy4sDefaultRenderMode = "FULL"\n\nstructure FullExample {\n  one: Integer = 1\n  two: String\n  @required\n  three: String\n}\n')),(0,i.kt)("p",null,"would render to something like:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"case class FullExample(three: String, one: Int = 1, two: Option[String] = None)\n")),(0,i.kt)("p",null,"Notice how the fields above are ordered. The reason for this is that fields are ordered as:"),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},"Required Fields"),(0,i.kt)("li",{parentName:"ol"},"Fields with defaults"),(0,i.kt)("li",{parentName:"ol"},"Optional Fields")),(0,i.kt)("h4",{id:"option_only"},"OPTION_ONLY"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-kotlin"},'metadata smithy4sDefaultRenderMode = "OPTION_ONLY"\n\nstructure OptionExample {\n  one: Integer = 1\n  two: String\n  @required\n  three: String\n}\n')),(0,i.kt)("p",null,"would render to something like:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"case class FullExample(one: String, three: String, two: Option[String] = None)\n")),(0,i.kt)("p",null,"Now ",(0,i.kt)("inlineCode",{parentName:"p"},"one")," doesn't have a default rendered and as such it is placed first in the case class."),(0,i.kt)("h4",{id:"none"},"NONE"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-kotlin"},'metadata smithy4sDefaultRenderMode = "NONE"\n\nstructure OptionExample {\n  one: Integer = 1\n  two: String\n  @required\n  three: String\n}\n')),(0,i.kt)("p",null,"would render to something like:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"case class FullExample(one: String, two: Option[String], three: String)\n")),(0,i.kt)("p",null,"Now none of the fields are rendered with defaults. As such, the order of the fields is the same as is defined in the smithy structure."),(0,i.kt)("admonition",{type:"caution"},(0,i.kt)("p",{parentName:"admonition"},"The presence of the ",(0,i.kt)("inlineCode",{parentName:"p"},"smithy4sDefaultRenderMode")," metadata does NOT change the way smithy4s codecs behave. As such, defaults will still be used when decoding\nfields inside of clients and servers. This feature is purely for changing the generated code for your convenience.")))}m.isMDXComponent=!0}}]);