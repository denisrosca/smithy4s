"use strict";(self.webpackChunksmithy4s=self.webpackChunksmithy4s||[]).push([[947],{3905:function(e,t,n){n.d(t,{Zo:function(){return p},kt:function(){return m}});var r=n(7294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function a(e,t){if(null==e)return{};var n,r,i=function(e,t){if(null==e)return{};var n,r,i={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var l=r.createContext({}),c=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},p=function(e){var t=c(e.components);return r.createElement(l.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},d=r.forwardRef((function(e,t){var n=e.components,i=e.mdxType,o=e.originalType,l=e.parentName,p=a(e,["components","mdxType","originalType","parentName"]),d=c(n),m=i,y=d["".concat(l,".").concat(m)]||d[m]||u[m]||o;return n?r.createElement(y,s(s({ref:t},p),{},{components:n})):r.createElement(y,s({ref:t},p))}));function m(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var o=n.length,s=new Array(o);s[0]=d;var a={};for(var l in t)hasOwnProperty.call(t,l)&&(a[l]=t[l]);a.originalType=e,a.mdxType="string"==typeof e?e:i,s[1]=a;for(var c=2;c<o;c++)s[c]=n[c];return r.createElement.apply(null,s)}return r.createElement.apply(null,n)}d.displayName="MDXCreateElement"},6757:function(e,t,n){n.r(t),n.d(t,{contentTitle:function(){return l},default:function(){return d},frontMatter:function(){return a},metadata:function(){return c},toc:function(){return p}});var r=n(7462),i=n(3366),o=(n(7294),n(3905)),s=["components"],a={sidebar_label:"Installation (SBT)",title:"Installation (SBT)"},l=void 0,c={unversionedId:"overview/sbt",id:"overview/sbt",title:"Installation (SBT)",description:"smithy4s-sbt-codegen is a code generating sbt plugin that creates .scala files corresponding to the provided .smithy specs.",source:"@site/../docs/target/jvm-2.13/mdoc/01-overview/03-sbt.md",sourceDirName:"01-overview",slug:"/overview/sbt",permalink:"/smithy4s/docs/overview/sbt",editUrl:"https://github.com/disneystreaming/smithy4s/edit/main/modules/docs/src/01-overview/03-sbt.md",tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_label:"Installation (SBT)",title:"Installation (SBT)"},sidebar:"tutorialSidebar",previous:{title:"Quick Start",permalink:"/smithy4s/docs/overview/quickstart"},next:{title:"Installation (CLI)",permalink:"/smithy4s/docs/overview/cli"}},p=[],u={toc:p};function d(e){var t=e.components,n=(0,i.Z)(e,s);return(0,o.kt)("wrapper",(0,r.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("p",null,(0,o.kt)("inlineCode",{parentName:"p"},"smithy4s-sbt-codegen")," is a code generating sbt plugin that creates ",(0,o.kt)("inlineCode",{parentName:"p"},".scala")," files corresponding to the provided ",(0,o.kt)("inlineCode",{parentName:"p"},".smithy")," specs."),(0,o.kt)("p",null,'The generated code includes traits for any services you might define, as well as case classes for models used in these services. It has no dependencies on external libraries or any specific protocol like HTTP or JSON. It does, however, depend on a "core" library that contains a number of interfaces implemented by the generated code.'),(0,o.kt)("p",null,"In ",(0,o.kt)("inlineCode",{parentName:"p"},"project/plugins.sbt"),":"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'addSbtPlugin("com.disneystreaming.smithy4s"  % "smithy4s-sbt-codegen" % "0.13.1")\n')),(0,o.kt)("p",null,"and enable the plugin in the desired sbt module:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'import smithy4s.codegen.Smithy4sCodegenPlugin\n\nval myModule = project\n  .in(file("modules/my-module"))\n  .enablePlugins(Smithy4sCodegenPlugin)\n  // version for smithy4s-core is sourced from Smithy4sCodegenPlugin\n  .settings(libraryDependencies += "com.disneystreaming.smithy4s" %% "smithy4s-core" % smithy4sVersion.value)\n')),(0,o.kt)("p",null,"This will enable the plugin on ",(0,o.kt)("inlineCode",{parentName:"p"},"myModule"),". We also need to add ",(0,o.kt)("inlineCode",{parentName:"p"},"smithy4s-core ")," here since it is needed for compiling the generated code."),(0,o.kt)("p",null,"By default, the plugin will look in the ",(0,o.kt)("inlineCode",{parentName:"p"},"$MY_MODULE/src/main/smithy")," directory and will write scala code in ",(0,o.kt)("inlineCode",{parentName:"p"},"$MY_MODULE/target/scala-<version>/src_managed/")," when invoking ",(0,o.kt)("inlineCode",{parentName:"p"},"compile"),". The paths are configurable via the ",(0,o.kt)("inlineCode",{parentName:"p"},"smithy4sInputDir")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"smithy4sOutputDir")," settings keys."),(0,o.kt)("p",null,"For example, in order for the plugin to source ",(0,o.kt)("inlineCode",{parentName:"p"},".smithy")," specs from ",(0,o.kt)("inlineCode",{parentName:"p"},"./smithy_input")," (inside the folder where our ",(0,o.kt)("inlineCode",{parentName:"p"},"build.sbt")," is) and output the generated files into ",(0,o.kt)("inlineCode",{parentName:"p"},"./smithy_output"),"."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'val myModule = project\n  .in(file("modules/my-module"))\n  .enablePlugins(Smithy4sCodegenPlugin)\n  .settings(\n    scalaVersion := "2.13.8",\n    smithy4sInputDir in Compile  := (baseDirectory in ThisBuild).value / "smithy_input",\n    smithy4sOutputDir in Compile := (baseDirectory in ThisBuild).value / "smithy_output",\n    libraryDependencies += "com.disneystreaming.smithy4s" %% "smithy4s-core" % smithy4sVersion.value\n  )\n')))}d.isMDXComponent=!0}}]);