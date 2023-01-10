"use strict";(self.webpackChunksmithy4s=self.webpackChunksmithy4s||[]).push([[176],{3905:function(e,t,n){n.d(t,{Zo:function(){return p},kt:function(){return c}});var i=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);t&&(i=i.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,i)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,i,a=function(e,t){if(null==e)return{};var n,i,a={},r=Object.keys(e);for(i=0;i<r.length;i++)n=r[i],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(i=0;i<r.length;i++)n=r[i],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=i.createContext({}),d=function(e){var t=i.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},p=function(e){var t=d(e.components);return i.createElement(s.Provider,{value:t},e.children)},h={inlineCode:"code",wrapper:function(e){var t=e.children;return i.createElement(i.Fragment,{},t)}},u=i.forwardRef((function(e,t){var n=e.components,a=e.mdxType,r=e.originalType,s=e.parentName,p=l(e,["components","mdxType","originalType","parentName"]),u=d(n),c=a,m=u["".concat(s,".").concat(c)]||u[c]||h[c]||r;return n?i.createElement(m,o(o({ref:t},p),{},{components:n})):i.createElement(m,o({ref:t},p))}));function c(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var r=n.length,o=new Array(r);o[0]=u;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l.mdxType="string"==typeof e?e:a,o[1]=l;for(var d=2;d<r;d++)o[d]=n[d];return i.createElement.apply(null,o)}return i.createElement.apply(null,n)}u.displayName="MDXCreateElement"},7807:function(e,t,n){n.r(t),n.d(t,{assets:function(){return p},contentTitle:function(){return s},default:function(){return c},frontMatter:function(){return l},metadata:function(){return d},toc:function(){return h}});var i=n(7462),a=n(3366),r=(n(7294),n(3905)),o=["components"],l={sidebar_label:"Endpoint Specific Middleware",title:"Endpoint Specific Middleware"},s=void 0,d={unversionedId:"guides/endpoint-middleware",id:"guides/endpoint-middleware",title:"Endpoint Specific Middleware",description:"It used to be the case that any middleware implemented for smithy4s services would have to operate at the http4s level, without any knowledge of smithy4s or access to the constructs to utilizes.",source:"@site/../docs/target/jvm-2.13/mdoc/06-guides/endpoint-middleware.md",sourceDirName:"06-guides",slug:"/guides/endpoint-middleware",permalink:"/smithy4s/docs/guides/endpoint-middleware",draft:!1,editUrl:"https://github.com/disneystreaming/smithy4s/edit/main/modules/docs/src/06-guides/endpoint-middleware.md",tags:[],version:"current",frontMatter:{sidebar_label:"Endpoint Specific Middleware",title:"Endpoint Specific Middleware"},sidebar:"tutorialSidebar",previous:{title:"Services and endpoints",permalink:"/smithy4s/docs/design/services"},next:{title:"Extracting Request Info",permalink:"/smithy4s/docs/guides/extract-request-info"}},p={},h=[{value:"ServerEndpointMiddleware / ClientEndpointMiddleware",id:"serverendpointmiddleware--clientendpointmiddleware",level:2},{value:"Smithy Spec",id:"smithy-spec",level:2},{value:"Server-side Middleware",id:"server-side-middleware",level:2},{value:"AuthChecker",id:"authchecker",level:4},{value:"The Inner Middleware Implementation",id:"the-inner-middleware-implementation",level:4},{value:"ServerEndpointMiddleware.Simple",id:"serverendpointmiddlewaresimple",level:4},{value:"Using the Middleware",id:"using-the-middleware",level:4},{value:"Client-side Middleware",id:"client-side-middleware",level:2},{value:"ClientEndpointMiddleware.Simple",id:"clientendpointmiddlewaresimple",level:4},{value:"SimpleRestJsonBuilder",id:"simplerestjsonbuilder",level:4},{value:"Conclusion",id:"conclusion",level:2}],u={toc:h};function c(e){var t=e.components,n=(0,a.Z)(e,o);return(0,r.kt)("wrapper",(0,i.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("p",null,"It used to be the case that any middleware implemented for smithy4s services would have to operate at the http4s level, without any knowledge of smithy4s or access to the constructs to utilizes."),(0,r.kt)("p",null,"As of version ",(0,r.kt)("inlineCode",{parentName:"p"},"0.17.x")," of smithy4s, we have changed this by providing a new mechanism to build and provide middleware. This mechanism is aware of the smithy4s service and endpoints that are derived from your smithy specifications. As such, this unlocks the possibility to build middleware that utilizes and is compliant to the traits and shapes of your smithy specification."),(0,r.kt)("p",null,"In this guide, we will show how you can implement a smithy4s middleware that is aware of the authentication traits in your specification and is able to implement authenticate on an endpoint-by-endpoint basis. This is useful if you have different or no authentication on one or more endpoints."),(0,r.kt)("h2",{id:"serverendpointmiddleware--clientendpointmiddleware"},"ServerEndpointMiddleware / ClientEndpointMiddleware"),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"ServerEndpointMiddleware")," is the interface that we have provided for implementing service middleware. For some use cases, you will need to use the full interface. However, for this guide and for many use cases, you will be able to rely on the simpler interface called ",(0,r.kt)("inlineCode",{parentName:"p"},"ServerEndpointMiddleware.Simple"),". This interface requires a single method which looks as follows:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"def prepareWithHints(\n        serviceHints: Hints,\n        endpointHints: Hints\n    ): HttpApp[F] => HttpApp[F]\n")),(0,r.kt)("p",null,"This means that given the hints for the service and a specific endpoint, our implementation will provide a transformation of an ",(0,r.kt)("inlineCode",{parentName:"p"},"HttpApp"),". If you are not familiar with ",(0,r.kt)("inlineCode",{parentName:"p"},"Hints"),", they are the smithy4s construct that represents Smithy Traits. They are called hints to avoid naming conflicts and confusion with Scala ",(0,r.kt)("inlineCode",{parentName:"p"},"trait"),"s."),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"ClientEndpointMiddleware")," interface is essentially the same as the one for ",(0,r.kt)("inlineCode",{parentName:"p"},"ServerEndpointMiddleware")," with the exception that we are returning a transformation on ",(0,r.kt)("inlineCode",{parentName:"p"},"Client[F]")," instead of ",(0,r.kt)("inlineCode",{parentName:"p"},"HttpApp[F]"),". This looks like:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"def prepareWithHints(\n        serviceHints: Hints,\n        endpointHints: Hints\n    ): Client[F] => Client[F]\n")),(0,r.kt)("h2",{id:"smithy-spec"},"Smithy Spec"),(0,r.kt)("p",null,"Let's look at the smithy specification that we will use for this guide. First, let's define the service."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-kotlin"},'$version: "2"\n\nnamespace smithy4s.guides.auth\n\nuse alloy#simpleRestJson\n\n@simpleRestJson\n@httpBearerAuth\nservice HelloWorldAuthService {\n  version: "1.0.0",\n  operations: [SayWorld, HealthCheck]\n  errors: [NotAuthorizedError]\n}\n')),(0,r.kt)("p",null,"Here we defined a service that has two operations, ",(0,r.kt)("inlineCode",{parentName:"p"},"SayWorld")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"HealthCheck"),". We defined it such that any of these operations may return an ",(0,r.kt)("inlineCode",{parentName:"p"},"NotAuthorizedError"),". Finally, we annotated the service with the ",(0,r.kt)("inlineCode",{parentName:"p"},"@httpBearerAuth")," ",(0,r.kt)("a",{parentName:"p",href:"https://smithy.io/2.0/spec/authentication-traits.html#httpbearerauth-trait"},"trait")," to indicate that the service supports authentication via a bearer token. If you are using a different authentication scheme, you can still follow this guide and adapt it for your needs. You can find a full list of smithy-provided schemes ",(0,r.kt)("a",{parentName:"p",href:"https://smithy.io/2.0/spec/authentication-traits.html"},"here"),". If none of the provided traits suit your use case, you can always create a custom trait too."),(0,r.kt)("p",null,"Next, let's define our first operation, ",(0,r.kt)("inlineCode",{parentName:"p"},"SayWorld"),":"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-kotlin"},'@readonly\n@http(method: "GET", uri: "/hello", code: 200)\noperation SayWorld {\n  output: World\n}\n\nstructure World {\n  message: String = "World !"\n}\n')),(0,r.kt)("p",null,"There is nothing authentication-specific defined with this operation, this means that the operation inherits the service-defined authentication scheme (",(0,r.kt)("inlineCode",{parentName:"p"},"httpBearerAuth")," in this case). Let's contrast this with the ",(0,r.kt)("inlineCode",{parentName:"p"},"HealthCheck")," operation:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-kotlin"},'@readonly\n@http(method: "GET", uri: "/health", code: 200)\n@auth([])\noperation HealthCheck {\n  output := {\n    @required\n    message: String\n  }\n}\n')),(0,r.kt)("p",null,"Notice that on this operation we have added the ",(0,r.kt)("inlineCode",{parentName:"p"},"@auth([])")," trait with an empty array. This means that there is no authentication required for this endpoint. In other words, although the service defines an authentication scheme of ",(0,r.kt)("inlineCode",{parentName:"p"},"httpBearerAuth"),", that scheme will not apply to this endpoint."),(0,r.kt)("p",null,"Finally, let's define the ",(0,r.kt)("inlineCode",{parentName:"p"},"NotAuthorizedError")," that will be returned when an authentication token is missing or invalid."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-kotlin"},'@error("client")\n@httpError(401)\nstructure NotAuthorizedError {\n  @required\n  message: String\n}\n')),(0,r.kt)("p",null,"There is nothing authentication specific about this error, this is a standard smithy http error that will have a 401 status code when returned."),(0,r.kt)("p",null,"If you want to see the full smithy model we defined above, you can do so ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/disneystreaming/smithy4s/blob/2961b9cd2b9abb60c89b8f8424f5da61546f6a5a/modules/guides/smithy/auth.smithy"},"here"),"."),(0,r.kt)("h2",{id:"server-side-middleware"},"Server-side Middleware"),(0,r.kt)("p",null,"To see the ",(0,r.kt)("strong",{parentName:"p"},"full code")," example of what we walk through below, go ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/disneystreaming/smithy4s/blob/2961b9cd2b9abb60c89b8f8424f5da61546f6a5a/modules/guides/src/smithy4s/guides/Auth.scala"},"here"),"."),(0,r.kt)("p",null,"We will create a server-side middleware that implements the authentication as defined in the smithy spec above. Let's start by creating a few classes that we will use in our middleware."),(0,r.kt)("h4",{id:"authchecker"},"AuthChecker"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"case class ApiToken(value: String)\n\ntrait AuthChecker {\n  def isAuthorized(token: ApiToken): IO[Boolean]\n}\n\nobject AuthChecker extends AuthChecker {\n  def isAuthorized(token: ApiToken): IO[Boolean] = {\n    IO.pure(\n      token.value.nonEmpty\n    ) // put your logic here, currently just makes sure the token is not empty\n  }\n}\n")),(0,r.kt)("p",null,"This is a simple class that we will use to check the validity of a given token. This will be more complex in your own service, but we are keeping it simple here since it is out of the scope of this article and implementations will vary widely depending on your specific application."),(0,r.kt)("h4",{id:"the-inner-middleware-implementation"},"The Inner Middleware Implementation"),(0,r.kt)("p",null,"This function is what is called once we have made sure that the middleware is applicable for a given endpoint. We will show in the next step how to tell if the middleware is applicable or not. For now though, we will just focus on what the middleware does once we know that it needs to be applied to a given endpoint."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'def middleware(\n    authChecker: AuthChecker // 1\n): HttpApp[IO] => HttpApp[IO] = { inputApp => // 2\n  HttpApp[IO] { request => // 3\n    val maybeKey = request.headers // 4\n      .get[`Authorization`]\n      .collect {\n        case Authorization(\n              Credentials.Token(AuthScheme.Bearer, value)\n            ) =>\n          value\n      }\n      .map { ApiToken.apply }\n\n    val isAuthorized = maybeKey\n      .map { key =>\n        authChecker.isAuthorized(key) // 5\n      }\n      .getOrElse(IO.pure(false))\n\n    isAuthorized.ifM(\n      ifTrue = inputApp(request), // 6\n      ifFalse = IO.raiseError(new NotAuthorizedError("Not authorized!")) // 7\n    )\n  }\n}\n')),(0,r.kt)("p",null,"Let's break down what we did above step by step. The step numbers below correspond to the comment numbers above."),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"Pass an instance of ",(0,r.kt)("inlineCode",{parentName:"li"},"AuthChecker")," that we can use to verify auth tokens are valid in this middleware"),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("inlineCode",{parentName:"li"},"inputApp")," is the ",(0,r.kt)("inlineCode",{parentName:"li"},"HttpApp[IO]")," that we are transforming in this middleware."),(0,r.kt)("li",{parentName:"ol"},"Here we create a new HttpApp, the one that we will be returning from this function we are creating."),(0,r.kt)("li",{parentName:"ol"},"Here we extract the value of the ",(0,r.kt)("inlineCode",{parentName:"li"},"Authorization")," header, if it is present."),(0,r.kt)("li",{parentName:"ol"},"If the header had a value, we now send that value into the ",(0,r.kt)("inlineCode",{parentName:"li"},"AuthChecker")," to see if it is valid."),(0,r.kt)("li",{parentName:"ol"},"If the token was found to be valid, we pass the request into the ",(0,r.kt)("inlineCode",{parentName:"li"},"inputApp")," from step 2 in order to get a response."),(0,r.kt)("li",{parentName:"ol"},"If the header was found to be invalid, we return the ",(0,r.kt)("inlineCode",{parentName:"li"},"NotAuthorizedError")," that we defined in our smithy file above.")),(0,r.kt)("h4",{id:"serverendpointmiddlewaresimple"},"ServerEndpointMiddleware.Simple"),(0,r.kt)("p",null,"Next, let's create our middleware by implementing the ",(0,r.kt)("inlineCode",{parentName:"p"},"ServerEndpointMiddleware.Simple")," interface we discussed above."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"object AuthMiddleware {\n  def apply(\n      authChecker: AuthChecker // 1\n  ): ServerEndpointMiddleware[IO] =\n    new ServerEndpointMiddleware.Simple[IO] {\n      private val mid: HttpApp[IO] => HttpApp[IO] = middleware(authChecker) // 2\n      def prepareWithHints(\n          serviceHints: Hints,\n          endpointHints: Hints\n      ): HttpApp[IO] => HttpApp[IO] = {\n        serviceHints.get[smithy.api.HttpBearerAuth] match { // 3\n          case Some(_) =>\n            endpointHints.get[smithy.api.Auth] match { // 4\n              case Some(auths) if auths.value.isEmpty => identity // 5\n              case _                                  => mid // 6\n            }\n          case None => identity\n        }\n      }\n    }\n}\n")),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"Pass in an instance of ",(0,r.kt)("inlineCode",{parentName:"li"},"AuthChecker")," for the middleware to use. This is how the middleware will know if a given token is valid or not."),(0,r.kt)("li",{parentName:"ol"},"This is the function that we defined in the step above."),(0,r.kt)("li",{parentName:"ol"},"Check and see if the service at hand does in fact have the ",(0,r.kt)("inlineCode",{parentName:"li"},"httpBearerAuth")," trait on it. If it doesn't, then we will not do our auth checks. If it does, then we will proceed."),(0,r.kt)("li",{parentName:"ol"},"Here we are getting the ",(0,r.kt)("inlineCode",{parentName:"li"},"@auth")," trait from the operation (endpoint in smithy4s lingo). We need to check for this trait because of step 5."),(0,r.kt)("li",{parentName:"ol"},"Here we are checking that IF the auth trait is on this endpoint AND the auth trait contains an empty array THEN we are performing NO authentication checks. This is how we handle the ",(0,r.kt)("inlineCode",{parentName:"li"},"@auth([])")," trait that is present on the ",(0,r.kt)("inlineCode",{parentName:"li"},"HealthCheck")," operation we defined above."),(0,r.kt)("li",{parentName:"ol"},"IF the auth trait is NOT present on the operation, OR it is present AND it contains one or more authentication schemes, we apply the middleware.")),(0,r.kt)("h4",{id:"using-the-middleware"},"Using the Middleware"),(0,r.kt)("p",null,"From here, we can pass our middleware into our ",(0,r.kt)("inlineCode",{parentName:"p"},"SimpleRestJsonBuilder")," as follows:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'object HelloWorldAuthImpl extends HelloWorldAuthService[IO] {\n  def sayWorld(): IO[World] = World().pure[IO]\n  def healthCheck(): IO[HealthCheckOutput] = HealthCheckOutput("Okay!").pure[IO]\n}\n\nval routes = SimpleRestJsonBuilder\n      .routes(HelloWorldAuthImpl)\n      .middleware(AuthMiddleware(AuthChecker))\n      .resource\n')),(0,r.kt)("p",null,"And that's it. Now we have a middleware that will apply an authentication check on incoming requests whenever relevant, as defined in our smithy file."),(0,r.kt)("h2",{id:"client-side-middleware"},"Client-side Middleware"),(0,r.kt)("p",null,"To see the ",(0,r.kt)("strong",{parentName:"p"},"full code")," example of what we walk through below, go ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/disneystreaming/smithy4s/blob/2961b9cd2b9abb60c89b8f8424f5da61546f6a5a/modules/guides/src/smithy4s/guides/AuthClient.scala"},"here"),"."),(0,r.kt)("p",null,"It is possible that you have a client where you want to apply a similar type of middleware that alters some part of a request depending on the endpoint being targeted. In this part of the guide, we will show how you can do this for a client using the same smithy specification we defined above. We will make it so our authentication token is only sent if we are targeting an endpoint which requires it."),(0,r.kt)("h4",{id:"clientendpointmiddlewaresimple"},"ClientEndpointMiddleware.Simple"),(0,r.kt)("p",null,"The interface that we define for this middleware is going to look very similar to the one we defined above. This makes sense because this middleware is effectively the dual of the middleware above."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"object Middleware {\n\n  private def middleware(bearerToken: String): Client[IO] => Client[IO] = { // 1\n    inputClient =>\n      Client[IO] { request =>\n        val newRequest = request.withHeaders( // 2\n          Authorization(Credentials.Token(AuthScheme.Bearer, bearerToken))\n        )\n\n        inputClient.run(newRequest)\n      }\n  }\n\n  def apply(bearerToken: String): ClientEndpointMiddleware[IO] = // 3\n    new ClientEndpointMiddleware.Simple[IO] {\n      private val mid = middleware(bearerToken)\n      def prepareWithHints(\n          serviceHints: Hints,\n          endpointHints: Hints\n      ): Client[IO] => Client[IO] = {\n        serviceHints.get[smithy.api.HttpBearerAuth] match {\n          case Some(_) =>\n            endpointHints.get[smithy.api.Auth] match {\n              case Some(auths) if auths.value.isEmpty => identity\n              case _                                  => mid\n            }\n          case None => identity\n        }\n      }\n    }\n\n}\n")),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"Here we are creating an inner middleware function, just like we did above. The only differences are that this time we are adding a value to the request instead of extracting one from it and we are operating on ",(0,r.kt)("inlineCode",{parentName:"li"},"Client")," instead of ",(0,r.kt)("inlineCode",{parentName:"li"},"HttpApp"),"."),(0,r.kt)("li",{parentName:"ol"},"Add the ",(0,r.kt)("inlineCode",{parentName:"li"},"Authorization")," header to the request and pass it to the ",(0,r.kt)("inlineCode",{parentName:"li"},"inputClient")," that we are transforming in this middleware."),(0,r.kt)("li",{parentName:"ol"},"This function is actually the ",(0,r.kt)("em",{parentName:"li"},"exact same")," as the function for the middleware we implemented above. The only differences are that this apply method accepts a ",(0,r.kt)("inlineCode",{parentName:"li"},"bearerToken")," as a parameter and returns a function on ",(0,r.kt)("inlineCode",{parentName:"li"},"Client")," instead of ",(0,r.kt)("inlineCode",{parentName:"li"},"HttpApp"),". The provided ",(0,r.kt)("inlineCode",{parentName:"li"},"bearerToken")," is what we will add into the ",(0,r.kt)("inlineCode",{parentName:"li"},"Authorization")," header when applicable.")),(0,r.kt)("h4",{id:"simplerestjsonbuilder"},"SimpleRestJsonBuilder"),(0,r.kt)("p",null,"As above, we now just need to wire our middleware into our actual implementation. Here we are constructing a client and specifying the middleware we just defined."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'def apply(http4sClient: Client[IO]): Resource[IO, HelloWorldAuthService[IO]] =\n    SimpleRestJsonBuilder(HelloWorldAuthService)\n      .client(http4sClient)\n      .uri(Uri.unsafeFromString("http://localhost:9000"))\n      .middleware(Middleware("my-token")) // creating our middleware here\n      .resource\n')),(0,r.kt)("h2",{id:"conclusion"},"Conclusion"),(0,r.kt)("p",null,"Once again, if you want to see the ",(0,r.kt)("strong",{parentName:"p"},"full code")," examples of the above, you can find them ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/disneystreaming/smithy4s/blob/2961b9cd2b9abb60c89b8f8424f5da61546f6a5a/modules/guides/src/smithy4s/guides/"},"here"),"."),(0,r.kt)("p",null,"Hopefully this guide gives you a good idea of how you can create a middleware that takes your smithy specification into account. This guide shows a very simple use case of what is possible with a middleware like this. If you have a more advanced use case, you can use this guide as a reference and as always you can reach out to us for insight or help."))}c.isMDXComponent=!0}}]);