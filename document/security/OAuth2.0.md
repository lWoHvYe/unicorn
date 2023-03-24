## spring-security-oauth (legacy)

- [code base](https://github.com/spring-attic/spring-security-oauth)
- [samples](https://github.com/spring-attic/spring-security-oauth/tree/main/samples)
- [oauth2.0 samples](https://github.com/jgrandja/spring-security-oauth-2-4-migrate)

## OAuth 2.0 Migration

- [OAuth-2.0-Migration-Guide](https://github.com/spring-projects/spring-security/wiki/OAuth-2.0-Migration-Guide)
- [announcing-the-spring-authorization-server](https://spring.io/blog/2020/04/15/announcing-the-spring-authorization-server)
- [spring-authorization-server](https://github.com/spring-projects/spring-authorization-server)
- [Official Samples](https://github.com/spring-projects/spring-authorization-server/tree/main/samples)

## Tips

- 有三个角色，Authorization Server、Resources Server、Client，这里AuthServer和ResServer是一起的，Client可以看作是TP，
  User通过Client访问ResServer，需要使用从AuthServer申请的Token，之后Client使用Token访问ResServer，ResServer会找AuthServer
  validate Provided-Token
- 在AuthServer中的User和Register进来的Client，User有Roles，Client有Scopes，这俩功能还不是特别清楚，已知Register时确定的scope明确了在申请token时可以选择的scope
- 授权模式
  客户端必须得到用户的授权（authorization grant），才能获得令牌（access token）。OAuth 2.0一共分成四种授权类型（authorization
  grant）

    - 授权码模式（authorization code）
    - 简化模式（implicit）
    - 密码模式（resource owner password credentials）
    - 客户端模式（client credentials）

  授权码模式和密码模式比较常用。但密码模式已经不推荐使用了

  第三方应用申请令牌之前，都必须先到系统备案，说明自己的身份，然后会拿到两个身份识别码：客户端 ID（client ID）和客户端密钥（client
  secret）。这是为了防止令牌被滥用，没有备案过的第三方应用，是不会拿到令牌的。
