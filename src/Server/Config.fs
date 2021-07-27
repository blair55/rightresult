namespace Server

open Server

module Config =

  type ApplicationConfiguration =
    { neo4jUrl : string
      eventStoreUrl : string
      eventStoreUsername : string
      eventStorePassword : string
      clientHost : string
      encryptionKey : Jwt.EncryptionSecretBase64
      facebookClientId : string
      facebookClientSecret : string
      twitterConsumerKey : string
      twitterConsumerSecret : string
      feedbackFilePath : string
      pushSubscriptionPublicKey : string
      pushSubscriptionPrivateKey : string }

  let buildAppConfig (env:string -> string) =
    { neo4jUrl                   = env "NEO4JURL"
      eventStoreUrl              = env "EVENTSTOREURL"
      eventStoreUsername         = env "EVENTSTOREUSERNAME"
      eventStorePassword         = env "EVENTSTOREPASSWORD"
      clientHost                 = env "CLIENTHOST"
      encryptionKey              = env "ENCRYPTIONKEY" |> Jwt.EncryptionSecretBase64
      facebookClientId           = env "FACEBOOKCLIENTID"
      facebookClientSecret       = env "FACEBOOKCLIENTSECRET"
      twitterConsumerKey         = env "TWITTERCONSUMERKEY"
      twitterConsumerSecret      = env "TWITTERCONSUMERSECRET"
      feedbackFilePath           = env "FEEDBACKFILEPATH"
      pushSubscriptionPublicKey  = env "PUSHSUBSCRIPTIONPUBLICKEY"
      pushSubscriptionPrivateKey = env "PUSHSUBSCRIPTIONPRIVATEKEY" }
