{
  inputs,
  config,
  ...
}:
{
  imports = [
    inputs.agenix.homeManagerModules.default
  ];

  age.secrets."gemini-api-key".file = ../../../../secrets/gemini-api-key.age;
  # This must match the name of your encrypted file: gemini-api-key.age
  # It will be decrypted to /run/user/1000/secrets/gemini-api-key on activation

  home.sessionVariables = {
    GEMINI_API_KEY = "$(<${config.age.secrets."gemini-api-key".path})";
  };
}
