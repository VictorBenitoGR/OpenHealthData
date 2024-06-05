library(rsconnect)
library(dotenv)

# Carga las variables de entorno
dotenv::load_dot_env()

# Configura tu cuenta
rsconnect::setAccountInfo(
  name = Sys.getenv("ACCOUNT_NAME"),
  token = Sys.getenv("ACCOUNT_TOKEN"),
  secret = Sys.getenv("ACCOUNT_SECRET")
)

# Despliega tu aplicación
deployApp(appDir = "./src/lesotho")
