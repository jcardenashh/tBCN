#' Recupera el tipo de cambio del día solicitado. Parametros: (fecha)
#'  Tipo de cambio diario
#'
#' @description Recupera el tipo de cambio del día solicitado. Parametros: (fecha)
#'
#' @param fecha date
#' @return double tipo de cambio de la fecha solicitado.
#' @examples  tc_diario('2022-09-29')
#'
#' @export
#' @importFrom  XML xmlTreeParse
#' @importFrom   lubridate year month day
#' @importFrom  httr add_headers POST
#' @importFrom curl has_internet



tc_diario <- function(fecha) {

    # comprobamos que el parámetro fecha este indicado
    if(missing(fecha)){
        stop("fecha es un un parametro obligatorio")
    }

    tryCatch(
        {
        fecha <- as.Date(fecha)
        # parámetros para ser asignados en el body de la petición
        Anio <- year(fecha) # Extraemos el año del parámetro fecha
        Mes <- month(fecha) # Extraemos el mes del parámetro fecha
        Dia <- day(fecha)  # Extraemos el día del parámetro fecha


        servicio <-
            "https://servicios.bcn.gob.ni/Tc_Servicio/ServicioTC.asmx"

        # header para la petición
        header <- add_headers('Content-Type' = 'text/xml; charset=utf-8',
                                    'Accept' = 'text/xml; charset=utf-8')

        # body para la petición asignando los campos anio,mes,dia
        bodydia =
            paste0(
                '<?xml version="1.0" encoding="utf-8"?>
                <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                    <soap:Body>
                        <RecuperaTC_Dia xmlns="http://servicios.bcn.gob.ni/">
                            <Ano>',Anio,'</Ano>
                            <Mes>',Mes,'</Mes>
                            <Dia>',Dia,'</Dia>
                        </RecuperaTC_Dia>
                    </soap:Body>
                </soap:Envelope>'
            )

        #solicitud
        res <- POST(servicio, body = bodydia, header)

        # convertir resultado en xml
        contenido <- rawToChar(res$content) # binario a texto
        contenido_xml <-
            xmlTreeParse(contenido,
                         getDTD = F,
                         addAttributeNamespaces = F)
        rm(res)

        #obtenervalor
        tc_d = contenido_xml$children$Envelope[1]$Body[1]$RecuperaTC_DiaResponse[1]$RecuperaTC_DiaResult[1]$text$value
        return(as.double(tc_d))

    },
        error = function(e)
            {

            if(!has_internet()){

                message(paste0("Por favor verifique su conexion a internet"))

                return(NA)

            } else {

                message(paste0("Error indefinido: "))

                return(NA)
            }

    }
    )
}

