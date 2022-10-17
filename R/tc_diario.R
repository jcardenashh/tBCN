#' Recupera el tipo de cambio del día solicitado. Parametros: (fecha)
#'
#' @param fecha date
#'
#' @import XML *
#' @import lubridate *
#' @import httr *
#'
#' @return double tipo de cambio de la fecha solicitado.
#' @export
#'
#' @examples  tc_diario('2022-09-29')
#'
tc_diario <- function(fecha) {
    tryCatch({
        fecha <- as.Date(fecha)
        # parametros
        Anio <- lubridate::year(fecha)
        Mes <- lubridate::month(fecha)
        Dia <- lubridate::day(fecha)


        servicio <-
            "https://servicios.bcn.gob.ni/Tc_Servicio/ServicioTC.asmx"

        # header
        header <- httr::add_headers('Content-Type' = 'text/xml; charset=utf-8',
                                    'Accept' = 'text/xml; charset=utf-8')

        # body
        bodydia =
            paste0(
                '<?xml version="1.0" encoding="utf-8"?>
            <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
            <soap:Body>
              <RecuperaTC_Dia xmlns="http://servicios.bcn.gob.ni/">
                <Ano>',
            Anio,
            '</Ano>
                <Mes>',
            Mes,
            '</Mes>
                <Dia>',
            Dia,
            '</Dia>
              </RecuperaTC_Dia>
            </soap:Body>
            </soap:Envelope>'
            )

        #solicitud
        res <- httr::POST(servicio, body = bodydia, header)

        # convertir resultado en xml
        contenido <- rawToChar(res$content) # binario a texto
        contenido_xml <-
            XML::xmlTreeParse(contenido,
                              getDTD = F,
                              addAttributeNamespaces = F)
        rm(res)

        #obtenervalor
        tc_d = contenido_xml$children$Envelope[1]$Body[1]$RecuperaTC_DiaResponse[1]$RecuperaTC_DiaResult[1]$text$value
        return(as.double(tc_d))

    },
    error = function(e) {
        return(NA)
    })
}
