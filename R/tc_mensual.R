
#' Recupera el detalle completo de la base de tipos de cambio para el mes solicitado. Parametros: (Año, Mes)
#'
#' @param Anio numeric año
#' @param Mes  numeric (1-12)
#'
#' @import XML *
#' @import lubridate *
#' @import httr *
#'
#' @return dataframe detalle completo de lostipos de cambio para el mes solicitado.
#' @export
#'
#' @examples  tc_mensual(Anio=2019,Mes=9)

tc_mensual <- function(Anio, Mes) {
    try({
        # servicio
        servicio <-
            "https://servicios.bcn.gob.ni/Tc_Servicio/ServicioTC.asmx"
        # header
        header <-
            httr::add_headers('Content-Type' = 'text/xml; charset=utf-8',
                              'Accept' = 'text/xml; charset=utf-8')

        # body
        bodymes <-
            paste0(
                '<?xml version="1.0" encoding="utf-8"?>
        <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
        <soap:Body>
        <RecuperaTC_Mes xmlns="http://servicios.bcn.gob.ni/">
        <Ano>',
        Anio,
        '</Ano>
        <Mes>',
        Mes,
        '</Mes>
        </RecuperaTC_Mes>
        </soap:Body>
        </soap:Envelope>'
            )

        #solicitud
        res <- httr::POST(servicio, body = bodymes, header)

        # convertir resultado en xml
        contenido <- rawToChar(res$content) # binario a texto
        contenido_xml <-
            XML::xmlTreeParse(contenido,
                              getDTD = F,
                              addAttributeNamespaces = F)
        rm(res)

        # obtener valores solicitud mensual
        tc = contenido_xml$children[1]$Envelope[1]$Body[1]$RecuperaTC_MesResponse[1]$RecuperaTC_MesResult[1]$Detalle_TC
        rm(contenido_xml)

        datos <-
            XML::xmlSApply(tc, function(x)
                XML::xmlSApply(x,XML::xmlValue))
        rm(tc)
        # convertirlo en marco de datos
        return (data.frame(t(datos), row.names = NULL))
    }
    )
}
