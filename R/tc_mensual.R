
#' Recupera el detalle completo de la base de tipos de cambio para el mes solicitado. Parámetros: (Anio, Mes)
#'
#' Tipo de cambio del mes solicitado
#'
#' @description Recupera el detalle completo de la base de tipos de cambio para el mes solicitado. Parámetros: (Anio, Mes)
#'
#' @param Anio numeric año
#' @param Mes  numeric (1-12)
#' @return dataframe detalle completo de los tipos de cambio para el mes solicitado.
#' @examples  tc_mensual(Anio=2019,Mes=9)
#'
#' @export
#' @importFrom  XML xmlTreeParse xmlSApply xmlValue xmlSApply
#' @importFrom  httr add_headers POST
#' @importFrom curl has_internet
#' @importFrom dplyr arrange

tc_mensual <- function(Anio, Mes) {


    # comprobamos que el parámetro anio este indicado
    if(missing(Anio)){
        stop("Anio es un un parametro obligatorio")
    }
    # comprobamos que el parámetro Mes este indicado
    if(missing(Mes)){
        stop("Mes es un un parametro obligatorio")
    }

    tryCatch(
        {
        # servicio
        servicio <-
            "https://servicios.bcn.gob.ni/Tc_Servicio/ServicioTC.asmx"
        # header para la petición
        header <-
            add_headers('Content-Type' = 'text/xml; charset=utf-8',
                              'Accept' = 'text/xml; charset=utf-8')

        # body para la peticion asignamos los parametros
        bodymes <-
            paste0(
            '<?xml version="1.0" encoding="utf-8"?>
            <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                <soap:Body>
                    <RecuperaTC_Mes xmlns="http://servicios.bcn.gob.ni/">
                        <Ano>',Anio,'</Ano>
                        <Mes>',Mes,'</Mes>
                    </RecuperaTC_Mes>
                </soap:Body>
            </soap:Envelope>'
            )

        #solicitud
        res <- POST(servicio, body = bodymes, header)

        # convertir resultado en xml
        contenido <- rawToChar(res$content) # binario a texto

        # convertimos a xml
        contenido_xml <-
            xmlTreeParse(contenido,
                         getDTD = F,
                         addAttributeNamespaces = F)
        rm(res)

        # obtener valores solicitud mensual
        tc = contenido_xml$children[1]$Envelope[1]$Body[1]$RecuperaTC_MesResponse[1]$RecuperaTC_MesResult[1]$Detalle_TC
        rm(contenido_xml)

        datos <-
            xmlSApply(tc, function(x)
                xmlSApply(x,xmlValue))
        rm(tc)

        # convertirlo en marco de datos ordenado por fecha
        return (arrange(data.frame(t(datos), row.names = NULL),Fecha))
    },
        error = function(e)
            {

            if(!has_internet()){

                message(paste0("Por favor verifique su conexion a internet"))

                return(NA)

            } else {

                message(paste0("Error indefinido: ", e))

                return(NA)
            }


    }


    )
}
