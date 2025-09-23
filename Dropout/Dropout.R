#Primero, definamos dropout.

#-------------------------------------------------------------------------------
#                        ¿CUÁNTOS DROPOUTS HAY?
#-------------------------------------------------------------------------------

CantidadDrops <- df %>%
  filter(Eventos == "24m")%>%
  group_by(center)%>%
  summarise(
    cantidad = sum(EsDropout == "Dropout")
  )

CantidadDrops



#-------------------------------------------------------------------------------
#                ¿CAMBIAN SEGÚN EL ARM?
#-------------------------------------------------------------------------------

CantidadDropArm <- df %>%
  filter(Eventos == "24m")%>%
  group_by(Arm)%>%
  summarise(
    cantidad = sum(EsDropout == "Dropout"))

CantidadDropArm
