# Instalar y cargar el paquete crayon para mejorar salida por terminal
if (!requireNamespace("crayon", quietly = TRUE)) {
  install.packages("crayon")
}
library(crayon)

# ---------------------------
# Generar mazo de cartas
generate_deck <- function() {
  palos <- c("Corazones", "Diamantes", "Tréboles", "Picas")
  valores <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  mazo <- expand.grid(Valor = valores, Palo = palos)
  return(mazo)
}

# ---------------------------
# Repartir cartas a jugadores y comunitarias
deal_cards <- function(mazo, n_jugadores) {
  mazo_barajado <- mazo[sample(nrow(mazo)), ]
  manos <- split(mazo_barajado[1:(n_jugadores * 2), ], rep(1:n_jugadores, each = 2))
  comunitarias <- mazo_barajado[(n_jugadores * 2 + 1):(n_jugadores * 2 + 5), ]
  return(list(manos = manos, comunitarias = comunitarias))
}

# ---------------------------
# Determinar el ganador de la partida
determine_winner <- function(manos, comunitarias) {
  evaluaciones <- sapply(manos, evaluate_hand, comunitarias = comunitarias)
  jerarquia <- c("Carta Alta", "Pareja", "Doble Pareja", "Trío", "Escalera", 
                 "Color", "Full House", "Póker", "Escalera de Color", "Escalera Real")
  
  # Convertir las evaluaciones a niveles de jerarquía
  ranking <- match(evaluaciones, jerarquia)
  mejor_ranking <- min(ranking)
  
  posibles_ganadores <- which(ranking == mejor_ranking)
  
  if (length(posibles_ganadores) == 1) {
    ganador <- posibles_ganadores
    mano_ganadora <- manos[[ganador]]  # Mano del ganador
    jugada_ganadora <- evaluaciones[ganador]  # Jugada del ganador
  } else {
    # Empate: resolverlo basado en el puntaje más alto de las cartas
    manos_combinadas <- lapply(posibles_ganadores, function(i) {
      rbind(manos[[i]], comunitarias)
    })
    puntajes <- sapply(manos_combinadas, function(cartas) {
      max(match(cartas$Valor, c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")))
    })
    ganador <- posibles_ganadores[which.max(puntajes)]
    mano_ganadora <- list(manos[[ganador]])  # Mano del ganador después del desempate
    jugada_ganadora <- evaluaciones[ganador]  # Jugada del ganador después del desempate
  }
  
  return(list(
    ganador = ganador, 
    evaluaciones = evaluaciones, 
    mano_ganadora = mano_ganadora, 
    jugada_ganadora = jugada_ganadora
  ))
}


# ---------------------------
# Simulación de Texas Hold'em con métricas de acciones
simulate_texas_holdem <- function(n_jugadores, strategies) {
  mazo <- generate_deck()
  juego <- deal_cards(mazo, n_jugadores)
  source("C:/Users/Dilan/Documents/simulacion/texas_simulacion_R/heuristic_decisions.R")
  
  # Inicializar estadísticas de jugadores como data.frame
  player_stats <- data.frame(
    Jugador = 1:n_jugadores,
    Apuestas = 0,
    Igualaciones = 0,
    Retiros = 0
  )
  
  turnos <- play_turns_with_decisions(juego$manos, juego$comunitarias, strategies, player_stats)
  if (!is.null(turnos$ganador)) {
    return(turnos)
  }
  
  resultado <- determine_winner(juego$manos, juego$comunitarias)
  return(list(decisiones = turnos$decisiones, player_stats = turnos$player_stats, resultado = resultado))
}




