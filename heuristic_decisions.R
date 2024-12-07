# ---------------------------
# Evaluación de manos
evaluate_hand <- function(mano, comunitarias) {
  cartas_totales <- rbind(mano, comunitarias)
  valores_ordenados <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  cartas_totales$ValorNum <- match(cartas_totales$Valor, valores_ordenados)
  conteo_valores <- table(cartas_totales$Valor)
  conteo_palos <- table(cartas_totales$Palo)
  
  if (any(conteo_palos >= 5)) {
    return("Color")
  }
  if (any(conteo_valores == 4)) {
    return("Póker")
  }
  if (any(conteo_valores == 3) && any(conteo_valores == 2)) {
    return("Full House")
  }
  if (any(conteo_valores == 3)) {
    return("Trío")
  }
  if (sum(conteo_valores == 2) == 2) {
    return("Doble Pareja")
  }
  if (any(conteo_valores == 2)) {
    return("Pareja")
  }
  return("Carta Alta")
}

# ---------------------------
# Estrategias de decisión
decision_strategy <- function(strategy, hand_rank, turn, decisions_so_far) {
  decision_count <- sum(decisions_so_far == "Apostar" | decisions_so_far == "Igualar")
  if (strategy == "agresiva") {
    if (hand_rank >= 2 || decision_count >= turn) return("Apostar")
    else if (hand_rank >= 1) return("Igualar")
    else return("Retirarse")
  } else if (strategy == "conservadora") {
    if (hand_rank >= 4) return("Apostar")
    else if (hand_rank >= 2 && decision_count < 2) return("Igualar")
    else return("Retirarse")
  } else if (strategy == "mixta") {
    if (hand_rank >= 3 || (decision_count == 0 && turn == 1)) return("Apostar")
    else if (hand_rank >= 2) return("Igualar")
    else return("Retirarse")
  }
}

# ---------------------------
# Flujo de turnos con decisiones acumuladas y registro de acciones
play_turns_with_decisions <- function(manos, comunitarias, strategies, player_stats) {
  turnos <- list("Flop" = comunitarias[1:3, ], "Turn" = comunitarias[4, , drop = FALSE], "River" = comunitarias[5, , drop = FALSE])
  decisiones <- list()
  activos <- rep(TRUE, nrow(player_stats))
  decisions_so_far <- vector("list", nrow(player_stats))

  for (turno_idx in seq_along(turnos)) {
    turno <- names(turnos)[turno_idx]
    cartas_reveladas <- turnos[[turno]]
    
    evaluaciones <- sapply(manos, evaluate_hand, comunitarias = cartas_reveladas)
    jerarquia <- c("Carta Alta", "Pareja", "Doble Pareja", "Trío", "Escalera", "Color", "Full House", "Póker", "Escalera de Color", "Escalera Real")
    ranking <- match(evaluaciones, jerarquia)
    
    turno_decisiones <- sapply(1:nrow(player_stats), function(i) {
      if (!activos[i]) return("Retirado")
      decision <- decision_strategy(strategies[i], ranking[i], turno_idx, unlist(decisions_so_far[i]))
      decisions_so_far[[i]] <- c(decisions_so_far[[i]], decision)
      
      # Registrar acción en player_stats
      if (decision == "Retirarse") {
        activos[i] <- FALSE
        player_stats$Retiros[i] <<- player_stats$Retiros[i] + 1
      } else if (decision == "Apostar") {
        player_stats$Apuestas[i] <<- player_stats$Apuestas[i] + 1
      } else if (decision == "Igualar") {
        player_stats$Igualaciones[i] <<- player_stats$Igualaciones[i] + 1
      }
      return(decision)
    })
    decisiones[[turno]] <- turno_decisiones
    
    # Si queda un solo jugador activo, termina la partida
    if (sum(activos) == 1) {
      print(player_stats)
      ganador <- which(activos)
      return(list(ganador = ganador, decisiones = decisiones, player_stats = player_stats))
    }
  }
  return(list(ganador = NULL, decisiones = decisiones, player_stats = player_stats))
}
