source("C:/Users/Dilan/Documents/simulacion/texas_simulacion_R/texas_holdem_logic.R")

# ---------------------------
# Simulación Monte Carlo con métricas detalladas
monte_carlo_simulation <- function(n_simulations, n_players, strategies) {
  # Inicializar estadísticas globales
  stats <- data.frame(
    Jugador = 1:n_players,
    Estrategia = strategies,
    Victorias = 0,
    Apuestas = 0,
    Igualaciones = 0,
    Retiros = 0
  )
  
  # Inicializar registro de juegos
  juegos <- data.frame(
    Simulacion = integer(),
    Jugador = integer(),
    ManoGanadora = I(list()),
    JugadaGanadora = character()
  )
  
  # Ejecutar simulaciones
  for (sim in 1:n_simulations) {
    cat(blue(paste("\n=== Simulación", sim, "de", n_simulations, "===\n")))
    
    # Simular una partida
    partida <- simulate_texas_holdem(n_players, strategies)
    
    # Actualizar estadísticas de ganador
    ganador <- partida$resultado$ganador
    if (!is.null(ganador)) {
      stats$Victorias[stats$Jugador == ganador] <- stats$Victorias[stats$Jugador == ganador] + 1
      
      # Verificar datos de mano y jugada ganadora
      mano_ganadora <- if (!is.null(partida$resultado$mano_ganadora)) partida$resultado$mano_ganadora else NA
      jugada_ganadora <- if (!is.null(partida$resultado$jugada_ganadora)) partida$resultado$jugada_ganadora else NA
      
      # Registrar la mano y jugada ganadora
      juegos <- rbind(juegos, data.frame(
        Simulacion = sim,
        Jugador = ganador,
        ManoGanadora = list(mano_ganadora),
        JugadaGanadora = jugada_ganadora
      ))
    }
    
    # Consolidar acciones de jugadores
    stats$Apuestas <- stats$Apuestas + partida$player_stats$Apuestas
    stats$Igualaciones <- stats$Igualaciones + partida$player_stats$Igualaciones
    stats$Retiros <- stats$Retiros + partida$player_stats$Retiros
  }
  
  # Mostrar estadísticas finales
  cat(blue("\n======= Resultados Finales =======\n"))
  print(stats)
  
  # Mostrar registro de juegos
  cat(blue("\n======= Registro de Juegos =======\n"))
  print(juegos)
  
  return(list(Estadisticas = stats, Juegos = juegos))
}

# ---------------------------
# Configuración de Monte Carlo
n_simulations <- 10
n_players <- 3
strategies <- c("agresiva", "conservadora", "mixta")

resultados_montecarlo <- monte_carlo_simulation(n_simulations, n_players, strategies)
