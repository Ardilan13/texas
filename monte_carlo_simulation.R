source("C:/Users/Dilan/Documents/simulacion/texas/texas_holdem_logic.R")

# ---------------------------
# Simulación Monte Carlo con métricas detalladas
monte_carlo_simulation <- function(n_simulations, n_players, strategies) {
  # Inicializar estadísticas globales
  stats <- data.frame(
    Jugador = 1:n_players,
    Estrategia = strategies,
    Victorias = 0
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
    }
    
    # Consolidar acciones de jugadores
    #stats$Apuestas <- stats$Apuestas + partida$player_stats$Apuestas
    #stats$Igualaciones <- stats$Igualaciones + partida$player_stats$Igualaciones
    #stats$Retiros <- stats$Retiros + partida$player_stats$Retiros
  }
  
  # Mostrar estadísticas finales
  cat(blue("\n======= Resultados Finales =======\n"))
  print(stats)
  
  return(stats)
}

# ---------------------------
# Configuración de Monte Carlo
n_simulations <- 10
n_players <- 3
strategies <- c("agresiva", "conservadora", "mixta")

resultados_montecarlo <- monte_carlo_simulation(n_simulations, n_players, strategies)
