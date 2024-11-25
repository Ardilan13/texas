README - Simulación de Texas Hold'em
Este proyecto implementa una simulación del juego de Texas Hold'em Poker, permitiendo modelar decisiones de los jugadores basadas en estrategias heurísticas y realizar análisis estadísticos mediante simulaciones Monte Carlo.

Estructura del Proyecto
texas_holdem_logic.R
    - Contiene la lógica principal del juego, incluyendo la generación del mazo, la distribución de cartas, y la determinación del ganador de cada partida.
    - Se encarga de ejecutar los turnos del juego (Preflop, Flop, Turn, River).

heuristic_decisions.R
    - Implementa las estrategias heurísticas de los jugadores, como decisiones de apostar, igualar o retirarse, basadas en la fuerza de la mano y el progreso del juego.
    - Lleva un registro de las acciones de los jugadores (apuestas, igualaciones, retiros) durante cada turno.

monte_carlo_simulation.R
    - Realiza simulaciones de múltiples partidas para analizar el rendimiento de diferentes estrategias.
    - Consolida estadísticas como el número de victorias, apuestas, igualaciones y retiros de cada jugador.

para ejecutar la simulacion se debe ajustar los valores de las variables en el archivo monte_carlo_simulation.R y ejecutar el script.
